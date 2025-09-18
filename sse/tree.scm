(library (sse tree)
  (export make-node go-left? branch-data make-tree)
  (import (rnrs base) (sse utils) (sse serialization))

  (define (make-node data l-child r-child)
    (vector data l-child r-child))
  
  (define (node->data    node) (vector-ref node 0))
  (define (node->l-child node) (vector-ref node 1))
  (define (node->r-child node) (vector-ref node 2))

  (define (go-left? depth branch)
    (zero? (&& (<< 1 depth) branch)))

  (define (l-child-id node-id) (+ (<< node-id 1) 0))
  (define (r-child-id node-id) (+ (<< node-id 1) 1))
  
  (define (next-node-id node-id depth target)
    (if (go-left? depth target)
        (l-child-id node-id)
        (r-child-id node-id)))

  (define (branch-data tree target)
    (let loop ((node tree) (node-id 0) (depth 0))
      (match node
        (#(bytes l-child r-child)
         (append bytes
                 (if (go-left? depth target)
                     (loop l-child (l-child-id node-id) (+ depth 1))
                     (loop r-child (r-child-id node-id) (+ depth 1)))))
        ('() (list)))))


  (define (make-tree N G data->bytes bytes->data
                     AE-init AE-keygen AE-encrypt AE-decrypt
                     memory-setup memory-read memory-write)

    (define max-depth (lg N))

    (define (make-address node-id depth) (G node-id depth))

    (define (subtree-addresses branches)
      (hset->list
       (foldl (λ (addresses branch)
                (foldl hset-add!
                       addresses
                       (branch-addresses branch)))
              (make-hset)
              branches)))

    (define (branch-addresses branch)
      (let loop ((node-id 0) (depth 0))
        (if (< max-depth depth)
            (list)
            (cons (make-address node-id depth)
                  (loop (next-node-id node-id depth branch)
                        (+ depth 1))))))

    (define (bindings->tree get-word word->data)
      (define sentinel (list))
      (define (unread? word) (eq? word 'unread))
      (define (free?   word) (eq? word 'free))
      (let loop ((node-id 0) (depth 0))
        (let* ((address (make-address node-id depth))
               (word    (get-word address)))
          (if (unread? word)
              sentinel
              (make-node (if (free? word) (list) (word->data word))
                         (loop (l-child-id node-id) (+ depth 1))
                         (loop (r-child-id node-id) (+ depth 1)))))))

    (define (tree->bindings tree data->word)
      (let loop ((node tree) (node-id 0) (depth 0))
        (match node
          (#(data l-child r-child)
           (let ((address (make-address node-id depth))
                 (word    (data->word data)))
             (append (list (cons address word))
                     (loop l-child (l-child-id node-id) (+ depth 1))
                     (loop r-child (r-child-id node-id) (+ depth 1)))))
          (_ (list)))))

    (define (tree-setup)
      (memory-setup)
      (AE-keygen))

    (define (tree-fetch key branches)
      (define AE (AE-init key))
      (let* ((addresses (subtree-addresses branches))
             (words     (memory-read addresses))
             (bindings  (foldl hmap-bind! (make-hmap) addresses words)))
        (bindings->tree
         (λ (address) (hmap-find bindings address 'unread))
         (λ (bytes) (bytes->data (AE-decrypt AE bytes))))))

    (define (tree-merge key subtree)
      (define AE (AE-init key))
      (let ((data->word (λ (data) (AE-encrypt AE (data->bytes data)))))
        (memory-write (tree->bindings subtree data->word))
        (values)))

    (values tree-setup tree-fetch tree-merge)))
