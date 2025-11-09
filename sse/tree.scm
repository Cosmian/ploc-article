(library (sse tree)
  (export make-tree)
  (import (rnrs base) (sse utils) (sse serialization))

  (define (make-tree D G data->bytes bytes->data
                     AE-init AE-keygen AE-encrypt AE-decrypt
                     memory-setup memory-read memory-write)

    (define max-depth D)

    (define (make-address depth target)
      (G depth (node-id depth target)))

    (define (subtree-addresses branches)
      (let loop ((depth 0) (branches branches) (addresses (list)))
        (if (or (null? branches) (< max-depth depth))
            addresses
            (let-values (((lhs rhs)
                          (split-with (λ (b) (go-left? depth b))
                                      branches)))
              (loop (+ depth 1) rhs
                    (loop (+ depth 1) lhs
                          (pair (make-address depth (car branches))
                                addresses)))))))

    (define (word->data AE word)
      (if (null? word)
          (list)
          (bytes->data (AE-decrypt AE word))))

    (define (words->tree AE branches words)
      (let loop ((depth 0) (branches branches) (words words))
        (if (or (null? branches) (< max-depth depth))
            (values (sentinel) words)
            (let*-values
                (((lhs rhs) (split-with (λ (b) (go-left? depth b))
                                        branches))
                 ((r-child words) (loop (+ depth 1) rhs words))
                 ((l-child words) (loop (+ depth 1) lhs words)))
              (values (new-node (word->data AE (car words))
                                l-child r-child)
                      (cdr words))))))

    (define (tree->bindings AE root)
      (let loop ((depth 0) (branch 0) (node root) (bindings (list)))
        (match node
          (#(data l-child r-child)
           (let ((address (make-address depth branch))
                 (word    (AE-encrypt AE (data->bytes data))))
             (loop (+ depth 1) (go-right depth branch) r-child
                   (loop (+ depth 1) (go-left depth branch) l-child
                         (pair (pair address word) bindings)))))
          (() bindings))))

    (define (tree-setup)
      (memory-setup)
      (AE-keygen))

    (define (tree-select key branches)
      (let* ((addresses (subtree-addresses branches))
             (words     (memory-read addresses)))
        (words->tree (AE-init key) branches words)))

    (define (tree-mutate key branches transformation)
      (let* ((addresses (subtree-addresses branches))
             (words     (memory-read addresses))
             (AE        (AE-init key))
             (old-tree  (words->tree AE branches words))
             (new-tree  (transformation old-tree)))
        (assert (equal-tree-structure? old-tree new-tree))
        (memory-write (tree->bindings AE new-tree))))

    (values tree-setup tree-select tree-mutate)))
