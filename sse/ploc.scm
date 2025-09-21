(library (sse ploc)
  (export make-ploc)
  (import (guile)
          (rnrs base)
          (rnrs lists)
          (rnrs control)
          (sse tree)
          (sse utils)
          (sse crypto)
          (sse serialization)
          (sse ploc scheduler)
          (srfi :11))


  (define (new-datum target value)
    (vector target value))
  
  (define (datum->target datum) (vector-ref datum 0))
  (define (datum->value  datum) (vector-ref datum 1))


  (define (make-ploc n B V c H value-size read-value write-value!
                     ;; Tree dependencies.
                     G AE-init AE-keygen AE-encrypt AE-decrypt
                     memory-setup memory-read memory-write)

    (assert (u8?  c))
    (assert (u64? n))
    (assert (u64? B))
    (assert (u64? V))

    (define datum-size (+ 16 value-size))

    (define (write-datum! bytes pos datum)
      (let* ((pos (write-u128!  bytes pos (datum->target datum)))
             (pos (write-value! bytes pos (datum->value  datum))))
        pos))

    (define (read-datum bytes pos)
      (let*-values (((target pos) (read-u128  bytes pos))
                    ((value  pos) (read-value bytes pos)))
        (values (new-datum target value)
                pos)))

    (define (data->bytes data)
      (assert (<= (length data) c))
      (let ((bytes (make-u8vector (+ 1 (* c datum-size)))))
        (foldl (λ (pos datum) (write-datum! bytes pos datum))
               (write-u8! bytes 0 (length data))
               data)
        bytes))

    (define (bytes->data bytes)
      (assert (= (+ 1 (* c datum-size)) (bytevector-length bytes)))
      (let-values (((len pos) (read-u8 bytes 0)))
        (assert (<= len c))
        (let loop ((i 0) (pos pos) (data (list)))
          (if (= i len)
              data
              (let-values (((datum pos) (read-datum bytes pos)))
                (loop (+ i 1) pos (cons datum data)))))))

    (define-values (tree-setup tree-fetch tree-merge)
      (make-tree B G data->bytes bytes->data
                 AE-init AE-keygen AE-encrypt AE-decrypt
                 memory-setup memory-read memory-write))

    (define scheduler (make-scheduler n))

    (define volumes (make-hmap))
    
    (define (get-volume label)
      (hmap-find volumes label 0))
    
    (define (increment-volume label)
      (let ((volume (get-volume label)))
        (hmap-bind! volumes label (+ volume 1))))

    (define (get-target key label volume)
      (H key label volume))

    (define (make-datum key label value)
      (let* ((volume (get-volume label))
             (target (get-target key label volume)))
        (new-datum target value)))

    (define (get-all-targets key label)
      (map (λ (v) (get-target key label v))
           (iota V)))

    (define (find-data subtree target)
      (define (matching-datum? datum)
        (= (datum->target datum) target))
      (filter matching-datum? (branch-data subtree target)))

    (define (compact node depth data)
      (define (left-datum? datum)
        (go-left? depth (datum->target datum)))
      (define (direct-data data)
        (split-with left-datum? data))
      (match node
        (#(node-data l-child r-child)
         (let*-values
             (((l-data  r-data) (direct-data (append node-data data)))
              ((l-child l-data) (compact l-child (+ depth 1) l-data))
              ((r-child r-data) (compact r-child (+ depth 1) r-data))
              ((node-data rest) (take c (append l-data r-data))))
           (values (make-node node-data l-child r-child) rest)))
        (_ (values node data))))

    (define (ploc-setup) (tree-setup))

    (define (ploc-insert key label value)
      (let ((data    (list (make-datum key label value)))
            (subtree (tree-fetch key (scheduler))))
        (let-values (((subtree data) (compact subtree 0 data)))
          (unless (null? data)
            (error "tree overflow" subtree label value))
          (increment-volume label)
          (tree-merge key subtree))))

    (define (ploc-search key label)
      (let* ((targets  (get-all-targets key label))
             (subtree  (tree-fetch key targets)))
        (reverse
         (foldl (λ (result target)
                  (match (find-data subtree target)
                    (()      result)
                    ((datum) (cons (datum->value datum) result))
                    ( data   (error ploc-search
                                    "more than one datum found"
                                    data))))
                (list)
                targets))))

    (values ploc-setup ploc-search ploc-insert)))
