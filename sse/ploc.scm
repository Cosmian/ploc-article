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


  (define (datum value target)  (pair value target))
  (define (datum->value  datum) (car datum))
  (define (datum->target datum) (cdr datum))


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
        (values (datum value target)
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
                (loop (+ i 1) pos (pair datum data)))))))

    (define-values (tree-setup tree-select tree-mutate)
      (let ((D (inexact->exact (ceiling (lg B)))))
        (make-tree D G data->bytes bytes->data
                   AE-init AE-keygen AE-encrypt AE-decrypt
                   memory-setup memory-read memory-write)))

    (define scheduler (make-scheduler n))

    (define volumes (hash-map))
    
    (define (get-volume label)
      (hash-map-find volumes label 0))
    
    (define (increment-volume label)
      (let ((volume (get-volume label)))
        (hash-map-bind! volumes label (+ volume 1))))

    (define (get-target key label volume)
      (H key label volume))

    (define (make-datum key label value)
      (let* ((volume (get-volume label))
             (target (get-target key label volume)))
        (datum value target)))

    (define (get-all-targets key label)
      (map (λ (v) (get-target key label v))
           (iota V)))

    (define (tree-data tree)
      (define (index-datum mm datum)
        (let ((value  (datum->value  datum))
              (target (datum->target datum)))
          (hash-map-bind! mm target value)))
      (define (index-data mm data)
        (foldl index-datum mm data))
      (tree-dfs index-data (hash-map) tree))

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
           (values (new-node node-data l-child r-child) rest)))
        (() (values (list) data))))

    (define (ploc-setup) (tree-setup))

    (define (ploc-insert key label value)
      (let* ((data (list (make-datum key label value)))
             (tx   (λ (tree)
                     (call-with-values (λ () (compact tree 0 data))
                       (λ (tree overflow)
                         (if (null? overflow)
                             tree
                             (error ploc-insert "compaction overflow"
                                    label value tree overflow)))))))
        (tree-mutate key (scheduler) tx)
        (increment-volume label)))

    (define (ploc-search key label)
      (let* ((targets (get-all-targets key label))
             (subtree (tree-select key targets))
             (values  (tree-data subtree)))
        (reverse
         (foldl (λ (acc target)
                  (let ((value (hash-map-find values target #f)))
                    (if value (pair value acc) acc)))
                (list)
                targets))))

    (values ploc-setup ploc-search ploc-insert)))
