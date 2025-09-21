(define-module (tests ploc)
  #:use-module (sse utils)
  #:use-module (sse ploc)
  #:use-module (sse serialization)
  #:use-module (sse crypto)
  #:use-module (tests tree)
  #:use-module (tests memory)
  #:use-module (ice-9 format)
  #:use-module (rnrs base)
  #:use-module (rnrs hashtables)
  #:use-module (rnrs bytevectors)
  #:export (make-test-ploc test-ploc))


(define (make-test-ploc n B V c)
  (define (H key label i)
    "Returns a 128-bit cryptograohic hash of the given label and index."
    (let ((bytes (make-bytevector (+ (bytevector-length key)
                                     (bytevector-length label)
                                     8))))
      (let* ((pos (write-bytes! bytes 0   key))
             (pos (write-bytes! bytes pos label))
             (pos (write-u64!   bytes pos i)))
        (read-u128 (sha3 bytes) 0))))

  (define (G node-id depth)
    "Returns a 128-bit non-cryptographic hash of the given node ID and depth."
    (+ (<< (equal-hash node-id) 8)
       (equal-hash depth)))

  (define-values (setup read write . _) (in-memory))

  (define-values (ploc-setup ploc-search ploc-insert)
    (make-ploc n B V c H
               8 read-u64 write-u64!
               G aes-gcm-256:init
               aes-gcm-256:keygen
               aes-gcm-256:encrypt
               aes-gcm-256:decrypt
               setup read write))

  (let ((setup  (λ () (ploc-setup)))
        (search (λ (key label)
                  (ploc-search key (string->utf8 label))))
        (insert (λ (key label value)
                  (ploc-insert key (string->utf8 label) value))))
    (values setup search insert)))


(define (make-sse-test sse-setup sse-search sse-insert)
  (define bindings-list
    (map (λ (i)
           (map (λ (j)
                  (let ((k (format #f "k-~d" i))
                        (v j))
                    (cons k v)))
                (iota (1+ i))))
         (iota 10)))

  (define searched-keywords
    (map (λ (bindings) (caar bindings))
         bindings-list))

  (define searched-results
    (map (λ (bindings) (map cdr bindings))
         bindings-list))

  (λ ()
    (define key (sse-setup))

    (for-each (λ (bindings)
                (for-each (λ (b) (sse-insert key (car b) (cdr b)))
                          bindings))
              bindings-list)

    (let ((res (map (lambda (bindings)
                      (sse-search key (caar bindings)))
                    bindings-list)))

      (if (equal? res searched-results)
          #t
          (values res searched-results)))))


(define (test-ploc n B V c)
  (define test
    (call-with-values (λ () (make-test-ploc n B V c))
      (λ (ploc-setup ploc-search ploc-insert)
        (make-sse-test ploc-setup ploc-search ploc-insert))))

  (test))
