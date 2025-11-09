(define-module (tests tree)
  #:use-module (rnrs base)
  #:use-module (rnrs hashtables)
  #:use-module (ice-9 match)
  #:use-module (sse utils)
  #:use-module (sse crypto)
  #:use-module (sse serialization)
  #:use-module (tests memory)
  #:use-module (sse tree)
  #:use-module (srfi srfi-11)
  #:export (make-test-tree test-tree))


;;;
;;; Helpers
;;;

(define (make-counter)
  (let ((i 0))
    (λ _
      (let ((i* i))
        (set! i (1+ i))
        i))))

(define (tree-depth tree)
  (let loop ((node tree))
    (match node
      (#(_ l-child _)  (1+ (loop l-child)))
      (()              -1))))

(define (tree-map f tree)
  (let loop ((node tree))
    (match node
      (#(data l-child r-child)
       (vector (f data)
               (loop l-child)
               (loop r-child)))
      (_ (list)))))

(define (tree->data tree)
  (let loop ((node tree))
    (match node
      (#(data l-child r-child)
       (append data
               (loop l-child)
               (loop r-child)))
      (_ (list)))))


(define (data->bytes data)
  (let ((bytes (make-bytevector 5 0)))
    (match data
      (()  bytes)
      ((n) (begin
             (write-u8!  bytes 0 1)
             (write-u32! bytes 1 (car data))
             bytes))
      (_ (error data->bytes "incorrect number of datum" data)))))


(define (bytes->data bytes)
  (let ((len (read-u8 bytes 0)))
    (if (= 0 len)
        (list)
        (list (read-u32 bytes 1)))))


(define (G depth target)
    "Returns a 128-bit non-cryptographic hash of the given depth and branch."
    (+ (<< depth 64) target))


(define (make-test-tree D data->bytes bytes->data)
  (define-values (memory-setup memory-read memory-write . _)
    (in-memory))
  (make-tree D G data->bytes bytes->data
             aes-gcm-256:init aes-gcm-256:keygen
             aes-gcm-256:encrypt aes-gcm-256:decrypt
             memory-setup memory-read memory-write))


(define (test-tree D)
  (define-values (tree-setup tree-fetch tree-mutate)
    (make-test-tree D data->bytes bytes->data))

  (define N (<< 1 D))

  (define key (tree-setup))

  (define tree (tree-fetch key (iota N)))

  (assert (= (tree-depth tree) D))

  ;; Fills the entire the tree with zeros.
  (tree-mutate key (iota N)
               (λ (tree) (tree-map (const (list 0)) tree)))


  ;; Enumerates each node of the left part of the tree.
  (tree-mutate key (filter even? (iota N))
               (λ (tree) (tree-map (let ((counter (make-counter)))
                                     (compose list counter))
                                   tree)))

  ;; Reads the entire tree to check the results.
  (define tree-data (tree->data (tree-fetch key (iota N))))

  (define expected-data
    (append
     ;; Left part is just an enumeration
     (iota N 1)
     ;; Right part is only zeros, and there is one less element since the root
     ;; belongs to the left part.
     (map (const 0)
          (iota (1- N)))))

  (if (equal? tree-data expected-data)
      #t
      (begin (display "data mismatch:") (newline)
             (values tree-data expected-data))))
