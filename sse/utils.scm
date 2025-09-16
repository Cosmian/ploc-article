(library (sse utils)
  (export λ lg << >> && u8? u64? iota foldl split-with take match
          make-hmap hmap-find hmap-bind!
          make-hset hset-has? hset-add!  hset->list)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs hashtables)
          (rnrs arithmetic bitwise)
          (only (ice-9 match) match)
          (only (srfi :1) iota))

  (define-syntax λ
    (syntax-rules ()
      ((_ expr ...) (lambda expr ...))))

  (define (u8? n)
    (and (integer? n)
         (<= 0 n) (< n (<< 1 8))))

  (define (u64? n)
    (and (integer? n)
         (<= 0 n) (< n (<< 1 64))))

  (define (lg n) (/ (log n) (log 2)))

  (define (<< n e) (bitwise-arithmetic-shift-left  n e))
  (define (>> n e) (bitwise-arithmetic-shift-right n e))
  (define (&& n m) (bitwise-and n m))

  (define head car)
  (define tail cdr)

  (define foldl fold-left)

  (define (take n lst)
    (let loop ((n n) (pfx (list)) (lst lst))
      (if (or (zero? n) (null? lst))
          (values pfx lst)
          (loop (- n 1) (cons (head lst) pfx) (tail lst)))))

  (define (split-with pred? xs)
    (let loop ((xs xs) (lhs (list)) (rhs (list)))
      (if (pair? xs)
          (if (pred? (head xs))
              (loop (tail xs) (cons (head xs) lhs) rhs)
              (loop (tail xs) lhs (cons (head xs) rhs)))
          (values lhs rhs))))

  (define (make-hmap)           (make-hashtable equal-hash equal?))
  (define (hmap-find  hmap k d) (hashtable-ref hmap k d))
  (define (hmap-bind! hmap k v) (begin (hashtable-set! hmap k v) hmap))

  (define (make-hset)        (make-hashtable equal-hash equal?))
  (define (hset-has? hset v) (hashtable-ref hset v #f))
  (define (hset-add! hset v) (begin (hashtable-set! hset v #t) hset))
  (define (hset->list hset)  (vector->list (hashtable-keys hset))))
