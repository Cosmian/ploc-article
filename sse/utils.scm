(library (sse utils)
  (export λ lg << >> && u8? u64? iota foldl split-with take match
          pair hash-map hash-map-find hash-map-bind!
          new-node sentinel node-id go-left go-right go-left? tree-dfs
          equal-tree-structure?)
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

  (define pair cons)
  (define head car)
  (define tail cdr)
  (define foldl fold-left)

  (define (take n lst)
    (let loop ((n n) (pfx (list)) (lst lst))
      (if (or (zero? n) (null? lst))
          (values pfx lst)
          (loop (- n 1) (pair (head lst) pfx) (tail lst)))))

  (define (split-with pred? xs)
    (let loop ((xs xs) (lhs (list)) (rhs (list)))
      (if (pair? xs)
          (if (pred? (head xs))
              (loop (tail xs) (pair (head xs) lhs) rhs)
              (loop (tail xs) lhs (pair (head xs) rhs)))
          (values lhs rhs))))

  (define (hash-map)                (make-hashtable equal-hash equal?))
  (define (hash-map-find  hmap k d) (hashtable-ref hmap k d))
  (define (hash-map-bind! hmap k v) (begin (hashtable-set! hmap k v) hmap))

  (define (new-node data lnode rnode) (vector data lnode rnode))
  (define (sentinel)                  (list))

  (define (tree-dfs f acc root)
    (let loop ((node root) (acc acc))
      (match node
        (#(data l-child r-child)
         (loop l-child
               (loop r-child
                     (f acc data))))
        (() acc))))

  (define (node-id  depth branch) (&& branch (- (<< 1 depth) 1)))
  (define (go-left  depth branch)    branch)
  (define (go-right depth branch) (+ branch (<< 1 depth)))
  (define (go-left? depth branch) (zero? (&& branch (<< 1 depth))))

  (define (equal-tree-structure? root1 root2)
    (define (l-child node) (vector-ref node 1))
    (define (r-child node) (vector-ref node 2))
    (let loop ((lhs root1) (rhs root2))
      (cond ((and (vector? lhs) (vector? rhs))
             (and
              (equal-tree-structure? (l-child lhs) (l-child rhs))
              (equal-tree-structure? (r-child lhs) (r-child rhs))))
            ((and (null? lhs) (null? rhs)) #t)
            (else #f)))))
