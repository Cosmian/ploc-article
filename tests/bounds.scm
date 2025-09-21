(library (tests bounds)
  (export get-min-c* get-table-data)
  (import (rnrs base) (rnrs r5rs) (rnrs arithmetic fixnums) (sse utils))

  (define ln log)
  (define m (lambda (d) (<< 1 d)))
  (define p (lambda (d) (/ (<< 1 d))))


  (define (repeat n thunk) (map (lambda _ (thunk)) (iota n)))
  (define (mean vs) (/ (sum vs) (length vs)))
  (define (sum vs) (foldl + 0 vs))
  (define (T d n) (/ (<< 1 d) n))


  (define (k λ m B p)
    "Returns the smallest integer solution of the Chernoff bound generated with
  the given multiplicity m, number of bindings b and probability p."

    (assert (integer? m))
    (assert (and (rational? p) (<= 0 p) (<= p 1)))

    (define (Chernoff c)
      (/ (+ (* λ (ln 2)) (ln m))
         (- (ln c) (ln (* B p)) 1)))

    (define c-min
      (max 1 (inexact->exact
              (ceiling
               (exp (+ 1 (ln (* B p))))))))

    (let loop ((c c-min))
      (let ((bound (Chernoff c)))
        (if (> c bound)
            (- c 1)
            (loop (+ c 1))))))


  (define (b-max d k n κ) (k κ (m d) (* k (T d n)) (p d)))
  (define (bc-max d k n κ) (k κ (m d) (* k (T d n)) (p (+ d 1))))


  (define (ω κ c d d-max)
    (define m (<< 1 d))
    (define B (<< 1 d-max))
    (define p (/ m))
    (let loop ((m m) (b (k κ m B p)) (p p))
      (cond ((zero? b) 0)
            ((= m B)   (max 0 (- b c)))
            (else (let* ((m* (<< m 1))
                         (p* (/ p 2))
                         (b* (min b (k κ m* B p*))))
                    (max 0 (+ (loop m* b* p*)
                              (loop m* (- b b*) p*)
                              (- c))))))))

  (define (get-min-c d-max κ)
    (define (non-overflowing? c)
      (let loop ((d d-max) (overflow (greatest-fixnum)))
        (if (< 0 d)
            (let* ((b         (k κ (m d) (<< 1 d-max) (p d)))
                   (overflow* (ω κ c d d-max)))
              (cond ((zero? overflow*)      d)
                    ((< overflow overflow*) #f)
                    (else (loop (- d 1) overflow*))))
            #f)))

    (call/cc
     (lambda (return)
       (for-each (lambda (c)
                   (let ((min-depth (non-overflowing? c)))
                     (if min-depth
                         (return (list c min-depth)))))
                 (iota 1000 1)))))


  (define examples
    (apply append
           (map (lambda (κ) (map (lambda (d-max)
                                   (match (get-min-c d-max κ)
                                     ((c-min . d-min) `((κ     . ,κ)
                                                        (d-max . ,d-max)
                                                        (c-min . ,c-min)
                                                        (d-min . ,d-min)))))
                                 (map (lambda (e) (<< 1 e))
                                      (iota 6 1))))
                (list 64 128))))

  (define (get-k* κ c* m)
    (/ (* (/ c* (ln 2))
          (+ 1 (sqrt (+ 1 (/ (* 4 (ln 2) (+ κ (lg m)))
                             c*)))))
       c*))

  (define (get-min-c* κ n d-max)
    (define m (+ (- (* 2 n) 1) (* n (- d-max (lg n)))))
    (define static-d* (cadr (get-min-c d-max κ)))
    (let loop ((d 0))
      (let* ((B  (T d n))
             (p  (p d))
             (c  (k κ m B p)))
        (define dynamic-d* (get-k* κ c m))
        (cond ((and (< (- d-max d) dynamic-d*)
                    (= 0 (ω κ c d d-max)))
               c)
              ((< d d-max) (loop (+ d 1)))
              (else (error get-min-c* "no solution" κ n d-max))))))

  (define (get-table-data κ)
    (map (lambda (n)
           (map (lambda (D)
                  (get-min-c* κ n D))
                (list 16 32 40)))
         (list 16 32 64 128 256))))
