(library (tests memory)
  (export in-memory)
  (import (rnrs base) (rnrs control) (rnrs hashtables))

  (define (in-memory)
    (define m #f)

    (define (assert-initialized)
      (unless m (error assert-initilized "uninitialized memory")))

    (define (setup)
      (set! m (make-hashtable equal-hash equal?)))

    (define (dump)
      (assert-initialized)
      (call-with-values (lambda () (hashtable-entries m))
        (lambda (keys vals)
          (map cons (vector->list keys) (vector->list vals)))))

    (define (clear)
      (assert-initialized)
      (hashtable-clear! m))

    (define (read as)
      (assert-initialized)
      (map (lambda (a) (hashtable-ref m a 'free))
	   as))

    (define (write bs)
      (assert-initialized)
      (for-each (lambda (b) (hashtable-set! m (car b) (cdr b)))
	        bs))

    (values setup read write clear dump)))
