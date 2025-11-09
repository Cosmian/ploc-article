(define-module (sse ploc simulator)
  #:use-module (sse utils)
  #:use-module (sse ploc scheduler))

(define (make-psi pp-tree c n B V value-size PRF make-phi)

  (define phi (make-phi pp-tree (* c value-size) (lg B)))

  (define scheduler (make-scheduler n))

  (define (psi-setup) (phi 'setup))

  (define (psi-search label-idx)
    (let* ((targets (map (λ (v) (PRF label-idx v)) (iota V)))
           (subtree (phi 'select targets)))
      (values)))

  (define (psi-insert)
    (phi 'mutate (scheduler))
    (values))

  (λ (op . args)
    (match op
      ('setup  (apply psi-setup  args))
      ('search (apply psi-search args))
      ('insert (apply psi-insert args)))))
