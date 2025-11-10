(define-module (tests benches)
  #:use-module (bench)
  #:use-module (sse utils)
  #:use-module (ice-9 format)
  #:use-module (tests ploc)
  #:use-module (tests bounds)
  #:export (run-benches run-all-benches))

(define* (run-benches κ D #:key
                      (log-level 1)
                      (max-total-time 60)
                      (min-n-samples 5))
  (map (λ (n)
         (let ((V  (ash 1 (ash D (- 1))))
               (N  (ash 1 D))
               (c* (get-min-c* κ n D)))
           (define-values (ploc-setup ploc-search ploc-insert)
             (make-test-ploc n N V c*))
           (define key (ploc-setup))
           (gc) (gc)
           (define-values (μ2 σ2)
             (bench (λ () (ploc-insert key "1" 1))
                    (cons 'max-total-time max-total-time)
                    (cons 'log-level log-level)
                    (cons 'min-n-samples min-n-samples)))
           (gc) (gc)
           (define-values (μ1 σ1)
             (bench (λ () (ploc-search key "1"))
                    (cons 'max-total-time max-total-time)
                    (cons 'log-level log-level)
                    (cons 'min-n-samples min-n-samples)))
           (format #t "
κ: ~d, n: ~d, lg(N): ~d
———————————————————————
search: [~,2e, ~,2e, ~,2e]
insert: [~,2e, ~,2e, ~,2e]
\n"
                   κ n D
                   (- μ1 σ1) μ1 (+ μ1 σ1)
                   (- μ2 σ2) μ2 (+ μ2 σ2))
           (values μ1 σ1 μ2 σ2)))
       (list 16 64 256)))

(define (run-all-benches κ)
  (map (λ (D) (run-benches κ D))
       (list 10 16 20)))
