#lang racket/base

(module+ test
  (require
   rackunit
   rackunit/text-ui ;; convenience
   compatibility/mlist ;; Mut/Immutable cons/list translation
   "chapter1.rkt")
  
  (define/provide-test-suite ch1-tests
    (test-case "ex1.1"
      (define a 3)
      (define b (+ a 1))
      (check-equal?
       (mlist->list ex1.1)
       (list 10
             (+ 5 3 4)
             (- 9 1)
             (/ 6 2)
             (+ (* 2 4) (- 4 6))
             a ;; (define a 3) ;; can't actually evaluate this define here
             b ;; (define b (+ a 1)) 
             (= a b)
             (if (and (> b a) (< b (* a b))) b a)
             (cond ((= a 4) 6) ((= b 4) (+ 6 7 a)) (else 25))
             (+ 2 (if (> b a) b a))
             (* (cond ((> a b) a) ((< a b) b) (else -1)) (+ a 1)))))
    
    (test-case "ex1.2"
      (check-equal? ex1.2 (/ (- 37) 150)))

    (test-case "ex1.3"
      (check-equal? (ex1.3 1 2 3) 13)
      (check-equal? (ex1.3 3 2 1) 13)
      (check-equal? (ex1.3 3 1 2) 13)
      (check-equal? (ex1.3 3 3 3) 18))
    )

  (run-tests ch1-tests))


