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

    (test-case "ex1.4"
      "Because our model of evaluation allows for combinations whose
operators are compound expressions, we can use an if statement in the
place we'd normally expect the name of a function or special operator.
Any expression that evaluates to a function can be used this way,
although doing so may make for confusing code.

The code in question uses the sign of `b` to determine the function
used to combine `a` and `b`. If `b` is positive, it uses `+`, adding
`a` to `b`. If `b` is negative, it uses `-`, subtracting `b` from `a`.
In this way, it adds the absolute value of `b` to `a`."
      (check-equal? (ex1.4 2 3) 5)
      (check-equal? (ex1.4 2 -3) 5)
      (check-equal? (ex1.4 -3 5) 2)
      (check-equal? (ex1.4 -4 -4) 0))

    (test-case "ex1.5"
      "When Ben uses an interpreter with applicative-order evaluation,
to evaluate (test 0 (p)), the interpreter will appear to hang as the
code is an infinite recursive loop. Because (p) is evaluated first,
due to being applicative-order, and evaluates to itself, it will
simply loop evaluating to itself until interrupted by a user.

When using a normal-order interpreter, Ben would instead see the
expression (test 0 (p)) evaluate to 0. Because the expression would
get expanded to (if (= 0 0) 0 (p)), which becomes (if #t 0 (p)),
which evaluates to 0."))

  

  (run-tests ch1-tests))


