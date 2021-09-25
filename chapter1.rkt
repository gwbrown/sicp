#lang racket/base

(module chapter1 sicp
  (define ex1.1 '(10 ;; 10
                  12 ;; (+ 5 3 4)
                  8  ;; (- 9 1)
                  3  ;; (/ 6 2)
                  6  ;; (+ (* 2 4) (- 4 6))
                  3  ;; (define a 3)
                  4  ;; (define b (+ a 1))
                  #f ;; (= a b)
                  4  ;; (if (and (> b a) (< b (* a b))) b a)
                  16 ;; (cond ((= a 4) 6) ((= b 4) (+ 6 7 a) (else 25)))
                  6  ;; (+ 2 (if (> b a) b a))
                  16 ;; (* (cond ((> a b) a) ((< a b) b) (else -1)) (+ a 1))
                  ))

  (define ex1.2
    (/
     (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))) )
     (* 3 (- 6 2) (- 2 7))))

  (define (square x)
    (* x x))

  (define (sum-squares a b)
    (+ (square a) (square b)))

  (define (ex1.3 a b c)
    (if (> a b)
        (if (> b c)
            (sum-squares a b)
            (sum-squares a c))
        (sum-squares b c)))

  (define (ex1.4 a b)
    ((if (> b 0) + -) a b))

  ;; Answer for ex1.5 & 1.6 is in the tests file

  ;; ex1.7
  (define (sqrt x)
    (sqrt-iter 1 x))

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

  (define (btr-sqrt x)
    (better-sqrt-iter 1 0 x))

  (define (better-sqrt-iter guess old-guess x)
    (if (better-enough? guess old-guess x)
        guess
        (better-sqrt-iter (improve guess x) guess x)))

  (define (better-enough? guess old-guess x)
    (< (abs (- guess old-guess)) (/ guess 1000)))

  ;; ex1.8
  (define (cube x)
    (* x x x))
  
  (define (cuberoot x)
    (cuberoot-iter 1 0 x))
  
  (define (cuberoot-iter guess old-guess x)
    (if (better-enough? guess old-guess x)
        guess
        (cuberoot-iter (improve-cuberoot guess x) guess x)))
  
  (define (improve-cuberoot guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (#%provide
   cuberoot-iter
   
   ex1.1
   ex1.2
   ex1.3
   ex1.4
   square
   sqrt
   btr-sqrt
   cube
   cuberoot))

(module+ test
  (require
   (submod ".." chapter1)
   rackunit
   rackunit/text-ui ;; convenience
   compatibility/mlist)
  
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
which evaluates to 0.")

    (test-case "ex1.6"
      "Because new-if is a procedure, rather than a special form,
the usual evaluation order for our interpreter applies - and since
our interpreter uses applicative-order evaluation, both `guess` and
the call to `sqrt-iter` are evaluated before the `new-if` call. Due
to this, `sqrt-iter` always calls itself, and therefore never halts.")

    (test-case "ex1.7"
      "The sqrt implementation as defined in the text will indeed work
quite poorly on small inputs - we can make a very rough check for this:
if the difference between the square and the guess is larger than the
absolute value of the original, the guess sucks."
      (define (does-sqrt-suck guess original)
        (< (abs original) (abs (- original (square guess)))))
      (check-true (does-sqrt-suck 3 4)) ;; Just to check that it works

      ;; The square root defined in the book doesn't suck for larger numbers
      (check-false (does-sqrt-suck (sqrt 4) 4))
      (check-false (does-sqrt-suck (sqrt 0.3) 0.3))

      ;; But for smaller numbers, especially smaller than the constant
      ;; we picked arbitrarily, it doesn't work so hot
      (check-true (does-sqrt-suck (sqrt 0.0003) 0.0003))
      (check-true (does-sqrt-suck (sqrt 0.00007) 0.00007))

      ;; Having defined an alternative that compares how much the guess
      ;; changes per iteration works much better, because making the
      ;; threshold relative to the guess, rather than a static threshold,
      ;; because no matter how small (or large) `guess` gets, it will
      ;; cause the iteration to halt once the guess isn't changing very
      ;; much (relative to itself!)
      (check-false (does-sqrt-suck (btr-sqrt 4) 4))
      (check-false (does-sqrt-suck (btr-sqrt 0.3) 0.3))
      (check-false (does-sqrt-suck (btr-sqrt 0.0003) 0.0003))
      (check-false (does-sqrt-suck (btr-sqrt 0.00007) 0.00007)))

    (test-case "ex1.8"
      (check-within (cuberoot 27) 3 0.00001) ;; arbitrary "close enough"
      (check-within (cuberoot (cube 4)) 4 0.00001)))

  

  (run-tests ch1-tests))


