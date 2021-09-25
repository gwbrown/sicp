#lang sicp

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

;; Answer for ex1.5 is in the tests file

(#%provide ex1.1 ex1.2 ex1.3 ex1.4)
