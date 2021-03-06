(define (iterative-improve good-enough? improve-guess)
  (define (iter init-guess)
    (if (good-enough? init-guess)
      init-guess
      (iter (improve-guess init-guess))))
  iter
  )

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  ((iterative-improve 
     (lambda (guess)        
       (< (abs (- (square guess) x)) 0.001))

     (lambda (guess)        
       (average guess (/ x guess)))
     )
    1.0))

(sqrt 2)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  ((iterative-improve 
     (lambda (guess)        
       (< (abs (- guess (f guess))) tolerance))
     f)
    first-guess))

(fixed-point cos 1.0)
