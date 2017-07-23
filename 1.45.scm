(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 3)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

(define (4th-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (* y y y))))
               1.0))

(4th-root 16)

(define (power x n)
  (if (= n 0)
    1
    (* x (power x (- n 1)))))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (- n 2)) (lambda (y) (/ x (power y (- n 1)))))
	       1.0))

(nth-root 16 4)
(nth-root 32 5)
(nth-root 64 6)
