
(define (improve guess x)
    (/ (+ (* 2 guess) (/ x (* guess guess))) 3))

(define (good-enough? guess next-guess)
    (< (/ (abs (- guess next-guess)) guess) 0.00001))

(define (cube-iter guess x)
  (let ((next-guess (improve guess x)))
    (if (good-enough? guess next-guess)
            guess
	          (cube-iter next-guess x))))

(define (cube-root x) 
  (cube-iter 1.0 x))


(cube-root 27)

(cube-root 0.0001)
