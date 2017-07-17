(define (sqrt-iter guess x)
    (if (good-enough? guess x)
            guess
	          (sqrt-iter (improve guess x)
			                      x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))


(sqrt-iter 0.0000001 0.001)

(define (good-enough? guess next-guess)
    (< (/ (abs (- guess next-guess)) guess) 0.001))

(define (sqrt-iter guess x)
  (let ((next-guess (improve guess x)))
    (if (good-enough? guess next-guess)
            guess
	          (sqrt-iter next-guess x))))

(sqrt-iter 0.0000001 0.001)
