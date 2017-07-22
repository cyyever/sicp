(define (cont-frac n d k)
  (define (cont-frac-helper n d k i)
    (if (= i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (cont-frac-helper n d k (+ i 1))))))
  (cont-frac-helper n d k 1))

(define (tan-cf x k)
  (cont-frac 
    (lambda (i)
      (if (= 1 i) 
	x
	(- (* x x))
	))
    (lambda (i) 
      (- (* i 2) 1))
    k))

(define pi 3.1415926)

(tan-cf (/ pi 4) 100)
