(define (cont-frac n d k)
  (define (cont-frac-helper n d k i)
    (if (= i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (cont-frac-helper n d k (+ i 1))))))
  (cont-frac-helper n d k 1))

(define tolerance 0.0001)
(define (try-cont-frac n d)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess n d k)
    (let ((next (cont-frac n
		    d
		    (+ 1 k))))

      (if (close-enough? guess next)
	next
	(try next n d (+ k 1)))))
  (try 0 n d 1))

(try-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) )
