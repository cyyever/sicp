(define (square x) (* x x))
(define (sum-of-squares a b) (+ (square a) (square b)))
(sum-of-squares 3 4)
(define (sum-of-max-2 a b c) (cond 
			       ((and (< a b) 
				    (< a c))
			       (sum-of-squares b c))
			       ((and (< b a) 
				    (< b c)) 
			       (sum-of-squares a c))
			       (else
				 (sum-of-squares a b)
				 )))
(sum-of-max-2 5 4 3)
