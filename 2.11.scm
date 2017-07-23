(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (mul-interval x y)
  (cond 
	 ((and (>= (lower-bound x) 0) (>= (upper-bound x) 0) (>= (lower-bound y) 0) (>= (upper-bound y) 0))
	  (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))

	 ((and (>= (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (< (upper-bound y) 0))
	  (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
	
	 ((and (< (lower-bound x) 0) (< (upper-bound x) 0) (< (lower-bound y) 0) (< (upper-bound y) 0))
	  (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))

	 ((and (< (lower-bound x) 0) (< (upper-bound x) 0) (>= (lower-bound y) 0) (>= (upper-bound y) 0))
	  (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))

	 ((and (>= (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y) 0))
	  (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))

	 ((and (< (lower-bound x) 0) (< (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y) 0))
	  (make-interval (* (upper-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))

	 ((and (< (lower-bound x) 0) (>= (upper-bound x) 0) (>= (lower-bound y) 0) (>= (upper-bound y) 0))
	  (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))

	 ((and (< (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (< (upper-bound y) 0))
	  (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))

	 ((and (< (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y) 0))
	  (make-interval 
	    (min (* (lower-bound x) (upper-bound y)) 
		 (* (lower-bound y) (upper-bound x)))
	    (max (* (lower-bound x) (lower-bound y))  
		 (* (upper-bound x) (upper-bound y)))))
	 ))

(define interval-a (make-interval 1 3))

(define interval-b (make-interval -1 1))

(mul-interval interval-a interval-b)
