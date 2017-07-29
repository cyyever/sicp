(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
    (error "divide zero")
    (mul-interval x 
		  (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y))))))

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (percent i) 
  (/ (width i) (center i)))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))


(define A (make-center-percent 4 0.001))
(define B (make-center-percent 8 0.001))

(center (par1 A A))
(center	(par2 A A))

(percent (par1 A A))
(percent (par2 A A))

(center	(par1 A B))
(center (par2 A B))

(percent (par1 A B))
(percent (par2 A B))
