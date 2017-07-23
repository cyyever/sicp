(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (percent i) 
  (/ (width i) (center i)))

(percent (make-center-percent 6.8 0.1))
