(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-rect lt-point rb-point)
  (cons lt-point rb-point))

(define (width rect)
  (- (x-point (cdr rect)) (x-point (car rect))))

(define (height rect)
  (- (y-point (cdr rect)) (y-point (car rect))))

(define (perimeter rect) 
  (* 2 (+ (width rect) (height rect))))

(define (area rect) 
  (* (width rect) (height rect)))

(define rect (make-rect (make-point 0 0) (make-point 1 2)))

(perimeter rect)

(area rect)

(define (make-rect lt-point width height)
  (cons lt-point (cons width height)))

(define (width rect)
  (car (cdr rect)))

(define (height rect)
  (cdr (cdr rect)))

(define (perimeter rect) 
  (* 2 (+ (width rect) (height rect))))

(define (area rect) 
  (* (width rect) (height rect)))

(define rect (make-rect (make-point 0 0) 1 2))

(perimeter rect)

(area rect)

