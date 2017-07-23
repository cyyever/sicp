(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(((add-1 zero) (lambda (x) (- x 1))) 0)

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

((one (lambda (x) (- x 1))) 0)
((two (lambda (x) (- x 1))) 0)

(define (+ m n) (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(((+ one two) (lambda (x) (- x 1))) 0)
