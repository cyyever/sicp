(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ 1 x))
(define (squara x) (* x x))

((compose square inc) 6)
