(define (cons x y)
  (define (power x n)
    (if (= n 0)
      1
      (* x (power x (- n 1)))))
  (* (power 2 x) (power 3 y)))

(define (car n)
  (if (not (= 0 (remainder n 2)))
    0
    (+ 1 (car (/ n 2)))))

(define (cdr n)
  (if (not (= 0 (remainder n 3)))
    0
    (+ 1 (cdr (/ n 3)))))

(car (cons 1 2))
(cdr (cons 1 2))
