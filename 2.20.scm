(define (same-parity x . y)
  (define (iter-arg y)
  (if (null? y)
    y
    (if (= (remainder (- x (car y)) 2) 0)
      (cons (car y) (iter-arg (cdr y)))
      (iter-arg (cdr y)))))
  (cons x (iter-arg y)))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)
