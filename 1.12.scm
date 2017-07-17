(define (pascal-tri n m)
  (cond 
    ((= m 0) 1)
    ((= m n) 1)
    (else
      (+ (pascal-tri (- n 1) (- m 1)) (pascal-tri (- n 1) m)))
  ))

(pascal-tri 0 0)
(pascal-tri 2 1)
(pascal-tri 4 2)
