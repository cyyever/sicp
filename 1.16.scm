(define (even? n)
    (= (remainder n 2) 0))

(define (fast-expt-iter b n)
  (define (help b n a) 
    (cond 
      ((= n 0) a) 
      ((even? n) (help (* b b) (/ n 2) a))
      (else (help b (- n 1) (* a b)))))
    (help b n 1))

(fast-expt-iter 2 10)
