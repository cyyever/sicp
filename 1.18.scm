(define (fast-mul-iter a b)
  (define (help a b n) 
    (cond 
      ((< a b) (help b a n))
      ((= b 0) n)
      ((= b 1) (+ n a)
      ((even? b) (help (double a) (halv b) n))
      (else (help a (- b 1) (+ n a))))))
  (help a b 0))

(define (even? n)
  (= (double (halve n)) n))

