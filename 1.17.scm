(define (fast-mul a b)
    (cond 
      ((< a b) (fast-mul b a))
      ((= b 0) 0)
      ((= b 1) a)
      ((even? b) (double (fast-mul a (halve b))))
      (else (+ a (fast-mul a (- b 1))))))

(define (even? n)
  (= (double (halve n)) n))
