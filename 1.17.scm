(define (fast-mul a b)
    (cond 
      ((< a b) (fast-mul b a))
      ((= b 0) a)
      ((even? n) (+a (double (fast-mul 0 (halve b)))))
      (else (+ a (fast-mul a (- b 1))))))

(define (even? n)
  (= (double (halve n)) n))
