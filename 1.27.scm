(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m)))) 

(define (fermat-test-2 n) 
  (define (help a n)
    (cond ((= a n) true)
	  ((= (expmod a n n) a) 
	   (help (+ a 1) n))
	  (else false)))
  (help 1 n))

(fermat-test-2 6601)
(fermat-test-2 6600)
