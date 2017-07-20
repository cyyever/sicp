(define (expmod base exp m)
  (define (check_square n)
    (define n_square (remainder (* n n) m))
    (if (and (not (= n 1)) (not (= n (- m 1))) (= n_square 1))
      0
      n_square
      ))

  (cond ((= exp 0) 1)
	((even? exp) (check_square (expmod base (/ exp 2) m)))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))

(define (miller-rabin-test n)
    (define (try-it a)
          (= (expmod a (- n 1) n) 1))
      (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
	          ((miller-rabin-test n) (fast-prime? n (- times 1)))
		          (else false)))

(miller-rabin-test 6601)
(miller-rabin-test 61)
