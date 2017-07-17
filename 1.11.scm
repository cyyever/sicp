(define (f n) 
  (cond ((< n 3) n)
	(else (+ 
		(f (- n 1))
		(* 2 (f (- n 2)))
		(* 3 (f (- n 3)))))))

(f 10)


(define (f-iter n) 
  (define (help a b c count) 
    (if (= count 0)
      c
      (help (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (help 2 1 0 n))

(f-iter 10)
