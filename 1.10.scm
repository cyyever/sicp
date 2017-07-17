(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))


(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))

(f 10)

(define (f n) (* 2 n))

(f 10)

(define (g n) (A 1 n))

(g 3)

(define (g n) (expt 2 n))

(g 3)

(define (h n) (A 2 n))

(h 3)

(define (h n)
  (if (= n 1) 2
    (expt 2 (h (- n 1)))))

(h 3)

(define (k n) (* 5 n n))
