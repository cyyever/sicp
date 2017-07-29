(define (enumerate-interval-exclude low high n)
  (cond 
    ((> low high) `())
    ((= low n) (enumerate-interval-exclude (+ low 1) high n))
    (else (cons low (enumerate-interval-exclude (+ low 1) high n)))))

(enumerate-interval-exclude 1 10 5)

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (exclude s n)
  (filter (lambda (x) (not (= x n))) s))

(exclude (enumerate-interval 1 10) 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (flat seq)
  (accumulate append `() seq))

(define (triple-sum n s)
  (filter 
    (lambda (x)  (not (null? x)))
    (flat
      (map 
	(lambda (i) 
	  (map 
	    (lambda (j) 

	      (let ((k (- s i j)))
		(if (and (> k 0) (not (= k i)) (not (= k j)))
		  (list i j k)
		  `()
		  )
		)
	      )
	    (exclude (enumerate-interval 1 (- s i)) i))
	  )
	(enumerate-interval 1 n))
      )
    )
  )

(triple-sum 10 10)
