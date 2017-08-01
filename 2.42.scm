(define empty-board `())

(define (flatmap proc seq)
  (accumulate append `() (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))


(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (reverse s)
  (if (null? s)
    s
    (append (reverse (cdr s)) (list (car s)))))


(define (last-position positions)
  (car (reverse positions)))

(define (prev-positions positions)
  (reverse (cdr (reverse positions))))

(define (row-safe? prev-positions last-position)
  (cond ( (null? prev-positions)
	 true)
	(	(= (car prev-positions) last-position)
	 false
	 )
	(else
	  (row-safe? (cdr prev-positions) last-position))))

(define (diagonal-safe? prev-positions last-position k)
  (cond 
    ((null? prev-positions) true)
    ((= (car prev-positions) (- last-position (- k 1))) false)
    (else
      (diagonal-safe? (cdr prev-positions) last-position (- k 1)))))

(define (antidiagonal-safe? prev-positions last-position k)
  (cond 
    ((null? prev-positions) true)
    ((= (car prev-positions) (+ (- k 1) last-position)) false)
    (else
      (antidiagonal-safe? (cdr prev-positions) last-position (- k 1)))))

(define (safe? k positions) 
  (let (
	(last-position (car (reverse positions)))
	(prev-positions (reverse (cdr (reverse positions)))))

    (and (row-safe? prev-positions last-position)
	 (diagonal-safe? prev-positions last-position k)
	 (antidiagonal-safe? prev-positions last-position k))))
  
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
      (list empty-board)
      (filter
	(lambda (positions) 
	  (safe? k positions)
	  )
	(flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)
