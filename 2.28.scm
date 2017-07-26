(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (fringe t) 
  (cond ((null? t) t)
	((pair? t) (append (fringe (car t)) (fringe (cdr t))))
	(else (list t))))

(define x (list (list 1 2) (list 3 4)))

(fringe (list x x ))
