(define (square-tree t)
  (cond ((null? t) t)
	((pair? t) (cons (square-tree (car t)) (square-tree (cdr t))))
	(else
	  (* t t))))

(square-tree
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7)))

(define (square-tree t)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (square-tree sub-tree)
	   (* sub-tree sub-tree))) t))

(square-tree
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7)))
