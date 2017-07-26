(define (tree-map proc t)
  (cond ((null? t) t)
	((pair? t) (cons (tree-map proc (car t)) (tree-map proc (cdr t))))
	(else
	  (proc t))))

(define (square-tree tree) (tree-map square tree))

(square-tree
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7)))
