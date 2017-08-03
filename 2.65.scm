(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
    (cond ((null? set) false)
	          ((= x (car set)) true)
		          ((< x (car set)) false)
			          (else (element-of-set? x (cdr set)))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
		    (cons (entry tree)
			  (copy-to-list (right-branch tree)
					result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
	(let ((left-tree (car left-result))
	      (non-left-elts (cdr left-result))
	      (right-size (- n (+ left-size 1))))
	  (let ((this-entry (car non-left-elts))
		(right-result (partial-tree (cdr non-left-elts)
					    right-size)))
	    (let ((right-tree (car right-result))
		  (remaining-elts (cdr right-result)))
	      (cons (make-tree this-entry left-tree right-tree)
		    remaining-elts))))))))

(define (intersection-list-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
	          ((element-of-set? (car set1) set2)        
		            (cons (car set1)
				                 (intersection-list-set (cdr set1) set2)))
		          (else (intersection-list-set (cdr set1) set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-list-set (tree->list-2 set1) (tree->list-2 set2))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-list-set set1 set2)
  (cond 
    ((null? set1) set2)
    ((null? set2) set1)
    (else 
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (union-list-set (cdr set1)
				   (cdr set2))))
	      ((< x1 x2)
	       (cons x1 (union-list-set (cdr set1) set2)))
	      (else
	       (cons x2 (union-list-set set1 (cdr set2)))))))))

(define (union-set set1 set2)
  (list->tree (union-list-set (tree->list-2 set1) (tree->list-2 set2))))


(union-set (make-tree 1 '() '()) (make-tree 2 '() '()))
(intersection-set (make-tree 1 '() '()) (make-tree 2 '() '()))
(intersection-set (make-tree 1 '() '()) (make-tree 1 '() '()))
