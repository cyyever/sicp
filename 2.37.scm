(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    `()
    (cons (accumulate 
	    op 
	    init 
	    (map (lambda (x) (car x)) seqs)) 
	    (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2) (list 3 4))

(define (matrix-*-vector m v)
  (map (lambda(row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons `() mat))

(transpose (list (list 1 2) (list 3 4)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4)))
