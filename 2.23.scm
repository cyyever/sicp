(define (for-each proc s)
  (cond ((null? s) true)
	(else
	  (proc (car s))
	  (for-each proc (cdr s)))))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 8))
