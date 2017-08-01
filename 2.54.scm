(define (equal? a b)
  (cond 
    ((and (null? a) (null? b))
     true)
    ((and (pair? a) (pair? b))
     (and (equal? (car a) (car b))
	  (equal? (cdr a) (cdr b))))
    ((and (symbol? a) (symbol? b))
     (eq? a b))
    (else false)))

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))
