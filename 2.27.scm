(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (deep-reverse s)
  (cond 
    ((pair? s)
     (append (deep-reverse (cdr s)) (list (deep-reverse (car s)))))
    (else s))
  )

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)
