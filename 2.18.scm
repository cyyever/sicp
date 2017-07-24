(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (reverse s)
  (if (null? s)
    s
    (append (reverse (cdr s)) (list (car s)))))

(reverse (list 1 4 9 16 25))
