(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
     (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))

(define (branch-weight b)
  (if (pair? (branch-structure b))
    (total-weight (branch-structure b))
    (branch-structure b)))

(define (total-weight m)
  (+ (branch-weight (left-branch m)) (branch-weight (right-branch m))))

(define (branch-balanced? b)
  (if (pair? (branch-structure b))
    (balanced? (branch-structure b))
    true))


(define (balanced? m)
  (and 
    (branch-balanced? (left-branch m)) 
    (branch-balanced? (right-branch m))
    (= 
      (* (branch-length (left-branch m)) (branch-weight (left-branch m)))
      (* (branch-length (right-branch m)) (branch-weight (right-branch m)))
      )))

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cdr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cdr b))
