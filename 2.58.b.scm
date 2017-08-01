(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	   (make-product (multiplier exp)
			 (deriv (multiplicand exp) var))
	   (make-product (deriv (multiplier exp) var)
			 (multiplicand exp))))
	(else
	  (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (if (memq '+ x) 
    true
    false))

(define (addend s) 
  (define (help s)
  (if (eq? (car s) '+) 
	    '()
	    (cons (car s) (help (cdr s)))))
  (let ((sub-sum (help s)))
    (if (null? (cdr sub-sum))
      (car sub-sum)
      sub-sum)))


(define (augend s) 
  (let 
    ((sub-sum
	  (cdr (memq '+ s)))) 
    (if (null? (cdr sub-sum))
      (car sub-sum)
      sub-sum)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*) (not (sum? x))))

(define (multiplier p) (car p))

(define (multiplicand p) 
  (let 
    ((sub-product
	  (caddr p)))
    (if (null? (cdr sub-product))
      (car sub-product)
      sub-product)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else 
	  (let 
	    ((m3 (if (sum? m1)
		   (list m1)
		   m1))
	     (m4 (if (sum? m2)
		   (list m2)
		   m2)))
	    (list m3 '* m4)
	    ))))

(deriv '(x + 3 * (x + y + 2)) 'x)
