(define (accumulate combiner null-value term a next b)
  (if (> a b) 
    null-value
    (accumulate combiner (combiner null-value (term a)) term (next a) next b)))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (inc n) (+ n 1))

(define (id n) n)

(sum id 1 inc 10)

(product id 1 inc 5)

(define (accumulate-recu combiner null-value term a next b)
  (if (> a b) 
    null-value
    (combiner (term a) (accumulate-recu combiner null-value term (next a) next b))))

(define (sum-recu term a next b)
  (accumulate-recu + 0 term a next b))

(define (product-recu term a next b)
  (accumulate-recu * 1 term a next b))

(sum-recu id 1 inc 10)

(product-recu id 1 inc 5)
