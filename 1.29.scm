(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (simpsom-integral f a b n)
  (define h (/ (- b a) n))
  (define (inc i) (+ i 1))
  (define (term i)
    (* (f (+ a (* i h))) (* 2 (+ 1 (remainder i 2)))) 
    )
  (/ (* h (+ (f a) (f b) (sum term 1 inc (- n 1)))) 3.0)) 

(define (cube x) (* x x x))

(simpsom-integral cube 0 1 100)
(simpsom-integral cube 0 1 1000)
