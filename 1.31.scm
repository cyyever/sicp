(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (factorial n) 
  (define (id a) a)
  (define (inc a) (+ 1 a))
  (product id 1 inc n))

(factorial 5)

(define (pi n) 
  (define (term a) 
    (if (= 0 (remainder a 2))
      (/ a (+ a 1))
      (/ (+ a 1) a)
      ))
  (define (inc a) (+ 1 a))
  (* 4.0 (product term 2 inc n)))

(pi 1000)

  
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-iter n) 
  (define (id a) a)
  (define (inc a) (+ 1 a))
  (product-iter id 1 inc n))


(factorial-iter 5)
