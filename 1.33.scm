(define (filtered-accumulate combiner null-value term a next b filter)
 (cond
  ((> a b) 
   null-value)
  ((filter a)
    (filtered-accumulate combiner (combiner null-value (term a)) term (next a) next b filter))
  (else
    (filtered-accumulate combiner null-value term (next a) next b filter)))) 
  

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square a)
  (* a a))

(define (inc a)
  (+ a 1))

(define (id a) a)


(define (sum-square-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(sum-square-prime 2 10)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))


(define (sum-relative-prime n)
  (define (relative-prime? a) 
    (= (gcd a n) 1))
  (filtered-accumulate + 0 id 1 inc n relative-prime?))

(sum-relative-prime 5)
