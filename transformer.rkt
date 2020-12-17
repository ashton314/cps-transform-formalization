#lang racket

(define/match (all? pred? lst)
  [(_ '()) #t]
  [(pred? (cons hd tl)) (and (pred? hd) (all? pred? tl))])

;; Defines a set of "simple" expressions that require no cps conversion
(define (simple? expr)
  (or (boolean? expr) (number? expr) (symbol? expr) (builtin? expr) (null? expr)
      (and (builtin? (car expr)) (all? simple? (cdr expr)))))

(define (builtin? expr) (member expr '(+ - / * = car cdr cons null?)))

(define (cps expr k)
  (match expr
    [(? simple? S) `(,k ,S)]

    [`(lambda (,args ...) ,body)
     (let ([k₀ (gensym)])
       `(,k (lambda ,(append args (list k₀)) ,(cps body k₀))))]

    [`(,(? simple? M₁) ,(? simple? M₂))
     `(,M₁ ,M₂ ,k)]

    [`(,(? (compose not simple?) M₁) ,M₂)
     (let* ([k₀ (gensym)]
            [body (cps `(,k₀ ,M₂) k)])
       (cps M₁ `(lambda (,k₀) ,body)))]

    [`(,(? simple? S₁) ,M₂)
     (let* ([k₀ (gensym)]
            [body (cps `(,S₁ ,k₀) k)])
       (cps M₂ `(lambda (,k₀) ,body)))]

    [`(if ,test ,tcase ,fcase)
     (let ([v₀ (gensym)])
       (cps test `(lambda (,v₀) (if ,v₀ ,(cps tcase k) ,(cps fcase k)))))]
    ))

