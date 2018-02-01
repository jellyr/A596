#lang racket

;Q1
(define (countdown n)
  (if (= n 0)
      (cons 0 '())
      (cons n (countdown (sub1 n)))))

;Q2
(define (insertR s1 s2 list)
  (cond [(null? list) '()]
        [(eqv? (car list) s1) (cons (car list)
                                    (cons s2 (insertR s1 s2 (cdr list))))]
        [else (cons (car list)
                    (insertR s1 s2 (cdr list)))]))

;Q3
(define (remv-1st s list)
          (cond [(null? list) '()]
                [(eqv? (car list) s) (cdr list)]
                [else (cons (car list)
                            (remv-1st s (cdr list)))]))

;Q4
(define (list-index-ofv? s list)
  (cond [(null? list) -1]
        [(eqv? (car list) s) 0]
        [else (add1 (list-index-ofv? s (cdr list)))]))

;Q5
(define (filter predicate list)
  (cond [(null? list) '()]
        [(predicate (car list)) (cons (car list)
                                      (filter predicate (cdr list)))]
        [else (filter predicate (cdr list))]))

;Q6
(define (zip list1 list2)
  (cond [(or (null? list1) (null? list2)) '()]
        [else (cons (cons (car list1) (car list2))
                    (zip (cdr list1) (cdr list2)))]))

;Q7
(define (map procedure list)
  (cond [(null? list) '()]
        [else (cons (procedure (car list))
                    (map procedure (cdr list)))]))

;Q8
(define (append list1 list2)
  (cond [(null? list1) list2]
        [else (cons (car list1)
                    (append (cdr list1) list2))]))

;Q9
(define (reverse list)
  (cond [(null? list) '()]
        [else (append (reverse (cdr list))
                      (cons (car list) '()))]))

;Q10
(define (fact n)
  (cond [(zero? n) 1]
        [else (* n (fact (sub1 n)))]))

;Q11
(define (memv e list)
  (cond [(null? list) #f]
        [(eqv? (car list) e) list]
        [else (memv e (cdr list))]))

;Q12
(define (fib n)
  (cond [(eqv? 0 n) 0]
        [(eqv? 1 n) 1]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))
;well, this is not an efficient way

;Q13
;'((w . (x . ())) y (z))
;'(y . (w . (x . ())) . (z . ())) ;;; why this weird order???

;Q14
(define (binary->natural list)
  (cond [(null? list) 0]
        [(= (car list) 0) (* 2 (binary->natural (cdr list)))]
        [(= (car list) 1) (add1 (* 2 (binary->natural (cdr list))))]
        [else '"invalid input"]))

;Q15
(define (minus a b)
  (cond [(zero? b) a]
        [else (minus (sub1 a) (sub1 b))]))

;Q16
(define (div a b)
  (cond [(zero? a) 0]
        [else (add1 (div (minus a b) b))]))

;Q17
(define (append-map procedure list)
  (cond [(null? list) '()]
        [else (append (procedure (car list))
                      (append-map procedure (cdr list)))]))

;Q18
(define (set-difference s1 s2)
  (cond [(null? s1) '()]
        [(null? s2) s1]
        [else (set-difference (remv-1st (car s2) s1) (cdr s2))]))

;Q19 DFS problem
(define (powerset ls)
  (cond [(null? ls) (list '())]
        [(null? (cdr ls)) (list (list (car ls)) '())]
        [else (append (map (lambda x (cons (car ls) (car x))) (powerset (cdr ls)))
                      (powerset (cdr ls)))]))


;Q20
(define (cartesian-product x)
  (cond [(and (list? (car x)) (list? (car (cdr x))) (null? (cdr (cdr x))))
         (append* (map (lambda a (cartesian-product(cons (car a) (cdr x)))) (car x)))]
        [(and (number? (car x)) (list? (car (cdr x))) (null? (cdr (cdr x))))
          (map (lambda a (list (car x) (car a))) (car (cdr x)))]
        [else (error "invalid input")]))
;is there some more convenient way like pattern matching?...



;Q22
(define collatz
  (letrec
    ((odd-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (even? x)) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
       (lambda (recur)
         (lambda (x)
           (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
       (lambda (x)
         (error 'error "Invalid value ~s~n" x))))
    (one-case (odd-case (even-case base)));; this should be a single line, without lambda
    ))


;Q21

(define (insertR-fr a b ls)
  (foldr (lambda (x almost)
           (cond [(eqv? a x) (cons a (cons b almost))]
                 [else (cons x almost)]))
         '()
         ls))

(define (filter-fr predicate ls)
  (foldr (lambda (x almost)
           (cond [(predicate x) (cons x almost)]
                 [else almost]))
         '()
         ls))

(define (map-fr procedure ls)
  (foldr (lambda (x almost)
           (cons (procedure x) almost))
         '()
         ls))

(define (append-fr list1 list2)
  (foldr (lambda (x almost)
           (cons x almost))
         list2
         list1))

(define (reverse-fr ls)
  (foldr (lambda (x almost)
           (append almost (list x)))
         '()
         ls))

(define (binary->natural-fr ls)
  (foldr (lambda (x almost)
           (cond [(= x 0) (* 2 almost)]
                 [(= x 1) (add1 (* 2 almost))]))
         0
         ls))

(define (append-map-fr procedure ls)
  (foldr (lambda (x almost)
           (append (procedure x) almost))
         '()
         ls))

(define (set-difference-fr s1 s2)
  (foldr (lambda (x almost)
           (remv-1st x almost))
         s1
         s2))


;;; I quit the last two...








