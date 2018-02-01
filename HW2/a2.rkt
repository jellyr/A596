#lang racket

;Q1
(define list-ref
  (lambda (ls n)
    (letrec
        ((nth-cdr
          (lambda (n)
            (if (eqv? n 0)
                ls
                (cdr (nth-cdr (sub1 n)))))))
      (car (nth-cdr n)))))

;Q2
(define (union l1 l2)
  (cond [(null? l2) l1]
        [else (if (memv (car l2) l1)
                  (union l1 (cdr l2))
                  (union (append l1 (list (car l2))) (cdr l2)))]))

;Q3
(define (extend x pred)
  (lambda (a)
    (or (eqv? x a) (pred a))))

;Q4
(define (walk-symbol x s)
  (if (not (assv x s))
      x
      (if (symbol? (cdr (assv x s)))
          (walk-symbol (cdr (assv x s)) s)
          (cdr (assv x s)))))

;Q5 Given in lecture note
(define lambda->lumbda
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y) y]
      [`(lambda (,x) ,b) #:when (symbol? x) `(lumbda (,x) ,(lambda->lumbda b))]
      [`(,rator ,rand) `(,(lambda->lumbda rator) ,(lambda->lumbda rand))])))

;Q6
(define var-occurs?
  (lambda (v exp)
    (match exp
      [`,y #:when (symbol? y) (eqv? v y)]
      [`(lambda (,x) ,b) (var-occurs? v b)]
      [`(,rator ,rand) (or (var-occurs? v rator)
                           (var-occurs? v rand))])))


;Q7
(define vars
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y) (list y)]
      [`(lambda (,x) ,b) #:when (symbol? x) (vars b)]
      [`(,rator ,rand) (append (vars rator) (vars rand))])))

;Q8
(define unique-vars
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y) (list y)]
      [`(lambda (,x) ,b) #:when (symbol? x) (unique-vars b)]
      [`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))])))

;Q9 Given in lecture note
(define var-occurs-free?
  (lambda (v e)
    (match e
      [`,y #:when (symbol? y) (eqv? y v)]
      [`(lambda (,x) ,b) (and (not (eqv? x v)) (var-occurs-free? v b))]
      [`(,rator ,rand) (or (var-occurs-free? v rator)
                           (var-occurs-free? v rand))])))

;Q10 Given in lecture note
(define var-occurs-bound?
  (lambda (v e)
    (match e
      [`,y #:when (symbol? y) #f]
      [`(lambda (,x) ,b) (or (var-occurs-bound? v b)
                             (and (eqv? x v) (var-occurs-free? v b)))]
      [`(,rator ,rand) (or (var-occurs-bound? v rator)
                           (var-occurs-bound? v rand))])))

;Q11
(define unique-free-vars
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y) (list y)]
      [`(lambda (,x) ,b) #:when (symbol? x) (remv x (unique-free-vars b))]
      [`(,rator ,rand) (union (unique-free-vars rator) (unique-free-vars rand))])))

;Q12
(define unique-bound-vars
  (lambda (exp)
    (match exp
      [`,y #:when (symbol? y) '()]
      [`(lambda (,x) ,b) #:when (symbol? x) (if (var-occurs-bound? x `(lambda (,x) ,b))
                                                (list x)
                                                '())]
      [`(,rator ,rand) (union (unique-bound-vars rator) (unique-bound-vars rand))])))

;Q13
(define lex
  (lambda (exp acc)
    (match exp
      [`,y #:when (symbol? y) (if (memv y acc)
                                  `(var ,(- (length acc) (length (memv y acc))))
                                  `())]
      [`(lambda (,x) ,b) #:when (symbol? x) `(lambda ,(lex b (cons x acc)))]
      [`(,rator ,rand) `(,(lex rator acc) ,(lex rand acc))])))



