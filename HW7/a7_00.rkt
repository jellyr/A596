#lang racket

;Q1
(require racket/trace)
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                ;; fill in lines here
                [(null? ls) '()]
                [(eqv? 0 (car ls)) (k (last-non-zero (cdr ls)))]
                [else (cons (car ls)(last-non-zero (cdr ls)))]
                ))))
        (last-non-zero ls)))))

;Q2
(define lex
  (lambda (exp acc)
    (match exp
      [`,y #:when (symbol? y) (if (memv y acc)
                                  `(var ,(- (length acc) (length (memv y acc))))
                                  y)]
      [`(lambda (,x) ,b) #:when (symbol? x) `(lambda ,(lex b (cons x acc)))]
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))]
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      ;[`(,rator ,rand) `(,(lex rator acc) ,(lex rand acc))]
      [`,n #:when (number? n) `(const ,n)]
      ;[`(zero? ,x) `(zero? ,(lex x acc))]
      ;[`(sub1 ,x) `(sub1 ,(lex x acc))]
      ;[`(* ,x ,y) `(* ,(lex x acc) ,(lex y acc))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(if ,a ,b ,c) `(if ,(lex a acc) ,(lex b acc) ,(lex c acc))]
      [`(let ([,id ,e]) ,body) `(let  ,(lex e acc) ,(lex body (cons id acc)))]
      [`(let/cc ,k ,e) (let ([v (lex e acc)]) `(letcc ,v))]
      [`(throw ,k ,e) (let ([v (cons k acc)])
                        `(throw ,(lex k v) ,(lex e v)))])))

;Q3
(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(letcc ,body) (let/cc k
                         (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
      [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,expr) (env expr)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))
 
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))
