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
(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env (lambda (v1)
                                              (value-of-cps x2 env
                                                            (lambda (v2) (apply-k k (* v1 v2))))))]
      [`(sub1 ,x) (value-of-cps x env (lambda (v) (apply-k k (sub1 v))))]
      [`(zero ,x) (value-of-cps x env (lambda (v) (apply-k k (zero? v))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (pred)
                                                         (apply-k k (if pred
                                                                        (value-of-cps conseq env k)
                                                                        (value-of-cps alt env k)))))]
      [`(letcc ,body) (extend-env env k (lambda (env) (value-of-cps body env k)))]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env
                                            (lambda (v) (value-of-cps v-exp env
                                                                      (lambda (v2)(v v2)))))]
      [`(let ,e ,body) (value-of-cps e env
                                     (lambda (a) (extend-env env a
                                                             (lambda (env) (value-of-cps body env k)))))]
      [`(var ,expr) (apply-env env expr k)]
      [`(lambda ,body) (apply-k k (lambda (a k^)  (extend-env env a (lambda (env) (value-of-cps body env k^)))))]
      [`(app ,rator ,rand) (value-of-cps rator env
                                         (lambda (v1)(value-of-cps rand env
                                                                   (lambda (v2)
                                                                     (apply-closure v1 v2 k)))))])))

(define extend-env
  (lambda (env a k)
    (apply-k k `(var ,env ,a))))

(define apply-closure
  (lambda (v1 v2 k) (v1 v2 k)))

(define apply-env
  (lambda (env y k)
    (match env
      [`(var ,env ,a) (if (zero? y) (apply-k k a)
                          (apply-env (sub1 y) k))])))

(define apply-k
  (lambda (k expr) (k expr)))

(define empty-env
  (lambda ()
    (lambda (y k^)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))
