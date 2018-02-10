#lang racket

;Q1
(define lex
  (lambda (exp acc)
    (match exp
      [`,y #:when (symbol? y) (if (memv y acc)
                                  `(var ,(- (length acc) (length (memv y acc))))
                                  y)]
      [`(lambda (,x) ,b) #:when (symbol? x) `(lambda ,(lex b (cons x acc)))]
      [`(,rator ,rand) `(,(lex rator acc) ,(lex rand acc))]
      [`,n #:when (number? n) `(const ,n)]
      ;[`(zero? ,x) `(zero? ,(lex x acc))]
      ;[`(sub1 ,x) `(sub1 ,(lex x acc))]
      [`(* ,x ,y) `(* ,(lex x acc) ,(lex y acc))]
      [`(if ,a ,b ,c) `(if ,(lex a acc) ,(lex b acc) ,(lex c acc))]
      [`(let ([,id ,e]) ,body) `(let  ,(lex e acc) ,(lex body (cons id acc)))]
      )))


;Q2
;value-of-fn using higher-order function
(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,i #:when (integer? i) i]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(zero? ,z) (zero? (value-of-fn z env))]
      [`(sub1 ,s) (sub1 (value-of-fn s env))]
      [`(* ,a ,b) (* (value-of-fn a env) (value-of-fn b env))]
      [`(lambda (,x) ,body)

       (closure-fn body env x)
       #;(value-of-fn body (extend-env env x arg))]
      
      [`(if ,condition ,then ,else)
       (if (value-of-fn condition env)
           (value-of-fn then env)
           (value-of-fn else env))]
      
      [`(let ([,x ,e]) ,body)
       (apply-closure-fn (closure-fn body env x) (value-of-fn e env))
       #;(value-of-fn body (extend-env env x (value-of-fn e env)))]
      
      [`(,rator ,rand)
       (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))])))

(define (apply-closure-fn clos a)
  (clos a))

(define (closure-fn b env x)
  (lambda (arg)
    (value-of-fn b (extend-env env x arg))))

(define empty-env
  (lambda ()
    (lambda (y) (error "badness:( unbound" y))))

(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (env x value)
    (lambda (y) (if (eqv? y x) value (env y)))))



;value-of-ds using data structure
(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,i #:when (integer? i) i]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`(zero? ,z) (zero? (value-of-ds z env))]
      [`(sub1 ,s) (sub1 (value-of-ds s env))]
      [`(* ,a ,b) (* (value-of-ds a env) (value-of-ds b env))]
      [`(lambda (,x) ,body)
       (closure-ds body env x)]
      
      #;(lambda (arg)
          (value-of-ds body (extend-env-ds env x arg)))
      
      [`(if ,condition ,then ,else)
       (if (value-of-ds condition env)
           (value-of-ds then env)
           (value-of-ds else env))]
      
      [`(let ([,x ,e]) ,body)
       (apply-closure-ds (closure-ds body env x) (value-of-ds e env))
       #;(value-of-ds body (extend-env-ds env x (value-of-ds e env)))]

      ;we still allow higher-order function so that we can pass a function
      [`(,rator ,rand)
       (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))])))

(define (apply-closure-ds clos a)
  (match clos
    [`(clos ,b ,env ,x) (value-of-ds b (extend-env-ds env x a))]))

(define (closure-ds b env x)
  `(clos ,b ,env ,x))

(define empty-env-ds
  (lambda()
    '()))

(define apply-env-ds
  (lambda (env y)
    (match env
      ['() (error "empty environment")]
      [`(ext-env ,x ,a ,env)
       (if (eqv? x y) a (apply-env-ds env y))])))

(define extend-env-ds
  (lambda (env x value)
    `(ext-env ,x ,value ,env)))



;Q3