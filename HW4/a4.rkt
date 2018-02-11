#lang racket
(require racket/trace)

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



;Q3 dynamic scope
(define value-of-dynamic
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,i #:when (integer? i) i]
      [`,y #:when (symbol? y) (unbox (env y))]
      [`(zero? ,z) (zero? (value-of-dynamic z env))]
      [`(null? ,x) (null? (value-of-dynamic x env))]
      [`(begin2 ,first ,second) (begin (value-of-dynamic first env)
                                       (value-of-dynamic second env))]
      [`(sub1 ,s) (sub1 (value-of-dynamic s env))]
      [`(* ,a ,b) (* (value-of-dynamic a env) (value-of-dynamic b env))]
      [`(lambda (,x) ,body)
       (lambda (arg env)
         (value-of-dynamic body (lambda (y) (if (eqv? y x) arg (env y)))))]
      [`(if ,condition ,then ,else)
       (if (value-of-dynamic condition env)
           (value-of-dynamic then env)
           (value-of-dynamic else env))]
      [`(let ([,x ,e]) ,body)
       (let ([value (box (value-of-dynamic e env))])
         ;calculate the expression first!
         ;Because the body can have some set expression which can mutate the binding.
         (value-of-dynamic body (lambda (y) (if (eqv? y x) value (env y)))))]
      ;in racket, the [] in let expression can be also matched with ()
      ;Q:what if there are multiple binidng...
      [`(set! ,var ,value) (set-box! (env var) (value-of-dynamic value env))]
      [`(quote ,v) v]
      [`(cons ,a ,b) (cons (value-of-dynamic a env) (value-of-dynamic b env))]
      [`(car ,x) (car (value-of-dynamic x env))]
      [`(cdr ,x) (cdr (value-of-dynamic x env))]
      [`(,rator ,rand)
       ((value-of-dynamic rator env) (box (value-of-dynamic rand env)) env)])))


;Q4
(define value-of-ri
  (lambda (empty-env extend-env apply-env closure apply-closure)
    (letrec ([value-of
              (lambda (exp env)
                (match exp
                  [`,n #:when (number? n) n]
                  [`,b #:when (boolean? b) b]
                  [`,i #:when (integer? i) i]
                  [`,y #:when (symbol? y) (apply-env env y)]
                  [`(zero? ,z) (zero? (value-of z env))]
                  [`(sub1 ,s) (sub1 (value-of s env))]
                  [`(* ,a ,b) (* (value-of a env) (value-of b env))]
                  [`(lambda (,x) ,body)
                   ((closure value-of extend-env) body x env)]
                  [`(if ,condition ,then ,else)
                   (if (value-of condition env)
                       (value-of then env)
                       (value-of else env))]
                  [`(let ([,x ,e]) ,body)
      
                   (((closure value-of extend-env) body x env) (value-of e env))
                   ]
                  [`(,rator ,rand)
                   (apply-closure (value-of rator env) (value-of rand env))]))])
      (lambda (exp) (value-of exp empty-env)))))



