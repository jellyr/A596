#lang racket

;1 call-by-value
(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(null? ,n) (null? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(quote ()) '()]
      [`(let ([,x ,e]) ,body)
       (let ([value (box (val-of-cbv e env))])
         (val-of-cbv body (lambda (y) (if (eqv? y x) value (env y)))))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv conseq env)
                                    (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(cons ,x ,xs) (cons (val-of-cbv x env) (val-of-cbv xs env))]
      [`(car ,e) (car (val-of-cbv e env))]
      [`(cdr ,e) (cdr (val-of-cbv e env))]
      [`(cons^ ,x ,xs) (cons (λ()(val-of-cbv x env)) (λ()(val-of-cbv xs env)))]
      [`(car^ ,e) ((car (val-of-cbv e env)))]
      [`(cdr^ ,e) ((cdr (val-of-cbv e env)))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(set! ,var ,value) (set-box! (env var) (val-of-cbv value env))]
      [`(lambda (,x) ,body) (make-closure val-of-cbv x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (box (val-of-cbv rand env)))])))

(define empty-env
  (lambda ()
    (lambda (y) (error "Empty environment"))))

(define extend-env
  (lambda (env x value)
    (lambda (y) (if (eqv? y x) value (env y)))))

(define apply-env
  (lambda (env y)
    (env y)))

(define (apply-closure clos a)
  (clos a))

(define make-closure
  (lambda (val-of-cb x b env)
    (lambda (arg)
      (val-of-cb b (extend-env env x arg)))))

;2 call-by-ref
(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                    (val-of-cbr conseq env)
                                    (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      
      [`(set! ,var ,value) #:when (symbol? var)
                           (set-box! (env var) (val-of-cbr value env))]
      [`(lambda (,x) ,body) (make-closure val-of-cbr x body env)]
      [`(,rator ,x) #:when (symbol? x)
                    (apply-closure (val-of-cbr rator env)
                                   (apply-env env x))]
      ; store into the environment once the arg is symbol
      [`(,rator ,rand) #:when (not (symbol? rand))
                       (apply-closure (val-of-cbr rator env)
                                      (box (val-of-cbr rand env)))])))

;3 call-by-name
(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                    (val-of-cbname conseq env)
                                    (val-of-cbname alt env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))]
      [`(lambda (,x) ,body) (make-closure val-of-cbname x body env)]
      [`(,rator ,x) #:when (symbol? x)
                    (apply-closure (val-of-cbname rator env)
                                   (apply-env env x))]
      [`(,rator ,rand) #:when (not (symbol? rand))
                       (apply-closure (val-of-cbname rator env)
                                      (box (lambda() (val-of-cbname rand env))))])))


;4 call-by-need
(define unbox/need
  (lambda (b)
    (let ([val ((unbox b))])
      (set-box! b (lambda () val))
      val)))

(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`,y #:when (symbol? y) (unbox/need (apply-env env y))]
      [`(lambda (,x) ,body) (make-closure val-of-cbneed x body env)]
      [`(,rator ,x) #:when (symbol? x)
                    (apply-closure (val-of-cbneed rator env)
                                   (apply-env env x))]
      [`(,rator ,rand) #:when (not (symbol? rand))
                       (apply-closure (val-of-cbneed rator env)
                                      (box (lambda() (val-of-cbneed rand env))))])))

