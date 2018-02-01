#lang racket

(require racket/trace)

;1 value-of representation-dependent
(define value-of
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,i #:when (integer? i) i]
      [`,y #:when (symbol? y) (unbox (env y))]
      [`(zero? ,z) (zero? (value-of z env))]
      [`(begin2 ,first ,second) (begin (value-of first env)
                                       (value-of second env))]
      [`(sub1 ,s) (sub1 (value-of s env))]
      [`(* ,a ,b) (* (value-of a env) (value-of b env))]
      [`(lambda (,x) ,body)
       (lambda (arg)
         (value-of body (lambda (y) (if (eqv? y x) arg (env y)))))]
      [`(if ,condition ,then ,else)
       (if (value-of condition env)
           (value-of then env)
           (value-of else env))]
      [`(let ([,x ,e]) ,body)
       (let ([value (box (value-of e env))])
         ;calculate the expression first!
         ;Because the body can have some set expression which can mutate the binding.
         (value-of body (lambda (y) (if (eqv? y x) value (env y)))))]
      ;in racket, the [] in let expression can be also matched with ()
      ;Q:what if there are multiple binidng...
      [`(set! ,var ,value) (set-box! (env var) (value-of value env)) ]
      [`(,rator ,rand)
       ((value-of rator env) (box (value-of rand env)))])))


(define empty-env
  (lambda ()
    (lambda (y) (error "badness:( unbound" y))))

;2 value-of-fn using higher-order function
(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,i #:when (integer? i) i]
      [`,y #:when (symbol? y) (apply-env-fn env y)]
      [`(zero? ,z) (zero? (value-of-fn z env))]
      [`(sub1 ,s) (sub1 (value-of-fn s env))]
      [`(* ,a ,b) (* (value-of-fn a env) (value-of-fn b env))]
      [`(lambda (,x) ,body)
       (lambda (arg)
         (value-of-fn body (extend-env-fn env x arg)))]
      
      [`(if ,condition ,then ,else)
       (if (value-of-fn condition env)
           (value-of-fn then env)
           (value-of-fn else env))]
      [`(let ([,x ,e]) ,body)
       (value-of-fn body (extend-env-fn env x (value-of-fn e env)))]
      [`(,rator ,rand)
       ((value-of-fn rator env) (value-of-fn rand env))])))

(define empty-env-fn
  (lambda ()
    (lambda (y) (error "badness:( unbound" y))))

(define apply-env-fn
  (lambda (env y)
    (env y)))
; well... this is just the same as in Q1...
; we make a direct function into a procedure call

(define extend-env-fn
  (lambda (env x value)
    (lambda (y) (if (eqv? y x) value (env y)))))

;3 value-of-ds using data structure
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
       (lambda (arg)
         (value-of-ds body (extend-env-ds env x arg)))]
      
      [`(if ,condition ,then ,else)
       (if (value-of-ds condition env)
           (value-of-ds then env)
           (value-of-ds else env))]
      [`(let ([,x ,e]) ,body)
       (value-of-ds body (extend-env-ds env x (value-of-ds e env)))]
      ;we still allow higher-order function so that we can pass a function
      [`(,rator ,rand)
       ((value-of-ds rator env) (value-of-ds rand env))])))

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

;Q4 fo-eulav nothing special
(define fo-eulav
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,i #:when (integer? i) i]
      [`,y #:when (symbol? y) (env y)]
      [`(,z ?orez) (zero? (fo-eulav z env))]
      [`(,s 1bus) (sub1 (fo-eulav s env))]
      [`(,a ,b *) (* (fo-eulav a env) (fo-eulav b env))]
      [`(,body (,x) adbmal)
       (lambda (arg)
         (fo-eulav body (lambda (y) (if (eqv? y x) arg (env y)))))]
      [`(,else ,then ,condition fi)
       (if (fo-eulav condition env)
           (fo-eulav then env)
           (fo-eulav else env))]
      [`(,body ([,e ,x]) tel)
       (fo-eulav body (lambda (y) (if (eqv? y x) (fo-eulav e env) (env y))))]
      ;in racket, the [] in let expression can be also matched with ()
      ;Q:what if there are multiple binidng...
      [`(,rand ,rator)
       ((fo-eulav rator env) (fo-eulav rand env))])))

;Q6 value-of-lex
(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))

(define (apply-env-lex env num)
  (cond
    [(eqv? num 0) (car env)]
    [else (apply-env-lex (cdr env) (sub1 num))]))

(define extend-env-lex cons)

;Q7 Church sub1
(define c0 (lambda (f) (lambda (x) x)))
(define c1 (lambda (f) (lambda (x) (f x))))
(define c2 (lambda (f) (lambda (x) (f (f x)))))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))
(define c+ (lambda (m) 
             (lambda (n) 
               (lambda (a) (lambda (b) ((m a) ((n a) b)))))))


; pred = \n . \f . \x . n (\g . \h . h (g f)) (\u . x) (\u . u)
; too hard... this is given from the wikipedia page

(define csub1
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (((n (lambda (g)
               (lambda (h)
                 (h (g f)))))
          (lambda (u) x))
         (lambda (u) u))))))


