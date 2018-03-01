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
      
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      ;[`(,rator ,rand) `(,(lex rator acc) ,(lex rand acc))]
      [`,n #:when (number? n) `(const ,n)]
      ;[`(zero? ,x) `(zero? ,(lex x acc))]
      [`(sub1 ,x) `(sub1 ,(lex x acc))]
      ;[`(* ,x ,y) `(* ,(lex x acc) ,(lex y acc))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(if ,a ,b ,c) `(if ,(lex a acc) ,(lex b acc) ,(lex c acc))]
      [`(let ([,id ,e]) ,body) `(let  ,(lex e acc) ,(lex body (cons id acc)))]
      [`(let/cc ,k ,e) (let ([v (lex e acc)]) `(letcc ,v))]
      [`(throw ,k ,e) (let ([v (cons k acc)])
                        `(throw ,(lex k v) ,(lex e v)))]
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))])))

;Q3
(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env (outer-mult x2 env k))]
      [`(sub1 ,x) (value-of-cps x env (outer-sub1 k))]
      [`(zero ,x) (value-of-cps x env (outer-zero k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (outer-if k conseq alt env))]
      [`(letcc ,body) (extend-env env k (outer-letcc body k))]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env (outer-throw v-exp env))]
      [`(let ,e ,body) (value-of-cps e env (outer-let env body k))]
      [`(var ,expr) (apply-env env expr k)]
      [`(lambda ,body) (make-closure env body k)]
      [`(app ,rator ,rand) (value-of-cps rator env (outer-app rand env k))])))

(define inner-mult ;(inner-mult k v1)
  (lambda (k v1) `(inner-mult ,k ,v1)))

(define outer-mult ;(outer-mult x2 env k)
  (lambda (x2 env k) `(outer-mult ,x2 ,env ,k)))

(define outer-sub1 ;(outer-sub1 k)
  (lambda (k) `(outer-sub1 ,k)))

(define outer-zero ;(outer-zero k)
  (lambda (k) `(outer-zero ,k)))

(define outer-if ;(outer-if k conseq alt env)
  (lambda (k conseq alt env) `(outer-if ,k ,conseq ,alt ,env)))

(define outer-letcc ;(outer-letcc body k)
  (lambda (body k) `(outer-letcc ,body ,k)))


(define outer-throw ;(outer-throw v-exp env)
  (lambda (v-exp env) `(outer-throw ,v-exp ,env)))


(define inner-let ;(inner-let body k)
  (lambda (body^ k^) `(inner-let ,body^ ,k^)))


(define outer-let ;(outer-let env body k)
  (lambda (env body k) `(outer-let ,env ,body ,k)))


(define inner-app ;(inner-app v1 k)
  (lambda (v1 k) `(inner-app ,v1 ,k)))


(define outer-app ;(outer-app rand env k)
  (lambda (rand env k) `(outer-app ,rand ,env ,k)))



(define apply-k
  (lambda (k v)
    (match k
      [`(inner-mult ,k ,v1) (apply-k k (* v1 v))]
      [`(outer-mult ,x2 ,env ,k) (value-of-cps x2 env (inner-mult k v))]
      [`(outer-sub1 ,k) (apply-k k (sub1 v))]
      [`(outer-zero ,k) (apply-k k (zero? v))]
      [`(outer-if ,k ,conseq ,alt ,env) (if v
                      (value-of-cps conseq env k)
                      (value-of-cps alt env k))]
      [`(outer-letcc ,body ,k) (value-of-cps body v k)]
      [`(outer-throw ,v-exp ,env) (value-of-cps v-exp env v)]
      [`(inner-let ,body^ ,k^) (value-of-cps body^ v k^)]
      [`(outer-let ,env ,body ,k) (extend-env env v (inner-let body k))]
      [`(inner-app ,v1 ,k)  (apply-closure v1 v k)]
      [`(outer-app ,rand ,env ,k) (value-of-cps rand env (inner-app v k))]
      [`(outer-closure ,body ,k^) (value-of-cps body v k^)]
      [`(empty) v])))

(define make-closure
  (lambda (env body k)
    (apply-k k `(cls ,env ,body))))


(define apply-closure
  (lambda (v1 a k^)
    (match v1
      [`(cls ,env ,body) (extend-env env a (outer-closure body k^))])))

(define outer-closure
  (lambda (body k^) `(outer-closure ,body ,k^)))


(define extend-env
  (lambda (env a k)
    (apply-k k `(var ,env ,a))))

(define apply-env
  (lambda (env y k)
    (match env
      [`(var ,env ,a) (if (zero? y) (apply-k k a)
                          (apply-env env (sub1 y) k))])))


(define empty-env
  (lambda ()
    (lambda (y k^)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda () `(empty)))


;Q4

(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))
 
(define car$ car)
 
(define cdr$
  (lambda ($) (force (cdr $))))

(define inf-1s (cons$ 1 inf-1s))

(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) 
              (let ((n- (sub1 n)))
                (cond
                  ((zero? n-) '())
                  (else (take$ n- (cdr$ $))))))))))

(define 3sum
  (lambda (l1 l2 l3)
    (cons$ (+ (car$ l1) (car$ l2) (car$ l3)) (3sum (cdr$ l1)(cdr$ l2)(cdr$ l3)))))

(define trib$
  (cons$ 0 (cons$ 1 (cons$ 1 (3sum trib$ (cdr$ trib$) (cdr$ (cdr$ trib$)))))))
  
