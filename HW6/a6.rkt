#lang racket
(require racket/trace)

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))

;Q1
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n)
                                   (lambda (v)
                                     (k (+ (car n) (* 2 v)))))])))

;(binary-to-decimal-cps '(1 0 0 0 1) (empty-k))

;Q2
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls)
                       (lambda (v) (k (* (car ls) v))))])))

;Q3
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps-shortcut (cdr ls)
                                (lambda (v) (k (* (car ls) v))))])))

;Q4
(define plus-cps
  (lambda (m k)
    (k (lambda (n k)
         (k (+ m n))))))

;Q5
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (cond
         [(remv-first-9*-cps (car ls) (lambda (v) (equal? v (car ls))))
          (remv-first-9*-cps (cdr ls) (lambda (v) (k (cons (car ls) v))))]
         [else
          (remv-first-9*-cps (car ls) (lambda (v) (k (cons v (cdr ls)))))])]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (v) (k (cons (car ls) v))))])))

;Q6
#;
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps
        (cdr ls)
        (lambda (v1)
          (cons-cell-count-cps (car ls)
                               (lambda (v2)
                                 (k (add1 (+ v1 v2)))))))]
      [else (k 0)])))


(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (add1
        (k (cons-cell-count-cps (car ls)
                                (lambda (v)
                                  (cons-cell-count-cps (cdr ls)
                                                       (lambda (v2) (+ v v2)))))))]
      [else (k 0)])))


;Q7
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s k) u))))
; Well, it seems no differece whether apply k to u.

;Q8
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v) (ack-cps (sub1 m) v k)))])))

;Q9
(define fib-cps
  (lambda (n k)
    ((lambda (fib1 k1)
       (fib1 fib1 n k1))
     (lambda (fib2 n k2)
       (cond
         [(zero? n) (k2 0)]
         [(zero? (sub1 n)) (k2 1)]
         [else (fib2 fib2 (sub1 n)
                     (lambda (v)
                       (fib2 fib2 (sub1 (sub1 n))
                             (lambda (v2) (k2 (+ v v2))))))])) k)))

;Q10
(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
         (if (p seed)
             ans
             ((h h) (g seed) (cons (f seed) ans))))))))

(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k)
       (h h (lambda (h-fun) (h-fun seed '() k))))
     (lambda (h k)
       (k (lambda (seed ans k)
            (p seed
               (lambda (if-pred)
                 (if if-pred
                     (k ans)
                     (h h (lambda (h-fun)
                            (g seed (lambda (g-v)
                                      (f seed (lambda (f-v)
                                                (h-fun g-v (cons f-v ans) k)))))))))))))k)))

(define null?-cps
  (lambda (ls k)
    (k (null? ls))))

(define car-cps
  (lambda (pr k)
    (k (car pr))))

(define cdr-cps
  (lambda (pr k)
    (k (cdr pr))))

;(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))

;(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (lambda (v) v))

;Q11
(define empty-s
  (lambda ()
    '()))

(define find 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))
 
(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
       (if (pair? v)
           (unify-cps (find (car u) s) (find (car v) s) s
                      (lambda (v2) (k (if v2
                                          (unify-cps (find (cdr u) v2) (find (cdr v) v2) v2 k)
                                          #f))))   
           (k #f)))
      (else (k #f)))))


;Q12
(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))

(define M-cps
  (lambda (f k1)
    (k1 (lambda (ls k2)
          (cond
            ((null? ls) (k2 '()))
            (else (M-cps f (lambda (fun)
                             (f (car ls)
                                (lambda (v)
                                  (fun (cdr ls) (lambda (d) (k2 (cons v d))))
                                  ))))))))))

;Q13
(define use-of-M-cps
  (M-cps (lambda (n k) (k (add1 n))) (lambda (f) (f '(1 2 3 4 5) (empty-k)))))

;Q14
(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))

(define strange-cps
  (lambda (x k)
    ((lambda (g k) (k (lambda (x k) (g g k))))
     (lambda (g k) (k (lambda (x k) (g g k)))) k)))

;Q15
(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))

(define use-of-strange-cps
  (let ([strange^ (((strange-cps 5 (empty-k)) 6 (empty-k)) 7 (empty-k))])
    (((strange^ 8 (empty-k)) 9 (empty-k)) 10 (empty-k))))

;Q16
(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

(define why-cps
  (lambda (f k)
    ((lambda (g k)
       (f (lambda (x k) (g g (lambda (g-fun) (g-fun x k))))k))
     (lambda (g k)
       (f (lambda (x k) (g g (lambda (g-fun) (g-fun x k))))k))k)))

(define almost-length
  (lambda (f)
    (lambda (ls)
      (if (null? ls)
          0
          (add1 (f (cdr ls)))))))

(define almost-length-cps
  (lambda (f k)
    (k (lambda (ls k)
         (if (null? ls)
             (k 0)
             (f (cdr ls) (lambda (v) (k (add1 v)))))))))

;((why-cps almost-length-cps (empty-k)) '(a b c d e) (empty-k))
