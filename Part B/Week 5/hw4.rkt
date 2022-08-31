
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
          (cond [(> low high) null]
                [(= low high) (cons low null)]
                [#t (cons low (sequence (+ low stride) high stride))]))

(define (string-append-map strList suffix)
  (map (lambda (str)
         (string-append str suffix)) strList))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps st n)
  (if (= n 0)
      null
      (cons (car (st))
            (stream-for-n-steps (cdr (st)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons
                           (if (= (remainder x 5) 0)
                               (- 0 x)
                               x)
                           (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons x
                                (if (string=? x "dan.jpg")
                                    (lambda () (f "dog.jpg"))
                                    (lambda () (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))
    
(define (stream-add-zero s)
  (lambda ()
    (let ([next (s)])
      (cons (cons 0 (car next)) (stream-add-zero (cdr next))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons
                                 (list-nth-mod xs n)
                                 (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (n) (cond [(= n len) #f]
                                [(not (pair? (vector-ref vec n))) (f (+ n 1)) ]
                                [(equal? v (car (vector-ref vec n))) (vector-ref vec n)]
                                [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [index 0]
           [f (lambda (v) (let ([possible-anws (vector-assoc v cache)])
                            (if possible-anws
                                possible-anws
                                (let ([answ (assoc v xs)])
                                      (begin (vector-set! cache index answ)
                                             (if (= index (vector-length cache))
                                                 (set! index 0)
                                                 (set! index (+ index 1)))
                                             answ)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([e1-value e1]
              [e2-values (lambda () (cons e2 e2-values))]
              [evaluate (lambda (e2-val) (cond [(> e1 (car (e2-val))) (evaluate (cdr (e2-val)))]
                                               [#t ]))])
       (evaluate e2-values))]))




