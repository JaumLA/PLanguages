#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

(define (palindromic xs)
  (if (null? xs)
      '()
      (letrec ([endindex (- (length xs) 1)]
               [f (lambda (start end)
                    (if (< end 0)
                        null
                        (cons (+ (list-ref xs start) (list-ref xs end))
                              (f (+ start 1) (- end 1)))))])
        (f 0 endindex))))

(define (fibonacci) (letrec ([f (lambda (actual next)
                                  (cons actual
                                        (lambda () (f next (+ actual next)))))])
                      (f 0 1)))

(define (stream-until f s) (if (eq? (f (car (s))) #t)
                               null
                               (cons (f (car (s))) (stream-until f (cdr (s))))))

(define (stream-map f s) (lambda () (cons (f (car (s)))
                               (stream-map f (cdr (s))))))

(define (stream-zip s1 s2) (lambda ()
                             (cons (cons (car (s1)) (car (s2)))
                                   (stream-zip (cdr (s1)) (cdr (s2))))))

(define (interleave streamlist) (letrec ([len (length streamlist)]
                                         [createnext (lambda (ls) (if (null? ls)
                                                                      null
                                                                      (cons (cdr ((car ls))) (cdr ls))))]
                                         [traverse (lambda (index)
                                                     (if (= index len)
                                                         (interleave (createnext streamlist))
                                                         (lambda () (cons (car ((list-ref streamlist index)))
                                                               (traverse (+ index 1))))))])
                                  (traverse 0)))

(define (pack n s) (letrec ([n-elements (lambda (i actuals)
                                         (if (<= i 0)
                                             null
                                             (cons (car (actuals))
                                                   (n-elements (- i 1) (cdr (actuals))))))])
                     (lambda () (cons (n-elements n s) (pack n (cdr (s)))))))

(define (sqrt-stream n) (lambda ()
                          (lambda (x) (letrec ([guess (/ (+ x (/ n x)) 2)])
                                        (cons guess (sqrt-stream guess))))))

(define (approx-sqrt n e) )


