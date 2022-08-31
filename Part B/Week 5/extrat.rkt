#lang racket
;; Programming Languages Homework 5 Extra Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "extra.rkt")

(require rackunit)

(define ones (lambda () (cons 1 (lambda () (ones)))))

(define (stream-pos s pos) (if (<= pos 1)
                               (car (s))
                               (stream-pos (cdr (s)) (- pos 1))))

(define tests
  (test-suite
   "Sample tests for Extra 5"
   
   ;; check palindromic
   (check-equal? (palindromic (list 1 2 3 4 5)) (list 6 6 6 6 6) "palindromic test")
   (check-equal? (palindromic (list 1 2 4 8)) (list 9 6 6 9) "palindromic test")

   ;; check fibonacci stream
   (check-equal? (car (fibonacci)) 0 "fibonacci test")
   (check-equal? (car ((cdr (fibonacci)))) 1 "fibonacci test")
   (check-equal? (car ((cdr ((cdr (fibonacci)))))) 1 "fibonacci test")
   (check-equal? (car ((cdr ((cdr ((cdr (fibonacci)))))))) 2 "fibonacci test")
   (check-equal? (car ((cdr ((cdr ((cdr ((cdr (fibonacci)))))))))) 3 "fibonacci test")

   ;; check stream until
   (check-equal? (stream-until (lambda (x) (if (= x 3) true (+ x 10))) fibonacci) (list 10 11 11 12) "stream-until test")

   ;; check stream map
   (check-equal? (stream-pos (stream-map (lambda (x) (+ x 10)) fibonacci) 1) 10 "stream-map test")
   (check-equal? (stream-pos (stream-map (lambda (x) (+ x 10)) fibonacci) 2) 11 "stream-map test")
   
   ;; check stream zip
   (check-equal? (stream-pos (stream-zip fibonacci ones) 1) (cons 0 1) "stream-zip test")
   (check-equal? (stream-pos (stream-zip fibonacci ones) 4) (cons 2 1) "stream-zip test")

   ;; check interleave
   (check-equal? (stream-pos (interleave (list fibonacci ones)) 1) 0 "interleave test")
   (check-equal? (stream-pos (interleave (list fibonacci ones)) 2) 1 "interleave test")
   (check-equal? (stream-pos (interleave (list fibonacci ones)) 3) 1 "interleave test")
   (check-equal? (stream-pos (interleave (list fibonacci ones)) 4) 1 "interleave test")
   (check-equal? (stream-pos (interleave (list fibonacci ones)) 7) 2 "interleave test")
   (check-equal? (stream-pos (interleave (list fibonacci ones)) 9) 3 "interleave test")

   ;; check pack
   (check-equal? (stream-pos (pack 3 fibonacci) 1) (list 0 1 1) "pack test")
   (check-equal? (stream-pos (pack 3 fibonacci) 2) (list 1 1 2) "pack test")
   (check-equal? (stream-pos (pack 3 fibonacci) 3) (list 1 2 3) "pack test")
   (check-equal? (stream-pos (pack 3 fibonacci) 4) (list 2 3 5) "pack test")
   
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)