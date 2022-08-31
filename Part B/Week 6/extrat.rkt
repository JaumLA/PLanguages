#lang racket
;; Programming Languages Homework 5 Extra Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "extra.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Extra 5"
   
   ;; check height of a binary tree
   (check-equal? (tree-height (btree-leaf)) 0 "tree-height test")
   
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)