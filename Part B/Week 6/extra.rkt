#lang racket

(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

(define (tree-height bt) (if (btree-leaf? bt)
                             0
                             (let ([llen (+ (tree-height (btree-node-left)) 1)]
                                   [rlen (+ (tree-height (btree-node-right)) 1)])
                               (if (< llen rlen)
                                   rlen
                                   llen))))