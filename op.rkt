#lang racket

(require eopl/eopl)
(require "ast.rkt")
(require "semantic-domains.rkt")
(require rackunit)

(provide
	operators
	op-name
	op-prim
	op-sig
	op-find
	)

;;; nonzero? : any/c -> boolean?
(define nonzero?
  (lambda (n)
    (and (number? n)
      (not (zero? n)))))

(define-struct op (name prim sig))

(define  +op    (make-op '+     +     (list number? number? number?)))
(define  -op    (make-op '-     -     (list number? number? number?)))
(define  *op    (make-op '*     *     (list number? number? number?)))
(define  /op    (make-op '/     /     (list number? number? nonzero?)))
(define  <op    (make-op '<     <     (list boolean? number? number?)))
(define  <=op   (make-op '<=    <=    (list boolean? number? number?)))
(define  =op    (make-op '=     =     (list boolean? number? number?)))
(define  >op    (make-op '>     >     (list boolean? number? number?)))
(define  >=op   (make-op '>=    >=    (list boolean? number? number?)))
(define  notop  (make-op 'not   not   (list boolean? expressible-value?)))
(define  eq?op  (make-op 'eq?   eq?   (list boolean? expressible-value? expressible-value?)))
(define  0?op   (make-op '0?    zero? (list boolean? number?)))

(define operators
  (list +op -op *op /op <op <=op =op >op >=op notop eq?op 0?op))

(define op-find
  (lambda (opsym)
    (findf (lambda (op)
             (eq? opsym (op-name op)))
           operators)))