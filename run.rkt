#lang racket

;;; run : any -> expressible-value?
;;; run : throws parse error if input does not parse.
;;; run : throws apply-prim-op error if prim operator is
;;;       not given the right # or types of arguments.

(provide
	run
	)

(require "parser.rkt")
(require "eval-ast.rkt")
(require "env.rkt")
(require rackunit)

(define run
  (lambda (e)
    (eval-ast (parse e) (empty-env))))