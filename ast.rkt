#lang racket

;;; Concrete Syntax for LEXICAL

;;; <Exp>  ::=   <Num> | <Bool> | <Id>  | 
;;;              (<Op> <Exp> <Exp> ...) |
;;;              (my-if <Exp> <Exp> <Exp>) |
;;;              (my-cond [<Exp> <Exp>] ... [else <Exp>]) | 
;;;              (assume ([<Id> <Exp>] ...) Exp)
;;; <Num>  ::=   any scheme real or integer
;;; <Bool> ::=   #t | #f
;;; <Id>   ::=   Any scheme symbol not conflicting with a keyword.
;;; <Op>   ::=   + | - | * | / | <= | < | = | > | >= | not 


(require eopl/eopl)
(require rackunit)

(provide
	keywords
	op-symbols
	op-symbol?
	ast
	ast?
	number
	boolean
	id
	prim-app
	if-ast
	my-cond
	assume
	make-id-ast-bind
	id-ast-bind
	id-ast-bind?
	make-ast-ast-bind
	ast-ast-bind
	ast-ast-bind?
	my-else
	else-ast-bind
	else-ast-bind?
	bind-id
	bind-ast
	)

(define keywords
	'(if cond assume else))

(define op-symbols 
	'(+ - * / <= < = > >= not))

;;; op-symbol? : symbol? -> bool?
(define op-symbol?
	(lambda (symbol)
	    (if (memq symbol op-symbols)
	    	#t
	    #f)))

(define-datatype ast ast?
	[number (datum number?)]
	[boolean (datum boolean?)]
	[id (sym symbol?)]
	[prim-app (op op-symbol?) (rands (list-of ast?))]
	[if-ast (test-ast ast?) (then-ast ast?) (else-ast ast?)]
	[my-cond (ast-ast-binds (list-of ast-ast-bind?)) (else-bind else-ast-bind?)]
	[assume (id-ast-binds (list-of id-ast-bind?)) (body ast?)]
	)


(define-datatype id-ast-bind id-ast-bind?
  	[make-id-ast-bind (id symbol?) (ast1 ast?)]
  	)

(define bind-id
    (lambda (b)
        (cases id-ast-bind b
            [make-id-ast-bind (id ast1) 
            	id ] 
        )))

(define bind-ast
    (lambda (b)
        (cases id-ast-bind b
            [make-id-ast-bind (id ast1) 
            	ast1 ] 
        )))

(define-datatype ast-ast-bind ast-ast-bind?
	[make-ast-ast-bind (ast1 ast?) (ast2 ast?)]
	)

(define-datatype else-ast-bind else-ast-bind?
	[my-else (ast ast?)]
	)
