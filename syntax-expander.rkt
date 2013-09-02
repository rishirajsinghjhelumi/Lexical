#lang racket

(require "parser.rkt")
(require "ast.rkt")
(require rackunit)

(provide (all-from-out "ast.rkt"))

(provide
	syntax-expander
	prepend-element
	)

(define syntax-expander
	(lambda (input)
		(cond
			[(and (>= (length input) 2) (equal? (length (second input)) 2) (equal? (first input) 'cond ) )
				(cond
					[(not (equal? (first (second input)) 'else )) 
						(if-ast (parse (first (second input))) (parse (second (second input))) 
						(syntax-expander (prepend-element (rest (rest input)) 'cond)) ) ]
					[(and (equal? (first (second input)) 'else ) (equal? (length (rest input)) 1) )
						(parse (second (second input))) ]
					[else
						(error 'parse-error-syntax-expander "invalid cond syntax ~a" input) ]
				)]
			[else
				(error 'parse-error-syntax-expander "invalid cond syntax ~a" input) ]
		)))

(define prepend-element 
	(lambda (lst elem)
  		(append (list elem) lst)))
