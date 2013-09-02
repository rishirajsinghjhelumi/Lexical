#lang racket

(require "ast.rkt")
(require rackunit)

(provide
	parse
	parse-my-cond
	parse-my-if
	parse-assume
	)

;;; parse : any/c -> ast?
;;; parse : throws parse-error when input
;;;         does not parse.


(define parse
	(lambda (input)
		(cond 
			[(number? input) 
				(number input) ]
			[(boolean? input) 
				(boolean input) ]
			[(and (symbol? input) (not (memq input keywords)) ) 
				(id input) ]
			[(and (list? input) (not (null? input)) (memq (first input) op-symbols) ) 
				(prim-app (first input) (map parse (rest input)) ) ]
			[(and (list? input) (not (null? input)) (memq (first input) keywords) )
				(cond 
					[(equal? (first input) 'if)
						(parse-my-if input) ]
					[(equal? (first input) 'cond)
						(syntax-expander input) ]
					[(equal? (first input) 'assume)
						(parse-assume input) ]
				)]
			[else 
				(parse-error input) ]
		)))


(define parse-my-if
	(lambda (input)
		(cond
			[(equal? (length input) 4) 
				(if-ast (parse (second input)) (parse (third input)) (parse (fourth input)) ) ]
			[else
				(error 'parse-error-my-if "invalid my-if syntax ~a" input) ]
		)))

(define parse-my-cond
	(lambda (input)
		(cond
			[(and (equal? (first (last input)) 'else) (andmap check-ast-ast-bind (all-but-last (rest input))) )
				(my-cond (map parse-ast-ast-bind (all-but-last (rest input))) (my-else (parse (second (last input)))) ) ]
			[else
				(error 'parse-error-my-cond "invalid my-cond syntax ~a" input) ]
		)))

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

(define parse-assume
	(lambda (input)
		(cond
			[(and (equal? (length input) 3) (andmap check-id-ast-bind (second input)) )
				(assume (map parse-id-ast-bind (second input)) (parse (third input)) ) ]
			[else
				(error 'parse-error-assume "invalid assume syntax ~a" input) ]
		)))

(define check-id-ast-bind
	(lambda (bind-pair)
		(and 
			(list? bind-pair)
			(equal? (length bind-pair) 2)
			(symbol? (first bind-pair) )
		)))

(define parse-id-ast-bind
	(lambda (bind-pair)
		(make-id-ast-bind (first bind-pair) (parse (second bind-pair))
		)))

(define check-ast-ast-bind
	(lambda (bind-pair)
		(and 
			(list? bind-pair)
			(equal? (length bind-pair) 2) 
		)))

(define parse-ast-ast-bind
	(lambda (bind-pair)
		(make-ast-ast-bind (parse (first bind-pair)) (parse (second bind-pair))
		)))

(define parse-error
  (lambda (input)
    (error 'parse-error "invalid syntax ~a" input)))

(define all-but-last
	(lambda (l) 
		(reverse (cdr (reverse l)))))

(define unique-list?
	(lambda (l)
  		(or (null? l)
    	  (and (not (member (first l) (rest l)))
        	   (unique-list? (rest l))))))
