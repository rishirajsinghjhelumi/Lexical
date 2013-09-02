#lang racket

(require "ast.rkt")
(require eopl/eopl)

(provide
    env
    env?
    empty-env
    extended-env
    lookup-env
    )

(define denotable-value?
	(lambda (v)
    	(or
      		(number? v)
      		(boolean? v)
     )))

(define-datatype env env?
	[empty-env ]
	[extended-env 
		(syms (list-of symbol?)) 
		(vals (list-of denotable-value?)) 
		(outer-env env?) ]
	)

(define list-index
	(lambda (ls a)
		(letrec ([loop
               	(lambda (ls ans)
                	(cond
                   		[(null? ls) -1]
                   		[(eq? (car ls) a) ans]
                   		[#t (loop (cdr ls) (+ 1 ans))]))])
      		(loop ls 0))))

(define lookup-env
	(lambda (e x)
		(cases env e
			[empty-env ()
				(error 'lookup-env "unbound identifier ~a" x) ]
			[extended-env (syms vals outer-env)
        		(let ([j (list-index syms x)])
          			(cond
           				[(= j -1) (lookup-env outer-env x)]
            			[#t (list-ref vals j)]
            		)) ]
		)))
