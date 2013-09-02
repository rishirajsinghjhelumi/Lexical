#lang racket

(require eopl/eopl)
(require "ast.rkt")
(require "semantic-domains.rkt")
(require "op.rkt")
(require "env.rkt")
(require "syntax-expander.rkt")

(provide
	eval-ast
	)

(define match-sig?
 	(lambda (sig? val)
    	(sig? val)))

;;; eval-ast : ast? -> expressible-value?
;;; eval-ast :  throws error

(define eval-ast
	(lambda (exp env)
		(cases ast exp
			[number (datum) 
				datum ]
			[boolean (datum) 
				datum ]
			[id (sym) 
				(lookup-env env sym) ]
			[prim-app (op rands) 
          		(eval-prim-app op (map (lambda(a) (eval-ast a env)) rands)) ]
			[if-ast (test-ast then-ast else-ast) 
				(eval-my-if exp env) ]
			[my-cond (ast-ast-binds else-bind) 
				(eval-ast (syntax-expander exp) env) ]
			[assume (id-ast-binds body)
				(eval-assume exp env) ]
			[else
				(error 'eval-ast-error "evaluation error") ]
		)))

(define eval-prim-app
	(lambda (opsym args)
		(let* (
    			[op (op-find opsym)]
         		[sig (op-sig op)]
           		[args-sig (rest sig)]
           		)
    	(cond
       		[(and (= (length args-sig) (length args)) (andmap match-sig? args-sig args) )
        		(apply (op-prim op) args) ]
       		[#t 
       			(error 'eval-prim-app-error "evaluation error") ]
    	))))

(define eval-my-if
	(lambda (exp env)
		(cases ast exp
			[if-ast (test-ast then-ast else-ast) 
				(let ([test-value (eval-ast test-ast env)])
            		(eval-ast (if test-value then-ast else-ast) env)
            	)]
			[else
				(error 'eval-my-if-error "evaluation error") ]
		)))

(define eval-assume
	(lambda (exp env)
		(cases ast exp
			[assume (id-ast-binds body)
				(let* ( [ids (map bind-id id-ast-binds)]
						[asts (map bind-ast id-ast-binds)]
						[vals (map (lambda (a) (eval-ast a env)) asts)]
						[new-env (extended-env ids vals env)] 
						)
					(eval-ast body new-env)
				) ]
			[else
				(error 'eval-assume-error "evaluation error")]
		)))