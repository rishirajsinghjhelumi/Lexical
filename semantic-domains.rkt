#lang racket

(require rackunit)

(provide
	expressible-value?
	)

;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  	(lambda (thing)
    	(or 
    		(number? thing)
      		(boolean? thing)
    )))
