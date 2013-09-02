#lang racket

(require rackunit "syntax-expander.rkt")

(test-case "syntax-expander"
           (check-equal? (syntax-expander '(cond [#f 10] [#t 5] [else 10]))
                                          (if-ast (boolean #f) 
                                                  (number 10) 
                                                  (if-ast (boolean #t)  (number 5) (number 10))))
           
           (check-equal? (syntax-expander '(cond [(if #f #f #t) 10 ] [(+ 3 4) 5] [else 10]))
                                          (if-ast
                                           (if-ast (boolean #f) (boolean #f) (boolean #t))
                                           (number 10)
                                           (if-ast
                                            (prim-app
                                             '+
                                             (list (number 3) (number 4)))
                                            (number 5)
                                            (number 10))) ))