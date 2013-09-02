#lang racket

(require rackunit "run.rkt")


(test-case "run"
           (check-equal? (run '(if #t 3 4)) 3)
           (check-equal? (run '(if (- 5 5) (+ (+ 3 4) 5) (if #t 3 5))) 12)
           (check-equal? (run '(cond [#t 10 ] [(+ 1 1) #t] [else 5] )) 10)
           (check-equal? (run '(cond [ (if #f -10 0) 101] [(+ 1 (- 1 2)) 10] [else 100])) 101)
           (check-equal? (run '(assume [(x 10) (y 5)] (+ x y))) 15)
           (check-equal? (run '(assume [(x (+ 10 (+ 5 9))) (y 0) (z (* 5  5)) ] (+ x (* z (+ y 10))))) 274))
