;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:         cpstak.sc
;;; Description:  continuation-passing version of TAK
;;; Author:       Will Clinger
;;; Created:      20-Aug-87
;;; Modified:     21-Mar-94 (Qobi)
;;;               31-Mar-98 (Qobi)
;;; Language:     Scheme
;;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.

(define (cpstak x y z)
 (define (tak x y z k)
  (if (not (< y x))			;Qobi: avoid temptation to optimize
      (k z)
      (tak (- x 1)
	   y
	   z
	   (lambda (v1)
	    (tak (- y 1)
		 z
		 x
		 (lambda (v2)
		  (tak (- z 1)
		       x
		       y
		       (lambda (v3) (tak v1 v2 v3 k)))))))))
 (tak x y z (lambda (a) a)))

(do ((i 0 (+ i 1))) ((= i 1000))
 (write (cpstak 18 12 6))
 (newline))
