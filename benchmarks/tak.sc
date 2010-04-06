;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:         tak.sc
;;; Description:  TAK benchmark from the Gabriel tests
;;; Author:       Richard Gabriel
;;; Created:      12-Apr-85
;;; Modified:     12-Apr-85 09:58:18 (Bob Shaw)
;;;               22-Jul-87 (Will Clinger)
;;;               21-Mar-94 (Qobi)
;;;               31-Mar-98 (Qobi)
;;; Language:     Scheme
;;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TAK -- A vanilla version of the TAKeuchi function

(define (tak x y z)
 (if (not (< y x))			;Qobi: avoid temptation to optimize
     z
     (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))

(do ((i 0 (+ i 1))) ((= i 1000))
 (write (tak 18 12 6))
 (newline))
