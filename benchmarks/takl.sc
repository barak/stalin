;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:         takl.sc
;;; Description:  TAKL benchmark from the Gabriel tests
;;; Author:       Richard Gabriel
;;; Created:      12-Apr-85
;;; Modified:     12-Apr-85 10:07:00 (Bob Shaw)
;;;               22-Jul-87 (Will Clinger)
;;;               21-Mar-94 (Qobi)
;;;               31-Mar-98 (Qobi)
;;; Language:     Scheme
;;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TAKL -- The TAKeuchi function using lists as counters.

(define (listn n)
 (if (not (= 0 n))			;Qobi: avoid temptation to optimize
     (cons n (listn (- n 1)))		;Qobi: avoid temptation to optimize
     '()))				;Qobi

(define *18l* (listn 18))		;Qobi
(define *12l* (listn 12))		;Qobi
(define  *6l* (listn 6))		;Qobi

(define (mas x y z)
 (if (not (shorterp y x))
     z
     (mas (mas (cdr x) y z) (mas (cdr y) z x) (mas (cdr z) x y))))

(define (shorterp x y)
 (and (not (null? y))			;Qobi: used to depend on () being false
      (or (null? x) (shorterp (cdr x) (cdr y)))))

;;; note: The LISTN is not done multiple times.
(do ((i 0 (+ i 1))) ((= i 1000))
 (write (mas *18l* *12l* *6l*))
 (newline))
