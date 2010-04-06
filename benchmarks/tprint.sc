;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:         tprint.sc
;;; Description:  TPRINT benchmark from the Gabriel tests
;;; Author:       Richard Gabriel
;;; Created:      12-Apr-85
;;; Modified:     19-Jul-85 19:05:26 (Bob Shaw)
;;;               23-Jul-87 (Will Clinger)
;;;               21-Mar-94 (Qobi)
;;;               31-Mar-98 (Qobi)
;;; Language:     Scheme
;;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TPRINT -- Benchmark to print and read to the terminal.

(define ttest-atoms '(abc1 cde2 efg3 ghi4 ijk5 klm6 mno7 opq8 qrs9
			   ;; Qobi: changed 123A to A123 etc. since Scheme->C
			   ;;       can't READ original symbols
			   stu0 uvw1 wxy2 xyz3 a123 b234 c345 d456
			   d567 e678 f789 g890))

(define (init m n atoms)
 (define (copy x) (if (pair? x) (cons (copy (car x)) (copy (cdr x))) x))
 (let ((atoms (copy atoms)))
  (do ((a atoms (cdr a))) ((null? (cdr a)) (set-cdr! a atoms) a))
  (init-aux m n atoms)))

(define (init-aux m n atoms)
 (cond ((= m 0) (car atoms))
       (else (do ((i n (- i 2)) (a '())) ((< i 1) a)
	      (set! a (cons (car atoms) a))
	      (set! atoms (cdr atoms))
	      ;; Qobi: changed (1- m) to (- m 1)
	      (set! a (cons (init-aux (- m 1) n atoms) a))))))

(define ttest-pattern (init 6 6 ttest-atoms))

;;; note: The INIT is not done multiple times.
(do ((i 0 (+ i 1))) ((= i 1000))
 (write ttest-pattern)
 (newline))
