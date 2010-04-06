;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:         fprint.sc
;;; Description:  FPRINT benchmark
;;; Author:       Richard Gabriel
;;; Created:      11-Apr-85
;;; Modified:     9-Jul-85 21:11:33 (Bob Shaw)
;;;               24-Jul-87 (Will Clinger)
;;;               16-Nov-94 (Qobi)
;;;               31-Mar-98 (Qobi)
;;; Language:     Scheme
;;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FPRINT -- Benchmark to print to a file.

(define test-atoms '(abcdef12 cdefgh23 efghij34 ghijkl45 ijklmn56 klmnop67
			      mnopqr78 opqrst89 qrstuv90 stuvwx01 uvwxyz12
			      ;; Qobi: changed 123456AB to AB123456 etc. since
			      ;;       Scheme->C can't READ original symbols
			      wxyzab23 xyzabc34 ab123456 bc234567 cd345678
			      de456789 ef567890 fg678901 gh789012 hi890123))

(define (init-aux m n atoms)
 (cond ((= m 0) (car atoms))
       (else (do ((i n (- i 2)) (a '())) ((< i 1) a)
	      (set! a (cons (car atoms) a))
	      (set! atoms (cdr atoms))
	      (set! a (cons (init-aux (- m 1) n atoms) a))))))

(define (init m n atoms)
 (define (copy x) (if (pair? x) (cons (copy (car x)) (copy (cdr x))) x))
 (let ((atoms (copy atoms)))
  (do ((a atoms (cdr a))) ((null? (cdr a)) (set-cdr! a atoms)))
  (init-aux m n atoms)))

(define test-pattern (init 6 6 test-atoms))

(define (fprint)
 (call-with-output-file "fprint.tst"
  (lambda (stream)
   (newline stream)
   (write test-pattern stream))
;;; begin Chez
   'replace
;;; end Chez
  ))

;;; note: The INIT is not done multiple times.
(do ((i 0 (+ i 1))) ((= i 1000)) (fprint))
