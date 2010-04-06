;;; LaHaShem HaAretz U'Mloah

;;; Stalin 0.11 - A global optimizing compiler for Scheme
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997, 1998, 1999, 2000, and 2001 NEC Research Institute, Inc. All
;;; rights reserved.
;;; Copyright 2002, 2003, 2004, 2005, and 2006 Purdue University. All rights
;;; reserved.

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; written by:
;;;    Jeffrey Mark Siskind
;;;    School of Electrical and Computer Engineering
;;;    Purdue University
;;;    Electrical Engineering Building, Room 330
;;;    465 Northwestern Avenue
;;;    West Lafayette IN 47907-2035 USA
;;;    voice: 765/496-3197
;;;    fax: 765/494-6440
;;;    qobi@purdue.edu
;;;    http://www.ece.purdue.edu/~qobi

;;; Begin delete for Trotsky
(module stalin3a)

(include "QobiScheme.sch")
(include "stalin3a.sch")
;;; End delete for Trotsky

;;; Fast tree shake

(define (fast-tree-shake!)
 (when (and (eq? (expression-kind *x*) 'lambda)
	    (list? (expression-parameters *x*))
	    (= (length (expression-parameters *x*)) 1)
	    (expression-body *x*)
	    (eq? (expression-kind (expression-body *x*)) 'call)
	    (eq? (expression-kind (expression-callee (expression-body *x*)))
		 'lambda)
	    (eq? (expression-kind
		  (expression-body
		   (expression-callee (expression-body *x*))))
		 'call)
	    (eq? (expression-kind
		  (expression-callee
		   (expression-body
		    (expression-callee (expression-body *x*)))))
		 'lambda)
	    (list? (expression-parameters
		    (expression-callee
		     (expression-body
		      (expression-callee (expression-body *x*))))))
	    (= (length (expression-parameters
			(expression-callee
			 (expression-body
			  (expression-callee (expression-body *x*))))))
	       219)
	    (every hunoz?
		   (expression-parameters
		    (expression-callee
		     (expression-body
		      (expression-callee (expression-body *x*))))))
	    (= (count-if-not
		(lambda (x) (eq? (expression-kind x) 'set!))
		(expression-arguments
		 (expression-body
		  (expression-callee (expression-body *x*)))))
	       1)
	    (eq? (expression-kind
		  (expression-body
		   (expression-callee
		    (expression-body
		     (expression-callee (expression-body *x*))))))
		 'call)
	    (= (length
		(expression-arguments
		 (expression-body
		  (expression-callee
		   (expression-body
		    (expression-callee (expression-body *x*)))))))
	       214)
	    (every (lambda (x) (eq? (expression-kind x) 'access))
		   (expression-arguments
		    (expression-body
		     (expression-callee
		      (expression-body
		       (expression-callee (expression-body *x*)))))))
	    (eq? (expression-kind
		  (expression-callee
		   (expression-body
		    (expression-callee
		     (expression-body
		      (expression-callee (expression-body *x*)))))))
		 'lambda)
	    (list? (expression-parameters
		    (expression-callee
		     (expression-body
		      (expression-callee
		       (expression-body
			(expression-callee (expression-body *x*))))))))
	    (= (length (expression-parameters
			(expression-callee
			 (expression-body
			  (expression-callee
			   (expression-body
			    (expression-callee (expression-body *x*))))))))
	       214)
	    (eq? (expression-kind
		  (expression-body
		   (expression-callee
		    (expression-body
		     (expression-callee
		      (expression-body
		       (expression-callee (expression-body *x*))))))))
		 'call)
	    (every
	     (lambda (x)
	      (and (eq? (expression-kind x) 'call)
		   (null? (expression-arguments x))
		   (eq? (expression-kind (expression-callee x))
			'lambda)
		   (not (expression-body (expression-callee x)))
		   (null? (expression-parameters (expression-callee x)))))
	     (expression-arguments
	      (expression-body
	       (expression-callee
		(expression-body
		 (expression-callee
		  (expression-body
		   (expression-callee (expression-body *x*)))))))))
	    (eq? (expression-kind
		  (expression-callee
		   (expression-body
		    (expression-callee
		     (expression-body
		      (expression-callee
		       (expression-body
			(expression-callee (expression-body *x*)))))))))
		 'lambda)
	    (list? (expression-parameters
		    (expression-callee
		     (expression-body
		      (expression-callee
		       (expression-body
			(expression-callee
			 (expression-body
			  (expression-callee (expression-body *x*))))))))))
	    (eq? (expression-kind
		  (expression-body
		   (expression-callee
		    (expression-body
		     (expression-callee
		      (expression-body
		       (expression-callee
			(expression-body
			 (expression-callee (expression-body *x*))))))))))
		 'call)
	    (eq? (expression-kind
		  (expression-callee
		   (expression-body
		    (expression-callee
		     (expression-body
		      (expression-callee
		       (expression-body
			(expression-callee
			 (expression-body
			  (expression-callee (expression-body *x*)))))))))))
		 'lambda)
	    (list?
	     (expression-parameters
	      (expression-callee
	       (expression-body
		(expression-callee
		 (expression-body
		  (expression-callee
		   (expression-body
		    (expression-callee
		     (expression-body
		      (expression-callee (expression-body *x*))))))))))))
	    (every
	     hunoz?
	     (expression-parameters
	      (expression-callee
	       (expression-body
		(expression-callee
		 (expression-body
		  (expression-callee
		   (expression-body
		    (expression-callee
		     (expression-body
		      (expression-callee (expression-body *x*)))))))))))))
  (let* ((gs (expression-parameters
	      (expression-callee
	       (expression-body
		(expression-callee
		 (expression-body
		  (expression-callee
		   (expression-body
		    (expression-callee (expression-body *x*))))))))))
	 (xs (cons
	      (expression-body
	       (expression-callee
		(expression-body
		 (expression-callee
		  (expression-body
		   (expression-callee
		    (expression-body
		     (expression-callee
		      (expression-body
		       (expression-callee (expression-body *x*)))))))))))
	      (expression-arguments
	       (expression-body
		(expression-callee
		 (expression-body
		  (expression-callee
		   (expression-body
		    (expression-callee
		     (expression-body
		      (expression-callee (expression-body *x*))))))))))))
	 (xs1 (remove-if-not
	       (lambda (x)
		(and (eq? (expression-kind x) 'set!)
		     (eq? (expression-kind (expression-source x)) 'lambda)
		     (memq (expression-variable x) gs)
		     (one (lambda (x1)
			   (and (eq? (expression-kind x1) 'set!)
				(eq? (expression-variable x)
				     (expression-variable x1))))
			  *xs*)))
	       xs))
	 (xs2 (set-differenceq xs xs1)))
   (for-each (lambda (x) (set-expression-accessed?! x #t)) *xs*)
   (for-each (lambda (g) (set-variable-accessed?! g #t)) *gs*)
   (for-each
    (lambda (x)
     (let loop ((x x))
      (set-expression-accessed?! x #f)
      (case (expression-kind x)
       ((null-constant) #f)
       ((true-constant) #f)
       ((false-constant) #f)
       ((char-constant) #f)
       ((fixnum-constant) #f)
       ((flonum-constant) #f)
       ((rectangular-constant) #f)
       ((string-constant) #f)
       ((symbol-constant) #f)
       ((pair-constant)
	(loop (car (expression-constant x)))
	(loop (cdr (expression-constant x))))
       ((vector-constant) (for-each-vector loop (expression-constant x)))
       ((lambda converted-lambda converted-continuation)
	(let loop ((gs (expression-parameters x)))
	 (cond ((pair? gs)
		(set-variable-accessed?! (first gs) #f)
		(loop (rest gs)))
	       ((variable? gs) (set-variable-accessed?! gs #f))))
	(when (expression-body x) (loop (expression-body x))))
       ((set!) (loop (expression-source x)))
       ((if)
	(loop (expression-antecedent x))
	(loop (expression-consequent x))
	(loop (expression-alternate x)))
       ((primitive-procedure) #f)
       ((foreign-procedure) #f)
       ((access) #f)
       ((call converted-call)
	(loop (expression-callee x))
	(for-each loop (expression-arguments x)))
       (else (fuck-up)))))
    xs1)
   (for-each (lambda (g)
	      (when (some (lambda (x) (eq? (expression-variable x) g)) xs1)
	       (set-variable-accessed?! g #f)))
	     gs)
   (for-each (lambda (x)
	      (let loop ((x x))
	       (case (expression-kind x)
		((null-constant) #f)
		((true-constant) #f)
		((false-constant) #f)
		((char-constant) #f)
		((fixnum-constant) #f)
		((flonum-constant) #f)
		((rectangular-constant) #f)
		((string-constant) #f)
		((symbol-constant) #f)
		((pair-constant) #f)
		((vector-constant) #f)
		((lambda converted-lambda converted-continuation)
		 (when (expression-body x) (loop (expression-body x))))
		((set!) (loop (expression-source x)))
		((if)
		 (loop (expression-antecedent x))
		 (loop (expression-consequent x))
		 (loop (expression-alternate x)))
		((primitive-procedure) #f)
		((foreign-procedure) #f)
		((access)
		 (when (memq (expression-variable x) gs)
		  (set-variable-accessed?! (expression-variable x) #t)))
		((call converted-call)
		 (loop (expression-callee x))
		 (for-each loop (expression-arguments x)))
		(else (fuck-up)))))
	     xs2)
   (let loop ()
    (let ((again? #f))
     (for-each
      (lambda (x)
       (when (variable-accessed? (expression-variable x))
	(let loop ((x x))
	 (set-expression-accessed?! x #t)
	 (case (expression-kind x)
	  ((null-constant) #f)
	  ((true-constant) #f)
	  ((false-constant) #f)
	  ((char-constant) #f)
	  ((fixnum-constant) #f)
	  ((flonum-constant) #f)
	  ((rectangular-constant) #f)
	  ((string-constant) #f)
	  ((symbol-constant) #f)
	  ((pair-constant)
	   (loop (car (expression-constant x)))
	   (loop (cdr (expression-constant x))))
	  ((vector-constant) (for-each-vector loop (expression-constant x)))
	  ((lambda converted-lambda converted-continuation)
	   (let loop ((gs (expression-parameters x)))
	    (cond ((pair? gs)
		   (set-variable-accessed?! (first gs) #t)
		   (loop (rest gs)))
		  ((variable? gs) (set-variable-accessed?! gs #t))))
	   (when (expression-body x) (loop (expression-body x))))
	  ((set!) (loop (expression-source x)))
	  ((if)
	   (loop (expression-antecedent x))
	   (loop (expression-consequent x))
	   (loop (expression-alternate x)))
	  ((primitive-procedure) #f)
	  ((foreign-procedure) #f)
	  ((access)
	   (when (and (memq (expression-variable x) gs)
		      (not (variable-accessed? (expression-variable x))))
	    (set-variable-accessed?! (expression-variable x) #t)
	    (set! again? #t)))
	  ((call converted-call)
	   (loop (expression-callee x))
	   (for-each loop (expression-arguments x)))
	  (else (fuck-up))))))
      xs1)
     (when again? (loop))))
   ;; Remove the unused variables.
   (set-expression-parameters!
    (expression-callee
     (expression-body
      (expression-callee
       (expression-body
	(expression-callee
	 (expression-body (expression-callee (expression-body *x*))))))))
    (remove-if-not
     variable-accessed?
     (expression-parameters
      (expression-callee
       (expression-body
	(expression-callee
	 (expression-body
	  (expression-callee
	   (expression-body (expression-callee (expression-body *x*)))))))))))
   ;; Remove the extra noops that initialize the unused variables to undefined.
   (for-each
    (lambda (x)
     (let loop ((x x))
      (set-expression-accessed?! x #f)
      (case (expression-kind x)
       ((null-constant) #f)
       ((true-constant) #f)
       ((false-constant) #f)
       ((char-constant) #f)
       ((fixnum-constant) #f)
       ((flonum-constant) #f)
       ((rectangular-constant) #f)
       ((string-constant) #f)
       ((symbol-constant) #f)
       ((pair-constant)
	(loop (car (expression-constant x)))
	(loop (cdr (expression-constant x))))
       ((vector-constant) (for-each-vector loop (expression-constant x)))
       ((lambda converted-lambda converted-continuation)
	(let loop ((gs (expression-parameters x)))
	 (cond ((pair? gs)
		(set-variable-accessed?! (first gs) #f)
		(loop (rest gs)))
	       ((variable? gs) (set-variable-accessed?! gs #f))))
	(when (expression-body x) (loop (expression-body x))))
       ((set!) (loop (expression-source x)))
       ((if)
	(loop (expression-antecedent x))
	(loop (expression-consequent x))
	(loop (expression-alternate x)))
       ((primitive-procedure) #f)
       ((foreign-procedure) #f)
       ((access) #f)
       ((call converted-call)
	(loop (expression-callee x))
	(for-each loop (expression-arguments x)))
       (else (fuck-up)))))
    (sublist
     (expression-arguments
      (expression-body
       (expression-callee
	(expression-body
	 (expression-callee
	  (expression-body (expression-callee (expression-body *x*))))))))
     (length
      (expression-parameters
       (expression-callee
	(expression-body
	 (expression-callee
	  (expression-body
	   (expression-callee
	    (expression-body (expression-callee (expression-body *x*))))))))))
     (length
      (expression-arguments
       (expression-body
	(expression-callee
	 (expression-body
	  (expression-callee
	   (expression-body (expression-callee (expression-body *x*)))))))))))
   (set-expression-arguments!
    (expression-body
     (expression-callee
      (expression-body
       (expression-callee
	(expression-body (expression-callee (expression-body *x*)))))))
    (sublist
     (expression-arguments
      (expression-body
       (expression-callee
	(expression-body
	 (expression-callee
	  (expression-body (expression-callee (expression-body *x*))))))))
     0
     (length
      (expression-parameters
       (expression-callee
	(expression-body
	 (expression-callee
	  (expression-body
	   (expression-callee
	    (expression-body
	     (expression-callee (expression-body *x*))))))))))))
   ;; Remove the unused definitions.
   (set-expression-arguments!
    (expression-body
     (expression-callee
      (expression-body
       (expression-callee
	(expression-body
	 (expression-callee
	  (expression-body (expression-callee (expression-body *x*)))))))))
    (remove-if-not
     expression-accessed?
     (expression-arguments
      (expression-body
       (expression-callee
	(expression-body
	 (expression-callee
	  (expression-body
	   (expression-callee
	    (expression-body
	     (expression-callee (expression-body *x*))))))))))))
   (unless (expression-accessed?
	    (expression-body
	     (expression-callee
	      (expression-body
	       (expression-callee
		(expression-body
		 (expression-callee
		  (expression-body
		   (expression-callee
		    (expression-body
		     (expression-callee (expression-body *x*))))))))))))
    (set-expression-body!
     (expression-callee
      (expression-body
       (expression-callee
	(expression-body
	 (expression-callee
	  (expression-body
	   (expression-callee
	    (expression-body (expression-callee (expression-body *x*))))))))))
     #f))
   ;; Remove the hunoz variables for the unused definitions.
   (for-each
    (lambda (g) (set-variable-accessed?! g #f))
    (sublist
     (expression-parameters
      (expression-callee
       (expression-body
	(expression-callee
	 (expression-body
	  (expression-callee
	   (expression-body
	    (expression-callee
	     (expression-body
	      (expression-callee (expression-body *x*)))))))))))
     (length
      (expression-arguments
       (expression-body
	(expression-callee
	 (expression-body
	  (expression-callee
	   (expression-body
	    (expression-callee
	     (expression-body
	      (expression-callee (expression-body *x*)))))))))))
     (length
      (expression-parameters
       (expression-callee
	(expression-body
	 (expression-callee
	  (expression-body
	   (expression-callee
	    (expression-body
	     (expression-callee
	      (expression-body
	       (expression-callee (expression-body *x*))))))))))))))
   (set-expression-parameters!
    (expression-callee
     (expression-body
      (expression-callee
       (expression-body
	(expression-callee
	 (expression-body
	  (expression-callee
	   (expression-body (expression-callee (expression-body *x*))))))))))
    (sublist
     (expression-parameters
      (expression-callee
       (expression-body
	(expression-callee
	 (expression-body
	  (expression-callee
	   (expression-body
	    (expression-callee
	     (expression-body
	      (expression-callee (expression-body *x*)))))))))))
     0
     (length
      (expression-arguments
       (expression-body
	(expression-callee
	 (expression-body
	  (expression-callee
	   (expression-body
	    (expression-callee
	     (expression-body
	      (expression-callee (expression-body *x*)))))))))))))
   (set! *gs* (remove-if-not variable-accessed? *gs*))
   (set! *xs* (remove-if-not expression-accessed? *xs*))
   (set! *calls* (remove-if-not expression-accessed? *calls*))
   (set! *accesses* (remove-if-not expression-accessed? *accesses*))
   (set! *assignments* (remove-if-not expression-accessed? *assignments*))
   (set! *references* (remove-if-not expression-accessed? *references*))
   (set! *es*
	 (remove-if-not
	  (lambda (e) (expression-accessed? (environment-expression e))) *es*))
   ;; This is just for error checking.
   (for-each (lambda (g) (set-variable-accessed?! g #f)) *gs*)
   (for-each (lambda (x) (set-expression-accessed?! x #f)) *xs*)
   (let loop ((x *x*))
    (unless (memq x *xs*) (fuck-up))
    (set-expression-accessed?! x #t)
    (case (expression-kind x)
     ((null-constant) #f)
     ((true-constant) #f)
     ((false-constant) #f)
     ((char-constant) #f)
     ((fixnum-constant) #f)
     ((flonum-constant) #f)
     ((rectangular-constant) #f)
     ((string-constant) #f)
     ((symbol-constant) #f)
     ((pair-constant)
      (loop (car (expression-constant x)))
      (loop (cdr (expression-constant x))))
     ((vector-constant) (for-each-vector loop (expression-constant x)))
     ((lambda converted-lambda converted-continuation)
      (let loop ((gs (expression-parameters x)))
       (cond ((pair? gs)
	      (unless (memq (first gs) *gs*) (fuck-up))
	      (set-variable-accessed?! (first gs) #t)
	      (loop (rest gs)))
	     ((variable? gs)
	      (unless (memq gs *gs*) (fuck-up))
	      (set-variable-accessed?! gs #t))))
      (when (expression-body x) (loop (expression-body x))))
     ((set!)
      (unless (memq (expression-variable x) *gs*) (fuck-up))
      (loop (expression-source x)))
     ((if)
      (loop (expression-antecedent x))
      (loop (expression-consequent x))
      (loop (expression-alternate x)))
     ((primitive-procedure) #f)
     ((foreign-procedure) #f)
     ((access) (unless (memq (expression-variable x) *gs*) (fuck-up)))
     ((call converted-call)
      (loop (expression-callee x))
      (for-each loop (expression-arguments x)))
     (else (fuck-up))))
   (unless (and (every variable-accessed? *gs*)
		(every expression-accessed? *xs*))
    (fuck-up)))))

;;; Annotate expressions with their parents

(define (annotate-expressions-with-their-parents!)
 (for-each (lambda (x) (set-expression-parent! x (unspecified))) *xs*)
 (set-expression-parent! *x* #f)
 (let loop ((x *x*))
  (case (expression-kind x)
   ((null-constant) #f)
   ((true-constant) #f)
   ((false-constant) #f)
   ((char-constant) #f)
   ((fixnum-constant) #f)
   ((flonum-constant) #f)
   ((rectangular-constant) #f)
   ((string-constant) #f)
   ((symbol-constant) #f)
   ((pair-constant)
    (unless (eq? (expression-parent (car (expression-constant x)))
		 (unspecified))
     (fuck-up))
    (set-expression-parent! (car (expression-constant x)) x)
    (unless (eq? (expression-parent (cdr (expression-constant x)))
		 (unspecified))
     (fuck-up))
    (set-expression-parent! (cdr (expression-constant x)) x)
    (loop (car (expression-constant x)))
    (loop (cdr (expression-constant x))))
   ((vector-constant)
    (for-each-vector
     (lambda (x1)
      (unless (eq? (expression-parent x1) (unspecified)) (fuck-up))
      (set-expression-parent! x1 x)
      (loop x1))
     (expression-constant x)))
   ((lambda converted-lambda converted-continuation)
    (unless (noop? x)
     (unless (eq? (expression-parent (expression-body x)) (unspecified))
      (fuck-up))
     (set-expression-parent! (expression-body x) x)
     (loop (expression-body x))))
   ((set!)
    (unless (eq? (expression-parent (expression-source x)) (unspecified))
     (fuck-up))
    (set-expression-parent! (expression-source x) x)
    (loop (expression-source x)))
   ((if)
    (unless (eq? (expression-parent (expression-antecedent x)) (unspecified))
     (fuck-up))
    (set-expression-parent! (expression-antecedent x) x)
    (unless (eq? (expression-parent (expression-consequent x)) (unspecified))
     (fuck-up))
    (set-expression-parent! (expression-consequent x) x)
    (unless (eq? (expression-parent (expression-alternate x)) (unspecified))
     (fuck-up))
    (set-expression-parent! (expression-alternate x) x)
    (loop (expression-antecedent x))
    (loop (expression-consequent x))
    (loop (expression-alternate x)))
   ((primitive-procedure) #f)
   ((foreign-procedure) #f)
   ((access) #f)
   ((call converted-call)
    (unless (eq? (expression-parent (expression-callee x)) (unspecified))
     (fuck-up))
    (set-expression-parent! (expression-callee x) x)
    (loop (expression-callee x))
    (for-each (lambda (x1)
	       (unless (eq? (expression-parent x1) (unspecified)) (fuck-up))
	       (set-expression-parent! x1 x)
	       (loop x1))
	      (expression-arguments x)))
   (else (fuck-up))))
 ;; This is needed because, after CPS conversion, there may be dangling
 ;; expressions, particularly clones.
 (set! *xs*
       (remove-if (lambda (x) (eq? (expression-parent x) (unspecified))) *xs*))
 (set! *calls*
       (remove-if (lambda (x) (eq? (expression-parent x) (unspecified)))
		  *calls*))
 (set! *accesses*
       (remove-if (lambda (x) (eq? (expression-parent x) (unspecified)))
		  *accesses*))
 (set! *assignments*
       (remove-if (lambda (x) (eq? (expression-parent x) (unspecified)))
		  *assignments*))
 (set! *references*
       (remove-if (lambda (x) (eq? (expression-parent x) (unspecified)))
		  *references*)))

;;; In-line first-order calls to primitive procedures

(define (first-order-position? x)
 (or (not (expression-parent x))
     (and (or (eq? (expression-kind (expression-parent x)) 'call)
	      (eq? (expression-kind (expression-parent x)) 'converted-call))
	  (eq? x (expression-callee (expression-parent x))))
     (and (eq? (expression-kind (expression-parent x)) 'if)
	  (or (eq? x (expression-antecedent (expression-parent x)))
	      (first-order-position? (expression-parent x))))))

(define *primitive-procedure-rewrites*
 ;; needs work: This doesn't in-line the procedures created by
 ;;             DEFINE-STRUCTURE.
 '((not not)
   (boolean? boolean?)
   (eq? eq?)
   (pair? structure? pair)
   (cons make-structure pair 2)
   (car structure-ref pair 0)
   (cdr structure-ref pair 1)
   (set-car! structure-set! pair 0)
   (set-cdr! structure-set! pair 1)
   (null? null?)
   (symbol? symbol?)
   (symbol->string symbol->string)
   (string->uninterned-symbol string->uninterned-symbol)
   (number? number?)
   (real? real?)
   (integer? integer?)
   (exact? exact?)
   (inexact? inexact?)
   (= =)
   (< <)
   (> >)
   (<= <=)
   (>= >=)
   (zero? zero?)
   (positive? positive?)
   (negative? negative?)
   (max max)
   (min min)
   (+ +)
   (* *)
   (- -)
   (/ /)
   (quotient quotient)
   (remainder remainder)
   (<< <<)
   (>> >>)
   (bitwise-not bitwise-not)
   (bitwise-and bitwise-and)
   (bitwise-or bitwise-or)
   (bitwise-xor bitwise-xor)
   (floor floor)
   (ceiling ceiling)
   (truncate truncate)
   (round round)
   (exp exp)
   (log log)
   (sin sin)
   (cos cos)
   (tan tan)
   (asin asin)
   (acos acos)
   (atan atan)
   (sqrt sqrt)
   (expt expt)
   (exact->inexact exact->inexact)
   (inexact->exact inexact->exact)
   (char? char?)
   (char->integer char->integer)
   (integer->char integer->char)
   (string? string?)
   (make-string make-string)
   (string string)
   (string-length string-length)
   (string-ref string-ref)
   (string-set! string-set!)
   (vector? vector?)
   (make-vector make-vector)
   (make-displaced-vector make-displaced-vector)
   (vector vector)
   (vector-length vector-length)
   (vector-ref vector-ref)
   (vector-set! vector-set!)
   (procedure? procedure?)
   (apply apply)
   (call-with-current-continuation call-with-current-continuation)
   (input-port? input-port?)
   (output-port? output-port?)
   (open-input-file open-input-file)
   (open-output-file open-output-file)
   (close-input-port close-input-port)
   (close-output-port close-output-port)
   (eof-object? eof-object?)
   (panic panic)
   (pointer? pointer?)
   (integer->string integer->string)
   (integer->input-port integer->input-port)
   (integer->output-port integer->output-port)
   (integer->pointer integer->pointer)))

(define (in-line-first-order-calls-to-primitive-procedures!)
 (let ((xs (remove-if-not
	    (lambda (x)
	     (and (first-order-position? x)
		  (expression-parent x)
		  (assq (variable-name (expression-variable x))
			*primitive-procedure-rewrites*)
		  (not
		   (empty?
		    (parent (variable-environment (expression-variable x)))))
		  (or (empty?
		       (parent
			(parent
			 (variable-environment (expression-variable x)))))
		      (and
		       (not
			(empty?
			 (parent
			  (parent
			   (parent
			    (variable-environment (expression-variable x)))))))
		       (empty?
			(parent
			 (parent
			  (parent
			   (parent
			    (variable-environment (expression-variable x)))))))
		       (not (some (lambda (x1)
				   (eq? (expression-variable x1)
					(expression-variable x)))
				  *assignments*))))))
	    *accesses*)))
  (for-each
   (lambda (x)
    ((cond ((or (eq? (expression-kind (expression-parent x)) 'call)
		(eq? (expression-kind (expression-parent x)) 'converted-call))
	    set-expression-callee!)
	   ((and (eq? (expression-kind (expression-parent x)) 'if)
		 (eq? x (expression-antecedent (expression-parent x))))
	    set-expression-antecedent!)
	   ((and (eq? (expression-kind (expression-parent x)) 'if)
		 (eq? x (expression-consequent (expression-parent x))))
	    set-expression-consequent!)
	   ((and (eq? (expression-kind (expression-parent x)) 'if)
		 (eq? x (expression-alternate (expression-parent x))))
	    set-expression-alternate!)
	   (else (fuck-up)))
     (expression-parent x)
     (create-expression 'primitive-procedure
			x
			(cdr (assq (variable-name (expression-variable x))
				   *primitive-procedure-rewrites*)))))
   xs)
  (set! *xs* (set-differenceq *xs* xs))
  (set! *accesses* (set-differenceq *accesses* xs))
  (set! *references* (set-differenceq *references* xs))))

;;; Annotate variables with their environments

(define (annotate-environment-variables-with-their-environment! e)
 (for-each (lambda (g)
	    (unless (eq? (variable-environment g) (unspecified)) (fuck-up))
	    (set-variable-environment! g e))
	   (variables e)))

(define (annotate-variables-with-their-environments!)
 (for-each (lambda (g) (set-variable-environment! g (unspecified))) *gs*)
 (for-each annotate-environment-variables-with-their-environment! *es*)
 (when (some (lambda (g) (eq? (variable-environment g) (unspecified))) *gs*)
  (fuck-up)))

;;; Annotate expressions with their environments

(define (annotate-environment-expressions-with-their-environment! e)
 (unless (eq? e (narrow-prototype e))
  (unless (eq? (expression-environment (environment-expression e))
	       (unspecified))
   (fuck-up))
  (set-expression-environment!
   (environment-expression e)
   (let loop ((x (expression-parent
		  (environment-expression (narrow-prototype e)))))
    (if (or (eq? (expression-kind x) 'lambda)
	    (eq? (expression-kind x) 'converted-lambda)
	    (eq? (expression-kind x) 'converted-continuation))
	(expression-lambda-environment x)
	(loop (expression-parent x))))))
 (unless (noop? e)
  (let loop ((x (expression-body (environment-expression e))))
   (unless (eq? (expression-environment x) (unspecified)) (fuck-up))
   (set-expression-environment! x e)
   (case (expression-kind x)
    ((null-constant) #f)
    ((true-constant) #f)
    ((false-constant) #f)
    ((char-constant) #f)
    ((fixnum-constant) #f)
    ((flonum-constant) #f)
    ((rectangular-constant) #f)
    ((string-constant) #f)
    ((symbol-constant) #f)
    ((pair-constant)
     (loop (car (expression-constant x)))
     (loop (cdr (expression-constant x))))
    ((vector-constant) (for-each-vector loop (expression-constant x)))
    ((lambda converted-lambda converted-continuation) #f)
    ((set!) (loop (expression-source x)))
    ((if)
     (loop (expression-antecedent x))
     (loop (expression-consequent x))
     (loop (expression-alternate x)))
    ((primitive-procedure) #f)
    ((foreign-procedure) #f)
    ((access) #f)
    ((call converted-call)
     (loop (expression-callee x))
     (for-each loop (expression-arguments x)))
    (else (fuck-up))))))

(define (annotate-expressions-with-their-environments!)
 (for-each (lambda (x) (set-expression-environment! x (unspecified))) *xs*)
 (set-expression-environment! *x* #f)
 (for-each annotate-environment-expressions-with-their-environment! *es*)
 (when (some (lambda (x) (eq? (expression-environment x) (unspecified))) *xs*)
  (fuck-up)))

;;; Annotate variables with their references

(define (annotate-variables-with-their-references!)
 (for-each (lambda (g)
	    (set-variable-accesses! g '())
	    (set-variable-assignments! g '())
	    (set-variable-references! g '()))
	   *gs*)
 (for-each (lambda (x)
	    (case (expression-kind x)
	     ((null-constant) #f)
	     ((true-constant) #f)
	     ((false-constant) #f)
	     ((char-constant) #f)
	     ((fixnum-constant) #f)
	     ((flonum-constant) #f)
	     ((rectangular-constant) #f)
	     ((string-constant) #f)
	     ((symbol-constant) #f)
	     ((pair-constant) #f)
	     ((vector-constant) #f)
	     ((lambda converted-lambda converted-continuation) #f)
	     ((set!)
	      (set-variable-assignments!
	       (expression-variable x)
	       (cons x (variable-assignments (expression-variable x))))
	      (set-variable-references!
	       (expression-variable x)
	       (cons x (variable-references (expression-variable x)))))
	     ((if) #f)
	     ((primitive-procedure) #f)
	     ((foreign-procedure) #f)
	     ((access)
	      (set-variable-accesses!
	       (expression-variable x)
	       (cons x (variable-accesses (expression-variable x))))
	      (set-variable-references!
	       (expression-variable x)
	       (cons x (variable-references (expression-variable x)))))
	     ((call converted-call) #f)
	     (else (fuck-up))))
	   *xs*))

;;; Perform flow analysis

;;; Prior to conversion, `continues' and `returns' should be the same. Errors,
;;; infinite loops, and calls to PANIC, no-return foreign procedures,
;;; continuations don't return and don't continue. After conversion, `returns'
;;; implies `continues' but not vice versa. But even here, `continues' and
;;; `returns' should be the same for implicit continuation calls. Errors,
;;; infinite loops, and (both converted and nonconverted) calls to PANIC,
;;; no-return foreign procedures, and nonconverted continuations don't
;;; continue or return. (Both converted and nonconverted) calls to converted
;;; continuations and converted native procedures might return but don't
;;; continue. And converted calls whose implicit continuation call doesn't
;;; return might continue but don't return.

(define (environment-continues? e)
 (when (or (converted? e) (converted-continuation? e)) (fuck-up))
 (or (noop? e)
     (expression-continues? (expression-body (environment-expression e)))))

(define (environment-returns? e)
 (or (noop? e)
     (expression-returns? (expression-body (environment-expression e)))))

(define (continues? ws w y)
 (when (continuation-argument-call-site? y) (fuck-up))
 (lambda (u)
  (and
   ((truly-compatible-procedure? ws w y) u)
   ;; Calls to nonconverted continuations never continue.
   (or
    (and
     (primitive-procedure-type? u)
     (cond
      (((primitive-procedure-type-named? 'apply) u)
       (and
	(if (converted? y)
	    (cond
	     ((null? ws)
	      (can-be?
	       (lambda (u)
		(and
		 ((list-type-of-length-at-least? 2) u)
		 (can-be? (continues? (list (pair-type-car u))
				      (pair-type-cdr (pair-type-cdr u))
				      (recreate-call-site y 'first-argument))
			  (pair-type-car (pair-type-cdr u)))))
	       w))
	     ((null? (rest ws))
	      (can-be?
	       (lambda (u)
		(and
		 ((list-type-of-length-at-least? 1) u)
		 (can-be? (continues? ws
				      (pair-type-cdr u)
				      (recreate-call-site y 'first-argument))
			  (pair-type-car u))))
	       w))
	     (else
	      (or (and (can-be? null-type? w)
		       (can-be?
			(continues?
			 (cons (first ws) (but-last (rest (rest ws))))
			 (last ws)
			 (recreate-call-site y 'first-argument))
			(second ws)))
		  (can-be? (continues? (cons (first ws) (rest (rest ws)))
				       w
				       (recreate-call-site y 'first-argument))
			   (second ws)))))
	    (if (null? ws)
		(can-be?
		 (lambda (u)
		  (and ((list-type-of-length-at-least? 1) u)
		       (can-be?
			(continues? '()
				    (pair-type-cdr u)
				    (recreate-call-site y 'first-argument))
			(pair-type-car u))))
		 w)
		(or (and (can-be? null-type? w)
			 (can-be?
			  (continues? (but-last (rest ws))
				      (last ws)
				      (recreate-call-site y 'first-argument))
			  (first ws)))
		    (can-be?
		     (continues?
		      (rest ws) w (recreate-call-site y 'first-argument))
		     (first ws)))))))
      (((primitive-procedure-type-named? 'call-with-current-continuation) u)
       (and
	(or (if (converted? y)
		(cond
		 ((null? ws)
		  (can-be?
		   (lambda (u)
		    (and ((list-type-of-length? 2) u)
			 (can-be?
			  (continues?
			   (list (pair-type-car u) (pair-type-car u))
			   (pair-type-cdr (pair-type-cdr u))
			   (recreate-call-site y 'first-argument))
			  (pair-type-car (pair-type-cdr u)))))
		   w))
		 ((null? (rest ws))
		  (can-be?
		   (lambda (u)
		    (and ((list-type-of-length? 1) u)
			 (can-be?
			  (continues? (list (first ws) (first ws))
				      (pair-type-cdr u)
				      (recreate-call-site y 'first-argument))
			  (pair-type-car u))))
		   w))
		 (else
		  (can-be?
		   (continues?
		    (cons (first ws) (cons (first ws) (rest (rest ws))))
		    w
		    (recreate-call-site y 'first-argument))
		   (second ws))))
		(if (null? ws)
		    (can-be?
		     (lambda (u)
		      (and
		       ((list-type-of-length? 1) u)
		       (can-be?
			(continues?
			 (list (create-anonymous-type-set
				(<continuation> (call-site-expression y))))
			 (pair-type-cdr u)
			 (recreate-call-site y 'first-argument))
			(pair-type-car u))))
		     w)
		    (can-be?
		     (continues?
		      (list (create-anonymous-type-set
			     (<continuation> (call-site-expression y))))
		      w
		      (recreate-call-site y 'first-argument))
		     (first ws))))
	    (some (lambda (u)
		   (and ((continuation-type-to? (call-site-expression y)) u)
			(continuation-type-continuation-accessed? u)))
		  *continuation-types*))))
      (((primitive-procedure-type-named? 'panic) u) #f)
      (((primitive-procedure-type-named? 'fork) u)
       ;; needs work: To handle converted calls to FORK.
       (when (converted? y) (unimplemented y "unimplemented"))
       ;; For now, calls to FORK always continue.
       #t)
      (((primitive-procedure-type-named? 'mutex) u)
       ;; needs work: To handle converted calls to MUTEX.
       (when (converted? y) (unimplemented y "unimplemented"))
       ;; For now, calls to MUTEX always continue.
       #t)
      (else #t)))
    (and (native-procedure-type? u)
	 (not (converted? (callee-environment u y)))
	 (not (converted-continuation? (callee-environment u y)))
	 (environment-continues? (callee-environment u y)))
    (and (foreign-procedure-type? u) (foreign-procedure-returns? u))))))

(define (returns? ws w y)
 (lambda (u)
  (and
   ((truly-compatible-procedure? ws w y) u)
   ;; Calls to nonconverted continuations never return.
   (or (and
	(primitive-procedure-type? u)
	(cond
	 (((primitive-procedure-type-named? 'apply) u)
	  (and
	   (if (converted? y)
	       (cond
		((null? ws)
		 (can-be?
		  (lambda (u)
		   (and
		    ((list-type-of-length-at-least? 2) u)
		    (can-be? (returns? (list (pair-type-car u))
				       (pair-type-cdr (pair-type-cdr u))
				       (recreate-call-site y 'first-argument))
			     (pair-type-car (pair-type-cdr u)))))
		  w))
		((null? (rest ws))
		 (can-be?
		  (lambda (u)
		   (and
		    ((list-type-of-length-at-least? 1) u)
		    (can-be? (returns? ws
				       (pair-type-cdr u)
				       (recreate-call-site y 'first-argument))
			     (pair-type-car u))))
		  w))
		(else
		 (or (and (can-be? null-type? w)
			  (can-be?
			   (returns?
			    (cons (first ws) (but-last (rest (rest ws))))
			    (last ws)
			    (recreate-call-site y 'first-argument))
			   (second ws)))
		     (can-be? (returns? (cons (first ws) (rest (rest ws)))
					w
					(recreate-call-site y 'first-argument))
			      (second ws)))))
	       (if (null? ws)
		   (can-be?
		    (lambda (u)
		     (and ((list-type-of-length-at-least? 1) u)
			  (can-be?
			   (returns? '()
				     (pair-type-cdr u)
				     (recreate-call-site y 'first-argument))
			   (pair-type-car u))))
		    w)
		   (or (and (can-be? null-type? w)
			    (can-be?
			     (returns? (but-last (rest ws))
				       (last ws)
				       (recreate-call-site y 'first-argument))
			     (first ws)))
		       (can-be?
			(returns?
			 (rest ws) w (recreate-call-site y 'first-argument))
			(first ws)))))))
	 (((primitive-procedure-type-named? 'call-with-current-continuation) u)
	  (and
	   (or (if (converted? y)
		   (cond
		    ((null? ws)
		     (can-be?
		      (lambda (u)
		       (and ((list-type-of-length? 2) u)
			    (can-be?
			     (returns?
			      (list (pair-type-car u) (pair-type-car u))
			      (pair-type-cdr (pair-type-cdr u))
			      (recreate-call-site y 'first-argument))
			     (pair-type-car (pair-type-cdr u)))))
		      w))
		    ((null? (rest ws))
		     (can-be?
		      (lambda (u)
		       (and ((list-type-of-length? 1) u)
			    (can-be?
			     (returns? (list (first ws) (first ws))
				       (pair-type-cdr u)
				       (recreate-call-site y 'first-argument))
			     (pair-type-car u))))
		      w))
		    (else
		     (can-be?
		      (returns?
		       (cons (first ws) (cons (first ws) (rest (rest ws))))
		       w
		       (recreate-call-site y 'first-argument))
		      (second ws))))
		   (if (null? ws)
		       (can-be?
			(lambda (u)
			 (and
			  ((list-type-of-length? 1) u)
			  (can-be?
			   (returns?
			    (list (create-anonymous-type-set
				   (<continuation> (call-site-expression y))))
			    (pair-type-cdr u)
			    (recreate-call-site y 'first-argument))
			   (pair-type-car u))))
			w)
		       (can-be?
			(returns?
			 (list (create-anonymous-type-set
				(<continuation> (call-site-expression y))))
			 w
			 (recreate-call-site y 'first-argument))
			(first ws))))
	       (some (lambda (u)
		      (and ((continuation-type-to? (call-site-expression y)) u)
			   (continuation-type-continuation-accessed? u)))
		     *continuation-types*))))
	 (((primitive-procedure-type-named? 'panic) u) #f)
	 (((primitive-procedure-type-named? 'fork) u)
	  ;; needs work: To handle converted calls to FORK.
	  (when (converted? y) (unimplemented y "unimplemented"))
	  ;; For now, calls to FORK always return.
	  #t)
	 (((primitive-procedure-type-named? 'mutex) u)
	  ;; needs work: To handle converted calls to MUTEX.
	  (when (converted? y) (unimplemented y "unimplemented"))
	  ;; For now, calls to MUTEX always return.
	  #t)
	 (else (or (not (converted? y))
		   (can-be?
		    (returns? (list *void*)
			      *null*
			      (recreate-call-site y 'continuation-argument))
		    (first ws))))))
       (and (native-procedure-type? u)
	    (environment-returns? (callee-environment u y))
	    (or (not (converted? y))
		(converted? (callee-environment u y))
		(converted-continuation? (callee-environment u y))
		(not (environment-continues? (callee-environment u y)))
		(can-be?
		 (returns? (list *void*)
			   *null*
			   (recreate-call-site y 'continuation-argument))
		 (first ws))))
       (and (foreign-procedure-type? u)
	    (foreign-procedure-returns? u)
	    (or (not (converted? y))
		(can-be?
		 (returns? (list *void*)
			   *null*
			   (recreate-call-site y 'continuation-argument))
		 (first ws))))))))

(define (needs-implicit-continuation-call? ws w y)
 (unless (converted? y) (fuck-up))
 (lambda (u)
  (and
   ((truly-compatible-procedure? ws w y) u)
   ;; Calls to nonconverted continuations never continue so you never need to
   ;; implicitly call the continuation after a call to a nonconverted
   ;; continuation.
   (or (and
	(primitive-procedure-type? u)
	(cond
	 ;; The call to APPLY doesn't implicitly call the continuation. The
	 ;; first-argument call does.
	 (((primitive-procedure-type-named? 'apply) u) #f)
	 ;; The call to CALL-WITH-CURRENT-CONTINUATION doesn't implicitly
	 ;; call the continuation. The first-argument call does.
	 (((primitive-procedure-type-named? 'call-with-current-continuation) u)
	  #f)
	 ;; PANIC doesn't continue so you never need to imlicitly call the
	 ;; continuation after a call to PANIC.
	 (((primitive-procedure-type-named? 'panic) u) #f)
	 ;; needs work: To handle converted calls to FORK.
	 (((primitive-procedure-type-named? 'fork) u)
	  (unimplemented y "unimplemented"))
	 ;; needs work: To handle converted calls to MUTEX.
	 (((primitive-procedure-type-named? 'mutex) u)
	  (unimplemented y "unimplemented"))
	 ;; Assume all other primitive procedures continue. Since the primitive
	 ;; procedures are not converted and don't implicitly call the
	 ;; continuation it must be implicitly called after the call to the
	 ;; primitive procedure continues.
	 (else #t)))
       (and (native-procedure-type? u)
	    ;; The call to a converted native procedure doesn't implicitly
	    ;; call the continuation. The converted native procedure does.
	    (not (converted? (callee-environment u y)))
	    ;; Calls to converted continuations never continue so you never
	    ;; need to implicitly call the continuation after a call to a
	    ;; converted continuation.
	    (not (converted-continuation? (callee-environment u y)))
	    (environment-continues? (callee-environment u y)))
       (and (foreign-procedure-type? u) (foreign-procedure-returns? u))))))

(define (first-argument-needs-implicit-continuation-call? ws w y)
 (unless (converted? y) (fuck-up))
 (lambda (u)
  (and
   ((truly-compatible-procedure? ws w y) u)
   (or (and
	((primitive-procedure-type-named? 'apply) u)
	(cond
	 ((null? ws)
	  (can-be?
	   (lambda (u)
	    (and ((list-type-of-length-at-least? 2) u)
		 (can-be? (needs-implicit-continuation-call?
			   (list (pair-type-car u))
			   (pair-type-cdr (pair-type-cdr u))
			   (recreate-call-site y 'first-argument))
			  (pair-type-car (pair-type-cdr u)))))
	   w))
	 ((null? (rest ws))
	  (can-be?
	   (lambda (u)
	    (and ((list-type-of-length-at-least? 1) u)
		 (can-be? (needs-implicit-continuation-call?
			   ws
			   (pair-type-cdr u)
			   (recreate-call-site y 'first-argument))
			  (pair-type-car u))))
	   w))
	 (else
	  (or (and (can-be? null-type? w)
		   (can-be?
		    (needs-implicit-continuation-call?
		     (cons (first ws) (but-last (rest (rest ws))))
		     (last ws)
		     (recreate-call-site y 'first-argument))
		    (second ws)))
	      (can-be? (needs-implicit-continuation-call?
			(cons (first ws) (rest (rest ws)))
			w
			(recreate-call-site y 'first-argument))
		       (second ws))))))
       (and
	((primitive-procedure-type-named? 'call-with-current-continuation) u)
	(or (cond ((null? ws)
		   (can-be?
		    (lambda (u)
		     (and ((list-type-of-length? 2) u)
			  (can-be? (needs-implicit-continuation-call?
				    (list (pair-type-car u) (pair-type-car u))
				    (pair-type-cdr (pair-type-cdr u))
				    (recreate-call-site y 'first-argument))
				   (pair-type-car (pair-type-cdr u)))))
		    w))
		  ((null? (rest ws))
		   (can-be?
		    (lambda (u)
		     (and ((list-type-of-length? 1) u)
			  (can-be? (needs-implicit-continuation-call?
				    (list (first ws) (first ws))
				    (pair-type-cdr u)
				    (recreate-call-site y 'first-argument))
				   (pair-type-car u))))
		    w))
		  (else (can-be?
			 (needs-implicit-continuation-call?
			  (cons (first ws) (cons (first ws) (rest (rest ws))))
			  w
			  (recreate-call-site y 'first-argument))
			 (second ws))))
	    (some (lambda (u)
		   (and ((continuation-type-to? (call-site-expression y)) u)
			(continuation-type-continuation-accessed? u)))
		  *continuation-types*)))
       (and ((primitive-procedure-type-named? 'fork) u)
	    ;; needs work: To handle converted calls to FORK.
	    (unimplemented y "unimplemented"))
       (and ((primitive-procedure-type-named? 'mutex) u)
	    ;; needs work: To handle converted calls to MUTEX.
	    (unimplemented y "unimplemented"))))))

(define (assert-member! u w)
 (unless (member? u w)
  (insert-member! u w)
  (set! *again?* #t)
  ;; This is purely for efficiency.
  ;; It runs the inference rules for an expression when the type set of a
  ;; subexpression is widened.
  (when (and
	 (type-set-location w)
	 (expression? (type-set-location w))
	 (expression? (expression-parent (type-set-location w)))
	 (not (eq? (expression-kind (expression-parent (type-set-location w)))
		   'lambda))
	 (not (eq? (expression-kind (expression-parent (type-set-location w)))
		   'converted-lambda))
	 (not (eq? (expression-kind (expression-parent (type-set-location w)))
		   'converted-continuation)))
   (push! (expression-parent (type-set-location w))))
  ;; This is purely for efficiency.
  ;; It runs the inference rules for a call when the return type set of
  ;; the callee is widened.
  (when (and (type-set-location w)
	     (expression? (type-set-location w))
	     (expression? (expression-parent (type-set-location w)))
	     ;; The next expression is a kludge.
	     (environment? (expression-lambda-environment
			    (expression-parent (type-set-location w)))))
   (for-each
    (lambda (y)
     (unless (top-level-call-site? y) (push! (call-site-expression y))))
    (call-sites (expression-lambda-environment
		 (expression-parent (type-set-location w))))))
  ;; This is purely for efficiency.
  ;; It runs the inference rules for an access when the type set of a
  ;; variable is widened.
  (when (and (type-set-location w) (variable? (type-set-location w)))
   (for-each push! (accesses (type-set-location w))))))

(define (assert-subset! w1 w2)
 (for-each-member (lambda (u1) (assert-member! u1 w2)) w1))

(define (assert-continues! x)
 (unless (expression-continues? x)
  (set-expression-continues?! x #t)
  (set! *again?* #t)))

(define (assert-returns! x)
 (unless (expression-returns? x)
  (set-expression-returns?! x #t)
  (set! *again?* #t)
  ;; This is purely for efficiency.
  ;; It runs the inference rules for an expression when a subexpression
  ;; returns.
  (when (and
	 (expression? (expression-parent x))
	 (not (eq? (expression-kind (expression-parent x)) 'lambda))
	 (not (eq? (expression-kind (expression-parent x)) 'converted-lambda))
	 (not (eq? (expression-kind (expression-parent x))
		   'converted-continuation)))
   (push! (expression-parent x)))
  ;; This is purely for efficiency.
  ;; It runs the inference rules for a call when the callee returns.
  (when (and (expression? (expression-parent x))
	     ;; The next expression is a kludge.
	     (environment?
	      (expression-lambda-environment (expression-parent x))))
   (for-each
    (lambda (y)
     (unless (top-level-call-site? y) (push! (call-site-expression y))))
    (call-sites (expression-lambda-environment (expression-parent x)))))))

(define (assert-expression-reached! x)
 (unless (expression-reached? x)
  (if (and (or (eq? (expression-kind x) 'lambda)
	       (eq? (expression-kind x) 'converted-lambda)
	       (eq? (expression-kind x) 'converted-continuation))
	   ;; The next expression is a kludge.
	   (environment? (expression-lambda-environment x)))
      ;; note: This is because the EXPRESSION-REACHED? slot gets reset for
      ;;       each round of flow analysis and clones are not anchored in
      ;;       lambda expressions so their EXPRESSION-REACHED? slot never gets
      ;;       set on subsequent passes.
      (for-each
       (lambda (e) (set-expression-reached?! (environment-expression e) #t))
       (narrow-clones (expression-lambda-environment x)))
      (set-expression-reached?! x #t))
  (set! *again?* #t)
  ;; This is purely for efficiency.
  ;; It runs the inference rule for an expression when it is used.
  (push! x)))

(define (propagate-call! y w0 ws w)
 (for-each-member
  (lambda (u)
   (when ((truly-compatible-procedure? ws w y) u)
    (cond
     ((primitive-procedure-type? u)
      ;; Top-level calls should never be to a primitive procedure.
      ;; Implicit continuation calls should never be to a primitive procedure.
      (when (or (top-level-call-site? y) (continuation-argument-call-site? y))
       (fuck-up))
      (let ((w0 (if (converted? y) (first ws) #f))
	    (ws (if (converted? y) (rest ws) ws)))
       (let ((propagate-result!
	      (if (converted? y)
		  (lambda (u)
		   (propagate-call!
		    (recreate-call-site y 'continuation-argument)
		    w0 (list (create-anonymous-type-set u)) *null*))
		  (lambda (u)
		   ;; This works because APPLY and
		   ;; CALL-WITH-CURRENT-CONTINUATION always return the result
		   ;; returned by the call to their procedure argument.
		   (assert-member!
		    u (expression-type-set (call-site-expression y)))))))
	;; conventions: PROPAGATE-RESULT!
	(((primitive-procedure-propagate-call!
	   (cdr (assq (primitive-procedure-type-name u)
		      *primitive-procedure-handlers*)))
	  y u propagate-result!
	  (lambda (m)
	   (when (can-be? m (first ws)) (propagate-result! <true>))
	   (when (can-be-non? m (first ws)) (propagate-result! <false>)))
	  w0)
	 ws w))
       ;; For the life of me I can't remember what this does but if it is
       ;; removed then test21.sc and em-functional.sc break.
       ;; On R23May96 I partially rediscovered why this is needed. It is
       ;; because a converted call to a primitive procedure like SET-CAR!
       ;; might not ever call PROPAGATE-RESULT! in the propagator so the
       ;; continuation never gets asserted as called.
       (when (and (converted? y)
		  ((needs-implicit-continuation-call? (cons w0 ws) w y) u))
	(propagate-call! (recreate-call-site y 'continuation-argument)
			 w0 (list *void*) *null*))))
     ((native-procedure-type? u)
      (let ((e (callee-environment u y)))
       (when (and (not (converted? y)) (converted? e)) (fuck-up))
       (unless (noop? e)
	(assert-expression-reached!
	 (expression-body (environment-expression e))))
       (unless (memp same-call-site? y (environment-call-sites e))
	(set-environment-call-sites! e (cons y (environment-call-sites e))))
       (let loop ((ws (if (and (converted? y) (not (converted? e)))
			  (rest ws)
			  ws))
		  (w w)
		  (gs (variables e)))
	(unless (null? gs)
	 (cond
	  ((and (null? (rest gs)) (rest? e))
	   (set-type-set-used?! (variable-type-set (first gs)) #t)
	   (if (null? ws)
	       (assert-subset! w (variable-type-set (first gs)))
	       ;; Since Y appears in the following and Y is #F at the top-level
	       ;; call this assumes that the top level doesn't take a variable
	       ;; number of arguments.
	       ;; note: This is suboptimal since type propagation is not yet
	       ;;       complete and APPLY-CLOSED-WORLD-ASSUMPTION! has not
	       ;;       been done yet.
	       (assert-member! (<pair+> (map members ws)
					(members w)
					(call-site-expression y))
			       (variable-type-set (first gs)))))
	  ((null? ws)
	   (for-each-member
	    (lambda (u)
	     (when ((if (rest? e)
			(list-type-of-length-at-least? (- (length gs) 1))
			(list-type-of-length? (length gs)))
		    u)
	      (set-type-set-used?! (variable-type-set (first gs)) #t)
	      (assert-subset! (pair-type-car u) (variable-type-set (first gs)))
	      (loop ws (pair-type-cdr u) (rest gs))))
	    w))
	  (else (set-type-set-used?! (variable-type-set (first gs)) #t)
		(assert-subset! (first ws) (variable-type-set (first gs)))
		(loop (rest ws) w (rest gs))))))
       ;; Top-level calls don't have an associated type set so there is no
       ;; need to propagate the return type.
       (unless (top-level-call-site? y)
	(when (and (converted? y)
		   ((needs-implicit-continuation-call? ws w y) u))
	 (propagate-call! (recreate-call-site y 'continuation-argument)
			  (first ws) (list (return-type-set e)) *null*))
	;; This works because APPLY and CALL-WITH-CURRENT-CONTINUATION always
	;; return the result returned by the call to their procedure argument
	;; and because implicit continuation calls should never be converted
	;; and should never be through APPLY or CALL-WITH-CURRENT-CONTINUATION.
	(when ((returns? ws w y) u)
	 (assert-subset! (return-type-set e)
			 (expression-type-set (call-site-expression y)))))))
     ((foreign-procedure-type? u)
      ;; Top-level calls should never be to a foreign procedure.
      ;; Implicit continuation calls should never be to a foreign procedure.
      (when (or (top-level-call-site? y) (continuation-argument-call-site? y))
       (fuck-up))
      (set-foreign-procedure-type-called?! u #t)
      (when (and (converted? y) ((needs-implicit-continuation-call? ws w y) u))
       (propagate-call!
	(recreate-call-site y 'continuation-argument)
	(first ws) (list (foreign-procedure-return-type-set u)) *null*))
      ;; This works because APPLY and CALL-WITH-CURRENT-CONTINUATION always
      ;; return the result returned by the call to their procedure argument.
      (when ((returns? ws w y) u)
       (assert-subset! (foreign-procedure-return-type-set u)
		       (expression-type-set (call-site-expression y)))))
     ((continuation-type? u)
      ;; Top-level calls should never be to a continuation.
      ;; Implicit continuation calls should never be to a continuation.
      (when (or (top-level-call-site? y) (continuation-argument-call-site? y))
       (fuck-up))
      ;; Calls to a nonconverted continuation should never be converted (even
      ;; if the argument to the call is converted) because they never return.
      ;; needs work: The above comment is not true because a call site might
      ;;             call both a nonconverted continuation and somethings else
      ;;             which does return and there might be a converted call to
      ;;             a nonconverted continuation.
      (when (converted? y) (unimplemented y "unimplemented"))
      (set-continuation-type-continuation-accessed?! u #t)
      ;; Calls to nonconverted continuations never return.
      (let ((w1 (expression-type-set
		 (continuation-type-allocating-expression u))))
       (set-type-set-used?! w1 #t)
       (if (null? ws)
	   (for-each-member (lambda (u)
			     (when ((list-type-of-length? 1) u)
			      (assert-subset! (pair-type-car u) w1)))
			    w)
	   (assert-subset! (first ws) w1))))
     (else (fuck-up)))))
  w0))

(define (assert-types-in-if-context! us w x g p?)
 ;; conventions: P?
 (let ((x1 (expression-antecedent (expression-parent x))))
  ;; note: Don't chain back through access, source of SET!, antecedent,
  ;;       consequent, or alternate of IF, or callee or arguments of call.
  (cond
   ((eq? (expression-kind x1) 'call)
    (cond
     ((and (not (converted? x1))
	   (= (length (expression-arguments x1)) 1)
	   (eq? (expression-kind (first (expression-arguments x1))) 'call))
      (when (can-be? (primitive-procedure-type-named? 'not)
		     (expression-type-set (expression-callee x1)))
       (let ((x2 (first (expression-arguments x1))))
	(when (eq? (expression-kind x2) 'call)
	 (for-each-member
	  (lambda (u)
	   (when ((truly-compatible-call? x2) u)
	    ;; note: Don't chain back through calls to continuations or native
	    ;;       or foreign procedures.
	    (if (primitive-procedure-type? u)
		(let loop ((us us)
			   (xs (expression-arguments x2))
			   (ps (((if p?
				     primitive-procedure-alternate-contexts
				     primitive-procedure-consequent-contexts)
				 (cdr (assq (primitive-procedure-type-name u)
					    *primitive-procedure-handlers*)))
				(create-call-site x2)
				u
				(length (expression-arguments x2))
				(if (converted? x2)
				    (expression-type-set
				     (first (expression-arguments x2)))
				    #f))))
		 ;; conventions: PS
		 (if (null? xs)
		     (assert-types-in-context! us w (expression-parent x) g)
		     (loop (if (and (eq? (expression-kind (first xs)) 'access)
				    (eq? (expression-variable (first xs)) g))
			       (remove-if-not (first ps) us)
			       us)
			   (rest xs)
			   (rest ps))))
		(assert-types-in-context! us w (expression-parent x) g))))
	  (expression-type-set (expression-callee x2))))))
      (when (can-be-non? (primitive-procedure-type-named? 'not)
			 (expression-type-set (expression-callee x1)))
       (assert-types-in-context! us w (expression-parent x) g)))
     (else
      (for-each-member
       (lambda (u)
	(when ((truly-compatible-call? x1) u)
	 ;; note: Don't chain back through calls to continuations or native or
	 ;;       foreign procedures.
	 (if (primitive-procedure-type? u)
	     (let loop ((us us)
			(xs (expression-arguments x1))
			(ps (((if p?
				  primitive-procedure-consequent-contexts
				  primitive-procedure-alternate-contexts)
			      (cdr (assq (primitive-procedure-type-name u)
					 *primitive-procedure-handlers*)))
			     (create-call-site x1)
			     u
			     (length (expression-arguments x1))
			     (if (converted? x1)
				 (expression-type-set
				  (first (expression-arguments x1)))
				 #f))))
	      ;; conventions: PS
	      (if (null? xs)
		  (assert-types-in-context! us w (expression-parent x) g)
		  (loop (if (and (eq? (expression-kind (first xs)) 'access)
				 (eq? (expression-variable (first xs)) g))
			    (remove-if-not (first ps) us)
			    us)
			(rest xs)
			(rest ps))))
	     (assert-types-in-context! us w (expression-parent x) g))))
       (expression-type-set (expression-callee x1))))))
   ((eq? (expression-kind x1) 'access)
    (assert-types-in-context!
     (if (eq? (expression-variable x1) g)
	 (remove-if-not (if p? (lambda (u) (not (false-type? u))) false-type?)
			us)
	 us)
     w (expression-parent x) g))
   (else (assert-types-in-context! us w (expression-parent x) g)))))

(define (assert-types-in-context! us w x g)
 (when (expression? (expression-parent x))
  (case (expression-kind (expression-parent x))
   ((lambda converted-lambda converted-continuation)
    ;; note: Don't chain back through callers except for LETs. Would have to
    ;;       union across callers using demon.
    (if (let? (expression-lambda-environment (expression-parent x)))
	(assert-types-in-context! us w (expression-parent x) g)
	(for-each (lambda (u) (assert-member! u w)) us)))
   ((if)
    (cond ((eq? x (expression-consequent (expression-parent x)))
	   (assert-types-in-if-context! us w x g #t))
	  ((eq? x (expression-alternate (expression-parent x)))
	   (assert-types-in-if-context! us w x g #f))
	  (else (assert-types-in-context! us w (expression-parent x) g))))
   (else (assert-types-in-context! us w (expression-parent x) g)))))

(define (infer! x)
 (when (expression-reached? x)
  (let ((w (expression-type-set x)))
   (set-type-set-used?! w #t)
   (case (expression-kind x)
    ((null-constant)
     (unless (expression-inferred? x)
      (assert-member! <null> w)
      (assert-continues! x)
      (assert-returns! x)))
    ((true-constant)
     (unless (expression-inferred? x)
      (assert-member! <true> w)
      (assert-continues! x)
      (assert-returns! x)))
    ((false-constant)
     (unless (expression-inferred? x)
      (assert-member! <false> w)
      (assert-continues! x)
      (assert-returns! x)))
    ((char-constant)
     (unless (expression-inferred? x)
      (assert-member! <char> w)
      (assert-continues! x)
      (assert-returns! x)))
    ((fixnum-constant)
     (unless (expression-inferred? x)
      (assert-member! <fixnum> w)
      (assert-continues! x)
      (assert-returns! x)))
    ((flonum-constant)
     (unless (expression-inferred? x)
      (assert-member! <flonum> w)
      (assert-continues! x)
      (assert-returns! x)))
    ((rectangular-constant)
     (unless (expression-inferred? x)
      (assert-member! <rectangular> w)
      (assert-continues! x)
      (assert-returns! x)))
    ((string-constant)
     (unless (expression-inferred? x)
      (assert-member! <nonreclaimable-string> w)
      (assert-continues! x)
      (assert-returns! x)))
    ((symbol-constant)
     (unless (expression-inferred? x)
      (assert-member! (if *treat-all-symbols-as-external?*
			  (<external-symbol> <nonreclaimable-string>)
			  (<internal-symbol> (expression-constant x)))
		      w)
      (assert-continues! x)
      (assert-returns! x)))
    ((pair-constant)
     (unless (expression-inferred? x)
      (assert-expression-reached! (car (expression-constant x)))
      (assert-expression-reached! (cdr (expression-constant x))))
     (assert-member!
      (<pair>
       ;; note: This is suboptimal since type propagation is not yet complete
       ;;       and APPLY-CLOSED-WORLD-ASSUMPTION! has not been done yet.
       (members (expression-type-set (car (expression-constant x))))
       (members (expression-type-set (cdr (expression-constant x))))
       x)
      w)
     (unless (expression-inferred? x)
      (assert-continues! x)
      (assert-returns! x)))
    ((vector-constant)
     (unless (expression-inferred? x)
      (for-each-vector assert-expression-reached! (expression-constant x)))
     (assert-member!
      (<headed-vector>
       ;; note: This is suboptimal since type propagation is not yet complete
       ;;       and APPLY-CLOSED-WORLD-ASSUMPTION! has not been done yet.
       (reduce-vector
	unionq
	(map-vector (lambda (x1) (members (expression-type-set x1)))
		    (expression-constant x))
	'())
       x)
      w)
     (unless (expression-inferred? x)
      (assert-continues! x)
      (assert-returns! x)))
    ((lambda converted-lambda converted-continuation)
     (unless (expression-inferred? x)
      (assert-member! (<native-procedure> (expression-lambda-environment x)) w)
      (assert-continues! x)
      (assert-returns! x)))
    ((set!)
     (assert-expression-reached! (expression-source x))
     (set-type-set-used?! (variable-type-set (expression-variable x)) #t)
     (assert-subset! (expression-type-set (expression-source x))
		     (variable-type-set (expression-variable x)))
     (when (expression-continues? (expression-source x)) (assert-continues! x))
     (when (expression-returns? (expression-source x)) (assert-returns! x)))
    ((if)
     (assert-expression-reached! (expression-antecedent x))
     (when (can-be-non? false-type?
			(expression-type-set (expression-antecedent x)))
      (assert-expression-reached! (expression-consequent x))
      (assert-subset! (expression-type-set (expression-consequent x)) w))
     (when (can-be? false-type?
		    (expression-type-set (expression-antecedent x)))
      (assert-expression-reached! (expression-alternate x))
      (assert-subset! (expression-type-set (expression-alternate x)) w))
     (when (and (expression-continues? (expression-antecedent x))
		(or (expression-continues? (expression-consequent x))
		    (expression-continues? (expression-alternate x))))
      (assert-continues! x))
     (when (and (expression-returns? (expression-antecedent x))
		(or (expression-returns? (expression-consequent x))
		    (expression-returns? (expression-alternate x))))
      (assert-returns! x)))
    ((primitive-procedure)
     (unless (expression-inferred? x)
      (assert-member! (<primitive-procedure> (first (expression-constant x))
					     (rest (expression-constant x)))
		      w)
      (assert-continues! x)
      (assert-returns! x)))
    ((foreign-procedure)
     (unless (expression-inferred? x)
      (assert-member!
       (<foreign-procedure> (third (expression-constant x))
			    (first (expression-constant x))
			    (second (expression-constant x))
			    (and (= (length (expression-constant x)) 4)
				 (fourth (expression-constant x))))
       w)
      (assert-continues! x)
      (assert-returns! x)))
    ((access)
     (set-type-set-used?! (variable-type-set (expression-variable x)) #t)
     ;; needs work: Should ignore assignments that aren't executed.
     ;; needs work: Should ignore void assignments.
     (if (null? (assignments (expression-variable x)))
	 (assert-types-in-context!
	  (members (variable-type-set (expression-variable x)))
	  w
	  x
	  (expression-variable x))
	 (assert-subset! (variable-type-set (expression-variable x)) w))
     (assert-continues! x)
     (assert-returns! x))
    ((call)
     ;; needs work: Only assert a subexpression as used if the previous
     ;;             subexpression returns. This assumes a left-to-right
     ;;             evaluation order. As Olin Shivers pointed out, since
     ;;             evaluation order is unspecified you can abort if any
     ;;             subexpression doesn't return. But given the way the
     ;;             propagator works, we can only determine whether an
     ;;             expression returns by asserting it as used. So we have to
     ;;             pick some order and it might as well be left to right.
     (assert-expression-reached! (expression-callee x))
     (for-each assert-expression-reached! (expression-arguments x))
     (when (executed? x)
      (propagate-call! (create-call-site x)
		       (expression-type-set (expression-callee x))
		       (map expression-type-set (expression-arguments x))
		       *null*)
      (when (can-be?
	     (continues? (map expression-type-set (expression-arguments x))
			 *null*
			 (create-call-site x))
	     (expression-type-set (expression-callee x)))
       (assert-continues! x))
      (when (can-be?
	     (returns? (map expression-type-set (expression-arguments x))
		       *null*
		       (create-call-site x))
	     (expression-type-set (expression-callee x)))
       (assert-returns! x))))
    ((converted-call)
     ;; needs work: Only assert a subexpression as used if the previous
     ;;             subexpression returns. This assumes a left-to-right
     ;;             evaluation order. As Olin Shivers pointed out, since
     ;;             evaluation order is unspecified you can abort if any
     ;;             subexpression doesn't return. But given the way the
     ;;             propagator works, we can only determine whether an
     ;;             expression returns by asserting it as used. So we have to
     ;;             pick some order and it might as well be left to right.
     (assert-expression-reached! (expression-callee x))
     (for-each assert-expression-reached! (expression-arguments x))
     (when (executed? x)
      (propagate-call! (create-call-site x)
		       (expression-type-set (expression-callee x))
		       (map expression-type-set (expression-arguments x))
		       *null*)
      (when (can-be?
	     (continues? (map expression-type-set (expression-arguments x))
			 *null*
			 (create-call-site x))
	     (expression-type-set (expression-callee x)))
       (assert-continues! x))
      (when (can-be?
	     (returns? (map expression-type-set (expression-arguments x))
		       *null*
		       (create-call-site x))
	     (expression-type-set (expression-callee x)))
       (assert-returns! x))))
    (else (fuck-up))))
  (set-expression-inferred?! x #t)))

(define (push! x)
 (unless (expression-link x)
  (set-expression-link! x (if *x1* *x1* #t))
  (set! *x1* x)))

(define (run-stack!)
 (let loop ()
  (when *x1*
   (let ((x *x1*))
    (set! *x1* (if (eq? (expression-link x) #t) #f (expression-link x)))
    (set-expression-link! x #f)
    (infer! x)
    (loop)))))

(define (perform-flow-analysis!)
 (set! *types-frozen?* #f)
 (reinitialize-types-and-type-sets!)
 (for-each
  (lambda (x)
   ;; Only narrow prototypes get a type set.
   (when (or (and (not (eq? (expression-kind x) 'lambda))
		  (not (eq? (expression-kind x) 'converted-lambda))
		  (not (eq? (expression-kind x) 'converted-continuation)))
	     ;; The next expression is a kludge.
	     (not (environment? (expression-lambda-environment x)))
	     (eq? (narrow-prototype (expression-lambda-environment x))
		  (expression-lambda-environment x)))
    (set-expression-type-set! x (create-type-set x)))
   (set-expression-reached?! x #f)
   (set-expression-inferred?! x #f)
   (set-expression-returns?! x #f))
  *xs*)
 (for-each
  (lambda (x)
   ;; Clones that are not narrow prototypes share their type set with their
   ;; narrow prototype.
   (when (and (or (eq? (expression-kind x) 'lambda)
		  (eq? (expression-kind x) 'converted-lambda)
		  (eq? (expression-kind x) 'converted-continuation))
	      ;; The next expression is a kludge.
	      (environment? (expression-lambda-environment x))
	      (not (eq? (narrow-prototype (expression-lambda-environment x))
			(expression-lambda-environment x))))
    (set-expression-type-set!
     x (expression-type-set
	(environment-expression
	 (narrow-prototype (expression-lambda-environment x)))))))
  *xs*)
 (for-each (lambda (e) (set-environment-call-sites! e '())) *es*)
 (for-each (lambda (g) (set-variable-type-set! g (create-type-set g))) *gs*)
 (assert-expression-reached! *x*)
 (let loop ()
  (clock-sample)			;To prevent overflow.
  (set! *again?* #f)
  (propagate-call! *y* (expression-type-set *x*) (list *w1*) *null*)
  (run-stack!)
  (for-each push! *xs*)
  (run-stack!)
  (when *again?* (loop)))
 (set! *types-frozen?* #t))

;;; Enumerate call sites

(define (enumerate-call-sites!)
 (set! *ys* '())
 (for-each
  (lambda (x)
   (when (and (executed? x)
	      (not (empty? (expression-environment x)))
	      (environment-used? (expression-environment x)))
    (set! *ys* (cons (create-call-site x) *ys*))
    (when (can-be? (lambda (u)
		    (and (or ((primitive-procedure-type-named? 'apply) u)
			     ((primitive-procedure-type-named?
			       'call-with-current-continuation)
			      u)
			     ((primitive-procedure-type-named? 'fork) u)
			     ((primitive-procedure-type-named? 'mutex) u))
			 ((truly-compatible-call? x) u)))
		   (expression-type-set (expression-callee x)))
     (set! *ys*
	   (cons (recreate-call-site (create-call-site x) 'first-argument)
		 *ys*)))
    (when (can-be? (lambda (u)
		    (and ((primitive-procedure-type-named? 'fork) u)
			 ((truly-compatible-call? x) u)))
		   (expression-type-set (expression-callee x)))
     (set! *ys*
	   (cons (recreate-call-site (create-call-site x) 'second-argument)
		 *ys*)))))
  *calls*))

;;; Determine which types and type sets are used

(define (determine-which-types-and-type-sets-are-used!)
 (let loop ()
  (let ((again? #f))
   (define (assert-used! u)
    (cond
     ((null-type? u) (set! *null-type-used?* #t))
     ((true-type? u) (set! *true-type-used?* #t))
     ((false-type? u) (set! *false-type-used?* #t))
     ((char-type? u) (set! *char-type-used?* #t))
     ((fixnum-type? u) (set! *fixnum-type-used?* #t))
     ((flonum-type? u) (set! *flonum-type-used?* #t))
     ((rectangular-type? u) (set! *rectangular-type-used?* #t))
     ((input-port-type? u) (set! *input-port-type-used?* #t))
     ((output-port-type? u) (set! *output-port-type-used?* #t))
     ((eof-object-type? u) (set! *eof-object-type-used?* #t))
     ((pointer-type? u) (set! *pointer-type-used?* #t))
     ((internal-symbol-type? u) (set-internal-symbol-type-used?! u #t))
     ((external-symbol-type? u)
      (unless (type-used? u)
       (set-external-symbol-type-used?! u #t)
       (set! again? #t)))
     ((primitive-procedure-type? u) (set-primitive-procedure-type-used?! u #t))
     ((native-procedure-type? u) (set-native-procedure-type-used?! u #t))
     ((foreign-procedure-type? u) (set-foreign-procedure-type-used?! u #t))
     ((continuation-type? u)
      (unless (type-used? u)
       (set-continuation-type-used?! u #t)))
     ((string-type? u) (set-string-type-used?! u #t))
     ((structure-type? u)
      (unless (type-used? u)
       (set-structure-type-used?! u #t)
       (set! again? #t)))
     ((headed-vector-type? u)
      (unless (type-used? u)
       (set-headed-vector-type-used?! u #t)
       (set! again? #t)))
     ((nonheaded-vector-type? u)
      (unless (type-used? u)
       (set-nonheaded-vector-type-used?! u #t)
       (set! again? #t)))
     ((displaced-vector-type? u)
      (unless (type-used? u)
       (set-displaced-vector-type-used?! u #t)
       (set! again? #t)))
     (else (fuck-up))))
   (for-each (lambda (u)
	      (when (type-used? u)
	       (assert-used! (external-symbol-type-displaced-string-type u))))
	     *external-symbol-types*)
   (for-each (lambda (u)
	      (when (type-used? u)
	       (for-each (lambda (w)
			  (unless (type-set-used? w)
			   (set-type-set-used?! w #t)
			   (set! again? #t)))
			 (structure-type-slots u))))
	     *structure-types*)
   (for-each (lambda (u)
	      (when (type-used? u)
	       (unless (type-set-used? (headed-vector-type-element u))
		(set-type-set-used?! (headed-vector-type-element u) #t)
		(set! again? #t))))
	     *headed-vector-types*)
   (for-each (lambda (u)
	      (when (type-used? u)
	       (unless (type-set-used? (nonheaded-vector-type-element u))
		(set-type-set-used?! (nonheaded-vector-type-element u) #t)
		(set! again? #t))))
	     *nonheaded-vector-types*)
   (for-each (lambda (u)
	      (when (type-used? u)
	       (assert-used! (displaced-vector-type-displaced-vector-type u))))
	     *displaced-vector-types*)
   (for-each
    (lambda (w)
     (when (type-set-used? w) (for-each-member assert-used! w)))
    *ws*)
   (when again? (loop)))))

(define (replace-expression! x1 x2)
 (case (expression-kind (expression-parent x1))
  ((lambda converted-lambda converted-continuation)
   (unless (eq? (expression-body (expression-parent x1)) x1) (fuck-up))
   (set-expression-body! (expression-parent x1) x2))
  ((set!)
   (unless (eq? (expression-source (expression-parent x1)) x1) (fuck-up))
   (set-expression-source! (expression-parent x1) x2))
  ((if)
   (cond ((eq? (expression-antecedent (expression-parent x1)) x1)
	  (set-expression-antecedent! (expression-parent x1) x2))
	 ((eq? (expression-consequent (expression-parent x1)) x1)
	  (set-expression-consequent! (expression-parent x1) x2))
	 ((eq? (expression-alternate (expression-parent x1)) x1)
	  (set-expression-alternate! (expression-parent x1) x2))
	 (else (fuck-up))))
  ((call converted-call)
   (cond ((eq? (expression-callee (expression-parent x1)) x1)
	  (set-expression-callee! (expression-parent x1) x2))
	 ((one (lambda (x3) (eq? x3 x1))
	       (expression-arguments (expression-parent x1)))
	  (set-expression-arguments!
	   (expression-parent x1)
	   (map (lambda (x3) (if (eq? x3 x1) x2 x3))
		(expression-arguments (expression-parent x1)))))
	 (else (fuck-up))))
  (else (fuck-up))))

(define (remove-unused-objects! p?)
 (when p?
  (set! *xs* (remove-if-not reached? *xs*))
  (set! *calls* (remove-if-not reached? *calls*))
  (set! *accesses* (remove-if-not reached? *accesses*))
  (set! *assignments* (remove-if-not reached? *assignments*))
  (set! *references* (remove-if-not reached? *references*))
  (for-each (lambda (x)
	     (when (and (or (eq? (expression-kind x) 'lambda)
			    (eq? (expression-kind x) 'converted-lambda)
			    (eq? (expression-kind x) 'converted-continuation))
			(not (called? (expression-lambda-environment x))))
	      (set-expression-parameters! x (unspecified)))
	     (when (and (or (eq? (expression-kind x) 'lambda)
			    (eq? (expression-kind x) 'converted-lambda)
			    (eq? (expression-kind x) 'converted-continuation))
			(not (called? (expression-lambda-environment x))))
	      (set-expression-lambda-environment! x (unspecified))))
	    *xs*)
  (set! *internal-symbol-types*
	(remove-if-not type-used? *internal-symbol-types*))
  (set! *external-symbol-types*
	(remove-if-not type-used? *external-symbol-types*))
  (set! *primitive-procedure-types*
	(remove-if-not type-used? *primitive-procedure-types*))
  (set! *native-procedure-types*
	(remove-if-not type-used? *native-procedure-types*))
  (set! *foreign-procedure-types*
	(remove-if-not type-used? *foreign-procedure-types*))
  (set! *continuation-types* (remove-if-not type-used? *continuation-types*))
  (set! *string-types* (remove-if-not type-used? *string-types*))
  (set! *structure-types* (remove-if-not type-used? *structure-types*))
  (set! *headed-vector-types* (remove-if-not type-used? *headed-vector-types*))
  (set! *nonheaded-vector-types*
	(remove-if-not type-used? *nonheaded-vector-types*))
  (set! *displaced-vector-types*
	(remove-if-not type-used? *displaced-vector-types*)))
 (for-each (lambda (u)
	    (set-native-procedure-type-call-site-environment-alist!
	     u
	     (remove-if-not
	      (lambda (y-e)
	       (and (or (top-level-call-site? (car y-e))
			(reached? (call-site-expression (car y-e))))
		    (called? (cdr y-e))))
	      (native-procedure-type-call-site-environment-alist u)))
	    (when p?
	     (unless (called? (native-procedure-type-narrow-prototype u))
	      (set-native-procedure-type-narrow-prototype!
	       u (find-if called? (narrow-clones u))))))
	   *native-procedure-types*)
 (when p? (set! *ws* (remove-if-not type-set-used? *ws*)))
 (for-each (lambda (w) (set-members! w (members-that type-used? w))) *ws*)
 (when p?
  (set! *gs* (remove-if-not variable-used? *gs*))
  (for-each (lambda (g)
	     (set-variable-accesses!
	      g (remove-if-not reached? (variable-accesses g)))
	     (set-variable-assignments!
	      g (remove-if-not reached? (variable-assignments g)))
	     (set-variable-references!
	      g (remove-if-not reached? (variable-references g))))
	    *gs*)
  (for-each
   (lambda (e)
    (when (and (eq? (narrow-prototype e) e)
	       (not (called? e))
	       (some
		(lambda (e1)
		 (and (called? e1)
		      (eq? (expression-accessed? (environment-expression e1))
			   (expression-accessed? (environment-expression e)))))
		(narrow-clones e)))
     (replace-expression!
      (environment-expression e)
      (environment-expression
       (find-if (lambda (e1)
		 (and (called? e1)
		      (eq? (expression-accessed? (environment-expression e1))
			   (expression-accessed? (environment-expression e)))))
		(narrow-clones e))))))
   *es*)
  (for-each (lambda (e)
	     (set-environment-narrow-clones!
	      e (remove-if-not called? (narrow-clones (narrow-prototype e))))
	     (set-environment-parent-parameter! e (parent-parameter e)))
	    *es*)
  (for-each (lambda (e)
	     (when (and (not (called? (environment-narrow-prototype e)))
			;; This is a kludge.
			(some called? (narrow-clones e)))
	      (set-environment-narrow-prototype!
	       e (find-if called? (narrow-clones e)))))
	    *es*)
  (for-each (lambda (e)
	     (unless (eq? e (environment-narrow-prototype e))
	      (set-environment-narrow-clones! e '())
	      (set-environment-parent-parameter! e (unspecified))))
	    *es*)
  (let ((es (remove-if-not called? *es*)))
   (for-each
    set-environment-wide-prototype!
    es
    (map (lambda (e)
	  (find-if (lambda (e1)
		    (and (eq? (environment-narrow-prototype e1) e1)
			 (eq? (environment-wide-prototype e1)
			      (environment-wide-prototype e))))
		   es))
	 es)))
  ;; This is just because of *CLOSURE-CONVERSION-METHOD*.
  (for-each (lambda (e)
	     (when (environment-used? e)
	      (set-environment-expressions!
	       e (remove-if-not reached? (environment-expressions e)))))
	    *es*)
  ;; This is just because of *CLOSURE-CONVERSION-METHOD*.
  (for-each
   (lambda (e)
    (when (environment-used? e)
     (set-environment-continuation-calls!
      e (remove-if-not reached? (environment-continuation-calls e)))))
   *es*)
  (set! *es0* (remove-if-not (lambda (e) (and (called? e) (noop? e))) *es*))
  (set! *es* (remove-if-not environment-used? *es*))))

(define (check-for-corruption p?)
 (define (memq-*us* u)
  (cond ((internal-symbol-type? u) (memq u *internal-symbol-types*))
	((external-symbol-type? u) (memq u *external-symbol-types*))
	((primitive-procedure-type? u) (memq u *primitive-procedure-types*))
	((native-procedure-type? u) (memq u *native-procedure-types*))
	((foreign-procedure-type? u) (memq u *foreign-procedure-types*))
	((continuation-type? u) (memq u *continuation-types*))
	((string-type? u) (memq u *string-types*))
	((structure-type? u) (memq u *structure-types*))
	((headed-vector-type? u) (memq u *headed-vector-types*))
	((nonheaded-vector-type? u) (memq u *nonheaded-vector-types*))
	((displaced-vector-type? u) (memq u *displaced-vector-types*))
	(else #t)))
 (let ((p0? #f)				;*XS*
       (p1? #f)				;*ES*
       (p2? #f)				;*WS*
       (p3? #f)				;*GS*
       (p4? #f))			;*US*
  (unless (and
	   (nonheaded-vector-type? (the-member *w1*))
	   (string-type?
	    (the-member (nonheaded-vector-type-element (the-member *w1*)))))
   (fuck-up))
  (unless (void? *void*) (fuck-up))
  (unless (null-type? (the-member *null*)) (fuck-up))
  (unless (input-port-type? (the-member *input-port*)) (fuck-up))
  (unless (output-port-type? (the-member *output-port*)) (fuck-up))
  (unless (char-type? (the-member *foreign-char-type-set*)) (fuck-up))
  (unless (fixnum-type? (the-member *foreign-fixnum-type-set*)) (fuck-up))
  (unless (flonum-type? (the-member *foreign-flonum-type-set*)) (fuck-up))
  (unless (string-type? (the-member *foreign-string-type-set*)) (fuck-up))
  (unless (input-port-type? (the-member *foreign-input-port-type-set*))
   (fuck-up))
  (unless (output-port-type? (the-member *foreign-output-port-type-set*))
   (fuck-up))
  (unless (pointer-type? (the-member *foreign-pointer-type-set*)) (fuck-up))
  (when p0?
   (unless (subsetq? *calls* *xs*) (fuck-up))
   (unless (subsetq? *accesses* *xs*) (fuck-up))
   (unless (subsetq? *assignments* *xs*) (fuck-up))
   (unless (subsetq? *references* *xs*) (fuck-up))
   (let loop ((x *x*))
    (when (reached? x)
     (unless (memq x *xs*) (fuck-up))
     (case (expression-kind x)
      ((null-constant) #f)
      ((true-constant) #f)
      ((false-constant) #f)
      ((char-constant) #f)
      ((fixnum-constant) #f)
      ((flonum-constant) #f)
      ((rectangular-constant) #f)
      ((string-constant) #f)
      ((symbol-constant) #f)
      ((pair-constant)
       (loop (car (expression-constant x)))
       (loop (cdr (expression-constant x))))
      ((vector-constant)
       (for-each-vector loop (expression-constant x)))
      ((lambda converted-lambda converted-continuation)
       (unless (noop? x) (loop (expression-body x))))
      ((set!) (loop (expression-source x)))
      ((if)
       (loop (expression-antecedent x))
       (loop (expression-consequent x))
       (loop (expression-alternate x)))
      ((primitive-procedure) #f)
      ((foreign-procedure) #f)
      ((access) #f)
      ((call converted-call)
       (loop (expression-callee x))
       (for-each loop (expression-arguments x)))
      (else (fuck-up))))))
  (for-each
   (lambda (x)
    ;; EXPRESSION-LINK
    (when (and (reached? x)
	       (environment? (expression-environment x))
	       (not (called? (expression-environment x))))
     (fuck-up))
    (when (and p1?
	       (environment? (expression-environment x))
	       (not (memq (expression-environment x) *es*)))
     (fuck-up))
    (when (and (reached? x) (not (type-set-used? (expression-type-set x))))
     (fuck-up))
    (when (and p2?
	       (reached? x)
	       (not (memq (expression-type-set x) *ws*))) (fuck-up))
    (when (and (reached? x)
	       (expression? (expression-parent x))
	       (not (reached? (expression-parent x))))
     (fuck-up))
    (when (and p0?
	       (expression? (expression-parent x))
	       (not (memq (expression-parent x) *xs*)))
     (fuck-up))
    ;; EXPRESSION-CONSTANT
    (when (and p?
	       (environment? (expression-lambda-environment x))
	       (not (called? (expression-lambda-environment x))))
     (fuck-up))
    (when (and p1?
	       (environment? (expression-lambda-environment x))
	       (not (noop? (expression-lambda-environment x)))
	       (not (memq (expression-lambda-environment x) *es*)))
     (fuck-up))
    (when (and p3?
	       (or (variable? (expression-parameters x))
		   (pair? (expression-parameters x))))
     (let loop ((gs (expression-parameters x)))
      (cond ((pair? gs)
	     (unless (memq (first gs) *gs*) (fuck-up))
	     (loop (rest gs)))
	    ((variable? gs) (unless (memq gs *gs*) (fuck-up)))
	    ((null? gs) #f)
	    (else (fuck-up)))))
    ;; EXPRESSION-BODY
    (when (and p3?
	       (variable? (expression-variable x))
	       (not (memq (expression-variable x) *gs*)))
     (fuck-up))
    (when (and (reached? x)
	       (expression? (expression-source x))
	       (not (reached? (expression-source x))))
     (fuck-up))
    (when (and p0?
	       (expression? (expression-source x))
	       (not (memq (expression-source x) *xs*)))
     (fuck-up))
    (when (and (reached? x)
	       (expression? (expression-antecedent x))
	       (not (reached? (expression-antecedent x))))
     (fuck-up))
    (when (and p0?
	       (expression? (expression-antecedent x))
	       (not (memq (expression-antecedent x) *xs*)))
     (fuck-up))
    ;; EXPRESSION-CONSEQUENT
    ;; EXPRESSION-ALTERNATE
    (when (and (reached? x)
	       (expression? (expression-callee x))
	       (not (reached? (expression-callee x))))
     (fuck-up))
    (when (and p0?
	       (expression? (expression-callee x))
	       (not (memq (expression-callee x) *xs*)))
     (fuck-up))
    (when (list? (expression-arguments x))
     (when (reached? x)
      (for-each (lambda (x) (unless (reached? x) (fuck-up)))
		(expression-arguments x)))
     (when p0?
      (for-each (lambda (x) (unless (memq x *xs*) (fuck-up)))
		(expression-arguments x))))
    ;; EXPRESSION-ORIGINAL-EXPRESSION
    (when p1?
     (for-each (lambda (u-e)
		(when (or (not (type-used? (car u-e)))
			  (not (memq-*us* (car u-e)))
			  (and (region-allocation? (cdr u-e))
			       (or (not (called? (cdr u-e)))
				   (not (memq (cdr u-e) *es*)))))
		 (fuck-up)))
	       (expression-type-allocation-alist x))))
   *xs*)
  (for-each
   (lambda (u)
    ;; EXTERNAL-SYMBOL-TYPE-LINK
    (unless (type-used? (external-symbol-type-displaced-string-type u))
     (fuck-up))
    (when (and p4?
	       (not (memq (external-symbol-type-displaced-string-type u)
			  *string-types*)))
     (fuck-up)))
   *external-symbol-types*)
  (for-each
   (lambda (u)
    (when (and (native-procedure-type-narrow-prototype u)
	       (not (eq? (environment-narrow-prototype
			  (native-procedure-type-narrow-prototype u))
			 (native-procedure-type-narrow-prototype u))))
     (fuck-up))
    (when (and p?
	       (native-procedure-type-narrow-prototype u)
	       (not (called? (native-procedure-type-narrow-prototype u))))
     (fuck-up))
    (when (and p?
	       p1?
	       (native-procedure-type-narrow-prototype u)
	       (not (noop? (native-procedure-type-narrow-prototype u)))
	       (not (memq (native-procedure-type-narrow-prototype u) *es*)))
     (fuck-up))
    (for-each
     (lambda (y-e)
      (when (and (call-site? (car y-e))
		 (not (reached? (call-site-expression (car y-e)))))
       (fuck-up))
      (when (and p0?
		 (call-site? (car y-e))
		 (not (memq (call-site-expression (car y-e)) *xs*)))
       (fuck-up))
      (unless (called? (cdr y-e)) (fuck-up))
      (when (and p1? (not (noop? (cdr y-e))) (not (memq (cdr y-e) *es*)))
       (fuck-up))
      (unless (memq (cdr y-e) (narrow-clones (cdr y-e))) (fuck-up)))
     (native-procedure-type-call-site-environment-alist u)))
   *native-procedure-types*)
  (for-each
   (lambda (u)
    (when (and (expression? (continuation-type-allocating-expression u))
	       (not (reached? (continuation-type-allocating-expression u))))
     (fuck-up))
    (when (and p0?
	       (expression? (continuation-type-allocating-expression u))
	       (not (memq (continuation-type-allocating-expression u) *xs*)))
     (fuck-up))
    ;; This is a real kludge.
    (unless (eq? (continuation-type-call-sites u) (unspecified))
     (for-each
      (lambda (y)
       (unless (reached? (call-site-expression y)) (fuck-up))
       (when (and p0? (not (memq (call-site-expression y) *xs*))) (fuck-up)))
      (continuation-type-call-sites u))))
   *continuation-types*)
  (for-each
   (lambda (u)
    ;; STRING-TYPE-LINK
    (for-each
     (lambda (x)
      (when (and (expression? x) (not (reached? x))) (fuck-up))
      (when (and p0? (expression? x) (not (memq x *xs*))) (fuck-up)))
     (string-type-allocating-expressions u)))
   *string-types*)
  (for-each
   (lambda (u)
    (for-each (lambda (w)
	       (unless (type-set-used? w) (fuck-up))
	       (when (and p2? (not (memq w *ws*))) (fuck-up)))
	      (structure-type-slots u))
    ;; STRUCTURE-TYPE-LINK
    (for-each
     (lambda (x)
      (when (and (expression? x) (not (reached? x))) (fuck-up))
      (when (and p0? (expression? x) (not (memq x *xs*))) (fuck-up)))
     (structure-type-allocating-expressions u)))
   *structure-types*)
  (for-each
   (lambda (u)
    (unless (type-set-used? (headed-vector-type-element u)) (fuck-up))
    (when (and p2? (not (memq (headed-vector-type-element u) *ws*)))
     (fuck-up))
    ;; HEADED-VECTOR-TYPE-LINK
    (for-each
     (lambda (x)
      (when (and (expression? x) (not (reached? x))) (fuck-up))
      (when (and p0? (expression? x) (not (memq x *xs*))) (fuck-up)))
     (headed-vector-type-allocating-expressions u)))
   *headed-vector-types*)
  (for-each
   (lambda (u)
    (unless (type-set-used? (nonheaded-vector-type-element u)) (fuck-up))
    (when (and p2? (not (memq (nonheaded-vector-type-element u) *ws*)))
     (fuck-up))
    ;; NONHEADED-VECTOR-TYPE-LINK
    (for-each
     (lambda (x)
      (when (and (expression? x) (not (reached? x))) (fuck-up))
      (when (and p0? (expression? x) (not (memq x *xs*))) (fuck-up)))
     (nonheaded-vector-type-allocating-expressions u)))
   *nonheaded-vector-types*)
  (for-each
   (lambda (u)
    (unless (type-used? (displaced-vector-type-displaced-vector-type u))
     (fuck-up))
    ;; DISPLACED-VECTOR-TYPE-LINK
    (when (and
	   p4?
	   (not (memq-*us* (displaced-vector-type-displaced-vector-type u))))
     (fuck-up)))
   *displaced-vector-types*)
  (for-each
   (lambda (w)
    (when (and (expression? (type-set-location w))
	       (not (reached? (type-set-location w))))
     (fuck-up))
    (when (and p0?
	       (expression? (type-set-location w))
	       (not (memq (type-set-location w) *xs*)))
     (fuck-up))
    (when (and p3?
	       (variable? (type-set-location w))
	       (not (memq (type-set-location w) *gs*)))
     (fuck-up))
    (when (and (structure-type? (type-set-location w))
	       (not (type-used? (type-set-location w))))
     (fuck-up))
    (when (and p4?
	       (structure-type? (type-set-location w))
	       (not (memq (type-set-location w) *structure-types*)))
     (fuck-up))
    (when (and (headed-vector-type? (type-set-location w))
	       (not (type-used? (type-set-location w))))
     (fuck-up))
    (when (and p4?
	       (headed-vector-type? (type-set-location w))
	       (not (memq (type-set-location w) *headed-vector-types*)))
     (fuck-up))
    (when (and (nonheaded-vector-type? (type-set-location w))
	       (not (type-used? (type-set-location w))))
     (fuck-up))
    (when (and p4?
	       (nonheaded-vector-type? (type-set-location w))
	       (not (memq (type-set-location w) *nonheaded-vector-types*)))
     (fuck-up))
    ;; TYPE-SET-LINK
    (for-each-member (lambda (u)
		      (unless (type-used? u) (fuck-up))
		      (when (and p4? (not (memq-*us* u))) (fuck-up)))
		     w))
   *ws*)
  (for-each (lambda (g)
	     (when (and p? (not (variable-used? g))) (fuck-up))
	     (when (and p1?
			(not (noop? (variable-environment g)))
			(not (memq (variable-environment g) *es*)))
	      (fuck-up))
	     (when (and (variable-used? g)
			(not (type-set-used? (variable-type-set g))))
	      (fuck-up))
	     (when (and p2?
			(variable-used? g)
			(not (memq (variable-type-set g) *ws*)))
	      (fuck-up))
	     (for-each (lambda (x)
			(unless (reached? x) (fuck-up))
			(when (and p0? (not (memq x *xs*))) (fuck-up)))
		       (accesses g))
	     (for-each (lambda (x)
			(unless (reached? x) (fuck-up))
			(when (and p0? (not (memq x *xs*))) (fuck-up)))
		       (assignments g))
	     (for-each (lambda (x)
			(unless (reached? x) (fuck-up))
			(when (and p0? (not (memq x *xs*))) (fuck-up)))
		       (references g)))
	    *gs*)
  (for-each
   (lambda (e)
    (when (environment-used? e)
     (when (and (environment-expression e)
		(not (reached? (environment-expression e))))
      (fuck-up))
     (when (and p0?
		(environment-expression e)
		(not (memq (environment-expression e) *xs*)))
      (fuck-up))
     (for-each (lambda (y)
		(unless (or (top-level-call-site? y)
			    (reached? (call-site-expression y)))
		 (fuck-up))
		(when (and p0?
			   (not (top-level-call-site? y))
			   (not (memq (call-site-expression y) *xs*)))
		 (fuck-up)))
	       (call-sites e))
     (when (and (region-allocation? (allocation e))
		(not (called? (allocation e))))
      (fuck-up))
     (when (and p1?
		(region-allocation? (allocation e))
		(not (memq (allocation e) *es*)))
      (fuck-up))
     ;; ENVIRONMENT-FREE-VARIABLES
     ;; ENVIRONMENT-QUICK-PARENT
     ;; ENVIRONMENT-PARENT-PARAMETER
     ;; ENVIRONMENT-PARENT-SLOT
     ;; ENVIRONMENT-ANCESTORS
     ;; ENVIRONMENT-CHILDREN
     ;; ENVIRONMENT-PROPERLY-IN-LINED-ENVIRONMENTS
     (unless (eq? (environment-narrow-prototype
		   (environment-narrow-prototype e))
		  (environment-narrow-prototype e))
      (fuck-up))
     (unless (eq? (expression-type-set (environment-expression e))
		  (expression-type-set
		   (environment-expression (environment-narrow-prototype e))))
      (fuck-up))
     (when (and (not (eq? e (environment-narrow-prototype e)))
		(not (null? (environment-narrow-clones e))))
      (fuck-up))
     (unless (memq
	      e (environment-narrow-clones (environment-narrow-prototype e)))
      (fuck-up))
     (when (and p? (not (called? (environment-narrow-prototype e))))
      (fuck-up))
     (when (and p?
		p1?
		(not (noop? (environment-narrow-prototype e)))
		(not (memq (environment-narrow-prototype e) *es*)))
      (fuck-up))
     (when p?
      (for-each
       (lambda (e)
	(unless (called? (environment-narrow-prototype e)) (fuck-up))
	(when (and p1?
		   (not (noop? (environment-narrow-prototype e)))
		   (not (memq (environment-narrow-prototype e) *es*)))
	 (fuck-up)))
       (environment-narrow-clones e)))
     (unless (eq? (environment-narrow-prototype (environment-wide-prototype e))
		  (environment-wide-prototype e))
      (fuck-up))
     (when (and p? (not (called? (environment-wide-prototype e))))
      (fuck-up))
     (when (and p?
		p1?
		(not (noop? (environment-wide-prototype e)))
		(not (memq (environment-wide-prototype e) *es*)))
      (fuck-up))
     ;; This is a real kludge.
     (unless (eq? (environment-direct-tail-callers e) (unspecified))
      (for-each (lambda (e1)
		 (unless (environment-used? e1) (fuck-up))
		 (when (and p1? (not (memq e1 *es*))) (fuck-up)))
		(environment-direct-tail-callers e)))
     ;; This is a real kludge.
     (unless (eq? (environment-direct-non-tail-callers e) (unspecified))
      (for-each (lambda (e1)
		 (unless (environment-used? e1) (fuck-up))
		 (when (and p1? (not (memq e1 *es*))) (fuck-up)))
		(environment-direct-non-tail-callers e)))
     ;; This is a real kludge.
     (unless (eq? (environment-direct-tail-callees e) (unspecified))
      (for-each (lambda (e1)
		 (unless (environment-used? e1) (fuck-up))
		 (when (and p1? (not (memq e1 *es*))) (fuck-up)))
		(environment-direct-tail-callees e)))
     ;; This is a real kludge.
     (unless (eq? (environment-direct-non-tail-callees e) (unspecified))
      (for-each (lambda (e1)
		 (unless (environment-used? e1) (fuck-up))
		 (when (and p1? (not (memq e1 *es*))) (fuck-up)))
		(environment-direct-non-tail-callees e)))
     ;; This is a real kludge.
     (unless (eq? (environment-expressions e) (unspecified))
      (for-each (lambda (x)
		 (unless (reached? x) (fuck-up))
		 (when (and p0? (not (memq x *xs*))) (fuck-up)))
		(environment-expressions e)))
     ;; This is a real kludge.
     (unless (eq? (environment-continuation-calls e) (unspecified))
      (for-each (lambda (x)
		 (unless (reached? x) (fuck-up))
		 (when (and p0? (not (memq x *xs*))) (fuck-up)))
		(environment-continuation-calls e)))
     ;; ENVIRONMENT-DIRECTLY-ESCAPING-TYPES
     ;; ENVIRONMENT-NON-SELF-TAIL-CALL-SITES
     (unless (eq? (parent e) (parent (narrow-prototype e))) (fuck-up))))
   *es*)))

;;; Determine which call sites to split

(define (splittable-call-site-count e)
 (count-if
  (lambda (y)
   (and
    ;; For now, only split explicit call sites.
    (explicit-call-site? y)
    ;; Don't split a call unless the call can actually happen.
    (executed? (call-site-expression y))
    ((truly-compatible-call? (call-site-expression y)) (environment-type e))
    ;; Don't split call sites to an environment that are nested in that
    ;; environment.
    (not (nested-in? (expression-environment (call-site-expression y)) e))))
  (call-sites e)))

(define (determine-which-call-sites-to-split!)
 ;; Reasons for splitting:
 ;;  1. to eliminate argument-parameter widening
 ;;  2. to allow in-lining
 ;;  3. to specialize the lifetime of a rest arg
 ;;  4. to specialize the lifetime of allocated data
 ;;  5. to make non-self tail calls self tail calls
 (let ((split? #f))
  (let loop ()
   (let ((again? #f))
    (let* ((es1
	    (remove-if-not
	     (lambda (e)
	      (and
	       ;; Don't split a procedure unless it can be called.
	       (called? e)
	       ;; Don't split noops.
	       (not (noop? e))
	       ;; Don't split a procedure that has already been split.
	       (eq? (environment-split e) #f)
	       ;; Don't split a procedure when some call site that targets
	       ;; that procedure is nested in some procedure that has
	       ;; already been split.
	       (not (some (lambda (y)
			   (and (explicit-call-site? y)
				(let loop? ((e1 (expression-environment
						 (call-site-expression y))))
				 (and (not (empty? e1))
				      (or (eq? (environment-split e1) #t)
					  (eq? (environment-split e1) 'never)
					  (loop? (parent e1)))))))
			  (call-sites e)))))
	     *es*))
	   ;; If both E1 and E2 are distinct candidates for splitting, don't
	   ;; split E1 there is a call to E2 nested in E1.
	   (es2 (remove-if (lambda (e1)
			    (clock-sample) ;To prevent overflow.
			    (some (lambda (e2)
				   (and (not (eq? e1 e2))
					(some (lambda (y)
					       (and (explicit-call-site? y)
						    (nested-in?
						     (expression-environment
						      (call-site-expression y))
						     e1)))
					      (call-sites e2))))
				  es1))
			   es1)))
     (for-each
      (lambda (e)
       (let ((u (environment-type e)))
	(cond
	 ((and
	   (or (= *clone-size-limit* -1)
	       (<= (clone-size e) *clone-size-limit*))
	   ;; Require some compelling reason for splitting.
	   ;; needs work: This should also allow splitting in the following
	   ;;             cases:
	   ;;             1. target is not reentrant; to allow in-lining
	   ;;             2. target or some procedure in-lined in target
	   ;;                conses; to specialize the lifetime of allocated
	   ;;                data
	   ;;             3. target is not in-lined and call site is not a self
	   ;;                tail call but there is a tail call path from the
	   ;;                target to the call site and the call site is a
	   ;;                tail call; to make non-self tail calls tail calls
	   (or
	    *split-even-if-no-widening?*
	    ;; Split calls to varargs targets. To specialize the lifetime of
	    ;; the rest arg.
	    (rest? e)
	    ;; Split calls to non-vararg targets that require widening some
	    ;; argument. To eliminate argument-parameter widening.
	    (some
	     (lambda (y)
	      (and (explicit-call-site? y)
		   (some (lambda (x g)
			  (not (set-equalp?
				;; This is a form of uniqueness.
				;; needs work: Doesn't handle displaced
				;;             vectors.
				(lambda (u1 u2)
				 (or (eq? u1 u2)
				     (and (string-type? u1) (string-type? u2))
				     (and (structure-type? u1)
					  ((structure-type-named?
					    (structure-type-name u1))
					   u2))
				     (and (headed-vector-type? u1)
					  (headed-vector-type? u2))
				     (and (nonheaded-vector-type? u1)
					  (nonheaded-vector-type? u2))))
				(members (expression-type-set x))
				(members (variable-type-set g)))))
			 (expression-arguments (call-site-expression y))
			 (expression-parameters (environment-expression e)))))
	     (call-sites e))))
	  (when #f			;debugging
	   (notify "Clone size ~a is ~s"
		   (environment-name e) (clone-size e)))
	  (set-environment-split! e #t)
	  (for-each
	   (lambda (y)
	    (when #f			;debugging
	     (notify "Cloning x~s ~a->[clone ~a ~s]"
		     (expression-index (call-site-expression y))
		     (environment-name e)
		     (environment-name (wide-prototype e))
		     *ei*)
	     (notify "~s" (undecorate (call-site-expression y))))
	    (set! *types-frozen?* #f)
	    (set-cdr! (assp
		       same-call-site?
		       y
		       (native-procedure-type-call-site-environment-alist u))
		      (clone e))
	    (set! *types-frozen?* #t))
	   (let ((ys
		  (remove-if-not
		   (lambda (y)
		    (and
		     ;; For now, only split explicit call sites.
		     (explicit-call-site? y)
		     ;; Don't split a call unless the call can actually happen.
		     (executed? (call-site-expression y))
		     ((truly-compatible-call? (call-site-expression y)) u)
		     ;; Don't split call sites to an environment that are
		     ;; nested in that environment.
		     (not (nested-in?
			   (expression-environment (call-site-expression y))
			   e))))
		   (call-sites e))))
	    ;; The first call site remains assigned to the wide prototype.
	    (if (null? ys) ys (rest ys))))
	  (set! split? #t))
	 (else (set-environment-split! e 'never)))
	(set! again? #t)))
      (if (null? es2)
	  (if (null? es1)
	      '()
	      (list (minp (lambda (e1 e2)
			   (> (splittable-call-site-count e1)
			      (splittable-call-site-count e2)))
			  es1)))
	  es2)))
    (when again? (loop))))
  split?))

;;; Compute call graph

(define (some-proper-callee p? marked? mark! e)
 ;; conventions: P? MARKED? MARK!
 ;; The PROPERLY-CALLS? relation is not necessarily reflexive.
 (define (loop? e)
  (and (not (marked? e))
       (begin (mark! e #t)
	      (or (p? e)
		  (some loop? (environment-direct-tail-callees e))
		  (some loop? (environment-direct-non-tail-callees e))))))
 (for-each (lambda (e) (mark! e #f)) *es*)
 (or (some loop? (environment-direct-tail-callees e))
     (some loop? (environment-direct-non-tail-callees e))))

(define (some-callee p? marked? mark! e)
 ;; conventions: P? MARKED? MARK!
 (for-each (lambda (e) (mark! e #f)) *es*)
 (let loop? ((e e))
  (and (not (marked? e))
       (begin (mark! e #t)
	      (or (p? e)
		  (some loop? (environment-direct-tail-callees e))
		  (some loop? (environment-direct-non-tail-callees e)))))))

(define (some-proper-caller p? marked? mark! e)
 ;; conventions: P? MARKED? MARK!
 ;; The PROPERLY-CALLS? relation is not necessarily reflexive.
 (define (loop? e)
  (and (not (marked? e))
       (begin (mark! e #t)
	      (or (p? e)
		  (some loop? (environment-direct-tail-callers e))
		  (some loop? (environment-direct-non-tail-callers e))))))
 (for-each (lambda (e) (mark! e #f)) *es*)
 (or (some loop? (environment-direct-tail-callers e))
     (some loop? (environment-direct-non-tail-callers e))))

(define (some-caller p? marked? mark! e)
 ;; conventions: P? MARKED? MARK!
 (for-each (lambda (e) (mark! e #f)) *es*)
 (let loop? ((e e))
  (and (not (marked? e))
       (begin (mark! e #t)
	      (or (p? e)
		  (some loop? (environment-direct-tail-callers e))
		  (some loop? (environment-direct-non-tail-callers e)))))))

(define (some-proper-tail-callee p? marked? mark! e)
 ;; conventions: P? MARKED? MARK!
 ;; The PROPERLY-TAIL-CALLS? relation is not necessarily reflexive.
 (define (loop? e)
  (and (not (marked? e))
       (begin (mark! e #t)
	      (or (p? e) (some loop? (environment-direct-tail-callees e))))))
 (for-each (lambda (e) (mark! e #f)) *es*)
 (some loop? (environment-direct-tail-callees e)))

(define (some-tail-callee p? marked? mark! e)
 ;; conventions: P? MARKED? MARK!
 (for-each (lambda (e) (mark! e #f)) *es*)
 (let loop? ((e e))
  (and (not (marked? e))
       (begin (mark! e #t)
	      (or (p? e) (some loop? (environment-direct-tail-callees e)))))))

(define (some-proper-tail-caller p? marked? mark! e)
 ;; conventions: P? MARKED? MARK!
 ;; The PROPERLY-TAIL-CALLS? relation is not necessarily reflexive.
 (define (loop? e)
  (and (not (marked? e))
       (begin (mark! e #t)
	      (or (p? e) (some loop? (environment-direct-tail-callers e))))))
 (for-each (lambda (e) (mark! e #f)) *es*)
 (some loop? (environment-direct-tail-callers e)))

(define (some-tail-caller p? marked? mark! e)
 ;; conventions: P? MARKED? MARK!
 (for-each (lambda (e) (mark! e #f)) *es*)
 (let loop? ((e e))
  (and (not (marked? e))
       (begin (mark! e #t)
	      (or (p? e) (some loop? (environment-direct-tail-callers e)))))))

(define (properly-calls? e1 e2)
 ;; The PROPERLY-CALLS? relation is not necessarily reflexive.
 (some-proper-caller
  (lambda (e) (eq? e e1)) environment-marked1? set-environment-marked1?! e2))

(define (calls? e1 e2) (or (eq? e1 e2) (properly-calls? e1 e2)))

(define (properly-tail-calls? e1 e2)
 ;; The PROPERLY-TAIL-CALLS? relation is not necessarily reflexive.
 (some-proper-tail-caller
  (lambda (e) (eq? e e1)) environment-marked1? set-environment-marked1?! e2))

(define (tail-calls? e1 e2) (or (eq? e1 e2) (properly-tail-calls? e1 e2)))

(define (properly-non-tail-calls? e1 e2) (unimplemented #f "unimplemented"))

(define (directly-calls? e1 e2)
 (or (directly-tail-calls? e1 e2) (directly-non-tail-calls? e1 e2)))

(define (directly-tail-calls? e1 e2)
 (memq e1 (environment-direct-tail-callers e2)))

(define (directly-non-tail-calls? e1 e2)
 (memq e1 (environment-direct-non-tail-callers e2)))

(define (proper-callees e)
 ;; The PROPERLY-CALLS? relation is not necessarily reflexive.
 ;; This is done just for side effect, to set the MARKED1? bits.
 (some-proper-callee
  (lambda (e) #f) environment-marked1? set-environment-marked1?! e)
 (remove-if-not environment-marked1? *es*))

(define (callees e)
 ;; This is done just for side effect, to set the MARKED1? bits.
 (some-callee (lambda (e) #f) environment-marked1? set-environment-marked1?! e)
 (remove-if-not environment-marked1? *es*))

(define (proper-callers e)
 ;; The PROPERLY-CALLS? relation is not necessarily reflexive.
 ;; This is done just for side effect, to set the MARKED1? bits.
 (some-proper-caller
  (lambda (e) #f) environment-marked1? set-environment-marked1?! e)
 (remove-if-not environment-marked1? *es*))

(define (callers e)
 ;; This is done just for side effect, to set the MARKED1? bits.
 (some-caller (lambda (e) #f) environment-marked1? set-environment-marked1?! e)
 (remove-if-not environment-marked1? *es*))

(define (proper-tail-callees e)
 ;; The PROPERLY-TAIL-CALLS? relation is not necessarily reflexive.
 ;; This is done just for side effect, to set the MARKED1? bits.
 (some-proper-tail-callee
  (lambda (e) #f) environment-marked1? set-environment-marked1?! e)
 (remove-if-not environment-marked1? *es*))

(define (tail-callees e)
 ;; This is done just for side effect, to set the MARKED1? bits.
 (some-tail-callee
  (lambda (e) #f) environment-marked1? set-environment-marked1?! e)
 (remove-if-not environment-marked1? *es*))

(define (proper-tail-callers e)
 ;; The PROPERLY-TAIL-CALLS? relation is not necessarily reflexive.
 ;; This is done just for side effect, to set the MARKED1? bits.
 (some-proper-tail-caller
  (lambda (e) #f) environment-marked1? set-environment-marked1?! e)
 (remove-if-not environment-marked1? *es*))

(define (tail-callers e)
 ;; This is done just for side effect, to set the MARKED1? bits.
 (some-tail-caller
  (lambda (e) #f) environment-marked1? set-environment-marked1?! e)
 (remove-if-not environment-marked1? *es*))

(define (proper-non-tail-callees e)
 (remove-if-not
  (lambda (e1) (and (environment-used? e1) (properly-non-tail-calls? e e1)))
  *es*))

(define (proper-non-tail-callers e)
 (remove-if-not
  (lambda (e1) (and (environment-used? e1) (properly-non-tail-calls? e1 e)))
  *es*))

(define (direct-callees e)
 (unionq (direct-tail-callees e) (direct-non-tail-callees e)))

(define (direct-callers e)
 (unionq (direct-tail-callers e) (direct-non-tail-callers e)))

(define (direct-tail-callees e) (environment-direct-tail-callees e))

(define (direct-tail-callers e) (environment-direct-tail-callers e))

(define (direct-non-tail-callees e) (environment-direct-non-tail-callees e))

(define (direct-non-tail-callers e) (environment-direct-non-tail-callers e))

(define (compute-call-graph! e)
 (define (assert-directly-tail-calls! e1 e2)
  (unless (directly-tail-calls? e1 e2)
   (set-environment-direct-tail-callers!
    e2 (cons e1 (environment-direct-tail-callers e2)))
   (set-environment-direct-tail-callees!
    e1 (cons e2 (environment-direct-tail-callees e1)))))
 (define (assert-directly-non-tail-calls! e1 e2)
  (unless (directly-non-tail-calls? e1 e2)
   (set-environment-direct-non-tail-callers!
    e2 (cons e1 (environment-direct-non-tail-callers e2)))
   (set-environment-direct-non-tail-callees!
    e1 (cons e2 (environment-direct-non-tail-callees e1)))))
 ;; Initialize.
 (for-each (lambda (e)
	    (set-environment-direct-tail-callers! e (unspecified))
	    (set-environment-direct-non-tail-callers! e (unspecified))
	    (set-environment-direct-tail-callees! e (unspecified))
	    (set-environment-direct-non-tail-callees! e (unspecified)))
	   *es*)
 (for-each (lambda (e)
	    (when (environment-used? e)
	     (set-environment-direct-tail-callers! e '())
	     (set-environment-direct-non-tail-callers! e '())
	     (set-environment-direct-tail-callees! e '())
	     (set-environment-direct-non-tail-callees! e '())))
	   *es*)
 ;; Compute direct callees/callers.
 (for-each
  (lambda (e1)
   ;; needs work: This notion of tail call, indicated by P?, doesn't take into
   ;;             account in-lining and vacuous SET!s.
   (define (mark! x p?)
    ;; conventions: P?
    (case (expression-kind x)
     ((null-constant) #f)
     ((true-constant) #f)
     ((false-constant) #f)
     ((char-constant) #f)
     ((fixnum-constant) #f)
     ((flonum-constant) #f)
     ((rectangular-constant) #f)
     ((string-constant) #f)
     ((symbol-constant) #f)
     ((pair-constant) #f)
     ((vector-constant) #f)
     ((lambda converted-lambda converted-continuation) #f)
     ((set!) (mark! (expression-source x) #f))
     ((if)
      (mark! (expression-antecedent x) #f)
      (when (can-be-non? false-type?
			 (expression-type-set (expression-antecedent x)))
       (mark! (expression-consequent x) p?))
      (when (can-be? false-type?
		     (expression-type-set (expression-antecedent x)))
       (mark! (expression-alternate x) p?)))
     ((primitive-procedure) #f)
     ((foreign-procedure) #f)
     ((access) #f)
     ((call converted-call)
      (mark! (expression-callee x) #f)
      (for-each (lambda (x) (mark! x #f)) (expression-arguments x))
      (when (executed? x)
       (when (converted? x)
	(when (can-be? (needs-implicit-continuation-call?
			(map expression-type-set (expression-arguments x))
			*null*
			(create-call-site x))
		       (expression-type-set (expression-callee x)))
	 (for-each-member
	  (lambda (u)
	   (unless (native-procedure-type? u) (fuck-up))
	   ;; Implicit continuation calls are always tail calls.
	   (assert-directly-tail-calls!
	    e1
	    (callee-environment
	     u
	     (recreate-call-site
	      (create-call-site x) 'continuation-argument))))
	  (expression-type-set (continuation-argument x))))
	;; There currently is no need for an analogue of the following for
	;; second-argument call sites because they currently cannot be
	;; converted.
	(when (can-be? (first-argument-needs-implicit-continuation-call?
			(map expression-type-set (expression-arguments x))
			*null*
			(create-call-site x))
		       (expression-type-set (expression-callee x)))
	 (for-each-member
	  (lambda (u)
	   (unless (native-procedure-type? u) (fuck-up))
	   ;; Implicit continuation calls are always tail calls.
	   (assert-directly-tail-calls!
	    e1
	    (callee-environment
	     u
	     (recreate-call-site
	      (recreate-call-site (create-call-site x) 'first-argument)
	      'continuation-argument))))
	  (expression-type-set (continuation-argument x)))))
       (for-each-member
	(lambda (u)
	 (when ((truly-compatible-call? x) u)
	  (cond
	   (((primitive-procedure-type-named? 'apply) u)
	    (for-each-member
	     (lambda (u)
	      (when (and (native-procedure-type? u)
			 ((truly-compatible-call-via-apply? x) u))
	       (let ((e2 (callee-environment
			  u (recreate-call-site
			     (create-call-site x) 'first-argument))))
		(if p?
		    (assert-directly-tail-calls! e1 e2)
		    (assert-directly-non-tail-calls! e1 e2)))))
	     (expression-type-set (first-argument x))))
	   (((primitive-procedure-type-named? 'call-with-current-continuation)
	     u)
	    (for-each-member
	     (lambda (u)
	      (when (and
		     (native-procedure-type? u)
		     ((truly-compatible-call-via-call-with-current-continuation? x)
		      u))
	       (let ((e2 (callee-environment
			  u (recreate-call-site
			     (create-call-site x) 'first-argument))))
		(if p?
		    (assert-directly-tail-calls! e1 e2)
		    (assert-directly-non-tail-calls! e1 e2)))))
	     (expression-type-set (first-argument x))))
	   (((primitive-procedure-type-named? 'fork) u)
	    (for-each-member
	     (lambda (u)
	      (when (and (native-procedure-type? u)
			 ((truly-compatible-call-via-fork1? x) u))
	       (assert-directly-non-tail-calls!
		e1
		(callee-environment
		 u
		 (recreate-call-site (create-call-site x) 'first-argument)))))
	     (expression-type-set (first-argument x)))
	    (for-each-member
	     (lambda (u)
	      (when (and (native-procedure-type? u)
			 ((truly-compatible-call-via-fork2? x) u))
	       (assert-directly-non-tail-calls!
		e1
		(callee-environment
		 u
		 (recreate-call-site (create-call-site x) 'second-argument)))))
	     (expression-type-set (second-argument x))))
	   (((primitive-procedure-type-named? 'mutex) u)
	    (for-each-member
	     (lambda (u)
	      (when (and (native-procedure-type? u)
			 ((truly-compatible-call-via-mutex? x) u))
	       (assert-directly-non-tail-calls!
		e1
		(callee-environment
		 u
		 (recreate-call-site (create-call-site x) 'first-argument)))))
	     (expression-type-set (first-argument x))))
	   ((native-procedure-type? u)
	    (let ((e2 (callee-environment u (create-call-site x))))
	     (unless (noop? e2)
	      (if p?
		  (assert-directly-tail-calls! e1 e2)
		  (assert-directly-non-tail-calls! e1 e2))))))))
	(expression-type-set (expression-callee x)))))
     (else (fuck-up))))
   (when (environment-used? e1)
    (mark! (expression-body (environment-expression e1)) #t)))
  *es*))

;;; Determine which environments are called more than once

(define (determine-which-environments-are-called-more-than-once!)
 (for-each (lambda (e)
	    (when (environment-used? e)
	     (clock-sample)		;To prevent overflow.
	     (set-environment-called-more-than-once?!
	      e
	      (or (> (length (call-sites e)) 1)
		  (some (lambda (e) (> (length (call-sites e)) 1))
			(proper-callers e))))))
	   *es*))

;;; Determine which variables are referenced

(define (determine-which-variables-are-referenced!)
 (for-each (lambda (x) (set-expression-accessed?! x #f)) *xs*)
 (for-each (lambda (g) (set-variable-accessed?! g #f)) *gs*)
 (for-each (lambda (g) (set-variable-assigned?! g #f)) *gs*)
 (let loop ()
  (let ((again? #f))
   (define (assert-callee-accessed! w0 y ws w p?)
    (for-each-member
     (lambda (u)
      (when ((truly-compatible-procedure? ws w y) u)
       (cond
	((primitive-procedure-type? u)
	 (when (explicit-call-site? y)
	  (when (can-be-non? null-type? w) (fuck-up))
	  ;; note: This assumes that all primitive procedures access all of
	  ;;       their arguments. This includes the continuation argument.
	  ;; needs work: The continuation argument should not be accessed
	  ;;             by primitive procedures that don't return.
	  (for-each (lambda (x) (assert-expression-accessed! x))
		    (expression-arguments (call-site-expression y)))
	  ;; Calls to CALL-WITH-CURRENT-CONTINUATION access the results of
	  ;; calling their procedure arguments only if they themselves are
	  ;; accessed.
	  (if (converted? y)
	      (cond (((primitive-procedure-type-named? 'apply) u)
		     (assert-callee-accessed!
		      (second ws) (recreate-call-site y 'first-argument)
		      (cons (first ws) (but-last (rest (rest ws)))) (last ws)
		      p?))
		    (((primitive-procedure-type-named?
		       'call-with-current-continuation)
		      u)
		     (assert-callee-accessed!
		      (second ws) (recreate-call-site y 'first-argument)
		      (list (first ws) (first ws)) *null* p?))
		    (((primitive-procedure-type-named? 'fork) u)
		     (assert-callee-accessed!
		      (second ws) (recreate-call-site y 'first-argument)
		      (list (first ws)) *null* p?)
		     (assert-callee-accessed!
		      (third ws) (recreate-call-site y 'second-argument)
		      (list (first ws)) *null* p?))
		    (((primitive-procedure-type-named? 'mutex) u)
		     (assert-callee-accessed!
		      (second ws) (recreate-call-site y 'first-argument)
		      (list (first ws)) *null* p?)))
	      (cond (((primitive-procedure-type-named? 'apply) u)
		     (assert-callee-accessed!
		      (first ws) (recreate-call-site y 'first-argument)
		      (but-last (rest ws)) (last ws) p?))
		    (((primitive-procedure-type-named?
		       'call-with-current-continuation)
		      u)
		     (assert-callee-accessed!
		      (first ws) (recreate-call-site y 'first-argument)
		      (list (create-anonymous-type-set
			     (<continuation> (call-site-expression y))))
		      *null* p?))
		    (((primitive-procedure-type-named? 'fork) u)
		     (assert-callee-accessed!
		      (first ws) (recreate-call-site y 'first-argument)
		      '() *null* p?)
		     (assert-callee-accessed!
		      (second ws) (recreate-call-site y 'second-argument)
		      '() *null* p?))
		    (((primitive-procedure-type-named? 'mutex) u)
		     (assert-callee-accessed!
		      (first ws) (recreate-call-site y 'first-argument)
		      '() *null* p?))))
	  (when (converted? y)
	   ;; The result of calling the continuation argument is accessed is
	   ;; the call site is accessed.
	   (assert-callee-accessed!
	    (first ws) (recreate-call-site y 'continuation-argument)
	    (list (expression-type-set (call-site-expression y))) *null* p?))))
	((native-procedure-type? u)
	 (let ((e (callee-environment u y)))
	  (when (and
		 (not (noop? e))
		 (or
		  p?
		  (and
		   (converted? y)
		   (not (converted? e))
		   (can-be?
		    (lambda (u)
		     (and
		      ((truly-compatible-procedure?
			(list (return-type-set e))
			*null*
			(recreate-call-site y 'continuation-argument))
		       u)
		      (or
		       ;; note: This assumes that all primitive procedures
		       ;;       access all of their arguments.
		       (primitive-procedure-type? u)
		       (and
			(native-procedure-type? u)
			(variable-accessed?
			 (first
			  (variables
			   (callee-environment
			    u
			    (recreate-call-site y 'continuation-argument))))))
		       ;; note: This assumes that all foreign procedures
		       ;;       access all of their arguments.
		       (foreign-procedure-type? u)
		       (and
			(continuation-type? u)
			;; Continuations access their arguments only if their
			;; allocation expression is accessed.
			(expression-accessed?
			 (continuation-type-allocating-expression u))))))
		    (expression-type-set
		     (continuation-argument (call-site-expression y)))))))
	   (assert-expression-accessed!
	    (expression-body (environment-expression e))))
	  (when (explicit-call-site? y)
	   (let loop ((gs (variables e))
		      (xs (if (and (converted? y) (not (converted? e)))
			      (rest (expression-arguments
				     (call-site-expression y)))
			      (expression-arguments
			       (call-site-expression y)))))
	    (unless (null? gs)
	     (cond ((and (rest? e) (null? (rest gs)))
		    (when (variable-accessed? (first gs))
		     (for-each assert-expression-accessed! xs)))
		   (else (when (variable-accessed? (first gs))
			  (assert-expression-accessed! (first xs)))
			 (loop (rest gs) (rest xs))))))
	   (when (and (converted? y) (not (converted? e)))
	    ;; The continuation argument is itself accessed.
	    (assert-expression-accessed!
	     (continuation-argument (call-site-expression y)))
	    ;; The result of calling the continuation argument is accessed if
	    ;; the call site is accessed.
	    (assert-callee-accessed!
	     (first ws) (recreate-call-site y 'continuation-argument)
	     (list (expression-type-set (call-site-expression y)))
	     *null* p?)))))
	((foreign-procedure-type? u)
	 (when (explicit-call-site? y)
	  ;; note: This assumes that all foreign procedures access all of their
	  ;;       arguments. This includes the continuation argument.
	  ;; needs work: The continuation argument should not be accessed
	  ;;             by foreign procedures that don't return.
	  (for-each assert-expression-accessed!
		    (expression-arguments (call-site-expression y)))
	  (when (converted? y)
	   ;; The result of calling the continuation argument is accessed if
	   ;; the call site is accessed.
	   (assert-callee-accessed!
	    (first ws) (recreate-call-site y 'continuation-argument)
	    (list (expression-type-set (call-site-expression y))) *null* p?))))
	((continuation-type? u)
	 (when (and (explicit-call-site? y)
		    ;; Continuations access their arguments only if their
		    ;; allocation expression is accessed.
		    (expression-accessed?
		     (continuation-type-allocating-expression u)))
	  ;; Since nonconverted continuations never return they don't access
	  ;; their continuation argument. And since the continuation argument
	  ;; is never called there is no call to ASSERT-CALLEE-ACCESSED!.
	  (assert-expression-accessed!
	   (first-argument (call-site-expression y)))))
	(else (fuck-up)))))
     w0))
   (define (assert-expression-accessed! x)
    (unless (expression-accessed? x)
     (set-expression-accessed?! x #t)
     (set! again? #t)))
   ;; The top-level lambda expression itself is accessed.
   (assert-expression-accessed! *x*)
   ;; The result of calling the top-level lambda expression is accessed only
   ;; if the result can be a fixnum.
   (assert-callee-accessed!
    (expression-type-set *x*)
    *y*
    (list *w1*)
    *null*
    (can-be? fixnum-type? (expression-type-set (expression-body *x*))))
   (for-each
    (lambda (x)
     (when (reached? x)
      (let ((w (expression-type-set x)))
       (case (expression-kind x)
	((null-constant) #f)
	((true-constant) #f)
	((false-constant) #f)
	((char-constant) #f)
	((fixnum-constant) #f)
	((flonum-constant) #f)
	((rectangular-constant) #f)
	((string-constant) #f)
	((symbol-constant) #f)
	((pair-constant) #f)
	((vector-constant) #f)
	((lambda converted-lambda converted-continuation) #f)
	((set!)
	 ;; The source of an assignment is accessed only if the destination
	 ;; variable is accessed.
	 (when (variable-accessed? (expression-variable x))
	  (assert-expression-accessed! (expression-source x))))
	((if)
	 ;; The antecedent is always accessed.
	 (assert-expression-accessed! (expression-antecedent x))
	 ;; The consequent and alternate are accessed only if the expression
	 ;; itself is accessed.
	 (when (expression-accessed? x)
	  (when (can-be-non? false-type?
			     (expression-type-set (expression-antecedent x)))
	   (assert-expression-accessed! (expression-consequent x)))
	  (when (can-be? false-type?
			 (expression-type-set (expression-antecedent x)))
	   (assert-expression-accessed! (expression-alternate x)))))
	((primitive-procedure) #f)
	((foreign-procedure) #f)
	((access)
	 (when (and (expression-accessed? x)
		    (not (variable-accessed? (expression-variable x))))
	  (set-variable-accessed?! (expression-variable x) #t)
	  (set! again? #t)))
	((call converted-call)
	 ;; The callee itself is accessed.
	 (assert-expression-accessed! (expression-callee x))
	 ;; But the result of calling the callee is accessed only if the
	 ;; call site is accessed.
	 (when (executed? x)
	  (assert-callee-accessed!
	   (expression-type-set (expression-callee x))
	   (create-call-site x)
	   (map expression-type-set (expression-arguments x))
	   *null*
	   (expression-accessed? x))))
	(else (fuck-up))))))
    *xs*)
   (when again? (loop))))
 (for-each
  (lambda (x)
   (when (executed? x) (set-variable-assigned?! (expression-variable x) #t)))
  *assignments*))

;;; Determine free variables

(define (determine-free-variables!)
 (for-each (lambda (e) (set-environment-free-variables! e (unspecified))) *es*)
 (for-each
  (lambda (e)
   (when (environment-used? e) (set-environment-free-variables! e '())))
  *es*)
 (for-each (lambda (x)
	    (when (case (expression-kind x)
		   ((access) (reached? x))
		   ((set!) (executed? x))
		   (else (fuck-up)))
	     (let* ((g (expression-variable x))
		    (e1 (variable-environment g)))
	      (when (accessed? g)
	       (let loop ((e (expression-environment x)))
		(unless (eq? e e1)
		 ;; This is just because of *CLOSURE-CONVERSION-METHOD*.
		 (when (environment-used? e)
		  (unless (memq g (environment-free-variables e))
		   (set-environment-free-variables!
		    e (cons g (environment-free-variables e)))))
		 (loop (parent e))))))))
	   *references*))

;;; Annotate environments and continuation types

(define (annotate-environments-and-continuation-types!)
 (for-each (lambda (e)
	    (set-environment-expressions! e (unspecified))
	    (set-environment-continuation-calls! e (unspecified)))
	   *es*)
 (for-each (lambda (e)
	    (when (environment-used? e)
	     (set-environment-expressions! e '())
	     (set-environment-continuation-calls! e '())))
	   *es*)
 (for-each (lambda (u) (set-continuation-type-call-sites! u (unspecified)))
	   *continuation-types*)
 (for-each
  (lambda (u) (when (type-used? u) (set-continuation-type-call-sites! u '())))
  *continuation-types*)
 (for-each (lambda (x)
	    (when (and (reached? x)
		       (not (empty? (expression-environment x)))
		       (environment-used? (expression-environment x)))
	     (set-environment-expressions!
	      (expression-environment x)
	      (cons x (environment-expressions (expression-environment x))))))
	   *xs*)
 (for-each
  (lambda (x)
   (when (and (reached? x)
	      (not (empty? (expression-environment x)))
	      (environment-used? (expression-environment x)))
    (when (can-be? (lambda (u)
		    (and (continuation-type? u)
			 ((truly-compatible-call? x) u)))
		   (expression-type-set (expression-callee x)))
     (set-environment-continuation-calls!
      (expression-environment x)
      (cons x (environment-continuation-calls (expression-environment x))))
     (for-each-member
      (lambda (u)
       (when (and (continuation-type? u) ((truly-compatible-call? x) u))
	(set-continuation-type-call-sites!
	 u (cons (create-call-site x) (continuation-type-call-sites u)))))
      (expression-type-set (expression-callee x))))
    (when (can-be?
	   (lambda (u)
	    (and ((primitive-procedure-type-named? 'apply) u)
		 ((truly-compatible-call? x) u)
		 (can-be? (lambda (u)
			   (and (continuation-type? u)
				((truly-compatible-call-via-apply? x) u)))
			  (expression-type-set (first-argument x)))))
	   (expression-type-set (expression-callee x)))
     (for-each-member
      (lambda (u)
       (when (and ((primitive-procedure-type-named? 'apply) u)
		  ((truly-compatible-call? x) u))
	(for-each-member
	 (lambda (u)
	  (when (and (continuation-type? u)
		     ((truly-compatible-call-via-apply? x) u))
	   (set-continuation-type-call-sites!
	    u (cons (recreate-call-site (create-call-site x) 'first-argument)
		    (continuation-type-call-sites u)))))
	 (expression-type-set (first-argument x)))))
      (expression-type-set (expression-callee x))))
    (when (can-be?
	   (lambda (u)
	    (and
	     ((primitive-procedure-type-named? 'call-with-current-continuation)
	      u)
	     ((truly-compatible-call? x) u)
	     (can-be?
	      (lambda (u)
	       (and
		(continuation-type? u)
		((truly-compatible-call-via-call-with-current-continuation? x)
		 u)))
	      (expression-type-set (first-argument x)))))
	   (expression-type-set (expression-callee x)))
     (for-each-member
      (lambda (u)
       (when (and ((primitive-procedure-type-named?
		    'call-with-current-continuation)
		   u)
		  ((truly-compatible-call? x) u))
	(for-each-member
	 (lambda (u)
	  (when (and
		 (continuation-type? u)
		 ((truly-compatible-call-via-call-with-current-continuation? x)
		  u))
	   (set-continuation-type-call-sites!
	    u (cons (recreate-call-site (create-call-site x) 'first-argument)
		    (continuation-type-call-sites u)))))
	 (expression-type-set (first-argument x)))))
      (expression-type-set (expression-callee x))))
    (when (can-be?
	   (lambda (u)
	    (and ((primitive-procedure-type-named? 'fork) u)
		 ((truly-compatible-call? x) u)
		 (can-be? (lambda (u)
			   (and (continuation-type? u)
				((truly-compatible-call-via-fork1? x) u)))
			  (expression-type-set (first-argument x)))))
	   (expression-type-set (expression-callee x)))
     (for-each-member
      (lambda (u)
       (when (and ((primitive-procedure-type-named? 'fork) u)
		  ((truly-compatible-call? x) u))
	(for-each-member
	 (lambda (u)
	  (when (and (continuation-type? u)
		     ((truly-compatible-call-via-fork1? x) u))
	   (set-continuation-type-call-sites!
	    u (cons (recreate-call-site (create-call-site x) 'first-argument)
		    (continuation-type-call-sites u)))))
	 (expression-type-set (first-argument x)))))
      (expression-type-set (expression-callee x))))
    (when (can-be?
	   (lambda (u)
	    (and ((primitive-procedure-type-named? 'fork) u)
		 ((truly-compatible-call? x) u)
		 (can-be? (lambda (u)
			   (and (continuation-type? u)
				((truly-compatible-call-via-fork2? x)  u)))
			  (expression-type-set (second-argument x)))))
	   (expression-type-set (expression-callee x)))
     (for-each-member
      (lambda (u)
       (when (and ((primitive-procedure-type-named? 'fork) u)
		  ((truly-compatible-call? x) u))
	(for-each-member
	 (lambda (u)
	  (when (and (continuation-type? u)
		     ((truly-compatible-call-via-fork2? x) u))
	   (set-continuation-type-call-sites!
	    u (cons (recreate-call-site (create-call-site x) 'second-argument)
		    (continuation-type-call-sites u)))))
	 (expression-type-set (second-argument x)))))
      (expression-type-set (expression-callee x))))
    (when (can-be?
	   (lambda (u)
	    (and ((primitive-procedure-type-named? 'mutex) u)
		 ((truly-compatible-call? x) u)
		 (can-be? (lambda (u)
			   (and (continuation-type? u)
				((truly-compatible-call-via-mutex? x) u)))
			  (expression-type-set (first-argument x)))))
	   (expression-type-set (expression-callee x)))
     (for-each-member
      (lambda (u)
       (when (and ((primitive-procedure-type-named? 'mutex) u)
		  ((truly-compatible-call? x) u))
	(for-each-member
	 (lambda (u)
	  (when (and (continuation-type? u)
		     ((truly-compatible-call-via-mutex? x) u))
	   (set-continuation-type-call-sites!
	    u (cons (recreate-call-site (create-call-site x) 'first-argument)
		    (continuation-type-call-sites u)))))
	 (expression-type-set (first-argument x)))))
      (expression-type-set (expression-callee x))))))
  *calls*))

;;; Invert points-to relation

(define (invert-points-to-relation!)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *internal-symbol-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *external-symbol-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *primitive-procedure-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *native-procedure-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *foreign-procedure-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *continuation-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *string-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *structure-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *headed-vector-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *nonheaded-vector-types*)
 (for-each (lambda (u) (set-types-and-type-sets-that-directly-point-to! u '()))
	   *displaced-vector-types*)
 (for-each
  (lambda (w)
   (for-each-member
    (lambda (u)
     (when (or (internal-symbol-type? u)
	       (external-symbol-type? u)
	       (primitive-procedure-type? u)
	       (native-procedure-type? u)
	       (foreign-procedure-type? u)
	       (continuation-type? u)
	       (string-type? u)
	       (structure-type? u)
	       (headed-vector-type? u)
	       (nonheaded-vector-type? u)
	       (displaced-vector-type? u))
      (set-types-and-type-sets-that-directly-point-to!
       u (cons w (types-and-type-sets-that-directly-point-to u)))))
    w))
  *ws*)
 (for-each (lambda (u)
	    (set-types-and-type-sets-that-directly-point-to!
	     (external-symbol-type-displaced-string-type u)
	     (cons u
		   (types-and-type-sets-that-directly-point-to
		    (external-symbol-type-displaced-string-type u)))))
	   *external-symbol-types*)
 (for-each (lambda (u)
	    (set-types-and-type-sets-that-directly-point-to!
	     (displaced-vector-type-displaced-vector-type u)
	     (cons u
		   (types-and-type-sets-that-directly-point-to
		    (displaced-vector-type-displaced-vector-type u)))))
	   *displaced-vector-types*))

;;; Tam V'Nishlam Shevah L'El Borei Olam
