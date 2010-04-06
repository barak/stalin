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
(module stalin2)

(include "QobiScheme.sch")
(include "stalin2.sch")
;;; End delete for Trotsky

;;; Error messages

(define *october?* #f)

;;; Begin delete for Trotsky

(define (notify format-string . args)
 ;; conventions: FORMAT-STRING ARGS
 (let ((string (apply format #f format-string args)))
  (cond
   (else (display string) (newline)))))

(define (split-into-lines s)
 ;; conventions: S
 (let loop ((characters (string->list s)) (lines '("")))
  ;; conventions: CHARACTERS LINES
  (cond ((null? characters) (reverse lines))
	((char=? (first characters) #\newline)
	 (loop (rest characters) (cons "" lines)))
	(else (loop (rest characters)
		    (cons (string-append (first lines)
					 (string (first characters)))
			  (rest lines)))))))

(define (notify-pp format-string . args)
 ;; conventions: FORMAT-STRING ARGS
 (cond
  (else (let ((pretty? (write-pretty)))
	 ;; conventions: PRETTY?
	 (set-write-pretty! #t)
	 (apply format #t format-string args)
	 (set-write-pretty! pretty?))
	(newline))))

(define (notify-pp3 format-string . args)
 ;; conventions: FORMAT-STRING ARGS
 (cond
  (else (let ((level (write-level))
	      (pretty? (write-pretty)))
	 ;; conventions: LEVEL PRETTY?
	 (set-write-level! 3)
	 (set-write-pretty! #t)
	 (apply format #t format-string args)
	 (set-write-level! level)
	 (set-write-pretty! pretty?))
	(newline))))

(define *throw* #f)			;debugging

(define (terminate)
 (cond (*throw* (*throw* #f))		;debugging
       (*october?* (abort))
       (else (exit -1))))

;;; End delete for Trotsky

(define (syntax-error s error)
 ;; conventions: ERROR
 (if (s-expression-pathname s)
     (notify "~a:~s:~s:~a"
	     (s-expression-pathname s)
	     (s-expression-line-position s)
	     (s-expression-character-position s)
	     error)
     (notify error))
 (terminate))

(define (unimplemented x/y error)
 ;; conventions: ERROR
 ;; needs work: Should give an indication of the call-site offset.
 (cond ((expression? x/y)
	(if (expression-pathname x/y)
	    (notify "~a:~s:~s:~a"
		    (expression-pathname x/y)
		    (expression-line-position x/y)
		    (expression-character-position x/y))
	    (notify error)))
       ((call-site? x/y) (unimplemented (call-site-expression x/y) error))
       (else (notify error)))
 (terminate))

;;; Macro utilities

(define (sx-datum s)
 ;; note: This can't be eta-converted because of bugs running under IRIX 5.3.
 (s-expression-datum s))

(define (macro? s) (assq (sx-datum s) *macros*))

(define (macro-expander s) (second (assq (sx-datum s) *macros*)))

(define (expand-macro s)
 (unless (s-expression-expansion s)
  (set-s-expression-expansion!
   s
   (create-s-expression
    (s-expression-pathname s)
    (s-expression-line-position s)
    (s-expression-character-position s)
    (s-expression-character-position-within-line s)
    (s-expression-comments s)
    ;; needs work: This encapsulation loses the line and character positions of
    ;;             the macro that is being expanded.
    (sx-datum (encapsulate ((macro-expander (sx-first s)) s))))))
 (s-expression-expansion s))

(define (sx-car s) (car (sx-datum s)))

(define (sx-cdr s) (cdr (sx-datum s)))

(define (sx-first s)
 ;; note: This can't be eta-converted because of bugs running under IRIX 5.3.
 (sx-car s))

(define (sx-second s) (sx-car (sx-cdr s)))

(define (sx-third s) (sx-car (sx-cdr (sx-cdr s))))

(define (sx-fourth s) (sx-car (sx-cdr (sx-cdr (sx-cdr s)))))

(define (sx-fifth s) (sx-car (sx-cdr (sx-cdr (sx-cdr (sx-cdr s))))))

(define (sx-sixth s) (sx-car (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr s)))))))

(define (sx-seventh s)
 (sx-car (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr s))))))))

(define (sx-eighth s)
 (sx-car (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr s)))))))))

(define (sx-ninth s)
 (sx-car
  (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr s))))))))))

(define (sx-tenth s)
 (sx-car
  (sx-cdr
   (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr s)))))))))))

(define (sx-eleventh s)
 (sx-car
  (sx-cdr
   (sx-cdr
    (sx-cdr
     (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr s))))))))))))

(define (sx-twelfth s)
 (sx-car
  (sx-cdr
   (sx-cdr
    (sx-cdr
     (sx-cdr
      (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr (sx-cdr s)))))))))))))

(define (sx-rest s)
 ;; note: This can't be eta-converted because of bugs running under IRIX 5.3.
 (sx-cdr s))

(define (sx-length s)
 (let loop ((s s) (c 0))
  ;; conventions: C
  (if (sx-null? s) c (loop (sx-rest s) (+ c 1)))))

(define (sx-null? s) (null? (sx-datum s)))

(define (sx-pair? s) (pair? (sx-datum s)))

(define (sx-symbol? s) (symbol? (sx-datum s)))

(define (sx-string? s) (string? (sx-datum s)))

(define (sx-vector? s) (vector? (sx-datum s)))

(define (sx-char? s) (char? (sx-datum s)))

(define (sx-eq? s q) (eq? (sx-datum s) q))

(define (sx-integer? s) (integer? (sx-datum s)))

(define (sx-rational? s) (rational? (sx-datum s)))

(define (sx-real? s) (real? (sx-datum s)))

(define (sx-complex? s) (complex? (sx-datum s)))

(define (sx-exact? s) (exact? (sx-datum s)))

(define (sx-negative? s) (negative? (sx-datum s)))

(define (sx-map-vector p s)
 ;; note: Not a real analog.
 ;; conventions: P
 (map-vector p (sx-datum s)))

(define (sx-map p s)
 ;; note: Not a real analog.
 ;; conventions: P
 (let loop ((s s) (c '()))
  ;; conventions: C
  (if (sx-null? s) (reverse c) (loop (sx-rest s) (cons (p (sx-first s)) c)))))

(define (sx-every p s)
 ;; conventions: P
 (or (sx-null? s) (and (p (sx-first s)) (sx-every p (sx-rest s)))))

(define (sx-some p s)
 ;; conventions: P
 (and (not (sx-null? s)) (or (p (sx-first s)) (sx-some p (sx-rest s)))))

(define (sx-for-each p s)
 ;; conventions: P
 (unless (sx-null? s) (p (sx-first s)) (sx-for-each p (sx-rest s))))

(define (sx-list? s)
 (or (sx-null? s) (and (sx-pair? s) (sx-list? (sx-rest s)))))

(define (sx-for-each-vector p s)
 ;; conventions: P
 (for-each-vector p (sx-datum s)))

(define (sx-last s)
 (if (sx-null? (sx-rest s)) (sx-first s) (sx-last (sx-rest s))))

(define (sx-vector->list s)
 ;; note: Not a real analog.
 (vector->list (sx-datum s)))

(define (sx-some-vector p s)
 ;; conventions: P
 (some-vector p (sx-datum s)))

(define (sx-unlist s) (sx-map (lambda (s) s) s))

;;; needs work: EVERY LIST?

(define (encapsulate s/q)
 (cond ((s-expression? s/q) s/q)
       ((pair? s/q)
	(create-anonymous-s-expression
	 (cons (encapsulate (car s/q)) (encapsulate (cdr s/q)))))
       ((vector? s/q)
	(create-anonymous-s-expression (map-vector encapsulate s/q)))
       (else (create-anonymous-s-expression s/q))))

(define (october-encapsulate version cursor s/q)
 (let loop ((s/q s/q))
  (cond
   ((s-expression? s/q) s/q)
   ((pair? s/q)
    (create-october-s-expression
     version cursor (cons (loop (car s/q)) (loop (cdr s/q)))))
   ((vector? s/q)
    (create-october-s-expression version cursor (map-vector loop s/q)))
   (else (create-october-s-expression version cursor s/q)))))

(define (unencapsulate s/q)
 (cond ((s-expression? s/q) (unencapsulate (s-expression-datum s/q)))
       ((vector? s/q) (map-vector unencapsulate s/q))
       ((pair? s/q) (cons (unencapsulate (car s/q)) (unencapsulate (cdr s/q))))
       (else s/q)))

;;; Foreign Procedures

(define (valid-foreign-parameter-type? s)
 (or (sx-eq? s 'char)
     (sx-eq? s 'signed-char)
     (sx-eq? s 'unsigned-char)
     (sx-eq? s 'short)
     (sx-eq? s 'unsigned-short)
     (sx-eq? s 'int)
     (sx-eq? s 'unsigned)
     (sx-eq? s 'long)
     (sx-eq? s 'unsigned-long)
     (sx-eq? s 'float)
     (sx-eq? s 'double)
     (sx-eq? s 'long-double)
     (sx-eq? s 'char*)
     (sx-eq? s 'file*)
     (sx-eq? s 'void*)))

(define (valid-foreign-return-type? s)
 (or (sx-eq? s 'char)
     (sx-eq? s 'signed-char)
     (sx-eq? s 'unsigned-char)
     (sx-eq? s 'short)
     (sx-eq? s 'unsigned-short)
     (sx-eq? s 'int)
     (sx-eq? s 'unsigned)
     (sx-eq? s 'long)
     (sx-eq? s 'unsigned-long)
     (sx-eq? s 'float)
     (sx-eq? s 'double)
     (sx-eq? s 'long-double)
     (sx-eq? s 'char*)
     (sx-eq? s 'input-port)
     (sx-eq? s 'output-port)
     (sx-eq? s 'void*)
     (sx-eq? s 'void)
     (sx-eq? s 'no-return)))

(define (foreign-type? f)
 (case f
  ((char signed-char unsigned-char) char-type?)
  ((short unsigned-short int unsigned long unsigned-long) fixnum-type?)
  ((float double long-double) flonum-type?)
  ((char*) string-type?)
  ((file*) (lambda (u) (or (input-port-type? u) (output-port-type? u))))
  ((input-port) input-port-type?)
  ((output-port) output-port-type?)
  ((void*) pointer-type?)
  (else (fuck-up))))

(define (foreign-return-type->type-set f)
 (case f
  ((char signed-char unsigned-char) *foreign-char-type-set*)
  ((short unsigned-short int unsigned long unsigned-long)
   *foreign-fixnum-type-set*)
  ((float double long-double) *foreign-flonum-type-set*)
  ((char*) *foreign-string-type-set*)
  ((input-port) *foreign-input-port-type-set*)
  ((output-port) *foreign-output-port-type-set*)
  ((void*) *foreign-pointer-type-set*)
  ((void no-return) *void*)
  (else (fuck-up))))

(define (foreign-procedure-return-type-set u)
 (foreign-return-type->type-set (foreign-procedure-type-result u)))

(define (foreign-procedure-returns? u)
 (not (eq? (foreign-procedure-type-result u) 'no-return)))

;;; Enumerating symbol constants

(define (valid-parameter? s)
 (and (sx-symbol? s)
      (not (sx-eq? s 'quote))
      (not (sx-eq? s 'lambda))
      (not (sx-eq? s 'set!))
      (not (sx-eq? s 'if))
      (not (sx-eq? s 'primitive-procedure))
      (not (sx-eq? s 'foreign-procedure))
      (not (sx-eq? s 'else))
      (not (sx-eq? s '=>))
      (not (sx-eq? s 'define))
      (not (macro? s))))

(define (valid-parameters? s)
 (or (and (sx-pair? s)
	  (valid-parameter? (sx-car s))
	  (valid-parameters? (sx-cdr s)))
     (sx-null? s)
     (valid-parameter? s)))

(define (disjoint? qs)
 (or (null? qs)
     (and (let loop? ((qs1 (rest qs)))
	   (or (null? qs1)
	       (and (not (eq? (first qs) (first qs1))) (loop? (rest qs1)))))
	  (disjoint? (rest qs)))))

(define (parameters s)
 (cond ((sx-null? s) '())
       ((sx-pair? s) (cons (sx-datum (sx-car s)) (parameters (sx-cdr s))))
       ((sx-symbol? s) (list (sx-datum s)))
       (else (fuck-up))))

(define (parameters->variables s)
 (cond
  ((sx-null? s) '())
  ((sx-pair? s)
   (cons (create-variable (sx-car s)) (parameters->variables (sx-cdr s))))
  ((sx-symbol? s) (create-variable s))
  (else (fuck-up))))

(define (symbols-in s)
 (let ((qs '()))
  (define (symbols-in-constant s)
   (cond
    ((sx-null? s) #f)
    ((sx-eq? s #t) #f)
    ((sx-eq? s #f) #f)
    ((sx-char? s) #f)
    ((and (sx-integer? s) (sx-exact? s)) #f)
    ((sx-rational? s)
     (when (sx-exact? s)
      (syntax-error
       s "Cannot (yet) compile exact rational noninteger constants")))
    ((sx-real? s)
     (when (sx-exact? s)
      (syntax-error
       s "Cannot (yet) compile exact real nonrational constants")))
    ((sx-complex? s)
     (when (sx-exact? s)
      (syntax-error
       s "Cannot (yet) compile exact complex nonreal constants")))
    ((sx-string? s) #f)
    ((sx-symbol? s)
     (unless (memq (sx-datum s) qs) (set! qs (cons (sx-datum s) qs))))
    ((sx-pair? s)
     (symbols-in-constant (sx-car s))
     (symbols-in-constant (sx-cdr s)))
    ((sx-vector? s) (sx-for-each-vector symbols-in-constant s))
    ;; procedures, input ports, output ports, eof objects, structures, and
    ;; pointers
    (else (syntax-error s "Unrecognized constant type"))))
  (define (symbols-in s)
   (cond
    ((sx-null? s) (syntax-error s "Improper expression"))
    ((sx-pair? s)
     (unless (sx-list? s) (syntax-error s "Improper expression"))
     (if (sx-symbol? (sx-first s))
	 (case (sx-datum (sx-first s))
	  ((quote)
	   (unless (= (sx-length s) 2) (syntax-error s "Improper QUOTE"))
	   (symbols-in-constant (sx-second s)))
	  ((lambda)
	   ;; Extension to R4RS: LAMBDA can have empty body.
	   (unless (and (>= (sx-length s) 2)
			(valid-parameters? (sx-second s))
			(disjoint? (parameters (sx-second s))))
	    (syntax-error s "Improper LAMBDA"))
	   (let ((s (macroexpand-body s)))
	    (unless (sx-null? (sx-rest (sx-rest s)))
	     (symbols-in (sx-third s)))))
	  ((set!)
	   (unless (and (= (sx-length s) 3) (sx-symbol? (sx-second s)))
	    (syntax-error s "Improper SET!"))
	   (symbols-in (sx-third s)))
	  ((if)
	   (unless (or (= (sx-length s) 3) (= (sx-length s) 4))
	    (syntax-error s "Improper IF"))
	   (symbols-in (sx-second s))
	   (symbols-in (sx-third s))
	   (when (= (sx-length s) 4) (symbols-in (sx-fourth s))))
	  ((primitive-procedure)
	   ;; Extension to R4RS: Link to primitive procedures.
	   (let ((s2 (sx-second s)))
	    (unless (or (and (or (sx-eq? s2 'make-structure)
				 (sx-eq? s2 'structure-ref)
				 (sx-eq? s2 'structure-set!))
			     (= (sx-length s) 4)
			     (sx-symbol? (sx-third s))
			     (sx-integer? (sx-fourth s))
			     (sx-exact? (sx-fourth s))
			     (not (sx-negative? (sx-fourth s))))
			(and (sx-eq? s2 'structure?)
			     (= (sx-length s) 3)
			     (sx-symbol? (sx-third s)))
			(and (= (sx-length s) 2)
			     (not (sx-eq? s2 'make-structure))
			     (not (sx-eq? s2 'structure-ref))
			     (not (sx-eq? s2 'structure-set!))
			     (not (sx-eq? s2 'structure?))
			     (assq (sx-datum s2)
				   *primitive-procedure-handlers*)))
	     (syntax-error s "Improper PRIMITIVE-PROCEDURE"))))
	  ((foreign-procedure)
	   (unless (or (and
			(= (sx-length s) 4)
			(sx-list? (sx-second s))
			(sx-every valid-foreign-parameter-type? (sx-second s))
			(valid-foreign-return-type? (sx-third s))
			(sx-string? (sx-fourth s)))
		       (and
			(= (sx-length s) 5)
			(sx-list? (sx-second s))
			(sx-every valid-foreign-parameter-type? (sx-second s))
			(valid-foreign-return-type? (sx-third s))
			(sx-string? (sx-fourth s))
			(sx-string? (sx-fifth s))))
	    (syntax-error s "Improper FOREIGN-PROCEDURE")))
	  (else (if (macro? (sx-first s))
		    (symbols-in (expand-macro s))
		    (sx-for-each symbols-in s))))
	 (sx-for-each symbols-in s)))
    ((sx-symbol? s) #f)
    (else (symbols-in-constant s))))
  (symbols-in s)
  qs))

;;; Macro expander

(define (variable s gs)
 (when (null? gs) (syntax-error s "Unbound variable"))
 (if (sx-eq? s (variable-name (first gs))) (first gs) (variable s (rest gs))))

(define (dotted-append gs1 gs2)
 (cond ((null? gs1) gs2)
       ((variable? gs1) (cons gs1 gs2))
       (else (cons (first gs1) (dotted-append (rest gs1) gs2)))))

(define (macroexpand s)
 (define (macroexpand s gs v f)
  ;; conventions: V F
  (define (macroexpand-constant s1)
   (let ((q (sx-datum s1)))
    (cond
     ((sx-null? s1) (create-expression 'null-constant s #f))
     ((sx-eq? s1 #t) (create-expression 'true-constant s #f))
     ((sx-eq? s1 #f) (create-expression 'false-constant s #f))
     ((sx-char? s1) (create-expression 'char-constant s q))
     ((and (sx-integer? s1) (sx-exact? s1))
      (create-expression 'fixnum-constant s q))
     ((sx-rational? s1)
      (when (sx-exact? s1) (fuck-up))
      (create-expression 'flonum-constant s q))
     ((sx-real? s1)
      (when (sx-exact? s1) (fuck-up))
      (create-expression 'flonum-constant s q))
     ((sx-complex? s1)
      (when (sx-exact? s1) (fuck-up))
      ;; needs work: 1.0+0.0i will create a FLONUM-CONSTANT so there is no way
      ;;             to create a RECTANGULAR-CONSTANT with a 0.0 imaginary
      ;;             component.
      (create-expression 'rectangular-constant s q))
     ((sx-string? s1) (create-expression 'string-constant s q))
     ((sx-symbol? s1) (create-expression 'symbol-constant s q))
     ((sx-pair? s1)
      (create-expression 'pair-constant s
			 (cons (macroexpand-constant (sx-car s1))
			       (macroexpand-constant (sx-cdr s1)))))
     ((sx-vector? s1)
      (create-expression
       'vector-constant s (sx-map-vector macroexpand-constant s1)))
     ;; procedures, input ports, output ports, eof objects, structures, and
     ;; pointers
     (else (fuck-up)))))
  (cond
   ((sx-null? s) (fuck-up))
   ((sx-pair? s)
    (unless (sx-list? s) (fuck-up))
    (if (sx-symbol? (sx-first s))
	(case (sx-datum (sx-first s))
	 ((quote)
	  (unless (= (sx-length s) 2) (fuck-up))
	  (macroexpand-constant (sx-second s)))
	 ((lambda)
	  (unless (and (>= (sx-length s) 2)
		       (valid-parameters? (sx-second s))
		       (disjoint? (parameters (sx-second s))))
	   (fuck-up))
	  (let ((s (macroexpand-body s)))
	   (cond
	    ((sx-null? (sx-rest (sx-rest s)))
	     (create-lambda-expression
	      s
	      (create-environment v (if v (symbol->string v) f))
	      (parameters->variables (sx-second s)) #f))
	    ((sx-null? (sx-rest (sx-rest (sx-rest s))))
	     (let ((f (if v (symbol->string v) f))
		   (gs1 (parameters->variables (sx-second s))))
	      ;; conventions: F
	      (create-lambda-expression
	       s (create-environment v f) gs1
	       (macroexpand (sx-third s) (dotted-append gs1 gs) #f f))))
	    (else (fuck-up)))))
	 ((set!)
	  (unless (and (= (sx-length s) 3) (sx-symbol? (sx-second s)))
	   (fuck-up))
	  (create-set!-expression
	   s (variable (sx-second s) gs)
	   (macroexpand (sx-third s) gs (sx-datum (sx-second s)) f)))
	 ((if)
	  (unless (or (= (sx-length s) 3) (= (sx-length s) 4)) (fuck-up))
	  (create-if-expression
	   s
	   (macroexpand (sx-second s) gs #f f)
	   (macroexpand (sx-third s) gs #f f)
	   (if (= (sx-length s) 3)
	       (create-call-expression
		s
		(create-lambda-expression
		 s (create-environment v (if v (symbol->string v) f)) '() #f)
		'())
	       (macroexpand (sx-fourth s) gs #f f))))
	 ((primitive-procedure)
	  ;; Extension to R4RS: Link to primitive procedures.
	  (let ((s2 (sx-second s)))
	   (unless (or (and (or (sx-eq? s2 'make-structure)
				(sx-eq? s2 'structure-ref)
				(sx-eq? s2 'structure-set!))
			    (= (sx-length s) 4)
			    (sx-symbol? (sx-third s))
			    (sx-integer? (sx-fourth s))
			    (sx-exact? (sx-fourth s))
			    (not (sx-negative? (sx-fourth s))))
		       (and (sx-eq? s2 'structure?)
			    (= (sx-length s) 3)
			    (sx-symbol? (sx-third s)))
		       (and (= (sx-length s) 2)
			    (not (sx-eq? s2 'make-structure))
			    (not (sx-eq? s2 'structure-ref))
			    (not (sx-eq? s2 'structure-set!))
			    (not (sx-eq? s2 'structure?))
			    (assq (sx-datum s2)
				  *primitive-procedure-handlers*)))
	    (fuck-up)))
	  (create-expression
	   'primitive-procedure s (unencapsulate (sx-rest s))))
	 ((foreign-procedure)
	  (unless (or (and
		       (= (sx-length s) 4)
		       (sx-list? (sx-second s))
		       (sx-every valid-foreign-parameter-type? (sx-second s))
		       (valid-foreign-return-type? (sx-third s))
		       (sx-string? (sx-fourth s)))
		      (and
		       (= (sx-length s) 5)
		       (sx-list? (sx-second s))
		       (sx-every valid-foreign-parameter-type? (sx-second s))
		       (valid-foreign-return-type? (sx-third s))
		       (sx-string? (sx-fourth s))
		       (sx-string? (sx-fifth s))))
	   (fuck-up))
	  (create-expression 'foreign-procedure s (unencapsulate (sx-rest s))))
	 (else
	  (if (macro? (sx-first s))
	      (macroexpand (expand-macro s) gs #f f)
	      (create-call-expression
	       s
	       (macroexpand (sx-first s) gs #f f)
	       (sx-map (lambda (s) (macroexpand s gs #f f)) (sx-rest s))))))
	(create-call-expression
	 s
	 (macroexpand (sx-first s) gs #f f)
	 (sx-map (lambda (s) (macroexpand s gs #f f)) (sx-rest s)))))
   ((sx-symbol? s) (create-access-expression s (variable s gs)))
   (else (macroexpand-constant s))))
 (macroexpand s '() #f "top level"))

;;; Tam V'Nishlam Shevah L'El Borei Olam
