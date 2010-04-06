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
(module stalin4c)

(include "QobiScheme.sch")
(include "stalin4c.sch")
;;; End delete for Trotsky

(define (compile-regions)
 ;; needs work: To use code-generation abstractions.
 (newlines-between
  (map
   (lambda (e)
    (if (has-region? e)
	(if *expandable-regions?*
	    (newline-between
	     (c:define (c:big-region-size e) "65536")
	     (semicolon-after
	      (newline-between
	       (space-between "struct" (c:region e))
	       (braces-around
		(newline-between
		 (semicolon-after
		  (space-between
		   "struct" (c:region e) (star-before (c:region))))
		 (if (reentrant? e)
		     (semicolon-after (space-between *length* (c:region-size)))
		     (c:noop))
		 (semicolon-after
		  (space-between (c:byte)
				 (c:raw-subscript (c:data) (c:1))))))))
	     (semicolon-after
	      (newline-between
	       "struct"
	       (space-between
		(braces-around
		 (newline-between
		  (semicolon-after
		   (space-between
		    "struct" (c:region e) (star-before (c:region))))
		  (if (reentrant? e)
		      (semicolon-after
		       (space-between *length* (c:region-size)))
		      (c:noop))
		  (semicolon-after
		   (space-between
		    (c:byte)
		    (c:raw-subscript (c:data) (c:big-region-size e))))))
		(c:initial-region e))))
	     ;; needs work: needs abstraction for initialized declaration
	     (semicolon-after
	      (space-between
	       "struct"
	       (c:region e)
	       (unparenthesize
		(c:= (star-before (c:region e))
		     (c:cast (space-between "struct" (c:region e) "*")
			     (c:& (c:initial-region e)))))))
	     (if (reentrant? e)
		 (newline-between
		  ;; needs work: needs abstraction for initialized declaration
		  (semicolon-after
		   (space-between
		    *length*
		    (unparenthesize
		     (c:= (c:region-size e) (c:big-region-size e)))))
		  ;; needs work: needs abstraction for initialized declaration
		  (semicolon-after
		   (space-between
		    (c:byte)
		    (unparenthesize
		     (c:= (star-before (c:fp e))
			  (c:& (c:subscript
				(c:. (c:initial-region e) (c:data))
				(c:0))))))))
		 (newline-between
		  (semicolon-after (space-between *length* (c:region-size e)))
		  (semicolon-after
		   (space-between (c:byte) (star-before (c:fp e)))))))
	    (newline-between
	     (c:define (c:big-region-size e) "8388608")
	     (semicolon-after
	      (space-between
	       (c:byte) (c:raw-subscript (c:region e) (c:big-region-size e))))
	     ;; needs work: needs abstraction for initialized declaration
	     (semicolon-after
	      (space-between
	       (c:byte)
	       (if (reentrant? e)
		   (unparenthesize (c:= (star-before (c:fp e)) (c:region e)))
		   (star-before (c:fp e)))))))
	(c:noop)))
   *es*)))

(define (compile-region-distribution)
 ;; needs work: To use code-generation abstractions.
 (newlines-between
  (map
   (lambda (e)
    (if (has-region? e)
	(cond
	 (*expandable-regions?*
	  (include! "Tmk")		;Tmk_distribute
	  (newline-between
	   (c:gosub "Tmk_distribute_hack"
		    (c:& (c:initial-region e)) (c:sizeof (c:initial-region e)))
	   (c:gosub "Tmk_distribute_hack"
		    (c:& (c:region e)) (c:sizeof (c:region e)))
	   (c:gosub "Tmk_distribute_hack"
		    (c:& (c:region-size e)) (c:sizeof (c:region-size e)))
	   (c:gosub "Tmk_distribute_hack" (c:& (c:fp e)) (c:sizeof (c:fp e)))))
	 (else
	  (include! "Tmk")		;Tmk_distribute
	  (newline-between
	   (c:gosub "Tmk_distribute_hack"
		    (c:& (c:region e)) (c:sizeof (c:region e)))
	   (c:gosub "Tmk_distribute_hack"
		    (c:& (c:fp e)) (c:sizeof (c:fp e))))))
	(c:noop)))
   *es*)))

(define (compile-closures)
 ;; needs work: To use code-generation abstractions.
 (newlines-between
  (map
   (lambda (e)
    (case *closure-representation*
     ((immediate-flat)
      (unimplemented #f "Immediate flat closures are not (yet) implemented"))
     ((indirect-flat)
      (unimplemented #f "Indirect flat closures are not (yet) implemented"))
     ((immediate-display indirect-display)
      (if (has-parent-parameter? e)
	  (semicolon-after
	   (newline-between
	    (space-between "struct" (c:p e))
	    (braces-around
	     (newlines-between
	      (map (lambda (e)
		    (semicolon-after
		     (space-between "struct" (c:e e) (star-before (c:e e)))))
		   (ancestors e))))))
	  (c:noop)))
     ((linked) (c:noop))
     (else (fuck-up))))
   *es*)))

(define (compile-type-declarations)
 ;; Check to see that the type declaration precedence graph is acyclic.
 ;; needs work: This double nested loop can be made more efficient.
 (when (some (lambda (w)
	      (some (lambda (u)
		     (and (structure-type-immediate? u)
			  (memq w (structure-type-slots u))
			  (member? u w)))
		    *structure-types*))
	     *ws*)
  (fuck-up))
 ;; needs work: To use code-generation abstractions.
 (newline-between
  ;; Nonheaded and displaced vectors come first, in any order, because they
  ;; reference everything else with * and are always referenced without *.
  (newlines-between
   (map (lambda (u)
	 (if (degenerate-vector-type? u)
	     (c:noop)
	     (semicolon-after
	      (newline-between
	       (space-between "struct" (c:u u))
	       (braces-around
		(newline-between
		 (semicolon-after (space-between *length* "length"))
		 (semicolon-after
		  (c:type-set (nonheaded-vector-type-element u)
			      (star-before "element")))))))))
	*nonheaded-vector-types*))
  (newlines-between
   (map (lambda (u)
	 (if (degenerate-vector-type? u)
	     (c:noop)
	     (semicolon-after
	      (newline-between
	       (space-between "struct" (c:u u))
	       (braces-around
		(newline-between
		 (semicolon-after (space-between *length* "length"))
		 (semicolon-after
		  (c:type-set (vector-type-element
			       (displaced-vector-type-displaced-vector-type u))
			      (star-before "element")))))))))
	*displaced-vector-types*))
  (newlines-between
   (map (lambda (u/w)
	 (cond
	  ((type-set? u/w)
	   (semicolon-after
	    (newline-between
	     (space-between "struct" (c:w u/w))
	     (braces-around
	      (newline-between
	       (semicolon-after (space-between *tag* "tag"))
	       (if (has-union? u/w)
		   (semicolon-after
		    (newline-between
		     "union"
		     (space-between
		      (braces-around
		       (newlines-between
			(map (lambda (u) (semicolon-after (c:type u (c:u u))))
			     (members-that (lambda (u)
					    (and (not (char-type? u))
						 (not (fictitious? u))))
					   u/w))))
		      "value")))
		   (semicolon-after
		    (c:type (the-member-that
			     (lambda (u)
			      (and (not (char-type? u)) (not (fictitious? u))))
			     u/w)
			    "value"))))))))
	  ((structure-type? u/w)
	   (semicolon-after
	    (newline-between
	     (space-between "struct" (c:u u/w))
	     (braces-around
	      (newlines-between
	       (map-indexed (lambda (w i)
			     (if (fictitious? w)
				 (c:noop)
				 (semicolon-after (c:type-set w (c:s i)))))
			    (structure-type-slots u/w)))))))
	  (else (fuck-up))))
	(topological-sort
	 (lambda (u/w1 u/w2)
	  (or
	   ;; Immediate structures must come before unions that contain them.
	   (and (structure-type? u/w1)
		(structure-type-immediate? u/w1)
		(type-set? u/w2)
		(member? u/w1 u/w2))
	   ;; Immediate structures must come before structures that have them
	   ;; as slots.
	   (and (structure-type? u/w1)
		(structure-type-immediate? u/w1)
		(structure-type? u/w2)
		(memq u/w1
		      (map the-member
			   (remove-if-not monomorphic?
					  (structure-type-slots u/w2)))))
	   ;; Unions must come before structures that have them as slots.
	   (and (type-set? u/w1)
		(structure-type? u/w2)
		(memq u/w1 (structure-type-slots u/w2)))))
	 (append (remove-if fictitious? *structure-types*)
		 (remove-if (lambda (w)
			     (or (fictitious? w)
				 (monomorphic? w)
				 (tag-only? w)
				 (squeezed? w)
				 (squished? w)))
			    *ws*)))))
  ;; Headed vectors come last, in any order, because they reference
  ;; everything else without * and are always referenced with *.
  (newlines-between
   (map (lambda (u)
	 (if (degenerate-vector-type? u)
	     (c:noop)
	     (semicolon-after
	      (newline-between
	       (space-between "struct" (c:u u))
	       (braces-around
		(newline-between
		 (semicolon-after (space-between *length* "length"))
		 (semicolon-after
		  (c:type-set (headed-vector-type-element u)
			      (c:raw-subscript "element" "1")))))))))
	*headed-vector-types*))))

(define (compile-closure-levels)
 ;; needs work: To use code-generation abstractions.
 (newlines-between
  (map (lambda (e)
	(if (has-closure? e)
	    (case *closure-representation*
	     ((immediate-flat)
	      (unimplemented
	       #f "Immediate flat closures are not (yet) implemented"))
	     ((indirect-flat)
	      (unimplemented
	       #f "Indirect flat closures are not (yet) implemented"))
	     ((immediate-display indirect-display)
	      (semicolon-after
	       (newline-between
		(space-between "struct" (c:e e))
		(braces-around
		 (newlines-between
		  (map (lambda (g)
			(semicolon-after
			 (c:type-set (variable-type-set g) (c:a g))))
		       (remove-if-not slotted? (variables e))))))))
	     ((linked)
	      (semicolon-after
	       (newline-between
		(space-between "struct" (c:p e))
		(braces-around
		 (newline-between
		  (if (has-parent-slot? e)
		      (semicolon-after
		       (space-between "struct"
				      (c:p (parent-slot e))
				      (star-before (c:p (parent-slot e)))))
		      (c:noop))
		  (newlines-between
		   (map (lambda (g)
			 (semicolon-after
			  (c:type-set (variable-type-set g) (c:a g))))
			(remove-if-not slotted? (variables e)))))))))
	     (else (fuck-up)))
	    (c:noop)))
       *es*)))

(define (compile-global-variables)
 (newlines-between
  (map (lambda (g)
	(c:declaration
	 (variable-type-set g) (c:a g) (symbol->string (variable-name g))))
       (remove-if-not global? *gs*))))

(define (compile-global-variable-distribution)
 (newlines-between
  (map (lambda (g)
	(include! "Tmk")		;Tmk_distribute
	(c:gosub "Tmk_distribute_hack" (c:& (c:a g)) (c:sizeof (c:a g))))
       (remove-if-not global? *gs*))))

(define (compile-prototype-variables e)
 (when (unique-call-site? e) (fuck-up))
 (if (environment-passes-parameters-globally? e)
     '()
     (let ((cs (map (lambda (g) (c:type-set (variable-type-set g) ""))
		    (remove-if-not (lambda (g) (or (local? g) (slotted? g)))
				   (variables e)))))
      (if (has-parent-parameter? e)
	  (cons (c:type (environment-type e) "") cs)
	  cs))))

(define (compile-parameter-variables e)
 (when (unique-call-site? e) (fuck-up))
 (if (environment-passes-parameters-globally? e)
     '()
     (let ((cs (map (lambda (g) (c:type-set (variable-type-set g) (c:a g)))
		    (remove-if-not (lambda (g) (or (local? g) (slotted? g)))
				   (variables e)))))
      (if (has-parent-parameter? e)
	  (cons (c:type (environment-type e) (c:p e)) cs)
	  cs))))

(define (compile-in-lined-variables e)
 ;; needs work: Can eliminate the nonslotted nonparameter variable of an OR
 ;;             that has been optimized away.
 (newlines-between
  (map (lambda (g)
	(c:declaration
	 (variable-type-set g) (c:a g) (symbol->string (variable-name g))))
       (remove-if-not
	(lambda (g) (or (local? g) (slotted? g)))
	(sort (reduce append
		      (map variables (properly-in-lined-environments e))
		      '())
	      <
	      variable-index)))))

(define (spill-slotted-variables e)
 (newline-between
  (case *closure-representation*
   ((immediate-flat indirect-flat indirect-display immediate-display) (c:noop))
   ((linked)
    (if (has-parent-slot? e)
	(c::= (c:-> (c:e e) (c:p (parent-slot e))) (parent-accessor e))
	(c:noop)))
   (else (fuck-up)))
  (newlines-between
   (map
    (lambda (g)
     (case *closure-representation*
      ((immediate-flat)
       (unimplemented #f "Immediate flat closures are not (yet) implemented"))
      ((indirect-flat)
       (unimplemented #f "Indirect flat closures are not (yet) implemented"))
      ((indirect-display immediate-display linked)
       (c::= (c:-> (c:e e) (c:a g)) (c:a g)))
      (else (fuck-up))))
    (remove-if-not slotted? (variables e))))))

(define *statements-per-constant-initialization-procedure* 3000)

(define (compile-constant-initialization-procedure-prototypes)
 (newlines-between
  (map-n (lambda (i)
	  (space-between "void" (c:prototype (c:initialize-constants i))))
	 (inexact->exact
	  (ceiling
	   (/ (length *inside-main*)
	      (exact->inexact
	       *statements-per-constant-initialization-procedure*)))))))

(define (compile-native-procedure-prototypes)
 (newlines-between
  (map
   (lambda (e)
    (if (unique-call-site? e)
	(c:noop)
	(let* ((cs (compile-prototype-variables e))
	       (c (apply
		   (if (or (environment-returns? e)
			   (environment-passes-parameters-globally? e)
			   ;; needs work: This is a bit overly conservative.
			   (some environment-passes-parameters-globally?
				 (callees e)))
		       c:prototype
		       c:noreturn-prototype)
			 (c:f e)
			 cs)))
	 (newline-between
	  (if (or (fictitious? (return-type-set e))
		  (not (expression-accessed?
			(expression-body (environment-expression e)))))
	      ;; needs work: To use code-generation abstractions.
	      (space-between "void" c)
	      (c:type-set (return-type-set e) c))
	  (if (environment-passes-parameters-globally? e)
	      (newline-between
	       (if (has-parent-parameter? e)
		   (semicolon-after (c:type (environment-type e) (c:d e)))
		   (c:noop))
	       (newlines-between
		(map (lambda (g)
		      (space-between
		       (semicolon-after
			(c:type-set (variable-type-set g) (c:b g)))
		       (c:/**/ (symbol->string (variable-name g)))))
		     (remove-if-not
		      (lambda (g) (or (local? g) (slotted? g)))
		      (variables e)))))
	      (c:noop))))))
   *es*)))

(define (compile-foreign-procedure-prototypes)
 (newlines-between
  (map (lambda (u)
	(if (foreign-procedure-type-called? u)
	    (cond ((foreign-procedure-type-include u)
		   (include! (foreign-procedure-type-include u))
		   (c:noop))
		  (else (c:foreign-type
			 (foreign-procedure-type-result u)
			 (apply (if (foreign-procedure-returns? u)
				    c:prototype
				    c:noreturn-prototype)
				(foreign-procedure-type-name u)
				(map (lambda (f) (c:foreign-type f ""))
				     (foreign-procedure-type-parameters u))))))
	    (c:noop)))
       *foreign-procedure-types*)))

(define *li* #f)

(define (allocate-label)
 (let ((l (c:l *li*)))
  (set! *li* (+ *li* 1))
  l))

(define (compile-goto l1 l2)
 (cond ((eq? l1 l2) (c:noop)) (else (unless l1 (fuck-up)) (c:goto l1))))

(define (unreturnify r)
 (if (return? r)
     (if (result-accessed? r)
	 (create-accessor-result (result-type-set r) (result-c r))
	 *discard*)
     r))

(define (contains? c1 c2)
 (or (eq? c1 c2)
     (and (pair? c1) (or (contains? (car c1) c2) (contains? (cdr c1) c2)))))

(define (compile-foreign-call r y u ts ws)
 (let ((ts (if (converted? y) (rest ts) ts))
       (ws (if (converted? y) (rest ws) ws)))
  (let loop ((fs1 (foreign-procedure-type-parameters u))
	     (ts1 ts)
	     (us1 '())
	     (ws1 ws))
   (if (null? fs1)
       (move-strict r
		    (apply c:call
			   (foreign-procedure-type-name u)
			   (map c:value ts (reverse us1) ws))
		    (foreign-procedure-return-type-set u))
       (type-switch
	(foreign-type? (first fs1))
	(first ws1)
	r
	(first ts1)
	(lambda (u0) (loop (rest fs1) (rest ts1) (cons u0 us1) (rest ws1)))
	(lambda (p?) (compile-error "foreign_call" y p?)))))))

(define (compile-pair+ r y ts ws t w)
 (cond
  ((discard? r) (c:noop))
  ((antecedent? r) (compile-goto (result-l1 r) (result-l0 r)))
  ((null? ws) (move r t w))
  (else
   (let loop ((uss (map members ws))
	      (w1 (result-type-set r))
	      (us1 '())
	      (ws1 '()))
    (if (null? uss)
	(let loop ((ts (reverse ts))
		   (ws (reverse ws))
		   (ts1 (reverse
			 (cons (result-c r)
			       (map allocate-temporary (rest (reverse ws1))))))
		   (us1 us1)
		   (ws1 ws1)
		   (c (c:noop))
		   (t t)
		   (w w))
	 (if (null? ts)
	     (newline-between c (compile-return r))
	     (let ((t2 (first ts))
		   (w2 (first ws))
		   (t1 (first ts1))
		   (u1 (first us1))
		   (w1 (first ws1)))
	      (loop
	       (rest ts)
	       (rest ws)
	       (rest ts1)
	       (rest us1)
	       (rest ws1)
	       (newline-between
		c
		(cond ((or (fictitious? w1)
			   (and (return? r) (not (result-accessed? r))))
		       (c:noop))
		      ((fictitious? u1)
		       (widen-type (create-accessor-result w1 t1) 'void10 u1))
		      (else (newline-between
			     (compile-allocate-structure t1 u1 w1 y)
			     (move (create-accessor-result
				    (pair-type-car u1) (value-car t1 u1 w1))
				   t2
				   w2)
			     (move (create-accessor-result
				    (pair-type-cdr u1) (value-cdr t1 u1 w1))
				   t
				   w)))))
	       t1
	       w1))))
	(let ((u1 (the-member-that
		   (pair+-type? uss (members w) (call-site-expression y)) w1)))
	 (loop (rest uss) (pair-type-cdr u1) (cons u1 us1) (cons w1 ws1))))))))

(define (compile-initialize-region e)
 (if (has-region? e)
     (if (reentrant? e)
	 (c::= (c:sfp e) (c:fp e))
	 (newline-between
	  (cond (*treadmarks?*
		 (include! "Tmk")	;Tmk_lock_acquire
		 (c:gosub "Tmk_lock_acquire" (c:0)))
		(else (c:noop)))
	  (if *expandable-regions?*
	      (newline-between
	       (c:while
		(c:!= (c:region e)
		      ;; needs work: To use code-generation abstractions.
		      (c:cast (space-between "struct" (c:region e) "*")
			      (c:& (c:initial-region e))))
		(newline-between
		 (semicolon-after
		  ;; needs work: To use code-generation abstractions.
		  (space-between
		   "struct" (c:region e) (star-before (c:region))))
		 (c::= (c:region) (c:region e))
		 (c::= (c:region e) (c:-> (c:region e) (c:region)))
		 (if *memory-messages?*
		     (c:printf
		      (c:string (format #f "Freeing region segment for ~a~%"
					(environment-name e))))
		     (c:noop))
		 (c:free (c:region) (has-nonatomic-region? e))))
	       (c::= (c:region-size e) (c:big-region-size e))
	       (c::= (c:fp e)
		     (c:& (c:subscript (c:-> (c:region e) (c:data)) (c:0)))))
	      (c::= (c:fp e) (c:region e)))
	  (c:align (c:fp e))
	  (cond (*treadmarks?*
		 (include! "Tmk")	;Tmk_lock_release
		 (c:gosub "Tmk_lock_release" (c:0)))
		(else (c:noop)))))
     (c:noop)))

(define (gather e0 y ts ws t w ts1 gs)
 (let loop ((ts ts) (ws ws) (t t) (w w) (ts1 ts1) (gs gs))
  (if (null? gs)
      (type-switch null-type?
		   w
		   *discard*
		   t
		   (lambda (u) (c:noop))
		   (lambda (p?) (compile-error "call" y p?)))
      (let* ((g (first gs))
	     (c (cond
		 ((and (pair? ts1) (or (local? g) (slotted? g))) (first ts1))
		 ((and (eq? ts1 #t) (not (global? g))) (c:b g))
		 ;; note: This must assign to the parameters and not the
		 ;;       slots since the self-tail-call entry point can't
		 ;;       come after the closure level allocation and spill
		 ;;       since that would unsoundly overwrite the existing
		 ;;       closure. cpstak.sc is an example of this.
		 (else (c:a g)))))
       (if (null? ws)
	   (if (and (rest? e0) (null? (rest gs)))
	       (if (or (local? g) (global? g) (slotted? g))
		   (move (create-accessor-result (variable-type-set g) c) t w)
		   (c:noop))
	       (type-switch
		pair-type?
		w
		*discard*
		t
		(lambda (u)
		 (newline-between
		  (if (or (local? g) (global? g) (slotted? g))
		      (move (create-accessor-result (variable-type-set g) c)
			    (value-car t u w)
			    (pair-type-car u))
		      (c:noop))
		  (loop ts
			ws
			(value-cdr t u w)
			(pair-type-cdr u)
			(if (and (pair? ts1) (or (local? g) (slotted? g)))
			    (rest ts1)
			    ts1)
			(rest gs))))
		(lambda (p?) (compile-error "call" y p?))))
	   (if (and (rest? e0) (null? (rest gs)))
	       (if (or (local? g) (global? g) (slotted? g))
		   (compile-pair+
		    (create-accessor-result (variable-type-set g) c)
		    y ts ws t w)
		   (c:noop))
	       ;; This is written this way, in a non-factored fashion, so that
	       ;; the last call to LOOP, which is the common case, is a tail
	       ;; call. When this was previously written in a factored fashion
	       ;; Trotsky would give a stack overflow when compiling
	       ;; benchmarks-to-latex.sc which is the only example, except for
	       ;; Marx, that I tried under Trotsky that had included
	       ;; QobiScheme.
	       (if (and (or (local? g) (global? g) (slotted? g))
			(not (eq? c (first ts))))
		   (newline-between
		    (move (create-accessor-result (variable-type-set g) c)
			  (first ts) (first ws))
		    (loop (rest ts)
			  (rest ws)
			  t
			  w
			  (if (and (pair? ts1) (or (local? g) (slotted? g)))
			      (rest ts1)
			      ts1)
			  (rest gs)))
		   (loop (rest ts)
			 (rest ws)
			 t
			 w
			 (if (and (pair? ts1) (or (local? g) (slotted? g)))
			     (rest ts1)
			     ts1)
			 (rest gs)))))))))

(define (compile-call r y t0 u0 w0 t1 w1 ts ws t w)
 ;; There is something slightly inefficient with the way rest arguments are
 ;; handled. Currently they are allocated by the caller, not the callee. So
 ;; if they never leave the callee they could have been allocated on the region
 ;; or the stack frame of the callee but they aren't. Instead, they will be
 ;; allocated on the region or the stack frame of the caller. If the caller
 ;; calls the callee in a loop before returning, this can delay storage
 ;; reclamation. It is for this same reason that you don't want the caller to
 ;; allocate the environment but rather you want the callee to allocate the
 ;; environment, which is how it is done now. Also, having the callee allocate
 ;; the environment and/or rest argument factors out common code.
 (cond
  ((primitive-procedure-type? u0)
   (when (can-be-non? null-type? w) (fuck-up))
   (if (some void? ws)
       (compile-error "void_primitive_procedure_call" y #t)
       ((primitive-procedure-compile-call
	 (cdr (assq (primitive-procedure-type-name u0)
		    *primitive-procedure-handlers*)))
	r y u0 ts ws t w
	(lambda (m) (compile-predicate m r (first ws) (first ts)))
	t1
	w1
	(if (>= (length ws) 1) (first ts) #f)
	(if (>= (length ws) 1) (first ws) #f)
	(if (>= (length ws) 2) (second ts) #f)
	(if (>= (length ws) 2) (second ws) #f)
	(if (>= (length ws) 3) (third ts) #f)
	(if (>= (length ws) 3) (third ws) #f))))
  ((native-procedure-type? u0)
   (let* ((e (expression-environment (call-site-expression y)))
	  (e0 (callee-environment u0 y))
	  (x0 (environment-expression e0))
	  (gs (variables e0)))
    (cond
     ((not (called? e0))
      ;; We should never get here because the callee or the arguments don't
      ;; return. Actually, if we ever fix up the unlinking of structures then
      ;; E0 should not even exist at this point.
      (c:noop))
     ((noop? e0) (compile-return r))
     ((can-be-self-tail-call-to? y e0)
      (newline-between
       ;; I'm not sure that the following is needed.
       (if (has-parent-parameter? e0)
	   (c::= (c:p e0) (c:value t0 u0 w0))
	   (c:noop))
       (gather e0 y ts ws t w #f gs)
       (c:goto (c:h e0))))
     ((unique-call-site? e0)
      (newline-between
       (if (has-parent-parameter? e0)
	   (c::= (c:p e0) (c:value t0 u0 w0))
	   (c:noop))
       (gather e0 y ts ws t w #f gs)
       (newline-between
	(compile-initialize-region e0)
	(if (has-self-tail-call? e0) (c:: (c:h e0)) (c:noop))
	;; note: The self-tail-call entry point can't come after the closure
	;;       level allocation and spill since that would unsoundly
	;;       overwrite the existing closure.
	;;       cpstak.sc is an example of this.
	(if (has-closure? e0)
	    (newline-between (compile-allocate-closure-level e0)
			     (spill-slotted-variables e0))
	    (c:noop))
	(if (restore? e0)
	    (newline-between (compile (unreturnify r) (expression-body x0))
			     (compile-restore e0)
			     (compile-return r))
	    (compile r (expression-body x0))))))
     ((environment-passes-parameters-globally? e0)
      (let* ((c (c:call (c:f e0))))
       (newline-between
	(if (has-parent-parameter? e0)
	    (c::= (c:d e0) (c:value t0 u0 w0))
	    (c:noop))
	(gather e0 y ts ws t w #t gs)
	(if (expression-accessed?
	     (expression-body (environment-expression e0)))
	    (if (or (discard? r)
		    (antecedent? r)
		    (eq? (result-type-set r) (return-type-set e0)))
		(if (and (return? r) (restore? r))
		    (newline-between
		     (move-strict (unreturnify r) c (return-type-set e0))
		     (compile-return r))
		    (move-strict r c (return-type-set e0)))
		(let ((t (allocate-temporary (return-type-set e0))))
		 (newline-between
		  (move-strict (create-accessor-result (return-type-set e0) t)
			       c
			       (return-type-set e0))
		  (move r t (return-type-set e0)))))
	    (newline-between (semicolon-after c) (compile-return r))))))
     (else
      (let* ((ts1
	      (if (and (must-be? null-type? w) (not (rest? e0)))
		  ;; This is a small amount of copy propagation.
		  (removeq
		   #f
		   (map (lambda (g w t)
			 (if (or (local? g) (slotted? g))
			     (if (eq? w (variable-type-set g))
				 t
				 (allocate-temporary (variable-type-set g)))
			     #f))
			gs ws ts))
		  (removeq
		   #f
		   (map (lambda (g)
			 (if (or (local? g) (slotted? g))
			     (allocate-temporary (variable-type-set g))
			     #f))
			gs))))
	     (c (if (has-parent-parameter? e0)
		    (apply c:call (c:f e0) (c:value t0 u0 w0) ts1)
		    (apply c:call (c:f e0) ts1))))
       (newline-between
	(gather e0 y ts ws t w ts1 gs)
	(if (expression-accessed?
	     (expression-body (environment-expression e0)))
	    (if (or (discard? r)
		    (antecedent? r)
		    (eq? (result-type-set r) (return-type-set e0)))
		(if (and (return? r) (restore? r))
		    (newline-between
		     (move-strict (unreturnify r) c (return-type-set e0))
		     (compile-return r))
		    (move-strict r c (return-type-set e0)))
		(let ((t (allocate-temporary (return-type-set e0))))
		 (newline-between
		  (move-strict (create-accessor-result (return-type-set e0) t)
			       c
			       (return-type-set e0))
		  (move r t (return-type-set e0)))))
	    (newline-between
	     (semicolon-after c)
	     (if (and (native-procedure-type? u0)
		      (converted-continuation? (callee-environment u0 y)))
		 ;; This case was instituted to fix the bug in except.sc. I'm
		 ;; not sure that this is the correct was to fix the bug. And I
		 ;; don't know if we need to do a compile-restore here.
		 (c:return)
		 (compile-return r))))))))))
  ((foreign-procedure-type? u0)
   (when (can-be-non? null-type? w)
    (unimplemented y "APPLY of a foreign procedure is not (yet) implemented"))
   (cond ((not (foreign-procedure-type-called? u0))
	  ;; We should never get here because the callee or the arguments don't
	  ;; return.
	  (c:noop))
	 ((some void? ws) (compile-error "void_foreign_procedure_call" y #t))
	 (else (compile-foreign-call r y u0 ts ws))))
  ((continuation-type? u0)
   ;; needs work: A call to a continuation should restore all intervening
   ;;             reentrant regions. Because we don't do this (yet), we need
   ;;             HAS-EXTERNAL-CALL? to prevent allocation on intervening
   ;;             reentrant regions because otherwise there would be a memory
   ;;             leak when a call to this continuation occurs.
   (when (can-be-non? null-type? w)
    (unimplemented y "APPLY of a continuation is not (yet) implemented"))
   (let ((t1 (if (converted? y) (second ts) (first ts)))
	 (w1 (if (converted? y) (second ws) (first ws))))
    (cond
     ((not (continuation-type-continuation-accessed? u0))
      ;; We should never get here because the callee or the arguments don't
      ;; return.
      (c:noop))
     ((goto? y u0)
      (newline-between
       ;; needs work: EXPRESSION-RESULT might not be set yet since we might
       ;;             not yet have COMPILEd
       ;;             (CONTINUATION-TYPE-ALLOCATING-EXPRESSION U0).
       (move (expression-result (continuation-type-allocating-expression u0))
	     t1
	     w1)
       (if (return?
	    (expression-result (continuation-type-allocating-expression u0)))
	   (c:noop)
	   (c:goto (c:x (continuation-type-allocating-expression u0))))))
     (else (newline-between
	    (move (if (or (fictitious?
			   (expression-type-set
			    (continuation-type-allocating-expression u0)))
			  (not (expression-accessed?
				(continuation-type-allocating-expression u0))))
		      *discard*
		      (create-accessor-result
		       (expression-type-set
			(continuation-type-allocating-expression u0))
		       (c:v (continuation-type-allocating-expression u0))))
		  t1
		  w1)
	    (c:longjmp (c:* (c:value t0 u0 w0)) (c:1)))))))
  (else (fuck-up))))

(define (continuation-argument-type-set u y)
 (unless (native-procedure-type? u) (fuck-up))
 (first-parameter-type-set
  (callee-environment u (recreate-call-site y 'continuation-argument))))

(define (compile-converted-call r y t0 u0 w0 ts ws t w)
 (unless (procedure-type? u0) (fuck-up))
 (when (continuation-type? u0) (unimplemented y "unimplemented"))
 (cond
  ;; CALL/CC==(LAMBDA (C X) (X C C))
  (((primitive-procedure-type-named? 'call-with-current-continuation) u0)
   (when (can-be-non? null-type? w) (fuck-up))
   (when #f				;debugging
    (when (can-be?
	   (lambda (u2)
	    (and
	     ((compatible-procedure? (list (first ws))
				     *null*
				     (recreate-call-site y 'first-argument))
	      u2)
	     (not (converted? (callee-environment u2 y)))))
	   (second ws))
     (unimplemented y "unimplemented")))
   (type-switch
    (compatible-procedure? (list (first ws) (first ws))
			   *null*
			   (recreate-call-site y 'first-argument))
    (second ws)
    r
    (second ts)
    (lambda (u2)
     (compile-converted-call
      r
      (recreate-call-site y 'first-argument)
      (second ts) u2 (second ws)
      (list (first ts) (first ts))
      (list (first ws) (first ws))
      'void11 *null*))
    (lambda (p?) (compile-error "call_with_current_continuation" y p?))))
  ((and (native-procedure-type? u0) (converted? (callee-environment u0 y)))
   (compile-call r y t0 u0 w0 (first ts) (first ws) ts ws t w))
  (((needs-implicit-continuation-call? ws w y) u0)
   (let* ((w1 (minp subtype-set?
		    (map (lambda (u) (continuation-argument-type-set u y))
			 (members (first ws)))))
	  (t1 (allocate-temporary w1)))
    (when (can-be-non?
	   (lambda (u) (subtype-set? w1 (continuation-argument-type-set u y)))
	   (first ws))
     (fuck-up))
    (newline-between
     (compile-call
      (if (fictitious? w1) *discard* (create-accessor-result w1 t1))
      y t0 u0 w0 (first ts) (first ws) (rest ts) (rest ws) t w)
     ;; This relies on the fact that the implicit continuation call is never
     ;; done through APPLY.
     (type-switch
      (compatible-procedure?
       (list w1) *null* (recreate-call-site y 'continuation-argument))
      (first ws)
      r
      (first ts)
      (lambda (u1)
       (compile-call
	r (recreate-call-site y 'continuation-argument)
	(first ts) u1 (first ws) #f #f (list t1) (list w1) 'void12 *null*))
      (lambda (p?) (c:noop))))))
  (else
   (compile-call r y t0 u0 w0 (first ts) (first ws) (rest ts) (rest ws) t w))))

(define (compile-antecedent x l1 l2 l0)
 ;; If the antecedent is true branch to L1 otherwise branch to L2 assuming
 ;; that L0 is the immediately following label so that no branch is generated
 ;; to that label and flow falls through instead.
 ;; needs work: To ignore body of a lambda expression that appears in an
 ;;             optimized antecedent.
 ;; needs work: To not generate constants that are used in an optimized
 ;;             antecedent.
 (compile (create-antecedent-result (expression-type-set x) l1 l2 l0) x))

(define (and-expression? x)
 ;; (IF x y #F)
 (and (eq? (expression-kind x) 'if)
      (eq? (expression-kind (expression-alternate x)) 'false-constant)))

(define (or-expression? x)
 ;; ((LAMBDA (X) (IF X X y)) x)
 (and
  (eq? (expression-kind x) 'call)
  (= (length (expression-arguments x)) 1)
  (let ((w (expression-type-set (expression-callee x))))
   (and
    (monomorphic? w)
    (let ((u (the-member w)))
     (and
      (native-procedure-type? u)
      ((compatible-call? x) u)
      (let ((e0 (callee-environment u (create-call-site x))))
       (and
	(called? e0)
	(not (noop? e0))
	(unique-call-site? e0)
	(let ((x0 (environment-expression e0)))
	 (and (not (rest? x0))
	      (= (length (variables x0)) 1)
	      (let ((x1 (expression-body x0)))
	       (and (eq? (expression-kind x1) 'if)
		    (eq? (expression-kind (expression-antecedent x1)) 'access)
		    (eq? (expression-kind (expression-consequent x1)) 'access)
		    (eq? (expression-variable (expression-antecedent x1))
			 (expression-variable (expression-consequent x1)))
		    (eq? (expression-variable (expression-antecedent x1))
			 (first (variables x0)))))))))))))))

(define (not-expression? x)
 ;; (NOT x)
 (and (eq? (expression-kind x) 'call)
      (= (length (expression-arguments x)) 1)
      (can-be? (primitive-procedure-type-named? 'not)
	       (expression-type-set (expression-callee x)))
      (must-be? (primitive-procedure-type-named? 'not)
		(expression-type-set (expression-callee x)))))

(define (maybe-mark-no-return x c)
 (let ((c (newline-between
	   (if (expression-pathname x)
	       (c:/**/ (string-append
			"x"
			(number->string (expression-index x))
			" "
			(strip-directory (expression-pathname x))
			":"
			(number->string (expression-line-position x))
			":"
			(number->string (expression-character-position x))))
	       (c:/**/
		(string-append "x" (number->string (expression-index x)))))
	   c)))
  (if (expression-returns? x) c (c:no-return c))))

(define (compile r x)
 (clock-sample)				;To prevent overflow.
 (set-expression-result! x r)
 (maybe-mark-no-return
  x
  (let ((e (expression-environment x)))
   (if (and (not (antecedent? r))
	    (must-be? boolean-type? (expression-type-set x))
	    (can-be-non? true-type? (expression-type-set x))
	    (can-be-non? false-type? (expression-type-set x))
	    (or (and-expression? x) (or-expression? x) (not-expression? x)))
       (let* ((l1 (allocate-label))
	      (l2 (allocate-label))
	      (l3 (allocate-label))
	      (c (compile-antecedent x l1 l2 l1)))
	(newline-between c
			 (if (contains? c l1) (c:: l1) (c:noop))
			 (return-true r)
			 (if (return? r) (c:noop) (compile-goto l3 l2))
			 (c:: l2)
			 (return-false r)
			 (if (return? r) (c:noop) (c:: l3))))
       (case (expression-kind x)
	((null-constant) (widen r 'void13 null-type?))
	((true-constant) (return-true r))
	((false-constant) (return-false r))
	((char-constant)
	 (widen r (c:character (expression-constant x)) char-type?))
	((fixnum-constant)
	 (widen r (c:fixnum (expression-constant x)) fixnum-type?))
	((flonum-constant)
	 (widen r (c:flonum (expression-constant x)) flonum-type?))
	((rectangular-constant)
	 (unimplemented y "Cannot (yet) handle rectangular constants"))
	((string-constant)
	 (when (some (lambda (c) (zero? (char->integer c)))
		     (string->list (expression-constant x)))
	  (unimplemented
	   x "Strings that contain ASCII NULs are not (yet) implemented"))
	 (widen r (c:string (expression-constant x)) string-type?))
	((symbol-constant)
	 (cond
	  (*treat-all-symbols-as-external?*
	   (unless (memq (expression-constant x) *symbols*)
	    (set! *symbols* (append *symbols* (list (expression-constant x))))
	    (outside-main
	     ;; needs work: needs abstraction for initialized declaration
	     (semicolon-after
	      (space-between
	       *char*
	       (unparenthesize
		(c:= (star-before
		      (c:q (positionq (expression-constant x) *symbols*)))
		     (c:string (symbol->string (expression-constant x)))))))))
	   (widen r
		  (c:q (positionq (expression-constant x) *symbols*))
		  (lambda (u)
		   (and (external-symbol-type? u)
			(eq? (external-symbol-type-displaced-string-type u)
			     <nonreclaimable-string>)))))
	  (else
	   (widen
	    r 'void14 (internal-symbol-type-named? (expression-constant x))))))
	((pair-constant)
	 ;; This is THE-MEMBER-THAT and not THE-MEMBER because when
	 ;; *INDEX-CONSTANT-STRUCTURE-TYPES-BY-EXPRESSION?* is true the
	 ;; expression type set might not be a singleton.
	 (let ((u (the-member-that
		   (lambda (u)
		    (and (pair-type? u)
			 (subtype-set?
			  (expression-type-set (car (expression-constant x)))
			  (pair-type-car u))
			 (subtype-set?
			  (expression-type-set (cdr (expression-constant x)))
			  (pair-type-cdr u))))
		   (expression-type-set x))))
	  (if (fictitious? u)
	      (widen-type r 'void15 u)
	      (let ((w1 (pair-type-car u))
		    (w2 (pair-type-cdr u)))
	       (if (structure-type-immediate? u)
		   (cond
		    ((discard? r) (c:noop))
		    ((antecedent? r)
		     (compile-goto (result-l1 r) (result-l0 r)))
		    ((and (return? r) (not (result-accessed? r)))
		     (compile-return r))
		    (else
		     (newline-between
		      (if (and (multimorphic? (result-type-set r))
			       (not (squeezed? (result-type-set r)))
			       (not (squished? (result-type-set r))))
			  (c::= (c:tag (result-c r) (result-type-set r))
				(c:type-tag u))
			  (c:noop))
		      (if (fictitious? w1)
			  (c:noop)
			  (compile
			   (create-accessor-result
			    w1 (value-car (result-c r) u (result-type-set r)))
			   (car (expression-constant x))))
		      (if (fictitious? w2)
			  (c:noop)
			  (compile
			   (create-accessor-result
			    w2 (value-cdr (result-c r) u (result-type-set r)))
			   (cdr (expression-constant x))))
		      (compile-return r))))
		   (let ((t (c:t *ti*)))
		    (set! *ti* (+ *ti* 1))
		    (outside-main (semicolon-after (c:type& u t)))
		    (unless (fictitious? w1)
		     (inside-main
		      (compile (create-accessor-result w1 (c:. t (c:s 0)))
			       (car (expression-constant x)))))
		    (unless (fictitious? w2)
		     (inside-main
		      (compile (create-accessor-result w2 (c:. t (c:s 1)))
			       (cdr (expression-constant x)))))
		    (widen-type r (c:& t) u)))))))
	((vector-constant)
	 ;; This is THE-MEMBER-THAT and not THE-MEMBER because when
	 ;; *INDEX-CONSTANT-HEADED-VECTOR-TYPES-BY-EXPRESSION?* is true the
	 ;; expression type set might not be a singleton.
	 (let ((u (the-member-that
		   (lambda (u)
		    (and (headed-vector-type? u)
			 (every-vector
			  (lambda (x)
			   (subtype-set? (expression-type-set x)
					 (headed-vector-type-element u)))
			  (expression-constant x))))
		   (expression-type-set x))))
	  (if (degenerate-vector-type? u)
	      (widen-type
	       r (c:fixnum (vector-length (expression-constant x))) u)
	      ;; needs work: To use code-generation abstractions.
	      (let ((t (c:t *ti*))
		    (w1 (headed-vector-type-element u)))
	       (set! *ti* (+ *ti* 1))
	       (outside-main
		(semicolon-after
		 (newline-between
		  "struct"
		  (space-between
		   (braces-around
		    (newline-between
		     (semicolon-after
		      (space-between *length* "length"))
		     (semicolon-after
		      (c:type-set
		       w1
		       (c:raw-subscript
			"element"
			(c:fixnum
			 (max 1 (vector-length (expression-constant x)))))))))
		   t))))
	       (inside-main
		(c::= (c:. t "length")
		      (c:fixnum (vector-length (expression-constant x)))))
	       (for-each-n
		(lambda (i)
		 (inside-main
		  (compile (create-accessor-result
			    w1 (c:subscript (c:. t "element") (c:fixnum i)))
			   (vector-ref (expression-constant x) i))))
		(vector-length (expression-constant x)))
	       (widen-type r (c:type-cast (c:& t) u) u)))))
	((lambda converted-lambda converted-continuation)
	 (let ((u (the-member (expression-type-set x))))
	  (cond
	   ((discard? r) (c:noop))
	   ((antecedent? r) (compile-goto (result-l1 r) (result-l0 r)))
	   ((and (return? r) (not (result-accessed? r))) (compile-return r))
	   ((fictitious? u) (widen-type r 'void16 u))
	   (else
	    (case *closure-representation*
	     ((immediate-flat)
	      (unimplemented
	       x "Immediate flat closures are not (yet) implemented"))
	     ((indirect-flat)
	      (unimplemented
	       x "Indirect flat closures are not (yet) implemented"))
	     ((immediate-display)
	      (when (or (squeezed? (result-type-set r))
			(squished? (result-type-set r)))
	       (fuck-up))
	      (newline-between
	       (if (multimorphic? (result-type-set r))
		   (c::= (c:tag (result-c r) (result-type-set r))
			 (c:type-tag u))
		   (c:noop))
	       (newlines-between
		(map (lambda (e1)
		      (c::= (c:. (c:value (result-c r) u (result-type-set r))
				 (c:e e1))
			    (if (eq? e1 e) (c:e e1) (c:. (c:p e) (c:e e1)))))
		     (ancestors u)))))
	     ((indirect-display)
	      ;; needs work: To allocate the closure.
	      (unimplemented
	       x "Indirect display closures are not (yet) implemented")
	      (when (or (squeezed? (result-type-set r))
			(squished? (result-type-set r)))
	       (fuck-up))
	      (newline-between
	       (if (multimorphic? (result-type-set r))
		   (c::= (c:tag (result-c r) (result-type-set r))
			 (c:type-tag u))
		   (c:noop))
	       (newlines-between
		(map (lambda (e1)
		      (c::= (c:-> (c:value (result-c r) u (result-type-set r))
				  (c:e e1))
			    (if (eq? e1 e) (c:e e1) (c:-> (c:p e) (c:e e1)))))
		     (ancestors u)))))
	     ((linked) (widen-type r (lambda-accessor u e) u))
	     (else (fuck-up)))))))
	((set!)
	 (newline-between
	  (compile (if (and (or (local? (expression-variable x))
				(global? (expression-variable x))
				(slotted? (expression-variable x)))
			    (nontrivial-reference? x)
			    (executed? x))
		       (create-accessor-result
			(variable-type-set (expression-variable x))
			(accessor (expression-variable x) e))
		       *discard*)
		   (expression-source x))
	  (if (expression-returns? x) (compile-return r) (c:noop))))
	((if)
	 (if (and (antecedent? r) (and-expression? x))
	     (if (reached? (expression-consequent x))
		 (if (reached? (expression-alternate x))
		     (let* ((l3 (allocate-label))
			    (c (compile-antecedent
				(expression-antecedent x)
				l3
				(result-l2 r) l3)))
		      (newline-between
		       c
		       (if (contains? c l3) (c:: l3) (c:noop))
		       (compile-antecedent (expression-consequent x)
					   (result-l1 r)
					   (result-l2 r)
					   (result-l0 r))))
		     (let* ((l3 (allocate-label))
			    (c (compile-antecedent
				(expression-antecedent x) l3 #f l3)))
		      (newline-between
		       c
		       (if (contains? c l3) (c:: l3) (c:noop))
		       (compile-antecedent (expression-consequent x)
					   (result-l1 r)
					   (result-l2 r)
					   (result-l0 r)))))
		 (if (reached? (expression-alternate x))
		     (compile-antecedent
		      (expression-antecedent x) #f (result-l2 r) (result-l0 r))
		     (compile-antecedent
		      (expression-antecedent x) #f #f (result-l0 r))))
	     (let ((w (expression-type-set (expression-antecedent x))))
	      (if (reached? (expression-consequent x))
		  (if (reached? (expression-alternate x))
		      (if (or (return? r)
			      ;; needs work: This is an attempt to eliminate
			      ;;             the dead branch around the
			      ;;             alternate if the consequent is a
			      ;;             self tail call. But it sometimes
			      ;;             fails as in kilo/browse.sc. The
			      ;;             reason this fails is because the
			      ;;             consequent is not a self tail
			      ;;             call but an IF whose both reached
			      ;;             branches are self tail calls, a
			      ;;             call to an in in-lined procedure
			      ;;             whose body is a self tail call,
			      ;;             or some combination thereof.
			      ;;             Anyway, should also eliminate the
			      ;;             dead branch when the consequent
			      ;;             doesn't return as would be the
			      ;;             case if it were a call to a
			      ;;             continuation.
			      ;;             This can't be ANTECEDENT? here
			      ;;             because antecedent results can
			      ;;             fall through.
			      (must-be-self-tail-call?
			       (expression-consequent x)))
			  (let* ((l1 (allocate-label))
				 (l2 (allocate-label))
				 (c (compile-antecedent
				     (expression-antecedent x) l1 l2 l1)))
			   (newline-between
			    c
			    (if (contains? c l1) (c:: l1) (c:noop))
			    (compile r (expression-consequent x))
			    (c:: l2)
			    (compile r (expression-alternate x))))
			  (let* ((l1 (allocate-label))
				 (l2 (allocate-label))
				 (l3 (allocate-label))
				 (c (compile-antecedent
				     (expression-antecedent x) l1 l2 l1)))
			   (newline-between
			    c
			    (if (contains? c l1) (c:: l1) (c:noop))
			    (compile r (expression-consequent x))
			    (compile-goto l3 l2)
			    (c:: l2)
			    (compile r (expression-alternate x))
			    (c:: l3))))
		      (let* ((l1 (allocate-label))
			     (c (compile-antecedent
				 (expression-antecedent x) l1 #f l1)))
		       (newline-between
			c
			(if (contains? c l1) (c:: l1) (c:noop))
			(compile r (expression-consequent x)))))
		  (if (reached? (expression-alternate x))
		      (let* ((l1 (allocate-label))
			     (c (compile-antecedent
				 (expression-antecedent x) #f l1 l1)))
		       (newline-between
			c
			(if (contains? c l1) (c:: l1) (c:noop))
			(compile r (expression-alternate x))))
		      (newline-between
		       (compile-antecedent (expression-antecedent x) #f #f #f)
		       (if (expression-returns? (expression-antecedent x))
			   (compile-error "void_if" x #t)
			   (c:noop))))))))
	((primitive-procedure)
	 (widen-type r 'void17 (the-member (expression-type-set x))))
	((foreign-procedure)
	 (widen-type r 'void18 (the-member (expression-type-set x))))
	((access)
	 (if (expression-accessed? x)
	     (cond
	      ((and (hidden? (expression-variable x))
		    (not (discard? r))
		    (not (antecedent? r)))
	       (case *closure-representation*
		((immediate-flat)
		 (unimplemented
		  x "Immediate flat closures are not (yet) implemented"))
		((indirect-flat)
		 (unimplemented
		  x "Indirect flat closures are not (yet) implemented"))
		((immediate-display)
		 (when (or (squeezed? (result-type-set r))
			   (squished? (result-type-set r)))
		  (fuck-up))
		 (let ((u (the-member
			   (variable-type-set (expression-variable x)))))
		  (newline-between
		   (if (multimorphic? (result-type-set r))
		       (c::= (c:tag (result-c r) (result-type-set r))
			     (c:type-tag u))
		       (c:noop))
		   (newlines-between
		    (map
		     (lambda (e1)
		      (c::= (c:. (c:value (result-c r) u (result-type-set r))
				 (c:e e1))
			    (if (eq? e1 e) (c:e e1) (c:. (c:p e) (c:e e1)))))
		     (ancestors u))))))
		((indirect-display)
		 ;; note: Accessing a hidden variable with indirect
		 ;;       display closures requires consing.
		 ;; needs work: To allocate the closure.
		 (unimplemented
		  x "Indirect display closures are not (yet) implemented")
		 (when (or (squeezed? (result-type-set r))
			   (squished? (result-type-set r)))
		  (fuck-up))
		 (let ((u (the-member
			   (variable-type-set (expression-variable x)))))
		  (newline-between
		   (if (multimorphic? (result-type-set r))
		       (c::= (c:tag (result-c r) (result-type-set r))
			     (c:type-tag u))
		       (c:noop))
		   (newlines-between
		    (map
		     (lambda (e1)
		      (c::= (c:-> (c:value (result-c r) u (result-type-set r))
				  (c:e e1))
			    (if (eq? e1 e) (c:e e1) (c:-> (c:p e) (c:e e1)))))
		     (ancestors u))))))
		((linked)
		 (move-access r
			      (accessor (expression-variable x) e)
			      (variable-type-set (expression-variable x))
			      (expression-type-set x)))
		(else (fuck-up))))
	      (else
	       (move-access
		r
		(if (or
		     (fictitious? (variable-type-set (expression-variable x)))
		     (hidden? (expression-variable x)))
		    'void19
		    (accessor (expression-variable x) e))
		(variable-type-set (expression-variable x))
		(expression-type-set x))))
	     (compile-return r)))
	((call converted-call)
	 (cond
	  ((and (antecedent? r) (or-expression? x))
	   (let* ((u (the-member (expression-type-set (expression-callee x))))
		  (e0 (callee-environment u (create-call-site x)))
		  (x0 (environment-expression e0))
		  (x1 (expression-body x0))
		  (x2 (first (expression-arguments x)))
		  (e1 (expression-environment x)))
	    (if (reached? (expression-consequent x1))
		(if (reached? (expression-alternate x1))
		    (let* ((l3 (allocate-label))
			   (c (compile-antecedent x2 (result-l1 r) l3 l3)))
		     (newline-between
		      c
		      (if (contains? c l3) (c:: l3) (c:noop))
		      (compile-initialize-region e0)
		      (if (has-parent-parameter? e0)
			  (case *closure-representation*
			   ((immediate-flat)
			    (unimplemented x "Immediate flat closures are not (yet) implemented"))
			   ((indirect-flat)
			    (unimplemented x "Indirect flat closures are not (yet) implemented"))
			   ((immediate-display)
			    (newlines-between
			     (map (lambda (e)
				   (c::= (c:. (c:p e0) (c:e e))
					 (if (eq? e e1)
					     (c:e e)
					     (c:. (c:p e1) (c:e e)))))
				  (ancestors u))))
			   ((indirect-display)
			    ;; needs work: To allocate the closure.
			    (unimplemented x "Indirect display closures are not (yet) implemented")
			    (newlines-between
			     (map (lambda (e)
				   (c::= (c:-> (c:p e0) (c:e e))
					 (if (eq? e e1)
					     (c:e e)
					     (c:-> (c:p e1) (c:e e)))))
				  (ancestors u))))
			   ((linked) (c::= (c:p e0) (lambda-accessor u e)))
			   (else (fuck-up)))
			  (c:noop))
		      ;; needs work: There can be a memory leak here if E0 is
		      ;;             reentrant and has a region because there
		      ;;             is no (COMPILE-RESTORE E0).
		      (compile-antecedent (expression-alternate x1)
					  (result-l1 r)
					  (result-l2 r)
					  (result-l0 r))))
		    (compile-antecedent x2 (result-l1 r) #f (result-l0 r)))
		(if (reached? (expression-alternate x1))
		    (let* ((l3 (allocate-label))
			   (c (compile-antecedent x2 #f l3 l3)))
		     (newline-between
		      c
		      (if (contains? c l3) (c:: l3) (c:noop))
		      (compile-initialize-region e0)
		      (if (has-parent-parameter? e0)
			  (case *closure-representation*
			   ((immediate-flat)
			    (unimplemented x "Immediate flat closures are not (yet) implemented"))
			   ((indirect-flat)
			    (unimplemented x "Indirect flat closures are not (yet) implemented"))
			   ((immediate-display)
			    (newlines-between
			     (map (lambda (e)
				   (c::= (c:. (c:p e0) (c:e e))
					 (if (eq? e e1)
					     (c:e e)
					     (c:. (c:p e1) (c:e e)))))
				  (ancestors u))))
			   ((indirect-display)
			    ;; needs work: To allocate the closure.
			    (unimplemented x "Indirect display closures are not (yet) implemented")
			    (newlines-between
			     (map (lambda (e)
				   (c::= (c:-> (c:p e0) (c:e e))
					 (if (eq? e e1)
					     (c:e e)
					     (c:-> (c:p e1) (c:e e)))))
				  (ancestors u))))
			   ((linked) (c::= (c:p e0) (lambda-accessor u e)))
			   (else (fuck-up)))
			  (c:noop))
		      ;; needs work: There can be a memory leak here if E0 is
		      ;;             reentrant and has a region because there
		      ;;             is no (COMPILE-RESTORE E0).
		      (compile-antecedent (expression-alternate x1)
					  (result-l1 r)
					  (result-l2 r)
					  (result-l0 r))))
		    (compile-antecedent x2 #f #f (result-l0 r))))))
	  ((and (antecedent? r) (not-expression? x))
	   (compile-antecedent (first (expression-arguments x))
			       (result-l2 r)
			       (result-l1 r)
			       (result-l0 r)))
	  (else
	   (let* ((w0 (expression-type-set (expression-callee x)))
		  (ws (map expression-type-set (expression-arguments x)))
		  (t0 (allocate-temporary w0))
		  (ts (map allocate-temporary ws)))
	    (newline-between
	     ;; needs work: Should not evaluate any arguments if callee doesn't
	     ;;             return and should not evaluate an argument that
	     ;;             follows an argument that doesn't return. This
	     ;;             assumes a left-to-right evaluation order. As Olin
	     ;;             Shivers pointed out, since evaluation order is
	     ;;             unspecified you can abort if any subexpression
	     ;;             doesn't return. But given the way the propagator
	     ;;             works, we can only determine whether an expression
	     ;;             returns by asserting it as used. So we have to pick
	     ;;             some order and it might as well be left to right.
	     ;; note: The callee is evaluated after the arguments in attempt to
	     ;;       match the Scheme->C argument evaluation order.
	     (newlines-between
	      ;; note: This is a kludge to reverse the evaluation order of
	      ;;       rest arguments in attempt to match the Scheme->C
	      ;;       argument evaluation order.
	      ((if (and (monomorphic? w0)
			(native-procedure-type? (the-member w0))
			(called? (the-member w0))
			(rest? (the-member w0)))
		   reverse
		   identity)
	       (map (lambda (w t x)
		     (compile (if (expression-accessed? x)
				  (create-accessor-result w t)
				  *discard*)
			      x))
		    ws ts (expression-arguments x))))
	     (compile (if (expression-accessed? (expression-callee x))
			  (create-accessor-result w0 t0)
			  *discard*)
		      (expression-callee x))
	     (if (executed? x)
		 (if (void? w0)
		     (compile-error "void_call" x #t)
		     (type-switch
		      (compatible-call? x)
		      w0
		      r
		      t0
		      (lambda (u0)
		       (if (converted? x)
			   (compile-converted-call
			    r (create-call-site x)
			    t0 u0 w0 ts ws 'void20 *null*)
			   (compile-call r (create-call-site x) t0 u0 w0
					 #f #f ts ws 'void21  *null*)))
		      (lambda (p?) (compile-error "call" x p?))))
		 (c:noop)))))))
	(else (fuck-up)))))))

(define (compile-native-procedures)
 (newlines-between
  (map
   (lambda (e)
    (if (unique-call-site? e)
	(c:noop)
	(let ((x (environment-expression e)))
	 (set! *outside-body* '())
	 ;; note: This can't be beta converted since it modifies
	 ;;       *OUTSIDE-BODY*.
	 (let*
	   ((c (compile (create-return-result
			 e (expression-type-set (expression-body x)))
			(expression-body x)))
	    (c
	     (newline-between
	      (apply c:header (c:f e) (compile-parameter-variables e))
	      (braces-around
	       (newline-between
		(if (or (fictitious? (return-type-set e))
			(not (expression-accessed?
			      (expression-body (environment-expression e)))))
		    (c:noop)
		    (c:declaration (return-type-set e) (c:r e) (c:noop)))
		(if (environment-passes-parameters-globally? e)
		    (newline-between
		     (if (has-parent-parameter? e)
			 ;; needs work: needs abstraction for initialized
			 ;;             declaration
			 (semicolon-after
			  (c:type (environment-type e)
				  (unparenthesize (c:= (c:p e) (c:d e)))))
			 (c:noop))
		     (newlines-between
		      (map (lambda (g)
			    (space-between
			     ;; needs work: needs abstraction for initialized
			     ;;             declaration
			     (semicolon-after
			      (c:type-set
			       (variable-type-set g)
			       (unparenthesize (c:= (c:a g) (c:b g)))))
			     (c:/**/ (symbol->string (variable-name g)))))
			   (remove-if-not
			    (lambda (g) (or (local? g) (slotted? g)))
			    (variables e)))))
		    (c:noop))
		(newlines-between
		 (map (lambda (e1)
		       (if (and (not (noop? e1))
				(has-region? e1)
				(reentrant? e1))
			   (semicolon-after
			    (space-between (c:byte) (star-before (c:sfp e1))))
			   (c:noop)))
		      ;; This assumes that the IN-LINED-IN? relation is
		      ;; reflexive.
		      (in-lined-environments e)))
		(compile-in-lined-variables e)
		(newlines-between (reverse *outside-body*))
		(newlines-between
		 (map (lambda (e1)
		       (if (and (not (noop? e1)) (has-closure? e1))
			   (semicolon-after
			    ;; needs work: To use code-generation
			    ;;             abstractions.
			    (space-between
			     "struct"
			     (case *closure-representation*
			      ((immediate-flat)
			       (unimplemented x "Immediate flat closures are not (yet) implemented"))
			      ((indirect-flat)
			       (unimplemented x	"Indirect flat closures are not (yet) implemented"))
			      ((immediate-display) (c:e e1))
			      ((indirect-display) (c:e e1))
			      ((linked) (c:p e1))
			      (else (fuck-up)))
			     (star-before (c:e e1))))
			   (c:noop)))
		      ;; This assumes that the IN-LINED-IN? relation is
		      ;; reflexive.
		      (in-lined-environments e)))
		(newlines-between
		 (map (lambda (e1)
		       (if (and (not (noop? e1)) (has-parent-parameter? e1))
			   (semicolon-after
			    (c:type (environment-type e1) (c:p e1)))
			   (c:noop)))
		      (properly-in-lined-environments e)))
		(compile-initialize-region e)
		;; note: The self-tail-call entry point can't come after the
		;;       closure level allocation and spill since that would
		;;       unsoundly overwrite the existing closure.
		;;       cpstak.sc is an example of this.
		(if (has-self-tail-call? e) (c:: (c:h e)) (c:noop))
		(if (has-closure? e)
		    (newline-between (compile-allocate-closure-level e)
				     (spill-slotted-variables e))
		    (c:noop))
		c)))))
	  (newline-between
	   (if (or (substring? "/*" (environment-name e))
		   (substring? "*/" (environment-name e)))
	       (c:noop)
	       (c:/**/ (environment-name e)))
	   (if (or (fictitious? (return-type-set e))
		   (not (expression-accessed?
			 (expression-body (environment-expression e)))))
	       ;; needs work: To use code-generation abstractions.
	       (space-between "void" c)
	       (c:type-set (return-type-set e) c)))))))
   *es*)))

(define (compile-offsets)
 (let ((us (sort (remove-if (lambda (u) (zero? (type-use-count u)))
			    (append (list <null>
					  <true>
					  <false>
					  <char>
					  <fixnum>
					  <flonum>
					  <rectangular>
					  <input-port>
					  <output-port>
					  <eof-object>
					  <pointer>)
				    *internal-symbol-types*
				    *external-symbol-types*
				    *primitive-procedure-types*
				    *native-procedure-types*
				    *foreign-procedure-types*
				    *continuation-types*
				    *string-types*
				    *structure-types*
				    *headed-vector-types*
				    *nonheaded-vector-types*
				    *displaced-vector-types*))
		 >
		 type-use-count)))
  (newline-between
   (newlines-between
    (map-indexed (lambda (u i)
		  (c:define (c:type-tag u)
			    (c:fixnum (* (+ i (if *char-type-used?* 256 0))
					 (expt 2 *worst-alignment*)))))
		 us))
   (c:define (c:value-offset)
	     (c:fixnum (* (+ (length us) (if *char-type-used?* 256 0))
			  (expt 2 *worst-alignment*))))
   (if *char-type-used?*
       (c:define (c:char-offset) (c:fixnum (* 256 (expt 2 *worst-alignment*))))
       (c:noop)))))

(define (compile-constant-initialization-procedures)
 (newlines-between
  (map-n (lambda (i)
	  (newline-between
	   (space-between "void" (c:header (c:initialize-constants i)))
	   (braces-around
	    (newlines-between
	     (sublist
	      (reverse *inside-main*)
	      (* *statements-per-constant-initialization-procedure* i)
	      (min (* *statements-per-constant-initialization-procedure*
		      (+ i 1))
		   (length *inside-main*)))))))
	 (inexact->exact
	  (ceiling
	   (/ (length *inside-main*)
	      (exact->inexact
	       *statements-per-constant-initialization-procedure*)))))))

(define (compile-constant-initialization-procedure-calls)
 (newlines-between
  (map-n (lambda (i) (c:gosub (c:initialize-constants i)))
	 (inexact->exact
	  (ceiling
	   (/ (length *inside-main*)
	      (exact->inexact
	       *statements-per-constant-initialization-procedure*)))))))

(define (compile-assertions)
 ;; needs work: To use code-generation abstractions.
 (newline-between
  (if *char-alignment?*
      (c:assert (c:== (c:alignof (lambda (c) (space-between *char* c)))
		      (c:fixnum (expt 2 *char-alignment*))))
      (c:noop))
  (if *fixnum-alignment?*
      (c:assert (c:== (c:alignof (lambda (c) (space-between *fixnum* c)))
		      (c:fixnum (expt 2 *fixnum-alignment*))))
      (c:noop))
  (if *flonum-alignment?*
      (c:assert (c:== (c:alignof (lambda (c) (space-between *flonum* c)))
		      (c:fixnum (expt 2 *flonum-alignment*))))
      (c:noop))
  (if *rectangular-alignment?*
      (c:assert (c:== (c:alignof
		       (lambda (c) (space-between "struct" "rectangular" c)))
		      (c:fixnum (expt 2 *flonum-alignment*))))
      (c:noop))
  (if *void*-alignment?*
      (c:assert (c:== (c:alignof
		       (lambda (c) (space-between "void" (star-before c))))
		      (c:fixnum (expt 2 *pointer-alignment*))))
      (c:noop))
  (if *char*-alignment?*
      (c:assert (c:== (c:alignof
		       (lambda (c) (space-between *char* (star-before c))))
		      (c:fixnum (expt 2 *pointer-alignment*))))
      (c:noop))
  (if *file*-alignment?*
      (c:assert (c:== (c:alignof
		       (lambda (c) (space-between *file* (star-before c))))
		      (c:fixnum (expt 2 *pointer-alignment*))))
      (c:noop))
  (if *jmpbuf*-alignment?*
      (c:assert (c:== (c:alignof
		       (lambda (c) (space-between *jmpbuf* (star-before c))))
		      (c:fixnum (expt 2 *pointer-alignment*))))
      (c:noop))
  (if *length-alignment?*
      (c:assert (c:== (c:alignof (lambda (c) (space-between *length* c)))
		      (c:fixnum (expt 2 *length-alignment*))))
      (c:noop))
  (if *tag-alignment?*
      (c:assert (c:== (c:alignof (lambda (c) (space-between *tag* c)))
		      (c:fixnum (expt 2 *tag-alignment*))))
      (c:noop))
  (if *squished-alignment?*
      (c:assert (c:== (c:alignof (lambda (c) (space-between *squished* c)))
		      (c:fixnum (expt 2 *squished-alignment*))))
      (c:noop))
  (if *file-alignment?*
      (c:assert (c:== (c:alignof (lambda (c) (space-between *file* c)))
		      (c:fixnum (expt 2 *file-alignment*))))
      (c:noop))
  (if *jmpbuf-alignment?*
      (c:assert (c:== (c:alignof (lambda (c) (space-between *jmpbuf* c)))
		      (c:fixnum (expt 2 *jmpbuf-alignment*))))
      (c:noop))
  (newlines-between
   (map (lambda (u)
	 (c:assert (c:== (c:alignof (lambda (c) (c:type u c)))
			 (c:fixnum (expt 2 (type-alignment u))))))
	(append
	 (remove-if-not native-procedure-type-alignment?
			*native-procedure-types*)
	 (remove-if-not structure-type-alignment? *structure-types*)
	 (remove-if-not headed-vector-type-alignment? *headed-vector-types*)
	 (remove-if-not nonheaded-vector-type-alignment?
			*nonheaded-vector-types*)
	 (remove-if-not displaced-vector-type-alignment?
			*displaced-vector-types*))))
  (case *closure-representation*
   ((immediate-flat immediate-display) (c:noop))
   ((indirect-flat indirect-display)
    (newlines-between
     (map (lambda (u)
	   (c:assert
	    (c:== (c:alignof (lambda (c) (space-between "struct" (c:p u) c)))
		  (c:fixnum (expt 2 (type-alignment& u))))))
	  (remove-if-not native-procedure-type-alignment&?
			 *native-procedure-types*))))
   ((linked)
    (newlines-between
     (map (lambda (e)
	   (if (and (or (eq? *closure-conversion-method* 'baseline)
			(eq? *closure-conversion-method* 'conventional))
		    (not (environment? e)))
	       (c:noop)
	       (c:assert
		(c:== (c:alignof
		       (lambda (c) (space-between "struct" (c:p e) c)))
		      (c:fixnum (expt
				 2
				 (type-alignment&
				  (find-if (lambda (u)
					    (and (called? u)
						 (not (noop? u))
						 (has-parent-parameter? u)
						 (eq? e (parent-parameter u))))
					   *native-procedure-types*))))))))
	  (remove-duplicatesq
	   (map parent-parameter
		(remove-if-not
		 native-procedure-type-alignment&?
		 ;; This is just because of *CLOSURE-CONVERSION-METHOD*.
		 (remove-if (lambda (u) (or (not (called? u)) (noop? u)))
			    *native-procedure-types*)))))))
   (else (fuck-up)))
  (newlines-between
   (map (lambda (u)
	 (if (and (or (eq? *closure-conversion-method* 'baseline)
		      (eq? *closure-conversion-method* 'conventional))
		  (or (not (environment?
			    (native-procedure-type-narrow-prototype u)))
		      (not (environment-used? (narrow-prototype u)))
		      (not (environment? (parent-parameter u)))
		      (not (environment-used? (parent-parameter u)))))
	     (c:noop)
	     (c:assert (c:== (c:alignof
			      (lambda (c)
			       (space-between
				"struct"
				(case *closure-representation*
				 ((immediate-flat immediate-display) (fuck-up))
				 ((indirect-flat indirect-display) (c:p u))
				 ((linked) (c:p (parent-parameter u)))
				 (else (fuck-up)))
				c)))
			     (c:fixnum (expt 2 (type-alignment& u)))))))
	(remove-if-not native-procedure-type-alignment&?
		       *native-procedure-types*)))
  (newlines-between
   (map (lambda (u)
	 (c:assert (c:== (c:alignof (lambda (c) (c:type& u c)))
			 (c:fixnum (expt 2 (type-alignment& u))))))
	(remove-if-not structure-type-alignment&? *structure-types*)))
  (newlines-between
   (map (lambda (u)
	 (c:assert (c:== (c:alignof
			  (lambda (c) (space-between "struct" (c:u u) c)))
			 (c:fixnum (expt 2 (type-alignment& u))))))
	(remove-if-not headed-vector-type-alignment&?
		       *headed-vector-types*)))
  (newlines-between (map (lambda (w)
			  (c:assert
			   (c:== (c:alignof (lambda (c) (c:type-set w c)))
				 (c:fixnum (expt 2 (type-set-alignment w))))))
			 (remove-if-not type-set-alignment? *ws*)))
  (if *char-size?*
      (c:assert (c:== (c:sizeof *char*) (c:fixnum *char-size*)))
      (c:noop))
  (if *fixnum-size?*
      (c:assert (c:== (c:sizeof *fixnum*) (c:fixnum *fixnum-size*)))
      (c:noop))
  (if *flonum-size?*
      (c:assert (c:== (c:sizeof *flonum*) (c:fixnum *flonum-size*)))
      (c:noop))
  (if *rectangular-size?*
      (c:assert (c:== (c:sizeof (list "struct" "rectangular"))
		      (c:fixnum (* 2 *flonum-size*))))
      (c:noop))
  (if *void*-size?*
      (c:assert
       (c:== (c:sizeof (space-between "void" "*")) (c:fixnum *pointer-size*)))
      (c:noop))
  (if *char*-size?*
      (c:assert
       (c:== (c:sizeof (space-between *char* "*")) (c:fixnum *pointer-size*)))
      (c:noop))
  (if *file*-size?*
      (c:assert
       (c:== (c:sizeof (space-between *file* "*")) (c:fixnum *pointer-size*)))
      (c:noop))
  (if *jmpbuf*-size?*
      (c:assert (c:== (c:sizeof (space-between *jmpbuf* "*"))
		      (c:fixnum *pointer-size*)))
      (c:noop))
  (if *length-size?*
      (c:assert (c:== (c:sizeof *length*) (c:fixnum *length-size*)))
      (c:noop))
  (if *tag-size?*
      (c:assert (c:== (c:sizeof *tag*) (c:fixnum *tag-size*)))
      (c:noop))
  (if (and *squish?* *squished-size?*)
      (c:assert (c:== (c:sizeof *squished*) (c:fixnum *squished-size*)))
      (c:noop))
  (if (and *squish?* *squished-size?*)
      (c:assert (c:== (c:sizeof *signed-squished*) (c:fixnum *squished-size*)))
      (c:noop))
  (newlines-between
   (map (lambda (u)
	 (c:assert (c:== (c:sizeof (c:type u "")) (c:fixnum (type-size u)))))
	(append
	 (remove-if-not native-procedure-type-size? *native-procedure-types*)
	 (remove-if-not structure-type-size? *structure-types*)
	 (remove-if-not headed-vector-type-size? *headed-vector-types*)
	 (remove-if-not nonheaded-vector-type-size? *nonheaded-vector-types*)
	 (remove-if-not displaced-vector-type-size?
			*displaced-vector-types*))))
  (newlines-between
   (map (lambda (w)
	 (c:assert
	  (c:== (c:sizeof (c:type-set w "")) (c:fixnum (type-set-size w)))))
	(remove-if-not type-set-size? *ws*)))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
