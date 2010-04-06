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
(module stalin4b)

(include "QobiScheme.sch")
(include "stalin4b.sch")
;;; End delete for Trotsky

(define *errors*
 '(("out_of_memory" "Out of memory" #f)
   ("call"
    "Attempt to call a non-procedure or call a procedure with the wrong number of arguments"
    "Might call a non-procedure or call a procedure with the wrong number of arguments"
    "Will call a non-procedure or call a procedure with the wrong number of arguments")
   ("foreign_call"
    "Attempt to call a foreign procedure with arguments of the wrong type"
    "Might call a foreign procedure with arguments of the wrong type"
    "Will call a foreign procedure with arguments of the wrong type")
   ("void_if"
    "The antecedent to an IF has an unspecified value"
    "The antecedent to an IF might have an unspecified value"
    "The antecedent to an IF will have an unspecified value")
   ("void_call"
    "Attempt to call an unspecified value"
    "Might call an unspecified value"
    "Will call an unspecified value")
   ("void_primitive_procedure_call"
    "Attempt to call a primitive procedure with an unspecified value"
    "Might call a primitive procedure with an unspecified value"
    "Will call a primitive procedure with an unspecified value")
   ("void_foreign_procedure_call"
    "Attempt to call a foreign procedure with an unspecified value"
    "Might call a foreign procedure with an unspecified value"
    "Will call a foreign procedure with an unspecified value")
   ("structure_ref"
    "Argument to STRUCTURE-REF ~a a structure of the correct type"
    #t)
   ("structure_set"
    "First argument to STRUCTURE-SET! ~a a structure of the correct type"
    #t)
   ("string_to_uninterned_symbol"
    "Argument to STRING->UNINTERNED-SYMBOL ~a a string"
    #t)
   ("exact" "Argument to EXACT? ~a a number" #t)
   ("inexact" "Argument to INEXACT? ~a a number" #t)
   ("symbol_string" "Argument to SYMBOL->STRING ~a a symbol" #t)
   ("eql" "Argument to = ~a a number" #t)
   ("lt" "Argument to < ~a a real number" #t)
   ("gt" "Argument to > ~a a real number" #t)
   ("le" "Argument to <= ~a a real number" #t)
   ("ge" "Argument to >= ~a a real number" #t)
   ("zero" "Argument to ZERO? ~a a number or a pointer" #t)
   ("positive" "Argument to POSITIVE? ~a a real number" #t)
   ("negative" "Argument to NEGATIVE? ~a a real number" #t)
   ("max" "Argument to MAX ~a a real number" #t)
   ("min" "Argument to MIN ~a a real number" #t)
   ("plus" "Argument to + ~a a number" #t)
   ("minus" "Argument to - ~a a number" #t)
   ("times" "Argument to * ~a a number" #t)
   ("divide" "Argument to / ~a a number" #t)
   ("quotient1" "First argument to QUOTIENT ~a an integer" #t)
   ("quotient2" "Second argument to QUOTIENT ~a an integer" #t)
   ("remainder1" "First argument to REMAINDER ~a an integer" #t)
   ("remainder2" "Second argument to REMAINDER ~a an integer" #t)
   ("lsh1" "First argument to << ~a an integer" #t)
   ("lsh2" "Second argument to >> ~a an integer" #t)
   ("rsh1" "First argument to << ~a an integer" #t)
   ("rsh2" "Second argument to >> ~a an integer" #t)
   ("bitwise_not" "Argument to BITWISE-NOT ~a an exact integer" #t)
   ("bitwise_and" "Argument to BITWISE-AND ~a an exact integer" #t)
   ("bitwise_or" "Argument to BITWISE-OR ~a an exact integer" #t)
   ("floor" "Argument to FLOOR ~a a real number" #t)
   ("ceiling" "Argument to CEILING ~a a real number" #t)
   ("truncate" "Argument to TRUNCATE ~a a real number" #t)
   ("round" "Argument to ROUND ~a a real number" #t)
   ("exp" "Argument to EXP ~a a number" #t)
   ("log" "Argument to LOG ~a a number" #t)
   ("sin" "Argument to SIN ~a a number" #t)
   ("cos" "Argument to COS ~a a number" #t)
   ("tan" "Argument to TAN ~a a number" #t)
   ("asin" "Argument to ASIN ~a a number" #t)
   ("acos" "Argument to ACOS ~a a number" #t)
   ("atan1" "Argument to ATAN ~a a number" #t)
   ("atan2" "First argument to ATAN ~a a number" #t)
   ("atan3" "Second argument to ATAN ~a a number" #t)
   ("sqrt" "Argument to SQRT ~a a number" #t)
   ("expt1" "First argument to EXPT ~a a number" #t)
   ("expt2" "Second argument to EXPT ~a a number" #t)
   ("exact_to_inexact" "Argument to EXACT->INEXACT ~a a number" #t)
   ("inexact_to_exact1" "Argument to INEXACT->EXACT ~a a number" #t)
   ("inexact_to_exact2"
    "Implementation restriction: Argument to INEXACT->EXACT ~a a real number"
    #t)
   ("char_to_integer" "Argument to CHAR->INTEGER ~a a character" #t)
   ("integer_to_char1" "Argument to INTEGER->CHAR ~a an exact integer" #t)
   ("integer_to_char2" "Argument to INTEGER->CHAR is out of bounds" #f)
   ("make_string1" "First argument to MAKE-STRING ~a an exact integer" #t)
   ("make_string2" "Second argument to MAKE-STRING ~a a character" #t)
   ("string" "Argument to STRING ~a a character" #t)
   ("string_length" "Argument to STRING-LENGTH ~a a string" #t)
   ("string_ref1" "First argument to STRING-REF ~a a string" #t)
   ("string_ref2" "Second argument to STRING-REF ~a an exact integer" #t)
   ("string_ref3" "Second argument to STRING-REF is out of bounds" #f)
   ("string_set1" "First argument to STRING-SET! ~a a string" #t)
   ("string_set2" "Second argument to STRING-SET! ~a an exact integer" #t)
   ("string_set3" "Third argument to STRING-SET! ~a a character" #t)
   ("string_set4" "Second argument to STRING-SET! is out of bounds" #f)
   ("make_vector" "First argument to MAKE-VECTOR ~a an exact integer" #t)
   ("make_displaced_vector1"
    "First argument to MAKE-DISPLACED-VECTOR ~a a vector"
    #t)
   ("make_displaced_vector2"
    "Second argument to MAKE-DISPLACED-VECTOR ~a an exact integer"
    #t)
   ("make_displaced_vector3"
    "Third argument to MAKE-DISPLACED-VECTOR ~a an exact integer"
    #t)
   ("make_displaced_vector4"
    "Second argument to MAKE-DISPLACED-VECTOR is out of bounds"
    #f)
   ("make_displaced_vector5"
    "Third argument to MAKE-DISPLACED-VECTOR is out of bounds"
    #f)
   ("vector_length" "Argument to VECTOR-LENGTH ~a a vector" #t)
   ("vector_ref1" "First argument to VECTOR-REF ~a a vector" #t)
   ("vector_ref2" "Second argument to VECTOR-REF ~a an exact integer" #t)
   ("vector_ref3" "Second argument to VECTOR-REF is out of bounds" #f)
   ("vector_set1" "First argument to VECTOR-SET! ~a a vector" #t)
   ("vector_set2" "Second argument to VECTOR-SET! ~a an exact integer" #t)
   ("vector_set3" "Second argument to VECTOR-SET! is out of bounds" #f)
   ("call_with_current_continuation"
    "First argument to CALL-WITH-CURRENT-CONTINUATION ~a a procedure of one argument"
    #t)
   ("open_input_file1" "Argument to OPEN-INPUT-FILE ~a a string" #t)
   ("open_input_file2" "OPEN-INPUT-FILE cannot open file" #f)
   ("open_output_file1" "Argument to OPEN-OUTPUT-FILE ~a a string" #t)
   ("open_output_file2" "OPEN-OUTPUT-FILE cannot open file" #f)
   ("close_input_port1" "Argument to CLOSE-INPUT-PORT ~a an input port" #t)
   ("close_input_port2" "CLOSE-INPUT-PORT cannot close input port" #f)
   ("close_output_port1" "Argument to CLOSE-OUTPUT-PORT ~a an output port" #t)
   ("close_output_port2" "CLOSE-OUTPUT-PORT cannot close output port" #f)
   ("read_char1" "Argument to READ-CHAR1 ~a an input port" #t)
   ("peek_char1" "Argument to PEEK-CHAR1 ~a an input port" #t)
   ("char_ready1" "Argument to CHAR-READY?1 ~a an input port" #t)
   ("write_char1" "First argument to WRITE-CHAR2 ~a a character" #t)
   ("write_char2" "Second argument to WRITE-CHAR2 ~a an output port" #t)
   ("panic" "Argument to PANIC ~a a string" #t)
   ("integer_to_string" "Argument to INTEGER->STRING ~a an exact integer" #t)
   ("integer_to_input_port"
    "Argument to INTEGER->INPUT-PORT ~a an exact integer"
    #t)
   ("integer_to_output_port"
    "Argument to INTEGER->OUTPUT-PORT ~a an exact integer"
    #t)
   ("integer_to_pointer"
    "Argument to INTEGER->POINTER ~a an exact integer"
    #t)
   ("infinity" "Argument to INFINITY? ~a a flonum" #t)))

(define *errors-used* #f)

(define *warnings* #f)

(define (compile-error-procedure-prototypes)
 (newlines-between
  (map (lambda (error)
	;; conventions: ERROR
	(set! *c:panic?* #t)
	;; needs work: To use code-generation abstractions.
	(space-between "void" (c:noreturn-prototype (c:error (first error)))))
       *errors-used*)))

(define (compile-error-procedures)
 (newlines-between
  (map (lambda (error)
	;; conventions: ERROR
	(set! *c:panic?* #t)
	;; needs work: To use code-generation abstractions.
	(space-between
	 "void"
	 (c:header (c:error (first error)))
	 (braces-around
	  (c:panic (c:string (if (eq? (third error) #t)
				 (format #f (second error) "is not")
				 (second error)))))))
       *errors-used*)))

(define (restore? e/r)
 (cond ((environment? e/r) (and (has-region? e/r) (reentrant? e/r)))
       ((result? e/r) (restore? (result-environment e/r)))
       (else (fuck-up))))

(define (compile-restore e/r)
 ;; needs work: Returning from a procedure should restore all in-lined
 ;;             reentrant regions that have self tail calls to that procedure.
 ;;             Because we don't do this (yet), we need HAS-EXTERNAL-CALL? to
 ;;             prevent allocation on such regions because otherwise there
 ;;             would be a memory leak when we exit from such a procedure.
 (cond
  ((environment? e/r)
   (if (and (has-region? e/r) (reentrant? e/r))
       (newline-between
	(cond (*treadmarks?*
	       (include! "Tmk")		;Tmk_lock_acquire
	       (c:gosub "Tmk_lock_acquire" (c:0)))
	      (else (c:noop)))
	(if *expandable-regions?*
	    (c:while
	     (c:boolean-or
	      (c:< (c:sfp e/r)
		   (c:& (c:subscript (c:-> (c:region e/r) (c:data)) (c:0))))
	      (c:> (c:sfp e/r)
		   (c:& (c:subscript (c:-> (c:region e/r) (c:data))
				     (c:region-size e/r)))))
	     (newline-between
	      (semicolon-after
	       ;; needs work: To use code-generation abstractions.
	       (space-between
		"struct" (c:region e/r) (star-before (c:region))))
	      (c::= (c:region) (c:region e/r))
	      (c::= (c:region-size e/r) (c:-> (c:region e/r) (c:region-size)))
	      (c::= (c:region e/r) (c:-> (c:region e/r) (c:region)))
	      (if *memory-messages?*
		  (c:printf
		   (c:string (format #f "Freeing region segment for ~a~%"
				     (environment-name e/r))))
		  (c:noop))
	      (c:free (c:region) (has-nonatomic-region? e/r))))
	    (c:noop))
	(c::= (c:fp e/r) (c:sfp e/r))
	(cond (*treadmarks?*
	       (include! "Tmk")		;Tmk_lock_release
	       (c:gosub "Tmk_lock_release" (c:0)))
	      (else (c:noop))))
       (c:noop)))
  ((result? e/r) (compile-restore (result-environment e/r)))
  (else (fuck-up))))

(define (result-accessed? r)
 (unless (return? r) (fuck-up))
 (expression-accessed?
  (expression-body (environment-expression (result-environment r)))))

(define (compile-return r)
 (if (and
      (return? r)
      (or (environment-returns? (result-environment r))
	  (environment-passes-parameters-globally? (result-environment r))))
     (newline-between
      (compile-restore r)
      (if (or (fictitious? (result-type-set r)) (not (result-accessed? r)))
	  (c:return)
	  (c:return (c:r (result-environment r)))))
     (c:noop)))

(define (zero-value c u w)
 (if *eq?-forgery?*
     ;; This zeros out the value field so that EQ? forgery works.
     ;; needs work: This can be eliminated if there is never any EQ? forgery.
     (let* ((size (reduce
		   max
		   (map type-size
			(members-that
			 (lambda (u)
			  (or (not (char-type? u)) (not (fictitious? u))))
			 w))
		   ;; This can't happen if the type set isn't fictitious,
		   ;; monomorphic, or tag only.
		   #f))
	    (fixnum-size (quotient size *fixnum-size*))
	    (char-size (remainder size *fixnum-size*)))
      ;; conventions: SIZE FIXNUM-SIZE CHAR-SIZE
      (if (or (eq? u #f) (not (= size (type-size u))))
	  (newline-between
	   (newlines-between
	    (map-n (lambda (i)
		    (c::= (c:raw-subscript
			   (c:fixnum*-cast (c:& (c:. c "value")))
			   (c:fixnum i))
			  (c:0)))
		   fixnum-size))
	   (newlines-between
	    (map-n (lambda (i)
		    (c::= (c:raw-subscript
			   (c:char*-cast (c:& (c:. c "value")))
			   (c:fixnum (+ (* fixnum-size *fixnum-size*) i)))
			  (c:0)))
		   char-size)))
	  (c:noop)))
     (c:noop)))

(define (move-general r c w w2 p?)
 ;; W is the source representation type set
 ;; W2 is the source restricted type set
 (let ((us (intersectionq (members w) (members w2))))
  (cond
   ((discard? r) (if p? (c:noop) (semicolon-after c)))
   ((and (return? r) (not (result-accessed? r)))
    (newline-between (if p? (c:noop) (semicolon-after c)) (compile-return r)))
   ((antecedent? r)
    ;; needs work: I'm not sure whether uniqueness screws up here.
    (if (every false-type? us)
	(if (some false-type? us)
	    (newline-between (if p? (c:noop) (semicolon-after c))
			     (compile-goto (result-l2 r) (result-l0 r)))
	    (if p? (c:noop) (semicolon-after c)))
	(if (some false-type? us)
	    (let ((u (first (remove-if-not false-type? us))))
	     (c:if (cond ((fake? w) (fuck-up))
			 ((monomorphic? w) (fuck-up))
			 ((tag-only? w) (c:== (c:tag c w) (c:type-tag u)))
			 ((squeezed? w) (squeeze-tag-test c u w))
			 ((squished? w) (squish-tag-test c u w))
			 (else (c:== (c:tag c w) (c:type-tag u))))
		   (compile-goto (result-l2 r) (result-l0 r))
		   (compile-goto (result-l1 r) (result-l0 r))
		   #t))
	    (newline-between (if p? (c:noop) (semicolon-after c))
			     (compile-goto (result-l1 r) (result-l0 r))))))
   (else
    (let* ((c1 (result-c r))
	   (w1 (result-type-set r))
	   ;; note: This was added because of uniqueness. With uniqueness, it
	   ;;       is possible for the source to be widened and contain
	   ;;       members that the more precise analysis determines can't
	   ;;       really occur so that they may be absent from the
	   ;;       destination.
	   (us (if *uniqueness?* (intersectionq (members w1) us) us)))
     (define (move c)
      (cond
       ((return? r)
	(if (or
	     (environment-returns? (result-environment r))
	     (environment-passes-parameters-globally? (result-environment r)))
	    ;; needs work: This will replicate the COMPILE-RESTORE in both
	    ;;             branches of an IF or in all branches of a dispatch.
	    (newline-between
	     ;; needs work: This potentially can do the COMPILE-RESTORE before
	     ;;             evaluating C.
	     (compile-restore r)
	     (c:return c))
	    (c:noop)))
       (else (c::= c1 c))))
     (unless (every (lambda (u) (member? u w1)) us) (fuck-up))
     (cond
      ((or (fake? w1) (null? us))
       (newline-between (if p? (c:noop) (semicolon-after c))
			(compile-return r)))
      ((monomorphic? w1) (move (c:value c (the-member w1) w)))
      ((tag-only? w1)
       (cond
	((fake? w) (move (c:type-tag (the-member w))))
	((monomorphic? w)
	 (unless (char-type? (the-member w)) (fuck-up))
	 ;; note: Converting from character to tag-only used to be free but now
	 ;;       requires a left shift when there is some squishing. This is
	 ;;       the price to pay for universal type tags.
	 ;; This assumes that *TAG* is unsigned so that << does a logical
	 ;; shift. The call to C:UNSIGNED-CHAR-CAST is in case *CHAR* is
	 ;; signed to force << to be a logical shift without a prior sign
	 ;; extend. The call to C:TYPE-SET-CAST is to prevent any overflow in
	 ;; the logical shift.
	 (move (c:<< (c:type-set-cast (c:unsigned-char-cast c) w1)
		     (c:fixnum *worst-alignment*))))
	((tag-only? w) (move c))
	((squeezed? w) (move (c:squeezed->tag-cast c w)))
	;; This works because of universal type tags. This assumes that
	;; casting from *SQUISHED* to *TAG* does not modify the bit pattern.
	((squished? w) (move (c:type-set-cast c w1)))
	(else (move (c:tag c w)))))
      ((squeezed? w1)
       (cond
	((fake? w) (move (c:type-set-cast (c:type-tag (the-member w)) w1)))
	((monomorphic? w)
	 (move (squeeze (c:value c (the-member w) w) (the-member w) w1)))
	((tag-only? w) (move (c:tag->squeezed-cast c w w1)))
	((squeezed? w)
	 (if (eq? (squeezed-member w1) (squeezed-member w))
	     (move c)
	     ;; The squeezed members can only differ in the case of narrowing
	     ;; when the only values being moved are the non-squeezed types.
	     (move (c:type-set-cast c w1))))
	((squished? w)
	 (move (c:type-set-cast
		(if (and (member? (squeezed-member w1) w)
			 (not (zero? (squish-tag (squeezed-member w1) w))))
		    (if (every
			 (lambda (u) (or (char-type? u) (fictitious? u))) us)
			(strip-known-squish-tag c (squeezed-member w1) w)
			(strip-squish-tag c w))
		    c)
		w1)))
	(else
	 (let ((u1 (squeezed-member w1)))
	  (if (some (lambda (u) (eq? u u1)) us)
	      (if (every (lambda (u) (eq? u u1)) us)
		  (move (squeeze (c:value c u1 w) u1 w1))
		  (newline-between
		   (c:/**/ "MOVE: branching general to squeezed")
		   (c:if (c:== (c:tag c w) (c:type-tag u1))
			 (move (squeeze (c:value c u1 w) u1 w1))
			 (move (c:tag->squeezed-cast c w w1))
			 #t)))
	      (move (c:tag->squeezed-cast c w w1)))))))
      ((squished? w1)
       (cond
	((fake? w) (move (c:type-set-cast (c:type-tag (the-member w)) w1)))
	((monomorphic? w)
	 (move (squish (c:value c (the-member w) w) (the-member w) w1)))
	;; This works because of universal type tags. This assumes that
	;; casting from *TAG* to *SQUISHED* does not modify the bit pattern.
	((tag-only? w) (move (c:type-set-cast (c:tag c w) w1)))
	((squeezed? w)
	 (if (or (every (lambda (u) (or (char-type? u) (fictitious? u))) us)
		 (zero? (squish-tag (squeezed-member w) w1)))
	     (move (c:type-set-cast c w1))
	     (if (some (lambda (u) (or (char-type? u) (fictitious? u))) us)
		 (newline-between
		  (c:/**/ "MOVE: branching squeezed to squished")
		  (c:if (c:>= c (c:type-set-cast (c:value-offset) w))
			(move (squish (c:value c (squeezed-member w) w)
				      (squeezed-member w)
				      w1))
			(move (c:type-set-cast c w1))
			#t))
		 (move (squish (c:value c (squeezed-member w) w)
			       (squeezed-member w)
			       w1)))))
	((squished? w)
	 (cond
	  ((and (every (lambda (u)
			(or (char-type? u)
			    (fictitious? u)
			    (= (squish-tag u w) (squish-tag u w1))))
		       us)
		(= (squish-alignment w) (squish-alignment w1)))
	   (move c))
	  (else
	   (unless (or (= (squish-alignment w) (squish-alignment w1))
		       (every (lambda (u)
			       (or (char-type? u)
				   (fictitious? u)
				   (fixnum-type? u)
				   ;; needs work: Can be extended to allow
				   ;;             squishing a singleton
				   ;;             immediate structure if its
				   ;;             slot is squished.
				   (degenerate-vector-type? u)))
			      us))
	    (unimplemented
	     #f "This case of squished-to-squished is not (yet) implemented"))
	   (if (or (not (some
			 (lambda (u) (or (char-type? u) (fictitious? u))) us))
		   (and (= (squish-alignment w) (squish-alignment w1))
			(every (lambda (u)
				(or (not (zero? (squish-tag u w)))
				    (zero? (squish-tag u w1))))
			       us)))
	       (newline-between
		(c:/**/ "MOVE: squished to squished")
		(move
		 (c:+
		  ;; needs work: This is wrong. Only the non-pointer values
		  ;;             need to be shifted. But fortunately this
		  ;;             doesn't cause a problem since if both W and W1
		  ;;             can contain the same pointer values then they
		  ;;             will have the same squish alignment because
		  ;;             all pointer values have the same alignment.
		  ;;             This is checked by the above panic.
		  (c:squished-cast
		   (cond
		    ((= (squish-alignment w) (squish-alignment w1))
		     (strip-squish-tag c w))
		    ((< (squish-alignment w) (squish-alignment w1))
		     (c:<< (c:signed-squished-cast (strip-squish-tag c w))
			   (c:fixnum
			    (- (squish-alignment w1) (squish-alignment w)))))
		    (else
		     (c:>> (c:signed-squished-cast (strip-squish-tag c w))
			   (c:fixnum
			    (- (squish-alignment w) (squish-alignment w1)))))))
		  (c:subscript
		   ;; needs work: To use code-generation abstractions.
		   (list
		    "\""
		    (reduce
		     string-append
		     (let ((alist
			    (map (lambda (u)
				  (cons (squish-tag u w) (squish-tag u w1)))
				 us)))
		      (map-n (lambda (i)
			      (let ((c (number->string
					(cdr (or (assv i alist) (cons 0 0))))))
			       (when (> (string-length c) 2) (fuck-up))
			       (string-append
				"\\"
				(make-string (- 3 (string-length c)) #\0)
				c)))
			     (+ (reduce max (map car alist) 0) 1)))
		     "")
		    "\"")
		   (extract-squish-tag c w)))))
	       (newline-between
		(c:/**/ "MOVE: branching squished to squished")
		(c:if
		 (c:boolean-or (c:!=0 (extract-squish-tag c w))
			       (c:>= c (c:type-set-cast (c:value-offset) w)))
		 (move
		  (c:+
		   ;; needs work: This is wrong. Only the non-pointer values
		   ;;             need to be shifted. But fortunately this
		   ;;             doesn't cause a problem since if both W and
		   ;;             W1 can contain the same pointer values then
		   ;;             they will have the same squish alignment
		   ;;             because all pointer values have the same
		   ;;             alignment. This is checked by the above
		   ;;             panic.
		   (c:squished-cast
		    (cond
		     ((= (squish-alignment w) (squish-alignment w1))
		      (strip-squish-tag c w))
		     ((< (squish-alignment w) (squish-alignment w1))
		      (c:<< (c:signed-squished-cast (strip-squish-tag c w))
			    (c:fixnum (- (squish-alignment w1)
					 (squish-alignment w)))))
		     (else
		      (c:>> (c:signed-squished-cast (strip-squish-tag c w))
			    (c:fixnum (- (squish-alignment w)
					 (squish-alignment w1)))))))
		   (c:subscript
		    ;; needs work: To use code-generation abstractions.
		    (list
		     "\""
		     (reduce
		      string-append
		      (let ((alist (map (lambda (u)
					 (cons (squish-tag u w)
					       (squish-tag u w1)))
					(remove-if
					 (lambda (u)
					  (or (char-type? u) (fictitious? u)))
					 us))))
		       (map-n
			(lambda (i)
			 (let ((c (number->string
				   (cdr (or (assv i alist) (cons 0 0))))))
			  (when (> (string-length c) 2) (fuck-up))
			  (string-append
			   "\\" (make-string (- 3 (string-length c)) #\0) c)))
			(+ (reduce max (map car alist) 0) 1)))
		      "")
		     "\"")
		    (extract-squish-tag c w))))
		 (move c)
		 #t))))))
	(else (newline-between
	       (if (or (every (lambda (u) (or (char-type? u) (fictitious? u)))
			      us)
		       (null? (rest us)))
		   (c:noop)
		   (c:/**/ "MOVE: dispatching general to squished"))
	       (nonchecking-type-switch
		(lambda (u) (some (lambda (u1) (eq? u u1)) us))
		w
		r
		c
		(lambda (u)
		 (move (if (or (char-type? u) (fictitious? u))
			   ;; This works because of universal type tags. This
			   ;; assumes that casting from *TAG* to *SQUISHED*
			   ;; does not modify the bit pattern.
			   (c:type-set-cast (c:tag c w) w1)
			   (squish (c:value c u w) u w1)))))))))
      (else
       (cond
	((fake? w)
	 (newline-between (zero-value c1 #f w1)
			  (c::= (c:tag c1 w1) (c:type-tag (the-member w)))
			  (compile-return r)))
	((monomorphic? w)
	 (newline-between
	  (if (char-type? (the-member w))
	      ;; note: Converting from character to general used to be free
	      ;;       but now requires a left shift when there is some
	      ;;       squishing. This is the price to pay for universal type
	      ;;       tags.
	      ;; This assumes that *TAG* is unsigned so that << does a logical
	      ;; shift. The call to C:UNSIGNED-CHAR-CAST is in case *CHAR* is
	      ;; signed to force << to be a logical shift without a prior sign
	      ;; extend. The call to C:TAG-CAST is to prevent any overflow in
	      ;; the logical shift.
	      (newline-between
	       (zero-value c1 #f w1)
	       (c::= (c:tag c1 w1)
		     (c:<< (c:tag-cast (c:unsigned-char-cast
					(c:value c (the-member w) w)))
			   (c:fixnum *worst-alignment*))))
	      (newline-between
	       (zero-value c1 (the-member w) w1)
	       (c::= (c:tag c1 w1) (c:type-tag (the-member w)))
	       (c::= (c:value c1 (the-member w) w1)
		     (c:value c (the-member w) w))))
	  (compile-return r)))
	((tag-only? w)
	 (newline-between (zero-value c1 #f w1)
			  (c::= (c:tag c1 w1) (c:tag c w))
			  (compile-return r)))
	((squeezed? w)
	 (newline-between
	  (if (every (lambda (u) (or (char-type? u) (fictitious? u))) us)
	      (newline-between
	       (zero-value c1 #f w1)
	       (c::= (c:tag c1 w1) (c:squeezed->tag-cast c w)))
	      (if (some (lambda (u) (or (char-type? u) (fictitious? u))) us)
		  (newline-between
		   (c:/**/ "MOVE: branching squeezed to general")
		   (c:if (c:>= c (c:type-set-cast (c:value-offset) w))
			 (newline-between
			  (zero-value c1 (squeezed-member w) w1)
			  (c::= (c:tag c1 w1) (c:type-tag (squeezed-member w)))
			  (c::= (c:value c1 (squeezed-member w) w1) c))
			 (newline-between
			  (zero-value c1 #f w1)
			  (c::= (c:tag c1 w1) (c:squeezed->tag-cast c w)))
			 #t))
		  (newline-between
		   (zero-value c1 (squeezed-member w) w1)
		   (c::= (c:tag c1 w1) (c:type-tag (squeezed-member w)))
		   (c::= (c:value c1 (squeezed-member w) w1) c))))
	  (compile-return r)))
	((squished? w)
	 (newline-between
	  (if (or (every (lambda (u) (or (char-type? u) (fictitious? u))) us)
		  (null? (rest us)))
	      (c:noop)
	      (c:/**/ "MOVE: dispatching squished to general"))
	  (nonchecking-type-switch
	   (lambda (u) (some (lambda (u1) (eq? u u1)) us))
	   w
	   r
	   c
	   (lambda (u)
	    (if (or (char-type? u) (fictitious? u))
		;; This works because of universal type tags. This assumes
		;; that casting from *SQUISHED* to *TAG* does not modify the
		;; bit pattern.
		(newline-between (zero-value c1 #f w1)
				 (c::= (c:tag c1 w1) (c:tag-cast c))
				 (compile-return r))
		(newline-between (zero-value c1 u w1)
				 (c::= (c:tag c1 w1) (c:type-tag u))
				 (c::= (c:value c1 u w1) (c:value c u w))
				 (compile-return r)))))))
	(else (cond ((eq? w w1) (move c))
		    ((and *forgery?* (= (type-set-size w) (type-set-size w1)))
		     (move (c:forgery-cast c w1)))
		    (else (newline-between
			   (if (or (every (lambda (u)
					   (or (char-type? u) (fictitious? u)))
					  us)
				   (null? (rest us)))
			       (c:noop)
			       (c:/**/ "MOVE: dispatching general to general"))
			   ;; This works because of universal type tags.
			   (c::= (c:tag c1 w1) (c:tag c w))
			   (nonchecking-type-switch
			    (lambda (u) (some (lambda (u1) (eq? u u1)) us))
			    w
			    r
			    c
			    (lambda (u)
			     (if (or (char-type? u) (fictitious? u))
				 (newline-between (zero-value c1 #f w1)
						  (compile-return r))
				 (newline-between
				  (zero-value c1 u w1)
				  (c::= (c:value c1 u w1) (c:value c u w))
				  (compile-return r)))))))))))))))))

(define (move r c w) (move-general r c w w #t))

(define (move-strict r c w) (move-general r (c:protect c) w w #f))

(define (move-access r c w w2) (move-general r c w w2 #t))

(define (widen-type r c u)
 (let ((w (create-anonymous-type-set u)))
  (set-type-set-fictitious?!
   w
   (case *closure-conversion-method*
    ((baseline conventional) #f)
    ((lightweight)
     (or (void? w) (and (not (multimorphic? w)) (must-be? fictitious? w))))
    (else (fuck-up))))
  (move-general r c w w #t)))

(define (widen r c m)
 (cond ((discard? r) (c:noop))
       ((antecedent? r)
	;; note: The following checks for EQ?-ness between procedures.
	(if (eq? m false-type?)
	    (compile-goto (result-l2 r) (result-l0 r))
	    (compile-goto (result-l1 r) (result-l0 r))))
       (else (widen-type r c (the-member-that m (result-type-set r))))))

(define (move-displaced-vector r u c1 c2)
 (cond
  ((discard? r) (c:noop))
  ((and (return? r) (not (result-accessed? r)))
   ;; note: C2 is not evaluated here. This is OK because in all uses of
   ;;       MOVE-DISPLACED-VECTOR C2 does not need to be strict.
   (compile-return r))
  ((antecedent? r) (compile-goto (result-l1 r) (result-l0 r)))
  (else
   (let ((c (result-c r))
	 (w (result-type-set r)))
    (when (or (squeezed? w) (squished? w))
     (unimplemented
      #f "Squeezing or squishing a displaced vector is not (yet) implemented"))
    (unless (member? u w) (fuck-up))
    (cond
     ((and (return? r)
	   (monomorphic? w)
	   (degenerate-vector-type? (the-member w)))
      (if (or (environment-returns? (result-environment r))
	      (environment-passes-parameters-globally? (result-environment r)))
	  (newline-between
	   ;; note: This potentially can do the COMPILE-RESTORE before
	   ;;       evaluating C2. This is OK because in all uses of
	   ;;       MOVE-DISPLACED-VECTOR C2 does not need to be strict.
	   (compile-restore r)
	   (c:return c2))
	  (c:noop)))
     ((and (return? r) (tag-only? w))
      (if (or (environment-returns? (result-environment r))
	      (environment-passes-parameters-globally? (result-environment r)))
	  (newline-between (compile-restore r) (c:return (c:type-tag u)))
	  (c:noop)))
     (else (newline-between
	    (if (or (monomorphic? w) (squeezed? w) (squished? w))
		(c:noop)
		(c::= (c:tag c w) (c:type-tag u)))
	    (c::= (value-vector-length c u w) c2)
	    (if (degenerate-vector-type? u)
		(c:noop)
		(c::= (value-vector-elements c u w) c1))
	    (compile-return r))))))))

(define *ti* #f)

(define (allocate-temporary w)
 (if (fictitious? w)
     'void9
     (let ((t (c:t *ti*)))
      (set! *ti* (+ *ti* 1))
      (outside-body (c:declaration w t (c:noop)))
      t)))

(define (compile-error c x/y p?)
 ;; needs work: Should give an indication of the call-site offset.
 (cond
  ((expression? x/y)
   (let ((error (assoc c *errors*)))
    ;; conventions: ERROR
    (unless error (fuck-up))
    (when (and p? (not (third error))) (fuck-up))
    (unless (memq error *errors-used*)
     (set! *errors-used* (cons error *errors-used*)))
    (when (third error)
     (cond
      ((expression-pathname x/y)
       (notify "~a:~s:~s:~a"
	       (expression-pathname x/y)
	       (expression-line-position x/y)
	       (expression-character-position x/y)
	       (cond ((eq? (third error) #t)
		      (format #f (second error)
			      (if p? "will not be" "might not be")))
		     ((string? (third error)) ((if p? fourth third) error))
		     (else (fuck-up)))))
      (else
       (notify "In ~a" (environment-name (expression-environment x/y)))
       (notify (cond ((eq? (third error) #t)
		      (format #f (second error)
			      (if p? "will not be" "might not be")))
		     ((string? (third error)) ((if p? fourth third) error))
		     (else (fuck-up))))))
     (set! *warnings*
	   (cons (list (expression-index x/y)
		       (replace-true-and-false-with-t-and-nil p?)
		       (cond ((eq? (third error) #t)
			      (format #f (second error)
				      (if p? "will not be" "might not be")))
			     ((string? (third error))
			      ((if p? fourth third) error))
			     (else (fuck-up)))
		       x/y)
		 *warnings*)))
    (c:no-return
     (newline-between
      (cond
       ((expression-pathname x/y)
	(c:backtrace (c:string (expression-pathname x/y))
		     (c:fixnum (expression-line-position x/y))
		     (c:fixnum (expression-character-position x/y))))
       ((empty? (expression-environment x/y))
	(c:backtrace-internal (c:string "top level")))
       (else (c:backtrace-internal
	      (c:string (environment-name (expression-environment x/y))))))
      (c:gosub (c:error c))))))
  ((call-site? x/y) (compile-error c (call-site-expression x/y) p?))
  (else (fuck-up))))

(define (compile-comparison r y cs ws c1 c2)
 ;; needs work: The code size generated can be exponential in the number of
 ;;             arguments.
 ;; needs work: To handle rectangular numbers.
 (let loop ((cs cs) (ws ws) (cs1 '()) (us '()))
  (if (null? cs)
      (let ((cs1 (reverse cs1)))
       (compile-test
	r
	(apply c:&&
	       (map (lambda (c2 c3 u1 u2) (c1 c2 c3 u1 u2))
		    (but-last cs1)
		    (rest cs1)
		    (but-last us)
		    (rest us)))))
      (type-switch number-type?
		   (first ws)
		   r
		   (first cs)
		   (lambda (u)
		    (loop (rest cs)
			  (rest ws)
			  (cons (c:value (first cs) u (first ws)) cs1)
			  (cons u us)))
		   (lambda (p?) (compile-error c2 y p?))))))

(define (compile-arithmetic m r y cs ws u1 c1 c2)
 ;; needs work: The code size generated can be exponential in the number of
 ;;             arguments.
 (let loop ((cs0 cs) (ws0 ws) (cs '()) (us '()))
  (define (arithmetic-result-type us)
   (if (null? (rest (rest us)))
       (u1 (second us) (first us))
       (u1 (arithmetic-result-type (rest us)) (first us))))
  (define (compile-arithmetic-internal cs us ws)
   (if (null? (rest (rest cs)))
       (c1 (second cs)
	   (second us)
	   (first cs)
	   (first us))
       (c1 (compile-arithmetic-internal (rest cs) (rest us) (rest ws))
	   (arithmetic-result-type (rest us))
	   (first cs)
	   (first us))))
  (if (null? ws0)
      (widen-type r
		  (compile-arithmetic-internal cs us (reverse ws))
		  (arithmetic-result-type us))
      (type-switch m
		   (first ws0)
		   r
		   (first cs0)
		   (lambda (u)
		    (loop (rest cs0)
			  (rest ws0)
			  (cons (c:value (first cs0) u (first ws0)) cs)
			  (cons u us)))
		   (lambda (p?) (compile-error c2 y p?))))))

(define (compile-allocate e y c c1 p?)
 ;; needs work: To use code-generation abstractions.
 (cond
  ((region-allocation? e)
   (if *expandable-regions?*
       (newline-between
	(cond (*treadmarks?*
	       (include! "Tmk")		;Tmk_lock_acquire
	       (c:gosub "Tmk_lock_acquire" (c:0)))
	      (else (c:noop)))
	(c:if (c:> (c:+ (c:fp e) c)	;needs work: To check for overflow.
		   (c:& (c:subscript (c:-> (c:region e) (c:data))
				     (c:region-size e))))
	      (newline-between
	       (semicolon-after
		(space-between "struct" (c:region e) (star-before (c:region))))
	       ;; needs work: needs abstraction for initialized declaration
	       (semicolon-after
		(space-between
		 *length*
		 (unparenthesize (c:= (c:region-size) (c:big-region-size e)))))
	       (c:if
		(c:> c (c:region-size)) (c::= (c:region-size) c) (c:noop) #t)
	       (if *memory-messages?*
		   (c:printf (c:string
			      (format #f "Allocating region segment for ~a~%"
				      (environment-name e))))
		   (c:noop))
	       (c::= (c:region)
		     (c:cast
		      (space-between "struct" (c:region e) "*")
		      (c:malloc
		       ;; needs work: To check for overflow.
		       (c:+ (c:sizeof (space-between "struct" (c:region e)))
			    ;; Overflow can't occur.
			    (c:- (c:region-size) (c:1)))
		       (has-nonatomic-region? e))))
	       (if *memory-checks?*
		   (c:if (c:==null (c:region))
			 (compile-error "out_of_memory" y #f)
			 (c:noop)
			 #t)
		   (c:noop))
	       (c::= (c:-> (c:region) (c:region)) (c:region e))
	       (if (reentrant? e)
		   (c::= (c:-> (c:region) (c:region-size)) (c:region-size e))
		   (c:noop))
	       (c::= (c:region-size e) (c:region-size))
	       (c::= (c:region e) (c:region))
	       (c::= (c:fp e)
		     (c:& (c:subscript (c:-> (c:region) (c:data)) (c:0))))
	       ;; needs work: There is a bug here. If the region_size was
	       ;;             bumped for a large object then there is no slack
	       ;;             for alignment adjustment.
	       (c:align (c:fp e)))
	      (c:noop)
	      #f)
	(c1 (c:fp e) (lambda (c) (c:noop)))
	;; needs work: To check for overflow.
	(c:+=
	 (c:fp e)
	 (if (zero? *allocation-alignment*)
	     c
	     ;; needs work: To check for overflow.
	     (c:+ c
		  ;; Overflow can't occur.
		  (c:& (c:- (c:fixnum (expt 2 *allocation-alignment*))
			    (c:% c (c:fixnum (expt 2 *allocation-alignment*))))
		       (c:fixnum (- (expt 2 *allocation-alignment*) 1))))))
	(cond (*treadmarks?*
	       (include! "Tmk")		;Tmk_lock_release
	       (c:gosub "Tmk_lock_release" (c:0)))
	      (else (c:noop))))
       (newline-between
	(cond (*treadmarks?*
	       (include! "Tmk")		;Tmk_lock_acquire
	       (c:gosub "Tmk_lock_acquire" (c:0)))
	      (else (c:noop)))
	(c1 (c:fp e) (lambda (c) (c:noop)))
	(if *memory-checks?*
	    (c:if (c:> (c:+ (c:fp e) c) ;needs work: To check for overflow.
		       (c:& (c:subscript (c:region e) (c:big-region-size e))))
		  (compile-error "out_of_memory" y #f)
		  (c:noop)
		  #f)
	    (c:noop))
	;; needs work: To check for overflow.
	(c:+=
	 (c:fp e)
	 ;; needs work: To check for overflow.
	 (if (zero? *allocation-alignment*)
	     c
	     (c:+ c
		  ;; Overflow can't occur.
		  (c:& (c:- (c:fixnum (expt 2 *allocation-alignment*))
			    (c:% c (c:fixnum (expt 2 *allocation-alignment*))))
		       (c:fixnum (- (expt 2 *allocation-alignment*) 1))))))
	(cond (*treadmarks?*
	       (include! "Tmk")		;Tmk_lock_release
	       (c:gosub "Tmk_lock_release" (c:0)))
	      (else (c:noop))))))
  ((stack-allocation? e)
   ;; needs work: Might not work for COMPILE-ALLOCATE-HEADED-VECTOR because of
   ;;             the nested braces. It is possible but tedious to fix this.
   (c1 (c:alloca c)
       (lambda (c2)
	(if *memory-checks?*
	    (c:if
	     ;; note: This assumes that alloca returns NULL upon stack
	     ;;       overflow.
	     (c:==null c2) (compile-error "out_of_memory" y #f) (c:noop) #f)
	    (c:noop)))))
  ((heap-allocation? e)
   (c1 ((if p? c:gc-malloc-atomic c:gc-malloc) c)
       (lambda (c2)
	(if *memory-checks?*
	    (c:if
	     ;; note: This assumes that GC_malloc and GC_malloc_atomic return
	     ;;       NULL when there is no more memory.
	     (c:==null c2) (compile-error "out_of_memory" y #f) (c:noop) #f)
	    (c:noop)))))
  (else (fuck-up))))

(define (compile-allocate-string c1 w1 c2 y)
 ;; needs work: To use code-generation abstractions.
 (let ((u (the-member-that string-type? w1)))
  (compile-allocate
   (cdr (assq u (expression-type-allocation-alist (call-site-expression y))))
   y
   ;; needs work: To check for overflow.
   (c:* (c:+ c2 (c:1)) (c:sizeof *char*))
   (lambda (c3 c4)
    (newline-between
     (widen (create-accessor-result w1 c1) (c:type-cast c3 u) string-type?)
     (c4 (c:value c1 u w1))
     (c::= (value-string-ref c1 u w1 c2) (c:nul))))
   #t)))

(define (compile-allocate-structure c u w y)
 ;; needs work: What happens if the structure is fictitious?
 (when (fictitious? u) (fuck-up))
 (if (structure-type-immediate? u)
     (cond ((fictitious? w) (fuck-up))
	   ((monomorphic? w) (c:noop))
	   ((tag-only? w) (fuck-up))
	   ((squeezed? w) (fuck-up))
	   ((squished? w) (fuck-up))
	   (else (c::= (c:tag c w) (c:type-tag u))))
     (compile-allocate
      (cdr
       (assq u (expression-type-allocation-alist (call-site-expression y))))
      y
      (c:sizeof (c:type& u ""))
      (lambda (c1 c2)
       (newline-between
	(widen-type (create-accessor-result w c) (c:type-cast c1 u) u)
	(c2 (c:value c u w))))
      (type-atomic? u))))

(define (compile-allocate-headed-vector c1 u1 w1 c2 y)
 ;; needs work: To use code-generation abstractions.
 (if (degenerate-vector-type? u1)
     (cond ((fictitious? w1) (fuck-up))
	   ((monomorphic? w1) (c::= (value-vector-length c1 u1 w1) c2))
	   ((tag-only? w1) (fuck-up))
	   ((squeezed? w1) (fuck-up))
	   ((squished? w1)
	    (c::= (vector-length-accessor c1 u1) (squish c2 u1 w1)))
	   (else (c::= (value-vector-length c1 u1 w1) c2)))
     (compile-allocate
      (cdr
       (assq u1 (expression-type-allocation-alist (call-site-expression y))))
      y
      ;; needs work: To check for overflow.
      (c:+ (c:sizeof (space-between "struct" (c:u u1)))
	   ;; needs work: To check for overflow.
	   (c:* (c:- c2 (c:1))
		(c:sizeof (c:type-set (headed-vector-type-element u1) ""))))
      (lambda (c3 c4)
       (newline-between
	(widen-type (create-accessor-result w1 c1) (c:type-cast c3 u1) u1)
	(c4 (c:value c1 u1 w1))
	(c::= (value-vector-length c1 u1 w1) c2)))
      (type-atomic? u1))))

(define (compile-allocate-closure-level e)
 ;; needs work: To use code-generation abstractions.
 (case *closure-representation*
  ((immediate-flat)
   (unimplemented #f "Immediate flat closures are not (yet) implemented"))
  ((indirect-flat)
   (unimplemented #f "Indirect flat closures are not (yet) implemented"))
  ((immediate-display indirect-display)
   (compile-allocate
    (allocation e)
    (environment-expression e)
    (c:sizeof (space-between "struct" (c:e e)))
    (lambda (c1 c2)
     (newline-between
      (c::= (c:e e) (c:cast (space-between "struct" (c:e e) "*") c1))
      (c2 (c:e e))))
    (environment-atomic? e)))
  ((linked)
   ;; Closures and displays are confluent with linked closures.
   (compile-allocate
    (allocation e)
    (environment-expression e)
    (c:sizeof (space-between "struct" (c:p e)))
    (lambda (c1 c2)
     (newline-between
      (c::= (c:e e) (c:cast (space-between "struct" (c:p e) "*") c1))
      (c2 (c:e e))))
    (environment-atomic? e)))
  (else (fuck-up))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
