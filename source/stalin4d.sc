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
(module stalin4d)

(include "QobiScheme.sch")
(include "stalin4d.sch")
;;; End delete for Trotsky

(define-structure primitive-procedure
 compatible-procedure?
 truly-compatible-procedure?
 consequent-contexts
 alternate-contexts
 propagate-call!
 promote!
 compile-call)

(define *primitive-procedure-handlers* '())

;;; Begin delete for Trotsky
(define-macro define-primitive-procedure
 (lambda (form expander)
  (unless (= (length form) 9)
   (error 'define-primitive-procedure "Wrong number of arguments: ~s" form))
  (expander `(set! *primitive-procedure-handlers*
		   (cons
		    (cons ',(second form)
			  (make-primitive-procedure
			   ,(third form)
			   (lambda (y u0 w0) ,(fourth form))
			   (lambda (y u0 n w0) ,(fifth form))
			   (lambda (y u0 n w0) ,(sixth form))
			   (lambda (y u0 propagate-result!
				      propagate-type-predicate! w0)
			    ,(seventh form))
			   (lambda (r y u0 ws w w0 w1 w2 w3) ,(eighth form))
			   (lambda (r y u0 ts ws t w compile-type-predicate
				      t0 w0 t1 w1 t2 w2 t3 w3)
			    ,(ninth form))))
		    *primitive-procedure-handlers*))
	    expander)))
;;; End delete for Trotsky

(define (zero-arguments-compatible? u0 ws w)
 (when (can-be-non? null-type? w) (fuck-up))
 (= (length ws) 0))

(define (one-argument-compatible? u0 ws w)
 (when (can-be-non? null-type? w) (fuck-up))
 (= (length ws) 1))

(define (two-arguments-compatible? u0 ws w)
 (when (can-be-non? null-type? w) (fuck-up))
 (= (length ws) 2))

(define (three-arguments-compatible? u0 ws w)
 (when (can-be-non? null-type? w) (fuck-up))
 (= (length ws) 3))

(define (n-arguments-compatible? u0 ws w)
 (when (can-be-non? null-type? w) (fuck-up))
 (= (length ws) (second (primitive-procedure-type-arguments u0))))

(define (one-or-two-arguments-compatible? u0 ws w)
 (when (can-be-non? null-type? w) (fuck-up))
 (or (one-argument-compatible? u0 ws w) (two-arguments-compatible? u0 ws w)))

(define (zero-or-more-arguments-compatible? u0 ws w)
 (when (can-be-non? null-type? w) (fuck-up))
 (>= (length ws) 0))

(define (one-or-more-arguments-compatible? u0 ws w)
 (when (can-be-non? null-type? w) (fuck-up))
 (>= (length ws) 1))

(define (two-or-more-arguments-compatible? u0 ws w)
 (when (can-be-non? null-type? w) (fuck-up))
 (>= (length ws) 2))

(define (zero-arguments-truly-compatible?)
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  #t))

(define (one-argument-truly-compatible? m)
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (and (= (length ws) 1) (can-be? m (first ws)))))

(define (two-arguments-truly-compatible? m1 m2)
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (and (= (length ws) 2)
       (can-be? m1 (first ws))
       (can-be? m2 (second ws)))))

(define (three-arguments-truly-compatible? m1 m2 m3)
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (and (= (length ws) 3)
       (can-be? m1 (first ws))
       (can-be? m2 (second ws))
       (can-be? m3 (third ws)))))

(define (n-arguments-truly-compatible?)
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  #t))

(define (one-or-two-arguments-truly-compatible? m1 m2)
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (or (and (= (length ws) 1) (can-be? m1 (first ws)))
      (and (= (length ws) 2)
	   (can-be? m1 (first ws))
	   (can-be? m2 (second ws))))))

(define (all-arguments-truly-compatible? m)
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (every (lambda (w) (can-be? m w)) ws)))

(define (zero-arguments-propagate! p)
 ;; conventions: P
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (unless (= (length ws) 0) (fuck-up))
  (p)))

(define (one-argument-propagate! p)
 ;; conventions: P
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (unless (= (length ws) 1) (fuck-up))
  (p (first ws))))

(define (two-arguments-propagate! p)
 ;; conventions: P
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (unless (= (length ws) 2) (fuck-up))
  (p (first ws) (second ws))))

(define (three-arguments-propagate! p)
 ;; conventions: P
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (unless (= (length ws) 3) (fuck-up))
  (p (first ws) (second ws) (third ws))))

(define (n-arguments-propagate! p)
 ;; conventions: P
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (p ws)))

(define (one-or-two-arguments-propagate! p1 p2)
 ;; conventions: P1 P2
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (cond ((= (length ws) 1) (p1 (first ws)))
	((= (length ws) 2) (p2 (first ws) (second ws)))
	(else (fuck-up)))))

(define (all-arguments-propagate! p)
 ;; conventions: P
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (p ws)))

;;; needs work: Every instance of WIDEN and WIDEN-TYPE in this file must be
;;;             checked for the case when the RESULT-KIND is DISCARD or
;;;             ANTECEDENT. If the generated C code could raise an exception
;;;             then one should use the strict option (P?=#F) to MOVE-GENERAL.
;;;             This is technically not required by R4RS.

(define-primitive-procedure structure?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list (structure-type-named? (first (primitive-procedure-type-arguments u0))))
 (list (lambda (u)
	(not ((structure-type-named?
	       (first (primitive-procedure-type-arguments u0)))
	      u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? (structure-type-named?
			(first (primitive-procedure-type-arguments u0)))
		       w1)
	      (can-be-non? (structure-type-named?
			    (first (primitive-procedure-type-arguments u0)))
			   w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate!
    (structure-type-named? (first (primitive-procedure-type-arguments u0))))))
 #f
 (compile-type-predicate
  (structure-type-named? (first (primitive-procedure-type-arguments u0)))))

(define-primitive-procedure make-structure
 n-arguments-compatible?
 (n-arguments-truly-compatible?)
 (map-n (lambda (i) type?) n)
 (map-n (lambda (i) type?) n)
 (n-arguments-propagate!
  (lambda (ws)
   (propagate-result!
    (<structure> (first (primitive-procedure-type-arguments u0))
		 (second (primitive-procedure-type-arguments u0))
		 ;; note: This is suboptimal since type propagation is not yet
		 ;;       complete and APPLY-CLOSED-WORLD-ASSUMPTION! has not
		 ;;       been done yet.
		 (map members ws)
		 (call-site-expression y)))))
 (unless (or (discard? r)
	     (antecedent? r)
	     (and (return? r) (not (result-accessed? r))))
  (let* ((w (result-type-set r))
	 (u (the-member-that
	     (lambda (u)
	      (and ((structure-type-named?
		     (first (primitive-procedure-type-arguments u0)))
		    u)
		   (memq (call-site-expression y)
			 (structure-type-allocating-expressions u))))
	     w)))
   (unless (fictitious? u)
    (for-each
     (lambda (w0 i w1)
      (promote!
       (if (fictitious? w0) *discard* (create-accessor-result w0 #f)) w1 w1))
     (structure-type-slots u)
     (enumerate (length (structure-type-slots u)))
     ws))))
 (cond
  ((discard? r) (c:noop))
  ((antecedent? r) (return-true r))
  ((and (return? r) (not (result-accessed? r))) (compile-return r))
  (else
   (let* ((c (result-c r))
	  (w (result-type-set r))
	  (u (the-member-that
	      (lambda (u)
	       (and ((structure-type-named?
		      (first (primitive-procedure-type-arguments u0)))
		     u)
		    (memq (call-site-expression y)
			  (structure-type-allocating-expressions u))))
	      w)))
    (if (fictitious? u)
	(widen-type r 'void22 u)
	(newline-between
	 (compile-allocate-structure c u w y)
	 (newlines-between
	  (map (lambda (w0 i t1 w1)
		(move
		 (if (fictitious? w0)
		     *discard*
		     (create-accessor-result w0 (value-structure-ref c u w i)))
		 t1
		 w1))
	       (structure-type-slots u)
	       (enumerate (length (structure-type-slots u)))
	       ts
	       ws))
	 (compile-return r)))))))

(define-primitive-procedure structure-ref
 one-argument-compatible?
 (one-argument-truly-compatible?
  (structure-type-named? (first (primitive-procedure-type-arguments u0))))
 (list (structure-type-named? (first (primitive-procedure-type-arguments u0))))
 (list (structure-type-named? (first (primitive-procedure-type-arguments u0))))
 (one-argument-propagate!
  (lambda (w1)
   (for-each-member
    (lambda (u1)
     (when ((structure-type-named?
	     (first (primitive-procedure-type-arguments u0)))
	    u1)
      (list-set! (structure-type-structure-ref-accessed? u1)
		 (second (primitive-procedure-type-arguments u0))
		 #t)
      (for-each-member
       propagate-result!
       (list-ref (structure-type-slots u1)
		 (second (primitive-procedure-type-arguments u0))))))
    w1)))
 (for-each-member
  (lambda (u1)
   (when ((structure-type-named?
	   (first (primitive-procedure-type-arguments u0)))
	  u1)
    (promote! r
	      (list-ref (structure-type-slots u1)
			(second (primitive-procedure-type-arguments u0)))
	      (list-ref (structure-type-slots u1)
			(second (primitive-procedure-type-arguments u0))))))
  w1)
 (type-switch
  (structure-type-named? (first (primitive-procedure-type-arguments u0)))
  w1
  r
  t1
  (lambda (u1)
   (move r
	 (value-structure-ref
	  t1 u1 w1 (second (primitive-procedure-type-arguments u0)))
	 (list-ref (structure-type-slots u1)
		   (second (primitive-procedure-type-arguments u0)))))
  (lambda (p?) (compile-error "structure_ref" y p?))))

(define-primitive-procedure structure-set!
 two-arguments-compatible?
 (two-arguments-truly-compatible?
  (structure-type-named? (first (primitive-procedure-type-arguments u0)))
  type?)
 (list (structure-type-named? (first (primitive-procedure-type-arguments u0)))
       type?)
 (list (structure-type-named? (first (primitive-procedure-type-arguments u0)))
       type?)
 (two-arguments-propagate!
  (lambda (w1 w2)
   (for-each-member
    (lambda (u1)
     (when ((structure-type-named?
	     (first (primitive-procedure-type-arguments u0)))
	    u1)
      (set-structure-type-immediate?! u1 #f)
      (assert-subset!
       w2
       (list-ref (structure-type-slots u1)
		 (second (primitive-procedure-type-arguments u0))))))
    w1)))
 (for-each-member
  (lambda (u1)
   (when ((structure-type-named?
	   (first (primitive-procedure-type-arguments u0)))
	  u1)
    (promote! (if (fictitious?
		   (list-ref (structure-type-slots u1)
			     (second (primitive-procedure-type-arguments u0))))
		  *discard*
		  (create-accessor-result
		   (list-ref (structure-type-slots u1)
			     (second (primitive-procedure-type-arguments u0)))
		   #f))
	      w2
	      w2)))
  w1)
 (type-switch
  (structure-type-named? (first (primitive-procedure-type-arguments u0)))
  w1
  r
  t1
  (lambda (u1)
   (newline-between
    (move (if (fictitious?
	       (list-ref (structure-type-slots u1)
			 (second (primitive-procedure-type-arguments u0))))
	      *discard*
	      (create-accessor-result
	       (list-ref (structure-type-slots u1)
			 (second (primitive-procedure-type-arguments u0)))
	       (value-structure-ref
		t1 u1 w1 (second (primitive-procedure-type-arguments u0)))))
	  t2
	  w2)
    (compile-return r)))
  (lambda (p?) (compile-error "structure_set" y p?))))

(define-primitive-procedure not
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list false-type?)
 (list (lambda (u) (not (false-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? false-type? w1) (can-be-non? false-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! false-type?)))
 #f
 (compile-type-predicate false-type?))

(define-primitive-procedure boolean?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list boolean-type?)
 (list (lambda (u) (not (boolean-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? boolean-type? w1) (can-be-non? boolean-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! boolean-type?)))
 #f
 (compile-type-predicate boolean-type?))

(define-primitive-procedure eq?
 two-arguments-compatible?
 (two-arguments-truly-compatible? type? type?)
 (list type? type?)
 (list type? type?)
 (two-arguments-propagate!
  (lambda (w1 w2)
   (let ((p0?
	  (and (not (void? w1))
	       (not (void? w2))
	       (or
		;; This is suboptimal. W1 or W2 might have multiple members now
		;; that will be merged into a monotype by
		;; APPLY-CLOSED-WORLD-ASSUMPTION!.
		(multimorphic? w1)
		(multimorphic? w2)
		;; This is suboptimal. A structure or a native procedure might
		;; in the end be fictitious but we can't know that at this
		;; point.
		(not (necessarily-fictitious? (the-member w1)))
		(not (necessarily-fictitious? (the-member w2)))
		;; This is suboptimal. The members of W1 and W2 might be
		;; equated by APPLY-CLOSED-WORLD-ASSUMPTION!.
		(not (eq? (the-member w1) (the-member w2))))))
	 ;; needs work: This is unsound. There might be a U1 in W1 and a U2 in
	 ;;             W2 that are not EQ? but that will become EQ? as a
	 ;;             result of APPLY-CLOSED-WORLD-ASSUMPTION!.
	 (p1? (can-be? (lambda (u1) (member? u1 w2)) w1)))
    (when p1? (propagate-result! <true>))
    (when p0? (propagate-result! <false>))
    (when (and p0? p1?)
     (for-each-member (lambda (u1) (set-type-eq?-accessed?! u1 #t)) w1)
     (for-each-member (lambda (u2) (set-type-eq?-accessed?! u2 #t)) w2)
     (for-each-member (lambda (u1)
		       (when (structure-type? u1)
			(set-structure-type-immediate?! u1 #f)))
		      w1)
     (for-each-member (lambda (u2)
		       (when (structure-type? u2)
			(set-structure-type-immediate?! u2 #f)))
		      w2)))))
 #f
 (let ((us (intersectionq (members w1) (members w2))))
  (if (or (null? us)
	  (cond ((converted? y)
		 (unless (and (eq? (expression-kind (call-site-expression y))
				   'converted-call)
			      (eq? (expression-kind
				    (expression-callee
				     (call-site-expression y)))
				   'converted-continuation)
			      (list? (expression-parameters
				      (expression-callee
				       (call-site-expression y))))
			      (not (null? (expression-parameters
					   (expression-callee
					    (call-site-expression y))))))
		  (fuck-up))
		 (must-be?
		  false-type?
		  (variable-type-set
		   (first (expression-parameters
			   (expression-callee (call-site-expression y)))))))
		(else
		 (must-be? false-type?
			   (expression-type-set (call-site-expression y))))))
      (return-false r)
      (cond
       ((fake? w1)
	(cond ((fake? w2)
	       ;; Must be true since fictitious type sets are monotypes and the
	       ;; intersection is non empty.
	       (return-true r))
	      ((monomorphic? w2)
	       ;; Nonfictitious monotypes are disjoint with fictitious type
	       ;; sets.
	       (fuck-up))
	      ((tag-only? w2)
	       (compile-test
		r (c:== (c:type-tag (the-member w1)) (c:tag t2 w2))))
	      ((squeezed? w2)
               ;; Rectangulars, immediate closures, nonfictitious immediate
	       ;; structures, nondegenerate nonheaded vectors, and
               ;; nondegenerate displaced vectors can't be squeezed so there
               ;; is no need to handle == on structs.
	       (compile-test
		r (c:== (c:type-set-cast (c:type-tag (the-member w1)) w2) t2)))
	      ((squished? w2)
               ;; Rectangulars, immediate closures, nonfictitious immediate
	       ;; structures, nondegenerate nonheaded vectors, and
               ;; nondegenerate displaced vectors can't be squished so there
               ;; is no need to handle == on structs.
	       (compile-test
		r (c:== (c:type-set-cast (c:type-tag (the-member w1)) w2) t2)))
	      (else (compile-test
		     r (c:== (c:type-tag (the-member w1)) (c:tag t2 w2))))))
       ((monomorphic? w1)
	(compile-test
	 r
	 (cond
	  ((fake? w2)
	   ;; Nonfictitious monotypes are disjoint with fictitious type sets.
	   (fuck-up))
	  ((monomorphic? w2)
	   (c:==struct (c:value t1 (the-member w1) w1)
		       (c:value t2 (the-member w2) w2)
		       (the-member w1)))
	  ((tag-only? w2)
	   ;; CHAR is the only fictitious type that can be a nonfictitious
	   ;; monotype.
	   (unless (char-type? (the-member w1)) (fuck-up))
	   ;; Must cast T1 to be a type and not cast T2 to be a char since T2
	   ;; could have other tags in it besides characters.
	   ;; This assumes that *TAG* is unsigned so that << does a logical
	   ;; shift. The call to C:UNSIGNED-CHAR-CAST is in case *CHAR* is
	   ;; signed to force << to be a logical shift without a prior sign
	   ;; extend. The call to C:TYPE-SET-CAST is to prevent any overflow
	   ;; in the logical shift.
	   (c:== (c:<< (c:type-set-cast
			(c:unsigned-char-cast (c:value t1 (the-member w1) w1))
			w2)
		       (c:fixnum *worst-alignment*))
		 (c:tag t2 w2)))
	  ((squeezed? w2)
           ;; Rectangulars, immediate closures, nonfictitious immediate
	   ;; structures, nondegenerate nonheaded vectors, and nondegenerate
	   ;; displaced vectors can't be squeezed so there is no need to
	   ;; handle == on structs.
           (c:== (squeeze t1 (the-member w1) w2) t2))
	  ((squished? w2)
           ;; Rectangulars, immediate closures, nonfictitious immediate
	   ;; structures, nondegenerate nonheaded vectors, and nondegenerate
	   ;; displaced vectors can't be squeezed so there is no need to
	   ;; handle == on structs.
           (c:== (squish t1 (the-member w1) w2) t2))
	  (else (if (char-type? (the-member w1))
		    (c:== (c:value t1 (the-member w1) w1)
			  (c:value t2 (the-member w1) w2))
		    (c:&& (c:== (c:type-tag (the-member w1)) (c:tag t2 w2))
			  (c:==struct (c:value t1 (the-member w1) w1)
				      (c:value t2 (the-member w1) w2)
				      (the-member w1))))))))
       ((tag-only? w1)
	(compile-test
	 r
	 (cond
	  ((fake? w2) (c:== (c:tag t1 w1) (c:type-tag (the-member w2))))
	  ((monomorphic? w2)
	   ;; CHAR is the only fictitious type that can be a nonfictitious
	   ;; monotype.
	   (unless (char-type? (the-member w2)) (fuck-up))
	   ;; Must cast T2 to be a type and not cast T1 to be a char since T1
	   ;; could have other tags in it besides characters.
	   ;; This assumes that *TAG* is unsigned so that << does a logical
	   ;; shift. The call to C:UNSIGNED-CHAR-CAST is in case *CHAR* is
	   ;; signed to force << to be a logical shift without a prior sign
	   ;; extend. The call to C:TYPE-SET-CAST is to prevent any overflow
	   ;; in the logical shift.
	   (c:== (c:tag t1 w1)
		 (c:<< (c:type-set-cast
			(c:unsigned-char-cast (c:value t2 (the-member w2) w2))
			w1)
		       (c:fixnum *worst-alignment*))))
	  ((tag-only? w2) (c:== (c:tag t1 w1) (c:tag t2 w2)))
	  ((squeezed? w2)
           ;; Rectangulars, immediate closures, nonfictitious immediate
	   ;; structures, nondegenerate nonheaded vectors, and nondegenerate
	   ;; displaced vectors can't be squeezed so there is no need to
	   ;; handle == on structs.
           (c:== (c:tag->squeezed-cast t1 w1 w2) t2))
	  ((squished? w2)
           ;; Rectangulars, immediate closures, nonfictitious immediate
	   ;; structures, nondegenerate nonheaded vectors, and nondegenerate
	   ;; displaced vectors can't be squished so there is no need to
	   ;; handle == on structs.
           (c:== (c:type-set-cast (c:tag t1 w1) w2) t2))
	  (else (c:== (c:tag t1 w1) (c:tag t2 w2))))))
       ((squeezed? w1)
        ;; Rectangulars, immediate closures, nonfictitious immediate
	;; structures, nondegenerate nonheaded vectors, and nondegenerate
	;; displaced vectors can't be squeezed so there is no need to handle
	;; == on structs.
	(compile-test
	 r
	 (cond
	  ((fake? w2)
	   (c:== t1 (c:type-set-cast (c:type-tag (the-member w2)) w1)))
	  ((monomorphic? w2) (c:== t1 (squeeze t2 (the-member w2) w1)))
	  ((tag-only? w2) (c:== t1 (c:tag->squeezed-cast t2 w2 w1)))
	  ((squeezed? w2)
	   (if (eq? (squeezed-member w1) (squeezed-member w2))
	       (c:== t1 t2)
	       (c:== (c:type-set-cast t1 w2) t2)))
	  ((squished? w2)
	   (if (can-be?
		(lambda (u1)
		 (and (or (char-type? u1) (fictitious? u1)) (member? u1 w2)))
		w1)
	       (if (member? (squeezed-member w1) w2)
		   ;; The two type sets share both fictitious and
		   ;; nonfictitious members.
		   (if (zero? (squish-tag (squeezed-member w1) w2))
		       ;; In this case the matching nonfictitious members have
		       ;; squish tag zero so checking for matching fictitious
		       ;; and nonfictitious members can be done with a single
		       ;; comparison.
		       (c:== (c:type-set-cast t1 w2) t2)
		       ;; In the general case need to check for either matching
		       ;; fictitious or nonfictitious members.
		       (c:boolean-or
			;; This checks for matching fictitious members.
			(c:== (c:type-set-cast t1 w2) t2)
			;; This checks for matching nonfictitious members.
			(c:== (squish t1 (squeezed-member w1) w2) t2)))
		   ;; The two type sets share only fictitious members.
		   ;; Note that the squeezed argument will always have squish
		   ;; tag zero (as a natural consequence of the squeezed
		   ;; representation) and the squished argument will have
		   ;; squish tag zero only when it is a pointer and not a
		   ;; fixnum so there can't be a mixup.
		   (c:== (c:type-set-cast t1 w2) t2))
	       (if (member? (squeezed-member w1) w2)
		   ;; The two type sets share only nonfictitious members. This
		   ;; makes sure that the two arguments have squish tags prior
		   ;; to comparison.
		   (c:== (squish t1 (squeezed-member w1) w2) t2)
		   ;; The two type sets don't share any members.
		   (fuck-up))))
	  (else
	   (cond ((every (lambda (u) (or (char-type? u) (fictitious? u))) us)
		  ;; In the case where the only possible match is for
		  ;; nonfictitious variants it is not necessary to check for
		  ;; matches along fictitious variants.
		  (c:&& (c:== (c:tag t2 w2) (c:type-tag (squeezed-member w1)))
			(c:== t1 (c:value t2 (squeezed-member w1) w2))))
		 ((some (lambda (u) (or (char-type? u) (fictitious? u))) us)
		  (c:boolean-or
		   (c:== t1 (c:tag->squeezed-cast t2 w2 w1))
		   (c:&& (c:== (c:tag t2 w2) (c:type-tag (squeezed-member w1)))
			 (c:== t1 (c:value t2 (squeezed-member w1) w2)))))
		 ;; In the case where the only possible match is for fictitious
		 ;; variants it is not necessary to check for matches along
		 ;; nonfictitious variants.
		 (else (c:== t1 (c:tag->squeezed-cast t2 w2 w1))))))))
       ((squished? w1)
        ;; Rectangulars, immediate closures, nonfictitious immediate
	;; structures, nondegenerate nonheaded vectors, and nondegenerate
	;; displaced vectors can't be squished so there is no need to handle
	;; == on structs.
	(compile-test
	 r
	 (cond
	  ((fake? w2)
	   (c:== t1 (c:type-set-cast (c:type-tag (the-member w2)) w1)))
	  ((monomorphic? w2) (c:== t1 (squish t2 (the-member w2) w1)))
	  ((tag-only? w2) (c:== t1 (c:type-set-cast (c:tag t2 w2) w1)))
	  ((squeezed? w2)
	   (if (can-be?
		(lambda (u2)
		 (and (or (char-type? u2) (fictitious? u2)) (member? u2 w1)))
		w2)
	       (if (member? (squeezed-member w2) w1)
		   ;; The two type sets share both fictitious and nonfictitious
		   ;; members.
		   (if (zero? (squish-tag (squeezed-member w2) w1))
		       ;; In this case the matching nonfictitious members have
		       ;; squish tag zero so checking for matching fictitious
		       ;; and nonfictitious members can be done with a single
		       ;; comparison.
		       (c:== t1 (c:type-set-cast t2 w1))
		       ;; In the general case need to check for matching
		       ;; fictitious and nonfictitious members.
		       (c:boolean-or
			;; This checks for matching fictitious members.
			(c:== t1 (c:type-set-cast t2 w1))
			;; This checks for matching nonfictitious members.
			(c:== t1 (squish t2 (squeezed-member w2) w1))))
		   ;; The two type sets share only fictitious members.
		   ;; Note that the squeezed argument will always have squish
		   ;; tag zero (as a natural consequence of the squeezed
		   ;; representation) and the squished argument will have
		   ;; squish tag zero only when it is a pointer and not a
		   ;; fixnum so there can't be a mixup.
		   (c:== t1 (c:type-set-cast t2 w1)))
	       (if (member? (squeezed-member w2) w1)
		   ;; The two type sets share only nonfictitious members. This
		   ;; makes sure that the two arguments have squish tags prior
		   ;; to comparison.
		   (c:== t1 (squish t2 (squeezed-member w2) w1))
		   ;; The two type sets don't share any members.
		   (fuck-up))))
	  ((squished? w2)
	   (cond
	    ((and (every (lambda (u)
			  (or (char-type? u)
			      (fictitious? u)
			      (= (squish-tag u w1) (squish-tag u w2))))
			 us)
		  (= (squish-alignment w1) (squish-alignment w2)))
	     (c:== t1 t2))
	    ((and (must-be?
		   (lambda (u1)
		    (or (char-type? u1) (fictitious? u1) (member? u1 w2)))
		   w1)
		  (must-be?
		   (lambda (u2)
		    (or (char-type? u2) (fictitious? u2) (member? u2 w1)))
		   w2))
	     (c:== t1 t2))
	    ((some (lambda (u) (or (char-type? u) (fictitious? u))) us)
	     (apply
	      c:boolean-or
	      (cons
	       (if (and (can-be? (lambda (u1)
				  (and (not (char-type? u1))
				       (not (fictitious? u1))
				       (zero? (squish-tag u1 w1))))
				 w1)
			(can-be? (lambda (u2)
				  (and (not (char-type? u2))
				       (not (fictitious? u2))
				       (zero? (squish-tag u2 w2))))
				 w2))
		   (c:&& (c:==0 (extract-squish-tag t1 w1))
			 (c:< t1 (c:value-offset))
			 (c:== t1 t2))
		   (c:&& (c:==0 (extract-squish-tag t1 w1)) (c:== t1 t2)))
	       (map (lambda (u)
		     (if (= (squish-tag u w1) (squish-tag u w2))
			 (c:&& (c:== (extract-squish-tag t1 w1)
				     (c:fixnum (squish-tag u w1)))
			       (c:== t1 t2))
			 (c:&& (c:== (extract-squish-tag t1 w1)
				     (c:fixnum (squish-tag u w1)))
			       (c:== (extract-squish-tag t2 w2)
				     (c:fixnum (squish-tag u w2)))
			       (c:== (c:value t1 u w1) (c:value t2 u w2)))))
		    (remove-if (lambda (u) (or (char-type? u) (fictitious? u)))
			       us)))))
	    (else
	     (apply
	      c:boolean-or
	      (map (lambda (u)
		    (if (= (squish-tag u w1) (squish-tag u w2))
			(c:&& (c:== (extract-squish-tag t1 w1)
				    (c:fixnum (squish-tag u w1)))
			      (c:== t1 t2))
			(c:&& (c:== (extract-squish-tag t1 w1)
				    (c:fixnum (squish-tag u w1)))
			      (c:== (extract-squish-tag t2 w2)
				    (c:fixnum (squish-tag u w2)))
			      (c:== (c:value t1 u w1) (c:value t2 u w2)))))
		   us)))))
	  (else
	   (apply
	    c:boolean-or
	    (cons (c:== t1 (c:type-set-cast (c:tag t2 w2) w1))
		  (map (lambda (u)
			(c:&& (c:== (c:type-tag u) (c:tag t2 w2))
			      (c:== (c:value t1 u w1) (c:value t2 u w2))))
		       (remove-if
			(lambda (u) (or (char-type? u) (fictitious? u)))
			us))))))))
       (else
	(cond
	 ((fake? w2)
	  (compile-test r (c:== (c:tag t1 w1) (c:type-tag (the-member w2)))))
	 ((monomorphic? w2)
	  (compile-test
	   r
	   (if (char-type? (the-member w2))
	       (c:== (c:value t1 (the-member w2) w1)
		     (c:value t2 (the-member w2) w2))
	       (c:&& (c:== (c:tag t1 w1) (c:type-tag (the-member w2)))
		     (c:==struct (c:value t1 (the-member w2) w1)
				 (c:value t2 (the-member w2) w2)
				 (the-member w2))))))
	 ((tag-only? w2) (compile-test r (c:== (c:tag t1 w1) (c:tag t2 w2))))
	 ((squeezed? w2)
          ;; Rectangulars, immediate closures, nonfictitious immediate
	  ;; structures, nondegenerate nonheaded vectors, and nondegenerate
          ;; displaced vectors can't be squeezed so there is no need to handle
          ;; == on structs.
	  (compile-test
	   r
	   (cond
	    ((every (lambda (u) (or (char-type? u) (fictitious? u))) us)
	     ;; In the case where the only possible match is for nonfictitious
	     ;; variants it is not necessary to check for matches along
	     ;; fictitious variants.
	     (c:&& (c:== (c:tag t1 w1) (c:type-tag (squeezed-member w2)))
		   (c:== (c:value t1 (squeezed-member w2) w1) t2)))
	    ((some (lambda (u) (or (char-type? u) (fictitious? u))) us)
	     (c:boolean-or
	      (c:== (c:tag->squeezed-cast t1 w1 w2) t2)
	      (c:&& (c:== (c:tag t1 w1) (c:type-tag (squeezed-member w2)))
		    (c:== (c:value t1 (squeezed-member w2) w1) t2))))
	    ;; In the case where the only possible match is for fictitious
	    ;; variants it is not necessary to check for matches along
	    ;; nonfictitious variants.
	    (else (c:== (c:tag->squeezed-cast t1 w1 w2) t2)))))
	 ((squished? w2)
          ;; Rectangulars, immediate closures, nonfictitious immediate
	  ;; structures, nondegenerate nonheaded vectors, and nondegenerate
          ;; displaced vectors can't be squished so there is no need to handle
          ;; == on structs.
	  (compile-test
	   r
	   (apply
	    c:boolean-or
	    (cons (c:== (c:type-set-cast (c:tag t1 w1) w2) t2)
		  (map (lambda (u)
			(c:&& (c:== (c:tag t1 w1) (c:type-tag u))
			      (c:== (c:value t1 u w1) (c:value t2 u w2))))
		       (remove-if
			(lambda (u) (or (char-type? u) (fictitious? u)))
			us))))))
	 (else
	  (if (every (lambda (u) (or (char-type? u) (fictitious? u))) us)
	      ;; In the case where the only possible match is for fictitious
	      ;; variants it is not necessary to check for matches along
	      ;; nonfictitious variants.
	      (compile-test r (c:== (c:tag t1 w1) (c:tag t2 w2)))
	      (if (and *eq?-forgery?*
		       (= (type-set-size w1) (type-set-size w2)))
		  (let* ((size
			  (reduce max
				  (map type-size
				       (members-that
					(lambda (u)
					 (and (not (char-type? u))
					      (not (fictitious? u))))
					w1))
				  ;; This can't happen if the type set isn't
				  ;; fictitious, monomorphic, or tag only.
				  #f))
			 (fixnum-size (quotient size *fixnum-size*))
			 (char-size (remainder size *fixnum-size*)))
		   ;; conventions: SIZE FIXNUM-SIZE CHAR-SIZE
		   (compile-test
		    r
		    (apply
		     c:&&
		     (cons
		      (c:== (c:tag t1 w1) (c:tag t2 w2))
		      (append
		       (map-n (lambda (i)
			       (c:== (c:raw-subscript
				      (c:fixnum*-cast (c:& (c:. t1 "value")))
				      (c:fixnum i))
				     (c:raw-subscript
				      (c:fixnum*-cast (c:& (c:. t2 "value")))
				      (c:fixnum i))))
			      fixnum-size)
		       (map-n (lambda (i)
			       (c:== (c:raw-subscript
				      (c:char*-cast (c:& (c:. t1 "value")))
				      (c:fixnum
				       (+ (* fixnum-size *fixnum-size*) i)))
				     (c:raw-subscript
				      (c:char*-cast (c:& (c:. t2 "value")))
				      (c:fixnum
				       (+ (* fixnum-size *fixnum-size*) i)))))
			      char-size))))))
		  (newline-between
		   (c:/**/ "EQ: dispatching general to general")
		   (c:if
		    (c:== (c:tag t1 w1) (c:tag t2 w2))
		    (nonerror-type-switch
		     (lambda (u1)
		      (and (not (char-type? u1)) (not (fictitious? u1))))
		     w1
		     r
		     t1
		     (lambda (u1)
		      (if (member? u1 w2)
			  (compile-test
			   r
			   (c:==struct
			    (c:value t1 u1 w1) (c:value t2 u1 w2) u1))
			  (return-false r)))
		     (lambda (p?) (return-true r)))
		    (return-false r)
		    #f)))))))))))

(define-primitive-procedure null?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list null-type?)
 (list (lambda (u) (not (null-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? null-type? w1) (can-be-non? null-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! null-type?)))
 #f
 (compile-type-predicate null-type?))

(define-primitive-procedure symbol?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list symbol-type?)
 (list (lambda (u) (not (symbol-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? symbol-type? w1) (can-be-non? symbol-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! symbol-type?)))
 #f
 (compile-type-predicate symbol-type?))

(define-primitive-procedure symbol->string
 one-argument-compatible?
 (one-argument-truly-compatible? symbol-type?)
 (list symbol-type?)
 (list symbol-type?)
 (one-argument-propagate!
  (lambda (w1) (propagate-result! <nonreclaimable-string>)))
 #f
 (let ((w (result-type-set r)))
  (type-switch
   symbol-type?
   w1
   r
   t1
   (lambda (u1)
    (cond
     ((internal-symbol-type? u1)
      (widen r
	     (c:string (symbol->string (internal-symbol-type-name u1)))
	     string-type?))
     ((external-symbol-type? u1) (widen r (c:value t1 u1 w1) string-type?))
     (else (fuck-up))))
   (lambda (p?) (compile-error "symbol_string" y p?)))))

(define-primitive-procedure string->uninterned-symbol
 one-argument-compatible?
 (one-argument-truly-compatible? string-type?)
 (list string-type?)
 (list string-type?)
 (one-argument-propagate!
  (lambda (w1)
   (for-each-member
    (lambda (u1)
     (when (string-type? u1) (propagate-result! (<external-symbol> u1))))
    w1)))
 #f
 (type-switch
  string-type?
  w1
  r
  t1
  (lambda (u1)
   (widen r
	  (c:value t1 u1 w1)
	  (lambda (u)
	   (and (external-symbol-type? u)
		(eq? (external-symbol-type-displaced-string-type u) u1)))))
  (lambda (p?) (compile-error "string_to_uninterned_symbol" y p?))))

(define-primitive-procedure number?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list number-type?)
 (list (lambda (u) (not (number-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? number-type? w1) (can-be-non? number-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! number-type?)))
 #f
 (compile-type-predicate number-type?))

(define-primitive-procedure real?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list number-type?)
 (list (lambda (u)
	(or (not (number-type? u))
	    (and (not (fixnum-type? u)) (not (flonum-type? u))))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? number-type? w1)
	      (can-be-non? nonrectangular-number-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (when (can-be? number-type? w1) (propagate-result! <true>))
   (when (can-be-non? nonrectangular-number-type? w1)
    (propagate-result! <false>))))
 #f
 (type-switch type?
	      w1
	      r
	      t1
	      (lambda (u1)
	       (cond ((nonrectangular-number-type? u1) (return-true r))
		     ((rectangular-type? u1)
		      (compile-test r (c:==0.0 (c:i (c:value t1 u1 w1)))))
		     (else (return-false r))))
	      (lambda (p?) (fuck-up))))

(define-primitive-procedure integer?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list number-type?)
 (list (lambda (u) (or (not (number-type? u)) (not (fixnum-type? u)))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? number-type? w1) (can-be-non? fixnum-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (when (can-be? number-type? w1) (propagate-result! <true>))
   (when (can-be-non? fixnum-type? w1) (propagate-result! <false>))))
 #f
 (type-switch
  type?
  w1
  r
  t1
  (lambda (u1)
   (cond
    ((fixnum-type? u1) (return-true r))
    ((flonum-type? u1)
     (compile-test r (c:== (c:value t1 u1 w1) (c:rint (c:value t1 u1 w1)))))
    ((rectangular-type? u1)
     (compile-test r
		   (c:&& (c:==0.0 (c:i (c:value t1 u1 w1)))
			 (c:== (c:r (c:value t1 u1 w1))
			       (c:rint (c:r (c:value t1 u1 w1)))))))
    (else (return-false r))))
  (lambda (p?) (fuck-up))))

(define-primitive-procedure exact?
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list fixnum-type?)
 (list (lambda (u) (and (number-type? u) (not (fixnum-type? u)))))
 (one-argument-propagate!
  (lambda (w1)
   (when (can-be? fixnum-type? w1) (propagate-result! <true>))
   (when (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
    (propagate-result! <false>))))
 #f
 (type-switch number-type?
	      w1
	      r
	      t1
	      (lambda (u1) (compile-time-test r (exact-type? u1)))
	      (lambda (p?) (compile-error "exact" y p?))))

(define-primitive-procedure inexact?
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list (lambda (u) (and (number-type? u) (not (fixnum-type? u)))))
 (list fixnum-type?)
 (one-argument-propagate!
  (lambda (w1)
   (when (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
    (propagate-result! <true>))
   (when (can-be? fixnum-type? w1) (propagate-result! <false>))))
 #f
 (type-switch number-type?
	      w1
	      r
	      t1
	      (lambda (u1) (compile-time-test r (inexact-type? u1)))
	      (lambda (p?) (compile-error "inexact" y p?))))

(define-primitive-procedure =
 two-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws) (propagate-result! <true>) (propagate-result! <false>)))
 #f
 (compile-comparison
  r y ts ws
  (lambda (c1 c2 u1 u2)
   (if (rectangular-type? u1)
       (if (rectangular-type? u2)
           (c:&& (c:== (c:r c1) (c:r c2)) (c:== (c:i c1) (c:i c2)))
           (c:&& (c:== (c:r c1) c2) (c:==0.0 (c:i c1))))
       (if (rectangular-type? u2)
           (c:&& (c:== c1 (c:r c2)) (c:==0.0 (c:i c2)))
           (c:== c1 c2))))
  "eql"))

(define-primitive-procedure <
 two-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws) (propagate-result! <true>) (propagate-result! <false>)))
 #f
 (compile-comparison r y ts ws (lambda (c1 c2 u1 u2) (c:< c1 c2)) "lt"))

(define-primitive-procedure >
 two-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws) (propagate-result! <true>) (propagate-result! <false>)))
 #f
 (compile-comparison r y ts ws (lambda (c1 c2 u1 u2) (c:> c1 c2)) "gt"))

(define-primitive-procedure <=
 two-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws) (propagate-result! <true>) (propagate-result! <false>)))
 #f
 (compile-comparison r y ts ws (lambda (c1 c2 u1 u2) (c:<= c1 c2)) "le"))

(define-primitive-procedure >=
 two-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws) (propagate-result! <true>) (propagate-result! <false>)))
 #f
 (compile-comparison r y ts ws (lambda (c1 c2 u1 u2) (c:>= c1 c2)) "ge"))

(define-primitive-procedure zero?
 ;; Extension to R4RS: ZERO? can apply to pointers, strings, and ports.
 one-argument-compatible?
 (one-argument-truly-compatible?
  (lambda (u1)
   (or (number-type? u1)
       (pointer-type? u1)
       (string-type? u1)
       (input-port-type? u1)
       (output-port-type? u1))))
 (list (lambda (u)
	(or (number-type? u)
	    (pointer-type? u)
	    (string-type? u)
	    (input-port-type? u)
	    (output-port-type? u))))
 (list (lambda (u)
	(or (number-type? u)
	    (pointer-type? u)
	    (string-type? u)
	    (input-port-type? u)
	    (output-port-type? u))))
 (one-argument-propagate! (lambda (w1)
			   (propagate-result! <true>)
			   (propagate-result! <false>)))
 #f
 (type-switch
  (lambda (u1)
   (or (number-type? u1)
       (pointer-type? u1)
       (string-type? u1)
       (input-port-type? u1)
       (output-port-type? u1)))
  w1
  r
  t1
  (lambda (u1)
   (cond ((or (pointer-type? u1)
	      (string-type? u1)
	      (input-port-type? u1)
	      (output-port-type? u1))
	  (compile-test r (c:==null (c:value t1 u1 w1))))
	 ((fixnum-type? u1) (compile-test r (c:==0 (c:value t1 u1 w1))))
	 ((flonum-type? u1) (compile-test r (c:==0.0 (c:value t1 u1 w1))))
	 ((rectangular-type? u1)
	  (compile-test r
			(c:&& (c:==0.0 (c:r (c:value t1 u1 w1)))
			      (c:==0.0 (c:i (c:value t1 u1 w1))))))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "zero" y p?))))

(define-primitive-procedure positive?
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1)
			   (propagate-result! <true>)
			   (propagate-result! <false>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond ((fixnum-type? u1) (compile-test r (c:>0 (c:value t1 u1 w1))))
	 ((flonum-type? u1) (compile-test r (c:>0.0 (c:value t1 u1 w1))))
	 ((rectangular-type? u1)
	  (newline-between (if *type-checks?*
			       (c:if (c:!=0.0 (c:i (c:value t1 u1 w1)))
				     (compile-error "positive" y #f)
				     (c:noop)
				     #f)
			       (c:noop))
			   (compile-test r (c:>0.0 (c:r (c:value t1 u1 w1))))))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "positive" y p?))))

(define-primitive-procedure negative?
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1)
			   (propagate-result! <true>)
			   (propagate-result! <false>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond ((fixnum-type? u1) (compile-test r (c:<0 (c:value t1 u1 w1))))
	 ((flonum-type? u1) (compile-test r (c:<0.0 (c:value t1 u1 w1))))
	 ((rectangular-type? u1)
	  (newline-between (if *type-checks?*
			       (c:if (c:!=0.0 (c:i (c:value t1 u1 w1)))
				     (compile-error "negative" y #f)
				     (c:noop)
				     #f)
			       (c:noop))
			   (compile-test r (c:<0.0 (c:r (c:value t1 u1 w1))))))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "negative" y p?))))

(define-primitive-procedure max
 one-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws)
   (when (every (lambda (w) (can-be? fixnum-type? w)) ws)
    (propagate-result! <fixnum>))
   (when (and (every (lambda (w) (can-be? nonrectangular-number-type? w)) ws)
	      (some (lambda (w) (can-be? flonum-type? w)) ws))
    (propagate-result! <flonum>))
   (when (and (every (lambda (w) (can-be? number-type? w)) ws)
	      (some (lambda (w) (can-be? rectangular-type? w)) ws))
    (propagate-result! <rectangular>))))
 #f
 (if (null? (rest ws))
     (type-switch
      number-type?
      w1
      r
      t1
      (lambda (u1)
       (cond ((fixnum-type? u1) (widen r (c:value t1 u1 w1) fixnum-type?))
	     ((flonum-type? u1) (widen r (c:value t1 u1 w1) flonum-type?))
	     ;; needs work: To handle rectangular numbers.
	     (else (fuck-up))))
      (lambda (p?) (compile-error "max" y p?)))
     (compile-arithmetic
      number-type? r y ts ws
      ;; needs work: To handle rectangular numbers.
      (lambda (u1 u2)
       ;; This is a violation of the no-<...>-after-type-propagation principle.
       (if (and (fixnum-type? u1) (fixnum-type? u2)) <fixnum> <flonum>))
      ;; needs work: To handle rectangular numbers.
      (lambda (c1 u1 c2 u2)
       (if (and (fixnum-type? u1) (fixnum-type? u2))
	   (c:imax c1 c2)
	   (c:rmax c1 c2)))
      "max")))

(define-primitive-procedure min
 one-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws)
   (when (every (lambda (w) (can-be? fixnum-type? w)) ws)
    (propagate-result! <fixnum>))
   (when (and (every (lambda (w) (can-be? nonrectangular-number-type? w)) ws)
	      (some (lambda (w) (can-be? flonum-type? w)) ws))
    (propagate-result! <flonum>))
   (when (and (every (lambda (w) (can-be? number-type? w)) ws)
	      (some (lambda (w) (can-be? rectangular-type? w)) ws))
    (propagate-result! <rectangular>))))
 #f
 (if (null? (rest ws))
     (type-switch
      number-type?
      w1
      r
      t1
      (lambda (u1)
       (cond ((fixnum-type? u1) (widen r (c:value t1 u1 w1) fixnum-type?))
	     ((flonum-type? u1) (widen r (c:value t1 u1 w1) flonum-type?))
	     ;; needs work: To handle rectangular numbers.
	     (else (fuck-up))))
      (lambda (p?) (compile-error "min" y p?)))
     (compile-arithmetic
      number-type? r y ts ws
      ;; needs work: To handle rectangular numbers.
      (lambda (u1 u2)
       ;; This is a violation of the no-<...>-after-type-propagation principle.
       (if (and (fixnum-type? u1) (fixnum-type? u2)) <fixnum> <flonum>))
      ;; needs work: To handle rectangular numbers.
      (lambda (c1 u1 c2 u2)
       (if (and (fixnum-type? u1) (fixnum-type? u2))
	   (c:imin c1 c2)
	   (c:rmin c1 c2)))
      "min")))

(define-primitive-procedure +
 zero-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws)
   (cond
    ((null? ws) (propagate-result! <fixnum>))
    (else
     (when (every (lambda (w) (can-be? fixnum-type? w)) ws)
      (propagate-result! <fixnum>))
     (when (and (every (lambda (w) (can-be? nonrectangular-number-type? w)) ws)
		(some (lambda (w) (can-be? flonum-type? w)) ws))
      (propagate-result! <flonum>))
     (when (and (every (lambda (w) (can-be? number-type? w)) ws)
		(some (lambda (w) (can-be? rectangular-type? w)) ws))
      (propagate-result! <rectangular>))))))
 #f
 (cond
  ((null? ws) (widen r (c:0) fixnum-type?))
  ((null? (rest ws))
   (type-switch number-type?
		w1
		r
		t1
		(lambda (u1) (widen-type r (c:value t1 u1 w1) u1))
		(lambda (p?) (compile-error "plus" y p?))))
  (else
   (compile-arithmetic
    number-type? r y ts ws
    (lambda (u1 u2)
     ;; This is a violation of the no-<...>-after-type-propagation principle.
     (cond ((and (fixnum-type? u1) (fixnum-type? u2)) <fixnum>)
	   ((or (rectangular-type? u1) (rectangular-type? u2)) <rectangular>)
	   (else <flonum>)))
    (lambda (c1 u1 c2 u2)
     (cond
      ((and (fixnum-type? u1) (fixnum-type? u2))
       (when *overflow-checks?*
	(unimplemented y "Safe exact arithmetic is not (yet) implemented"))
       (c:+ c1 c2))
      ((rectangular-type? u1)
       (if (rectangular-type? u2) (c:pluscc c1 c2) (c:pluscr c1 c2)))
      ((rectangular-type? u2) (c:plusrc c1 c2))
      (else (c:+ c1 c2))))
    "plus"))))

(define-primitive-procedure *
 zero-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws)
   (cond
    ((null? ws) (propagate-result! <fixnum>))
    (else
     (when (every (lambda (w) (can-be? fixnum-type? w)) ws)
      (propagate-result! <fixnum>))
     (when (and (every (lambda (w) (can-be? nonrectangular-number-type? w)) ws)
		(some (lambda (w) (can-be? flonum-type? w)) ws))
      (propagate-result! <flonum>))
     (when (and (every (lambda (w) (can-be? number-type? w)) ws)
		(some (lambda (w) (can-be? rectangular-type? w)) ws))
      (propagate-result! <rectangular>))))))
 #f
 (cond
  ((null? ws) (widen r (c:1) fixnum-type?))
  ((null? (rest ws))
   (type-switch number-type?
		w1
		r
		t1
		(lambda (u1) (widen-type r (c:value t1 u1 w1) u1))
		(lambda (p?) (compile-error "times" y p?))))
  (else
   (compile-arithmetic
    number-type? r y ts ws
    (lambda (u1 u2)
     ;; This is a violation of the no-<...>-after-type-propagation principle.
     (cond ((and (fixnum-type? u1) (fixnum-type? u2)) <fixnum>)
	   ((or (rectangular-type? u1) (rectangular-type? u2)) <rectangular>)
	   (else <flonum>)))
    (lambda (c1 u1 c2 u2)
     (cond
      ((and (fixnum-type? u1) (fixnum-type? u2))
       (when *overflow-checks?*
	(unimplemented y "Safe exact arithmetic is not (yet) implemented"))
       (c:* c1 c2))
      ((rectangular-type? u1)
       (if (rectangular-type? u2) (c:timescc c1 c2) (c:timescr c1 c2)))
      ((rectangular-type? u2) (c:timesrc c1 c2))
      (else (c:* c1 c2))))
    "times"))))

(define-primitive-procedure -
 one-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws)
   (when (every (lambda (w) (can-be? fixnum-type? w)) ws)
    (propagate-result! <fixnum>))
   (when (and (every (lambda (w) (can-be? nonrectangular-number-type? w)) ws)
	      (some (lambda (w) (can-be? flonum-type? w)) ws))
    (propagate-result! <flonum>))
   (when (and (every (lambda (w) (can-be? number-type? w)) ws)
	      (some (lambda (w) (can-be? rectangular-type? w)) ws))
    (propagate-result! <rectangular>))))
 #f
 (if (null? (rest ws))
     (type-switch
      number-type?
      w1
      r
      t1
      (lambda (u1)
       (cond
	((fixnum-type? u1)
	 (when *overflow-checks?*
	  (unimplemented y "Safe exact arithmetic is not (yet) implemented"))
	 (widen r (c:- (c:value t1 u1 w1)) fixnum-type?))
	((flonum-type? u1) (widen r (c:- (c:value t1 u1 w1)) flonum-type?))
	((rectangular-type? u1)
	 (widen r (c:negc (c:value t1 u1 w1)) rectangular-type?))
	(else (fuck-up))))
      (lambda (p?) (compile-error "minus" y p?)))
     (compile-arithmetic
      number-type? r y ts ws
      (lambda (u1 u2)
       ;; This is a violation of the no-<...>-after-type-propagation principle.
       (cond ((and (fixnum-type? u1) (fixnum-type? u2)) <fixnum>)
	     ((or (rectangular-type? u1) (rectangular-type? u2)) <rectangular>)
	     (else <flonum>)))
      (lambda (c1 u1 c2 u2)
       (cond
	((and (fixnum-type? u1) (fixnum-type? u2))
	 (when *overflow-checks?*
	  (unimplemented y "Safe exact arithmetic is not (yet) implemented"))
	 (c:- c1 c2))
	((rectangular-type? u1)
	 (if (rectangular-type? u2) (c:minuscc c1 c2) (c:minuscr c1 c2)))
	((rectangular-type? u2) (c:minusrc c1 c2))
	(else (c:- c1 c2))))
      "minus")))

(define-primitive-procedure /
 ;; note: / will always produce a flonum result, even if given fixnum arguments
 ;;       that could produce a fixnum result. This is allowed by R4RS since
 ;;       / is not included in the table on the top of page 20, the table that
 ;;       specifies which procedures must return exact results when given
 ;;       exact arguments. If we did allow / to return a fixnum then it would
 ;;       have to return a union of a fixnum and a flonum since whether or not
 ;;       a fixnum could be returned would depend on whether or not the
 ;;       denominator divides the numerator. This is beyond the scope of the
 ;;       type system. It is preferable not to have a union type and not to
 ;;       require run-time divisability checks.
 one-or-more-arguments-compatible?
 (all-arguments-truly-compatible? number-type?)
 (map-n (lambda (i) number-type?) n)
 (map-n (lambda (i) number-type?) n)
 (all-arguments-propagate!
  (lambda (ws)
   (when (every (lambda (w) (can-be? nonrectangular-number-type? w)) ws)
    (propagate-result! <flonum>))
   (when (and (every (lambda (w) (can-be? number-type? w)) ws)
	      (some (lambda (w) (can-be? rectangular-type? w)) ws))
    (propagate-result! <rectangular>))))
 #f
 (if (null? (rest ws))
     (type-switch
      number-type?
      w1
      r
      t1
      (lambda (u1)
       (cond ((nonrectangular-number-type? u1)
	      (widen r (c:/ (c:1.0) (c:value t1 u1 w1)) flonum-type?))
	     ((rectangular-type? u1)
	      (widen r (c:recipc (c:value t1 u1 w1)) rectangular-type?))
	     (else (fuck-up))))
      (lambda (p?) (compile-error "divide" y p?)))
     (compile-arithmetic
      number-type? r y ts ws
      (lambda (u1 u2)
       (if (or (rectangular-type? u1) (rectangular-type? u2))
	   ;; This is a violation of the no-<...>-after-type-propagation
	   ;; principle.
	   <rectangular>
	   <flonum>))
      (lambda (c1 u1 c2 u2)
       (cond
	((and (fixnum-type? u1) (fixnum-type? u2))
	 (c:/ (c:flonum-cast c1) (c:flonum-cast c2)))
	((rectangular-type? u1)
	 (if (rectangular-type? u2) (c:dividecc c1 c2) (c:dividecr c1 c2)))
	((rectangular-type? u2) (c:dividerc c1 c2))
	(else (c:/ c1 c2))))
      "divide")))

(define-primitive-procedure quotient
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 ;; needs work: To check for division by zero.
 two-arguments-compatible?
 (two-arguments-truly-compatible? number-type? number-type?)
 (list number-type? number-type?)
 (list number-type? number-type?)
 (two-arguments-propagate!
  (lambda (w1 w2)
   (when (and (can-be? fixnum-type? w1) (can-be? fixnum-type? w2))
    (propagate-result! <fixnum>))
   (when (or
	  (and (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
	       (can-be? number-type? w2))
	  (and (can-be? number-type? w1)
	       (or (can-be? flonum-type? w2) (can-be? rectangular-type? w2))))
    (propagate-result! <flonum>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    number-type?
    w2
    r
    t2
    (lambda (u2)
     (newline-between
      (if *type-checks?*
	  (newline-between
	   (cond ((flonum-type? u1)
		  (c:if (c:!= (c:value t1 u1 w1) (c:rint (c:value t1 u1 w1)))
			(compile-error "quotient1" y #f)
			(c:noop)
			#f))
		 ((rectangular-type? u1)
		  (c:if (c:boolean-or
			 (c:!=0.0 (c:i (c:value t1 u1 w1)))
			 (c:!= (c:r (c:value t1 u1 w1))
			       (c:rint (c:r (c:value t1 u1 w1)))))
			(compile-error "quotient1" y #f)
			(c:noop)
			#f))
		 (else (c:noop)))
	   (cond ((flonum-type? u2)
		  (c:if (c:!= (c:value t2 u2 w2) (c:rint (c:value t2 u2 w2)))
			(compile-error "quotient2" y #f)
			(c:noop)
			#f))
		 ((rectangular-type? u2)
		  (c:if (c:boolean-or
			 (c:!=0.0 (c:i (c:value t2 u2 w2)))
			 (c:!= (c:r (c:value t2 u2 w2))
			       (c:rint (c:r (c:value t2 u2 w2)))))
			(compile-error "quotient2" y #f)
			(c:noop)
			#f))
		 (else (c:noop))))
	  (c:noop))
      (widen
       r
       (c:/ (cond
	     ((fixnum-type? u1) (c:value t1 u1 w1))
	     ((flonum-type? u1) (c:fixnum-cast (c:value t1 u1 w1)))
	     ((rectangular-type? u1) (c:fixnum-cast (c:r (c:value t1 u1 w1))))
	     (else (fuck-up)))
	    (cond
	     ((fixnum-type? u2) (c:value t2 u2 w2))
	     ((flonum-type? u2) (c:fixnum-cast (c:value t2 u2 w2)))
	     ((rectangular-type? u2) (c:fixnum-cast (c:r (c:value t2 u2 w2))))
	     (else (fuck-up))))
       (if (and (fixnum-type? u1) (fixnum-type? u2))
	   fixnum-type?
	   flonum-type?))))
    (lambda (p?) (compile-error "quotient2" y p?))))
  (lambda (p?) (compile-error "quotient1" y p?))))

(define-primitive-procedure remainder
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 ;; needs work: To check for division by zero.
 two-arguments-compatible?
 (two-arguments-truly-compatible? number-type? number-type?)
 (list number-type? number-type?)
 (list number-type? number-type?)
 (two-arguments-propagate!
  (lambda (w1 w2)
   (when (and (can-be? fixnum-type? w1) (can-be? fixnum-type? w2))
    (propagate-result! <fixnum>))
   (when (or
	  (and (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
	       (can-be? number-type? w2))
	  (and (can-be? number-type? w1)
	       (or (can-be? flonum-type? w2) (can-be? rectangular-type? w2))))
    (propagate-result! <flonum>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    number-type?
    w2
    r
    t2
    (lambda (u2)
     (newline-between
      (if *type-checks?*
	  (newline-between
	   (cond ((flonum-type? u1)
		  (c:if (c:!= (c:value t1 u1 w1) (c:rint (c:value t1 u1 w1)))
			(compile-error "remainder1" y #f)
			(c:noop)
			#f))
		 ((rectangular-type? u1)
		  (c:if (c:boolean-or
			 (c:!=0.0 (c:i (c:value t1 u1 w1)))
			 (c:!= (c:r (c:value t1 u1 w1))
			       (c:rint (c:r (c:value t1 u1 w1)))))
			(compile-error "remainder1" y #f)
			(c:noop)
			#f))
		 (else (c:noop)))
	   (cond ((flonum-type? u2)
		  (c:if (c:!= (c:value t2 u2 w2) (c:rint (c:value t2 u2 w2)))
			(compile-error "remainder2" y #f)
			(c:noop)
			#f))
		 ((rectangular-type? u2)
		  (c:if (c:boolean-or
			 (c:!=0.0 (c:i (c:value t2 u2 w2)))
			 (c:!= (c:r (c:value t2 u2 w2))
			       (c:rint (c:r (c:value t2 u2 w2)))))
			(compile-error "remainder2" y #f)
			(c:noop)
			#f))
		 (else (c:noop))))
	  (c:noop))
      (widen
       r
       (c:% (cond
	     ((fixnum-type? u1) (c:value t1 u1 w1))
	     ((flonum-type? u1) (c:fixnum-cast (c:value t1 u1 w1)))
	     ((rectangular-type? u1) (c:fixnum-cast (c:r (c:value t1 u1 w1))))
	     (else (fuck-up)))
	    (cond
	     ((fixnum-type? u2) (c:value t2 u2 w2))
	     ((flonum-type? u2) (c:fixnum-cast (c:value t2 u2 w2)))
	     ((rectangular-type? u2) (c:fixnum-cast (c:r (c:value t2 u2 w2))))
	     (else (fuck-up))))
       (if (and (fixnum-type? u1) (fixnum-type? u2))
	   fixnum-type?
	   flonum-type?))))
    (lambda (p?) (compile-error "remainder2" y p?))))
  (lambda (p?) (compile-error "remainder1" y p?))))

(define-primitive-procedure <<
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 ;; needs work: To check for overflow.
 two-arguments-compatible?
 (two-arguments-truly-compatible? number-type? number-type?)
 (list number-type? number-type?)
 (list number-type? number-type?)
 (two-arguments-propagate!
  (lambda (w1 w2)
   (when (and (can-be? fixnum-type? w1) (can-be? fixnum-type? w2))
    (propagate-result! <fixnum>))
   (when (or
	  (and (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
	       (can-be? number-type? w2))
	  (and (can-be? number-type? w1)
	       (or (can-be? flonum-type? w2) (can-be? rectangular-type? w2))))
    (propagate-result! <flonum>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    number-type?
    w2
    r
    t2
    (lambda (u2)
     (newline-between
      (if *type-checks?*
	  (newline-between
	   (cond ((flonum-type? u1)
		  (c:if (c:!= (c:value t1 u1 w1) (c:rint (c:value t1 u1 w1)))
			(compile-error "lsh1" y #f)
			(c:noop)
			#f))
		 ((rectangular-type? u1)
		  (c:if (c:boolean-or
			 (c:!=0.0 (c:i (c:value t1 u1 w1)))
			 (c:!= (c:r (c:value t1 u1 w1))
			       (c:rint (c:r (c:value t1 u1 w1)))))
			(compile-error "lsh1" y #f)
			(c:noop)
			#f))
		 (else (c:noop)))
	   (cond ((flonum-type? u2)
		  (c:if (c:!= (c:value t2 u2 w2) (c:rint (c:value t2 u2 w2)))
			(compile-error "lsh2" y #f)
			(c:noop)
			#f))
		 ((rectangular-type? u2)
		  (c:if (c:boolean-or
			 (c:!=0.0 (c:i (c:value t2 u2 w2)))
			 (c:!= (c:r (c:value t2 u2 w2))
			       (c:rint (c:r (c:value t2 u2 w2)))))
			(compile-error "lsh2" y #f)
			(c:noop)
			#f))
		 (else (c:noop))))
	  (c:noop))
      (widen
       r
       (c:<< (cond
	      ((fixnum-type? u1) (c:value t1 u1 w1))
	      ((flonum-type? u1) (c:fixnum-cast (c:value t1 u1 w1)))
	      ((rectangular-type? u1) (c:fixnum-cast (c:r (c:value t1 u1 w1))))
	      (else (fuck-up)))
	     (cond
	      ((fixnum-type? u2) (c:value t2 u2 w2))
	      ((flonum-type? u2) (c:fixnum-cast (c:value t2 u2 w2)))
	      ((rectangular-type? u2) (c:fixnum-cast (c:r (c:value t2 u2 w2))))
	      (else (fuck-up))))
       (if (and (fixnum-type? u1) (fixnum-type? u2))
	   fixnum-type?
	   flonum-type?))))
    (lambda (p?) (compile-error "lsh2" y p?))))
  (lambda (p?) (compile-error "lsh1" y p?))))

(define-primitive-procedure >>
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 two-arguments-compatible?
 (two-arguments-truly-compatible? number-type? number-type?)
 (list number-type? number-type?)
 (list number-type? number-type?)
 (two-arguments-propagate!
  (lambda (w1 w2)
   (when (and (can-be? fixnum-type? w1) (can-be? fixnum-type? w2))
    (propagate-result! <fixnum>))
   (when (or
	  (and (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
	       (can-be? number-type? w2))
	  (and (can-be? number-type? w1)
	       (or (can-be? flonum-type? w2) (can-be? rectangular-type? w2))))
    (propagate-result! <flonum>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    number-type?
    w2
    r
    t2
    (lambda (u2)
     (newline-between
      (if *type-checks?*
	  (newline-between
	   (cond ((flonum-type? u1)
		  (c:if (c:!= (c:value t1 u1 w1) (c:rint (c:value t1 u1 w1)))
			(compile-error "rsh1" y #f)
			(c:noop)
			#f))
		 ((rectangular-type? u1)
		  (c:if (c:boolean-or
			 (c:!=0.0 (c:i (c:value t1 u1 w1)))
			 (c:!= (c:r (c:value t1 u1 w1))
			       (c:rint (c:r (c:value t1 u1 w1)))))
			(compile-error "rsh1" y #f)
			(c:noop)
			#f))
		 (else (c:noop)))
	   (cond ((flonum-type? u2)
		  (c:if (c:!= (c:value t2 u2 w2) (c:rint (c:value t2 u2 w2)))
			(compile-error "rsh2" y #f)
			(c:noop)
			#f))
		 ((rectangular-type? u2)
		  (c:if (c:boolean-or
			 (c:!=0.0 (c:i (c:value t2 u2 w2)))
			 (c:!= (c:r (c:value t2 u2 w2))
			       (c:rint (c:r (c:value t2 u2 w2)))))
			(compile-error "rsh2" y #f)
			(c:noop)
			#f))
		 (else (c:noop))))
	  (c:noop))
      (widen
       r
       (c:>> (cond
	      ((fixnum-type? u1) (c:value t1 u1 w1))
	      ((flonum-type? u1) (c:fixnum-cast (c:value t1 u1 w1)))
	      ((rectangular-type? u1) (c:fixnum-cast (c:r (c:value t1 u1 w1))))
	      (else (fuck-up)))
	     (cond
	      ((fixnum-type? u2) (c:value t2 u2 w2))
	      ((flonum-type? u2) (c:fixnum-cast (c:value t2 u2 w2)))
	      ((rectangular-type? u2) (c:fixnum-cast (c:r (c:value t2 u2 w2))))
	      (else (fuck-up))))
       (if (and (fixnum-type? u1) (fixnum-type? u2))
	   fixnum-type?
	   flonum-type?))))
    (lambda (p?) (compile-error "rsh2" y p?))))
  (lambda (p?) (compile-error "rsh1" y p?))))

(define-primitive-procedure bitwise-not
 ;; needs work: To handle inexact numbers.
 one-argument-compatible?
 (one-argument-truly-compatible? fixnum-type?)
 (list fixnum-type?)
 (list fixnum-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <fixnum>)))
 #f
 (type-switch fixnum-type?
	      w1
	      r
	      t1
	      (lambda (u1) (widen r (c:~ (c:value t1 u1 w1)) fixnum-type?))
	      (lambda (p?) (compile-error "bitwise_not" y p?))))

(define-primitive-procedure bitwise-and
 ;; needs work: To handle inexact numbers.
 zero-or-more-arguments-compatible?
 (all-arguments-truly-compatible? fixnum-type?)
 (map-n (lambda (i) fixnum-type?) n)
 (map-n (lambda (i) fixnum-type?) n)
 (all-arguments-propagate! (lambda (ws) (propagate-result! <fixnum>)))
 #f
 (cond ((null? ws) (widen r (c:~ (c:0)) fixnum-type?))
       ((null? (rest ws))
	(type-switch fixnum-type?
		     w1
		     r
		     t1
		     (lambda (u1) (widen r (c:value t1 u1 w1) fixnum-type?))
		     (lambda (p?) (compile-error "bitwise_and" y p?))))
       (else (compile-arithmetic
	      fixnum-type? r y ts ws
	      ;; This is a violation of the no-<...>-after-type-propagation
	      ;; principle.
	      (lambda (u1 u2) <fixnum>)
	      (lambda (c1 u1 c2 u2) (c:& c1 c2))
	      "bitwise_and"))))

(define-primitive-procedure bitwise-or
 ;; needs work: To handle inexact numbers.
 zero-or-more-arguments-compatible?
 (all-arguments-truly-compatible? fixnum-type?)
 (map-n (lambda (i) fixnum-type?) n)
 (map-n (lambda (i) fixnum-type?) n)
 (all-arguments-propagate! (lambda (ws) (propagate-result! <fixnum>)))
 #f
 (cond ((null? ws) (widen r (c:0) fixnum-type?))
       ((null? (rest ws))
	(type-switch fixnum-type?
		     w1
		     r
		     t1
		     (lambda (u1) (widen r (c:value t1 u1 w1) fixnum-type?))
		     (lambda (p?) (compile-error "bitwise_or" y p?))))
       (else (compile-arithmetic
	      fixnum-type? r y ts ws
	      ;; This is a violation of the no-<...>-after-type-propagation
	      ;; principle.
	      (lambda (u1 u2) <fixnum>)
	      (lambda (c1 u1 c2 u2) (c:bitwise-or c1 c2))
	      "bitwise_or"))))

(define-primitive-procedure bitwise-xor
 ;; needs work: To handle inexact numbers.
 zero-or-more-arguments-compatible?
 (all-arguments-truly-compatible? fixnum-type?)
 (map-n (lambda (i) fixnum-type?) n)
 (map-n (lambda (i) fixnum-type?) n)
 (all-arguments-propagate! (lambda (ws) (propagate-result! <fixnum>)))
 #f
 (cond ((null? ws) (widen r (c:0) fixnum-type?))
       ((null? (rest ws))
	(type-switch fixnum-type?
		     w1
		     r
		     t1
		     (lambda (u1) (widen r (c:value t1 u1 w1) fixnum-type?))
		     (lambda (p?) (compile-error "bitwise_xor" y p?))))
       (else (compile-arithmetic
	      fixnum-type? r y ts ws
	      ;; This is a violation of the no-<...>-after-type-propagation
	      ;; principle.
	      (lambda (u1 u2) <fixnum>)
	      (lambda (c1 u1 c2 u2) (c:^ c1 c2))
	      "bitwise_xor"))))

(define-primitive-procedure floor
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate!
  (lambda (w1)
   (when (can-be? fixnum-type? w1) (propagate-result! <fixnum>))
   (when (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
    (propagate-result! <flonum>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond
    ((fixnum-type? u1) (widen r (c:value t1 u1 w1) fixnum-type?))
    ((flonum-type? u1) (widen r (c:floor (c:value t1 u1 w1)) flonum-type?))
    ((rectangular-type? u1)
     (newline-between
      (if *type-checks?*
	  (c:if (c:!=0.0 (c:i (c:value t1 u1 w1)))
		(compile-error "floor" y #f)
		(c:noop)
		#f)
	  (c:noop))
      (widen r (c:floor (c:r (c:value t1 u1 w1))) flonum-type?)))
    (else (fuck-up))))
  (lambda (p?) (compile-error "floor" y p?))))

(define-primitive-procedure ceiling
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate!
  (lambda (w1)
   (when (can-be? fixnum-type? w1) (propagate-result! <fixnum>))
   (when (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
    (propagate-result! <flonum>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond ((fixnum-type? u1) (widen r (c:value t1 u1 w1) fixnum-type?))
	 ((flonum-type? u1) (widen r (c:ceil (c:value t1 u1 w1)) flonum-type?))
	 ((rectangular-type? u1)
	  (newline-between
	   (if *type-checks?*
	       (c:if (c:!=0.0 (c:i (c:value t1 u1 w1)))
		     (compile-error "ceiling" y #f)
		     (c:noop)
		     #f)
	       (c:noop))
	   (widen r (c:ceil (c:r (c:value t1 u1 w1))) flonum-type?)))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "ceiling" y p?))))

(define-primitive-procedure truncate
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate!
  (lambda (w1)
   (when (can-be? fixnum-type? w1) (propagate-result! <fixnum>))
   (when (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
    (propagate-result! <flonum>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond ((fixnum-type? u1) (widen r (c:value t1 u1 w1) fixnum-type?))
	 ((flonum-type? u1)
	  (widen r
		 (c:?: (c:<0.0 (c:value t1 u1 w1))
		       (c:ceil (c:value t1 u1 w1))
		       (c:floor (c:value t1 u1 w1)))
		 flonum-type?))
	 ((rectangular-type? u1)
	  (newline-between (if *type-checks?*
			       (c:if (c:!=0.0 (c:i (c:value t1 u1 w1)))
				     (compile-error "truncate" y #f)
				     (c:noop)
				     #f)
			       (c:noop))
			   (widen r
				  (c:?: (c:<0.0 (c:r (c:value t1 u1 w1)))
					(c:ceil (c:r (c:value t1 u1 w1)))
					(c:floor (c:r (c:value t1 u1 w1))))
				  flonum-type?)))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "truncate" y p?))))

(define-primitive-procedure round
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate!
  (lambda (w1)
   (when (can-be? fixnum-type? w1) (propagate-result! <fixnum>))
   (when (or (can-be? flonum-type? w1) (can-be? rectangular-type? w1))
    (propagate-result! <flonum>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond ((fixnum-type? u1) (widen r (c:value t1 u1 w1) fixnum-type?))
	 ((flonum-type? u1) (widen r (c:rint (c:value t1 u1 w1)) flonum-type?))
	 ((rectangular-type? u1)
	  (newline-between
	   (if *type-checks?*
	       (c:if (c:!=0.0 (c:i (c:value t1 u1 w1)))
		     (compile-error "round" y #f)
		     (c:noop)
		     #f)
	       (c:noop))
	   (widen r (c:rint (c:r (c:value t1 u1 w1))) flonum-type?)))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "round" y p?))))

(define-primitive-procedure exp
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <flonum>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond
    ((nonrectangular-number-type? u1)
     (widen r (c:exp (c:value t1 u1 w1)) flonum-type?))
    ((rectangular-type? u1)
     (unimplemented y "EXP on rectangular arguments is not (yet) implemented"))
    (else (fuck-up))))
  (lambda (p?) (compile-error "exp" y p?))))

(define-primitive-procedure log
 ;; needs work: To handle negative, zero, +inf, and NaN arguments.
 ;; needs work: If this were implemented properly and supported rectangular
 ;;             results we would have two choices: either always return a
 ;;             rectangular result or return a union of a rectangular and a
 ;;             flonum. The former is undesirable since that would cause
 ;;             rectangular pollution. The later is undesirable since that
 ;;             would cause union type pollution and require run-time checking
 ;;             of whether the argument is negative. There is no way with the
 ;;             current type system to force the result of LOG to be a monotype
 ;;             flonum. Thus we currently violate R4RS in this regard.
 ;; note: Since LOG is not included in the table on the top of page 20 of R4RS
 ;;       it can return inexact results even when given exact arguments that
 ;;       could otherwise yield an exact result. This licenses never returning
 ;;       a fixnum.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <flonum>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond
    ((nonrectangular-number-type? u1)
     (widen r (c:log (c:value t1 u1 w1)) flonum-type?))
    ((rectangular-type? u1)
     (unimplemented y "LOG on rectangular arguments is not (yet) implemented"))
    (else (fuck-up))))
  (lambda (p?) (compile-error "log" y p?))))

(define-primitive-procedure sin
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <flonum>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond
    ((nonrectangular-number-type? u1)
     (widen r (c:sin (c:value t1 u1 w1)) flonum-type?))
    ((rectangular-type? u1)
     (unimplemented y "SIN on rectangular arguments is not (yet) implemented"))
    (else (fuck-up))))
  (lambda (p?) (compile-error "sin" y p?))))

(define-primitive-procedure cos
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <flonum>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond
    ((nonrectangular-number-type? u1)
     (widen r (c:cos (c:value t1 u1 w1)) flonum-type?))
    ((rectangular-type? u1)
     (unimplemented y "COS on rectangular arguments is not (yet) implemented"))
    (else (fuck-up))))
  (lambda (p?) (compile-error "cos" y p?))))

(define-primitive-procedure tan
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <flonum>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond
    ((nonrectangular-number-type? u1)
     (widen r (c:tan (c:value t1 u1 w1)) flonum-type?))
    ((rectangular-type? u1)
     (unimplemented y "TAN on rectangular arguments is not (yet) implemented"))
    (else (fuck-up))))
  (lambda (p?) (compile-error "tan" y p?))))

(define-primitive-procedure asin
 ;; needs work: To handle >1, <-1, and NaN arguments.
 ;; needs work: If this were implemented properly and supported rectangular
 ;;             results we would have two choices: either always return a
 ;;             rectangular result or return a union of a rectangular and a
 ;;             flonum. The former is undesirable since that would cause
 ;;             rectangular pollution. The later is undesirable since that
 ;;             would cause union type pollution and require run-time checking
 ;;             of whether the argument is <-1 or >1. There is no way with the
 ;;             current type system to force the result of ASIN to be a
 ;;             monotype flonum. Thus we currently violate R4RS in this regard.
 ;; note: Since ASIN is not included in the table on the top of page 20 of R4RS
 ;;       it can return inexact results even when given exact arguments that
 ;;       could otherwise yield an exact result. This licenses never returning
 ;;       a fixnum.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <flonum>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond ((nonrectangular-number-type? u1)
	  (widen r (c:asin (c:value t1 u1 w1)) flonum-type?))
	 ((rectangular-type? u1)
	  (unimplemented
	   y "ASIN on rectangular arguments is not (yet) implemented"))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "asin" y p?))))

(define-primitive-procedure acos
 ;; needs work: To handle >1, <-1, and NaN arguments.
 ;; needs work: If this were implemented properly and supported rectangular
 ;;             results we would have two choices: either always return a
 ;;             rectangular result or return a union of a rectangular and a
 ;;             flonum. The former is undesirable since that would cause
 ;;             rectangular pollution. The later is undesirable since that
 ;;             would cause union type pollution and require run-time checking
 ;;             of whether the argument is <-1 or >1. There is no way with the
 ;;             current type system to force the result of ACOS to be a
 ;;             monotype flonum. Thus we currently violate R4RS in this regard.
 ;; note: Since ACOS is not included in the table on the top of page 20 of R4RS
 ;;       it can return inexact results even when given exact arguments that
 ;;       could otherwise yield an exact result. This licenses never returning
 ;;       a fixnum.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <flonum>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond ((nonrectangular-number-type? u1)
	  (widen r (c:acos (c:value t1 u1 w1)) flonum-type?))
	 ((rectangular-type? u1)
	  (unimplemented
	   y "ACOS on rectangular arguments is not (yet) implemented"))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "acos" y p?))))

(define-primitive-procedure atan
 ;; needs work: To handle +inf, -inf, and NaN arguments.
 one-or-two-arguments-compatible?
 (one-or-two-arguments-truly-compatible? number-type? number-type?)
 (list number-type? number-type?)
 (list number-type? number-type?)
 (one-or-two-arguments-propagate!
  (lambda (w1) (propagate-result! <flonum>))
  (lambda (w1 w2) (propagate-result! <flonum>)))
 #f
 (if (= (length ws) 1)
     (type-switch
      number-type?
      w1
      r
      t1
      (lambda (u1)
       (cond ((nonrectangular-number-type? u1)
	      (widen r (c:atan (c:value t1 u1 w1)) flonum-type?))
	     ((rectangular-type? u1)
	      (unimplemented
	       y "ATAN on rectangular arguments is not (yet) implemented"))
	     (else (fuck-up))))
      (lambda (p?) (compile-error "atan1" y p?)))
     (type-switch
      number-type?
      w1
      r
      t1
      (lambda (u1)
       (cond
	((nonrectangular-number-type? u1)
	 (type-switch
	  number-type?
	  w2
	  r
	  t2
	  (lambda (u2)
	   (cond ((nonrectangular-number-type? u2)
		  (widen r
			 (c:atan2 (c:value t1 u1 w1) (c:value t2 u2 w2))
			 flonum-type?))
		 ((rectangular-type? u2)
		  (unimplemented
		   y "ATAN on rectangular arguments is not (yet) implemented"))
		 (else (fuck-up))))
	  (lambda (p?) (compile-error "atan3" y p?))))
	((rectangular-type? u1)
	 (unimplemented
	  y "ATAN on rectangular arguments is not (yet) implemented"))
	(else (fuck-up))))
      (lambda (p?) (compile-error "atan2" y p?)))))

(define-primitive-procedure sqrt
 ;; needs work: To handle negative, +inf, and NaN arguments.
 ;; needs work: If this were implemented properly and supported rectangular
 ;;             results we would have two choices: either always return a
 ;;             rectangular result or return a union of a rectangular and a
 ;;             flonum. The former is undesirable since that would cause
 ;;             rectangular pollution. The later is undesirable since that
 ;;             would cause union type pollution and require run-time checking
 ;;             of whether the argument is negative. There is no way with the
 ;;             current type system to force the result of SQRT to be a
 ;;             monotype flonum. Thus we currently violate R4RS in this regard.
 ;; note: Since SQRT is not included in the table on the top of page 20 of R4RS
 ;;       it can return inexact results even when given exact arguments that
 ;;       could otherwise yield an exact result. This licenses never returning
 ;;       a fixnum.
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <flonum>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond ((nonrectangular-number-type? u1)
	  (widen r (c:sqrt (c:value t1 u1 w1)) flonum-type?))
	 ((rectangular-type? u1)
	  (unimplemented
	   y "SQRT on rectangular arguments is not (yet) implemented"))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "sqrt" y p?))))

(define-primitive-procedure expt
 ;; needs work: To handle negative, zero, +inf, and NaN first argument.
 ;; needs work: To handle -inf, +inf, and NaN second argument.
 ;; needs work: If this were implemented properly and supported rectangular
 ;;             results we would have two choices: either always return a
 ;;             rectangular result or return a union of a rectangular, a
 ;;             flonum, and a fixnum. The former is undesirable since that
 ;;             would cause rectangular pollution. The later is undesirable
 ;;             since that would cause union type pollution and require
 ;;             run-time checking of whether the first argument is negative
 ;;             and whether the first argument is an integer raised to the
 ;;             second argument. There is no way with the current type system
 ;;             to force the result of EXPT to be a flonum or a fixnum. Thus
 ;;             we currently violate R4RS in this regard.
 ;; note: Unfortunately, EXPT is listed in the table on the top of page 20 of
 ;;       R4RS so it must return an exact result when given exact arguments
 ;;       that can produce an exact result. Because of this, the output can
 ;;       never be a fixnum monotype and must be a union of a fixnum and a
 ;;       flonum. Furthermore, there must be run-time checking when both
 ;;       arguments are fixnums. Bummer. I may change this someday to violate
 ;;       R4RS in this regard and have EXPT always return a flonum monotype and
 ;;       eliminate ipow().
 two-arguments-compatible?
 (two-arguments-truly-compatible? number-type? number-type?)
 (list number-type? number-type?)
 (list number-type? number-type?)
 (two-arguments-propagate!
  (lambda (w1 w2)
   (when (and (can-be? fixnum-type? w1) (can-be? fixnum-type? w2))
    (propagate-result! <fixnum>))
   (when (and (can-be? number-type? w1) (can-be? number-type? w2))
    (propagate-result! <flonum>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    number-type?
    w2
    r
    t2
    (lambda (u2)
     (cond
      ((and (fixnum-type? u1) (fixnum-type? u2))
       (c:if
	(c:<0 (c:value t2 u2 w2))
	(widen r (c:pow (c:value t1 u1 w1) (c:value t2 u2 w2)) flonum-type?)
	(widen r (c:ipow (c:value t1 u1 w1) (c:value t2 u2 w2)) fixnum-type?)
	#f))
      ((or (rectangular-type? u1) (rectangular-type? u2))
       (unimplemented
	y "EXPT on rectangular arguments is not (yet) implemented"))
      ((and (nonrectangular-number-type? u1) (nonrectangular-number-type? u2))
       (widen r (c:pow (c:value t1 u1 w1) (c:value t2 u2 w2)) flonum-type?))
      (else (fuck-up))))
    (lambda (p?) (compile-error "expt2" y p?))))
  (lambda (p?) (compile-error "expt1" y p?))))

(define-primitive-procedure exact->inexact
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate!
  (lambda (w1)
   (when (can-be? nonrectangular-number-type? w1)
    (propagate-result! <flonum>))
   (when (can-be? rectangular-type? w1) (propagate-result! <rectangular>))))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond
    ((fixnum-type? u1)
     (widen r (c:flonum-cast (c:value t1 u1 w1)) flonum-type?))
    ((flonum-type? u1) (widen r (c:value t1 u1 w1) flonum-type?))
    ((rectangular-type? u1) (widen r (c:value t1 u1 w1) rectangular-type?))
    (else (fuck-up))))
  (lambda (p?) (compile-error "exact_to_inexact" y p?))))

(define-primitive-procedure inexact->exact
 one-argument-compatible?
 (one-argument-truly-compatible? number-type?)
 (list number-type?)
 (list number-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <fixnum>)))
 #f
 (type-switch
  number-type?
  w1
  r
  t1
  (lambda (u1)
   (cond ((fixnum-type? u1) (widen r (c:value t1 u1 w1) fixnum-type?))
	 ((flonum-type? u1)
	  (widen r (c:fixnum-cast (c:value t1 u1 w1)) fixnum-type?))
	 ((rectangular-type? u1)
	  (newline-between
	   (c:if (c:!=0.0 (c:i (c:value t1 u1 w1)))
		 (compile-error "inexact_to_exact2" y #f)
		 (c:noop)
		 #f)
	   (widen r (c:fixnum-cast (c:r (c:value t1 u1 w1))) fixnum-type?)))
	 (else (fuck-up))))
  (lambda (p?) (compile-error "inexact_to_exact1" y p?))))

(define-primitive-procedure char?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list char-type?)
 (list (lambda (u) (not (char-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? char-type? w1) (can-be-non? char-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! char-type?)))
 #f
 (compile-type-predicate char-type?))

(define-primitive-procedure char->integer
 one-argument-compatible?
 (one-argument-truly-compatible? char-type?)
 (list char-type?)
 (list char-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <fixnum>)))
 #f
 (type-switch
  char-type?
  w1
  r
  t1
  (lambda (u1)
   (widen
    r (c:fixnum-cast (c:unsigned-char-cast (c:value t1 u1 w1))) fixnum-type?))
  (lambda (p?) (compile-error "char_to_integer" y p?))))

(define-primitive-procedure integer->char
 one-argument-compatible?
 (one-argument-truly-compatible? fixnum-type?)
 (list fixnum-type?)
 (list fixnum-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <char>)))
 #f
 (type-switch
  fixnum-type?
  w1
  r
  t1
  (lambda (u1)
   (newline-between
    (if *bounds-checks?*
	(c:if (c:boolean-or (c:<0 (c:value t1 u1 w1))
			    (c:>= (c:value t1 u1 w1) (c:256)))
	      (compile-error "integer_to_char2" y #f)
	      (c:noop)
	      #f)
	(c:noop))
    (widen r (c:unsigned-char-cast (c:value t1 u1 w1)) char-type?)))
  (lambda (p?) (compile-error "integer_to_char1" y p?))))

(define-primitive-procedure string?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list string-type?)
 (list (lambda (u) (not (string-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? string-type? w1) (can-be-non? string-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! string-type?)))
 #f
 (compile-type-predicate string-type?))

(define-primitive-procedure make-string
 one-or-two-arguments-compatible?
 (one-or-two-arguments-truly-compatible? fixnum-type? char-type?)
 (list fixnum-type? char-type?)
 (list fixnum-type? char-type?)
 (one-or-two-arguments-propagate!
  (lambda (w1) (propagate-result! (<string> (call-site-expression y))))
  (lambda (w1 w2) (propagate-result! (<string> (call-site-expression y)))))
 ;; needs work: This doesn't check that the size of the string is nonnegative.
 #f
 (cond
  ((discard? r)
   (type-switch
    fixnum-type?
    w1
    r
    t1
    (lambda (u1)
     (if (= (length ws) 2)
	 (type-switch char-type?
		      w2
		      r
		      t2
		      (lambda (u2) (c:noop))
		      (lambda (p?) (compile-error "make_string2" y p?)))
	 (c:noop)))
    (lambda (p?) (compile-error "make_string1" y p?))))
  ((antecedent? r)
   (type-switch
    fixnum-type?
    w1
    r
    t1
    (lambda (u1)
     (if (= (length ws) 2)
	 (type-switch char-type?
		      w2
		      r
		      t2
		      (lambda (u2) (return-true r))
		      (lambda (p?) (compile-error "make_string2" y p?)))
	 (return-true r)))
    (lambda (p?) (compile-error "make_string1" y p?))))
  ((and (return? r) (not (result-accessed? r)))
   (type-switch
    fixnum-type?
    w1
    r
    t1
    (lambda (u1)
     (if (= (length ws) 2)
	 (type-switch char-type?
		      w2
		      r
		      t2
		      (lambda (u2) (compile-return r))
		      (lambda (p?) (compile-error "make_string2" y p?)))
	 (compile-return r)))
    (lambda (p?) (compile-error "make_string1" y p?))))
  (else
   (let ((c (result-c r))
	 (w (result-type-set r)))
    (type-switch
     fixnum-type?
     w1
     r
     t1
     (lambda (u1)
      (if (= (length ws) 2)
	  (type-switch
	   char-type?
	   w2
	   r
	   t2
	   (lambda (u2)
	    (newline-between
	     (compile-allocate-string c w (c:value t1 u1 w1) y)
	     (let ((t (c:t *ti*)))
	      (set! *ti* (+ *ti* 1))
	      (outside-body
	       ;; needs work: To use code-generation abstractions.
	       (semicolon-after (space-between *char* (star-before t))))
	      (c:for (c:= t
			  (c:& (value-string-ref
				c (the-member-that string-type? w) w (c:0))))
		     (c:< t (c:& (value-string-ref
				  c
				  (the-member-that string-type? w)
				  w
				  (c:value t1 u1 w1))))
		     (c:++ t)
		     (c::= (c:* t) (c:value t2 u2 w2))))
	     (compile-return r)))
	   (lambda (p?) (compile-error "make_string2" y p?)))
	  ;; note: This is necessary since due to C bogosity, one can't
	  ;;       determine the length of an uninitialized string since it
	  ;;       might contain nulls.
	  (newline-between
	   (compile-allocate-string c w (c:value t1 u1 w1) y)
	   (let ((t (c:t *ti*)))
	    (set! *ti* (+ *ti* 1))
	    (outside-body
	     ;; needs work: To use code-generation abstractions.
	     (semicolon-after (space-between *char* (star-before t))))
	    (c:for (c:= t (c:& (value-string-ref
				c (the-member-that string-type? w) w (c:0))))
		   (c:< t (c:& (value-string-ref
				c
				(the-member-that string-type? w)
				w
				(c:value t1 u1 w1))))
		   (c:++ t)
		   (c::= (c:* t) (c:character #\space))))
	   (compile-return r))))
     (lambda (p?) (compile-error "make_string1" y p?)))))))

(define-primitive-procedure string
 zero-or-more-arguments-compatible?
 (all-arguments-truly-compatible? char-type?)
 (map-n (lambda (i) char-type?) n)
 (map-n (lambda (i) char-type?) n)
 (all-arguments-propagate!
  (lambda (ws) (propagate-result! (<string> (call-site-expression y)))))
 #f
 (let loop ((ts1 ts) (us '()) (ws1 ws))
  (if (null? ts1)
      (cond
       ((discard? r) (c:noop))
       ((antecedent? r) (return-true r))
       ((and (return? r) (not (result-accessed? r))) (compile-return r))
       (else (let ((c (result-c r))
		   (us (reverse us))
		   (w (result-type-set r)))
	      (newline-between
	       (compile-allocate-string c w (c:fixnum (length ws)) y)
	       (newlines-between
		(map-n
		 (lambda (i)
		  (c::= (value-string-ref
			 c (the-member-that string-type? w) w (c:fixnum i))
			(c:value (list-ref ts i)
				 (list-ref us i)
				 (list-ref ws i))))
		 (length ws)))
	       (compile-return r)))))
      (type-switch char-type?
		   (first ws1)
		   r
		   (first ts1)
		   (lambda (u1) (loop (rest ts1) (cons u1 us) (rest ws1)))
		   (lambda (p?) (compile-error "string" y p?))))))

(define-primitive-procedure string-length
 one-argument-compatible?
 (one-argument-truly-compatible? string-type?)
 (list string-type?)
 (list string-type?)
 (one-argument-propagate!
  (lambda (w1)
   (for-each-member (lambda (u1)
		     (when (string-type? u1)
		      (set-string-type-string-length-accessed?! u1 #t)))
		    w1)
   (propagate-result! <fixnum>)))
 #f
 (type-switch
  string-type?
  w1
  r
  t1
  (lambda (u1) (widen r (value-string-length t1 u1 w1) fixnum-type?))
  (lambda (p?) (compile-error "string_length" y p?))))

(define-primitive-procedure string-ref
 two-arguments-compatible?
 (two-arguments-truly-compatible? string-type? fixnum-type?)
 (list string-type? fixnum-type?)
 (list string-type? fixnum-type?)
 (two-arguments-propagate!
  (lambda (w1 w2)
   (for-each-member (lambda (u1)
		     (when (string-type? u1)
		      (set-string-type-string-ref-accessed?! u1 #t)))
		    w1)
   (propagate-result! <char>)))
 #f
 (type-switch
  string-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    fixnum-type?
    w2
    r
    t2
    (lambda (u2)
     (newline-between
      (if *bounds-checks?*
	  (c:if (c:boolean-or
		 (c:<0 (c:value t2 u2 w2))
		 (c:>= (c:value t2 u2 w2) (value-string-length t1 u1 w1)))
		(compile-error "string_ref3" y #f)
		(c:noop)
		#f)
	  (c:noop))
      (widen r (value-string-ref t1 u1 w1 (c:value t2 u2 w2)) char-type?)))
    (lambda (p?) (compile-error "string_ref2" y p?))))
  (lambda (p?) (compile-error "string_ref1" y p?))))

(define-primitive-procedure string-set!
 three-arguments-compatible?
 (three-arguments-truly-compatible? string-type? fixnum-type? char-type?)
 (list string-type? fixnum-type? char-type?)
 (list string-type? fixnum-type? char-type?)
 (three-arguments-propagate! (lambda (w1 w2 w3) #f))
 #f
 (type-switch
  string-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    fixnum-type?
    w2
    r
    t2
    (lambda (u2)
     (newline-between
      (if *bounds-checks?*
	  (c:if (c:boolean-or
		 (c:<0 (c:value t2 u2 w2))
		 (c:>= (c:value t2 u2 w2) (value-string-length t1 u1 w1)))
		(compile-error "string_set4" y #f)
		(c:noop)
		#f)
	  (c:noop))
      (type-switch char-type?
		   w3
		   r
		   t3
		   (lambda (u3)
		    (newline-between
		     (c::= (value-string-ref t1 u1 w1 (c:value t2 u2 w2))
			   (c:value t3 u3 w3))
		     (compile-return r)))
		   (lambda (p?) (compile-error "string_set3" y p?)))))
    (lambda (p?) (compile-error "string_set2" y p?))))
  (lambda (p?) (compile-error "string_set1" y p?))))

(define-primitive-procedure vector?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list vector-type?)
 (list (lambda (u) (not (vector-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? vector-type? w1) (can-be-non? vector-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! vector-type?)))
 #f
 (compile-type-predicate vector-type?))

(define-primitive-procedure make-vector
 one-or-two-arguments-compatible?
 (one-or-two-arguments-truly-compatible? fixnum-type? type?)
 (list fixnum-type? type?)
 (list fixnum-type? type?)
 (one-or-two-arguments-propagate!
  (lambda (w1)
   (propagate-result!
    (<headed-vector>
     ;; note: This is suboptimal since type propagation is not yet complete
     ;;       and APPLY-CLOSED-WORLD-ASSUMPTION! has not been done yet.
     '()
     (call-site-expression y))))
  (lambda (w1 w2)
   (propagate-result!
    (<headed-vector>
     ;; note: This is suboptimal since type propagation is not yet complete
     ;;       and APPLY-CLOSED-WORLD-ASSUMPTION! has not been done yet.
     (members w2)
     (call-site-expression y)))))
 (unless (or (discard? r)
	     (antecedent? r)
	     (and (return? r) (not (result-accessed? r))))
  (let* ((w (result-type-set r))
	 (u (the-member-that
	     (lambda (u)
	      (and (headed-vector-type? u)
		   (memq (call-site-expression y)
			 (headed-vector-type-allocating-expressions u))))
	     w)))
   (for-each-member
    (lambda (u1)
     (when (and (fixnum-type? u1)
		(= (length ws) 2)
		(not (fictitious? (headed-vector-type-element u))))
      (for-each-member
       (lambda (u2)
	(promote!
	 (create-accessor-result (headed-vector-type-element u) #f) w2 w2))
       w2)))
    w1)))
 ;; needs work: This doesn't check that the size of the vector is nonnegative.
 (cond
  ((discard? r)
   (type-switch fixnum-type?
		w1
		r
		t1
		(lambda (u1) (c:noop))
		(lambda (p?) (compile-error "make_vector" y p?))))
  ((antecedent? r)
   (type-switch fixnum-type?
		w1
		r
		t1
		(lambda (u1) (return-true r))
		(lambda (p?) (compile-error "make_vector" y p?))))
  ((and (return? r) (not (result-accessed? r)))
   (type-switch fixnum-type?
		w1
		r
		t1
		(lambda (u1) (compile-return r))
		(lambda (p?) (compile-error "make_vector" y p?))))
  (else
   (let* ((c (result-c r))
	  (w (result-type-set r))
	  (u (the-member-that
	      (lambda (u)
	       (and (headed-vector-type? u)
		    (memq (call-site-expression y)
			  (headed-vector-type-allocating-expressions u))))
	      w)))
    (type-switch
     fixnum-type?
     w1
     r
     t1
     (lambda (u1)
      (newline-between
       (compile-allocate-headed-vector c u w (c:value t1 u1 w1) y)
       (if (and (= (length ws) 2)
		(not (fictitious? (headed-vector-type-element u))))
	   (type-switch
	    type?
	    w2
	    r
	    t2
	    (lambda (u2)
	     (let ((t (c:t *ti*)))
	      (set! *ti* (+ *ti* 1))
	      (outside-body
	       ;; note: This violates the abstraction.
	       (list 'c:declaration
		     t
		     (semicolon-after
		      (c:type-set (headed-vector-type-element u)
				  (star-before t)))
		     #f))
	      (newline-between
	       (c:for (c:= t (c:& (value-vector-ref c u w (c:0))))
		      (c:< t (c:& (value-vector-ref c u w (c:value t1 u1 w1))))
		      (c:++ t)
		      (move (create-accessor-result
			     (headed-vector-type-element u)
			     (parentheses-around (star-before t)))
			    t2
			    w2))
	       (compile-return r))))
	    (lambda (p?) (fuck-up)))
	   (compile-return r))))
     (lambda (p?) (compile-error "make_vector" y p?)))))))

(define-primitive-procedure make-displaced-vector
 three-arguments-compatible?
 (three-arguments-truly-compatible? vector-type? fixnum-type? fixnum-type?)
 (list vector-type? fixnum-type? fixnum-type?)
 (list vector-type? fixnum-type? fixnum-type?)
 (three-arguments-propagate!
  (lambda (w1 w2 w3)
   (for-each-member
    (lambda (u1)
     (when (vector-type? u1) (propagate-result! (<displaced-vector> u1))))
    w1)))
 #f
 (let ((w (result-type-set r)))
  (type-switch
   vector-type?
   w1
   r
   t1
   (lambda (u1)
    (let ((u (the-member-that
	      (lambda (u)
	       (and (displaced-vector-type? u)
		    (eq? u (displaced-vector-type-displaced-vector-type u1))))
	      w)))
     (type-switch
      fixnum-type?
      w2
      r
      t2
      (lambda (u2)
       (type-switch
	fixnum-type?
	w3
	r
	t3
	(lambda (u3)
	 (newline-between
	  (if *bounds-checks?*
	      (c:if (c:boolean-or
		     (c:<0 (c:value t2 u2 w2))
		     (c:>= (c:value t2 u2 w2) (value-vector-length t1 u1 w1)))
		    (compile-error "make_displaced_vector4" y #f)
		    (c:noop)
		    #f)
	      (c:noop))
	  (cond (*bounds-checks?*
		 (when *overflow-checks?*
		  (unimplemented
		   y "Safe exact arithmetic is not (yet) implemented"))
		 (c:if (c:boolean-or
			(c:<0 (c:value t3 u3 w3))
			(c:> (c:+ (c:value t2 u2 w2) (c:value t3 u3 w3))
			     (value-vector-length t1 u1 w1)))
		       (compile-error "make_displaced_vector5" y #f)
		       (c:noop)
		       #f))
		(else (c:noop)))
	  (move-displaced-vector
	   r
	   u
	   (c:& (value-vector-ref t1 u1 w1 (c:value t2 u2 w2)))
	   (c:value t2 u3 w3))))
	(lambda (p?) (compile-error "make_displaced_vector3" y p?))))
      (lambda (p?) (compile-error "make_displaced_vector2" y p?)))))
   (lambda (p?) (compile-error "make_displaced_vector1" y p?)))))

(define-primitive-procedure vector
 zero-or-more-arguments-compatible?
 (all-arguments-truly-compatible? type?)
 (map-n (lambda (i) type?) n)
 (map-n (lambda (i) type?) n)
 (all-arguments-propagate!
  (lambda (ws)
   (propagate-result!
    (<headed-vector>
     ;; note: This is suboptimal since type propagation is not yet complete
     ;;       and APPLY-CLOSED-WORLD-ASSUMPTION! has not been done yet.
     (reduce unionq (map members ws) '())
     (call-site-expression y)))))
 (unless (or (discard? r)
	     (antecedent? r)
	     (and (return? r) (not (result-accessed? r))))
  (let* ((w (result-type-set r))
	 (u (the-member-that
	     (lambda (u)
	      (and (headed-vector-type? u)
		   (memq (call-site-expression y)
			 (headed-vector-type-allocating-expressions u))))
	     w)))
   (for-each (lambda (w)
	      (promote!
	       (if (degenerate-vector-type? u)
		   *discard*
		   (create-accessor-result (headed-vector-type-element u) #f))
	       w
	       w))
	     ws)))
 (cond
  ((discard? r) (c:noop))
  ((antecedent? r) (return-true r))
  ((and (return? r) (not (result-accessed? r))) (compile-return r))
  (else (let* ((c (result-c r))
	       (w (result-type-set r))
	       (u (the-member-that
		   (lambda (u)
		    (and (headed-vector-type? u)
			 (memq (call-site-expression y)
			       (headed-vector-type-allocating-expressions u))))
		   w)))
	 (newline-between
	  (compile-allocate-headed-vector c u w (c:fixnum (length ws)) y)
	  (newlines-between
	   (map-n (lambda (i)
		   (move (if (degenerate-vector-type? u)
			     *discard*
			     (create-accessor-result
			      (headed-vector-type-element u)
			      (value-vector-ref c u w (c:fixnum i))))
			 (list-ref ts i)
			 (list-ref ws i)))
		  (length ws)))
	  (compile-return r))))))

(define-primitive-procedure vector-length
 one-argument-compatible?
 (one-argument-truly-compatible? vector-type?)
 (list vector-type?)
 (list vector-type?)
 (one-argument-propagate!
  (lambda (w1)
   (for-each-member
    (lambda (u1)
     (cond ((headed-vector-type? u1)
	    (set-headed-vector-type-vector-length-accessed?! u1 #t))
	   ((nonheaded-vector-type? u1)
	    (set-nonheaded-vector-type-vector-length-accessed?! u1 #t))
	   ((displaced-vector-type? u1)
	    (set-displaced-vector-type-vector-length-accessed?! u1 #t))))
    w1)
   (propagate-result! <fixnum>)))
 #f
 (type-switch
  vector-type?
  w1
  r
  t1
  (lambda (u1) (widen r (value-vector-length t1 u1 w1) fixnum-type?))
  (lambda (p?) (compile-error "vector_length" y p?))))

(define-primitive-procedure vector-ref
 two-arguments-compatible?
 (two-arguments-truly-compatible? vector-type? fixnum-type?)
 (list vector-type? fixnum-type?)
 (list vector-type? fixnum-type?)
 (two-arguments-propagate!
  (lambda (w1 w2)
   (for-each-member
    (lambda (u1)
     (cond ((headed-vector-type? u1)
	    (set-headed-vector-type-vector-ref-accessed?! u1 #t))
	   ((nonheaded-vector-type? u1)
	    (set-nonheaded-vector-type-vector-ref-accessed?! u1 #t))
	   ((displaced-vector-type? u1)
	    (set-displaced-vector-type-vector-ref-accessed?! u1 #t))))
    w1)
   (for-each-member
    (lambda (u1)
     (when (vector-type? u1)
      (for-each-member propagate-result! (vector-type-element u1))))
    w1)))
 (for-each-member
  (lambda (u1)
   (when (vector-type? u1)
    (for-each-member
     (lambda (u2)
      (when (fixnum-type? u2)
       (promote! r (vector-type-element u1) (vector-type-element u1))))
     w2)))
  w1)
 (type-switch
  vector-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    fixnum-type?
    w2
    r
    t2
    (lambda (u2)
     (newline-between
      (if *bounds-checks?*
	  (c:if (c:boolean-or
		 (c:<0 (c:value t2 u2 w2))
		 (c:>= (c:value t2 u2 w2) (value-vector-length t1 u1 w1)))
		(compile-error "vector_ref3" y #f)
		(c:noop)
		#f)
	  (c:noop))
      (move r
	    (value-vector-ref t1 u1 w1 (c:value t2 u2 w2))
	    (vector-type-element u1))))
    (lambda (p?) (compile-error "vector_ref2" y p?))))
  (lambda (p?) (compile-error "vector_ref1" y p?))))

(define-primitive-procedure vector-set!
 three-arguments-compatible?
 (three-arguments-truly-compatible? vector-type? fixnum-type? type?)
 (list vector-type? fixnum-type? type?)
 (list vector-type? fixnum-type? type?)
 (three-arguments-propagate!
  (lambda (w1 w2 w3)
   (for-each-member
    (lambda (u1)
     (when (vector-type? u1) (assert-subset! w3 (vector-type-element u1))))
    w1)))
 (for-each-member
  (lambda (u1)
   (when (vector-type? u1)
    (for-each-member
     (lambda (u2)
      (when (fixnum-type? u2)
       (promote! (if (degenerate-vector-type? u1)
		     *discard*
		     (create-accessor-result (vector-type-element u1) #f))
		 w3
		 w3)))
     w2)))
  w1)
 (type-switch
  vector-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    fixnum-type?
    w2
    r
    t2
    (lambda (u2)
     (newline-between
      (if *bounds-checks?*
	  (c:if (c:boolean-or
		 (c:<0 (c:value t2 u2 w2))
		 (c:>= (c:value t2 u2 w2) (value-vector-length t1 u1 w1)))
		(compile-error "vector_set3" y #f)
		(c:noop)
		#f)
	  (c:noop))
      (move (if (degenerate-vector-type? u1)
		*discard*
		(create-accessor-result
		 (vector-type-element u1)
		 (value-vector-ref t1 u1 w1 (c:value t2 u2 w2))))
	    t3
	    w3)
      (compile-return r)))
    (lambda (p?) (compile-error "vector_set2" y p?))))
  (lambda (p?) (compile-error "vector_set1" y p?))))

(define-primitive-procedure procedure?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list procedure-type?)
 (list (lambda (u) (not (procedure-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? procedure-type? w1) (can-be-non? procedure-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! procedure-type?)))
 #f
 (compile-type-predicate procedure-type?))

(define-primitive-procedure apply
 two-or-more-arguments-compatible?
 ;; needs work: To abstract.
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (and (can-be? (truly-compatible-procedure?
		 (if (converted? y)
		     (cons w0 (but-last (rest ws)))
		     (but-last (rest ws)))
		 (last ws)
		 (recreate-call-site y 'first-argument))
		(first ws))
       (can-be? list-type? (last ws))))
 ;; needs work: This could be made more precise.
 (cons procedure-type?
       (append (map-n (lambda (i) type?) (- n 2)) (list list-type?)))
 ;; needs work: This could be made more precise.
 (cons procedure-type?
       (append (map-n (lambda (i) type?) (- n 2)) (list list-type?)))
 ;; needs work: To abstract.
 (lambda (ws w)
  (when (can-be-non? null-type? w) (fuck-up))
  (when #f				;debugging
   (when (can-be? continuation-type? (first ws))
    (unimplemented y "Passing continuations as the first argument of APPLY is not (yet) implemented")))
  (propagate-call!
   (recreate-call-site y 'first-argument)
   (first ws)
   (if (converted? y) (cons w0 (rest (but-last ws))) (rest (but-last ws)))
   (last ws)))
 #f
 (if (void? w1)
     (compile-error "void_call" y #t)
     (type-switch
      (compatible-procedure?
       (if (converted? y) (cons w0 (but-last (rest ws))) (but-last (rest ws)))
       (last ws)
       (recreate-call-site y 'first-argument))
      w1
      r
      t1
      (lambda (u1)
       (if (converted? y)
	   (compile-converted-call
	    r (recreate-call-site y 'first-argument) t1 u1 w1
	    (cons t0 (but-last (rest ts))) (cons w0 (but-last (rest ws)))
	    (last ts) (last ws))
	   (compile-call r (recreate-call-site y 'first-argument)
			 t1 u1 w1 t0 w0
			 (but-last (rest ts)) (but-last (rest ws))
			 (last ts) (last ws))))
      (lambda (p?) (compile-error "call" y p?)))))

(define-primitive-procedure call-with-current-continuation
 one-argument-compatible?
 (one-argument-truly-compatible?
  (truly-compatible-procedure?
   (if (converted? y)
       (list w0 w0)
       (list (create-anonymous-type-set
	      (<continuation> (call-site-expression y)))))
   *null*
   (recreate-call-site y 'first-argument)))
 (list (truly-compatible-procedure?
	(if (converted? y)
	    (list w0 w0)
	    (list (create-anonymous-type-set
		   (<continuation> (call-site-expression y)))))
	*null*
	(recreate-call-site y 'first-argument)))
 (list (truly-compatible-procedure?
	(if (converted? y)
	    (list w0 w0)
	    (list (create-anonymous-type-set
		   (<continuation> (call-site-expression y)))))
	*null*
	(recreate-call-site y 'first-argument)))
 (one-argument-propagate!
  (lambda (w1)
   (when #f				;debugging
    (when (can-be? continuation-type? w1)
     (unimplemented y "Passing continuations as the first argument of CALL-WITH-CURRENT-CONTINUATION is not (yet) implemented")))
   (propagate-call! (recreate-call-site y 'first-argument)
		    w1
		    (if (converted? y)
			(list w0 w0)
			(list (create-anonymous-type-set
			       (<continuation> (call-site-expression y)))))
		    *null*)))
 (let ((w (result-type-set r)))
  (when (converted? y) (fuck-up))
  (newline-between
   (when (can-be?
	  (lambda (u1)
	   (and
	    (native-procedure-type? u1)
	    ((truly-compatible-procedure?
	      (if (converted? y)
		  (list w0 w0)
		  (list (create-anonymous-type-set
			 (<continuation> (call-site-expression y)))))
	      *null*
	      (recreate-call-site y 'first-argument))
	     u1)
	    (some (lambda (e)
		   (can-be?
		    (lambda (u4)
		     (and (eq? u4 (<continuation> (call-site-expression y)))
			  (some (lambda (y) (not (goto? y u4)))
				(continuation-type-call-sites u4))))
		    (first-parameter-type-set (environment-expression e))))
		  (narrow-clones u1))))
	  w1)
    (for-each-member
     (lambda (u1)
      (when ((compatible-procedure?
	      (list (create-anonymous-type-set
		     (<continuation> (call-site-expression y))))
	      *null*
	      (recreate-call-site y 'first-argument))
	     u1)
       (let ((w3 (create-anonymous-type-set
		  (<continuation> (call-site-expression y)))))
	(promote-call!
	 (if (or (discard? r) (fictitious? w))
	     *discard*
	     (create-accessor-result w (c:v (call-site-expression y))))
	 (recreate-call-site y 'first-argument) u1 (list w3) *null*)
	(promote! r w w))))
     w1))))
 (let ((w (result-type-set r)))
  (when (converted? y) (fuck-up))
  (newline-between
   (cond
    ((can-be?
      (lambda (u1)
       (and (native-procedure-type? u1)
	    ((truly-compatible-procedure?
	      (if (converted? y)
		  (list w0 w0)
		  (list (create-anonymous-type-set
			 (<continuation> (call-site-expression y)))))
	      *null*
	      (recreate-call-site y 'first-argument))
	     u1)
	    (some (lambda (e)
		   (can-be?
		    (lambda (u4)
		     (and (eq? u4 (<continuation> (call-site-expression y)))
			  (some (lambda (y) (not (goto? y u4)))
				(continuation-type-call-sites u4))))
		    (first-parameter-type-set (environment-expression e))))
		  (narrow-clones u1))))
      w1)
     (include! "setjmp")		;jmp_buf
     (unless (or (discard? r) (fictitious? w))
      (outside-main (c:declaration w (c:v (call-site-expression y)) (c:noop))))
     ;; needs work: To use code-generation abstractions.
     (outside-body (semicolon-after
		    (space-between *jmpbuf* (c:j (call-site-expression y)))))
     (type-switch
      (compatible-procedure? (list (create-anonymous-type-set
				    (<continuation> (call-site-expression y))))
			     *null*
			     (recreate-call-site y 'first-argument))
      w1
      r
      t1
      (lambda (u1)
       (let* ((w3 (create-anonymous-type-set
		   (<continuation> (call-site-expression y))))
	      (t3 (allocate-temporary w3)))
	(newline-between
	 (c:if (c:setjmp (c:j (call-site-expression y)))
	       (c:noop)
	       (newline-between
		(widen (create-accessor-result w3 t3)
		       (c:& (c:j (call-site-expression y)))
		       (lambda (u)
			(and (continuation-type? u)
			     (eq? (continuation-type-allocating-expression u)
				  (call-site-expression y)))))
		(compile-call
		 (if (or (discard? r) (fictitious? w))
		     *discard*
		     (create-accessor-result w (c:v (call-site-expression y))))
		 (recreate-call-site y 'first-argument)
		 t1 u1 w1 t0 w0 (list t3) (list w3) 'void23 *null*))
	       #t)
	 (if (or (discard? r) (fictitious? w))
	     (compile-return r)
	     (move r (c:v (call-site-expression y)) w)))))
      (lambda (p?) (compile-error "call_with_current_continuation" y p?))))
    (else
     (type-switch
      (compatible-procedure? (list (create-anonymous-type-set
				    (<continuation> (call-site-expression y))))
			     *null*
			     (recreate-call-site y 'first-argument))
      w1
      r
      t1
      (lambda (u1)
       (let* ((w3 (create-anonymous-type-set
		   (<continuation> (call-site-expression y))))
	      (t3 (allocate-temporary w3)))
	(compile-call r (recreate-call-site y 'first-argument)
		      t1 u1 w1 t0 w0 (list t3) (list w3) 'void24 *null*)))
      (lambda (p?) (compile-error "call_with_current_continuation" y p?)))))
   (if (can-be?
	(lambda (u1)
	 (and (native-procedure-type? u1)
	      ((truly-compatible-procedure?
		(if (converted? y)
		    (list w0 w0)
		    (list (create-anonymous-type-set
			   (<continuation> (call-site-expression y)))))
		*null*
		(recreate-call-site y 'first-argument))
	       u1)
	      (some (lambda (e)
		     (can-be?
		      (lambda (u4)
		       (and (eq? u4 (<continuation> (call-site-expression y)))
			    (some (lambda (y) (goto? y u4))
				  (continuation-type-call-sites u4))))
		      (first-parameter-type-set (environment-expression e))))
		    (narrow-clones u1))))
	w1)
       (c:: (c:x (call-site-expression y)))
       (c:noop)))))

(define-primitive-procedure input-port?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list input-port-type?)
 (list (lambda (u) (not (input-port-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? input-port-type? w1) (can-be-non? input-port-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! input-port-type?)))
 #f
 (compile-type-predicate input-port-type?))

(define-primitive-procedure output-port?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list output-port-type?)
 (list (lambda (u) (not (output-port-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? output-port-type? w1)
	      (can-be-non? output-port-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! output-port-type?)))
 #f
 (compile-type-predicate output-port-type?))

(define-primitive-procedure open-input-file
 one-argument-compatible?
 (one-argument-truly-compatible? string-type?)
 (list string-type?)
 (list string-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <input-port>)))
 #f
 (type-switch
  string-type?
  w1
  r
  t1
  (lambda (u1)
   (if *runtime-checks?*
       (if (or (discard? r)
	       (antecedent? r)
	       (and (return? r) (not (result-accessed? r))))
	   (let ((t (allocate-temporary *input-port*)))
	    (newline-between
	     (widen (create-accessor-result *input-port* t)
		    (c:fopen (c:value t1 u1 w1) (c:string "r"))
		    input-port-type?)
	     (c:if (c:==null
		    (c:value t (the-member *input-port*) *input-port*))
		   (compile-error "open_input_file2" y #f)
		   (c:noop)
		   #f)
	     (compile-return r)))
	   (newline-between
	    (widen (unreturnify r)
		   (c:fopen (c:value t1 u1 w1) (c:string "r"))
		   input-port-type?)
	    (c:if (c:==null (c:value (result-c r)
				     (the-member-that input-port-type?
						      (result-type-set r))
				     (result-type-set r)))
		  (compile-error "open_input_file2" y #f)
		  (c:noop)
		  #f)
	    (compile-return r)))
       (widen r (c:fopen (c:value t1 u1 w1) (c:string "r")) input-port-type?)))
  (lambda (p?) (compile-error "open_input_file1" y p?))))

(define-primitive-procedure open-output-file
 one-argument-compatible?
 (one-argument-truly-compatible? string-type?)
 (list string-type?)
 (list string-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <output-port>)))
 #f
 (type-switch
  string-type?
  w1
  r
  t1
  (lambda (u1)
   (if *runtime-checks?*
       (if (or (discard? r)
	       (antecedent? r)
	       (and (return? r) (not (result-accessed? r))))
	   (let ((t (allocate-temporary *output-port*)))
	    (newline-between
	     (widen (create-accessor-result *output-port* t)
		    (c:fopen (c:value t1 u1 w1) (c:string "w"))
		    output-port-type?)
	     (c:if (c:==null
		    (c:value t (the-member *output-port*) *output-port*))
		   (compile-error "open_output_file2" y #f)
		   (c:noop)
		   #f)
	     (compile-return r)))
	   (newline-between
	    (widen (unreturnify r)
		   (c:fopen (c:value t1 u1 w1) (c:string "w"))
		   output-port-type?)
	    (c:if (c:==null (c:value (result-c r)
				     (the-member-that output-port-type?
						      (result-type-set r))
				     (result-type-set r)))
		  (compile-error "open_output_file2" y #f)
		  (c:noop)
		  #f)
	    (compile-return r)))
       (widen
	r (c:fopen (c:value t1 u1 w1) (c:string "w")) output-port-type?)))
  (lambda (p?) (compile-error "open_output_file1" y p?))))

(define-primitive-procedure close-input-port
 one-argument-compatible?
 (one-argument-truly-compatible? input-port-type?)
 (list input-port-type?)
 (list input-port-type?)
 (one-argument-propagate! (lambda (w1) #f))
 #f
 (type-switch input-port-type?
	      w1
	      r
	      t1
	      (lambda (u1)
	       (if *runtime-checks?*
		   (c:if (c:fclose (c:value t1 u1 w1))
			 (compile-error "close_input_port2" y #f)
			 (compile-return r)
			 #t)
		   (newline-between
		    (semicolon-after (c:fclose (c:value t1 u1 w1)))
		    (compile-return r))))
	      (lambda (p?) (compile-error "close_input_port1" y p?))))

(define-primitive-procedure close-output-port
 one-argument-compatible?
 (one-argument-truly-compatible? output-port-type?)
 (list output-port-type?)
 (list output-port-type?)
 (one-argument-propagate! (lambda (w1) #f))
 #f
 (type-switch output-port-type?
	      w1
	      r
	      t1
	      (lambda (u1)
	       (if *runtime-checks?*
		   (c:if (c:fclose (c:value t1 u1 w1))
			 (compile-error "close_output_port2" y #f)
			 (compile-return r)
			 #t)
		   (newline-between
		    (semicolon-after (c:fclose (c:value t1 u1 w1)))
		    (compile-return r))))
	      (lambda (p?) (compile-error "close_output_port1" y p?))))

(define-primitive-procedure read-char1
 one-argument-compatible?
 (one-argument-truly-compatible? input-port-type?)
 (list input-port-type?)
 (list input-port-type?)
 (one-argument-propagate! (lambda (w1)
			   (propagate-result! <char>)
			   (propagate-result! <eof-object>)))
 #f
 (type-switch input-port-type?
	      w1
	      r
	      t1
	      (lambda (u1)
	       (c:if (c:==eof (c:= (c:c) (c:getc (c:value t1 u1 w1))))
		     (widen r 'void25 eof-object-type?)
		     (widen r (c:c) char-type?)
		     #t))
	      (lambda (p?) (compile-error "read_char1" y p?))))

(define-primitive-procedure peek-char1
 one-argument-compatible?
 (one-argument-truly-compatible? input-port-type?)
 (list input-port-type?)
 (list input-port-type?)
 (one-argument-propagate! (lambda (w1)
			   (propagate-result! <char>)
			   (propagate-result! <eof-object>)))
 #f
 (type-switch input-port-type?
	      w1
	      r
	      t1
	      (lambda (u1)
	       (c:if (c:==eof (c:= (c:c) (c:ungetc (c:getc (c:value t1 u1 w1))
						   (c:value t1 u1 w1))))
		     (widen r 'void26 eof-object-type?)
		     (widen r (c:c) char-type?)
		     #t))
	      (lambda (p?) (compile-error "peek_char1" y p?))))

(define-primitive-procedure eof-object?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list eof-object-type?)
 (list (lambda (u) (not (eof-object-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? eof-object-type? w1) (can-be-non? eof-object-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! eof-object-type?)))
 #f
 (compile-type-predicate eof-object-type?))

(define-primitive-procedure char-ready?1
 one-argument-compatible?
 (one-argument-truly-compatible? input-port-type?)
 (list input-port-type?)
 (list input-port-type?)
 (one-argument-propagate! (lambda (w1)
			   (propagate-result! <true>)
			   (propagate-result! <false>)))
 #f
 (type-switch
  input-port-type?
  w1
  r
  t1
  (lambda (u1) (compile-test r (c:input-waiting (c:value t1 u1 w1))))
  (lambda (p?) (compile-error "char_ready1" y p?))))

(define-primitive-procedure standard-input-port
 zero-arguments-compatible?
 (zero-arguments-truly-compatible?)
 '()
 '()
 (zero-arguments-propagate! (lambda () (propagate-result! <input-port>)))
 #f
 (widen r (c:stdin) input-port-type?))

(define-primitive-procedure standard-output-port
 zero-arguments-compatible?
 (zero-arguments-truly-compatible?)
 '()
 '()
 (zero-arguments-propagate! (lambda () (propagate-result! <output-port>)))
 #f
 (widen r (c:stdout) output-port-type?))

(define-primitive-procedure write-char2
 two-arguments-compatible?
 (two-arguments-truly-compatible? char-type? output-port-type?)
 (list char-type? output-port-type?)
 (list char-type? output-port-type?)
 (two-arguments-propagate! (lambda (w1 w2) #f))
 #f
 (type-switch
  char-type?
  w1
  r
  t1
  (lambda (u1)
   (type-switch
    output-port-type?
    w2
    r
    t2
    (lambda (u2)
     (newline-between (c:putc (c:value t1 u1 w1) (c:value t2 u2 w2))
		      (compile-return r)))
    (lambda (p?) (compile-error "write_char2" y p?))))
  (lambda (p?) (compile-error "write_char1" y p?))))

(define-primitive-procedure panic
 one-argument-compatible?
 (one-argument-truly-compatible? string-type?)
 (list string-type?)
 (list string-type?)
 (one-argument-propagate! (lambda (w1) #f))
 #f
 (type-switch string-type?
	      w1
	      r
	      t1
	      (lambda (u1) (c:panic (c:value t1 u1 w1)))
	      (lambda (p?) (compile-error "panic" y p?))))

(define-primitive-procedure pointer?
 one-argument-compatible?
 (one-argument-truly-compatible? type?)
 (list pointer-type?)
 (list (lambda (u) (not (pointer-type? u))))
 (one-argument-propagate!
  (lambda (w1)
   (when (and (can-be? pointer-type? w1) (can-be-non? pointer-type? w1))
    (for-each-member (lambda (u1) (set-type-type-tag-accessed?! u1 #t)) w1))
   (propagate-type-predicate! pointer-type?)))
 #f
 (compile-type-predicate pointer-type?))

(define-primitive-procedure integer->string
 one-argument-compatible?
 (one-argument-truly-compatible? fixnum-type?)
 (list fixnum-type?)
 (list fixnum-type?)
 (one-argument-propagate!
  (lambda (w1) (propagate-result! <nonreclaimable-string>)))
 #f
 (type-switch
  fixnum-type?
  w1
  r
  t1
  (lambda (u1) (widen r (c:char*-cast (c:value t1 u1 w1)) string-type?))
  (lambda (p?) (compile-error "integer_to_string" y p?))))

(define-primitive-procedure integer->input-port
 one-argument-compatible?
 (one-argument-truly-compatible? fixnum-type?)
 (list fixnum-type?)
 (list fixnum-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <input-port>)))
 #f
 (type-switch
  fixnum-type?
  w1
  r
  t1
  (lambda (u1) (widen r (c:file*-cast (c:value t1 u1 w1)) input-port-type?))
  (lambda (p?) (compile-error "integer_to_input_port" y p?))))

(define-primitive-procedure integer->output-port
 one-argument-compatible?
 (one-argument-truly-compatible? fixnum-type?)
 (list fixnum-type?)
 (list fixnum-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <output-port>)))
 #f
 (type-switch
  fixnum-type?
  w1
  r
  t1
  (lambda (u1) (widen r (c:file*-cast (c:value t1 u1 w1)) output-port-type?))
  (lambda (p?) (compile-error "integer_to_output_port" y p?))))

(define-primitive-procedure integer->pointer
 one-argument-compatible?
 (one-argument-truly-compatible? fixnum-type?)
 (list fixnum-type?)
 (list fixnum-type?)
 (one-argument-propagate! (lambda (w1) (propagate-result! <pointer>)))
 #f
 (type-switch
  fixnum-type?
  w1
  r
  t1
  (lambda (u1) (widen r (c:void*-cast (c:value t1 u1 w1)) pointer-type?))
  (lambda (p?) (compile-error "integer_to_pointer" y p?))))

(define-primitive-procedure clocks-per-second
 zero-arguments-compatible?
 (zero-arguments-truly-compatible?)
 '()
 '()
 (zero-arguments-propagate! (lambda () (propagate-result! <fixnum>)))
 #f
 (widen r (c:clocks-per-second) fixnum-type?))

(define-primitive-procedure rand-max
 zero-arguments-compatible?
 (zero-arguments-truly-compatible?)
 '()
 '()
 (zero-arguments-propagate! (lambda () (propagate-result! <fixnum>)))
 #f
 (widen r (c:rand-max) fixnum-type?))

(define-primitive-procedure pointer-size
 zero-arguments-compatible?
 (zero-arguments-truly-compatible?)
 '()
 '()
 (zero-arguments-propagate! (lambda () (propagate-result! <fixnum>)))
 #f
 (widen r (c:pointer-size) fixnum-type?))

(define-primitive-procedure infinity?
 one-argument-compatible?
 (one-argument-truly-compatible? flonum-type?)
 (list flonum-type?)
 (list flonum-type?)
 (one-argument-propagate! (lambda (w1)
			   (propagate-result! <true>)
			   (propagate-result! <false>)))
 #f
 (type-switch flonum-type?
	      w1
	      r
	      t1
	      (lambda (u1) (compile-test r (c:==infinity (c:value t1 u1 w1))))
	      (lambda (p?) (compile-error "infinity" y p?))))

(define-primitive-procedure fork
 ;; note: FORK will fail miserably if you escape by calling a continuation.
 two-arguments-compatible?
 (two-arguments-truly-compatible?
  (truly-compatible-procedure?
   (if (converted? y) (list w0) '())
   *null*
   (recreate-call-site y 'first-argument))
  (truly-compatible-procedure?
   (if (converted? y) (list w0) '())
   *null*
   (recreate-call-site y 'second-argument)))
 (list (truly-compatible-procedure?
	(if (converted? y) (list w0) '())
	*null*
	(recreate-call-site y 'first-argument))
       (truly-compatible-procedure?
	(if (converted? y) (list w0) '())
	*null*
	(recreate-call-site y 'second-argument)))
 (list (truly-compatible-procedure?
	(if (converted? y) (list w0) '())
	*null*
	(recreate-call-site y 'first-argument))
       (truly-compatible-procedure?
	(if (converted? y) (list w0) '())
	*null*
	(recreate-call-site y 'second-argument)))
 (two-arguments-propagate!
  (lambda (w1 w2)
   (propagate-call! (recreate-call-site y 'first-argument)
		    w1
		    (if (converted? y) (list w0) '())
		    *null*)
   (propagate-call! (recreate-call-site y 'second-argument)
		    w2
		    (if (converted? y) (list w0) '())
		    *null*)))
 ;; needs work: I can't remember what promotors do and thus can't figure out
 ;;             if FORK needs one and, if so, how to write it.
 #f
 ;; needs work: To use code-generation abstractions.
 (begin
  (set! *program-has-pthreads?* #t)
  (include! "pthread")			;pthread_t pthread_create pthread_join
  (when (converted? y)
   (unimplemented y "Converted calls to FORK are not (yet) implemented"))
  (braces-around
   (newline-between
    (semicolon-after "pthread_t thread")
    ;; note: This uses GNU C internal functions.
    "void *branch(void *ignore)"
    (braces-around
     (newline-between
      (if (void? w1)
	  (compile-error "void_call" y #t)
	  (type-switch
	   (compatible-procedure?
	    '() *null* (recreate-call-site y 'first-argument))
	   w1
	   (unreturnify r)
	   t1
	   (lambda (u1)
	    (compile-call
	     (unreturnify r) (recreate-call-site y 'first-argument)
	     t1 u1 w1 t0 w0 '() '() #f *null*))
	   (lambda (p?) (compile-error "call" y p?))))
      (semicolon-after "return NULL")))
    (semicolon-after "pthread_create(&thread, NULL, &branch, NULL)")
    (if (void? w2)
	(compile-error "void_call" y #t)
	(type-switch
	 (compatible-procedure?
	  '() *null* (recreate-call-site y 'second-argument))
	 w2
	 (unreturnify r)
	 t2
	 (lambda (u2)
	  (compile-call (unreturnify r) (recreate-call-site y 'second-argument)
			t2 u2 w2 t0 w0 '() '() #f *null*))
	 (lambda (p?) (compile-error "call" y p?))))
    (semicolon-after "pthread_join(thread, NULL)")
    (compile-return r)))))

(define-primitive-procedure mutex
 ;; note: MUTEX will not unlock the mutex if you escape by calling a
 ;;       continuation.
 one-argument-compatible?
 (one-argument-truly-compatible?
  (truly-compatible-procedure?
   (if (converted? y) (list w0) '())
   *null*
   (recreate-call-site y 'first-argument)))
 (list (truly-compatible-procedure?
	(if (converted? y) (list w0) '())
	*null*
	(recreate-call-site y 'first-argument)))
 (list (truly-compatible-procedure?
	(if (converted? y) (list w0) '())
	*null*
	(recreate-call-site y 'first-argument)))
 (one-argument-propagate!
  (lambda (w1)
   (propagate-call! (recreate-call-site y 'first-argument)
		    w1
		    (if (converted? y) (list w0) '())
		    *null*)))
 ;; needs work: I can't remember what promotors do and thus can't figure out
 ;;             if MUTEX needs one and, if so, how to write it.
 #f
 ;; needs work: To use code-generation abstractions.
 (begin
  (set! *program-has-pthreads?* #t)
  ;; pthread_mutex_t pthread_mutex_lock pthread_mutex_unlock
  ;; PTHREAD_MUTEX_INITIALIZER
  (include! "pthread")
  (when (converted? y)
   (unimplemented y "Converted calls to MUTEX are not (yet) implemented"))
  (outside-main (semicolon-after (space-between
				  "pthread_mutex_t"
				  (c:mutex (call-site-expression y))
				  "="
				  "PTHREAD_MUTEX_INITIALIZER")))
  (newline-between
   ;; needs work: Can use PTHREAD_MUTEX_INITIALIZER to make a fast mutex when
   ;;             this expression isn't in a recursive path
   ;;             in the call graph.
   ;; needs work: For now, under Linux PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
   ;;             is broken.
   (c:gosub "pthread_mutex_lock"
	    (c:& (c:mutex (call-site-expression y))))
   (if (void? w1)
       (compile-error "void_call" y #t)
       (type-switch (compatible-procedure?
		     '() *null* (recreate-call-site y 'first-argument))
		    w1
		    r
		    t1
		    (lambda (u1)
		     (compile-call r (recreate-call-site y 'first-argument)
				   t1 u1 w1 t0 w0 '() '() #f *null*))
		    (lambda (p?) (compile-error "call" y p?))))
   (c:gosub "pthread_mutex_unlock" (c:& (c:mutex (call-site-expression y))))
   (compile-return r))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
