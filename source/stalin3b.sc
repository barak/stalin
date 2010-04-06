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
(module stalin3b)

(include "QobiScheme.sch")
(include "stalin3b.sch")
;;; End delete for Trotsky

;;; Determine directly escaping types

(define (mark-referencing-environments-proper-callees! u)
 (define (mark-proper-callees! e)
  ;; The PROPERLY-CALLS? relation is not necessarily reflexive.
  (define (loop e)
   (unless (environment-marked1? e)
    (set-environment-marked1?! e #t)
    (for-each loop (environment-direct-tail-callees e))
    (for-each loop (environment-direct-non-tail-callees e))))
  (for-each loop (environment-direct-tail-callees e))
  (for-each loop (environment-direct-non-tail-callees e)))
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
  (let outer ((u/w u))
   (cond ((type? u/w)
	  (unless (type-marked? u/w)
	   (set-type-marked?! u/w #t)
	   (for-each outer (types-and-type-sets-that-directly-point-to u/w))))
	 ((type-set? u/w)
	  (unless (type-set-marked? u/w)
	   (set-type-set-marked?! u/w #t)
	   (cond
	    ((expression? (type-set-location u/w))
	     (when (reached? (type-set-location u/w))
	      (mark-proper-callees!
	       (expression-environment (type-set-location u/w)))))
	    ((variable? (type-set-location u/w))
	     (when (and (accessed? (type-set-location u/w))
			(not (necessarily-fictitious? u/w)))
	      (let inner ((e (variable-environment (type-set-location u/w))))
	       (when (memq (type-set-location u/w) (free-variables e))
		(outer (environment-type e)))
	       (for-each
		(lambda (x)
		 (case (expression-kind x)
		  ((lambda converted-lambda converted-continuation)
		   (when (environment-used? (expression-lambda-environment x))
		    (inner (expression-lambda-environment x))))))
		(environment-expressions e)))))
	    ((type? (type-set-location u/w)) (outer (type-set-location u/w)))
	    ((eq? (type-set-location u/w) #f) #f)
	    (else (fuck-up)))))
	 (else (fuck-up))))))

(define (for-each-marked-caller p e)
 (let loop ((e e))
  (when (environment-marked1? e)
   (set-environment-marked1?! e #f)
   (p e)
   (for-each loop (environment-direct-tail-callers e))
   (for-each loop (environment-direct-non-tail-callers e)))))

(define (important? u)
 (and (or (native-procedure-type? u)
	  (continuation-type? u)
	  (string-type? u)
	  (structure-type? u)
	  (headed-vector-type? u)
	  (nonheaded-vector-type? u))
      (not (necessarily-fictitious? u))))

(define (important-marked-types)
 (remove-if
  necessarily-fictitious?
  (append
   (remove-if-not native-procedure-type-marked? *native-procedure-types*)
   (remove-if-not continuation-type-marked? *continuation-types*)
   (remove-if-not string-type-marked? *string-types*)
   (remove-if-not structure-type-marked? *structure-types*)
   (remove-if-not headed-vector-type-marked? *headed-vector-types*)
   (remove-if-not nonheaded-vector-type-marked? *nonheaded-vector-types*))))

(define (determine-escaping-types!)
 (for-each (lambda (e) (set-environment-escaping-types! e (unspecified))) *es*)
 (for-each (lambda (e)
	    (when (environment-used? e)
	     (set-environment-escaping-types! e '())
	     ;; Nothing escapes the top-level environment.
	     (unless (empty? (parent e))
	      (clock-sample)		;To prevent overflow.
	      (unmark-types!)
	      ;; This is done just for side effect, to set the MARKED? bits.
	      (for-each-pointed-to-type (lambda (u) #f) (return-type-set e))
	      (set-environment-escaping-types! e (important-marked-types)))))
	   *es*)
 (for-each
  (lambda (g)
   (when (and (accessed? g)
	      (assigned? g)
	      ;; This tries to state that G must be hidden thus the assignment
	      ;; X is trivial and should be ignored.
	      ;; needs work: But G will not be hidden if it is global, its
	      ;;             hidden native-procedure type is fictitious, or
	      ;;             some access causes its environment to be
	      ;;             nonfictitious.
	      (not (and (monomorphic? (variable-type-set g))
			(native-procedure-type?
			 (the-member (variable-type-set g)))
			(called? (the-member (variable-type-set g)))
			(not (noop? (the-member (variable-type-set g))))
			(every
			 (lambda (e)
			  ;; This is a weaker condition than used by
			  ;; DETERMINE-WHETHER-HIDDEN? so G might actually
			  ;; turn out to be hidden.
			  (let loop ((e1 (variable-environment g)))
			   (and (not (empty? e1))
				(or (eq? e1 (parent e)) (loop (parent e1))))))
			 (narrow-clones (the-member (variable-type-set g))))))
	      (begin
	       (unmark-types!)
	       (some (lambda (x)
		      (some-pointed-to-type
		       important? (expression-type-set (expression-source x))))
		     (assignments g))))
    (clock-sample)			;To prevent overflow.
    (unmark-types-and-type-sets!)
    (for-each (lambda (e) (set-environment-marked1?! e #f)) *es*)
    (for-each
     (lambda (x1)
      (let loop ((e (expression-environment x1)))
       (unless (eq? e (variable-environment g))
	(mark-referencing-environments-proper-callees! (environment-type e))
	(loop (parent e)))))
     (accesses g))
    (for-each
     (lambda (x)
      (when (executed? x)
       (clock-sample)			;To prevent overflow.
       ;; If E points to U1 then it points to U since U1 points to U. But not
       ;; vice versa. It could be that a caller of E points to U but not U1
       ;; because that caller is also a callee that is passed U as an argument
       ;; but not passed U1. Such cases do not count as escaping. Even so, this
       ;; is still suboptimal because a caller of E could point to U1 only by
       ;; virtue of that caller also being a callee that is called with U1 as
       ;; an argument.
       (unmark-types!)
       ;; This is done just for side effect, to set the MARKED? bits.
       (for-each-pointed-to-type
	(lambda (u) #f) (expression-type-set (expression-source x)))
       (let ((us (important-marked-types)))
	(unless (null? us)
	 (for-each-marked-caller
	  (lambda (e)
	   (set-environment-escaping-types!
	    e (unionq (environment-escaping-types e) us)))
	  (expression-environment x))))))
     (assignments g))))
  *gs*)
 (for-each
  (lambda (x)
   (when (and (executed? x)
	      (can-be? (lambda (u1)
			(and (continuation-type? u1)
			     ((truly-compatible-call? x) u1)))
		       (expression-type-set (expression-callee x))))
    (clock-sample)			;To prevent overflow.
    (unmark-types!)
    ;; This is done just for side effect, to set the MARKED? bits.
    (for-each-pointed-to-type
     (lambda (u) #f) (expression-type-set (first-argument x)))
    (let ((us (important-marked-types)))
     (unless (null? us)
      (for-each
       (lambda (e)
	(when (can-be?
	       (lambda (u1)
		(and (continuation-type? u1)
		     ((truly-compatible-call? x) u1)
		     ;; This checks that the call to the continuation can
		     ;; actually escape E by checking that the creator of the
		     ;; continuation can be a caller of E. This is suboptimal
		     ;; because the creator of the continuation could also be a
		     ;; callee of E. If the creator is not a callee then
		     ;; calling the continuation must escape E. But if the
		     ;; creator is also a callee then calling the continuation
		     ;; might or might not escape E.
		     (can-be?
		      (lambda (u2)
		       (and (native-procedure-type? u2)
			    (some (lambda (e1) (properly-calls? e1 e))
				  (narrow-clones u2))))
		      (expression-type-set
		       (first-argument
			(continuation-type-allocating-expression u1))))))
	       (expression-type-set (expression-callee x)))
	 (set-environment-escaping-types!
	  e (unionq (environment-escaping-types e) us))))
       (callers (expression-environment x)))))))
  ;; needs work: Doesn't handle implicit continuation calls.
  *calls*)
 (for-each
  (lambda (u2)
   (unmark-types!)
   (when (some
	  (lambda (x)
	   (and (executed? x)
		(can-be?
		 (lambda (u1)
		  (and ((primitive-procedure-type-named? 'structure-set!) u1)
		       ((structure-type-named?
			 (first (primitive-procedure-type-arguments u1))) u2)
		       ((truly-compatible-call? x) u1)))
		 (expression-type-set (expression-callee x)))
		(member? u2 (expression-type-set (first-argument x)))
		(some-pointed-to-type
		 important? (expression-type-set (second-argument x)))))
	  ;; note: Because of eta expansion there can be no implicit calls to
	  ;;       STRUCTURE-SET!.
	  *calls*)
    (clock-sample)			;To prevent overflow.
    (unmark-types-and-type-sets!)
    (for-each (lambda (e) (set-environment-marked1?! e #f)) *es*)
    (mark-referencing-environments-proper-callees! u2)
    (for-each
     (lambda (x)
      (when (and (executed? x)
		 (can-be?
		  (lambda (u1)
		   (and ((primitive-procedure-type-named? 'structure-set!) u1)
			((structure-type-named?
			  (first (primitive-procedure-type-arguments u1))) u2)
			((truly-compatible-call? x) u1)))
		  (expression-type-set (expression-callee x)))
		 (member? u2 (expression-type-set (first-argument x))))
       (clock-sample)			;To prevent overflow.
       ;; If E points to U2 then it points to U since U2 points to U. But not
       ;; vice versa. It could be that a caller of E points to U but not U2
       ;; because that caller is also a callee that is passed U as an argument
       ;; but not passed U2. Such cases do not count as escaping. Even so, this
       ;; is still suboptimal because a caller of E could point to U2 only by
       ;; virtue of that caller also being a callee that is called with U2 as
       ;; an argument.
       (unmark-types!)
       ;; This is done just for side effect, to set the MARKED? bits.
       (for-each-pointed-to-type
	(lambda (u) #f) (expression-type-set (second-argument x)))
       (let ((us (important-marked-types)))
	(unless (null? us)
	 (for-each-marked-caller
	  (lambda (e)
	   (set-environment-escaping-types!
	    e (unionq (environment-escaping-types e) us)))
	  (expression-environment x))))))
     ;; note: Because of eta expansion there can be no implicit calls to
     ;;       STRUCTURE-SET!.
     *calls*)))
  *structure-types*)
 (for-each
  (lambda (u2)
   (unmark-types!)
   (when (some
	  (lambda (x)
	   (and (executed? x)
		(can-be?
		 (lambda (u1)
		  (and ((primitive-procedure-type-named? 'vector-set!) u1)
		       ((truly-compatible-call? x) u1)))
		 (expression-type-set (expression-callee x)))
		(member? u2 (expression-type-set (first-argument x)))
		(some-pointed-to-type
		 important? (expression-type-set (third-argument x)))))
	  ;; note: Because of eta expansion there can be no implicit calls to
	  ;;       VECTOR-SET!.
	  *calls*)
    (clock-sample)			;To prevent overflow.
    (unmark-types-and-type-sets!)
    (for-each (lambda (e) (set-environment-marked1?! e #f)) *es*)
    (mark-referencing-environments-proper-callees! u2)
    (for-each
     (lambda (x)
      (when (and (executed? x)
		 (can-be?
		  (lambda (u1)
		   (and ((primitive-procedure-type-named? 'vector-set!) u1)
			((truly-compatible-call? x) u1)))
		  (expression-type-set (expression-callee x)))
		 (member? u2 (expression-type-set (first-argument x))))
       (clock-sample)			;To prevent overflow.
       ;; If E points to U2 then it points to U since U2 points to U. But not
       ;; vice versa. It could be that a caller of E points to U but not U2
       ;; because that caller is also a callee that is passed U as an argument
       ;; but not passed U2. Such cases do not count as escaping. Even so, this
       ;; is still suboptimal because a caller of E could point to U2 only by
       ;; virtue of that caller also being a callee that is called with U2 as
       ;; an argument.
       (unmark-types!)
       ;; This is done just for side effect, to set the MARKED? bits.
       (for-each-pointed-to-type
	(lambda (u) #f) (expression-type-set (third-argument x)))
       (let ((us (important-marked-types)))
	(unless (null? us)
	 (for-each-marked-caller
	  (lambda (e)
	   (set-environment-escaping-types!
	    e (unionq (environment-escaping-types e) us)))
	  (expression-environment x))))))
     ;; note: Because of eta expansion there can be no implicit calls to
     ;;       VECTOR-SET!.
     *calls*)))
  (append
   *headed-vector-types* *nonheaded-vector-types* *displaced-vector-types*)))

;;; Determine which environments have unique call sites

(define (determine-which-environments-have-unique-call-sites!)
 (for-each (lambda (e)
	    (when (environment-used? e)
	     (set-environment-non-self-tail-call-sites! e (call-sites e))))
	   *es*)
 (let loop ()
  (set! *again?* #f)
  (infer-all-unique-call-site!)
  (when *again?* (loop))))

;;; Determine which environments are recursive

(define (determine-which-environments-are-recursive!)
 (for-each (lambda (e)
	    (when (environment-used? e)
	     (set-environment-recursive?! e (properly-calls? e e))))
	   *es*))

;;; Determine which environments are reentrant

(define (determine-which-environments-are-reentrant!)
 ;; note: In principle, this can be (NON-TAIL-CALLS? E E) but in practise it
 ;;       can't because tail merging is done only on self tail calls.
 (for-each
  (lambda (e)
   (when (environment-used? e)
    (clock-sample)			;To prevent overflow.
    ;; This is done just for side effect, to set the MARKED2? bits.
    (some-proper-callee
     (lambda (e) #f) environment-marked2? set-environment-marked2?! e)
    (set-environment-reentrant?!
     e
     (some
      (lambda (e1)
       (and (environment-used? e1)
	    (not (unique-call-site? e1))
	    (some (lambda (y)
		   (and (not (top-level-call-site? y))
			(environment-marked2?
			 (expression-environment (call-site-expression y)))
			(not (can-be-self-tail-call-to? y e1))))
		  (call-sites e1))))
      (proper-callers e)))))
  *es*))

;;; Assert uniqueness

(define (assert-uniqueness!)
 ;; This is a special case for when the type set members are sorted.
 (define (set-equalq? us1 us2)
  (or (and (null? us1) (null? us2))
      (and (not (null? us1))
	   (not (null? us2))
	   (eq? (first us1) (first us2))
	   (set-equalq? (rest us1) (rest us2)))))
 ;; This is a special case for when the type set members are sorted.
 (define (unionq us1 us2)
  (cond ((null? us1) us2)
	((null? us2) us1)
	((eq? (first us1) (first us2))
	 (cons (first us1) (unionq (rest us1) (rest us2))))
	((< (type-index (first us1)) (type-index (first us2)))
	 (cons (first us1) (unionq (rest us1) us2)))
	(else (cons (first us2) (unionq us1 (rest us2))))))
 ;; We no longer need to sort the members of W because this is done by the
 ;; red-black trees.
 (let loop ()
  (let ((again? #f))
   (for-each
    (lambda (v)
     ;; conventions: V
     (let ((uss (map list (remove-if-not (structure-type-named? v)
					 *structure-types*))))
      (for-each
       (lambda (w)
	(let ((us (members-that (structure-type-named? v) w)))
	 (unless (or (null? us) (null? (rest us)))
	  (do ((us us (rest us))) ((null? (rest us)))
	   (let ((us1 (find-if (lambda (us0) (memq (first us) us0)) uss))
		 (us2 (find-if (lambda (us0) (memq (second us) us0)) uss)))
	    (unless (eq? us1 us2)
	     (set! uss
		   (cons (append us1 us2)
			 (removeq us1 (removeq us2 uss))))))))))
       *ws*)
      (for-each
       (lambda (us)
	(let ((uss (map-n
		    (lambda (i)
		     (reduce unionq
			     (map (lambda (u)
				   (members
				    (list-ref (structure-type-slots u) i)))
				  us)
			     '()))
		    (length (structure-type-slots (first us))))))
	 (for-each (lambda (u)
		    (for-each (lambda (w us)
			       (unless (set-equalq? (members w) us)
				(set-members! w us)
				(set! again? #t)))
			      (structure-type-slots u)
			      uss))
		   us)))
       uss)))
    (remove-duplicates (map structure-type-name *structure-types*)))
   (let ((uss (map list *headed-vector-types*)))
    (for-each
     (lambda (w)
      (let ((us (members-that headed-vector-type? w)))
       (unless (or (null? us) (null? (rest us)))
	(do ((us us (rest us)))	((null? (rest us)))
	 (let ((us1 (find-if (lambda (us0) (memq (first us) us0)) uss))
	       (us2 (find-if (lambda (us0) (memq (second us) us0)) uss)))
	  (unless (eq? us1 us2)
	   (set! uss
		 (cons (append us1 us2) (removeq us1 (removeq us2 uss))))))))))
     *ws*)
    (for-each
     (lambda (us)
      (let ((us1 (reduce
		  unionq
		  (map (lambda (u) (members (headed-vector-type-element u)))
		       us)
		  '())))
       (for-each
	(lambda (u)
	 (unless (set-equalq? (members (headed-vector-type-element u)) us1)
	  (set-members! (headed-vector-type-element u) us1)
	  (set! again? #t)))
	us)))
     uss))
   (let ((uss (map list *nonheaded-vector-types*)))
    (for-each
     (lambda (w)
      (let ((us (members-that nonheaded-vector-type? w)))
       (unless (or (null? us) (null? (rest us)))
	(do ((us us (rest us))) ((null? (rest us)))
	 (let ((us1 (find-if (lambda (us0) (memq (first us) us0)) uss))
	       (us2 (find-if (lambda (us0) (memq (second us) us0)) uss)))
	  (unless (eq? us1 us2)
	   (set! uss
		 (cons (append us1 us2) (removeq us1 (removeq us2 uss))))))))))
     *ws*)
    (for-each
     (lambda (us)
      (let ((us1 (reduce
		  unionq
		  (map (lambda (u) (members (nonheaded-vector-type-element u)))
		       us)
		  '())))
       (for-each
	(lambda (u)
	 (unless (set-equalq? (members (nonheaded-vector-type-element u)) us1)
	  (set-members! (nonheaded-vector-type-element u) us1)
	  (set! again? #t)))
	us)))
     uss))
   (when again? (loop)))))

;;; Perform lightweight closure conversion

(define (perform-lightweight-closure-conversion!)
 (for-each (lambda (u) (set-native-procedure-type-fictitious?! u #t))
	   *native-procedure-types*)
 (for-each (lambda (u) (set-continuation-type-fictitious?! u #t))
	   *continuation-types*)
 (for-each (lambda (u) (set-structure-type-fictitious?! u #t))
	   *structure-types*)
 (for-each (lambda (w) (set-type-set-fictitious?! w #t)) *ws*)
 (for-each (lambda (g)
	    (set-variable-local?! g #f)
	    (set-variable-global?! g #f)
	    (set-variable-hidden?! g #f)
	    (set-variable-slotted?! g #f))
	   *gs*)
 (for-each (lambda (e)
	    (set-environment-ancestors! e (unspecified))
	    (set-environment-has-closure?! e #f))
	   *es*)
 (for-each (lambda (e)
	    (when (environment-used? e) (set-environment-ancestors! e '())))
	   *es*)
 (for-each (lambda (e2)
	    (when (environment-used? e2)
	     (let loop ((e1 (parent e2)))
	      (if (or (empty? e1) (some accessed? (variables e1)))
		  (set-environment-quick-parent! e2 e1)
		  (loop (parent e1))))))
	   *es*)
 (let loop ()
  (clock-sample)			;To prevent overflow.
  (set! *again?* #f)
  ;; The order in which the following inferences are made should only
  ;; affect compilation time and not soundness or the code produced.
  (infer-all-whether-type-fictitious?! #f)
  (infer-all-whether-type-set-fictitious?! #f)
  (infer-all-whether-local?! #f)
  (infer-all-whether-global?! #f)
  (infer-all-whether-hidden?! #f)
  (infer-all-whether-slotted?! #f)
  (infer-all-whether-ancestor?! #f)
  (infer-all-whether-has-closure?! #f)
  (when *again?* (loop)))
 (clock-sample)				;To prevent overflow.
 (set! *again?* #f)
 ;; The order in which the following inferences are made should only
 ;; affect compilation time and not soundness or the code produced.
 (infer-all-whether-type-fictitious?! #t)
 (infer-all-whether-type-set-fictitious?! #t)
 (infer-all-whether-local?! #t)
 (infer-all-whether-global?! #t)
 (infer-all-whether-hidden?! #t)
 (infer-all-whether-slotted?! #t)
 (infer-all-whether-ancestor?! #t)
 (infer-all-whether-has-closure?! #t)
 (when *again?* (fuck-up)))

;;; Determine parents

(define (determine-parents!)
 (for-each (lambda (e)
	    (set-environment-parent-parameter! e (unspecified))
	    (set-environment-parent-slot! e (unspecified))
	    (set-environment-descendents! e '())
	    (set-environment-properly-in-lined-environments! e '()))
	   *es*)
 ;; needs work: This can be made faster.
 (for-each
  (lambda (e1)
   (when (environment-used? e1)
    (clock-sample)			;To prevent overflow.
    (for-each (lambda (e2)
	       (when (environment-used? e2)
		(set-environment-descendents! e2 (cons e1 (descendents e2)))))
	      (ancestors e1))))
  *es*)
 (when (eq? *closure-representation* 'linked)
  ;; It used to be possible for two different narrow clones to have different
  ;; parent parameters. This was discovered with the matrix.sc example of
  ;; jbs@quiotix.com. This created problems when applying PARENT-PARAMETER to
  ;; a type instead of an environment and also caused generation of incorrect
  ;; code where one backchain was accessed as the backchain of a narrow clone.
  ;; Now we take the most-nested parent parameter of all the narrow clones.
  ;; This might cause some procedures to have a parent parameter that is used
  ;; only to indirect through a parent slot and not to access other slots (i.e.
  ;; reducing the amount of parent-parameter compression). So it goes.
  (for-each (lambda (e)
	     (when (environment-used? e)
	      (clock-sample)		;To prevent overflow.
	      (set-environment-parent-parameter!
	       (narrow-prototype e)
	       (let ((es (reduce unionq
				 (map ancestors
				      (remove-if-not environment-used?
						     (narrow-clones e)))
				 '())))
		(if (null? es) #f (minp nested-in? es))))))
	    *es*)
  (let loop ((x *x*))
   (define (update x)
    (reduce
     unionq
     (map
      (lambda (e)
       (cond
	((or (noop? e) (not (environment-used? e)))
	 (set-environment-parent-slot! e #f)
	 '())
	(else
	 (let ((es (removeq
		    e (loop (expression-body (environment-expression e))))))
	  (set-environment-parent-slot!
	   e
	   (if (and (has-closure? e) (not (null? es)))
	       (minp nested-in? es)
	       #f))
	  (unionq es
		  (if (has-parent-parameter? e)
		      (removeq (parent-parameter e) (ancestors e))
		      '()))))))
      (narrow-clones (expression-lambda-environment x)))
     '()))
   (case (expression-kind x)
    ((null-constant) '())
    ((true-constant) '())
    ((false-constant) '())
    ((char-constant) '())
    ((fixnum-constant) '())
    ((flonum-constant) '())
    ((rectangular-constant) '())
    ((string-constant) '())
    ((symbol-constant) '())
    ((pair-constant) '())
    ((vector-constant) '())
    ((lambda) (update x))
    ((converted-lambda) (update x))
    ((converted-continuation) (update x))
    ((set!) (loop (expression-source x)))
    ((if) (unionq (loop (expression-antecedent x))
		  (unionq (loop (expression-consequent x))
			  (loop (expression-alternate x)))))
    ((primitive-procedure) '())
    ((foreign-procedure) '())
    ((access) '())
    ((call) (unionq (loop (expression-callee x))
		    (reduce unionq (map loop (expression-arguments x)) '())))
    ((converted-call)
     (unionq (loop (expression-callee x))
	     (reduce unionq (map loop (expression-arguments x)) '())))
    (else (fuck-up))))
  (for-each
   (lambda (e)
    (when (environment-used? e)
     (clock-sample)			;To prevent overflow.
     (unless (and (pairwise? (lambda (e1 e2)
			      (or (not (environment-used? e1))
				  (not (environment-used? e2))
				  (and (eq? (has-parent-parameter? e1)
					    (has-parent-parameter? e2))
				       (eq? (parent-parameter e1)
					    (parent-parameter e2)))))
			     (narrow-clones e))
		  (eq? (has-parent-parameter? e)
		       (environment? (parent-parameter e)))
		  (or (not (has-parent-slot? e))
		      (and (has-parent-parameter? e) (has-closure? e))))
      (fuck-up))))
   *es*))
 (for-each (lambda (e)
	    (when (environment-used? e)
	     (clock-sample)		;To prevent overflow.
	     (let loop ((e1 e))
	      (unless (eq? e e1)
	       (set-environment-properly-in-lined-environments!
		e1 (cons e (environment-properly-in-lined-environments e1))))
	      (when (unique-call-site? e1)
	       (loop (expression-environment
		      (call-site-expression (unique-call-site e1))))))))
	   *es*))

;;; Determine which expressions need conversion to CPS

(define (escapes-expression? u x)
 ;; debugging: This is a temporary kludge to handle the NEPLS pyth benchmark.
 ;;            It is unsound because it doesn't check for escapes by way of
 ;;            continuation calls, STRUCTURE-SET!, and VECTOR-SET!. And you
 ;;            really want to check that the continuation is allocated by some
 ;;            expression called by X. This is only a kludge to make the NEPLS
 ;;            pyth benchmark work.
 (or #t
     ;; This handles escaping by returning.
     (points-to? (expression-type-set x) u)
     ;; This handles escaping by SET!.
     (some (lambda (g)
	    (and (nested-in? (expression-environment x)
			     (variable-environment g))
		 (points-to? (variable-type-set g) u)))
	   *gs*)))

(define (determine-which-expressions-need-conversion-to-CPS!)
 (define (some-subexpression-calls? x x1)
  (let loop ((x x))
   (and
    (reached? x)
    (case (expression-kind x)
     ((set!) (loop (expression-source x)))
     ((if) (or (loop (expression-antecedent x))
	       (loop (expression-consequent x))
	       (loop (expression-alternate x))))
     ;; needs work: Doesn't handle implicit call sites.
     ((call converted-call)
      (or
       (eq? x x1)
       (can-be?
	(lambda (u)
	 (and
	  ((truly-compatible-call? x) u)
	  (or
	   (and (native-procedure-type? u)
		(calls? (callee-environment u (create-call-site x))
			(expression-environment x1)))
	   (and ((primitive-procedure-type-named? 'apply) u)
		(can-be?
		 (lambda (u)
		  (and (native-procedure-type? u)
		       ((truly-compatible-call-via-apply? x) u)
		       (calls?
			(callee-environment
			 u
			 (recreate-call-site
			  (create-call-site x) 'first-argument))
			(expression-environment x1))))
		 (expression-type-set (first-argument x))))
	   (and
	    ((primitive-procedure-type-named?
	      'call-with-current-continuation)
	     u)
	    (can-be?
	     (lambda (u)
	      (and
	       (native-procedure-type? u)
	       ((truly-compatible-call-via-call-with-current-continuation? x)
		u)
	       (calls?
		(callee-environment
		 u
		 (recreate-call-site
		  (create-call-site x) 'first-argument))
		(expression-environment x1))))
	     (expression-type-set (first-argument x))))
	   (and ((primitive-procedure-type-named? 'fork) u)
		(or (can-be?
		     (lambda (u)
		      (and (native-procedure-type? u)
			   ((truly-compatible-call-via-fork1? x) u)
			   (calls?
			    (callee-environment
			     u
			     (recreate-call-site
			      (create-call-site x) 'first-argument))
			    (expression-environment x1))))
		     (expression-type-set (first-argument x)))
		    (can-be?
		     (lambda (u)
		      (and (native-procedure-type? u)
			   ((truly-compatible-call-via-fork2? x) u)
			   (calls?
			    (callee-environment
			     u
			     (recreate-call-site
			      (create-call-site x) 'second-argument))
			    (expression-environment x1))))
		     (expression-type-set (second-argument x)))))
	   (and ((primitive-procedure-type-named? 'mutex) u)
		(can-be?
		 (lambda (u)
		  (and (native-procedure-type? u)
		       ((truly-compatible-call-via-mutex? x) u)
		       (calls?
			(callee-environment
			 u
			 (recreate-call-site
			  (create-call-site x) 'first-argument))
			(expression-environment x1))))
		 (expression-type-set (first-argument x)))))))
	(expression-type-set (expression-callee x)))
       (loop (expression-callee x))
       (some loop (expression-arguments x))))
     (else #f)))))
 ;; XS are all the nonconverted calls to CALL-WITH-CURRENT-CONTINUATION where
 ;; the continuation created escapes the call site. For each X in XS, the
 ;; corresponding entry in XSS is the set of all calls to the continuation
 ;; created by X.
 (let* ((xs (remove-if-not
	     (lambda (x)
	      (and
	       (executed? x)
	       (not (converted? x))
	       (can-be?
		(lambda (u)
		 (and
		  ((primitive-procedure-type-named?
		    'call-with-current-continuation)
		   u)
		  ((truly-compatible-call? x) u)
		  (can-be?
		   (lambda (u)
		    (and
		     (native-procedure-type? u)
		     ((truly-compatible-procedure?
		       (list (create-anonymous-type-set (<continuation> x)))
		       *null*
		       (recreate-call-site
			(create-call-site x) 'first-argument))
		      u)
		     (escapes? (<continuation> x)
			       (callee-environment
				u
				(recreate-call-site
				 (create-call-site x) 'first-argument)))))
		   (expression-type-set (first-argument x)))))
		(expression-type-set (expression-callee x)))))
	     ;; note: Because of eta expansion there can be no implicit calls
	     ;;       to CALL-WITH-CURRENT-CONTINUATION.
	     *calls*))
	(xss (map (lambda (x)
		   (remove-if-not
		    (lambda (x1)
		     (and
		      (executed? x1)
		      (can-be?
		       (lambda (u)
			(and
			 (continuation-type? u)
			 ((truly-compatible-call? x1) u)
			 (eq? (continuation-type-allocating-expression u) x)))
		       (expression-type-set (expression-callee x1)))))
		    ;; needs work: Doesn't handle implicit continuation calls.
		    *calls*))
		  xs)))
  (define (needs-conversion-to-CPS? x)
   (and (some
	 (lambda (x1 xs)
	  (and (some-subexpression-calls? x x1)
	       (some (lambda (x2) (control-flows? (after x) (before x2))) xs)))
	 xs
	 xss)
	(some (lambda (u) (escapes-expression? u x)) *continuation-types*)))
  (for-each
   (lambda (x)
    (set-expression-needs-conversion-to-CPS?! x (needs-conversion-to-CPS? x))
    (set-expression-needs-stop-conversion-to-CPS?!
     x
     (and (not (needs-conversion-to-CPS? x))
	  (case (expression-kind x)
	   ((set!) (needs-conversion-to-CPS? (expression-source x)))
	   ((if) (or (needs-conversion-to-CPS? (expression-antecedent x))
		     (needs-conversion-to-CPS? (expression-consequent x))
		     (needs-conversion-to-CPS? (expression-alternate x))))
	   ((call converted-call)
	    (or (needs-conversion-to-CPS? (expression-callee x))
		(some needs-conversion-to-CPS? (expression-arguments x))
		(can-be?
		 (lambda (u)
		  (and
		   (native-procedure-type? u)
		   ((truly-compatible-call? x) u)
		   (not (noop? (callee-environment u (create-call-site x))))
		   (needs-conversion-to-CPS?
		    (expression-body
		     (environment-expression
		      (callee-environment u (create-call-site x)))))))
		 (expression-type-set (expression-callee x)))))
	   (else #f)))))
   *xs*)))

;;; Convert to CPS

;;; Needs work: To convert a call to APPLY or CALL-WITH-CURRENT-CONTINUATION
;;;             to CPS when its procedure argument is a converted native
;;;             procedure.

(define (maybe-create-access-expression g/x)
 ;; needs work: Should give S/X argument so that it has a file position.
 (if (variable? g/x) (create-access-expression #f g/x) g/x))

(define (call-continuation g/x x)
 ;; needs work: Should give S/X argument so that it has a file position.
 (create-call-expression #f (maybe-create-access-expression g/x) (list x)))

(define (string->variable string)
 (create-variable (create-anonymous-s-expression (gensym string))))

(define (convert-to-CPS g/x x)
 (cond
  ((or (expression-needs-conversion-to-CPS? x)
       (expression-needs-stop-conversion-to-CPS? x))
   (case (expression-kind x)
    ((null-constant) (fuck-up))
    ((true-constant) (fuck-up))
    ((false-constant) (fuck-up))
    ((char-constant) (fuck-up))
    ((fixnum-constant) (fuck-up))
    ((flonum-constant) (fuck-up))
    ((rectangular-constant) (fuck-up))
    ((string-constant) (fuck-up))
    ((symbol-constant) (fuck-up))
    ((pair-constant) (fuck-up))
    ((vector-constant) (fuck-up))
    ((lambda) (fuck-up))
    ((converted-lambda converted-continuation) (fuck-up))
    ((set!)
     ;; [(SET! g x)]_g/x --> [x]_(LAMBDA (g1) (g/x (SET! g g1)))
     (unless (or (expression-needs-conversion-to-CPS? (expression-source x))
		 (expression-needs-stop-conversion-to-CPS?
		  (expression-source x)))
      (fuck-up))
     (let ((g1 (string->variable "x")))
      (convert-to-CPS
       (create-converted-continuation-expression
	x
	;; needs work: To give the environment a name.
	(create-environment #f "continuation")
	(list g1)
	(call-continuation
	 g/x
	 (create-set!-expression
	  x (expression-variable x) (create-access-expression x g1))))
       (expression-source x))))
    ((if)
     (cond
      ((or (expression-needs-conversion-to-CPS? (expression-antecedent x))
	   (expression-needs-stop-conversion-to-CPS?
	    (expression-antecedent x)))
       ;; [(IF x2 x3 x4)]_c --> [x2]_(LAMBDA (g1) (IF g1 [x3]_c [x4]_c))
       ;; [(IF x2 x3 x4)]_c -->
       ;; ((LAMBDA (g2) [x2]_(LAMBDA (g1) (IF g1 [x3]_g2 [x4]_g2))) c)
       (let ((g1 (string->variable "x")))
	(if (expression? g/x)
	    (let ((g2 (string->variable "x")))
	     (create-call-expression
	      x
	      (create-converted-continuation-expression
	       x
	       ;; needs work: To give the environment a name.
	       (create-environment #f "continuation")
	       (list g2)
	       (convert-to-CPS (create-converted-continuation-expression
				x
				;; needs work: To give the environment a name.
				(create-environment #f "continuation")
				(list g1)
				(create-if-expression
				 x
				 (create-access-expression x g1)
				 (convert-to-CPS g2 (expression-consequent x))
				 (convert-to-CPS g2 (expression-alternate x))))
			       (expression-antecedent x)))
	      (list g/x)))
	    (convert-to-CPS (create-converted-continuation-expression
			     x
			     ;; needs work: To give the environment a name.
			     (create-environment #f "continuation")
			     (list g1)
			     (create-if-expression
			      x
			      (create-access-expression x g1)
			      (convert-to-CPS g/x (expression-consequent x))
			      (convert-to-CPS g/x (expression-alternate x))))
			    (expression-antecedent x)))))
      (else
       ;; [(IF x1 x2 x3)]_c --> (IF x1 [x2]_c [x3]_c)
       ;; [(IF x1 x2 x3)]_c --> ((LAMBDA (g2) (IF x1 [x2]_g2 [x3]_g2)) c)
       (unless (or
		(expression-needs-conversion-to-CPS? (expression-consequent x))
		(expression-needs-stop-conversion-to-CPS?
		 (expression-consequent x))
		(expression-needs-conversion-to-CPS? (expression-alternate x))
		(expression-needs-stop-conversion-to-CPS?
		 (expression-alternate x)))
	(fuck-up))
       (if (expression? g/x)
	   (let ((g2 (string->variable "x")))
	    (create-call-expression
	     x
	     (create-converted-continuation-expression
	      x
	      ;; needs work: To give the environment a name.
	      (create-environment #f "continuation")
	      (list g2)
	      (create-if-expression
	       x
	       (nonconvert-to-CPS (expression-antecedent x))
	       (convert-to-CPS g2 (expression-consequent x))
	       (convert-to-CPS g2 (expression-alternate x))))
	     (list g/x)))
	   (create-if-expression
	    x
	    (nonconvert-to-CPS (expression-antecedent x))
	    (convert-to-CPS g/x (expression-consequent x))
	    (convert-to-CPS g/x (expression-alternate x)))))))
    ((primitive-procedure) (fuck-up))
    ((foreign-procedure) (fuck-up))
    ((access) (fuck-up))
    ((call)
     ;; [(x3 ...)]_g/x --> [x3]_(LAMBDA (g1) ... (g1 g/x ...) ...)
     (let loop ((xs1 (cons (expression-callee x) (expression-arguments x)))
		(gs '()))
      (if (null? xs1)
	  (create-converted-call-expression
	   x
	   (create-access-expression x (last gs))
	   (cons (maybe-create-access-expression g/x)
		 (map (lambda (g) (create-access-expression x g))
		      (rest (reverse gs)))))
	  (let* ((g1 (string->variable "x"))
		 (x1 (create-converted-continuation-expression
		      x
		      ;; needs work: To give the environment a name.
		      (create-environment #f "continuation")
		      (list g1)
		      (loop (rest xs1) (cons g1 gs)))))
	   (if (or (expression-needs-conversion-to-CPS? (first xs1))
		   (expression-needs-stop-conversion-to-CPS? (first xs1)))
	       (convert-to-CPS x1 (first xs1))
	       (create-call-expression
		x x1 (list (nonconvert-to-CPS (first xs1)))))))))
    ((converted-call) (fuck-up))
    (else (fuck-up))))
  (else (call-continuation g/x (nonconvert-to-CPS x)))))

(define (nonconvert-to-CPS x)
 (when (expression-needs-conversion-to-CPS? x) (fuck-up))
 (if (expression-needs-stop-conversion-to-CPS? x)
     ;; x --> [x]_(LAMBDA (g) g)
     (convert-to-CPS (let ((g (string->variable "x")))
		      (create-converted-continuation-expression
		       x
		       ;; needs work: To give the environment a name.
		       (create-environment #f "continuation")
		       (list g)
		       (create-access-expression x g)))
		     x)
     (case (expression-kind x)
      ((null-constant) x)
      ((true-constant) x)
      ((false-constant) x)
      ((char-constant) x)
      ((fixnum-constant) x)
      ((flonum-constant) x)
      ((rectangular-constant) x)
      ((string-constant) x)
      ((symbol-constant) x)
      ((pair-constant) x)
      ((vector-constant) x)
      ((lambda)
       ;; (LAMBDA (g ...) x) --> (LAMBDA (c g ...) [x]_c)
       (cond ((noop? x) x)
	     ((expression-needs-conversion-to-CPS? (expression-body x))
	      (let ((g (string->variable "c")))
	       (create-converted-lambda-expression
		x
		(expression-lambda-environment x)
		(cons g (expression-parameters x))
		(convert-to-CPS g (expression-body x)))))
	     (else (create-lambda-expression
		    x
		    (expression-lambda-environment x)
		    (expression-parameters x)
		    (nonconvert-to-CPS (expression-body x))))))
      ((converted-lambda converted-continuation) (fuck-up))
      ((set!)
       (create-set!-expression
	x (expression-variable x) (nonconvert-to-CPS (expression-source x))))
      ((if)
       (create-if-expression x
			     (nonconvert-to-CPS (expression-antecedent x))
			     (nonconvert-to-CPS (expression-consequent x))
			     (nonconvert-to-CPS (expression-alternate x))))
      ((primitive-procedure) x)
      ((foreign-procedure) x)
      ((access) x)
      ((call)
       (create-call-expression
	x
	(nonconvert-to-CPS (expression-callee x))
	(map nonconvert-to-CPS (expression-arguments x))))
      ((converted-call) (fuck-up))
      (else (fuck-up)))))

(define (fully-convert-to-CPS x)
 (define (fully-convert-to-CPS g/x x)
  (case (expression-kind x)
   ((null-constant) (call-continuation g/x x))
   ((true-constant) (call-continuation g/x x))
   ((false-constant) (call-continuation g/x x))
   ((char-constant) (call-continuation g/x x))
   ((fixnum-constant) (call-continuation g/x x))
   ((flonum-constant) (call-continuation g/x x))
   ((rectangular-constant) (call-continuation g/x x))
   ((string-constant) (call-continuation g/x x))
   ((symbol-constant) (call-continuation g/x x))
   ((pair-constant) (call-continuation g/x x))
   ((vector-constant) (call-continuation g/x x))
   ((lambda)
    (call-continuation
     g/x
     (if (noop? x)
	 x
	 (let ((g (string->variable "c")))
	  (create-converted-lambda-expression
	   x
	   (expression-lambda-environment x)
	   (cons g (expression-parameters x))
	   (fully-convert-to-CPS g (expression-body x)))))))
   ((converted-lambda converted-continuation) (fuck-up))
   ((set!)
    ;; [(SET! g x)]_g/x --> [x]_(LAMBDA (g1) (g/x (SET! g g1)))
    (let ((g1 (string->variable "x")))
     (fully-convert-to-CPS
      (create-converted-continuation-expression
       x
       ;; needs work: To give the environment a name.
       (create-environment #f "continuation")
       (list g1)
       (call-continuation
	g/x
	(create-set!-expression
	 x (expression-variable x) (create-access-expression x g1))))
      (expression-source x))))
   ((if)
    ;; [(IF x2 x3 x4)]_c --> [x2]_(LAMBDA (g1) (IF g1 [x3]_c [x4]_c))
    ;; [(IF x2 x3 x4)]_c -->
    ;; ((LAMBDA (g2) [x2]_(LAMBDA (g1) (IF g1 [x3]_g2 [x4]_g2))) c)
    (let ((g1 (string->variable "x")))
     (if (expression? g/x)
	 (let ((g2 (string->variable "x")))
	  (create-call-expression
	   x
	   (create-converted-continuation-expression
	    x
	    ;; needs work: To give the environment a name.
	    (create-environment #f "continuation")
	    (list g2)
	    (fully-convert-to-CPS
	     (create-converted-continuation-expression
	      x
	      ;; needs work: To give the environment a name.
	      (create-environment #f "continuation")
	      (list g1)
	      (create-if-expression
	       x
	       (create-access-expression x g1)
	       (fully-convert-to-CPS g2 (expression-consequent x))
	       (fully-convert-to-CPS g2 (expression-alternate x))))
	     (expression-antecedent x)))
	   (list g/x)))
	 (fully-convert-to-CPS
	  (create-converted-continuation-expression
	   x
	   ;; needs work: To give the environment a name.
	   (create-environment #f "continuation")
	   (list g1)
	   (create-if-expression
	    x
	    (create-access-expression x g1)
	    (fully-convert-to-CPS g/x (expression-consequent x))
	    (fully-convert-to-CPS g/x (expression-alternate x))))
	  (expression-antecedent x)))))
   ((primitive-procedure) (call-continuation g/x x))
   ((foreign-procedure) (call-continuation g/x x))
   ((access) (call-continuation g/x x))
   ((call)
    ;; [(x3 ...)]_g/x --> [x3]_(LAMBDA (g1) ... (g1 g/x ...) ...)
    (let loop ((xs1 (cons (expression-callee x) (expression-arguments x)))
	       (gs '()))
     (if (null? xs1)
	 (create-converted-call-expression
	  x
	  (create-access-expression x (last gs))
	  (cons (maybe-create-access-expression g/x)
		(map (lambda (g) (create-access-expression x g))
		     (rest (reverse gs)))))
	 (let* ((g1 (string->variable "x"))
		(x1 (create-converted-continuation-expression
		     x
		     ;; needs work: To give the environment a name.
		     (create-environment #f "continuation")
		     (list g1)
		     (loop (rest xs1) (cons g1 gs)))))
	  (fully-convert-to-CPS x1 (first xs1))))))
   ((converted-call) (fuck-up))
   (else (fuck-up))))
 (create-lambda-expression
  x
  (expression-lambda-environment x)
  (expression-parameters x)
  ;; x --> [x]_(LAMBDA (g) g)
  (fully-convert-to-CPS (let ((g (string->variable "x")))
			 (create-converted-continuation-expression
			  (expression-body x)
			  ;; needs work: To give the environment a name.
			  (create-environment #f "continuation")
			  (list g)
			  (create-access-expression (expression-body x) g)))
			(expression-body x))))

;;; Determine environment distances from root

(define (determine-environment-distances-from-root!)
 ;; We used to be anal here and find the root by two means. One was the
 ;; procedure that called every procedure. The other was the procedure that
 ;; wasn't properly called by any other procedure. And we even checked that
 ;; these two means produced the same singleton result. But now that computing
 ;; the CALLS? and PROPERLY-CALLS? relations are expensive we punt and make
 ;; use of the hardwired assumption that the root is the procedure with no
 ;; parent. Life is short.
 (let ((root (find-if (lambda (e) (empty? (parent e))) *es*)))
  ;; conventions: ROOT
  (let loop ((es (list root)) (i 0))
   ;; conventions: I
   (unless (null? es)
    (for-each (lambda (e) (set-environment-distance-from-root! e i)) es)
    (loop (remove-if-not
	   (lambda (e1)
	    (and (not (number? (distance-from-root e1)))
		 (some (lambda (e2) (directly-calls? e2 e1)) es)))
	   *es*)
	  (+ i 1))))
  (unless (every (lambda (e) (number? (distance-from-root e))) *es*)
   (fuck-up))))

;;; Determine which environments have external self tail calls

(define (determine-which-environments-have-external-self-tail-calls!)
 ;; An external self tail call is a self tail call that is in-lined in E to a
 ;; procedure that E is properly in-lined in. This situation can cause a
 ;; memory leak if E has a reentrant region because E will not be returned from
 ;; and its region will not be restored. Also, E can't have a non-reentrant
 ;; region because the external self tail call will cause the region to be
 ;; clobbered the next time E is entered. It is OK to for there to be a self
 ;; tail call to E (but not to a procedure that E is properly in-lined in)
 ;; because the self-tail-call entry point comes after the region
 ;; initialization code.
 ;; note: The following is an efficiency hack.
 (for-each (lambda (e)
	    (set-environment-marked1?!
	     e
	     (some (lambda (y)
		    (and (not (top-level-call-site? y))
			 (can-be-self-tail-call-to? y e)))
		   (call-sites e))))
	   *es*)
 (for-each
  (lambda (e)
   (set-environment-has-external-self-tail-call?!
    e
    (and (unique-call-site? e)
	 (let loop? ((e1 (expression-environment
			  (call-site-expression (unique-call-site e)))))
	  (or (and (environment-marked1? e1)
		   (some (lambda (y)
			  (and (not (top-level-call-site? y))
			       (can-be-self-tail-call-to? y e1)
			       (in-lined-in? (call-site-expression y) e)))
			 (call-sites e1)))
	      (and (unique-call-site? e1)
		   (loop? (expression-environment
			   (call-site-expression (unique-call-site e1))))))))))
  *es*))

;;; Determine which environments have external continuation calls

(define (determine-which-environments-have-external-continuation-calls!)
 ;; An external continuation call is a call that is in a procedure that can be
 ;; called by E to a continuation created not by E but by a procedure that
 ;; calls E. This situation can cause a memory leak if E has a reentrant region
 ;; because E will not be returned from and its region will not be restored.
 ;; note: This is really misnamed. It is not an external continuation call but
 ;;       rather an external call to a continuation. But then then the name
 ;;       becomes too long.
 (for-each
  (lambda (e)
   (clock-sample)			;To prevent overflow.
   ;; This is done just for side effect, to set the MARKED2? bits.
   (some-proper-caller
    (lambda (e) #f) environment-marked2? set-environment-marked2?! e)
   (set-environment-has-external-continuation-call?!
    e
    (some
     (lambda (e1)
      (some
       (lambda (x)
	(can-be?
	 (lambda (u)
	  (and (continuation-type? u)
	       ((truly-compatible-call? x) u)
	       (can-be?
		(lambda (u1)
		 (and (native-procedure-type? u1)
		      ((truly-compatible-procedure?
			(list (create-anonymous-type-set u))
			*null*
			(recreate-call-site
			 (create-call-site
			  (continuation-type-allocating-expression u))
			 'first-argument))
		       u1)
		      (some (lambda (e1)
			     (and (not (eq? e1 e)) (environment-marked2? e1)))
			    (narrow-clones u1))))
		(expression-type-set
		 (first-argument
		  (continuation-type-allocating-expression u))))))
	 (expression-type-set (expression-callee x))))
       ;; needs work: What about implicit call sites?
       (environment-continuation-calls e1)))
     (proper-callees e))))
  *es*))

;;; Determine blocked environments

(define (determine-blocked-environments!)
 (for-each
  (lambda (e)
   (clock-sample)			;To prevent overflow.
   (when (and (environment-used? e)
	      (not (has-external-self-tail-call? e))
	      (not (and (reentrant? e) (has-external-continuation-call? e))))
    (for-each (lambda (e) (set-environment-marked1?! e #f)) *es*)
    (set-environment-marked1?! e #t)
    (let loop ((e1 (find-if (lambda (e) (empty? (parent e))) *es*)))
     (unless (environment-marked1? e1)
      (set-environment-marked1?! e1 #t)
      (for-each loop (environment-direct-tail-callees e1))
      (for-each loop (environment-direct-non-tail-callees e1))))
    (set-environment-blocked-environments!
     e (cons e (remove-if environment-marked1? *es*)))))
  *es*))

;;; Determine which environments need to pass parameters globally

(define (determine-which-environments-need-to-pass-parameters-globally!)
 (for-each (lambda (e) (set-environment-passes-parameters-globally?! e #f))
	   *es*)
 (when *tail-call-optimization?*
  (for-each
   (lambda (y)
    (for-each (lambda (e) (set-environment-passes-parameters-globally?! e #t))
	      (nonmerged-tail-recursive-purely-tail-call-site-callees y)))
   (remove-if-not nonmerged-tail-recursive-purely-tail-call-site? *ys*))))

;;; Determine allocations

(define (environment-necessarily-on-path-from-root-to-expression? e x)
 (when (or (empty? e)
	   (not (environment-used? e))
	   (has-external-self-tail-call? e)
	   (and (reentrant? e) (has-external-continuation-call? e)))
  (fuck-up))
 (memq (expression-environment x) (environment-blocked-environments e)))

(define (safe-type-allocation-environment? e u x)
 (and (environment-marked2? e)		;This is just an efficiency hack.
      (not (has-external-self-tail-call? e))
      (not (and (reentrant? e) (has-external-continuation-call? e)))
      (environment-necessarily-on-path-from-root-to-expression? e x)
      (not (escapes? u e))))

(define (safe-environment-allocation-environment? e1 e2)
 (and (environment-marked2? e1)		;This is just an efficiency hack.
      (not (has-external-self-tail-call? e1))
      (not (and (reentrant? e1) (has-external-continuation-call? e1)))
      (environment-necessarily-on-path-from-root-to-expression?
       e1 (expression-body (environment-expression e2)))
      ;; It is safe to allocate E2 on E1 if no native procedure that has E2 as
      ;; an ancestor escapes E1.
      (not (some (lambda (e3) (escapes? (environment-type e3) e1))
		 (descendents e2)))))

(define (minimal f distance-from-root-to-environments-map)
 ;; conventions: F DISTANCE-FROM-ROOT-TO-ENVIRONMENTS-MAP
 (let loop ((i (- (vector-length distance-from-root-to-environments-map) 1)))
  ;; conventions: I
  (let ((e (find-if f (vector-ref distance-from-root-to-environments-map i))))
   (cond (e
	  (unless (one f (vector-ref distance-from-root-to-environments-map i))
	   (fuck-up))
	  e)
	 (else (loop (- i 1)))))))

(define (choose-allocation e p?)
 (cond
  ((and *stack-allocation?*
	p?
	;; alloca foils tail-call optimization.
	(not (environment-passes-parameters-globally? (home e)))
	;; If E is in-lined then stack allocating on E allocates on the home
	;; of E. Don't stack allocate on an environment that is not called
	;; more than once because then reclaimation never occurs. This is a
	;; heuristic because it actually might be a good policy to allocate in
	;; such a way because the program might consist of a sequence of major
	;; subcomponents and transition between subcomponents might be the
	;; appropriate reclaimation time.
	(called-more-than-once? (home e))
	;; If E is in-lined then stack allocating on E allocates on the home
	;; of E. If E is in-lined in a recursive procedure but E itself is
	;; is not recursive then you don't want to stack allocate on the home
	;; of E because then you don't reclaim on exit from E. It is likely
	;; that the home of E repeatedly calls E so it is better to be able
	;; to reclaim on each call.
	(not (and (unique-call-site? e)
		  (in-lined-in-recursive? e)
		  (not (recursive? e)))))
   'stack)
  ((and *region-allocation?*
	;; Reentrant regions foil tail-call optimization.
	(not (and (reentrant? e)
		  (let loop ((e e))
		   (if (unique-call-site? e)
		       (and (directly-tail-calls?
			     (expression-environment
			      (call-site-expression (unique-call-site e)))
			     e)
			    (loop
			     (expression-environment
			      (call-site-expression (unique-call-site e)))))
		       (environment-passes-parameters-globally? e)))))
	;; Don't allocate on the region of an environment that is not called
	;; more than once because then reclaimation never occurs. This is a
	;; heuristic because it actually might be a good policy to allocate in
	;; such a way because the program might consist of a sequence of major
	;; subcomponents and transition between subcomponents might be the
	;; appropriate reclaimation time.
	(called-more-than-once? e))
   e)
  (*heap-allocation?* 'heap)
  ((and *stack-allocation?* p?) 'stack)
  (else e)))

(define (determine-allocations!)
 (let ((distance-from-root-to-environments-map
	(make-vector (+ (reduce max (map distance-from-root *es*) 0) 1) '())))
  (for-each
   (lambda (e)
    (vector-set! distance-from-root-to-environments-map
		 (distance-from-root e)
		 (cons e (vector-ref distance-from-root-to-environments-map
				     (distance-from-root e)))))
   *es*)
  (for-each
   (lambda (u)
    (clock-sample)			;To prevent overflow.
    (for-each
     (lambda (x)
      (when (expression? x)
       (unless (eq? (expression-kind x) 'string-constant)
	;; This is done just for side effect, to set the MARKED2? bits.
	(some-caller (lambda (e) #f)
		     environment-marked2?
		     set-environment-marked2?!
		     (expression-environment x))
	(let ((e (minimal
		  (lambda (e) (safe-type-allocation-environment? e u x))
		  distance-from-root-to-environments-map)))
	 (set-expression-type-allocation-alist!
	  x
	  (cons
	   (cons u (choose-allocation e (eq? e (expression-environment x))))
	   (expression-type-allocation-alist x)))))))
     (string-type-allocating-expressions u)))
   *string-types*)
  (for-each
   (lambda (u)
    ;; Note: Need to determine an allocation even for immediate structures
    ;;       because they may become indirect.
    (unless (fictitious? u)
     (clock-sample)			;To prevent overflow.
     (for-each
      (lambda (x)
       (unless (eq? (expression-kind x) 'pair-constant)
	;; This is done just for side effect, to set the MARKED2? bits.
	(some-caller (lambda (e) #f)
		     environment-marked2?
		     set-environment-marked2?!
		     (expression-environment x))
	(let ((e (minimal
		  (lambda (e) (safe-type-allocation-environment? e u x))
		  distance-from-root-to-environments-map)))
	 (set-expression-type-allocation-alist!
	  x
	  (cons
	   (cons u (choose-allocation e (eq? e (expression-environment x))))
	   (expression-type-allocation-alist x))))))
      (structure-type-allocating-expressions u))))
   *structure-types*)
  (for-each
   (lambda (u)
    (unless (degenerate-vector-type? u)
     (clock-sample)			;To prevent overflow.
     (for-each
      (lambda (x)
       (unless (eq? (expression-kind x) 'vector-constant)
	;; This is done just for side effect, to set the MARKED2? bits.
	(some-caller (lambda (e) #f)
		     environment-marked2?
		     set-environment-marked2?!
		     (expression-environment x))
	(let ((e (minimal
		  (lambda (e) (safe-type-allocation-environment? e u x))
		  distance-from-root-to-environments-map)))
	 (set-expression-type-allocation-alist!
	  x
	  (cons
	   (cons u (choose-allocation e (eq? e (expression-environment x))))
	   (expression-type-allocation-alist x))))))
      (headed-vector-type-allocating-expressions u))))
   *headed-vector-types*)
  (for-each
   (lambda (u)
    (unless (degenerate-vector-type? u)
     (clock-sample)			;To prevent overflow.
     (for-each
      (lambda (x)
       (when (expression? x)
	;; This is done just for side effect, to set the MARKED2? bits.
	(some-caller (lambda (e) #f)
		     environment-marked2?
		     set-environment-marked2?!
		     (expression-environment x))
	(let ((e (minimal
		  (lambda (e) (safe-type-allocation-environment? e u x))
		  distance-from-root-to-environments-map)))
	 (set-expression-type-allocation-alist!
	  x
	  (cons
	   (cons u (choose-allocation e (eq? e (expression-environment x))))
	   (expression-type-allocation-alist x))))))
      (nonheaded-vector-type-allocating-expressions u))))
   *nonheaded-vector-types*)
  (for-each
   (lambda (e)
    (when (and (not (noop? e)) (has-closure? e))
     (clock-sample)			;To prevent overflow.
     ;; This is done just for side effect, to set the MARKED2? bits.
     (some-caller
      (lambda (e) #f) environment-marked2? set-environment-marked2?! e)
     (let ((e1 (minimal
		(lambda (e1) (safe-environment-allocation-environment? e1 e))
		distance-from-root-to-environments-map)))
      (set-environment-allocation! e (choose-allocation e1 (eq? e1 e))))))
   *es*)))

;;; Apply closed-world assumption

(define (dereference-type u)
 (cond ((null-type? u) u)
       ((true-type? u) u)
       ((false-type? u) u)
       ((char-type? u) u)
       ((fixnum-type? u) u)
       ((flonum-type? u) u)
       ((rectangular-type? u) u)
       ((input-port-type? u) u)
       ((output-port-type? u) u)
       ((eof-object-type? u) u)
       ((pointer-type? u) u)
       ((internal-symbol-type? u) u)
       ((external-symbol-type? u)
	(if (eq? (external-symbol-type-link u) u)
	    u
	    (let ((u1 (dereference-type (external-symbol-type-link u))))
	     (set-external-symbol-type-link! u u1)
	     u1)))
       ((primitive-procedure-type? u) u)
       ((native-procedure-type? u) u)
       ((foreign-procedure-type? u) u)
       ((continuation-type? u) u)
       ((string-type? u)
	(if (eq? (string-type-link u) u)
	    u
	    (let ((u1 (dereference-type (string-type-link u))))
	     (set-string-type-link! u u1)
	     u1)))
       ((structure-type? u)
	(if (eq? (structure-type-link u) u)
	    u
	    (let ((u1 (dereference-type (structure-type-link u))))
	     (set-structure-type-link! u u1)
	     u1)))
       ((headed-vector-type? u)
	(if (eq? (headed-vector-type-link u) u)
	    u
	    (let ((u1 (dereference-type (headed-vector-type-link u))))
	     (set-headed-vector-type-link! u u1)
	     u1)))
       ((nonheaded-vector-type? u)
	(if (eq? (nonheaded-vector-type-link u) u)
	    u
	    (let ((u1 (dereference-type (nonheaded-vector-type-link u))))
	     (set-nonheaded-vector-type-link! u u1)
	     u1)))
       ((displaced-vector-type? u)
	(if (eq? (displaced-vector-type-link u) u)
	    u
	    (let ((u1 (dereference-type (displaced-vector-type-link u))))
	     (set-displaced-vector-type-link! u u1)
	     u1)))
       (else (fuck-up))))

(define (dereference-type-set w)
 (if (eq? (type-set-link w) w)
     w
     (let ((w1 (dereference-type-set (type-set-link w))))
      (set-type-set-link! w w1)
      w1)))

;;; Note that one can't filter out subtypes from a type set, i.e. replace
;;; union(u1,u2,u3) with union(u1,u3) if subtype(u2,u1). This won't work
;;; because types denote representations, not sets of values. Even though
;;; subtype(fixnum,union(fixnum,flonum)) and even though
;;; subset(headed-vector(fixnum),headed-vector(union(fixnum,flonum))) it is not
;;; the case that
;;; subtype(headed-vector(fixnum),headed-vector(union(fixnum,flonum))) since
;;; headed-vector(fixnum) has a different representation from
;;; headed-vector(union(fixnum,flonum)) and you can't widen the former into the
;;; later without deep widening.

(define (apply-closed-world-assumption!)
 (define (sorting-remove-duplicatesq us)
  (let loop ((us (sort us > type-index)) (us1 '()))
   (if (null? us)
       us1
       (loop (rest us)
	     (if (and (not (null? us1)) (eq? (first us) (first us1)))
		 us1
		 (cons (first us) us1))))))
 (define (create-trie . initial-value)
  (if (null? initial-value)
      (set! initial-value #f)
      (set! initial-value (first initial-value)))
  (make-trie #f #f #f initial-value (make-trie-node '() initial-value)))
 (define (trie-ref trie list)
  (let loop ((trie-node (trie-trie-node trie)) (list list))
   (if trie-node
       (if (null? list)
	   (trie-node-value trie-node)
	   (loop (and (assq (first list) (trie-node-table trie-node))
		      (cdr (assq (first list) (trie-node-table trie-node))))
		 (rest list)))
       (trie-initial-value trie))))
 (define (trie-set! trie list value)
  (let loop ((trie-node (trie-trie-node trie)) (list list))
   (if (null? list)
       (set-trie-node-value! trie-node value)
       (let ((entry (assq (first list) (trie-node-table trie-node))))
	;; conventions: ENTRY
	(unless entry
	 (set! entry
	       (cons (first list)
		     (make-trie-node '() (trie-initial-value trie))))
	 (set-trie-node-table!
	  trie-node (cons entry (trie-node-table trie-node))))
	(loop (cdr entry) (rest list))))))
 (let loop ()
  (let ((again? #f))
   ;; Congruence Closure
   (do ((us *external-symbol-types* (rest us))) ((null? us))
    (let ((u1 (first us)))
     (when (eq? (external-symbol-type-link u1) u1)
      (clock-sample)			;To prevent overflow.
      (for-each
       (lambda (u2)
	(when (and (eq? (external-symbol-type-link u1) u1)
		   (eq? (external-symbol-type-link u2) u2)
		   (not (eq? u1 u2))
		   (eq? (dereference-type
			 (external-symbol-type-displaced-string-type u1))
			(dereference-type
			 (external-symbol-type-displaced-string-type u2))))
	 (set-external-symbol-type-link! u2 u1)
	 (set! again? #t)))
       (rest us)))))
   (do ((us *string-types* (rest us))) ((null? us))
    (let ((u1 (first us)))
     (when (eq? (string-type-link u1) u1)
      (clock-sample)			;To prevent overflow.
      (for-each
       (lambda (u2)
	(when (and (eq? (string-type-link u1) u1)
		   (eq? (string-type-link u2) u2)
		   (not (eq? u1 u2)))
	 (set-string-type-allocating-expressions!
	  u1
	  (unionq (string-type-allocating-expressions u1)
		  (string-type-allocating-expressions u2)))
	 (set-string-type-link! u2 u1)
	 (set! again? #t)))
       (rest us)))))
   (do ((us *structure-types* (rest us))) ((null? us))
    (let ((u1 (first us)))
     (when (eq? (structure-type-link u1) u1)
      (clock-sample)			;To prevent overflow.
      (for-each
       (lambda (u2)
	(when (and (eq? (structure-type-link u1) u1)
		   (eq? (structure-type-link u2) u2)
		   (not (eq? u1 u2))
		   (eq? (structure-type-name u1) (structure-type-name u2))
		   (= (length (structure-type-slots u1))
		      (length (structure-type-slots u2)))
		   (every (lambda (w1 w2)
			   (eq? (dereference-type-set w1)
				(dereference-type-set w2)))
			  (structure-type-slots u1)
			  (structure-type-slots u2)))
	 (unless (structure-type-immediate? u2)
	  (set-structure-type-immediate?! u1 #f))
	 (set-structure-type-allocating-expressions!
	  u1
	  (unionq (structure-type-allocating-expressions u1)
		  (structure-type-allocating-expressions u2)))
	 (when (structure-type-alignment? u2)
	  (set-structure-type-alignment?! u1 #t))
	 (when (structure-type-alignment&? u2)
	  (set-structure-type-alignment&?! u1 #t))
	 (when (structure-type-size? u2) (set-structure-type-size?! u1 #t))
	 (set-structure-type-link! u2 u1)
	 (set! again? #t)))
       (rest us)))))
   (do ((us *headed-vector-types* (rest us))) ((null? us))
    (let ((u1 (first us)))
     (when (eq? (headed-vector-type-link u1) u1)
      (clock-sample)			;To prevent overflow.
      (for-each
       (lambda (u2)
	(when (and
	       (eq? (headed-vector-type-link u1) u1)
	       (eq? (headed-vector-type-link u2) u2)
	       (not (eq? u1 u2))
	       (eq? (dereference-type-set (headed-vector-type-element u1))
		    (dereference-type-set (headed-vector-type-element u2))))
	 (set-headed-vector-type-allocating-expressions!
	  u1
	  (unionq (headed-vector-type-allocating-expressions u1)
		  (headed-vector-type-allocating-expressions u2)))
	 (when (headed-vector-type-alignment? u2)
	  (set-headed-vector-type-alignment?! u1 #t))
	 (when (headed-vector-type-alignment&? u2)
	  (set-headed-vector-type-alignment&?! u1 #t))
	 (when (headed-vector-type-size? u2)
	  (set-headed-vector-type-size?! u1 #t))
	 (set-headed-vector-type-link! u2 u1)
	 (set! again? #t)))
       (rest us)))))
   (do ((us *nonheaded-vector-types* (rest us))) ((null? us))
    (let ((u1 (first us)))
     (when (eq? (nonheaded-vector-type-link u1) u1)
      (clock-sample)			;To prevent overflow.
      (for-each
       (lambda (u2)
	(when (and (eq? (nonheaded-vector-type-link u1) u1)
		   (eq? (nonheaded-vector-type-link u2) u2)
		   (not (eq? u1 u2))
		   (eq? (dereference-type-set
			 (nonheaded-vector-type-element u1))
			(dereference-type-set
			 (nonheaded-vector-type-element u2))))
	 (set-nonheaded-vector-type-allocating-expressions!
	  u1
	  (unionq (nonheaded-vector-type-allocating-expressions u1)
		  (nonheaded-vector-type-allocating-expressions u2)))
	 (when (nonheaded-vector-type-alignment? u2)
	  (set-nonheaded-vector-type-alignment?! u1 #t))
	 (when (nonheaded-vector-type-size? u2)
	  (set-nonheaded-vector-type-size?! u1 #t))
	 (set-nonheaded-vector-type-link! u2 u1)
	 (set! again? #t)))
       (rest us)))))
   (do ((us *displaced-vector-types* (rest us))) ((null? us))
    (let ((u1 (first us)))
     (when (eq? (displaced-vector-type-link u1) u1)
      (clock-sample)			;To prevent overflow.
      (for-each
       (lambda (u2)
	(when (and (eq? (displaced-vector-type-link u1) u1)
		   (eq? (displaced-vector-type-link u2) u2)
		   (eq? (dereference-type
			 (displaced-vector-type-displaced-vector-type u1))
			(dereference-type
			 (displaced-vector-type-displaced-vector-type u2))))
	 (when (displaced-vector-type-alignment? u2)
	  (set-displaced-vector-type-alignment?! u1 #t))
	 (when (displaced-vector-type-size? u2)
	  (set-displaced-vector-type-size?! u1 #t))
	 (set-displaced-vector-type-link! u2 u1)
	 (set! again? #t)))
       (rest us)))))
   ;; Closed World Assumption
   (let ((trie (create-trie '())))
    ;; conventions: TRIE
    (for-each
     (lambda (w)
      (when (eq? (type-set-link w) w)
       (clock-sample)			;To prevent overflow.
       (let ((us (sorting-remove-duplicatesq
		  (map dereference-type (members w)))))
	;; This is the case where we previously determined that a type set was
	;; not fictitious because it was not monomorphic but we now discover
	;; that all of its members are equivalent (and fictitious). Thus
	;; the type set is really ficitious. But all havoc will break loose if
	;; we have a monomorphic nonfictitious type set whose member is
	;; fictitious. And it is way too late to propagate ficition. So we
	;; force the typeset to be tag-only even though it will always have a
	;; known tag. So it goes.
	(when (and (multimorphic? w)
		   (= (length us) 1)
		   (fictitious? (dereference-type (first (members w)))))
	 (notify "Warning! W~a should be fictitious but isn't"
		 (type-set-index w)))
	(set-members! w us)
	(trie-set! trie us (cons w (trie-ref trie us))))))
     *ws*)
    (for-each (lambda (w)
	       (when (eq? (type-set-link w) w)
		(clock-sample)		;To prevent overflow.
		(let* ((ws (trie-ref trie (members w)))
		       (w1 (first ws)))
		 (when (eq? w1 w)
		  (for-each
		   (lambda (w2)
		    (cond
		     ((eq? (fictitious? w1) (fictitious? w2))
		      (set-type-set-link! w2 w1)
		      (set! again? #t))
		     (else
		      (notify
		       "Warning! Not merging W~s and W~s because the former is ~a and the latter is ~a"
		       (type-set-index w1)
		       (type-set-index w2)
		       (if (fictitious? w1) "fictitious" "not fictitious")
		       (if (fictitious? w2) "fictitious" "not fictitious")))))
		   (rest ws))))))
	      *ws*))
   (when again? (loop))))
 (set! *external-symbol-types*
       (remove-if-not (lambda (u) (eq? (external-symbol-type-link u) u))
		      *external-symbol-types*))
 (set-members! *foreign-string-type-set*
	       (map dereference-type (members *foreign-string-type-set*)))
 (set! *string-types*
       (remove-if-not (lambda (u) (eq? (string-type-link u) u))
		      *string-types*))
 (set! *structure-types*
       (remove-if-not (lambda (u) (eq? (structure-type-link u) u))
		      *structure-types*))
 (set! *headed-vector-types*
       (remove-if-not (lambda (u) (eq? (headed-vector-type-link u) u))
		      *headed-vector-types*))
 (set! *nonheaded-vector-types*
       (remove-if-not (lambda (u) (eq? (nonheaded-vector-type-link u) u))
		      *nonheaded-vector-types*))
 (set! *displaced-vector-types*
       (remove-if-not (lambda (u) (eq? (displaced-vector-type-link u) u))
		      *displaced-vector-types*))
 (set! *ws* (remove-if-not (lambda (w) (eq? (type-set-link w) w)) *ws*))
 (for-each
  (lambda (x)
   (set-expression-type-set! x (dereference-type-set (expression-type-set x)))
   (for-each (lambda (u-e) (set-car! u-e (dereference-type (car u-e))))
	     (expression-type-allocation-alist x))
   (set-expression-type-allocation-alist!
    x
    (remove-duplicatesp
     (lambda (u-e1 u-e2)
      (and (eq? (car u-e1) (car u-e2)) (eq? (cdr u-e1) (cdr u-e2))))
     (expression-type-allocation-alist x)))
   (unless (= (length (expression-type-allocation-alist x))
	      (length (remove-duplicatesq
		       (map car (expression-type-allocation-alist x)))))
    (fuck-up)))
  *xs*)
 (for-each
  (lambda (u)
   (set-external-symbol-type-displaced-string-type!
    u (dereference-type (external-symbol-type-displaced-string-type u))))
  *external-symbol-types*)
 (for-each (lambda (u)
	    (set-structure-type-slots!
	     u (map dereference-type-set (structure-type-slots u))))
	   *structure-types*)
 (for-each (lambda (u)
	    (set-headed-vector-type-element!
	     u (dereference-type-set (headed-vector-type-element u))))
	   *headed-vector-types*)
 (for-each (lambda (u)
	    (set-nonheaded-vector-type-element!
	     u (dereference-type-set (nonheaded-vector-type-element u))))
	   *nonheaded-vector-types*)
 (for-each
  (lambda (u)
   (set-displaced-vector-type-displaced-vector-type!
    u (dereference-type (displaced-vector-type-displaced-vector-type u))))
  *displaced-vector-types*)
 (for-each
  (lambda (g)
   (set-variable-type-set! g (dereference-type-set (variable-type-set g))))
  *gs*)
 (for-each (lambda (e)
	    (set-environment-escaping-types!
	     e
	     (sorting-remove-duplicatesq
	      (map dereference-type (environment-escaping-types e)))))
	   *es*)
 (set! <nonreclaimable-string> (dereference-type <nonreclaimable-string>))
 (set! <top-level-nonheaded-vector>
       (dereference-type <top-level-nonheaded-vector>))
 (for-each (lambda (w) (set-type-set-location! w #f)) *ws*))

;;; Determine indirect structure types

(define (determine-indirect-structure-types!)
 ;; needs work: This is conservative. It makes all structures that are part of
 ;;             a points-to cycle indirect. It is sufficient to make only one
 ;;             edge in the cycle indirect. Also, this doesn't take into
 ;;             account that native procedures and vectors are always indirect.
 (for-each
  (lambda (u)
   (when (some (lambda (w) (points-to? w u)) (structure-type-slots u))
    (set-structure-type-immediate?! u #f)))
  *structure-types*))

;;; Determine which types are never allocated on the heap

(define (determine-which-types-are-never-allocated-on-the-heap!)
 (define (never-allocated-on-the-heap? u)
  (not (some (lambda (x)
	      (let ((u-e (assq u (expression-type-allocation-alist x))))
	       (and u-e (heap-allocation? (cdr u-e)))))
	     *calls*)))
 (for-each (lambda (u)
	    (set-string-type-never-allocated-on-the-heap?!
	     u (never-allocated-on-the-heap? u)))
	   *string-types*)
 (for-each (lambda (u)
	    (set-structure-type-never-allocated-on-the-heap?!
	     u (never-allocated-on-the-heap? u)))
	   *structure-types*)
 (for-each (lambda (u)
	    (set-headed-vector-type-never-allocated-on-the-heap?!
	     u (never-allocated-on-the-heap? u)))
	   *headed-vector-types*)
 (for-each (lambda (u)
	    (set-nonheaded-vector-type-never-allocated-on-the-heap?!
	     u (never-allocated-on-the-heap? u)))
	   *nonheaded-vector-types*))

;;; Determine which environments have regions

(define (region-allocation? e) (environment? e))

(define (stack-allocation? e) (eq? e 'stack))

(define (heap-allocation? e) (eq? e 'heap))

(define *program-has-heap?* #f)

(define (determine-which-environments-have-regions!)
 (for-each (lambda (e)
	    (set-environment-has-region?! e #f)
	    (set-environment-has-nonatomic-region?! e #f))
	   *es*)
 (set! *program-has-heap?* #f)
 (for-each
  (lambda (x)
   (for-each (lambda (u-e)
	      (cond ((region-allocation? (cdr u-e))
		     (set-environment-has-region?! (cdr u-e) #t)
		     (unless (type-atomic? (car u-e))
		      (set-environment-has-nonatomic-region?! (cdr u-e) #t)))
		    ((heap-allocation? (cdr u-e))
		     (set! *program-has-heap?* #t))))
	     (expression-type-allocation-alist x)))
  *calls*)
 (for-each
  (lambda (e)
   (cond ((region-allocation? (allocation e))
	  (set-environment-has-region?! (allocation e) #t)
	  (unless (environment-atomic? e)
	   (set-environment-has-nonatomic-region?! (allocation e) #t)))
	 ((heap-allocation? (allocation e))
	  (set! *program-has-heap?* #t))))
  *es*))

;;; The remaining procedures are used just for debugging.

(define (hunoz? g) (string=? (symbol->string (variable-name g)) "hunoz"))

(define *abbreviate?* #f)

(define (list+-type? u)
 (and (pair-type? u)
      (= (length (members (pair-type-cdr u))) 2)
      (or (and (null-type? (first (members (pair-type-cdr u))))
	       (eq? (second (members (pair-type-cdr u))) u))
	  (and (null-type? (second (members (pair-type-cdr u))))
	       (eq? (first (members (pair-type-cdr u))) u)))))

(define (list-slots u)
 (if (null-type? u)
     '()
     (cons (pair-type-car u) (list-slots (the-member (pair-type-cdr u))))))

(define (list*-type? w)
 (and (= (length (members w)) 2)
      (or (and (null-type? (first (members w)))
	       (pair-type? (second (members w)))
	       (eq? (pair-type-cdr (second (members w))) w))
	  (and (null-type? (second (members w)))
	       (pair-type? (first (members w)))
	       (eq? (pair-type-cdr (first (members w))) w)))))

(define (up u/w u/ws)
 (if (or (eq? u/w (first u/ws))
	 (and (type-set? u/w)
	      (monomorphic? u/w)
	      (eq? (the-member u/w) (first u/ws))))
     0
     (+ (up u/w (rest u/ws)) 1)))

(define (externalize-type-internal u u/ws)
 (define (list-type? u)
  (or (null-type? u)
      (and (pair-type? u)
	   (monomorphic? (pair-type-cdr u))
	   (list-type? (the-member (pair-type-cdr u))))))
 (cond
  ((memq u u/ws) `(up ,(up u u/ws)))
  ((null-type? u) 'null)
  ((true-type? u) 'true)
  ((false-type? u) 'false)
  ((char-type? u) 'char)
  ((fixnum-type? u) 'fixnum)
  ((flonum-type? u) 'flonum)
  ((rectangular-type? u) 'rectangular)
  ((input-port-type? u) 'input-port)
  ((output-port-type? u) 'output-port)
  ((eof-object-type? u) 'eof-object)
  ((pointer-type? u) 'pointer)
  ((internal-symbol-type? u) `',(internal-symbol-type-name u))
  ((external-symbol-type? u)
   ;; note: Ambiguous between external symbol type and structure type.
   `(external-symbol
     ,(externalize-type-internal (external-symbol-type-displaced-string-type u)
				 (cons u u/ws))))
  ((primitive-procedure-type? u)
   (if (null? (primitive-procedure-type-arguments u))
       (primitive-procedure-type-name u)
       (cons (primitive-procedure-type-name u)
	     (primitive-procedure-type-arguments u))))
  ((native-procedure-type? u)
   `(native-procedure ,@(map environment-name (narrow-clones u))))
  ((foreign-procedure-type? u) (foreign-procedure-type-name u))
  ((continuation-type? u)
   `(continuation
     ,(expression-index (continuation-type-allocating-expression u))))
  ;; note: Ambiguous between string type and primitive-procedure type.
  ((string-type? u) 'string)
  ((structure-type? u)
   (cond
    ((list+-type? u)
     ;; note: Ambiguous between list+ type and structure type.
     `(list+ ,(if *abbreviate?*
		  (type-set-index (pair-type-car u))
		  (externalize-type-set-internal
		   (pair-type-car u) (cons u u/ws)))))
    ((list-type? u)
     ;; note: Ambiguous between list type and structure type.
     `(list ,@(map (lambda (w) (externalize-type-set-internal w (cons u u/ws)))
		   (list-slots u))))
    (else `(,(structure-type-name u)
	    ,@(map (lambda (w)
		    (if *abbreviate?*
			(type-set-index w)
			(externalize-type-set-internal w (cons u u/ws))))
		   (structure-type-slots u))))))
  ((headed-vector-type? u)
   ;; note: Ambiguous between headed-vector type and structure type.
   `(headed-vector
     ,(if *abbreviate?*
	  (type-set-index (headed-vector-type-element u))
	  (externalize-type-set-internal
	   (headed-vector-type-element u) (cons u u/ws)))))
  ((nonheaded-vector-type? u)
   ;; note: Ambiguous between nonheaded-vector type and structure type.
   `(nonheaded-vector
     ,(if *abbreviate?*
	  (type-set-index (nonheaded-vector-type-element u))
	  (externalize-type-set-internal
	   (nonheaded-vector-type-element u) (cons u u/ws)))))
  ((displaced-vector-type? u)
   ;; note: Ambiguous between displaced-vector type and structure type.
   `(displaced-vector
     ,(externalize-type-internal
       (displaced-vector-type-displaced-vector-type u) (cons u u/ws))))
  (else (fuck-up))))

(define (externalize-type-set-internal w u/ws)
 (cond ((or (memq w u/ws) (and (monomorphic? w) (memq (the-member w) u/ws)))
	`(up ,(up w u/ws)))
       ((void? w) 'void)
       ((monomorphic? w) (externalize-type-internal (the-member w) u/ws))
       ((list*-type? w)
	;; note: Ambiguous between list* type and structure type.
	`(list* ,(externalize-type-set-internal
		  (pair-type-car (the-member-that structure-type? w))
		  (cons w u/ws))))
       (else `(union ,@(map (lambda (u)
			     (externalize-type-internal u (cons w u/ws)))
			    (members w))))))

(define (externalize-type u) (externalize-type-internal u '()))

(define (externalize-type-set w) (externalize-type-set-internal w '()))

(define (variable-names x)
 ;; This is a real kludge.
 (if (eq? (expression-parameters x) (unspecified))
     'unspecified
     (let loop ((gs (variables x)))
      (cond ((null? gs) '())
	    ((and (null? (rest gs)) (rest? x)) (variable-name (first gs)))
	    (else (cons (variable-name (first gs)) (loop (rest gs))))))))

(define (externalize-expression x)
 (define (undecorate-constant x)
  (case (expression-kind x)
   ((null-constant) '())
   ((true-constant) #t)
   ((false-constant) #f)
   ((char-constant) (expression-constant x))
   ((fixnum-constant) (expression-constant x))
   ((flonum-constant) (expression-constant x))
   ((rectangular-constant) (expression-constant x))
   ((string-constant) (expression-constant x))
   ((symbol-constant) (expression-constant x))
   ((pair-constant)
    (cons (undecorate-constant (car (expression-constant x)))
	  (undecorate-constant (cdr (expression-constant x)))))
   ((vector-constant) (map-vector undecorate-constant (expression-constant x)))
   (else (fuck-up))))
 (define (externalize-expressions x)
  (if (and (eq? (expression-kind x) 'call)
	   (= (length (expression-arguments x)) 1)
	   (eq? (expression-kind (expression-callee x)) 'lambda)
	   ;; This is a real kludge.
	   (not (eq? (expression-parameters (expression-callee x))
		     (unspecified)))
	   (= (length (variables (expression-callee x))) 1)
	   (not (rest? (expression-callee x)))
	   (hunoz? (first (variables (expression-callee x)))))
      (cons (externalize-expression (first (expression-arguments x)))
	    (externalize-expressions (expression-body (expression-callee x))))
      (list (externalize-expression x))))
 ;; conventions: X1
 (let ((x1 (case (expression-kind x)
	    ((null-constant) ''())
	    ((true-constant) #t)
	    ((false-constant) #f)
	    ((char-constant) (expression-constant x))
	    ((fixnum-constant) (expression-constant x))
	    ((flonum-constant) (expression-constant x))
	    ((rectangular-constant) (expression-constant x))
	    ((string-constant) (expression-constant x))
	    ((symbol-constant) `',(expression-constant x))
	    ((pair-constant) `',(undecorate-constant x))
	    ((vector-constant) (undecorate-constant x))
	    ((lambda converted-lambda converted-continuation)
	     (if (noop? x)
		 `(lambda ,(variable-names x))
		 `(lambda ,(variable-names x)
		   ,@(externalize-expressions (expression-body x)))))
	    ((set!)
	     `(set! ,(variable-name (expression-variable x))
		    ,(externalize-expression (expression-source x))))
	    ((if)
	     `(if ,(externalize-expression (expression-antecedent x))
		  ,(externalize-expression (expression-consequent x))
		  ,(externalize-expression (expression-alternate x))))
	    ((primitive-procedure)
	     `(primitive-procedure ,@(expression-constant x)))
	    ((foreign-procedure)
	     `(foreign-procedure ,@(expression-constant x)))
	    ((access) (variable-name (expression-variable x)))
	    ((call converted-call)
	     (cons (externalize-expression (expression-callee x))
		   (map externalize-expression (expression-arguments x))))
	    (else (fuck-up)))))
  `(the ,(if *abbreviate?*
	     (type-set-index (expression-type-set x))
	     (externalize-type-set (expression-type-set x)))
	,x1)))

(define (undecorate x)
 (define (undecorate-constant x)
  (case (expression-kind x)
   ((null-constant) '())
   ((true-constant) #t)
   ((false-constant) #f)
   ((char-constant) (expression-constant x))
   ((fixnum-constant) (expression-constant x))
   ((flonum-constant) (expression-constant x))
   ((rectangular-constant) (expression-constant x))
   ((string-constant) (expression-constant x))
   ((symbol-constant) (expression-constant x))
   ((pair-constant)
    (cons (undecorate-constant (car (expression-constant x)))
	  (undecorate-constant (cdr (expression-constant x)))))
   ((vector-constant) (map-vector undecorate-constant (expression-constant x)))
   (else (fuck-up))))
 (define (undecorate-expressions x)
  (if (and (eq? (expression-kind x) 'call)
	   (= (length (expression-arguments x)) 1)
	   (eq? (expression-kind (expression-callee x)) 'lambda)
	   ;; This is a real kludge.
	   (not (eq? (expression-parameters (expression-callee x))
		     (unspecified)))
	   (= (length (variables (expression-callee x))) 1)
	   (not (rest? (expression-callee x)))
	   (hunoz? (first (variables (expression-callee x)))))
      (cons (undecorate (first (expression-arguments x)))
	    (undecorate-expressions (expression-body (expression-callee x))))
      (list (undecorate x))))
 (case (expression-kind x)
  ((null-constant) ''())
  ((true-constant) #t)
  ((false-constant) #f)
  ((char-constant) (expression-constant x))
  ((fixnum-constant) (expression-constant x))
  ((flonum-constant) (expression-constant x))
  ((rectangular-constant) (expression-constant x))
  ((string-constant) (expression-constant x))
  ((symbol-constant) `',(expression-constant x))
  ((pair-constant) `',(undecorate-constant x))
  ((vector-constant) (undecorate-constant x))
  ((lambda converted-lambda converted-continuation)
   (if (noop? x)
       `(lambda ,(variable-names x))
       `(lambda ,(variable-names x)
	 ,@(undecorate-expressions (expression-body x)))))
  ((set!)
   `(set! ,(variable-name (expression-variable x))
	  ,(undecorate (expression-source x))))
  ((if)
   `(if ,(undecorate (expression-antecedent x))
	,(undecorate (expression-consequent x))
	,(undecorate (expression-alternate x))))
  ((primitive-procedure) `(primitive-procedure ,@(expression-constant x)))
  ((foreign-procedure) `(foreign-procedure ,@(expression-constant x)))
  ((access) (variable-name (expression-variable x)))
  ((call converted-call)
   (cons (undecorate (expression-callee x))
	 (map undecorate (expression-arguments x))))
  (else (fuck-up))))

(define (small? x)
 (define (atoms-in list)
  (if (pair? list) (+ (atoms-in (car list)) (atoms-in (cdr list))) 1))
 (< (atoms-in (undecorate x)) 50))

(define *accounts* '#())

(define (create-accounts! n) (set! *accounts* (make-vector n 0)))

(define (account index thunk)
 (let* ((start (clock-sample))
	(result (thunk))
	(end (clock-sample)))
  (vector-set!
   *accounts* index (+ (vector-ref *accounts* index) (- end start)))
  result))

(define (print-accounts)
 (notify-pp "~s"
	    (let ((sum (reduce-vector + *accounts* 0.0)))
	     (map-vector (lambda (account)
			  (inexact->exact (floor (/ (* 100.0 account) sum))))
			 *accounts*))))

(define (debug-generate c)
 ;; note: This will not handle braces inside comments.
 (let ((backslash? #f)
       (newline? #f)
       (open? #f)
       (state 'code)
       (indent 0))
  ;; conventions: BACKSLASH? STATE INDENT
  (define (generate-char c)
   (case state
    ((code)
     (cond ((char=? c #\") (set! state 'string))
	   ((char=? c #\') (set! state 'char))
	   ((char=? c #\{) (set! indent (+ indent 1)))
	   ((char=? c #\}) (set! indent (- indent 1)))))
    ((string)
     (cond (backslash? (set! backslash? #f))
	   ((char=? c #\\) (set! backslash? #t))
	   ((char=? c #\") (set! state 'code))))
    ((char)
     (cond (backslash? (set! backslash? #f))
	   ((char=? c #\\) (set! backslash? #t))
	   ((char=? c #\') (set! state 'code)))))
   (set! newline? #f)
   (set! open? (char=? c #\{))
   (write-char c))
  (let loop ((c c))
   (cond ((char? c)
	  (unless (char=? c #\newline) (fuck-up))
	  (unless (or newline? open?)
	   (newline)
	   (for-each-n (lambda (i)
			;; conventions: I
			(write-char #\space))
		       indent)
	   (set! newline? #t)))
	 ((string? c)
	  (for-each-n (lambda (i)
		       ;; conventions: I
		       (generate-char (string-ref c i)))
		      (string-length c)))
	 ((c:declaration? c) (loop (third c)))
	 ((c:protect? c) (loop (second c)))
	 ((c:no-return? c) (loop (second c)))
	 ((pair? c) (loop (car c)) (loop (cdr c)))
	 ((null? c) #f)
	 (else (fuck-up))))))

(define (print-counts)
 (notify "~a expression~a"
	 (number->string-of-length (length *xs*) 6)
	 (if (= (length *xs*) 1) "" "s"))
 (notify "~a internal symbol type~a"
	 (number->string-of-length (length *internal-symbol-types*) 6)
	 (if (= (length *internal-symbol-types*) 1) "" "s"))
 (notify "~a external symbol type~a"
	 (number->string-of-length (length *external-symbol-types*) 6)
	 (if (= (length *external-symbol-types*) 1) "" "s"))
 (notify "~a primitive procedure type~a"
	 (number->string-of-length (length *primitive-procedure-types*) 6)
	 (if (= (length *primitive-procedure-types*) 1) "" "s"))
 (notify "~a non-called native procedure type~a"
	 (number->string-of-length
	  (count-if-not called? *native-procedure-types*) 6)
	 (if (one (lambda (u) (not (called? u))) *native-procedure-types*)
	     ""
	     "s"))
 (notify "~a called noop native procedure type~a"
	 (number->string-of-length
	  (count-if (lambda (u) (and (called? u) (noop? u)))
		    *native-procedure-types*)
	  6)
	 (if (one (lambda (u) (and (called? u) (noop? u)))
		  *native-procedure-types*)
	     ""
	     "s"))
 (notify "~a called non-noop native procedure type~a"
	 (number->string-of-length
	  (count-if (lambda (u) (and (called? u) (not (noop? u))))
		    *native-procedure-types*)
	  6)
	 (if (one (lambda (u) (and (called? u) (not (noop? u))))
		  *native-procedure-types*)
	     ""
	     "s"))
 (notify "~a foreign procedure type~a"
	 (number->string-of-length (length *foreign-procedure-types*) 6)
	 (if (= (length *foreign-procedure-types*) 1) "" "s"))
 (notify "~a continuation type~a"
	 (number->string-of-length (length *continuation-types*) 6)
	 (if (= (length *continuation-types*) 1) "" "s"))
 (notify "~a string type~a"
	 (number->string-of-length (length *string-types*) 6)
	 (if (= (length *string-types*) 1) "" "s"))
 (notify "~a structure type~a"
	 (number->string-of-length (length *structure-types*) 6)
	 (if (= (length *structure-types*) 1) "" "s"))
 (notify "~a headed vector type~a"
	 (number->string-of-length (length *headed-vector-types*) 6)
	 (if (= (length *headed-vector-types*) 1) "" "s"))
 (notify "~a nonheaded vector type~a"
	 (number->string-of-length (length *nonheaded-vector-types*) 6)
	 (if (= (length *nonheaded-vector-types*) 1) "" "s"))
 (notify "~a displaced vector type~a"
	 (number->string-of-length (length *displaced-vector-types*) 6)
	 (if (= (length *displaced-vector-types*) 1) "" "s"))
 (notify "~a type set~a"
	 (number->string-of-length (length *ws*) 6)
	 (if (= (length *ws*) 1) "" "s"))
 (notify "~a hunoz variable~a"
	 (number->string-of-length (count-if hunoz? *gs*) 6)
	 (if (one hunoz? *gs*) "" "s"))
 (notify "~a non-hunoz variable~a"
	 (number->string-of-length (count-if-not hunoz? *gs*) 6)
	 (if (one (lambda (g) (not (hunoz? g))) *gs*) "" "s"))
 (notify "~a noop environment~a"
	 (number->string-of-length (count-if noop? *es*) 6)
	 (if (one noop? *es*) "" "s"))
 (notify "~a non-noop environment~a"
	 (number->string-of-length (count-if-not noop? *es*) 6)
	 (if (one (lambda (e) (not (noop? e))) *es*) "" "s")))

(define (global-memory-usage)
 ;; needs work: This is out of date now that we merged Stalin with October.
 ;; This is all very specific to Scheme->C.
 ;; This will overflow without warning at 512M on Linux.
 (define (object-memory-usage object)
  ;; conventions: OBJECT
  ;; Doesn't handle continuations, records, forwarding pointers, and undefined
  ;; objects.
  (cond ((and (number? object) (exact? object)) 0)
	;; Doesn't trace name, value, or property list.
	((symbol? object) (* c-sizeof-s2cuint 5))
	((string? object)
	 (let ((l (+ (string-length object) 1)))
	  (+ c-sizeof-s2cuint
	     l
	     (if (zero? (remainder l c-sizeof-s2cuint))
		 0
		 (- c-sizeof-s2cuint (remainder l c-sizeof-s2cuint))))))
	;; Doesn't trace elements.
	((vector? object)
	 (+ c-sizeof-s2cuint (* c-sizeof-s2cuint (vector-length object))))
	;; Doesn't trace closure.
	((procedure? object) (* c-sizeof-s2cuint 3))
	;; Can be 4 words if doubles must be aligned.
	((and (number? object) (inexact? object)) (* c-sizeof-s2cuint 3))
	((null? object) 0)
	((not object) 0)
	((eq? object #t) 0)
	((char? object) 0)
	((eof-object? object) 0)
	((pair? object) (* c-sizeof-s2cuint 2))
	(else (fuck-up))))
 (define (recursive-object-memory-usage object)
  ;; conventions: OBJECT
  ;; Doesn't handle continuations, records, forwarding pointers, and undefined
  ;; objects.
  (cond ((and (number? object) (exact? object)) 0)
	;; Assume all symbols are interned. Don't count the size of interned
	;; symbols. Assume that symbols don't have values or property lists.
	((symbol? object) 0)
	;; Assume that strings aren't shared.
	((string? object)
	 (let ((l (+ (string-length object) 1)))
	  (+ c-sizeof-s2cuint
	     l
	     (if (zero? (remainder l c-sizeof-s2cuint))
		 0
		 (- c-sizeof-s2cuint (remainder l c-sizeof-s2cuint))))))
	;; Assume that vectors aren't shared.
	((vector? object)
	 (+ c-sizeof-s2cuint
	    (* c-sizeof-s2cuint (vector-length object))
	    (reduce-vector +
			   (map-vector recursive-object-memory-usage object)
			   0)))
	;; Assume that procedures aren't shared.
	;; Doesn't trace closure.
	((procedure? object) (* c-sizeof-s2cuint 3))
	;; Can be 4 words if doubles must be aligned.
	((and (number? object) (inexact? object)) (* c-sizeof-s2cuint 3))
	((null? object) 0)
	((not object) 0)
	((eq? object #t) 0)
	((char? object) 0)
	((eof-object? object) 0)
	;; Assume that pairs aren't shared.
	((pair? object)
	 (+ (* c-sizeof-s2cuint 2)
	    (recursive-object-memory-usage (car object))
	    (recursive-object-memory-usage (cdr object))))
	(else (fuck-up))))
 (define (flat-memory-usage object)
  ;; conventions: OBJECT
  (cond ((and (number? object) (exact? object)) 0)
	;; Assume all symbols are interned. Don't count the size of interned
	;; symbols. Assume that symbols don't have values or property lists.
	((symbol? object) 0)
	;; Assume that strings aren't shared.
	((string? object)
	 (let ((l (+ (string-length object) 1)))
	  (+ c-sizeof-s2cuint
	     l
	     (if (zero? (remainder l c-sizeof-s2cuint))
		 0
		 (- c-sizeof-s2cuint (remainder l c-sizeof-s2cuint))))))
	;; Assume that vectors aren't shared. Count the size of elements
	;; elsewhere.
	((vector? object)
	 (+ c-sizeof-s2cuint (* c-sizeof-s2cuint (vector-length object))))
	;; Assume that no top-level variable has a procedure as its value.
	((procedure? object) (fuck-up))
	;; Can be 4 words if doubles must be aligned.
	((and (number? object) (inexact? object)) (* c-sizeof-s2cuint 3))
	((null? object) 0)
	((not object) 0)
	((eq? object #t) 0)
	((char? object) 0)
	((eof-object? object) 0)
	((pair? object)
	 ;; Assume that pairs aren't shared. Count the size of the car slots
	 ;; elsewhere.
	 (+ (* c-sizeof-s2cuint 2) (flat-memory-usage (cdr object))))
	(else (fuck-up))))
 (define (scalar-one-level-memory-usage object)
  ;; conventions: OBJECT
  ;; Assume all top-level scalar variables hold vectors, symbols, or #F.
  ;; Assume all symbols are interned. Don't count the size of interned
  ;; symbols. Assume that symbols don't have values or property lists.
  (unless (or (not object) (symbol? object) (vector? object)) (fuck-up))
  ;; Assume that vectors aren't shared.
  (if (vector? object)
      (+ c-sizeof-s2cuint
	 (* c-sizeof-s2cuint (vector-length object))
	 (reduce-vector + (map-vector flat-memory-usage object) 0))
      0))
 (define (list-one-level-memory-usage object)
  ;; conventions: OBJECT
  ;; Assume all top-level list variables hold lists or #F.
  (unless (or (not object) (list? object)) (fuck-up))
  ;; Assume that pairs aren't shared.
  (if object
      (+ (* c-sizeof-s2cuint 2 (length object))
	 (reduce + (map scalar-one-level-memory-usage object) 0))
      0))
 ;; The following aren't counted:
 ;;   *PRIMITIVE-PROCEDURE-REWRITES*   quote
 ;;   *ERRORS*                         quote
 ;;   *MACROS*                         computed at startup
 ;;   *READ*                           backquote
 ;;   *I/O*                            backquote
 ;;   *Scheme->C-compatibility-macros* computed at startup
 ;;   *Xlib-and-GL-macros*             computed at startup
 ;;   *QobiScheme-macros*              computed at startup
 ;;   *Trotsky-macros*                 computed at startup
 (+ (flat-memory-usage *types-frozen?*)
    (flat-memory-usage *again?*)
    (flat-memory-usage *xi*)
    (list-one-level-memory-usage *xs*)
    (flat-memory-usage *calls*)		;subsumed by *XS*
    (flat-memory-usage *accesses*)	;subsumed by *XS*
    (flat-memory-usage *assignments*)	;subsumed by *XS*
    (flat-memory-usage *references*)	;subsumed by *XS*
    ;; *X* is contained in *XS*
    ;; *X1* is contained in *XS*
    (flat-memory-usage *ui*)
    (scalar-one-level-memory-usage <null>)
    (flat-memory-usage *null-type-used?*)
    (flat-memory-usage *null-type-use-count*)
    (scalar-one-level-memory-usage <true>)
    (flat-memory-usage *true-type-used?*)
    (flat-memory-usage *true-type-use-count*)
    (scalar-one-level-memory-usage <false>)
    (flat-memory-usage *false-type-used?*)
    (flat-memory-usage *false-type-use-count*)
    (scalar-one-level-memory-usage <char>)
    (flat-memory-usage *char-type-used?*)
    (flat-memory-usage *char-type-use-count*)
    (scalar-one-level-memory-usage <fixnum>)
    (flat-memory-usage *fixnum-type-used?*)
    (flat-memory-usage *fixnum-type-use-count*)
    (scalar-one-level-memory-usage <flonum>)
    (flat-memory-usage *flonum-type-used?*)
    (flat-memory-usage *flonum-type-use-count*)
    (scalar-one-level-memory-usage <rectangular>)
    (flat-memory-usage *rectangular-type-used?*)
    (flat-memory-usage *rectangular-type-use-count*)
    (scalar-one-level-memory-usage <input-port>)
    (flat-memory-usage *input-port-type-used?*)
    (flat-memory-usage *input-port-type-use-count*)
    (scalar-one-level-memory-usage <output-port>)
    (flat-memory-usage *output-port-type-used?*)
    (flat-memory-usage *output-port-type-use-count*)
    (scalar-one-level-memory-usage <eof-object>)
    (flat-memory-usage *eof-object-type-used?*)
    (flat-memory-usage *eof-object-type-use-count*)
    (scalar-one-level-memory-usage <pointer>)
    (flat-memory-usage *pointer-type-used?*)
    (flat-memory-usage *pointer-type-use-count*)
    (list-one-level-memory-usage *internal-symbol-types*)
    (list-one-level-memory-usage *external-symbol-types*)
    (list-one-level-memory-usage *primitive-procedure-types*)
    (list-one-level-memory-usage *native-procedure-types*)
    (list-one-level-memory-usage *foreign-procedure-types*)
    (list-one-level-memory-usage *continuation-types*)
    (list-one-level-memory-usage *string-types*)
    ;; <NONRECLAIMABLE-STRING> is contained in *STRING-TYPES*
    (list-one-level-memory-usage *structure-types*)
    (list-one-level-memory-usage *headed-vector-types*)
    (list-one-level-memory-usage *nonheaded-vector-types*)
    ;; <TOP-LEVEL-NONHEADED-VECTOR> is contained in *NONHEADED-VECTOR-TYPES*
    (list-one-level-memory-usage *displaced-vector-types*)
    (flat-memory-usage *wi*)
    (list-one-level-memory-usage *ws*)
    ;; *W0* is contained in *WS*
    ;; *W1* is contained in *WS*
    ;; *W* is contained in *WS*
    ;; *VOID* is contained in *WS*
    ;; *NULL* is contained in *WS*
    ;; *INPUT-PORT* is contained in *WS*
    ;; *OUTPUT-PORT* is contained in *WS*
    ;; *FOREIGN-CHAR-TYPE-SET* is contained in *WS*
    ;; *FOREIGN-FIXNUM-TYPE-SET* is contained in *WS*
    ;; *FOREIGN-FLONUM-TYPE-SET* is contained in *WS*
    ;; *FOREIGN-STRING-TYPE-SET* is contained in *WS*
    ;; *FOREIGN-INPUT-PORT-TYPE-SET* is contained in *WS*
    ;; *FOREIGN-OUTPUT-PORT-TYPE-SET* is contained in *WS*
    ;; *FOREIGN-POINTER-TYPE-SET* is contained in *WS*
    (flat-memory-usage *gi*)
    (list-one-level-memory-usage *gs*)
    (flat-memory-usage *ei*)
    (list-one-level-memory-usage *es*)
    (list-one-level-memory-usage *es0*)
    (flat-memory-usage *y*)		;not counted
    (list-one-level-memory-usage *ys*)	;not fully counted
    (flat-memory-usage *program-has-heap?*)
    (flat-memory-usage *abbreviate?*)
    (recursive-object-memory-usage *accounts*)
    (flat-memory-usage *char*)
    (flat-memory-usage *fixnum*)
    (flat-memory-usage *flonum*)
    (flat-memory-usage *length*)
    (flat-memory-usage *tag*)
    (flat-memory-usage *squished*)
    (flat-memory-usage *signed-squished*)
    (flat-memory-usage *file*)
    (flat-memory-usage *jmpbuf*)
    (flat-memory-usage *char-alignment*)
    (flat-memory-usage *fixnum-alignment*)
    (flat-memory-usage *flonum-alignment*)
    (flat-memory-usage *pointer-alignment*)
    (flat-memory-usage *length-alignment*)
    (flat-memory-usage *tag-alignment*)
    (flat-memory-usage *squished-alignment*)
    (flat-memory-usage *file-alignment*)
    (flat-memory-usage *jmpbuf-alignment*)
    (flat-memory-usage *char-size*)
    (flat-memory-usage *fixnum-size*)
    (flat-memory-usage *flonum-size*)
    (flat-memory-usage *pointer-size*)
    (flat-memory-usage *length-size*)
    (flat-memory-usage *tag-size*)
    (flat-memory-usage *squished-size*)
    (flat-memory-usage *worst-alignment*)
    (flat-memory-usage *allocation-alignment*)
    (flat-memory-usage *char-alignment?*)
    (flat-memory-usage *fixnum-alignment?*)
    (flat-memory-usage *flonum-alignment?*)
    (flat-memory-usage *rectangular-alignment?*)
    (flat-memory-usage *void*-alignment?*)
    (flat-memory-usage *char*-alignment?*)
    (flat-memory-usage *file*-alignment?*)
    (flat-memory-usage *jmpbuf*-alignment?*)
    (flat-memory-usage *length-alignment?*)
    (flat-memory-usage *tag-alignment?*)
    (flat-memory-usage *squished-alignment?*)
    (flat-memory-usage *file-alignment?*)
    (flat-memory-usage *jmpbuf-alignment?*)
    (flat-memory-usage *char-size?*)
    (flat-memory-usage *fixnum-size?*)
    (flat-memory-usage *flonum-size?*)
    (flat-memory-usage *rectangular-size?*)
    (flat-memory-usage *void*-size?*)
    (flat-memory-usage *char*-size?*)
    (flat-memory-usage *file*-size?*)
    (flat-memory-usage *jmpbuf*-size?*)
    (flat-memory-usage *length-size?*)
    (flat-memory-usage *tag-size?*)
    (flat-memory-usage *squished-size?*)
    (flat-memory-usage *uss*)		;subsumed elsewhere
    (flat-memory-usage *strings*)	;not counted
    (flat-memory-usage *symbols*)	;not counted
    (flat-memory-usage *outside-main*)	;not counted
    (flat-memory-usage *inside-main*)	;not counted
    (flat-memory-usage *outside-body*)	;not counted
    (flat-memory-usage *discard*)
    (flat-memory-usage *errors-used*)	;not counted
    (flat-memory-usage *warnings*)	;not counted
    (flat-memory-usage *ti*)
    (flat-memory-usage *statements-per-constant-initialization-procedure*)
    (flat-memory-usage *li*)
    (flat-memory-usage *primitive-procedure-handlers*) ;not counted
    (recursive-object-memory-usage *list->vector*)
    (recursive-object-memory-usage *append*)
    (recursive-object-memory-usage *cons*)
    (recursive-object-memory-usage *eqv?*)
    (flat-memory-usage *c:noreturn?*)
    (flat-memory-usage *c:c?*)
    (flat-memory-usage *c:panic?*)
    (flat-memory-usage *c:backtrace?*)
    (flat-memory-usage *c:backtrace-internal?*)
    (flat-memory-usage *c:ipow?*)
    (flat-memory-usage *c:input-waiting?*)
    (flat-memory-usage *p7?*)
    (flat-memory-usage *closure-representation*)
    (flat-memory-usage *type-if?*)
    (flat-memory-usage *immediate-structures?*)
    (flat-memory-usage *bounds-checks?*)
    (flat-memory-usage *memory-checks?*)
    (flat-memory-usage *overflow-checks?*)
    (flat-memory-usage *type-checks?*)
    (flat-memory-usage *runtime-checks?*)
    (flat-memory-usage *heap-allocation?*)
    (flat-memory-usage *stack-allocation?*)
    (flat-memory-usage *region-allocation?*)
    (flat-memory-usage *memory-messages?*)
    (flat-memory-usage *globals?*)
    (flat-memory-usage *expandable-regions?*)
    (flat-memory-usage *forgery?*)
    (flat-memory-usage *eq?-forgery?*)
    (flat-memory-usage *uniqueness?*)
    (flat-memory-usage *align-strings?*)
    (flat-memory-usage *treat-all-symbols-as-external?*)
    (flat-memory-usage *index-allocated-string-types-by-expression?*)
    (flat-memory-usage *index-constant-structure-types-by-slot-types?*)
    (flat-memory-usage *index-constant-structure-types-by-expression?*)
    (flat-memory-usage *index-allocated-structure-types-by-slot-types?*)
    (flat-memory-usage *index-allocated-structure-types-by-expression?*)
    (flat-memory-usage *index-constant-headed-vector-types-by-element-type?*)
    (flat-memory-usage *index-constant-headed-vector-types-by-expression?*)
    (flat-memory-usage *index-allocated-headed-vector-types-by-element-type?*)
    (flat-memory-usage *index-allocated-headed-vector-types-by-expression?*)
    (flat-memory-usage
     *index-constant-nonheaded-vector-types-by-element-type?*)
    (flat-memory-usage *index-constant-nonheaded-vector-types-by-expression?*)
    (flat-memory-usage
     *index-allocated-nonheaded-vector-types-by-element-type?*)
    (flat-memory-usage *index-allocated-nonheaded-vector-types-by-expression?*)
    (recursive-object-memory-usage *include-path*)
    (recursive-object-memory-usage *includes*)
    (recursive-object-memory-usage *herald*)
    (recursive-object-memory-usage *heralds*)
    (recursive-object-memory-usage *program-has-pthreads?*)))

(define (print-global-memory-usage)
 (let* ((c1 (global-memory-usage))
	(c4 (cond ((>= c1 1048576) (inexact->exact (ceiling (/ c1 1048576))))
		  ((>= c1 1024) (inexact->exact (ceiling (/ c1 1024))))
		  (else c1)))
	(c5 (cond ((>= c1 1048576) "M")
		  ((>= c1 1024) "K")
		  (else ""))))
  ;; conventions: C1 C4 C5
  (notify "Stalin thinks ~s~a byte~a in use" c4 c5 (if (= c1 1) " is" "s are"))
  (collect-all)
  (let* ((c2 (first (collect-info)))
	 (c4 (cond ((>= c2 1048576) (inexact->exact (ceiling (/ c2 1048576))))
		   ((>= c2 1024) (inexact->exact (ceiling (/ c2 1024))))
		   (else c2)))
	 (c5 (cond ((>= c2 1048576) "M")
		   ((>= c2 1024) "K")
		   (else ""))))
   ;; conventions: C2 C4 C5
   (notify
    "Scheme->C thinks ~s~a byte~a in use" c4 c5 (if (= c2 1) " is" "s are"))
   (let* ((c3 (- c1 c2))
	  (c4 (cond ((>= (abs c3) 1048576)
		     (inexact->exact (ceiling (/ (abs c3) 1048576))))
		    ((>= (abs c3) 1024)
		     (inexact->exact (ceiling (/ (abs c3) 1024))))
		    (else (abs c3))))
	  (c5 (cond ((>= (abs c3) 1048576) "M")
		    ((>= (abs c3) 1024) "K")
		    (else ""))))
    (unless (zero? c3)
     (notify "~s~a byte~a too ~a"
	     c4 c5
	     (if (= (abs c3) 1) "" "s")
	     (if (negative? c3) "few" "many")))))))

(define (print-number-of-call-sites-that-dispatch-on-clones)
 (let ((n (count-if
	   (lambda (x)
	    (and (executed? x)
		 (let ((us (members
			    (expression-type-set (expression-callee x)))))
		  (some (lambda (u1)
			 (some (lambda (u2)
				(and (not (eq? u1 u2))
				     (native-procedure-type? u1)
				     (called? u1)
				     (native-procedure-type? u2)
				     (called? u2)
				     (wide-clones? u1 u2)))
			       us))
			us))))
	   ;; needs work: Doesn't handle implicit call sites.
	   *calls*)))
  (notify
   "~s call site~a on clones" n (if (= n 1) " dispatches" "s dispatch"))))

(define (print-maximal-non-let-lexical-nesting-depth)
 (notify "Maximal non-LET lexical nesting depth is ~s"
	 (reduce max
		 (map non-let-lexical-nesting-depth
		      (remove-if-not environment-used? *es*))
		 0)))

(define (print-maximal-clone-rate)
 (notify "Maximal clone rate is ~s"
	 (reduce
	  max
	  (map length
	       (equivalence-classesq
		(map wide-prototype (remove-if-not environment-used? *es*))))
	  0)))

(define (print-clone-rates)
 (for-each
  (lambda (e)
   (notify "~a ~a ~a ~a"
	   (number->string-of-length (lexical-nesting-depth e) 5)
	   (number->string-of-length (non-let-lexical-nesting-depth e) 5)
	   (number->string-of-length (count-if called? (wide-clones e)) 5)
	   (environment-name e)))
  (remove-if-not environment-used? *es*)))

(define (print-escaping-type-counts)
 (let ((n (reduce +
		  (map (lambda (e)
			(if (eq? (environment-escaping-types e) (unspecified))
			    0
			    (count-if native-procedure-type?
				      (environment-escaping-types e))))
		       *es*)
		  0)))
  (notify "~s escaping native procedure type~a" n (if (= n 1) "" "s")))
 (let ((n (reduce +
		  (map (lambda (e)
			(if (eq? (environment-escaping-types e) (unspecified))
			    0
			    (count-if continuation-type?
				      (environment-escaping-types e))))
		       *es*)
		  0)))
  (notify "~s escaping continuation type~a" n (if (= n 1) "" "s")))
 (let ((n (reduce +
		  (map (lambda (e)
			(if (eq? (environment-escaping-types e) (unspecified))
			    0
			    (count-if string-type?
				      (environment-escaping-types e))))
		       *es*)
		  0)))
  (notify "~s escaping string type~a" n (if (= n 1) "" "s")))
 (let ((n (reduce +
		  (map (lambda (e)
			(if (eq? (environment-escaping-types e) (unspecified))
			    0
			    (count-if structure-type?
				      (environment-escaping-types e))))
		       *es*)
		  0)))
  (notify "~s escaping structure type~a" n (if (= n 1) "" "s")))
 (let ((n (reduce +
		  (map (lambda (e)
			(if (eq? (environment-escaping-types e) (unspecified))
			    0
			    (count-if headed-vector-type?
				      (environment-escaping-types e))))
		       *es*)
		  0)))
  (notify "~s escaping headed vector type~a" n (if (= n 1) "" "s")))
 (let ((n (reduce +
		  (map (lambda (e)
			(if (eq? (environment-escaping-types e) (unspecified))
			    0
			    (count-if nonheaded-vector-type?
				      (environment-escaping-types e))))
		       *es*)
		  0)))
  (notify "~s escaping nonheaded vector type~a" n (if (= n 1) "" "s")))
 (let ((n (reduce +
		  (map (lambda (e)
			(if (eq? (environment-escaping-types e) (unspecified))
			    0
			    (length (environment-escaping-types e))))
		       *es*)
		  0)))
  (notify "~s total escaping type~a" n (if (= n 1) "" "s"))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
