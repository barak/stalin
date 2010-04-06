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
(module stalin1)

(include "QobiScheme.sch")
(include "stalin1.sch")

(define (no-cursor) #f)

(define (no-version) #f)
;;; End delete for Trotsky

;;; Structure definitions

(define-structure s-expression
 ;; The slots EXPANSION and MACROEXPAND-BODY are just for efficiency.
 version				;version
 cursor					;cursor
 pathname				;string #f
 line-position				;integer
 character-position			;integer
 character-position-within-line		;integer
 comments				;strings
 expansion				;s #f
 macroexpand-body			;s #f
 datum)					;q

(define-structure program-point
 before?				;#t #f
 expression)				;e

(define-structure expression
 kind					;symbol
 version				;version
 cursor					;cursor
 pathname				;string #f
 line-position				;integer
 character-position			;integer
 character-position-within-line		;integer
 index					;i
 link					;x #t #f
 environment				;e #f
 type-set				;w
 parent					;x #f
 constant				;q
 lambda-environment			;e
 parameters				;(union null g (pair g ^2))
 body					;x #f
 variable				;g
 source					;x
 antecedent				;x
 consequent				;x
 alternate				;x
 callee					;x
 arguments				;xs
 original-expression			;x #f
 result					;r
 type-allocation-alist			;(list* u-(e|'stack|'heap))
 continuation-type			;continuation-type #f
 string-type				;string-type #f
 structure-types			;structure-types
 headed-vector-types			;headed-vector-types
 nonheaded-vector-types			;nonheaded-vector-types
 booleans)

(define-structure result
 kind					;symbol
 environment				;e
 type-set				;w
 c					;c
 l1					;c
 l2					;c
 l0)					;c

(define-structure internal-symbol-type
 name					;symbol
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 booleans)

(define-structure external-symbol-type
 displaced-string-type			;string-type
 link					;external-symbol-type
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 booleans)

(define-structure primitive-procedure-type
 name					;symbol
 arguments				;(list* object)
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 booleans)

(define-structure native-procedure-type
 call-site-environment-alist		;(list* (union y #f)-e)
 narrow-prototype			;e #f
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 booleans)

(define-structure foreign-procedure-type
 name					;string
 ;; The next entry is somewhat misnamed because it is not a list of variables.
 parameters				;fs
 ;; The next entry is somewhat misnamed because it is not a result.
 result					;f
 include				;string #f
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 booleans)

(define-structure continuation-type
 allocating-expression			;x #f
 index					;i
 use-count				;integer
 call-sites				;ys
 types-and-type-sets-that-directly-point-to ;u/ws
 booleans)

(define-structure string-type
 allocating-expressions			;(list+ (union x #f))
 link					;string-type
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 external-symbol-type			;external-symbol-type #f
 booleans)

(define-structure structure-type
 name					;symbol
 slots					;ws
 allocating-expressions			;xs
 link					;structure-type
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 structure-ref-accessed?		;(list* (union #t #f))
 booleans)

(define-structure headed-vector-type
 element				;w
 allocating-expressions			;xs
 link					;headed-vector-type
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 displaced-vector-type			;displaced-vector-type #f
 booleans)

(define-structure nonheaded-vector-type
 element				;w
 allocating-expressions			;(list+ (union x #f))
 link					;nonheaded-vector-type
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 displaced-vector-type			;displaced-vector-type #f
 booleans)

(define-structure displaced-vector-type
 displaced-vector-type			;vector-type
 link					;displaced-vector-type
 index					;i
 use-count				;integer
 types-and-type-sets-that-directly-point-to ;u/ws
 booleans)

(define-structure red-black-tree-node type key left right red?)

(define-structure type-set
 location				;x g u #f
 red-black-tree-node			;#f red-black-tree-node
 link					;w
 minimal-alignment			;integer
 index					;i
 booleans)

(define-structure variable
 version				;version
 cursor					;cursor
 pathname				;string #f
 line-position				;integer
 character-position			;integer
 character-position-within-line		;integer
 index					;i
 name					;symbol
 environment				;e
 type-set				;w
 accesses				;xs
 assignments				;xs
 references				;xs
 booleans)

(define-structure environment
 ;; needs work: The following comment is out of date.
 ;; The slots QUICK-PARENT, PARENT-PARAMETER, PARENT-SLOT, ANCESTORS,
 ;; DESCENDENTS, and IN-LINED-ENVIRONMENTS, inter alia, are just for
 ;; efficiency.
 index					;i
 expression				;x
 name					;string
 split					;#t #f 'never
 call-sites				;(list* (union y #f))
 allocation				;e 'stack 'heap
 distance-from-root			;integer
 free-variables				;gs
 quick-parent				;e #f
 parent-parameter			;e #f
 parent-slot				;e #f
 ancestors				;es
 descendents				;es
 properly-in-lined-environments		;es
 narrow-prototype			;e
 narrow-clones				;es
 wide-prototype				;e
 direct-tail-callers			;es
 direct-non-tail-callers		;es
 direct-tail-callees			;es
 direct-non-tail-callees		;es
 blocked-environments			;es
 expressions				;xs
 continuation-calls			;xs
 escaping-types				;us
 non-self-tail-call-sites		;ys
 booleans)

(define-structure call-site
 expression				;x
 offsets)				;(list* symbol)

;;; GENSYM

;;; Begin delete for Trotsky
(define (gensym string) (string->uninterned-symbol string))
;;; End delete for Trotsky

;;; Global variables

(define *types-frozen?* #f)
(define *during-closure-conversion?* #f)
(define *again?* #f)

(define (unused) 'unused)

(define (unspecified) 'unspecified)

;;; S-Expression creation

(define (create-s-expression pathname
			     line-position
			     character-position
			     character-position-within-line
			     comments
			     datum)
 (make-s-expression (no-version)
		    (no-cursor)
		    pathname
		    line-position
		    character-position
		    character-position-within-line
		    comments
		    #f
		    #f
		    datum))

(define (create-anonymous-s-expression datum)
 (create-s-expression #f (unused) (unused) (unused) '() datum))

(define (create-october-s-expression version cursor datum)
 (make-s-expression version cursor #f #f #f #f '() #f #f datum))

;;; Expression creation

(define *xi* #f)
(define *xs* #f)
(define *calls* #f)
(define *accesses* #f)
(define *assignments* #f)
(define *references* #f)
(define *x* #f)
(define *x1* #f)

(define (initialize-expressions!)
 (set! *xi* 0)
 (set! *xs* '())
 (set! *calls* '())
 (set! *accesses* '())
 (set! *assignments* '())
 (set! *references* '())
 (set! *x1* #f))

(define (create-expression kind s/x q)
 (let ((x (cond
	   ((s-expression? s/x)
	    (make-expression
	     kind
	     (s-expression-version s/x)
	     (s-expression-cursor s/x)
	     (s-expression-pathname s/x)
	     (s-expression-line-position s/x)
	     (s-expression-character-position s/x)
	     (s-expression-character-position-within-line s/x)
	     *xi* #f (unspecified) (unspecified) (unspecified) q (unused)
	     (unused) (unused) (unused) (unused) (unused) (unused) (unused)
	     (unused) (unused) #f (unspecified) '() #f #f '() '() '() 0))
	   ((expression? s/x)
	    (make-expression
	     kind
	     (expression-version s/x)
	     (expression-cursor s/x)
	     (expression-pathname s/x)
	     (expression-line-position s/x)
	     (expression-character-position s/x)
	     (expression-character-position-within-line s/x)
	     *xi* #f (unspecified) (unspecified) (unspecified) q (unused)
	     (unused) (unused) (unused) (unused) (unused) (unused) (unused)
	     (unused) (unused) s/x (unspecified) '() #f #f '() '() '() 0))
	   (else
	    (make-expression
	     kind (no-version) (no-cursor) #f #f #f #f
	     *xi* #f (unspecified) (unspecified) (unspecified) q (unused)
	     (unused) (unused) (unused) (unused) (unused) (unused) (unused)
	     (unused) (unused) #f (unspecified) '() #f #f '() '() '() 0)))))
  (set! *xi* (+ *xi* 1))
  (set! *xs* (cons x *xs*))
  x))

(define (create-call-expression s/x callee arguments)
 (let ((x (cond
	   ((s-expression? s/x)
	    (make-expression
	     'call
	     (s-expression-version s/x)
	     (s-expression-cursor s/x)
	     (s-expression-pathname s/x)
	     (s-expression-line-position s/x)
	     (s-expression-character-position s/x)
	     (s-expression-character-position-within-line s/x)
	     *xi* #f (unspecified) (unspecified) (unspecified) (unused)
	     (unused) (unused) (unused) (unused) (unused) (unused) (unused)
	     (unused) callee arguments #f (unspecified) '() #f #f '() '() '()
	     0))
	   ((expression? s/x)
	    (make-expression
	     'call
	     (expression-version s/x)
	     (expression-cursor s/x)
	     (expression-pathname s/x)
	     (expression-line-position s/x)
	     (expression-character-position s/x)
	     (expression-character-position-within-line s/x)
	     *xi* #f (unspecified) (unspecified) (unspecified) (unused)
	     (unused) (unused) (unused) (unused) (unused) (unused) (unused)
	     (unused) callee arguments s/x (unspecified) '() #f #f '() '() '()
	     0))
	   (else
	    (make-expression
	     'call (no-version) (no-cursor) #f #f #f #f
	     *xi* #f (unspecified) (unspecified) (unspecified) (unused)
	     (unused) (unused) (unused) (unused) (unused) (unused) (unused)
	     (unused) callee arguments #f (unspecified) '() #f #f '() '() '()
	     0)))))
  (set! *xi* (+ *xi* 1))
  (set! *xs* (cons x *xs*))
  (set! *calls* (cons x *calls*))
  x))

(define (create-converted-call-expression s/x callee arguments)
 (let ((x (cond
	   ((s-expression? s/x)
	    (make-expression
	     'converted-call
	     (s-expression-version s/x)
	     (s-expression-cursor s/x)
	     (s-expression-pathname s/x)
	     (s-expression-line-position s/x)
	     (s-expression-character-position s/x)
	     (s-expression-character-position-within-line s/x)
	     *xi* #f (unspecified) (unspecified) (unspecified) (unused)
	     (unused) (unused) (unused) (unused) (unused) (unused) (unused)
	     (unused) callee arguments #f (unspecified) '() #f #f '() '() '()
	     0))
	   ((expression? s/x)
	    (make-expression
	     'converted-call
	     (expression-version s/x)
	     (expression-cursor s/x)
	     (expression-pathname s/x)
	     (expression-line-position s/x)
	     (expression-character-position s/x)
	     (expression-character-position-within-line s/x)
	     *xi* #f (unspecified) (unspecified) (unspecified) (unused)
	     (unused) (unused) (unused) (unused) (unused) (unused) (unused)
	     (unused) callee arguments s/x (unspecified) '() #f #f '() '() '()
	     0))
	   (else
	    (make-expression
	     'converted-call (no-version) (no-cursor) #f #f #f #f
	     *xi* #f (unspecified) (unspecified) (unspecified) (unused)
	     (unused) (unused) (unused) (unused) (unused) (unused) (unused)
	     (unused) callee arguments #f (unspecified) '() #f #f '() '() '()
	     0)))))
  (set! *xi* (+ *xi* 1))
  (set! *xs* (cons x *xs*))
  (set! *calls* (cons x *calls*))
  x))

(define (create-access-expression s/x variable)
 (let ((x (cond ((s-expression? s/x)
		 (make-expression
		  'access
		  (s-expression-version s/x)
		  (s-expression-cursor s/x)
		  (s-expression-pathname s/x)
		  (s-expression-line-position s/x)
		  (s-expression-character-position s/x)
		  (s-expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  (unused) (unused) (unused) variable (unused) (unused)
		  (unused) (unused) (unused) (unused) #f (unspecified) '()
		  #f #f '() '() '() 0))
		((expression? s/x)
		 (make-expression
		  'access
		  (expression-version s/x)
		  (expression-cursor s/x)
		  (expression-pathname s/x)
		  (expression-line-position s/x)
		  (expression-character-position s/x)
		  (expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  (unused) (unused) (unused) variable (unused) (unused)
		  (unused) (unused) (unused) (unused) s/x (unspecified) '()
		  #f #f '() '() '() 0))
		(else
		 (make-expression
		  'access (no-version) (no-cursor) #f #f #f #f
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  (unused) (unused) (unused) variable (unused) (unused)
		  (unused) (unused) (unused) (unused) #f (unspecified) '()
		  #f #f '() '() '() 0)))))
  (set! *xi* (+ *xi* 1))
  (set! *xs* (cons x *xs*))
  (set! *accesses* (cons x *accesses*))
  (set! *references* (cons x *references*))
  x))

(define (create-lambda-expression
	 s/x lambda-environment parameters expression)
 (let ((x (cond ((s-expression? s/x)
		 (make-expression
		  'lambda
		  (s-expression-version s/x)
		  (s-expression-cursor s/x)
		  (s-expression-pathname s/x)
		  (s-expression-line-position s/x)
		  (s-expression-character-position s/x)
		  (s-expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  lambda-environment parameters expression (unused) (unused)
		  (unused) (unused) (unused) (unused) (unused)
		  #f (unspecified) '() #f #f '() '() '() 0))
		((expression? s/x)
		 (make-expression
		  'lambda
		  (expression-version s/x)
		  (expression-cursor s/x)
		  (expression-pathname s/x)
		  (expression-line-position s/x)
		  (expression-character-position s/x)
		  (expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  lambda-environment parameters expression (unused) (unused)
		  (unused) (unused) (unused) (unused) (unused)
		  s/x (unspecified) '() #f #f '() '() '() 0))
		(else
		 (make-expression
		  'lambda (no-version) (no-cursor) #f #f #f #f
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  lambda-environment parameters expression (unused) (unused)
		  (unused) (unused) (unused) (unused) (unused)
		  #f (unspecified) '() #f #f '() '() '() 0)))))
  (set-environment-expression! lambda-environment x)
  (set! *xi* (+ *xi* 1))
  (set! *xs* (cons x *xs*))
  x))

(define (create-converted-lambda-expression
	 s/x lambda-environment parameters expression)
 (let ((x (cond ((s-expression? s/x)
		 (make-expression
		  'converted-lambda
		  (s-expression-version s/x)
		  (s-expression-cursor s/x)
		  (s-expression-pathname s/x)
		  (s-expression-line-position s/x)
		  (s-expression-character-position s/x)
		  (s-expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  lambda-environment parameters expression (unused) (unused)
		  (unused) (unused) (unused) (unused) (unused)
		  #f (unspecified) '() #f #f '() '() '() 0))
		((expression? s/x)
		 (make-expression
		  'converted-lambda
		  (expression-version s/x)
		  (expression-cursor s/x)
		  (expression-pathname s/x)
		  (expression-line-position s/x)
		  (expression-character-position s/x)
		  (expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  lambda-environment parameters expression (unused) (unused)
		  (unused) (unused) (unused) (unused) (unused)
		  s/x (unspecified) '() #f #f '() '() '() 0))
		(else
		 (make-expression
		  'converted-lambda (no-version) (no-cursor) #f #f #f #f
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  lambda-environment parameters expression (unused) (unused)
		  (unused) (unused) (unused) (unused) (unused)
		  #f (unspecified) '() #f #f '() '() '() 0)))))
  (set-environment-expression! lambda-environment x)
  (set! *xi* (+ *xi* 1))
  (set! *xs* (cons x *xs*))
  x))

(define (create-converted-continuation-expression
	 s/x lambda-environment parameters expression)
 (let ((x (cond ((s-expression? s/x)
		 (make-expression
		  'converted-continuation
		  (s-expression-version s/x)
		  (s-expression-cursor s/x)
		  (s-expression-pathname s/x)
		  (s-expression-line-position s/x)
		  (s-expression-character-position s/x)
		  (s-expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  lambda-environment parameters expression (unused) (unused)
		  (unused) (unused) (unused) (unused) (unused)
		  #f (unspecified) '() #f #f '() '() '() 0))
		((expression? s/x)
		 (make-expression
		  'converted-continuation
		  (expression-version s/x)
		  (expression-cursor s/x)
		  (expression-pathname s/x)
		  (expression-line-position s/x)
		  (expression-character-position s/x)
		  (expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  lambda-environment parameters expression (unused) (unused)
		  (unused) (unused) (unused) (unused) (unused)
		  s/x (unspecified) '() #f #f '() '() '() 0))
		(else
		 (make-expression
		  'converted-continuation (no-version) (no-cursor) #f #f #f #f
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  lambda-environment parameters expression (unused) (unused)
		  (unused) (unused) (unused) (unused) (unused)
		  #f (unspecified) '() #f #f '() '() '() 0)))))
  (set-environment-expression! lambda-environment x)
  (set! *xi* (+ *xi* 1))
  (set! *xs* (cons x *xs*))
  x))

(define (create-set!-expression s/x variable source)
 (let ((x (cond ((s-expression? s/x)
		 (make-expression
		  'set!
		  (s-expression-version s/x)
		  (s-expression-cursor s/x)
		  (s-expression-pathname s/x)
		  (s-expression-line-position s/x)
		  (s-expression-character-position s/x)
		  (s-expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  (unused) (unused) (unused) variable source (unused) (unused)
		  (unused) (unused) (unused) #f (unspecified) '()
		  #f #f '() '() '() 0))
		((expression? s/x)
		 (make-expression
		  'set!
		  (expression-version s/x)
		  (expression-cursor s/x)
		  (expression-pathname s/x)
		  (expression-line-position s/x)
		  (expression-character-position s/x)
		  (expression-character-position-within-line s/x)
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  (unused) (unused) (unused) variable source (unused) (unused)
		  (unused) (unused) (unused) s/x (unspecified) '()
		  #f #f '() '() '() 0))
		(else
		 (make-expression
		  'set! (no-version) (no-cursor) #f #f #f #f
		  *xi* #f (unspecified) (unspecified) (unspecified) (unused)
		  (unused) (unused) (unused) variable source (unused) (unused)
		  (unused) (unused) (unused) #f (unspecified) '()
		  #f #f '() '() '() 0)))))
  (set! *xi* (+ *xi* 1))
  (set! *xs* (cons x *xs*))
  (set! *assignments* (cons x *assignments*))
  (set! *references* (cons x *references*))
  x))

(define (create-if-expression s/x antecedent consequent alternate)
 (let ((x (cond
	   ((s-expression? s/x)
	    (make-expression
	     'if
	     (s-expression-version s/x)
	     (s-expression-cursor s/x)
	     (s-expression-pathname s/x)
	     (s-expression-line-position s/x)
	     (s-expression-character-position s/x)
	     (s-expression-character-position-within-line s/x)
	     *xi* #f (unspecified) (unspecified) (unspecified)
	     (unused) (unused) (unused) (unused) (unused) (unused)
	     antecedent consequent alternate (unused) (unused)
	     #f (unspecified) '() #f #f '() '() '() 0))
	   ((expression? s/x)
	    (make-expression
	     'if
	     (expression-version s/x)
	     (expression-cursor s/x)
	     (expression-pathname s/x)
	     (expression-line-position s/x)
	     (expression-character-position s/x)
	     (expression-character-position-within-line s/x)
	     *xi* #f (unspecified) (unspecified) (unspecified)
	     (unused) (unused) (unused) (unused) (unused) (unused)
	     antecedent consequent alternate (unused) (unused)
	     s/x (unspecified) '() #f #f '() '() '() 0))
	   (else
	    (make-expression
	     'if (no-version) (no-cursor) #f #f #f #f
	     *xi* #f (unspecified) (unspecified) (unspecified)
	     (unused) (unused) (unused) (unused) (unused) (unused)
	     antecedent consequent alternate (unused) (unused)
	     #f (unspecified) '() #f #f '() '() '() 0)))))
  (set! *xi* (+ *xi* 1))
  (set! *xs* (cons x *xs*))
  x))

;;; Expression properties

(define (expression-reached? x)
 (not (zero? (bit-and (expression-booleans x) 64))))

(define (set-expression-reached?! x p?)
 (unless (boolean? p?) (fuck-up))
 (set-expression-booleans!
  x
  (if p?
      (bit-or (expression-booleans x) 64)
      (bit-and (expression-booleans x) (bit-not 64)))))

(define (expression-inferred? x)
 (not (zero? (bit-and (expression-booleans x) 32))))

(define (set-expression-inferred?! x p?)
 (unless (boolean? p?) (fuck-up))
 (set-expression-booleans!
  x
  (if p?
      (bit-or (expression-booleans x) 32)
      (bit-and (expression-booleans x) (bit-not 32)))))

(define (expression-accessed? x)
 (not (zero? (bit-and (expression-booleans x) 16))))

(define (set-expression-accessed?! x p?)
 (unless (boolean? p?) (fuck-up))
 (set-expression-booleans!
  x
  (if p?
      (bit-or (expression-booleans x) 16)
      (bit-and (expression-booleans x) (bit-not 16)))))

(define (expression-needs-conversion-to-CPS? x)
 (not (zero? (bit-and (expression-booleans x) 8))))

(define (set-expression-needs-conversion-to-CPS?! x p?)
 (unless (boolean? p?) (fuck-up))
 (set-expression-booleans!
  x
  (if p?
      (bit-or (expression-booleans x) 8)
      (bit-and (expression-booleans x) (bit-not 8)))))

(define (expression-needs-stop-conversion-to-CPS? x)
 (not (zero? (bit-and (expression-booleans x) 4))))

(define (set-expression-needs-stop-conversion-to-CPS?! x p?)
 (unless (boolean? p?) (fuck-up))
 (set-expression-booleans!
  x
  (if p?
      (bit-or (expression-booleans x) 4)
      (bit-and (expression-booleans x) (bit-not 4)))))

(define (expression-continues? x)
 (not (zero? (bit-and (expression-booleans x) 2))))

(define (set-expression-continues?! x p?)
 (unless (boolean? p?) (fuck-up))
 (set-expression-booleans!
  x
  (if p?
      (bit-or (expression-booleans x) 2)
      (bit-and (expression-booleans x) (bit-not 2)))))

(define (expression-returns? x)
 (not (zero? (bit-and (expression-booleans x) 1))))

(define (set-expression-returns?! x p?)
 (unless (boolean? p?) (fuck-up))
 (set-expression-booleans!
  x
  (if p?
      (bit-or (expression-booleans x) 1)
      (bit-and (expression-booleans x) (bit-not 1)))))

(define (reached? x)
 (if *during-closure-conversion?*
     (case *closure-conversion-method*
      ((baseline conventional) #t)
      ((lightweight) (expression-reached? x))
      (else (fuck-up)))
     (expression-reached? x)))

(define (executed? x)
 (if *during-closure-conversion?*
     (case *closure-conversion-method*
      ((baseline conventional) #t)
      ((lightweight)
       (case (expression-kind x)
	((set!) (expression-returns? (expression-source x)))
	((call converted-call)
	 (and (expression-returns? (expression-callee x))
	      (every expression-returns? (expression-arguments x))))
	(else (fuck-up))))
      (else (fuck-up)))
     (case (expression-kind x)
      ((set!) (expression-returns? (expression-source x)))
      ((call converted-call)
       (and (expression-returns? (expression-callee x))
	    (every expression-returns? (expression-arguments x))))
      (else (fuck-up)))))

(define (free-reference? x)
 (and (not (eq? (expression-environment x)
		(variable-environment (expression-variable x))))
      (nontrivial-reference? x)))

(define (nontrivial-reference? x)
 ;; needs work: This is not memoized but should be.
 (case (expression-kind x)
  ((access) (and (reached? x) (not (fictitious? (expression-type-set x)))))
  ((set!)
   (and (executed? x)
	(accessed? (expression-variable x))
	(not (fictitious? (variable-type-set (expression-variable x))))
	(not (hidden? (expression-variable x)))
	;; This implies that the source returns.
	(not (void? (expression-type-set (expression-source x))))))
  (else (fuck-up))))

(define (must-be-self-tail-call? x)
 (and
  (or (eq? (expression-kind x) 'call)
      (eq? (expression-kind x) 'converted-call))
  (must-be?
   (lambda (u)
    (or (not ((compatible-call? x) u))
	(and (native-procedure-type? u)
	     (let ((e (callee-environment u (create-call-site x))))
	      ;; This assumes that the IN-LINED-IN? relation is reflexive.
	      (and (tail-call? (create-call-site x) e) (in-lined-in? x e))))))
   (expression-type-set (expression-callee x)))))

;;; Expression functions

(define (continuation-argument x)
 (unless (eq? (expression-kind x) 'converted-call) (fuck-up))
 (first (expression-arguments x)))

(define (first-argument x)
 (case (expression-kind x)
  ((call) (first (expression-arguments x)))
  ((converted-call) (second (expression-arguments x)))
  (else (fuck-up))))

(define (second-argument x)
 (case (expression-kind x)
  ((call) (second (expression-arguments x)))
  ((converted-call) (third (expression-arguments x)))
  (else (fuck-up))))

(define (third-argument x)
 (case (expression-kind x)
  ((call) (third (expression-arguments x)))
  ((converted-call) (fourth (expression-arguments x)))
  (else (fuck-up))))

;;; Expression environment relations

(define (tail-call? y e)
 ;; needs work: This is not memoized but should be.
 ;; needs work: A SET! to a non-accessed, fictitious, or hidden variable can be
 ;;             a tail call if its source is a tail call.
 ;; note: The argument E is needed to prevent infinite recursion on in-lined
 ;;       self tail calls.
 ;; APPLY and CALL-WITH-CURRENT-CONTINUATION tail call their first argument if
 ;; they themselves are tail calls. Implicit continuation calls are always
 ;; tail calls. FORK does not tail call its first or second argument and MUTEX
 ;; does not tail call its first argument.
 (or
  (continuation-argument-call-site? y)
  (and
   (or (explicit-call-site? y)
       (and (first-argument-call-site? y)
	    ;; needs work: It is conceivable that a first-argument call site
	    ;;             be called both by either APPLY or
	    ;;             CALL-WITH-CURRENT-CONTINUATION and by either FORK or
	    ;;             MUTEX. In this situation, the former could be tail
	    ;;             calls while the latter could not be. The current
	    ;;             representation of call sites cannot distinguish
	    ;;             between argument call sites of different primitive
	    ;;             procedures. So we err on the safe side and make such
	    ;;             call sites non tail.
	    (not (can-be? (lambda (u)
			   (or ((primitive-procedure-type-named? 'fork) u)
			       ((primitive-procedure-type-named? 'mutex) u)))
			  (expression-type-set
			   (expression-callee (call-site-expression y)))))))
   (let ((x (call-site-expression y)))
    (or
     (and (eq? (expression-kind (expression-parent x)) 'if)
	  (or (eq? x (expression-consequent (expression-parent x)))
	      (eq? x (expression-alternate (expression-parent x))))
	  (tail-call? (create-call-site (expression-parent x)) e))
     (and
      (or
       (eq? (expression-kind (expression-parent x)) 'lambda)
       (eq? (expression-kind (expression-parent x)) 'converted-lambda)
       (eq? (expression-kind (expression-parent x)) 'converted-continuation))
      (or (eq? (expression-environment x) e)
	  (not (unique-call-site? (expression-environment x)))
	  (tail-call? (unique-call-site (expression-environment x)) e))))))))

;;; Result creation

(define (create-accessor-result type-set c)
 (make-result 'accessor (unused) type-set c (unused) (unused) (unused)))

(define (create-discard-result)
 (make-result 'discard (unused) (unused) (unused) (unused) (unused) (unused)))

(define (create-return-result environment type-set)
 (make-result
  'return environment type-set (c:r environment) (unused) (unused) (unused)))

(define (create-antecedent-result type-set l1 l2 l0)
 (make-result 'antecedent (unused) type-set (unused) l1 l2 l0))

;;; Result properties

(define (accessor? r) (eq? (result-kind r) 'accessor))

(define (return? r) (eq? (result-kind r) 'return))

(define (discard? r) (eq? (result-kind r) 'discard))

(define (antecedent? r) (eq? (result-kind r) 'antecedent))

;;; Type creation

(define *ui* #f)
(define <null> #f)
(define *null-type-used?* #f)
(define *null-type-use-count* #f)
(define <true> #f)
(define *true-type-used?* #f)
(define *true-type-use-count* #f)
(define <false> #f)
(define *false-type-used?* #f)
(define *false-type-use-count* #f)
(define <char> #f)
(define *char-type-used?* #f)
(define *char-type-use-count* #f)
(define <fixnum> #f)
(define *fixnum-type-used?* #f)
(define *fixnum-type-use-count* #f)
(define <flonum> #f)
(define *flonum-type-used?* #f)
(define *flonum-type-use-count* #f)
(define <rectangular> #f)
(define *rectangular-type-used?* #f)
(define *rectangular-type-use-count* #f)
(define <input-port> #f)
(define *input-port-type-used?* #f)
(define *input-port-type-use-count* #f)
(define <output-port> #f)
(define *output-port-type-used?* #f)
(define *output-port-type-use-count* #f)
(define <eof-object> #f)
(define *eof-object-type-used?* #f)
(define *eof-object-type-use-count* #f)
(define <pointer> #f)
(define *pointer-type-used?* #f)
(define *pointer-type-use-count* #f)
(define *internal-symbol-types* #f)
(define *external-symbol-types* #f)
(define *primitive-procedure-types* #f)
(define *native-procedure-types* #f)
(define *foreign-procedure-types* #f)
(define *continuation-types* #f)
(define *string-types* #f)
(define <nonreclaimable-string> #f)
(define *structure-types* #f)
(define *headed-vector-types* #f)
(define *nonheaded-vector-types* #f)
(define <top-level-nonheaded-vector> #f)
(define *displaced-vector-types* #f)

(define (initialize-types!)
 (set! *ui* 11)
 (set! *native-procedure-types* '()))

(define (create-internal-symbol-type name)
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-internal-symbol-type name *ui* 0 (unspecified) 0)))
  (set! *ui* (+ *ui* 1))
  (set! *internal-symbol-types* (cons u *internal-symbol-types*))
  u))

(define (<internal-symbol> v)
 ;; conventions: V
 (when *types-frozen?* (fuck-up))
 (or (find-if (internal-symbol-type-named? v) *internal-symbol-types*)
     (create-internal-symbol-type v)))

(define (create-external-symbol-type displaced-string-type)
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-external-symbol-type
	    displaced-string-type (unspecified) *ui* 0 (unspecified) 0)))
  (set-external-symbol-type-link! u u)
  (set! *ui* (+ *ui* 1))
  (set! *external-symbol-types* (cons u *external-symbol-types*))
  (set-string-type-external-symbol-type! displaced-string-type u)
  u))

(define (<external-symbol> u)
 (when *types-frozen?* (fuck-up))
 (or (string-type-external-symbol-type u) (create-external-symbol-type u)))

(define (create-primitive-procedure-type name arguments)
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-primitive-procedure-type
	    name arguments *ui* 0 (unspecified) 0)))
  (set! *ui* (+ *ui* 1))
  (set! *primitive-procedure-types* (cons u *primitive-procedure-types*))
  u))

(define (<primitive-procedure> v vs)
 ;; conventions: V
 (when *types-frozen?* (fuck-up))
 (or (find-if (lambda (u)
	       (and ((primitive-procedure-type-named? v) u)
		    (equal? (primitive-procedure-type-arguments u) vs)))
	      *primitive-procedure-types*)
     (create-primitive-procedure-type v vs)))

(define (create-native-procedure-type e)
 (when *types-frozen?* (fuck-up))
 (unless (eq? e (narrow-prototype e)) (fuck-up))
 (let* ((u (make-native-procedure-type '() e *ui* 0 (unspecified) 0)))
  (set-native-procedure-type-atomic?! u #t)
  (set-native-procedure-type-fictitious?! u #t)
  (set! *ui* (+ *ui* 1))
  (set! *native-procedure-types* (cons u *native-procedure-types*))
  u))

(define (<native-procedure> e)
 (when *types-frozen?* (fuck-up))
 (let ((e (narrow-prototype e)))
  (or (find-if (lambda (u) (eq? e (narrow-prototype u)))
	       *native-procedure-types*)
      (create-native-procedure-type e))))

(define (create-foreign-procedure-type name parameters result include)
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-foreign-procedure-type
	    name parameters result include *ui* 0 (unspecified) 0)))
  (set! *ui* (+ *ui* 1))
  (set! *foreign-procedure-types* (cons u *foreign-procedure-types*))
  u))

(define (<foreign-procedure> v fs f v0)
 ;; conventions: V V0
 (when *types-frozen?* (fuck-up))
 (or (find-if (lambda (u) (string=? (foreign-procedure-type-name u) v))
	      *foreign-procedure-types*)
     (create-foreign-procedure-type v fs f v0)))

(define (create-continuation-type allocating-expression)
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-continuation-type
	    allocating-expression *ui* 0 (unspecified) (unspecified) 0)))
  (set-continuation-type-fictitious?! u #t)
  (set! *ui* (+ *ui* 1))
  (set! *continuation-types* (cons u *continuation-types*))
  (when allocating-expression
   (set-expression-continuation-type! allocating-expression u))
  u))

(define (<continuation> x)
 ;; This and CREATE-ANONYMOUS-TYPE-SET are the only type and type-set creators
 ;; that can be called when types are frozen.
 (or (expression-continuation-type x) (create-continuation-type x)))

(define (create-string-type allocating-expression)
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-string-type '() (unspecified) *ui* 0 (unspecified) #f 0)))
  (set-string-type-link! u u)
  (set! *ui* (+ *ui* 1))
  (set! *string-types* (cons u *string-types*))
  (when allocating-expression
   (set-expression-string-type! allocating-expression u))
  u))

(define (<string> x)
 (when *types-frozen?* (fuck-up))
 (let ((u (or (if x
		  (if *index-allocated-string-types-by-expression?*
		      (expression-string-type x)
		      (and (not (null? *string-types*))
			   (first *string-types*)))
		  <nonreclaimable-string>)
	      (create-string-type x))))
  (unless (memq x (string-type-allocating-expressions u))
   (set-string-type-allocating-expressions!
    u (cons x (string-type-allocating-expressions u))))
  u))

(define (create-structure-type name j allocating-expression)
 ;; conventions: J
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-structure-type
	    name (unspecified) '() (unspecified) *ui* 0 (unspecified)
	    (map-n (lambda (i) #f) j) 0)))
  (set-structure-type-immediate?! u *immediate-structures?*)
  (set-structure-type-atomic?! u #t)
  (set-structure-type-slots! u (map-n (lambda (i) (create-type-set u)) j))
  (set-structure-type-link! u u)
  (set! *ui* (+ *ui* 1))
  (set! *structure-types* (cons u *structure-types*))
  (set-expression-structure-types!
   allocating-expression
   (cons u (expression-structure-types allocating-expression)))
  u))

(define (<structure> v j uss x)
 ;; conventions: V J
 (when *types-frozen?* (fuck-up))
 (let ((u (or
	   (find-if
	    (lambda (u)
	     (and
	      ((structure-type-named? v) u)
	      (or (not (if (eq? (expression-kind x) 'pair-constant)
			   *index-constant-structure-types-by-slot-types?*
			   *index-allocated-structure-types-by-slot-types?*))
		  (every (lambda (us w) (every (lambda (u) (member? u w)) us))
			 uss (structure-type-slots u)))))
	    (if (if (eq? (expression-kind x) 'pair-constant)
		    *index-constant-structure-types-by-expression?*
		    *index-allocated-structure-types-by-expression?*)
		(expression-structure-types x)
		*structure-types*))
	   (create-structure-type v j x))))
  (unless (memq x (structure-type-allocating-expressions u))
   (set-structure-type-allocating-expressions!
    u (cons x (structure-type-allocating-expressions u))))
  (for-each (lambda (us w) (for-each (lambda (u) (assert-member! u w)) us))
	    uss
	    (structure-type-slots u))
  u))

(define (<pair> us1 us2 x) (<structure> 'pair 2 (list us1 us2) x))

(define (<pair+> uss us x)
 (cond ((null? uss) (fuck-up))
       ((null? (rest uss)) (<pair> (first uss) us x))
       (else (<pair> (first uss) (list (<pair+> (rest uss) us x)) x))))

(define (create-headed-vector-type allocating-expression)
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-headed-vector-type
	    (unspecified) '() (unspecified) *ui* 0 (unspecified) #f 0)))
  (set-headed-vector-type-atomic?! u #t)
  (set-headed-vector-type-element! u (create-type-set u))
  (set-headed-vector-type-link! u u)
  (set! *ui* (+ *ui* 1))
  (set! *headed-vector-types* (cons u *headed-vector-types*))
  (set-expression-headed-vector-types!
   allocating-expression
   (cons u (expression-headed-vector-types allocating-expression)))
  u))

(define (<headed-vector> us x)
 (when *types-frozen?* (fuck-up))
 (let ((u (or
	   (find-if
	    (lambda (u)
	     (or (not
		  (if (eq? (expression-kind x) 'vector-constant)
		      *index-constant-headed-vector-types-by-element-type?*
		      *index-allocated-headed-vector-types-by-element-type?*))
		 (every
		  (lambda (u1) (member? u1 (headed-vector-type-element u)))
		  us)))
	    (if (if (eq? (expression-kind x) 'vector-constant)
		    *index-constant-headed-vector-types-by-expression?*
		    *index-allocated-headed-vector-types-by-expression?*)
		(expression-headed-vector-types x)
		*headed-vector-types*))
	   (create-headed-vector-type x))))
  (unless (memq x (headed-vector-type-allocating-expressions u))
   (set-headed-vector-type-allocating-expressions!
    u (cons x (headed-vector-type-allocating-expressions u))))
  (for-each (lambda (u1) (assert-member! u1 (headed-vector-type-element u)))
	    us)
  u))

(define (create-nonheaded-vector-type allocating-expression)
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-nonheaded-vector-type
	    (unspecified) '() (unspecified) *ui* 0 (unspecified) #f 0)))
  (set-nonheaded-vector-type-atomic?! u #t)
  (set-nonheaded-vector-type-element! u (create-type-set u))
  (set-nonheaded-vector-type-link! u u)
  (set! *ui* (+ *ui* 1))
  (set! *nonheaded-vector-types* (cons u *nonheaded-vector-types*))
  (when allocating-expression
   (set-expression-nonheaded-vector-types!
    allocating-expression
    (cons u (expression-nonheaded-vector-types allocating-expression))))
  u))

(define (<nonheaded-vector> us x)
 (when *types-frozen?* (fuck-up))
 (let ((u
	(or
	 (if x
	     (find-if
	      (lambda (u)
	       (or
		(not
		 (if
		  (eq? (expression-kind x) 'vector-constant)
		  *index-constant-nonheaded-vector-types-by-element-type?*
		  *index-allocated-nonheaded-vector-types-by-element-type?*))
		(every
		 (lambda (u1) (member? u1 (nonheaded-vector-type-element u)))
		 us)))
	      (if (if (eq? (expression-kind x) 'vector-constant)
		      *index-constant-nonheaded-vector-types-by-expression?*
		      *index-allocated-nonheaded-vector-types-by-expression?*)
		  (expression-nonheaded-vector-types x)
		  *nonheaded-vector-types*))
	     <top-level-nonheaded-vector>)
	 (create-nonheaded-vector-type x))))
  (unless (memq x (nonheaded-vector-type-allocating-expressions u))
   (set-nonheaded-vector-type-allocating-expressions!
    u (cons x (nonheaded-vector-type-allocating-expressions u))))
  (for-each (lambda (u1) (assert-member! u1 (nonheaded-vector-type-element u)))
	    us)
  u))

(define (create-displaced-vector-type displaced-vector-type)
 (when *types-frozen?* (fuck-up))
 (let* ((u (make-displaced-vector-type
	    displaced-vector-type (unspecified) *ui* 0 (unspecified) 0)))
  (set-displaced-vector-type-link! u u)
  (set! *ui* (+ *ui* 1))
  (set! *displaced-vector-types* (cons u *displaced-vector-types*))
  (cond
   ((headed-vector-type? displaced-vector-type)
    (set-headed-vector-type-displaced-vector-type! displaced-vector-type u))
   ((nonheaded-vector-type? displaced-vector-type)
    (set-nonheaded-vector-type-displaced-vector-type! displaced-vector-type u))
   (else (fuck-up)))
  u))

(define (<displaced-vector> u)
 (when *types-frozen?* (fuck-up))
 (if (displaced-vector-type? u)
     (<displaced-vector> (displaced-vector-type-displaced-vector-type u))
     (or (cond ((headed-vector-type? u)
		(headed-vector-type-displaced-vector-type u))
	       ((nonheaded-vector-type? u)
		(nonheaded-vector-type-displaced-vector-type u))
	       (else (fuck-up)))
	 (create-displaced-vector-type u))))

;;; Type properties

(define (internal-symbol-type-type-tag-accessed? u)
 (not (zero? (bit-and (internal-symbol-type-booleans u) 16))))

(define (set-internal-symbol-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-internal-symbol-type-booleans!
  u
  (if p?
      (bit-or (internal-symbol-type-booleans u) 16)
      (bit-and (internal-symbol-type-booleans u) (bit-not 16)))))

(define (internal-symbol-type-eq?-accessed? u)
 (not (zero? (bit-and (internal-symbol-type-booleans u) 8))))

(define (set-internal-symbol-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-internal-symbol-type-booleans!
  u
  (if p?
      (bit-or (internal-symbol-type-booleans u) 8)
      (bit-and (internal-symbol-type-booleans u) (bit-not 8)))))

(define (internal-symbol-type-symbol->string-accessed? u)
 (not (zero? (bit-and (internal-symbol-type-booleans u) 4))))

(define (set-internal-symbol-type-symbol->string-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-internal-symbol-type-booleans!
  u
  (if p?
      (bit-or (internal-symbol-type-booleans u) 4)
      (bit-and (internal-symbol-type-booleans u) (bit-not 4)))))

(define (internal-symbol-type-marked? u)
 (not (zero? (bit-and (internal-symbol-type-booleans u) 2))))

(define (set-internal-symbol-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-internal-symbol-type-booleans!
  u
  (if p?
      (bit-or (internal-symbol-type-booleans u) 2)
      (bit-and (internal-symbol-type-booleans u) (bit-not 2)))))

(define (internal-symbol-type-used? u)
 (not (zero? (bit-and (internal-symbol-type-booleans u) 1))))

(define (set-internal-symbol-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-internal-symbol-type-booleans!
  u
  (if p?
      (bit-or (internal-symbol-type-booleans u) 1)
      (bit-and (internal-symbol-type-booleans u) (bit-not 1)))))

(define (external-symbol-type-type-tag-accessed? u)
 (not (zero? (bit-and (external-symbol-type-booleans u) 16))))

(define (set-external-symbol-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-external-symbol-type-booleans!
  u
  (if p?
      (bit-or (external-symbol-type-booleans u) 16)
      (bit-and (external-symbol-type-booleans u) (bit-not 16)))))

(define (external-symbol-type-eq?-accessed? u)
 (not (zero? (bit-and (external-symbol-type-booleans u) 8))))

(define (set-external-symbol-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-external-symbol-type-booleans!
  u
  (if p?
      (bit-or (external-symbol-type-booleans u) 8)
      (bit-and (external-symbol-type-booleans u) (bit-not 8)))))

(define (external-symbol-type-symbol->string-accessed? u)
 (not (zero? (bit-and (external-symbol-type-booleans u) 4))))

(define (set-external-symbol-type-symbol->string-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-external-symbol-type-booleans!
  u
  (if p?
      (bit-or (external-symbol-type-booleans u) 4)
      (bit-and (external-symbol-type-booleans u) (bit-not 4)))))

(define (external-symbol-type-marked? u)
 (not (zero? (bit-and (external-symbol-type-booleans u) 2))))

(define (set-external-symbol-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-external-symbol-type-booleans!
  u
  (if p?
      (bit-or (external-symbol-type-booleans u) 2)
      (bit-and (external-symbol-type-booleans u) (bit-not 2)))))

(define (external-symbol-type-used? u)
 (not (zero? (bit-and (external-symbol-type-booleans u) 1))))

(define (set-external-symbol-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-external-symbol-type-booleans!
  u
  (if p?
      (bit-or (external-symbol-type-booleans u) 1)
      (bit-and (external-symbol-type-booleans u) (bit-not 1)))))

(define (primitive-procedure-type-type-tag-accessed? u)
 (not (zero? (bit-and (primitive-procedure-type-booleans u) 8))))

(define (set-primitive-procedure-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-primitive-procedure-type-booleans!
  u
  (if p?
      (bit-or (primitive-procedure-type-booleans u) 8)
      (bit-and (primitive-procedure-type-booleans u) (bit-not 8)))))

(define (primitive-procedure-type-eq?-accessed? u)
 (not (zero? (bit-and (primitive-procedure-type-booleans u) 4))))

(define (set-primitive-procedure-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-primitive-procedure-type-booleans!
  u
  (if p?
      (bit-or (primitive-procedure-type-booleans u) 4)
      (bit-and (primitive-procedure-type-booleans u) (bit-not 4)))))

(define (primitive-procedure-type-marked? u)
 (not (zero? (bit-and (primitive-procedure-type-booleans u) 2))))

(define (set-primitive-procedure-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-primitive-procedure-type-booleans!
  u
  (if p?
      (bit-or (primitive-procedure-type-booleans u) 2)
      (bit-and (primitive-procedure-type-booleans u) (bit-not 2)))))

(define (primitive-procedure-type-used? u)
 (not (zero? (bit-and (primitive-procedure-type-booleans u) 1))))

(define (set-primitive-procedure-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-primitive-procedure-type-booleans!
  u
  (if p?
      (bit-or (primitive-procedure-type-booleans u) 1)
      (bit-and (primitive-procedure-type-booleans u) (bit-not 1)))))

(define (native-procedure-type-alignment? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 512))))

(define (set-native-procedure-type-alignment?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 512)
      (bit-and (native-procedure-type-booleans u) (bit-not 512)))))

(define (native-procedure-type-alignment&? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 256))))

(define (set-native-procedure-type-alignment&?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 256)
      (bit-and (native-procedure-type-booleans u) (bit-not 256)))))

(define (native-procedure-type-size? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 128))))

(define (set-native-procedure-type-size?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 128)
      (bit-and (native-procedure-type-booleans u) (bit-not 128)))))

(define (native-procedure-type-type-tag-accessed? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 64))))

(define (set-native-procedure-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 64)
      (bit-and (native-procedure-type-booleans u) (bit-not 64)))))

(define (native-procedure-type-eq?-accessed? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 32))))

(define (set-native-procedure-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 32)
      (bit-and (native-procedure-type-booleans u) (bit-not 32)))))

(define (native-procedure-type-marked? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 16))))

(define (set-native-procedure-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 16)
      (bit-and (native-procedure-type-booleans u) (bit-not 16)))))

(define (native-procedure-type-used? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 8))))

(define (set-native-procedure-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 8)
      (bit-and (native-procedure-type-booleans u) (bit-not 8)))))

(define (native-procedure-type-necessarily-fictitious? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 4))))

(define (set-native-procedure-type-necessarily-fictitious?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 4)
      (bit-and (native-procedure-type-booleans u) (bit-not 4)))))

(define (native-procedure-type-fictitious? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 2))))

(define (set-native-procedure-type-fictitious?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 2)
      (bit-and (native-procedure-type-booleans u) (bit-not 2)))))

(define (native-procedure-type-atomic? u)
 (not (zero? (bit-and (native-procedure-type-booleans u) 1))))

(define (set-native-procedure-type-atomic?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-native-procedure-type-booleans!
  u
  (if p?
      (bit-or (native-procedure-type-booleans u) 1)
      (bit-and (native-procedure-type-booleans u) (bit-not 1)))))

(define (foreign-procedure-type-called? u)
 (not (zero? (bit-and (foreign-procedure-type-booleans u) 16))))

(define (set-foreign-procedure-type-called?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-foreign-procedure-type-booleans!
  u
  (if p?
      (bit-or (foreign-procedure-type-booleans u) 16)
      (bit-and (foreign-procedure-type-booleans u) (bit-not 16)))))

(define (foreign-procedure-type-type-tag-accessed? u)
 (not (zero? (bit-and (foreign-procedure-type-booleans u) 8))))

(define (set-foreign-procedure-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-foreign-procedure-type-booleans!
  u
  (if p?
      (bit-or (foreign-procedure-type-booleans u) 8)
      (bit-and (foreign-procedure-type-booleans u) (bit-not 8)))))

(define (foreign-procedure-type-eq?-accessed? u)
 (not (zero? (bit-and (foreign-procedure-type-booleans u) 4))))

(define (set-foreign-procedure-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-foreign-procedure-type-booleans!
  u
  (if p?
      (bit-or (foreign-procedure-type-booleans u) 4)
      (bit-and (foreign-procedure-type-booleans u) (bit-not 4)))))

(define (foreign-procedure-type-marked? u)
 (not (zero? (bit-and (foreign-procedure-type-booleans u) 2))))

(define (set-foreign-procedure-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-foreign-procedure-type-booleans!
  u
  (if p?
      (bit-or (foreign-procedure-type-booleans u) 2)
      (bit-and (foreign-procedure-type-booleans u) (bit-not 2)))))

(define (foreign-procedure-type-used? u)
 (not (zero? (bit-and (foreign-procedure-type-booleans u) 1))))

(define (set-foreign-procedure-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-foreign-procedure-type-booleans!
  u
  (if p?
      (bit-or (foreign-procedure-type-booleans u) 1)
      (bit-and (foreign-procedure-type-booleans u) (bit-not 1)))))

(define (continuation-type-type-tag-accessed? u)
 (not (zero? (bit-and (continuation-type-booleans u) 32))))

(define (set-continuation-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-continuation-type-booleans!
  u
  (if p?
      (bit-or (continuation-type-booleans u) 32)
      (bit-and (continuation-type-booleans u) (bit-not 32)))))

(define (continuation-type-eq?-accessed? u)
 (not (zero? (bit-and (continuation-type-booleans u) 16))))

(define (set-continuation-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-continuation-type-booleans!
  u
  (if p?
      (bit-or (continuation-type-booleans u) 16)
      (bit-and (continuation-type-booleans u) (bit-not 16)))))

(define (continuation-type-continuation-accessed? u)
 (not (zero? (bit-and (continuation-type-booleans u) 8))))

(define (set-continuation-type-continuation-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-continuation-type-booleans!
  u
  (if p?
      (bit-or (continuation-type-booleans u) 8)
      (bit-and (continuation-type-booleans u) (bit-not 8)))))

(define (continuation-type-marked? u)
 (not (zero? (bit-and (continuation-type-booleans u) 4))))

(define (set-continuation-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-continuation-type-booleans!
  u
  (if p?
      (bit-or (continuation-type-booleans u) 4)
      (bit-and (continuation-type-booleans u) (bit-not 4)))))

(define (continuation-type-used? u)
 (not (zero? (bit-and (continuation-type-booleans u) 2))))

(define (set-continuation-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-continuation-type-booleans!
  u
  (if p?
      (bit-or (continuation-type-booleans u) 2)
      (bit-and (continuation-type-booleans u) (bit-not 2)))))

(define (continuation-type-fictitious? u)
 (not (zero? (bit-and (continuation-type-booleans u) 1))))

(define (set-continuation-type-fictitious?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-continuation-type-booleans!
  u
  (if p?
      (bit-or (continuation-type-booleans u) 1)
      (bit-and (continuation-type-booleans u) (bit-not 1)))))

(define (string-type-never-allocated-on-the-heap? u)
 (not (zero? (bit-and (string-type-booleans u) 64))))

(define (set-string-type-never-allocated-on-the-heap?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-string-type-booleans!
  u
  (if p?
      (bit-or (string-type-booleans u) 64)
      (bit-and (string-type-booleans u) (bit-not 64)))))

(define (string-type-type-tag-accessed? u)
 (not (zero? (bit-and (string-type-booleans u) 32))))

(define (set-string-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-string-type-booleans!
  u
  (if p?
      (bit-or (string-type-booleans u) 32)
      (bit-and (string-type-booleans u) (bit-not 32)))))

(define (string-type-eq?-accessed? u)
 (not (zero? (bit-and (string-type-booleans u) 16))))

(define (set-string-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-string-type-booleans!
  u
  (if p?
      (bit-or (string-type-booleans u) 16)
      (bit-and (string-type-booleans u) (bit-not 16)))))

(define (string-type-string-length-accessed? u)
 (not (zero? (bit-and (string-type-booleans u) 8))))

(define (set-string-type-string-length-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-string-type-booleans!
  u
  (if p?
      (bit-or (string-type-booleans u) 8)
      (bit-and (string-type-booleans u) (bit-not 8)))))

(define (string-type-string-ref-accessed? u)
 (not (zero? (bit-and (string-type-booleans u) 4))))

(define (set-string-type-string-ref-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-string-type-booleans!
  u
  (if p?
      (bit-or (string-type-booleans u) 4)
      (bit-and (string-type-booleans u) (bit-not 4)))))

(define (string-type-marked? u)
 (not (zero? (bit-and (string-type-booleans u) 2))))

(define (set-string-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-string-type-booleans!
  u
  (if p?
      (bit-or (string-type-booleans u) 2)
      (bit-and (string-type-booleans u) (bit-not 2)))))

(define (string-type-used? u)
 (not (zero? (bit-and (string-type-booleans u) 1))))

(define (set-string-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-string-type-booleans!
  u
  (if p?
      (bit-or (string-type-booleans u) 1)
      (bit-and (string-type-booleans u) (bit-not 1)))))

(define (structure-type-immediate? u)
 (not (zero? (bit-and (structure-type-booleans u) 1024))))

(define (set-structure-type-immediate?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 1024)
      (bit-and (structure-type-booleans u) (bit-not 1024)))))

(define (structure-type-alignment? u)
 (not (zero? (bit-and (structure-type-booleans u) 512))))

(define (set-structure-type-alignment?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 512)
      (bit-and (structure-type-booleans u) (bit-not 512)))))

(define (structure-type-alignment&? u)
 (not (zero? (bit-and (structure-type-booleans u) 256))))

(define (set-structure-type-alignment&?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 256)
      (bit-and (structure-type-booleans u) (bit-not 256)))))

(define (structure-type-size? u)
 (not (zero? (bit-and (structure-type-booleans u) 128))))

(define (set-structure-type-size?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 128)
      (bit-and (structure-type-booleans u) (bit-not 128)))))

(define (structure-type-never-allocated-on-the-heap? u)
 (not (zero? (bit-and (structure-type-booleans u) 64))))

(define (set-structure-type-never-allocated-on-the-heap?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 64)
      (bit-and (structure-type-booleans u) (bit-not 64)))))

(define (structure-type-type-tag-accessed? u)
 (not (zero? (bit-and (structure-type-booleans u) 32))))

(define (set-structure-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 32)
      (bit-and (structure-type-booleans u) (bit-not 32)))))

(define (structure-type-eq?-accessed? u)
 (not (zero? (bit-and (structure-type-booleans u) 16))))

(define (set-structure-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 16)
      (bit-and (structure-type-booleans u) (bit-not 16)))))

(define (structure-type-marked? u)
 (not (zero? (bit-and (structure-type-booleans u) 8))))

(define (set-structure-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 8)
      (bit-and (structure-type-booleans u) (bit-not 8)))))

(define (structure-type-used? u)
 (not (zero? (bit-and (structure-type-booleans u) 4))))

(define (set-structure-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 4)
      (bit-and (structure-type-booleans u) (bit-not 4)))))

(define (structure-type-fictitious? u)
 (not (zero? (bit-and (structure-type-booleans u) 2))))

(define (set-structure-type-fictitious?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 2)
      (bit-and (structure-type-booleans u) (bit-not 2)))))

(define (structure-type-atomic? u)
 (not (zero? (bit-and (structure-type-booleans u) 1))))

(define (set-structure-type-atomic?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-structure-type-booleans!
  u
  (if p?
      (bit-or (structure-type-booleans u) 1)
      (bit-and (structure-type-booleans u) (bit-not 1)))))

(define (headed-vector-type-alignment? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 1024))))

(define (set-headed-vector-type-alignment?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 1024)
      (bit-and (headed-vector-type-booleans u) (bit-not 1024)))))

(define (headed-vector-type-alignment&? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 512))))

(define (set-headed-vector-type-alignment&?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 512)
      (bit-and (headed-vector-type-booleans u) (bit-not 512)))))

(define (headed-vector-type-size? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 256))))

(define (set-headed-vector-type-size?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 256)
      (bit-and (headed-vector-type-booleans u) (bit-not 256)))))

(define (headed-vector-type-never-allocated-on-the-heap? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 128))))

(define (set-headed-vector-type-never-allocated-on-the-heap?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 128)
      (bit-and (headed-vector-type-booleans u) (bit-not 128)))))

(define (headed-vector-type-type-tag-accessed? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 64))))

(define (set-headed-vector-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 64)
      (bit-and (headed-vector-type-booleans u) (bit-not 64)))))

(define (headed-vector-type-eq?-accessed? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 32))))

(define (set-headed-vector-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 32)
      (bit-and (headed-vector-type-booleans u) (bit-not 32)))))

(define (headed-vector-type-vector-length-accessed? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 16))))

(define (set-headed-vector-type-vector-length-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 16)
      (bit-and (headed-vector-type-booleans u) (bit-not 16)))))

(define (headed-vector-type-vector-ref-accessed? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 8))))

(define (set-headed-vector-type-vector-ref-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 8)
      (bit-and (headed-vector-type-booleans u) (bit-not 8)))))

(define (headed-vector-type-marked? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 4))))

(define (set-headed-vector-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 4)
      (bit-and (headed-vector-type-booleans u) (bit-not 4)))))

(define (headed-vector-type-used? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 2))))

(define (set-headed-vector-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 2)
      (bit-and (headed-vector-type-booleans u) (bit-not 2)))))

(define (headed-vector-type-atomic? u)
 (not (zero? (bit-and (headed-vector-type-booleans u) 1))))

(define (set-headed-vector-type-atomic?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-headed-vector-type-booleans!
  u
  (if p?
      (bit-or (headed-vector-type-booleans u) 1)
      (bit-and (headed-vector-type-booleans u) (bit-not 1)))))

(define (nonheaded-vector-type-alignment? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 512))))

(define (set-nonheaded-vector-type-alignment?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 512)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 512)))))

(define (nonheaded-vector-type-size? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 256))))

(define (set-nonheaded-vector-type-size?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 256)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 256)))))

(define (nonheaded-vector-type-never-allocated-on-the-heap? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 128))))

(define (set-nonheaded-vector-type-never-allocated-on-the-heap?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 128)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 128)))))

(define (nonheaded-vector-type-type-tag-accessed? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 64))))

(define (set-nonheaded-vector-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 64)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 64)))))

(define (nonheaded-vector-type-eq?-accessed? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 32))))

(define (set-nonheaded-vector-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 32)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 32)))))

(define (nonheaded-vector-type-vector-length-accessed? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 16))))

(define (set-nonheaded-vector-type-vector-length-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 16)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 16)))))

(define (nonheaded-vector-type-vector-ref-accessed? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 8))))

(define (set-nonheaded-vector-type-vector-ref-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 8)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 8)))))

(define (nonheaded-vector-type-marked? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 4))))

(define (set-nonheaded-vector-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 4)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 4)))))

(define (nonheaded-vector-type-used? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 2))))

(define (set-nonheaded-vector-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 2)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 2)))))

(define (nonheaded-vector-type-atomic? u)
 (not (zero? (bit-and (nonheaded-vector-type-booleans u) 1))))

(define (set-nonheaded-vector-type-atomic?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-nonheaded-vector-type-booleans!
  u
  (if p?
      (bit-or (nonheaded-vector-type-booleans u) 1)
      (bit-and (nonheaded-vector-type-booleans u) (bit-not 1)))))

(define (displaced-vector-type-alignment? u)
 (not (zero? (bit-and (displaced-vector-type-booleans u) 128))))

(define (set-displaced-vector-type-alignment?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-displaced-vector-type-booleans!
  u
  (if p?
      (bit-or (displaced-vector-type-booleans u) 128)
      (bit-and (displaced-vector-type-booleans u) (bit-not 128)))))

(define (displaced-vector-type-size? u)
 (not (zero? (bit-and (displaced-vector-type-booleans u) 64))))

(define (set-displaced-vector-type-size?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-displaced-vector-type-booleans!
  u
  (if p?
      (bit-or (displaced-vector-type-booleans u) 64)
      (bit-and (displaced-vector-type-booleans u) (bit-not 64)))))

(define (displaced-vector-type-type-tag-accessed? u)
 (not (zero? (bit-and (displaced-vector-type-booleans u) 32))))

(define (set-displaced-vector-type-type-tag-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-displaced-vector-type-booleans!
  u
  (if p?
      (bit-or (displaced-vector-type-booleans u) 32)
      (bit-and (displaced-vector-type-booleans u) (bit-not 32)))))

(define (displaced-vector-type-eq?-accessed? u)
 (not (zero? (bit-and (displaced-vector-type-booleans u) 16))))

(define (set-displaced-vector-type-eq?-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-displaced-vector-type-booleans!
  u
  (if p?
      (bit-or (displaced-vector-type-booleans u) 16)
      (bit-and (displaced-vector-type-booleans u) (bit-not 16)))))

(define (displaced-vector-type-vector-length-accessed? u)
 (not (zero? (bit-and (displaced-vector-type-booleans u) 8))))

(define (set-displaced-vector-type-vector-length-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-displaced-vector-type-booleans!
  u
  (if p?
      (bit-or (displaced-vector-type-booleans u) 8)
      (bit-and (displaced-vector-type-booleans u) (bit-not 8)))))

(define (displaced-vector-type-vector-ref-accessed? u)
 (not (zero? (bit-and (displaced-vector-type-booleans u) 4))))

(define (set-displaced-vector-type-vector-ref-accessed?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-displaced-vector-type-booleans!
  u
  (if p?
      (bit-or (displaced-vector-type-booleans u) 4)
      (bit-and (displaced-vector-type-booleans u) (bit-not 4)))))

(define (displaced-vector-type-marked? u)
 (not (zero? (bit-and (displaced-vector-type-booleans u) 2))))

(define (set-displaced-vector-type-marked?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-displaced-vector-type-booleans!
  u
  (if p?
      (bit-or (displaced-vector-type-booleans u) 2)
      (bit-and (displaced-vector-type-booleans u) (bit-not 2)))))

(define (displaced-vector-type-used? u)
 (not (zero? (bit-and (displaced-vector-type-booleans u) 1))))

(define (set-displaced-vector-type-used?! u p?)
 (unless (boolean? p?) (fuck-up))
 (set-displaced-vector-type-booleans!
  u
  (if p?
      (bit-or (displaced-vector-type-booleans u) 1)
      (bit-and (displaced-vector-type-booleans u) (bit-not 1)))))

(define (set-type-type-tag-accessed?! u p?)
 (cond ((null-type? u) #f)
       ((true-type? u) #f)
       ((false-type? u) #f)
       ((char-type? u) #f)
       ((fixnum-type? u) #f)
       ((flonum-type? u) #f)
       ((rectangular-type? u) #f)
       ((input-port-type? u) #f)
       ((output-port-type? u) #f)
       ((eof-object-type? u) #f)
       ((pointer-type? u) #f)
       ((internal-symbol-type? u)
	(set-internal-symbol-type-type-tag-accessed?! u p?))
       ((external-symbol-type? u)
	(set-external-symbol-type-type-tag-accessed?! u p?))
       ((primitive-procedure-type? u)
	(set-primitive-procedure-type-type-tag-accessed?! u p?))
       ((native-procedure-type? u)
	(set-native-procedure-type-type-tag-accessed?! u p?))
       ((foreign-procedure-type? u)
	(set-foreign-procedure-type-type-tag-accessed?! u p?))
       ((continuation-type? u)
	(set-continuation-type-type-tag-accessed?! u p?))
       ((string-type? u)
	(set-string-type-type-tag-accessed?! u p?))
       ((structure-type? u)
	(set-structure-type-type-tag-accessed?! u p?))
       ((headed-vector-type? u)
	(set-headed-vector-type-type-tag-accessed?! u p?))
       ((nonheaded-vector-type? u)
	(set-nonheaded-vector-type-type-tag-accessed?! u p?))
       ((displaced-vector-type? u)
	(set-displaced-vector-type-type-tag-accessed?! u p?))
       (else (fuck-up))))

(define (set-type-eq?-accessed?! u p?)
 (cond ((null-type? u) #f)
       ((true-type? u) #f)
       ((false-type? u) #f)
       ((char-type? u) #f)
       ((fixnum-type? u) #f)
       ((flonum-type? u) #f)
       ((rectangular-type? u) #f)
       ((input-port-type? u) #f)
       ((output-port-type? u) #f)
       ((eof-object-type? u) #f)
       ((pointer-type? u) #f)
       ((internal-symbol-type? u)
	(set-internal-symbol-type-eq?-accessed?! u p?))
       ((external-symbol-type? u)
	(set-external-symbol-type-eq?-accessed?! u p?))
       ((primitive-procedure-type? u)
	(set-primitive-procedure-type-eq?-accessed?! u p?))
       ((native-procedure-type? u)
	(set-native-procedure-type-eq?-accessed?! u p?))
       ((foreign-procedure-type? u)
	(set-foreign-procedure-type-eq?-accessed?! u p?))
       ((continuation-type? u)
	(set-continuation-type-eq?-accessed?! u p?))
       ((string-type? u)
	(set-string-type-eq?-accessed?! u p?))
       ((structure-type? u)
	(set-structure-type-eq?-accessed?! u p?))
       ((headed-vector-type? u)
	(set-headed-vector-type-eq?-accessed?! u p?))
       ((nonheaded-vector-type? u)
	(set-nonheaded-vector-type-eq?-accessed?! u p?))
       ((displaced-vector-type? u)
	(set-displaced-vector-type-eq?-accessed?! u p?))
       (else (fuck-up))))

(define (null-type? u) (eq? u 'null))

(define (true-type? u) (eq? u 'true))

(define (false-type? u) (eq? u 'false))

(define (boolean-type? u) (or (true-type? u) (false-type? u)))

(define (char-type? u) (eq? u 'char))

(define (fixnum-type? u) (eq? u 'fixnum))

(define (flonum-type? u) (eq? u 'flonum))

(define (nonrectangular-number-type? u) (or (fixnum-type? u) (flonum-type? u)))

(define (rectangular-type? u) (eq? u 'rectangular))

(define (number-type? u)
 (or (fixnum-type? u) (flonum-type? u) (rectangular-type? u)))

(define (exact-type? u)
 (unless (number-type? u) (fuck-up))
 (fixnum-type? u))

(define (inexact-type? u)
 (unless (number-type? u) (fuck-up))
 (or (flonum-type? u) (rectangular-type? u)))

(define (input-port-type? u) (eq? u 'input-port))

(define (output-port-type? u) (eq? u 'output-port))

(define (eof-object-type? u) (eq? u 'eof-object))

(define (pointer-type? u) (eq? u 'pointer))

(define (internal-symbol-type-named? name)
 ;; conventions: NAME
 (lambda (u)
  (and (internal-symbol-type? u) (eq? (internal-symbol-type-name u) name))))

(define (symbol-type? u)
 (or (internal-symbol-type? u) (external-symbol-type? u)))

(define (primitive-procedure-type-named? name)
 ;; conventions: NAME
 (lambda (u)
  (and (primitive-procedure-type? u)
       (eq? (primitive-procedure-type-name u) name))))

(define (continuation-type-to? x)
 (lambda (u)
  (and (continuation-type? u)
       (eq? (continuation-type-allocating-expression u) x))))

(define (procedure-type? u)
 (or (primitive-procedure-type? u)
     (native-procedure-type? u)
     (foreign-procedure-type? u)
     (continuation-type? u)))

(define (nonreclaimable-string-type? u)
 (and (string-type? u) (memq #f (string-type-allocating-expressions u))))

(define (structure-type-named? name)
 ;; conventions: NAME
 (lambda (u) (and (structure-type? u) (eq? (structure-type-name u) name))))

(define (pair-type? u) ((structure-type-named? 'pair) u))

(define (pair+-type? uss us x)
 (lambda (u)
  (when (null? uss) (fuck-up))
  (and (pair-type? u)
       (memq x (structure-type-allocating-expressions u))
       (every (lambda (u1) (member? u1 (pair-type-car u))) (first uss))
       (if (null? (rest uss))
	   (every (lambda (u1) (member? u1 (pair-type-cdr u))) us)
	   (can-be? (pair+-type? (rest uss) us x) (pair-type-cdr u))))))

(define (list-type-of? m)
 (define (list-type-of? m us)
  (lambda (u)
   (or (memq u us)
       (null-type? u)
       (and (pair-type? u)
	    (can-be? m (pair-type-car u))
	    (can-be? (list-type-of? m (cons u us)) (pair-type-cdr u))))))
 (list-type-of? m '()))

(define (list-type? u) ((list-type-of? type?) u))

(define (list-type-of-length? j)
 ;; conventions: J
 (lambda (u)
  (and (not (negative? j))
       (if (zero? j)
	   (null-type? u)
	   (and (pair-type? u)
		(can-be? (list-type-of-length? (- j 1)) (pair-type-cdr u)))))))

(define (list-type-of-length-at-least? j)
 ;; conventions: J
 (lambda (u)
  (if (positive? j)
      (and (pair-type? u)
	   (can-be? (list-type-of-length-at-least? (- j 1)) (pair-type-cdr u)))
      (list-type? u))))

(define (top-level-nonheaded-vector-type? u)
 (and (nonheaded-vector-type? u)
      (memq #f (nonheaded-vector-type-allocating-expressions u))))

(define (vector-type? u)
 (or (headed-vector-type? u)
     (nonheaded-vector-type? u)
     (displaced-vector-type? u)))

(define (vector-type-eq?-accessed? u)
 (cond
  ((headed-vector-type? u) (headed-vector-type-eq?-accessed? u))
  ((nonheaded-vector-type? u) (nonheaded-vector-type-eq?-accessed? u))
  ((displaced-vector-type? u) (displaced-vector-type-eq?-accessed? u))
  (else (fuck-up))))

(define (vector-ref-accessed? u)
 (cond
  ((headed-vector-type? u) (headed-vector-type-vector-ref-accessed? u))
  ((nonheaded-vector-type? u) (nonheaded-vector-type-vector-ref-accessed? u))
  ((displaced-vector-type? u) (displaced-vector-type-vector-ref-accessed? u))
  (else (fuck-up))))

(define (degenerate-vector-type? u)
 (and (vector-type? u) (fictitious? (vector-type-element u))))

(define (type? u)
 (or (null-type? u)
     (true-type? u)
     (false-type? u)
     (char-type? u)
     (fixnum-type? u)
     (flonum-type? u)
     (rectangular-type? u)
     (input-port-type? u)
     (output-port-type? u)
     (eof-object-type? u)
     (pointer-type? u)
     (internal-symbol-type? u)
     (external-symbol-type? u)
     (primitive-procedure-type? u)
     (native-procedure-type? u)
     (foreign-procedure-type? u)
     (continuation-type? u)
     (string-type? u)
     (structure-type? u)
     (headed-vector-type? u)
     (nonheaded-vector-type? u)
     (displaced-vector-type? u)))

(define (never-allocated-on-the-heap? u)
 (cond
  ((null-type? u) #t)
  ((true-type? u) #t)
  ((false-type? u) #t)
  ((char-type? u) #t)
  ((fixnum-type? u) #t)
  ((flonum-type? u) #t)
  ((rectangular-type? u) #t)
  ((input-port-type? u) #t)
  ((output-port-type? u) #t)
  ((eof-object-type? u) #t)
  ((pointer-type? u) #t)
  ((internal-symbol-type? u) #t)
  ((external-symbol-type? u) #t)
  ((primitive-procedure-type? u) #t)
  ((native-procedure-type? u) #t)
  ((foreign-procedure-type? u) #t)
  ((continuation-type? u) #t)
  ((string-type? u) (string-type-never-allocated-on-the-heap? u))
  ((structure-type? u) (structure-type-never-allocated-on-the-heap? u))
  ((headed-vector-type? u) (headed-vector-type-never-allocated-on-the-heap? u))
  ((nonheaded-vector-type? u)
   (nonheaded-vector-type-never-allocated-on-the-heap? u))
  ((displaced-vector-type? u) #t)
  (else (fuck-up))))

(define (type-marked? u)
 (cond ((null-type? u) (fuck-up))
       ((true-type? u) (fuck-up))
       ((false-type? u) (fuck-up))
       ((char-type? u) (fuck-up))
       ((fixnum-type? u) (fuck-up))
       ((flonum-type? u) (fuck-up))
       ((rectangular-type? u) (fuck-up))
       ((input-port-type? u) (fuck-up))
       ((output-port-type? u) (fuck-up))
       ((eof-object-type? u) (fuck-up))
       ((pointer-type? u) (fuck-up))
       ((internal-symbol-type? u) (internal-symbol-type-marked? u))
       ((external-symbol-type? u) (external-symbol-type-marked? u))
       ((primitive-procedure-type? u) (primitive-procedure-type-marked? u))
       ((native-procedure-type? u) (native-procedure-type-marked? u))
       ((foreign-procedure-type? u) (foreign-procedure-type-marked? u))
       ((continuation-type? u) (continuation-type-marked? u))
       ((string-type? u) (string-type-marked? u))
       ((structure-type? u) (structure-type-marked? u))
       ((headed-vector-type? u) (headed-vector-type-marked? u))
       ((nonheaded-vector-type? u) (nonheaded-vector-type-marked? u))
       ((displaced-vector-type? u) (displaced-vector-type-marked? u))
       (else (fuck-up))))

(define (set-type-marked?! u p?)
 ;; needs work: This is not really a type property.
 (cond
  ((null-type? u) (fuck-up))
  ((true-type? u) (fuck-up))
  ((false-type? u) (fuck-up))
  ((char-type? u) (fuck-up))
  ((fixnum-type? u) (fuck-up))
  ((flonum-type? u) (fuck-up))
  ((rectangular-type? u) (fuck-up))
  ((input-port-type? u) (fuck-up))
  ((output-port-type? u) (fuck-up))
  ((eof-object-type? u) (fuck-up))
  ((pointer-type? u) (fuck-up))
  ((internal-symbol-type? u) (set-internal-symbol-type-marked?! u p?))
  ((external-symbol-type? u) (set-external-symbol-type-marked?! u p?))
  ((primitive-procedure-type? u) (set-primitive-procedure-type-marked?! u p?))
  ((native-procedure-type? u) (set-native-procedure-type-marked?! u p?))
  ((foreign-procedure-type? u) (set-foreign-procedure-type-marked?! u p?))
  ((continuation-type? u) (set-continuation-type-marked?! u p?))
  ((string-type? u) (set-string-type-marked?! u p?))
  ((structure-type? u) (set-structure-type-marked?! u p?))
  ((headed-vector-type? u) (set-headed-vector-type-marked?! u p?))
  ((nonheaded-vector-type? u) (set-nonheaded-vector-type-marked?! u p?))
  ((displaced-vector-type? u) (set-displaced-vector-type-marked?! u p?))
  (else (fuck-up))))

(define (type-used? u)
 (cond ((null-type? u) #t)
       ((true-type? u) #t)
       ((false-type? u) #t)
       ((char-type? u) #t)
       ((fixnum-type? u) #t)
       ((flonum-type? u) #t)
       ((rectangular-type? u) #t)
       ((input-port-type? u) #t)
       ((output-port-type? u) #t)
       ((eof-object-type? u) #t)
       ((pointer-type? u) #t)
       ((internal-symbol-type? u) (internal-symbol-type-used? u))
       ((external-symbol-type? u) (external-symbol-type-used? u))
       ((primitive-procedure-type? u) (primitive-procedure-type-used? u))
       ((native-procedure-type? u) (native-procedure-type-used? u))
       ((foreign-procedure-type? u) (foreign-procedure-type-used? u))
       ((continuation-type? u) (continuation-type-used? u))
       ((string-type? u) (string-type-used? u))
       ((structure-type? u) (structure-type-used? u))
       ((headed-vector-type? u) (headed-vector-type-used? u))
       ((nonheaded-vector-type? u) (nonheaded-vector-type-used? u))
       ((displaced-vector-type? u) (displaced-vector-type-used? u))
       (else (fuck-up))))

(define (compatible-procedure? ws w y)
 (if (converted? y)
     (lambda (u)
      (or (and (primitive-procedure-type? u)
	       ((primitive-procedure-compatible-procedure?
		 (cdr (assq (primitive-procedure-type-name u)
			    *primitive-procedure-handlers*)))
		u (rest ws) w))
	  (and (native-procedure-type? u)
	       ;; What a kludge!
	       (or (not *types-frozen?*) (callee-environment? u y))
	       (let ((e (callee-environment u y)))
		(if (rest? e)
		    (if (converted? e)
			(can-be? (list-type-of-length-at-least?
				  (- (- (length (variables e)) 1) (length ws)))
				 w)
			(can-be? (list-type-of-length-at-least?
				  (- (length (variables e)) (length ws)))
				 w))
		    (if (converted? e)
			(can-be? (list-type-of-length?
				  (- (length (variables e)) (length ws)))
				 w)
			(can-be? (list-type-of-length?
				  (- (+ (length (variables e)) 1) (length ws)))
				 w)))))
	  (and (foreign-procedure-type? u)
	       (can-be? (list-type-of-length?
			 (- (+ (length (foreign-procedure-type-parameters u))
			       1)
			    (length ws)))
			w))
	  (and (continuation-type? u)
	       (can-be? (list-type-of-length? (- 2 (length ws))) w))))
     (lambda (u)
      (or (and (primitive-procedure-type? u)
	       ((primitive-procedure-compatible-procedure?
		 (cdr (assq (primitive-procedure-type-name u)
			    *primitive-procedure-handlers*)))
		u ws w))
	  (and (native-procedure-type? u)
	       ;; What a kludge!
	       (or (not *types-frozen?*) (callee-environment? u y))
	       (let ((e (callee-environment u y)))
		;; note: I'm not sure that this is the right thing to do but
		;;       it is now needed for test21.sc since I changed MAP and
		;;       APPLY.
		(and (not (converted? e))
		     (if (rest? e)
			 (can-be? (list-type-of-length-at-least?
				   (- (- (length (variables e)) 1)
				      (length ws)))
				  w)
			 (can-be? (list-type-of-length?
				   (- (length (variables e)) (length ws)))
				  w)))))
	  (and (foreign-procedure-type? u)
	       (can-be? (list-type-of-length?
			 (- (length (foreign-procedure-type-parameters u))
			    (length ws)))
			w))
	  (and (continuation-type? u)
	       (can-be? (list-type-of-length? (- 1 (length ws))) w))))))

(define (compatible-call? x)
 (compatible-procedure? (map expression-type-set (expression-arguments x))
			*null*
			(create-call-site x)))

(define (compatible-call-via-apply? x)
 (compatible-procedure?
  (map expression-type-set
       (if (converted? x)
	   (cons (continuation-argument x)
		 (but-last (rest (rest (expression-arguments x)))))
	   (but-last (rest (expression-arguments x)))))
  (expression-type-set (last (expression-arguments x)))
  (recreate-call-site (create-call-site x) 'first-argument)))

(define (compatible-call-via-call-with-current-continuation? x)
 (compatible-procedure?
  (if (converted? x)
      (list (expression-type-set (continuation-argument x))
	    (expression-type-set (continuation-argument x)))
      (list (create-anonymous-type-set (<continuation> x))))
  *null*
  (recreate-call-site (create-call-site x) 'first-argument)))

(define (compatible-call-via-fork1? x)
 (compatible-procedure?
  (if (converted? x)
      (list (expression-type-set (continuation-argument x)))
      '())
  *null*
  (recreate-call-site (create-call-site x) 'first-argument)))

(define (compatible-call-via-fork2? x)
 (compatible-procedure?
  (if (converted? x)
      (list (expression-type-set (continuation-argument x)))
      '())
  *null*
  (recreate-call-site (create-call-site x) 'second-argument)))

(define (compatible-call-via-mutex? x)
 (compatible-procedure?
  (if (converted? x)
      (list (expression-type-set (continuation-argument x)))
      '())
  *null*
  (recreate-call-site (create-call-site x) 'first-argument)))

(define (truly-compatible-procedure? ws w y)
 ;; note: The reason that we have both COMPATIBLE-PROCEDURE? and
 ;;       TRULY-COMPATIBLE-PROCEDURE? is that if a call site to a primitive
 ;;       procedure is not compatible we generate a call_error where as if it
 ;;       is compatible but not truly compatible we actually call the
 ;;       PRIMITIVE-PROCEDURE-COMPILE-CALL to generate the error.
 (if (converted? y)
     (lambda (u)
      (or (and (primitive-procedure-type? u)
	       ((primitive-procedure-compatible-procedure?
		 (cdr (assq (primitive-procedure-type-name u)
			    *primitive-procedure-handlers*)))
		u (rest ws) w)
	       (((primitive-procedure-truly-compatible-procedure?
		  (cdr (assq (primitive-procedure-type-name u)
			     *primitive-procedure-handlers*)))
		 y u (first ws))
		(rest ws) w))
	  (and (native-procedure-type? u)
	       ;; What a kludge!
	       (or (not *types-frozen?*) (callee-environment? u y))
	       (let ((e (callee-environment u y)))
		(if (rest? e)
		    (if (converted? e)
			(can-be? (list-type-of-length-at-least?
				  (- (- (length (variables e)) 1) (length ws)))
				 w)
			(can-be? (list-type-of-length-at-least?
				  (- (length (variables e)) (length ws)))
				 w))
		    (if (converted? e)
			(can-be? (list-type-of-length?
				  (- (length (variables e)) (length ws)))
				 w)
			(can-be? (list-type-of-length?
				  (- (+ (length (variables e)) 1) (length ws)))
				 w)))))
	  (and (foreign-procedure-type? u)
	       (let loop? ((fs (foreign-procedure-type-parameters u))
			   (ws (rest ws))
			   (w w))
		(if (null? fs)
		    (and (null? ws) (can-be? null-type? w))
		    (or (and (not (null? ws))
			     (can-be? (foreign-type? (first fs)) (first ws))
			     (loop? (rest fs) (rest ws) w))
			(and (null? ws)
			     (can-be? (lambda (u)
				       (and (pair-type? u)
					    (can-be? (foreign-type? (first fs))
						     (pair-type-car u))
					    (loop?
					     (rest fs) ws (pair-type-cdr u))))
				      w))))))
	  (and (continuation-type? u)
	       (can-be? (list-type-of-length? (- 2 (length ws))) w))))
     (lambda (u)
      (or (and (primitive-procedure-type? u)
	       ((primitive-procedure-compatible-procedure?
		 (cdr (assq (primitive-procedure-type-name u)
			    *primitive-procedure-handlers*)))
		u ws w)
	       (((primitive-procedure-truly-compatible-procedure?
		  (cdr (assq (primitive-procedure-type-name u)
			     *primitive-procedure-handlers*)))
		 y u #f)
		ws w))
	  (and (native-procedure-type? u)
	       ;; What a kludge!
	       (or (not *types-frozen?*) (callee-environment? u y))
	       (let ((e (callee-environment u y)))
		;; note: I'm not sure that this is the right thing to do but
		;;       it is now needed for test21.sc since I changed MAP and
		;;       APPLY.
		(and (not (converted? e))
		     (if (rest? e)
			 (can-be? (list-type-of-length-at-least?
				   (- (- (length (variables e)) 1)
				      (length ws)))
				  w)
			 (can-be? (list-type-of-length?
				   (- (length (variables e)) (length ws)))
				  w)))))
	  (and (foreign-procedure-type? u)
	       (let loop? ((fs (foreign-procedure-type-parameters u))
			   (ws ws)
			   (w w))
		(if (null? fs)
		    (and (null? ws) (can-be? null-type? w))
		    (or (and (not (null? ws))
			     (can-be? (foreign-type? (first fs)) (first ws))
			     (loop? (rest fs) (rest ws) w))
			(and (null? ws)
			     (can-be? (lambda (u)
				       (and (pair-type? u)
					    (can-be? (foreign-type? (first fs))
						     (pair-type-car u))
					    (loop?
					     (rest fs) ws (pair-type-cdr u))))
				      w))))))
	  (and (continuation-type? u)
	       (can-be? (list-type-of-length? (- 1 (length ws))) w))))))

(define (truly-compatible-call? x)
 (truly-compatible-procedure?
  (map expression-type-set (expression-arguments x))
  *null*
  (create-call-site x)))

(define (truly-compatible-call-via-apply? x)
 (truly-compatible-procedure?
  (map expression-type-set
       (if (converted? x)
	   (cons (continuation-argument x)
		 (but-last (rest (rest (expression-arguments x)))))
	   (but-last (rest (expression-arguments x)))))
  (expression-type-set (last (expression-arguments x)))
  (recreate-call-site (create-call-site x) 'first-argument)))

(define (truly-compatible-call-via-call-with-current-continuation? x)
 (truly-compatible-procedure?
  (if (converted? x)
      (list (expression-type-set (continuation-argument x))
	    (expression-type-set (continuation-argument x)))
      (list (create-anonymous-type-set (<continuation> x))))
  *null*
  (recreate-call-site (create-call-site x) 'first-argument)))

(define (truly-compatible-call-via-fork1? x)
 (truly-compatible-procedure?
  (if (converted? x)
      (list (expression-type-set (continuation-argument x)))
      '())
  *null*
  (recreate-call-site (create-call-site x) 'first-argument)))

(define (truly-compatible-call-via-fork2? x)
 (truly-compatible-procedure?
  (if (converted? x)
      (list (expression-type-set (continuation-argument x)))
      '())
  *null*
  (recreate-call-site (create-call-site x) 'second-argument)))

(define (truly-compatible-call-via-mutex? x)
 (truly-compatible-procedure?
  (if (converted? x)
      (list (expression-type-set (continuation-argument x)))
      '())
  *null*
  (recreate-call-site (create-call-site x) 'first-argument)))

;;; Type functions

(define (pair-type-car u)
 (unless (pair-type? u) (fuck-up))
 (first (structure-type-slots u)))

(define (pair-type-cdr u)
 (unless (pair-type? u) (fuck-up))
 (second (structure-type-slots u)))

(define (vector-type-element u)
 (cond ((headed-vector-type? u) (headed-vector-type-element u))
       ((nonheaded-vector-type? u) (nonheaded-vector-type-element u))
       ((displaced-vector-type? u)
	(vector-type-element (displaced-vector-type-displaced-vector-type u)))
       (else (fuck-up))))

(define (type-index u)
 (cond ((null-type? u) 0)
       ((true-type? u) 1)
       ((false-type? u) 2)
       ((char-type? u) 3)
       ((fixnum-type? u) 4)
       ((flonum-type? u) 5)
       ((rectangular-type? u) 6)
       ((input-port-type? u) 7)
       ((output-port-type? u) 8)
       ((eof-object-type? u) 9)
       ((pointer-type? u) 10)
       ((internal-symbol-type? u) (internal-symbol-type-index u))
       ((external-symbol-type? u) (external-symbol-type-index u))
       ((primitive-procedure-type? u) (primitive-procedure-type-index u))
       ((native-procedure-type? u) (native-procedure-type-index u))
       ((foreign-procedure-type? u) (foreign-procedure-type-index u))
       ((continuation-type? u) (continuation-type-index u))
       ((string-type? u) (string-type-index u))
       ((structure-type? u) (structure-type-index u))
       ((headed-vector-type? u) (headed-vector-type-index u))
       ((nonheaded-vector-type? u) (nonheaded-vector-type-index u))
       ((displaced-vector-type? u) (displaced-vector-type-index u))
       (else (fuck-up))))

(define (type-use-count u)
 (cond ((null-type? u) *null-type-use-count*)
       ((true-type? u) *true-type-use-count*)
       ((false-type? u) *false-type-use-count*)
       ((char-type? u) *char-type-use-count*)
       ((fixnum-type? u) *fixnum-type-use-count*)
       ((flonum-type? u) *flonum-type-use-count*)
       ((rectangular-type? u) *rectangular-type-use-count*)
       ((input-port-type? u) *input-port-type-use-count*)
       ((output-port-type? u) *output-port-type-use-count*)
       ((eof-object-type? u) *eof-object-type-use-count*)
       ((pointer-type? u) *pointer-type-use-count*)
       ((internal-symbol-type? u) (internal-symbol-type-use-count u))
       ((external-symbol-type? u) (external-symbol-type-use-count u))
       ((primitive-procedure-type? u) (primitive-procedure-type-use-count u))
       ((native-procedure-type? u) (native-procedure-type-use-count u))
       ((foreign-procedure-type? u) (foreign-procedure-type-use-count u))
       ((continuation-type? u) (continuation-type-use-count u))
       ((string-type? u) (string-type-use-count u))
       ((structure-type? u) (structure-type-use-count u))
       ((headed-vector-type? u) (headed-vector-type-use-count u))
       ((nonheaded-vector-type? u) (nonheaded-vector-type-use-count u))
       ((displaced-vector-type? u) (displaced-vector-type-use-count u))
       (else (fuck-up))))

(define (types-and-type-sets-that-directly-point-to u)
 (cond
  ((null-type? u) (fuck-up))
  ((true-type? u) (fuck-up))
  ((false-type? u) (fuck-up))
  ((char-type? u) (fuck-up))
  ((fixnum-type? u) (fuck-up))
  ((flonum-type? u) (fuck-up))
  ((rectangular-type? u) (fuck-up))
  ((input-port-type? u) (fuck-up))
  ((output-port-type? u) (fuck-up))
  ((eof-object-type? u) (fuck-up))
  ((pointer-type? u) (fuck-up))
  ((internal-symbol-type? u)
   (internal-symbol-type-types-and-type-sets-that-directly-point-to u))
  ((external-symbol-type? u)
   (external-symbol-type-types-and-type-sets-that-directly-point-to u))
  ((primitive-procedure-type? u)
   (primitive-procedure-type-types-and-type-sets-that-directly-point-to u))
  ((native-procedure-type? u)
   (native-procedure-type-types-and-type-sets-that-directly-point-to u))
  ((foreign-procedure-type? u)
   (foreign-procedure-type-types-and-type-sets-that-directly-point-to u))
  ((continuation-type? u)
   (continuation-type-types-and-type-sets-that-directly-point-to u))
  ((string-type? u) (string-type-types-and-type-sets-that-directly-point-to u))
  ((structure-type? u)
   (structure-type-types-and-type-sets-that-directly-point-to u))
  ((headed-vector-type? u)
   (headed-vector-type-types-and-type-sets-that-directly-point-to u))
  ((nonheaded-vector-type? u)
   (nonheaded-vector-type-types-and-type-sets-that-directly-point-to u))
  ((displaced-vector-type? u)
   (displaced-vector-type-types-and-type-sets-that-directly-point-to u))
  (else (fuck-up))))

(define (set-types-and-type-sets-that-directly-point-to! u u/ws)
 ;; needs work: This is not really a type function.
 (cond
  ((null-type? u) (fuck-up))
  ((true-type? u) (fuck-up))
  ((false-type? u) (fuck-up))
  ((char-type? u) (fuck-up))
  ((fixnum-type? u) (fuck-up))
  ((flonum-type? u) (fuck-up))
  ((rectangular-type? u) (fuck-up))
  ((input-port-type? u) (fuck-up))
  ((output-port-type? u) (fuck-up))
  ((eof-object-type? u) (fuck-up))
  ((pointer-type? u) (fuck-up))
  ((internal-symbol-type? u)
   (set-internal-symbol-type-types-and-type-sets-that-directly-point-to!
    u u/ws))
  ((external-symbol-type? u)
   (set-external-symbol-type-types-and-type-sets-that-directly-point-to!
    u u/ws))
  ((primitive-procedure-type? u)
   (set-primitive-procedure-type-types-and-type-sets-that-directly-point-to!
    u u/ws))
  ((native-procedure-type? u)
   (set-native-procedure-type-types-and-type-sets-that-directly-point-to!
    u u/ws))
  ((foreign-procedure-type? u)
   (set-foreign-procedure-type-types-and-type-sets-that-directly-point-to!
    u u/ws))
  ((continuation-type? u)
   (set-continuation-type-types-and-type-sets-that-directly-point-to! u u/ws))
  ((string-type? u)
   (set-string-type-types-and-type-sets-that-directly-point-to! u u/ws))
  ((structure-type? u)
   (set-structure-type-types-and-type-sets-that-directly-point-to! u u/ws))
  ((headed-vector-type? u)
   (set-headed-vector-type-types-and-type-sets-that-directly-point-to! u u/ws))
  ((nonheaded-vector-type? u)
   (set-nonheaded-vector-type-types-and-type-sets-that-directly-point-to!
    u u/ws))
  ((displaced-vector-type? u)
   (set-displaced-vector-type-types-and-type-sets-that-directly-point-to!
    u u/ws))
  (else (fuck-up))))

;;; Type type relations

(define (wide-clones? u1 u2)
 ;; This is only used by PRINT-NUMBER-OF-CALL-SITES-THAT-DISPATCH-ON-CLONES.
 (and (native-procedure-type? u1)
      (native-procedure-type? u2)
      (eq? (wide-prototype u1) (wide-prototype u2))))

;;; Type type-set relations

(define (member? u w)
 (let ((i (type-index u)))
  (let loop ((node (type-set-red-black-tree-node w)))
   ;; conventions: NODE
   (and node
	(or (= i (red-black-tree-node-key node))
	    (if (< i (red-black-tree-node-key node))
		(loop (red-black-tree-node-left node))
		(loop (red-black-tree-node-right node))))))))

;;; Type type-set procedures

(define (insert-member! u w)
 (define (left-rotate node)
  ;; conventions: NODE
  (make-red-black-tree-node
   (red-black-tree-node-type (red-black-tree-node-right node))
   (red-black-tree-node-key (red-black-tree-node-right node))
   (make-red-black-tree-node
    (red-black-tree-node-type node)
    (red-black-tree-node-key node)
    (red-black-tree-node-left node)
    (red-black-tree-node-left (red-black-tree-node-right node))
    (red-black-tree-node-red? node))
   (red-black-tree-node-right (red-black-tree-node-right node))
   (red-black-tree-node-red? (red-black-tree-node-right node))))
 (define (right-rotate node)
  ;; conventions: NODE
  (make-red-black-tree-node
   (red-black-tree-node-type (red-black-tree-node-left node))
   (red-black-tree-node-key (red-black-tree-node-left node))
   (red-black-tree-node-left (red-black-tree-node-left node))
   (make-red-black-tree-node
    (red-black-tree-node-type node)
    (red-black-tree-node-key node)
    (red-black-tree-node-right (red-black-tree-node-left node))
    (red-black-tree-node-right node)
    (red-black-tree-node-red? node))
   (red-black-tree-node-red? (red-black-tree-node-left node))))
 (let ((i (type-index u)))
  (set-type-set-red-black-tree-node!
   w
   (if (type-set-red-black-tree-node w)
       (let loop ((node (type-set-red-black-tree-node w)))
	;; conventions: NODE
	(cond
	 ((= i (red-black-tree-node-key node)) node)
	 (else
	  (if (< i (red-black-tree-node-key node))
	      (set-red-black-tree-node-left!
	       node
	       (if (red-black-tree-node-left node)
		   (loop (red-black-tree-node-left node))
		   (make-red-black-tree-node u i #f #f #t)))
	      (set-red-black-tree-node-right!
	       node
	       (if (red-black-tree-node-right node)
		   (loop (red-black-tree-node-right node))
		   (make-red-black-tree-node u i #f #f #t))))
	  (cond
	   ;; Both children are red and one grandchild is red.
	   ((and (red-black-tree-node-left node)
		 (red-black-tree-node-red? (red-black-tree-node-left node))
		 (red-black-tree-node-right node)
		 (red-black-tree-node-red? (red-black-tree-node-right node))
		 (or (and (red-black-tree-node-left
			   (red-black-tree-node-left node))
			  (red-black-tree-node-red?
			   (red-black-tree-node-left
			    (red-black-tree-node-left node))))
		     (and (red-black-tree-node-right
			   (red-black-tree-node-left node))
			  (red-black-tree-node-red?
			   (red-black-tree-node-right
			    (red-black-tree-node-left node))))
		     (and (red-black-tree-node-left
			   (red-black-tree-node-right node))
			  (red-black-tree-node-red?
			   (red-black-tree-node-left
			    (red-black-tree-node-right node))))
		     (and (red-black-tree-node-right
			   (red-black-tree-node-right node))
			  (red-black-tree-node-red?
			   (red-black-tree-node-right
			    (red-black-tree-node-right node))))))
	    (when (red-black-tree-node-red? node) (fuck-up))
	    (set-red-black-tree-node-red?! node #t)
	    (set-red-black-tree-node-red?! (red-black-tree-node-left node) #f)
	    (set-red-black-tree-node-red?! (red-black-tree-node-right node) #f)
	    node)
	   (else
	    (cond
	     ;; The left child and its right child are red.
	     ((and
	       (red-black-tree-node-left node)
	       (red-black-tree-node-red? (red-black-tree-node-left node))
	       (red-black-tree-node-right (red-black-tree-node-left node))
	       (red-black-tree-node-red?
		(red-black-tree-node-right (red-black-tree-node-left node))))
	      (when (red-black-tree-node-red? node) (fuck-up))
	      (set-red-black-tree-node-left!
	       node (left-rotate (red-black-tree-node-left node))))
	     ;; The right child and its left child are red.
	     ((and
	       (red-black-tree-node-right node)
	       (red-black-tree-node-red? (red-black-tree-node-right node))
	       (red-black-tree-node-left (red-black-tree-node-right node))
	       (red-black-tree-node-red?
		(red-black-tree-node-left (red-black-tree-node-right node))))
	      (when (red-black-tree-node-red? node) (fuck-up))
	      (set-red-black-tree-node-right!
	       node (right-rotate (red-black-tree-node-right node)))))
	    (cond
	     ;; The left child and its left child are red.
	     ((and
	       (red-black-tree-node-left node)
	       (red-black-tree-node-red? (red-black-tree-node-left node))
	       (red-black-tree-node-left (red-black-tree-node-left node))
	       (red-black-tree-node-red?
		(red-black-tree-node-left (red-black-tree-node-left node))))
	      (when (red-black-tree-node-red? node) (fuck-up))
	      (set-red-black-tree-node-red?!
	       (red-black-tree-node-left node) #f)
	      (set-red-black-tree-node-red?! node #t)
	      (right-rotate node))
	     ;; The right child and its right child are red.
	     ((and
	       (red-black-tree-node-right node)
	       (red-black-tree-node-red? (red-black-tree-node-right node))
	       (red-black-tree-node-right (red-black-tree-node-right node))
	       (red-black-tree-node-red?
		(red-black-tree-node-right (red-black-tree-node-right node))))
	      (when (red-black-tree-node-red? node) (fuck-up))
	      (set-red-black-tree-node-red?!
	       (red-black-tree-node-right node) #f)
	      (set-red-black-tree-node-red?! node #t)
	      (left-rotate node))
	     (else node)))))))
       (make-red-black-tree-node u i #f #f #t))))
 (let ((node (type-set-red-black-tree-node w)))
  (when (and (red-black-tree-node-red? node)
	     (or (and (red-black-tree-node-left node)
		      (red-black-tree-node-red?
		       (red-black-tree-node-left node)))
		 (and (red-black-tree-node-right node)
		      (red-black-tree-node-red?
		       (red-black-tree-node-right node)))))
   (set-red-black-tree-node-red?! node #f))))

;;; Type environment relations

(define (escapes? u e)
 ;; It is possible to tighten the notion of escaping by requiring that U be
 ;; frobbed in the continuation of some call to E. For the types in question,
 ;; the notion of frobbing is:
 ;;   (EQ? X1 X2) frobs all members of the type sets of X1 and X2
 ;;   (STRING-LENGTH X) frobs all string members of the type set of X
 ;;   (STRING-REF X1 X2) frobs all string members of the type set of X1
 ;;   (STRING-SET! X1 X2 X3) frobs all string members of the type set of X1
 ;;   (VECTOR-LENGTH X) frobs all nondegenerate headed vector members of the
 ;;     type set of X
 ;;   (VECTOR-REF X1 X2) frobs all nondegenerate vector members of X1
 ;;   (VECTOR-SET! X1 X2 X3) frobs all nondegenerate vector members of X1
 ;;   ((PRIMITIVE-PROCEDURE STRUCTURE-REF FOO I) X) frobs all structure
 ;;     members named FOO of the type set of X that have value
 ;;   ((PRIMITIVE-PROCEDURE STRUCTURE-SET! FOO I) X1 X2) frobs all structure
 ;;     members named FOO of the type set of X1 that have value
 ;;   (C X) frobs all continuation members of the type set of C that have value
 ;;   (X0 X1 ... XN) frobs all native procedure members of the type set of X0
 ;;     that have value and that can take N arguments.
 (not (not (memq u (environment-escaping-types e)))))

;;; Type-set creation

(define *wi* #f)
(define *ws* #f)
(define *w1* #f)
(define *w* #f)
(define *void* #f)
(define *null* #f)
(define *input-port* #f)
(define *output-port* #f)
(define *foreign-char-type-set* #f)
(define *foreign-fixnum-type-set* #f)
(define *foreign-flonum-type-set* #f)
(define *foreign-string-type-set* #f)
(define *foreign-input-port-type-set* #f)
(define *foreign-output-port-type-set* #f)
(define *foreign-pointer-type-set* #f)

(define (reinitialize-types-and-type-sets!)
 (set! *wi* 0)
 (set! *ws* '())
 (for-each (lambda (x)
	    (set-expression-continuation-type! x #f)
	    (set-expression-string-type! x #f)
	    (set-expression-structure-types! x '())
	    (set-expression-headed-vector-types! x '())
	    (set-expression-nonheaded-vector-types! x '()))
	   *xs*)
 (set! <null> 'null)
 (set! *null-type-used?* #f)
 (set! *null-type-use-count* 0)
 (set! <true> 'true)
 (set! *true-type-used?* #f)
 (set! *true-type-use-count* 0)
 (set! <false> 'false)
 (set! *false-type-used?* #f)
 (set! *false-type-use-count* 0)
 (set! <char> 'char)
 (set! *char-type-used?* #f)
 (set! *char-type-use-count* 0)
 (set! <fixnum> 'fixnum)
 (set! *fixnum-type-used?* #f)
 (set! *fixnum-type-use-count* 0)
 (set! <flonum> 'flonum)
 (set! *flonum-type-used?* #f)
 (set! *flonum-type-use-count* 0)
 (set! <rectangular> 'rectangular)
 (set! *rectangular-type-used?* #f)
 (set! *rectangular-type-use-count* 0)
 (set! <input-port> 'input-port)
 (set! *input-port-type-used?* #f)
 (set! *input-port-type-use-count* 0)
 (set! <output-port> 'output-port)
 (set! *output-port-type-used?* #f)
 (set! *output-port-type-use-count* 0)
 (set! <eof-object> 'eof-object)
 (set! *eof-object-type-used?* #f)
 (set! *eof-object-type-use-count* 0)
 (set! <pointer> 'pointer)
 (set! *pointer-type-used?* #f)
 (set! *pointer-type-use-count* 0)
 (set! *internal-symbol-types* '())
 (set! *external-symbol-types* '())
 (set! *primitive-procedure-types* '())
 (for-each (lambda (u) (set-native-procedure-type-used?! u #f))
	   *native-procedure-types*)
 (set! *foreign-procedure-types* '())
 (set! *continuation-types* '())
 (set! *string-types* '())
 ;; The following is needed to reset the index cache.
 (set! <nonreclaimable-string> #f)
 (set! <nonreclaimable-string> (<string> #f))
 (set! *structure-types* '())
 (set! *headed-vector-types* '())
 (set! *nonheaded-vector-types* '())
 ;; The following is needed to reset the index cache.
 (set! <top-level-nonheaded-vector> #f)
 (set! <top-level-nonheaded-vector>
       (<nonheaded-vector> (list <nonreclaimable-string>) #f))
 (set! *displaced-vector-types* '())
 ;; needs work: To enforce that
 ;;             (NONHEADED-VECTOR-TYPE-ELEMENT <top-level-nonheaded-vector>)
 ;;             is never widened since ARGV will be passed a nonheaded vector
 ;;             of strings.
 (set! *w1* (create-anonymous-type-set <top-level-nonheaded-vector>))
 (set! *w* (create-anonymous-type-set))
 (set! *void* (create-anonymous-type-set))
 (set! *null* (create-anonymous-type-set <null>))
 (set! *input-port* (create-anonymous-type-set <input-port>))
 (set! *output-port* (create-anonymous-type-set <output-port>))
 (set! *foreign-char-type-set* (create-anonymous-type-set <char>))
 (set! *foreign-fixnum-type-set* (create-anonymous-type-set <fixnum>))
 (set! *foreign-flonum-type-set* (create-anonymous-type-set <flonum>))
 (set! *foreign-string-type-set*
       (create-anonymous-type-set <nonreclaimable-string>))
 (set! *foreign-input-port-type-set* (create-anonymous-type-set <input-port>))
 (set! *foreign-output-port-type-set*
       (create-anonymous-type-set <output-port>))
 (set! *foreign-pointer-type-set* (create-anonymous-type-set <pointer>)))

(define (create-type-set location)
 (when *types-frozen?* (fuck-up))
 (let ((w (make-type-set location #f (unspecified) 0 *wi* 0)))
  (set-type-set-fictitious?! w #t)
  (set-type-set-link! w w)
  (set! *wi* (+ *wi* 1))
  (set! *ws* (cons w *ws*))
  w))

(define (create-anonymous-type-set . types)
 ;; This and <CONTINUATION> are the only type and type-set creators that can
 ;; be called when types are frozen.
 (let ((w (make-type-set #f #f (unspecified) 0 (unused) 0)))
  ;; This is a real kludge.
  (set-type-set-fictitious?!
   w (and (<= (length types) 1) (every fictitious? types)))
  (for-each (lambda (u) (insert-member! u w)) types)
  w))

;;; Type-set properties

(define (type-set-alignment? w)
 (not (zero? (bit-and (type-set-booleans w) 64))))

(define (set-type-set-alignment?! w p?)
 (unless (boolean? p?) (fuck-up))
 (set-type-set-booleans!
  w
  (if p?
      (bit-or (type-set-booleans w) 64)
      (bit-and (type-set-booleans w) (bit-not 64)))))

(define (type-set-size? w)
 (not (zero? (bit-and (type-set-booleans w) 32))))

(define (set-type-set-size?! w p?)
 (unless (boolean? p?) (fuck-up))
 (set-type-set-booleans!
  w
  (if p?
      (bit-or (type-set-booleans w) 32)
      (bit-and (type-set-booleans w) (bit-not 32)))))

(define (type-set-marked? w)
 (not (zero? (bit-and (type-set-booleans w) 16))))

(define (set-type-set-marked?! w p?)
 (unless (boolean? p?) (fuck-up))
 (set-type-set-booleans!
  w
  (if p?
      (bit-or (type-set-booleans w) 16)
      (bit-and (type-set-booleans w) (bit-not 16)))))

(define (type-set-used? w)
 (not (zero? (bit-and (type-set-booleans w) 8))))

(define (set-type-set-used?! w p?)
 (unless (boolean? p?) (fuck-up))
 (set-type-set-booleans!
  w
  (if p?
      (bit-or (type-set-booleans w) 8)
      (bit-and (type-set-booleans w) (bit-not 8)))))

(define (type-set-squeezable? w)
 (not (zero? (bit-and (type-set-booleans w) 4))))

(define (set-type-set-squeezable?! w p?)
 (unless (boolean? p?) (fuck-up))
 (set-type-set-booleans!
  w
  (if p?
      (bit-or (type-set-booleans w) 4)
      (bit-and (type-set-booleans w) (bit-not 4)))))

(define (type-set-squishable? w)
 (not (zero? (bit-and (type-set-booleans w) 2))))

(define (set-type-set-squishable?! w p?)
 (unless (boolean? p?) (fuck-up))
 (set-type-set-booleans!
  w
  (if p?
      (bit-or (type-set-booleans w) 2)
      (bit-and (type-set-booleans w) (bit-not 2)))))

(define (type-set-fictitious? w)
 (not (zero? (bit-and (type-set-booleans w) 1))))

(define (set-type-set-fictitious?! w p?)
 (unless (boolean? p?) (fuck-up))
 (set-type-set-booleans!
  w
  (if p?
      (bit-or (type-set-booleans w) 1)
      (bit-and (type-set-booleans w) (bit-not 1)))))

(define (can-be? m w)
 (let loop ((node (type-set-red-black-tree-node w)))
  ;; conventions: NODE
  (and node
       (or (m (red-black-tree-node-type node))
	   (loop (red-black-tree-node-left node))
	   (loop (red-black-tree-node-right node))))))

(define (can-be-non? m w) (can-be? (lambda (u) (not (m u))) w))

(define (must-be? m w) (not (can-be-non? m w)))

(define (void? w) (not (type-set-red-black-tree-node w)))

(define (monomorphic? w)
 ;; This really shouldn't be called until after APPLY-CLOSED-WORLD-ASSUMPTION!
 ;; is called since all of the multiple members might turn out to be the same.
 (and (type-set-red-black-tree-node w)
      (not (red-black-tree-node-left (type-set-red-black-tree-node w)))
      (not (red-black-tree-node-right (type-set-red-black-tree-node w)))))

(define (multimorphic? w)
 ;; This really shouldn't be called until after APPLY-CLOSED-WORLD-ASSUMPTION!
 ;; is called since all of the multiple members might turn out to be the same.
 (and (type-set-red-black-tree-node w)
      (or (red-black-tree-node-left (type-set-red-black-tree-node w))
	  (red-black-tree-node-right (type-set-red-black-tree-node w)))))

(define (fake? w)
 (and (or (void? w) (monomorphic? w)) (must-be? fictitious? w)))

;;; Type-set functions

(define (the-member w)
 (unless (monomorphic? w) (fuck-up))
 (red-black-tree-node-type (type-set-red-black-tree-node w)))

(define (the-member-that m w)
 (let ((us (members-that m w)))
  (unless (= (length us) 1) (fuck-up))
  (first us)))

(define (members w)
 (let ((us '()))
  (let loop ((node (type-set-red-black-tree-node w)))
   ;; conventions: NODE
   (when node
    (loop (red-black-tree-node-right node))
    (set! us (cons (red-black-tree-node-type node) us))
    (loop (red-black-tree-node-left node))))
  us))

(define (members-that m w) (remove-if-not m (members w)))

;;; Type-set procedures

(define (for-each-member p w)
 (let loop ((node (type-set-red-black-tree-node w)))
  ;; conventions: NODE
  (when node
   (p (red-black-tree-node-type node))
   (loop (red-black-tree-node-left node))
   (loop (red-black-tree-node-right node)))))

(define (set-members! w us)
 (set-type-set-red-black-tree-node! w #f)
 (for-each (lambda (u) (insert-member! u w)) us))

;;; Type-set type-set relations

(define (subtype-set? w1 w2) (must-be? (lambda (u1) (member? u1 w2)) w1))

;;; Variable creation

(define *gi* #f)
(define *gs* #f)

(define (initialize-variables!)
 (set! *gi* 0)
 (set! *gs* '()))

(define (create-variable s/g)
 (let ((g (cond ((s-expression? s/g)
		 (make-variable
		  (s-expression-version s/g)
		  (s-expression-cursor s/g)
		  (s-expression-pathname s/g)
		  (s-expression-line-position s/g)
		  (s-expression-character-position s/g)
		  (s-expression-character-position-within-line s/g)
		  *gi* (s-expression-datum s/g) (unspecified) (unspecified)
		  '() '() '() 0))
		((variable? s/g)
		 (make-variable
		  (variable-version s/g)
		  (variable-cursor s/g)
		  (variable-pathname s/g)
		  (variable-line-position s/g)
		  (variable-character-position s/g)
		  (variable-character-position-within-line s/g)
		  *gi* (variable-name s/g) (unspecified) (unspecified)
		  '() '() '() 0))
		(else (fuck-up)))))
  (set! *gi* (+ *gi* 1))
  (set! *gs* (cons g *gs*))
  g))

;;; Variable properties

(define (variable-accessed? g)
 (not (zero? (bit-and (variable-booleans g) 32))))

(define (set-variable-accessed?! g p?)
 (unless (boolean? p?) (fuck-up))
 (set-variable-booleans!
  g
  (if p?
      (bit-or (variable-booleans g) 32)
      (bit-and (variable-booleans g) (bit-not 32)))))

(define (variable-assigned? g)
 (not (zero? (bit-and (variable-booleans g) 16))))

(define (set-variable-assigned?! g p?)
 (unless (boolean? p?) (fuck-up))
 (set-variable-booleans!
  g
  (if p?
      (bit-or (variable-booleans g) 16)
      (bit-and (variable-booleans g) (bit-not 16)))))

(define (variable-local? g)
 (not (zero? (bit-and (variable-booleans g) 8))))

(define (set-variable-local?! g p?)
 (unless (boolean? p?) (fuck-up))
 (set-variable-booleans!
  g
  (if p?
      (bit-or (variable-booleans g) 8)
      (bit-and (variable-booleans g) (bit-not 8)))))

(define (variable-global? g)
 (not (zero? (bit-and (variable-booleans g) 4))))

(define (set-variable-global?! g p?)
 (unless (boolean? p?) (fuck-up))
 (set-variable-booleans!
  g
  (if p?
      (bit-or (variable-booleans g) 4)
      (bit-and (variable-booleans g) (bit-not 4)))))

(define (variable-hidden? g)
 (not (zero? (bit-and (variable-booleans g) 2))))

(define (set-variable-hidden?! g p?)
 (unless (boolean? p?) (fuck-up))
 (set-variable-booleans!
  g
  (if p?
      (bit-or (variable-booleans g) 2)
      (bit-and (variable-booleans g) (bit-not 2)))))

(define (variable-slotted? g)
 (not (zero? (bit-and (variable-booleans g) 1))))

(define (set-variable-slotted?! g p?)
 (unless (boolean? p?) (fuck-up))
 (set-variable-booleans!
  g
  (if p?
      (bit-or (variable-booleans g) 1)
      (bit-and (variable-booleans g) (bit-not 1)))))

(define (variable-used? g) (called? (variable-environment g)))

(define (defined-at-top-level? g)
 (or (and (not (empty? (parent (variable-environment g))))
	  (empty? (parent (parent (variable-environment g))))
	  (let? (variable-environment g)))
     (and (not (empty? (parent (variable-environment g))))
	  (not (empty? (parent (parent (variable-environment g)))))
	  (empty? (parent (parent (parent (variable-environment g)))))
	  (let? (variable-environment g))
	  (let? (parent (variable-environment g))))
     (and (not (empty? (parent (variable-environment g))))
	  (not (empty? (parent (parent (variable-environment g)))))
	  (not (empty? (parent (parent (parent (variable-environment g))))))
	  (empty? (parent (parent (parent (parent (variable-environment g))))))
	  (let? (variable-environment g))
	  (let? (parent (variable-environment g)))
	  (let? (parent (parent (variable-environment g)))))
     (and (not (empty? (parent (variable-environment g))))
	  (not (empty? (parent (parent (variable-environment g)))))
	  (not (empty? (parent (parent (parent (variable-environment g))))))
	  (not
	   (empty?
	    (parent (parent (parent (parent (variable-environment g)))))))
	  (empty?
	   (parent
	    (parent (parent (parent (parent (variable-environment g)))))))
	  (let? (variable-environment g))
	  (let? (parent (variable-environment g)))
	  (let? (parent (parent (variable-environment g))))
	  (let? (parent (parent (parent (variable-environment g))))))))

(define (accessed? g)
 (if *during-closure-conversion?*
     (case *closure-conversion-method*
      ((baseline) #t)
      ((conventional)
       (or (defined-at-top-level? g) (not (null? (accesses g)))))
      ((lightweight) (variable-accessed? g))
      (else (fuck-up)))
     (variable-accessed? g)))

(define (assigned? g)
 (if *during-closure-conversion?*
     (case *closure-conversion-method*
      ((baseline) #t)
      ((conventional)
       (or (defined-at-top-level? g) (not (null? (assignments g)))))
      ((lightweight) (variable-assigned? g))
      (else (fuck-up)))
     (variable-assigned? g)))

(define (accesses g) (remove-if-not reached? (variable-accesses g)))

(define (assignments g)
 ;; needs work: Should this be EXECUTED??
 (remove-if-not reached? (variable-assignments g)))

(define (references g)
 ;; needs work: Should this be EXECUTED? for assignments?
 (remove-if-not reached? (variable-references g)))

(define (must-alias? g/u)
 ;; needs work: This is not memoized but should be.
 (cond
  ((variable? g/u)
   (and
    (not
     (and
      (accessed? g/u)			;This is just an optimization.
      ;; (\exists e\in A\cup S)
      (some (lambda (x)
	     (and
	      ;; \NONTRIVIALREFERENCE{e}
	      (nontrivial-reference? x)
	      ;; (\exists p\in P)
	      (let loop? ((e (expression-environment x)))
	       ;; \PROPERLYNESTEDIN{p}{p(x)}
	       (and
		(not (eq? e (variable-environment g/u)))
		(or
		 ;; This can't require (NOT (FICTITIOUS? (ENVIRONMENT-TYPE E)))
		 ;; because then X{645} in test32.sc unsoundly becomes local
		 ;; and Y{645} in test33.sc and Y{647} in test34.sc unsoundly
		 ;; become global.
		 ;; \ESCAPES{p}{p(x)}
		 (escapes? (environment-type e) (variable-environment g/u))
		 ;; \NESTEDIN{p(e)}{p}
		 (loop? (parent e)))))))
	    ;; x(e)=x
	    (references g/u))))
    (not
     (and
      (accessed? g/u)			;This is just an optimization.
      ;; \PROPERLYCALLS{p(x)}{p(x)}
      (recursive? (variable-environment g/u))
      (begin (for-each (lambda (e) (set-environment-marked1?! e #f)) *es*)
	     (unmark-types-and-type-sets!)
	     ;; \NESTEDIN{p(x)}{p(x(e'))}
	     (let loop ((e (variable-environment g/u)))
	      (unless (empty? e)
	       (set-environment-marked1?! e #t)
	       (loop (parent e))))
	     ;; \NESTEDIN{p(e')}{p(x)}
	     (let loop ((e (variable-environment g/u)))
	      (for-each
	       (lambda (x)
		(case (expression-kind x)
		 ;; (\exists e'\in A)
		 ((access)
		  (when (environment-marked1?
			 (variable-environment (expression-variable x)))
		   ;; \POINTSTO{\alpha(e')}{p}
		   ;; This is done just for side effect, to set the MARKED?
		   ;; bits.
		   (for-each-pointed-to-type
		    (lambda (u) #f) (expression-type-set x))))
		 ((lambda converted-lambda converted-continuation)
		  (when (environment-used? (expression-lambda-environment x))
		   (loop (expression-lambda-environment x))))))
	       (environment-expressions e)))
	     ;; This is done just for side effect, to set the MARKED1? bits.
	     ;; \PROPERLYCALLS{p(x)}{p}
	     (some-proper-callee (lambda (e) #f)
				 environment-marked1?
				 set-environment-marked1?!
				 (variable-environment g/u))
	     ;; (\exists e\in A\cup S)
	     (some (lambda (x)
		    (and
		     ;; \NONTRIVIALREFERENCE{e}
		     (nontrivial-reference? x)
		     ;; (\exists p\in P)
		     (let loop? ((e (expression-environment x)))
		      (and
		       ;; \PROPERLYNESTEDIN{p}{p(x(e))}
		       (not (eq? e (variable-environment g/u)))
		       ;; \PROPERLYCALLS{p(x)}{p}
		       (environment-marked1? e)
		       ;; (\exists e'\in A)
		       ;; \NESTEDIN{p(e')}{\NESTEDIN{p(x)}{p(x(e'))}}
		       (or
			;; \POINTSTO{\alpha(e')}{p}
			(native-procedure-type-marked? (environment-type e))
			;; \NESTEDIN{p(e)}{p}
			(loop? (parent e)))))))
		   ;; x(e)=x
		   (references g/u)))))))
  ((native-procedure-type? g/u)
   (and
    (not (some (lambda (e)
		(and
		 ;; note: We don't have \TYPEPREDICATEACCESSED{p}.
		 ;; \PROCEDUREACCESSED{p}
		 (environment-accessed? e)
		 ;; \ESCAPES{p}{\PARENTPARAMETER{p}}
		 (some (lambda (e) (escapes? g/u e)) (ancestors e))))
	       (narrow-clones g/u)))
    (not
     (and
      ;; This is just an optimization.
      (some (lambda (u/w)
	     (and (type-set? u/w)
		  (monomorphic? u/w)
		  (variable? (type-set-location u/w))
		  (accessed? (type-set-location u/w))))
	    (types-and-type-sets-that-directly-point-to g/u))
      (not (fictitious? g/u))		;This is just an optimization.
      ;; \POINTSTO{\alpha(e')}{p}
      (let ((xs '()))
       (unmark-types-and-type-sets!)
       (let loop ((u g/u))
	(unless (type-marked? u)
	 (set-type-marked?! u #t)
	 (for-each
	  (lambda (u/w)
	   (cond ((type-set? u/w)
		  (unless (type-set-marked? u/w)
		   (set-type-set-marked?! u/w #t)
		   (cond ((type? (type-set-location u/w))
			  (loop (type-set-location u/w)))
			 ((and (expression? (type-set-location u/w))
			       ;; e'\in A
			       (eq? (expression-kind (type-set-location u/w))
				    'access))
			  (set! xs (cons (type-set-location u/w) xs))))))
		 ((type? u/w) (loop u/w))
		 (else (fuck-up))))
	  (types-and-type-sets-that-directly-point-to u))))
       (some
	(lambda (e)
	 (and
	  (environment-used? e)		;This is just an optimization.
	  ;; (\exists e\in C)
	  (some
	   (lambda (y)
	    (or
	     ;; For now, punt on any procedure with an implicit call site and
	     ;; treat it as if it were referenced recursively. This won't
	     ;; affect the top-level call site because the top-level procedure
	     ;; is never bound to a variable and thus never bound to a hidden
	     ;; variable. This will, however, mean that any variables that are
	     ;; bound to a procedure that has first-argument and
	     ;; continuation-argument call sites will not be hidden.
	     (not (explicit-call-site? y))
	     (begin
	      ;; This is done just for side effect, to set the MARKED1? bits.
	      ;; \CALLS{p'}{p(e)}
	      (some-caller (lambda (e) #f)
			   environment-marked1?
			   set-environment-marked1?!
			   (expression-environment (call-site-expression y)))
	      ;; (\exists e'\in A)
	      (some
	       (lambda (x)
		;; (\exists p'\in P)
		(let loop? ((e1 (expression-environment x)))
		 (and
		  ;; \PROPERLYCALLS{p'}{p'}
		  (recursive? e1)
		  (or
		   (and
		    ;; \CALLS{p'}{p(e)}
		    (environment-marked1? e1)
		    ;; \NESTEDIN{\PARENTPARAMETER{p}}{p'}
		    (begin
		     (for-each (lambda (e) (set-environment-marked2?! e #f))
			       *es*)
		     (let loop ((e e1))
		      (when (environment-used? e)
		       (set-environment-marked2?! e #t)
		       (for-each
			(lambda (x)
			 (case (expression-kind x)
			  ((lambda converted-lambda converted-continuation)
			   (loop (expression-lambda-environment x)))))
			(environment-expressions e))))
		     (some (lambda (e)
			    (and (environment-used? e)
				 (some environment-marked2? (ancestors e))))
			   (narrow-clones g/u))))
		   (and
		    ;; \NESTEDIN{p'}{p(x(e'))}
		    (not
		     (eq? e1 (variable-environment (expression-variable x))))
		    ;; \NESTEDIN{p(e')}{p'}
		    (loop? (parent e1)))))))
	       ;; \POINTSTO{\alpha(e')}{p}
	       xs))))
	   ;; \DIRECTLYCALLS{e}{p}
	   (call-sites e))))
	(narrow-clones g/u)))))))
  ((continuation-type? g/u)
   (and
    (not
     (and
      ;; note: We don't have \TYPEPREDICATEACCESSED{\sigma}.
      ;; \CONTINUATIONACCESSED{\sigma}
      (continuation-type-continuation-accessed? g/u)
      ;; (\exists\CANBE{\alpha(\ARGUMENT{1}{e(\sigma)})}{p})
      (can-be?
       (lambda (u1)
	(and
	 ;; p\in P
	 (native-procedure-type? u1)
	 (some
	  (lambda (e)
	   ;; needs work: Doesn't handle varargs.
	   (when (rest? e) (unimplemented #f "unimplemented"))
	   (and
	    ;; \CANBE{\alpha(\PARAMETER{1}{p})}{\sigma}
	    (member? g/u (first-parameter-type-set (environment-expression e)))
	    ;; \ESCAPES{\sigma}{p}
	    (escapes? g/u e)))
	  (narrow-clones u1))))
       (expression-type-set
	(first-argument (continuation-type-allocating-expression g/u))))))
    (not
     (and
      ;; note: We don't have \TYPEPREDICATEACCESSES{e}{\sigma}.
      ;; This is just an optimization.
      (continuation-type-continuation-accessed? g/u)
      (begin
       ;; This is done just for side effect, to set the MARKED2? bits.
       ;; \PROPERLYCALLS{p(e(\sigma))}{p}
       (some-proper-callee (lambda (e) #f)
			   environment-marked1?
			   set-environment-marked1?!
			   (expression-environment
			    (continuation-type-allocating-expression g/u)))
       ;; \PROPERLYCALLS{p}{p(e(\sigma)){p}}
       (some-proper-caller (lambda (e) #f)
			   environment-marked2?
			   set-environment-marked2?!
			   (expression-environment
			    (continuation-type-allocating-expression g/u)))
       ;; \PROPERLYCALLS{p}{\PROPERLYCALLS{p(e(\sigma))}{p}}
       (for-each
	(lambda (e)
	 (unless (environment-marked1? e) (set-environment-marked2?! e #f)))
	*es*)
       (let ((xs '()))
	;; \NESTEDIN{p(e(\sigma))}{p(x(e'))}
	(let loop ((e (expression-environment
		       (continuation-type-allocating-expression g/u))))
	 (unless (empty? e)
	  (for-each (lambda (g)
		     (for-each (lambda (x)
				;; \POINTSTO{\alpha(e')}{\sigma}
				(when (points-to? (expression-type-set x) g/u)
				 (set! xs (cons x xs))))
			       ;; (\exists e'\in A)
			       (accesses g)))
		    (variables e))
	  (loop (parent e))))
	;; (\exists e\in C)
	(some
	 (lambda (y)
	  ;; needs work: Doesn't handle implicit continuation calls.
	  (unless (explicit-call-site? y) (unimplemented y "unimplemented"))
	  ;; (\exists\CANBE{\alpha(\ARGUMENT{1}{e(\sigma)})}{p})
	  (some
	   (lambda (e)
	    (and (environment-marked2? e)
		 (member? (environment-type e)
			  (expression-type-set
			   (first-argument
			    (continuation-type-allocating-expression g/u))))
		 (some (lambda (x)
			;; \PROPERLYNESTEDIN{p}{p(x(e'))}
			(properly-nested-in?
			 e (variable-environment (expression-variable x))))
		       xs)))
	   ;; \CALLS{p}{p(e)}
	   (callers (expression-environment (call-site-expression y)))))
	 ;; \CONTINUATIONACCESSES{e}{\sigma}
	 (continuation-type-call-sites g/u))))))))
  (else (fuck-up))))

(define (localizable? g)
 ;; needs work: This is not memoized but should be.
 (case *closure-conversion-method*
  ((baseline) #f)
  ((conventional) (not (some free-reference? (references g))))
  ((lightweight)
   (and (or (not (accessed? g))		;This is just an optimization.
	    (every (lambda (x)
		    (or (not (nontrivial-reference? x))
			(in-lined-in? x (variable-environment g))))
		   (references g)))
	(must-alias? g)))
  (else (fuck-up))))

(define (globalizable? g)
 ;; needs work: This is not memoized but should be.
 (case *closure-conversion-method*
  ((baseline) #f)
  ((conventional)
   (and (or (not (empty? (parent (variable-environment g))))
	    (= (length (variables (variable-environment g))) 1)
	    (not (eq? g (first (variables (variable-environment g))))))
	(let loop ((e (variable-environment g)))
	 (or (empty? (parent e)) (and (let? e) (loop (parent e)))))))
  ((lightweight)
   (or (not (called-more-than-once? (variable-environment g)))
       (and (not (reentrant? (variable-environment g))) (must-alias? g))))
  (else (fuck-up))))

(define (hideable? g)
 ;; needs work: This is not memoized but should be.
 (case *closure-conversion-method*
  ((baseline conventional) #f)
  ((lightweight)
   (and (accessed? g)			;This is just an optimization.
	(monomorphic? (variable-type-set g))
	(native-procedure-type? (the-member (variable-type-set g)))
	;; The paper doesn't contain this. This is here because we don't
	;; compute ANCESTOR? for unused environments.
	(environment-used? (the-member (variable-type-set g)))
	(every
	 (lambda (x)
	  (or (not (nontrivial-reference? x))
	      (every (lambda (e1)
		      ;; The paper doesn't contain this. This is here because
		      ;; we don't compute ANCESTOR? for unused environments.
		      (or (not (environment-used? e1))
			  (every (lambda (e2)
				  (nested-in? (expression-environment x) e2))
				 (ancestors e1))))
		     (narrow-clones (the-member (variable-type-set g))))))
	 (accesses g))
	(must-alias? (the-member (variable-type-set g)))))
  (else (fuck-up))))

(define (local? g) (variable-local? g))

(define (determine-whether-local? g)
 (if *globals?*
     (and (accessed? g)
	  (not (fictitious? (variable-type-set g)))
	  (localizable? g)
	  (not (global? g)))
     (and (accessed? g)
	  (not (fictitious? (variable-type-set g)))
	  (localizable? g))))

(define (infer-all-whether-local?! p?)
 (when *p7?* (notify "Determining whether variables are local"))
 (for-each
  (lambda (g)
   (cond ((local? g)
	  (when (and p? (not (determine-whether-local? g)))
	   (fuck-up)))
	 ((determine-whether-local? g)
	  (set-variable-local?! g #t)
	  (when *p7?*
	   (notify "  ~a{~s} is local" (variable-name g) (variable-index g)))
	  (set! *again?* #t))))
  *gs*))

(define (global? g) (variable-global? g))

(define (determine-whether-global? g)
 (if *globals?*
     (and (accessed? g)
	  (not (fictitious? (variable-type-set g)))
	  (globalizable? g))
     (and (accessed? g)
	  (not (fictitious? (variable-type-set g)))
	  (globalizable? g)
	  (not (local? g)))))

(define (infer-all-whether-global?! p?)
 (when *p7?* (notify "Determining whether variables are global"))
 (for-each
  (lambda (g)
   (cond ((global? g)
	  (when (and p? (not (determine-whether-global? g)))
	   (fuck-up)))
	 ((determine-whether-global? g)
	  (set-variable-global?! g #t)
	  (when *p7?*
	   (notify "  ~a{~s} is global" (variable-name g) (variable-index g)))
	  (set! *again?* #t))))
  *gs*))

(define (hidden? g) (variable-hidden? g))

(define (determine-whether-hidden? g)
 (and (accessed? g)
      (not (fictitious? (variable-type-set g)))
      (hideable? g)
      (not (local? g))
      (not (global? g))))

(define (infer-all-whether-hidden?! p?)
 (when *p7?* (notify "Determining whether variables are hidden"))
 (for-each
  (lambda (g)
   (cond ((hidden? g)
	  (when (and p? (not (determine-whether-hidden? g)))
	   (fuck-up)))
	 ((determine-whether-hidden? g)
	  (set-variable-hidden?! g #t)
	  (when *p7?*
	   (notify "  ~a{~s} is hidden" (variable-name g) (variable-index g)))
	  (set! *again?* #t))))
  *gs*))

(define (slotted? g) (variable-slotted? g))

(define (determine-whether-slotted? g)
 (and (accessed? g)
      (not (fictitious? (variable-type-set g)))
      (not (local? g))
      (not (global? g))
      (not (hidden? g))))

(define (infer-all-whether-slotted?! p?)
 (when *p7?* (notify "Determining whether variables are slotted"))
 (for-each
  (lambda (g)
   (cond ((slotted? g)
	  (when (and p? (not (determine-whether-slotted? g)))
	   (fuck-up)))
	 ((determine-whether-slotted? g)
	  (set-variable-slotted?! g #t)
	  (when *p7?*
	   (notify "  ~a{~s} is slotted" (variable-name g) (variable-index g)))
	  (set! *again?* #t))))
  *gs*))

;;; Environment creation

(define *ei* #f)
(define *es* #f)
(define *es0* #f)

(define (initialize-environments!)
 (set! *ei* 0)
 (set! *es* '())
 (set! *es0* '()))

(define (create-environment v f)
 ;; conventions: V F
 (let ((e (make-environment
	   *ei* (unspecified)
	   (if v
	       (if (symbol? v)
		   (string-append
		    (symbol->string v) "[" (number->string *ei*) "]")
		   (string-append "[clone " v " " (number->string *ei*) "]"))
	       (string-append "[inside " f " " (number->string *ei*) "]"))
	   #f '() (unspecified) (unspecified)
	   (unspecified) (unspecified) (unspecified) (unspecified)
	   (unspecified) (unspecified) (unspecified) (unspecified)
	   (unspecified) (unspecified) (unspecified) (unspecified)
	   (unspecified) (unspecified) (unspecified) (unspecified)
	   (unspecified) (unspecified) (unspecified) 0)))
  (set! *ei* (+ *ei* 1))
  (set! *es* (cons e *es*))
  (set-environment-narrow-prototype! e e)
  (set-environment-narrow-clones! e (list e))
  (set-environment-wide-prototype! e e)
  e))

;;; Environment properties

(define (environment-marked1? e)
 (not (zero? (bit-and (environment-booleans e) 1024))))

(define (set-environment-marked1?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 1024)
      (bit-and (environment-booleans e) (bit-not 1024)))))

(define (environment-marked2? e)
 (not (zero? (bit-and (environment-booleans e) 512))))

(define (set-environment-marked2?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 512)
      (bit-and (environment-booleans e) (bit-not 512)))))

(define (environment-passes-parameters-globally? e)
 (not (zero? (bit-and (environment-booleans e) 256))))

(define (set-environment-passes-parameters-globally?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 256)
      (bit-and (environment-booleans e) (bit-not 256)))))

(define (environment-has-region? e)
 (not (zero? (bit-and (environment-booleans e) 128))))

(define (set-environment-has-region?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 128)
      (bit-and (environment-booleans e) (bit-not 128)))))

(define (environment-has-nonatomic-region? e)
 (not (zero? (bit-and (environment-booleans e) 64))))

(define (set-environment-has-nonatomic-region?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 64)
      (bit-and (environment-booleans e) (bit-not 64)))))

(define (environment-recursive? e)
 (not (zero? (bit-and (environment-booleans e) 32))))

(define (set-environment-recursive?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 32)
      (bit-and (environment-booleans e) (bit-not 32)))))

(define (environment-reentrant? e)
 (not (zero? (bit-and (environment-booleans e) 16))))

(define (set-environment-reentrant?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 16)
      (bit-and (environment-booleans e) (bit-not 16)))))

(define (environment-called-more-than-once? e)
 (not (zero? (bit-and (environment-booleans e) 8))))

(define (set-environment-called-more-than-once?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 8)
      (bit-and (environment-booleans e) (bit-not 8)))))

(define (environment-has-external-self-tail-call? e)
 (not (zero? (bit-and (environment-booleans e) 4))))

(define (set-environment-has-external-self-tail-call?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 4)
      (bit-and (environment-booleans e) (bit-not 4)))))

(define (environment-has-external-continuation-call? e)
 (not (zero? (bit-and (environment-booleans e) 2))))

(define (set-environment-has-external-continuation-call?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 2)
      (bit-and (environment-booleans e) (bit-not 2)))))

(define (environment-has-closure? e)
 (not (zero? (bit-and (environment-booleans e) 1))))

(define (set-environment-has-closure?! e p?)
 (unless (boolean? p?) (fuck-up))
 (set-environment-booleans!
  e
  (if p?
      (bit-or (environment-booleans e) 1)
      (bit-and (environment-booleans e) (bit-not 1)))))

(define (environment-used? e) (and (called? e) (not (noop? e))))

(define (environment-accessed? e) (environment-used? e))

(define (has-region? e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-has-region? e))

(define (has-nonatomic-region? e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-has-nonatomic-region? e))

(define (recursive? e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-recursive? e))

(define (reentrant? e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-reentrant? e))

(define (called-more-than-once? e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-called-more-than-once? e))

(define (has-external-self-tail-call? e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-has-external-self-tail-call? e))

(define (has-external-continuation-call? e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-has-external-continuation-call? e))

(define (has-closure? e)
 (when (empty? e) (fuck-up))
 (environment-has-closure? e))

(define (determine-whether-has-closure? e)
 ;; note: It should never happen that an environment doesn't have a closure
 ;;       yet has hidden variables that hide as that environment.
 (some slotted? (variables e)))

(define (infer-all-whether-has-closure?! p?)
 (when *p7?* (notify "Determining whether environments have closures"))
 (for-each
  (lambda (e)
   (cond ((has-closure? e)
	  (when (and p? (not (determine-whether-has-closure? e))) (fuck-up)))
	 ((determine-whether-has-closure? e)
	  (set-environment-has-closure?! e #t)
	  (when *p7?* (notify "  ~a has a closure" (environment-name e)))
	  (set! *again?* #t))))
  *es*))

(define (empty? e) (eq? e #f))

(define (in-lined-in-recursive? e)
 (or (recursive? e)
     (and (unique-call-site? e)
	  (in-lined-in-recursive?
	   (expression-environment
	    (call-site-expression (unique-call-site e)))))))

(define (has-parent-slot? e) (not (empty? (parent-slot e))))

(define (unique-call-site? e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (and (= (length (environment-non-self-tail-call-sites e)) 1)
      (not (top-level-call-site? (unique-call-site e)))))

(define (has-self-tail-call? e)
 (some (lambda (y)
	(and (not (top-level-call-site? y)) (can-be-self-tail-call-to? y e)))
       (call-sites e)))

(define (converted-continuation? e)
 (eq? (expression-kind (environment-expression e)) 'converted-continuation))

(define (has-alloca? e)
 ;; note: This might not be the correct way to write this. I've been away from
 ;;       this code for a while I don't remember what the correct way is.
 (when (unique-call-site? e) (fuck-up))
 (or
  (some (lambda (x)
	 (and (executed? x)
	      (in-lined-in? (expression-environment x) e)
	      (some (lambda (u-e)
		     (and (type-used? (car u-e))
			  (stack-allocation? (cdr u-e))))
		    (expression-type-allocation-alist x))))
	*calls*)
  (case *closure-representation*
   ((immediate-flat)
    (unimplemented #f "Immediate flat closures are not (yet) implemented"))
   ((immediate-display)
    (unimplemented #f "Immediate display closures are not (yet) implemented"))
   ((indirect-flat)
    (unimplemented #f "Indirect flat closures are not (yet) implemented"))
   ((indirect-display)
    (unimplemented #f "Indirect display closures are not (yet) implemented"))
   ((linked)
    (some (lambda (e1)
	   (and (called? e1)
		(in-lined-in? e1 e)
		(stack-allocation? (allocation e1))))
	  *es*))
   (else (fuck-up)))))

(define (has-setjmp? e)
 ;; note: This might not be the correct way to write this. I've been away from
 ;;       this code for a while I don't remember what the correct way is.
 (when (unique-call-site? e) (fuck-up))
 (some (lambda (x)
	(and (environment-used? (expression-environment x))
	     (in-lined-in? (expression-environment x) e)
	     ;; needs work: This doesn't handle calls to CALL/CC via CALL/CC,
	     ;;             APPLY, FORK, or MUTEX. And it doesn't handle
	     ;;             CALL/CC calling a continuation.
	     (or (and (eq? (expression-kind x) 'call)
		      (= (length (expression-arguments x)) 1))
		 (and (eq? (expression-kind x) 'converted-call)
		      (= (length (expression-arguments x)) 2)))
	     (can-be? (primitive-procedure-type-named?
		       'call-with-current-continuation)
		      (expression-type-set (expression-callee x)))
	     (can-be? (lambda (u)
		       (and (native-procedure-type? u)
			    (callee-environment?
			     u
			     (recreate-call-site
			      (create-call-site x) 'first-argument))
			    (can-be-non?
			     fictitious?
			     (first-parameter-type-set
			      (callee-environment
			       u
			       (recreate-call-site
				(create-call-site x) 'first-argument))))))
		      (expression-type-set (first-argument x)))))
       *calls*))

;;; Environment functions

(define (call-sites e)
 (when (empty? e) (fuck-up))
 (environment-call-sites e))

(define (allocation e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-allocation e))

(define (distance-from-root e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-distance-from-root e))

(define (free-variables e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-free-variables e))

(define (quick-parent e)
 (when (or (empty? e) (not (called? e))) (fuck-up))
 (environment-quick-parent e))

(define (parent-slot e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-parent-slot e))

(define (descendents e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-descendents e))

(define (in-lined-environments e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (cons e (environment-properly-in-lined-environments e)))

(define (properly-in-lined-environments e)
 (when (or (empty? e) (not (environment-used? e))) (fuck-up))
 (environment-properly-in-lined-environments e))

(define (unique-call-site e)
 (when (or (empty? e)
	   (not (environment-used? e))
	   (not (= (length (environment-non-self-tail-call-sites e)) 1)))
  (fuck-up))
 (first (environment-non-self-tail-call-sites e)))

(define (infer-all-unique-call-site!)
 (when *p7?* (notify "Determining unique call sites"))
 (for-each
  (lambda (e)
   (when (environment-used? e)
    (let* ((ys0 (environment-non-self-tail-call-sites e))
	   (ys1 (remove-if-not (lambda (y)
				(or (top-level-call-site? y)
				    (not (can-be-self-tail-call-to? y e))))
			       ys0)))
     (when (< (length ys1) (length ys0))
      (set-environment-non-self-tail-call-sites! e ys1)
      (when (and *p7?* (= (length ys1) 1))
       (notify "  Determined the unique call site of ~a" (environment-name e)))
      (set! *again?* #t)))))
  *es*))

(define (parent e) (expression-environment (environment-expression e)))

(define (first-parameter-type-set e)
 (when (rest? e) (fuck-up))
 (if (converted? e)
     ;; needs work: I'm not sure this is correct.
     (variable-type-set (second (variables e)))
     (variable-type-set (first (variables e)))))

(define (return-type-set e)
 (when (empty? e) (fuck-up))
 ;; note: Nonconverted continuations never return.
 (if (noop? e)
     *void*
     (expression-type-set (expression-body (environment-expression e)))))

(define (environment-type e)
 (the-member (expression-type-set (environment-expression e))))

(define (home e)
 (if (unique-call-site? e)
     (home
      (expression-environment (call-site-expression (unique-call-site e))))
     e))

(define (lexical-nesting-depth e)
 (if (empty? (parent e)) 0 (+ (lexical-nesting-depth (parent e)) 1)))

(define (non-let-lexical-nesting-depth e)
 (cond ((empty? (parent e)) 0)
       ((let? (parent e)) (non-let-lexical-nesting-depth (parent e)))
       (else (+ (non-let-lexical-nesting-depth (parent e)) 1))))

;;; Environment environment relations

(define (ancestor? e1 e2)
 (when (or (empty? e1)
	   (not (environment-used? e1))
	   (empty? e2)
	   (not (environment-used? e2)))
  (fuck-up))
 (not (not (memq e1 (ancestors e2)))))

(define (determine-whether-ancestor? e1 e2)
 (or (some
      (lambda (g)
       (or (and (eq? e1 (variable-environment g)) (slotted? g))
	   (and (hidden? g)
		(some (lambda (e)
		       (and
			;; The paper doesn't contain this. This is here
			;; because we don't compute ANCESTOR? for unused
			;; environments.
			(environment-used? e)
			(ancestor? e1 e)))
		      (narrow-clones (the-member (variable-type-set g)))))))
      (free-variables e2))
     (some
      (lambda (g)
       (and (hidden? g)
	    (some (lambda (e)
		   (and
		    ;; The paper doesn't contain this. This is here because we
		    ;; don't compute ANCESTOR? for unused environments.
		    (environment-used? e)
		    (ancestor? e1 e)))
		  (narrow-clones (the-member (variable-type-set g))))))
      (variables e2))))

(define (infer-all-whether-ancestor?! p?)
 (when *p7?*
  (notify
   "Determining whether environments are ancestors of other environments"))
 (for-each (lambda (e2)
	    (when (environment-used? e2)
	     (let loop ((e1 (quick-parent e2)))
	      (unless (empty? e1)
	       (cond ((ancestor? e1 e2)
		      (when (and p? (not (determine-whether-ancestor? e1 e2)))
		       (fuck-up)))
		     ((determine-whether-ancestor? e1 e2)
		      (set-environment-ancestors! e2 (cons e1 (ancestors e2)))
		      (when #f		;debugging
		       (when *p7?*
			(notify "  ~a is an ancestor of ~a"
				(environment-name e1) (environment-name e2))))
		      (set! *again?* #t)))
	       (loop (quick-parent e1))))))
	   *es*))

(define (nested-in? e1 e2)
 ;; The NESTED-IN? relation is reflexive.
 (or (eq? e1 e2) (and (not (empty? e1)) (nested-in? (parent e1) e2))))

(define (properly-nested-in? e1 e2)
 ;; The PROPERLY-NESTED-IN? relation is irreflexive.
 (and (not (eq? e1 e2)) (nested-in? e1 e2)))

;;; Call-site creation

(define *y* #f)
(define *ys* #f)

(define (create-call-site expression)
 (unless expression (fuck-up))
 (make-call-site expression '()))

(define (recreate-call-site y p)
 ;; conventions: P
 (unless (and (not (top-level-call-site? y))
	      (or (eq? p 'first-argument)
		  (eq? p 'second-argument)
		  (eq? p 'continuation-argument))
	      (or (null? (call-site-offsets y))
		  (eq? (first (call-site-offsets y)) 'first-argument)
		  (eq? (first (call-site-offsets y)) 'second-argument)))
  (fuck-up))
 (make-call-site (call-site-expression y) (cons p (call-site-offsets y))))

;;; Call-site properties

(define (top-level-call-site? y) (eq? y *y*))

(define (explicit-call-site? y)
 (and (not (top-level-call-site? y)) (null? (call-site-offsets y))))

(define (first-argument-call-site? y)
 (and (not (top-level-call-site? y))
      (not (explicit-call-site? y))
      (eq? (first (call-site-offsets y)) 'first-argument)))

(define (second-argument-call-site? y)
 (and (not (top-level-call-site? y))
      (not (explicit-call-site? y))
      (eq? (first (call-site-offsets y)) 'second-argument)))

(define (continuation-argument-call-site? y)
 (and (not (top-level-call-site? y))
      (not (explicit-call-site? y))
      (eq? (first (call-site-offsets y)) 'continuation-argument)))

(define (purely-tail-call-site? y)
 ;; Common wisdom is that the notion of tail call is syntactic, i.e. a call in
 ;; tail position. But this contradicts the common wisdom that calls to
 ;; continuations are tail calls. Because a call to a continuation might be
 ;; from a non-tail position. And such a call site might be multimorphic so
 ;; might be both a tail-call site and a non-tail-call site. So much for common
 ;; wisdom.
 ;; APPLY and CALL-WITH-CURRENT-CONTINUATION tail call their first argument if
 ;; they themselves are tail calls. And implicit continuation calls are always
 ;; tail calls.
 ;; needs work: Calls to the first and second arguments of FORK and the first
 ;;             argument of MUTEX are not tail calls.
 (or
  (top-level-call-site? y)
  (continuation-argument-call-site? y)
  (let ((x (call-site-expression y)))
   (and
    (or (eq? (expression-kind x) 'call)
	(eq? (expression-kind x) 'converted-call))
    ;; needs work: A call in the source of a SET! to a non-accessed,
    ;;             fictitious, or hidden variable can be a pure tail call if
    ;;             the SET! is in tail position.
    (or (must-be? (lambda (u)
		   (or (continuation-type? u)
		       (not (executed? x))
		       (not ((truly-compatible-call? x) u))))
		  (expression-type-set (expression-callee x)))
	(let loop? ((x x))
	 (or (and (eq? (expression-kind (expression-parent x)) 'if)
		  (or (eq? x (expression-consequent (expression-parent x)))
		      (eq? x (expression-alternate (expression-parent x))))
		  (loop? (expression-parent x)))
	     (or (eq? (expression-kind (expression-parent x)) 'lambda)
		 (eq? (expression-kind (expression-parent x))
		      'converted-lambda)
		 (eq? (expression-kind (expression-parent x))
		      'converted-continuation)))))))))

(define (nonmerged-tail-recursive-purely-tail-call-site? y)
 (and
  (purely-tail-call-site? y)
  (some
   (lambda (e)
    (and (environment-marked1? e)
	 (can-be-call-to? y e)
	 (or (not (unique-call-site? e))
	     (not (same-call-site? y (unique-call-site e))))
	 (not (can-be-self-tail-call-to? y e))))
   (proper-tail-callers (expression-environment (call-site-expression y))))))

;;; Call-site functions

(define (nonmerged-tail-recursive-purely-tail-call-site-callees y)
 (remove-if-not
  (lambda (e)
   (and (can-be-call-to? y e)
	(or (not (unique-call-site? e))
	    (not (same-call-site? y (unique-call-site e))))
	(not (can-be-self-tail-call-to? y e))))
  (proper-tail-callers (expression-environment (call-site-expression y)))))

(define (call-site-callee y)
 (when (> (length (call-site-offsets y)) 1) (unimplemented y "unimplemented"))
 (cond
  ((explicit-call-site? y) (expression-callee (call-site-expression y)))
  ((first-argument-call-site? y) (first-argument (call-site-expression y)))
  ((second-argument-call-site? y) (second-argument (call-site-expression y)))
  ((continuation-argument-call-site? y)
   (continuation-argument (call-site-expression y)))
  (else (fuck-up))))

;;; Call-site call-site relations

(define (same-call-site? y1 y2)
 (or (and (top-level-call-site? y1) (top-level-call-site? y2))
     (and (not (top-level-call-site? y1))
	  (not (top-level-call-site? y2))
	  (eq? (call-site-expression y1) (call-site-expression y2))
	  (equal? (call-site-offsets y1) (call-site-offsets y2)))))

;;; Call-site environment relations

(define (can-be-call-to? y e)
 (not (not (memp same-call-site? y (call-sites e)))))

(define (can-be-self-tail-call-to? y e)
 ;; needs work: This is not memoized but should be.
 ;; note: Self tail calls need not be just to the immediately enclosing
 ;;       procedure but to any procedure that that is in-lined in.
 (and (not (noop? e))
      (can-be-call-to? y e)
      (tail-call? y e)
      ;; This assumes that the IN-LINED-IN? relation is reflexive.
      (in-lined-in? (call-site-expression y) e)))

;;; Call-site type relations

(define (goto? y u)
 (and (in-lined-in?
       (expression-environment (call-site-expression y))
       (expression-environment (continuation-type-allocating-expression u)))
      (must-alias? u)))

;;; Generic properties

(define (fictitious? u/w)
 (cond ((null-type? u/w) #t)
       ((true-type? u/w) #t)
       ((false-type? u/w) #t)
       ((char-type? u/w) #f)
       ((fixnum-type? u/w) #f)
       ((flonum-type? u/w) #f)
       ((rectangular-type? u/w) #f)
       ((input-port-type? u/w) #f)
       ((output-port-type? u/w) #f)
       ((eof-object-type? u/w) #t)
       ((pointer-type? u/w) #f)
       ((internal-symbol-type? u/w) #t)
       ((external-symbol-type? u/w) #f)
       ((primitive-procedure-type? u/w) #t)
       ((native-procedure-type? u/w) (native-procedure-type-fictitious? u/w))
       ((foreign-procedure-type? u/w) #t)
       ((continuation-type? u/w) (continuation-type-fictitious? u/w))
       ((string-type? u/w) #f)
       ((structure-type? u/w) (structure-type-fictitious? u/w))
       ((headed-vector-type? u/w) #f)
       ((nonheaded-vector-type? u/w) #f)
       ((displaced-vector-type? u/w) #f)
       ((type-set? u/w) (type-set-fictitious? u/w))
       (else (fuck-up))))

(define (determine-whether-native-procedure-type-fictitious? u)
 (case *closure-conversion-method*
  ((baseline conventional) #f)
  ((lightweight)
   (or (not (environment-accessed? u))
       (every (lambda (e)
	       (or (not (environment-used? e))
		   (let loop? ((e1 (quick-parent e)))
		    (or (empty? e1)
			(and (not (ancestor? e1 e))
			     (loop? (quick-parent e1)))))))
	      (narrow-clones u))))
  (else (fuck-up))))

(define (determine-whether-continuation-type-fictitious? u)
 (case *closure-conversion-method*
  ((baseline conventional) #f)
  ((lightweight)
   (or (not (continuation-type-continuation-accessed? u))
       (and (every (lambda (y)
		    (in-lined-in?
		     (expression-environment (call-site-expression y))
		     (expression-environment
		      (continuation-type-allocating-expression u))))
		   (continuation-type-call-sites u))
	    (must-alias? u))))
  (else (fuck-up))))

(define (determine-whether-structure-type-fictitious? u)
 (case *closure-conversion-method*
  ((baseline conventional) #f)
  ((lightweight) (every fictitious? (structure-type-slots u)))
  (else (fuck-up))))

(define (infer-all-whether-type-fictitious?! p?)
 (when *p7?* (notify "Determining whether types are fictitious"))
 (for-each
  (lambda (u)
   (cond ((fictitious? u)
	  (unless (determine-whether-native-procedure-type-fictitious? u)
	   (set-native-procedure-type-fictitious?! u #f)
	   (when *p7?* (notify "  U~s is not fictitious" (type-index u)))
	   (set! *again?* #t)))
	 ((and p? (determine-whether-native-procedure-type-fictitious? u))
	  (fuck-up))))
  *native-procedure-types*)
 (for-each
  (lambda (u)
   (cond
    ((fictitious? u)
     (unless (determine-whether-continuation-type-fictitious? u)
      (set-continuation-type-fictitious?! u #f)
      (when *p7?* (notify "  U~s is not fictitious" (type-index u)))
      (set! *again?* #t)))
    ((and p? (determine-whether-continuation-type-fictitious? u)) (fuck-up))))
  *continuation-types*)
 (for-each
  (lambda (u)
   (cond
    ((fictitious? u)
     (unless (determine-whether-structure-type-fictitious? u)
      (set-structure-type-fictitious?! u #f)
      (when *p7?* (notify "  U~s is not fictitious" (type-index u)))
      (set! *again?* #t)))
    ((and p? (determine-whether-structure-type-fictitious? u)) (fuck-up))))
  *structure-types*))

(define (determine-whether-type-set-fictitious? w)
 ;; needs work: This really won't work until APPLY-CLOSED-WORLD-ASSUMPTION! is
 ;;             called since all of the multiple members might turn out to be
 ;;             the same. But don't worry, this errs on the conservative side.
 (case *closure-conversion-method*
  ((baseline conventional) #f)
  ((lightweight)
   (or (void? w) (and (not (multimorphic? w)) (must-be? fictitious? w))))
  (else (fuck-up))))

(define (infer-all-whether-type-set-fictitious?! p?)
 (when *p7?* (notify "Determining whether type sets are fictitious"))
 (for-each
  (lambda (w)
   (cond ((fictitious? w)
	  (unless (determine-whether-type-set-fictitious? w)
	   (set-type-set-fictitious?! w #f)
	   (when *p7?* (notify "  W~s is not fictitious" (type-set-index w)))
	   (set! *again?* #t)))
	 ((and p? (determine-whether-type-set-fictitious? w)) (fuck-up))))
  *ws*))

(define (has-parent-parameter? u/e)
 ;; Different narrow clones can have different ancestor sets. Narrow clones
 ;; can differ in whether they need a parent parameter. This was discovered
 ;; with the matrix.sc example of jbs@quiotix.com. This created problems when
 ;; applying PARENT-PARAMETER to a type instead of an environment and also
 ;; caused generation of incorrect code where one backchain was accessed as
 ;; the backchain of a narrow clone. Now we force all narrow clones to have
 ;; a parent parameter if one of them does. This might cause some procedures to
 ;; have a parent parameter that isn't used (i.e. reducing the amount of
 ;; parent-parameter elimination). So it goes.
 (some (lambda (e) (and (environment-used? e) (not (null? (ancestors e)))))
       (narrow-clones u/e)))

(define (called? u/e)
 (cond ((native-procedure-type? u/e)
	(and (native-procedure-type-narrow-prototype u/e)
	     (some called? (narrow-clones u/e))))
       ((environment? u/e) (not (null? (call-sites u/e))))
       (else (fuck-up))))

(define (let? u/e/x)
 (cond
  ((native-procedure-type? u/e/x)
   ;; needs work: This could use the wide notion of clone but that would be
   ;;             just for error checking.
   (when (and (some let? (narrow-clones u/e/x))
	      (not (every let? (narrow-clones u/e/x))))
    (fuck-up))
   (let? (narrow-prototype u/e/x)))
  ((environment? u/e/x)
   ;; needs work: To say that a lambda expression that is the second argument
   ;;             to a procedure that calls its first or second argument is a
   ;;             let.
   (and (expression? (expression-parent (environment-expression u/e/x)))
	(or (eq? (expression-kind
		  (expression-parent (environment-expression u/e/x)))
		 'call)
	    (eq? (expression-kind
		  (expression-parent (environment-expression u/e/x)))
		 'converted-call))
	(eq? (expression-callee
	      (expression-parent (environment-expression u/e/x)))
	     (environment-expression u/e/x))))
  ((expression? u/e/x)
   (unless (or (eq? (expression-kind u/e/x) 'call)
	       (eq? (expression-kind u/e/x) 'converted-call))
    (fuck-up))
   (or
    (eq? (expression-kind (expression-callee u/e/x)) 'lambda)
    (eq? (expression-kind (expression-callee u/e/x)) 'converted-lambda)
    (eq? (expression-kind (expression-callee u/e/x)) 'converted-continuation)))
  (else (fuck-up))))

(define (noop? u/e/x)
 (cond ((native-procedure-type? u/e/x)
	;; needs work: This could use the wide notion of clone but that would
	;;             be just for error checking.
	(when (and (some noop? (narrow-clones u/e/x))
		   (not (every noop? (narrow-clones u/e/x))))
	 (fuck-up))
	(noop? (narrow-prototype u/e/x)))
       ((environment? u/e/x) (noop? (environment-expression u/e/x)))
       ((expression? u/e/x)
	(unless (or (eq? (expression-kind u/e/x) 'lambda)
		    (eq? (expression-kind u/e/x) 'converted-lambda)
		    (eq? (expression-kind u/e/x) 'converted-continuation))
	 (fuck-up))
	(not (expression? (expression-body u/e/x))))
       (else (fuck-up))))

(define (rest? u/e/x)
 (cond ((native-procedure-type? u/e/x)
	;; needs work: This could use the wide notion of clone but that would
	;;             be just for error checking.
	(when (and (some rest? (narrow-clones u/e/x))
		   (not (every rest? (narrow-clones u/e/x))))
	 (fuck-up))
	(rest? (narrow-prototype u/e/x)))
       ((environment? u/e/x) (rest? (environment-expression u/e/x)))
       ((expression? u/e/x)
	(unless (or (eq? (expression-kind u/e/x) 'lambda)
		    (eq? (expression-kind u/e/x) 'converted-lambda)
		    (eq? (expression-kind u/e/x) 'converted-continuation))
	 (fuck-up))
	(not (list? (expression-parameters u/e/x))))
       (else (fuck-up))))

(define (converted? e/x/y)
 ;; needs work: #F is ambiguous between the top-level call site and the empty
 ;;             environment.
 (cond ((environment? e/x/y) (converted? (environment-expression e/x/y)))
       ((expression? e/x/y)
	(or (eq? (expression-kind e/x/y) 'converted-call)
	    (eq? (expression-kind e/x/y) 'converted-lambda)))
       ((call-site? e/x/y)
	(and (not (continuation-argument-call-site? e/x/y))
	     ;; First argument call sites are converted if and only if their
	     ;; expression is converted.
	     (converted? (call-site-expression e/x/y))))
       ((top-level-call-site? e/x/y) (converted? *x*))
       (else (fuck-up))))

;;; Generic functions

(define (ancestors u/e)
 ;; note: This must use the narrow notion of clone because different wide
 ;;       clones can have different ancestor sets.
 (cond ((native-procedure-type? u/e)
	(unless (pairwise? (lambda (e1 e2)
			    (or (not (environment-used? e1))
				(not (environment-used? e2))
				(set-equalq? (ancestors e1) (ancestors e2))))
			   (narrow-clones u/e))
	 (fuck-up))
	(ancestors (narrow-prototype u/e)))
       ((environment? u/e)
	(when (or (empty? u/e) (not (environment-used? u/e))) (fuck-up))
	(environment-ancestors u/e))
       (else (fuck-up))))

(define (parent-parameter u/e)
 ;; It used to be possible for two different narrow clones to have different
 ;; parent parameters. This was discovered with the matrix.sc example of
 ;; jbs@quiotix.com. This created problems when applying PARENT-PARAMETER to
 ;; a type instead of an environment and also caused generation of incorrect
 ;; code where one backchain was accessed as the backchain of a narrow clone.
 ;; Now we take the most-nested parent parameter of all the narrow clones.
 ;; This might cause some procedures to have a parent parameter that is used
 ;; only to indirect through a parent slot and not to access other slots (i.e.
 ;; reducing the amount of parent-parameter compression). So it goes.
 (environment-parent-parameter (narrow-prototype u/e)))

(define (narrow-prototype u/e)
 (cond ((native-procedure-type? u/e)
	(unless (eq? (native-procedure-type-narrow-prototype u/e)
		     (narrow-prototype
		      (native-procedure-type-narrow-prototype u/e)))
	 (fuck-up))
	(native-procedure-type-narrow-prototype u/e))
       ((environment? u/e)
	(unless (eq? (environment-narrow-prototype u/e)
		     (environment-narrow-prototype
		      (environment-narrow-prototype u/e)))
	 (fuck-up))
	(environment-narrow-prototype u/e))
       (else (fuck-up))))

(define (wide-prototype u/e)
 ;; This is only used by WIDE-CLONES?, WIDE-CLONES, CLONE-EXPRESSION,
 ;; PRINT-MAXIMAL-CLONE-RATE, and STALIN.
 (cond ((native-procedure-type? u/e)
	(unless (pairwise? (lambda (e1 e2)
			    (eq? (wide-prototype e1) (wide-prototype e2)))
			   (narrow-clones u/e))
	 (fuck-up))
	(wide-prototype (narrow-prototype u/e)))
       ((environment? u/e) (environment-wide-prototype u/e))
       (else (fuck-up))))

(define (narrow-clones u/e)
 (if (and (native-procedure-type? u/e)
	  (not (native-procedure-type-narrow-prototype u/e)))
     '()
     (environment-narrow-clones (narrow-prototype u/e))))

(define (wide-clones u/e)
 ;; This is only used by PRINT-CLONE-RATES.
 (remove-if-not
  (lambda (e) (eq? (wide-prototype u/e) (wide-prototype e))) *es*))

(define (variables e/x)
 ;; This only returns the variables of E/X itself, not things in-lined in E/X.
 (cond ((environment? e/x) (variables (environment-expression e/x)))
       ((expression? e/x)
	(let loop ((gs (expression-parameters e/x)))
	 (cond ((null? gs) '())
	       ((pair? gs) (cons (first gs) (loop (rest gs))))
	       ((variable? gs) (list gs))
	       (else (fuck-up)))))
       (else (fuck-up))))

;;; Generic relations

(define (in-lined-in? e/x1 e/x2)
 ;; The IN-LINED-IN? relation is reflexive.
 (cond
  ((environment? e/x1)
   (and
    ;; This is just because of *CLOSURE-CONVERSION-METHOD*.
    (environment-used? e/x1)
    (cond
     ((environment? e/x2)
      (or (eq? e/x1 e/x2)
	  (and (unique-call-site? e/x1)
	       (in-lined-in? (call-site-expression (unique-call-site e/x1))
			     e/x2))))
     ((expression? e/x2)
      (and (unique-call-site? e/x1)
	   (in-lined-in? (call-site-expression (unique-call-site e/x1)) e/x2)))
     (else (fuck-up)))))
  ((expression? e/x1)
   (cond ((environment? e/x2)
	  (and (not (empty? (expression-environment e/x1)))
	       (in-lined-in? (expression-environment e/x1) e/x2)))
	 ((expression? e/x2)
	  (or (eq? e/x1 e/x2)
	      (if (or (eq? (expression-kind e/x1) 'lambda)
		      (eq? (expression-kind e/x1) 'converted-lambda)
		      (eq? (expression-kind e/x1) 'converted-continuation))
		  (in-lined-in? (expression-lambda-environment e/x1) e/x2)
		  (and (not (empty? (expression-parent e/x1)))
		       (in-lined-in? (expression-parent e/x1) e/x2)))))
	 (else (fuck-up))))
  (else (fuck-up))))

(define (properly-in-lined-in? e/x1 e/x2)
 ;; The PROPERLY-IN-LINED-IN? relation is irreflexive.
 (and (not (eq? e/x1 e/x2)) (in-lined-in? e/x1 e/x2)))

(define (unmark-types!)
 (for-each (lambda (u) (set-internal-symbol-type-marked?! u #f))
	   *internal-symbol-types*)
 (for-each (lambda (u) (set-external-symbol-type-marked?! u #f))
	   *external-symbol-types*)
 (for-each (lambda (u) (set-primitive-procedure-type-marked?! u #f))
	   *primitive-procedure-types*)
 (for-each (lambda (u) (set-native-procedure-type-marked?! u #f))
	   *native-procedure-types*)
 (for-each (lambda (u) (set-foreign-procedure-type-marked?! u #f))
	   *foreign-procedure-types*)
 (for-each (lambda (u) (set-continuation-type-marked?! u #f))
	   *continuation-types*)
 (for-each (lambda (u) (set-string-type-marked?! u #f))
	   *string-types*)
 (for-each (lambda (u) (set-structure-type-marked?! u #f))
	   *structure-types*)
 (for-each (lambda (u) (set-headed-vector-type-marked?! u #f))
	   *headed-vector-types*)
 (for-each (lambda (u) (set-nonheaded-vector-type-marked?! u #f))
	   *nonheaded-vector-types*)
 (for-each (lambda (u) (set-displaced-vector-type-marked?! u #f))
	   *displaced-vector-types*))

(define (unmark-types-and-type-sets!)
 (unmark-types!)
 (for-each (lambda (w) (set-type-set-marked?! w #f)) *ws*))

(define (some-pointed-to-type p? u/w1)
 ;; conventions: P?
 (let loop? ((u/w1 u/w1))
  (cond
   ((type? u/w1)
    (cond
     ;; Internal symbols do not point to anything.
     ((internal-symbol-type? u/w1)
      (and (not (internal-symbol-type-marked? u/w1))
	   (begin (set-internal-symbol-type-marked?! u/w1 #t) (p? u/w1))))
     ;; External symbols point to their displaced string.
     ((external-symbol-type? u/w1)
      (and (not (external-symbol-type-marked? u/w1))
	   (external-symbol-type-symbol->string-accessed? u/w1)
	   (begin
	    (set-external-symbol-type-marked?! u/w1 #t)
	    (or (p? u/w1)
		(loop? (external-symbol-type-displaced-string-type u/w1))))))
     ;; Primitive procedures do not point to anything.
     ((primitive-procedure-type? u/w1)
      (and (not (primitive-procedure-type-marked? u/w1))
	   (begin (set-primitive-procedure-type-marked?! u/w1 #t)
		  (p? u/w1))))
     ;; A native procedure points to all of the accessed variables in the
     ;; environments of all of its proper ancestors. It does *not* point to
     ;; its parent native procedure.
     ((native-procedure-type? u/w1)
      (and (not (native-procedure-type-marked? u/w1))
	   (begin (set-native-procedure-type-marked?! u/w1 #t)
		  (or (p? u/w1)
		      (some (lambda (e)
			     (and (environment-accessed? e)
				  (some (lambda (g)
					 (and (accessed? g)
					      (not (necessarily-fictitious?
						    (variable-type-set g)))
					      (loop? (variable-type-set g))))
					(free-variables e))))
			    (narrow-clones u/w1))))))
     ;; Foreign procedures do not point to anything.
     ((foreign-procedure-type? u/w1)
      (and (not (foreign-procedure-type-marked? u/w1))
	   (begin (set-foreign-procedure-type-marked?! u/w1 #t) (p? u/w1))))
     ;; Continuations do not point to anything.
     ((continuation-type? u/w1)
      (and (not (continuation-type-marked? u/w1))
	   (begin (set-continuation-type-marked?! u/w1 #t) (p? u/w1))))
     ;; Strings do not point to anything.
     ((string-type? u/w1)
      (and (not (string-type-marked? u/w1))
	   (begin (set-string-type-marked?! u/w1 #t) (p? u/w1))))
     ;; Structures point to their slots.
     ((structure-type? u/w1)
      (and (not (structure-type-marked? u/w1))
	   (begin (set-structure-type-marked?! u/w1 #t)
		  (or (p? u/w1)
		      (some (lambda (p? w) (and p? (loop? w)))
			    (structure-type-structure-ref-accessed? u/w1)
			    (structure-type-slots u/w1))))))
     ;; Headed vectors point to their element.
     ((headed-vector-type? u/w1)
      (and (not (headed-vector-type-marked? u/w1))
	   (begin (set-headed-vector-type-marked?! u/w1 #t)
		  (or (p? u/w1)
		      (and (headed-vector-type-vector-ref-accessed? u/w1)
			   (loop? (headed-vector-type-element u/w1)))))))
     ;; Nonheaded vectors point to their element.
     ((nonheaded-vector-type? u/w1)
      (and (not (nonheaded-vector-type-marked? u/w1))
	   (begin (set-nonheaded-vector-type-marked?! u/w1 #t)
		  (or (p? u/w1)
		      (and (nonheaded-vector-type-vector-ref-accessed? u/w1)
			   (loop? (nonheaded-vector-type-element u/w1)))))))
     ;; Displaced vectors point to their displaced vector.
     ((displaced-vector-type? u/w1)
      (and (not (displaced-vector-type-marked? u/w1))
	   (begin
	    (set-displaced-vector-type-marked?! u/w1 #t)
	    (or (p? u/w1)
		(and (displaced-vector-type-vector-ref-accessed? u/w1)
		     (loop?
		      (displaced-vector-type-displaced-vector-type u/w1)))))))
     (else (p? u/w1))))
   ((type-set? u/w1) (can-be? loop? u/w1))
   (else (fuck-up)))))

(define (for-each-pointed-to-type p u/w1)
 ;; conventions: P
 (let loop ((u/w1 u/w1))
  (cond ((type? u/w1)
	 (cond
	  ;; Internal symbols do not point to anything.
	  ((internal-symbol-type? u/w1)
	   (unless (internal-symbol-type-marked? u/w1)
	    (set-internal-symbol-type-marked?! u/w1 #t)
	    (p u/w1)))
	  ;; External symbols point to their displaced string.
	  ((external-symbol-type? u/w1)
	   (unless (external-symbol-type-marked? u/w1)
	    (set-external-symbol-type-marked?! u/w1 #t)
	    (p u/w1)
	    (loop (external-symbol-type-displaced-string-type u/w1))))
	  ;; Primitive procedures do not point to anything.
	  ((primitive-procedure-type? u/w1)
	   (unless (primitive-procedure-type-marked? u/w1)
	    (set-primitive-procedure-type-marked?! u/w1 #t)
	    (p u/w1)))
	  ;; A native procedure points to all of the accessed variables in the
	  ;; environments of all of its proper ancestors. It does *not* point
	  ;; to its parent native procedure.
	  ((native-procedure-type? u/w1)
	   (unless (native-procedure-type-marked? u/w1)
	    (set-native-procedure-type-marked?! u/w1 #t)
	    (p u/w1)
	    (for-each (lambda (e)
		       (when (environment-accessed? e)
			(for-each (lambda (g)
				   (when (and (accessed? g)
					      (not (necessarily-fictitious?
						    (variable-type-set g))))
				    (loop (variable-type-set g))))
				  (free-variables e))))
		      (narrow-clones u/w1))))
	  ;; Foreign procedures do not point to anything.
	  ((foreign-procedure-type? u/w1)
	   (unless (foreign-procedure-type-marked? u/w1)
	    (set-foreign-procedure-type-marked?! u/w1 #t)
	    (p u/w1)))
	  ;; Continuations do not point to anything.
	  ((continuation-type? u/w1)
	   (unless (continuation-type-marked? u/w1)
	    (set-continuation-type-marked?! u/w1 #t)
	    (p u/w1)))
	  ;; Strings do not point to anything.
	  ((string-type? u/w1)
	   (unless (string-type-marked? u/w1)
	    (set-string-type-marked?! u/w1 #t)
	    (p u/w1)))
	  ;; Structures point to their slots.
	  ((structure-type? u/w1)
	   (unless (structure-type-marked? u/w1)
	    (set-structure-type-marked?! u/w1 #t)
	    (p u/w1)
	    (for-each loop (structure-type-slots u/w1))))
	  ;; Headed vectors point to their element.
	  ((headed-vector-type? u/w1)
	   (unless (headed-vector-type-marked? u/w1)
	    (set-headed-vector-type-marked?! u/w1 #t)
	    (p u/w1)
	    (loop (headed-vector-type-element u/w1))))
	  ;; Nonheaded vectors point to their element.
	  ((nonheaded-vector-type? u/w1)
	   (unless (nonheaded-vector-type-marked? u/w1)
	    (set-nonheaded-vector-type-marked?! u/w1 #t)
	    (p u/w1)
	    (loop (nonheaded-vector-type-element u/w1))))
	  ;; Displaced vectors point to their displaced vector.
	  ((displaced-vector-type? u/w1)
	   (unless (displaced-vector-type-marked? u/w1)
	    (set-displaced-vector-type-marked?! u/w1 #t)
	    (p u/w1)
	    (loop (displaced-vector-type-displaced-vector-type u/w1))))
	  (else (p u/w1))))
	((type-set? u/w1) (for-each-member loop u/w1))
	(else (fuck-up)))))

(define (points-to? u/w1 u2)
 ;; The POINTS-TO? relation is reflexive.
 (unmark-types!)
 (some-pointed-to-type (lambda (u1) (eq? u1 u2)) u/w1))

;;; Polyvariance

(define (clone-size e)
 (let loop ((x (environment-expression e)) (es '()))
  (case (expression-kind x)
   ((null-constant) 1)
   ((true-constant) 1)
   ((false-constant) 1)
   ((char-constant) 1)
   ((fixnum-constant) 1)
   ((flonum-constant) 1)
   ((rectangular-constant) 1)
   ((string-constant) 1)
   ((symbol-constant) 1)
   ((pair-constant)
    (+ 1
       (loop (car (expression-constant x)) es)
       (loop (cdr (expression-constant x)) es)))
   ((vector-constant)
    (+ 1
       (reduce-vector
	+ (map-vector (lambda (x) (loop x es)) (expression-constant x)) 0)))
   ((lambda converted-lambda converted-continuation)
    (+ 1
       (if (noop? x)
	   0
	   (loop (expression-body x)
		 (cons (expression-lambda-environment x) es)))))
   ((set!) (+ 1 (loop (expression-source x) es)))
   ((if)
    (+ 1
       (loop (expression-antecedent x) es)
       (loop (expression-consequent x) es)
       (loop (expression-alternate x) es)))
   ((primitive-procedure) 1)
   ((foreign-procedure) 1)
   ((access) 1)
   ((call converted-call)
    (+ 1
       (loop (expression-callee x) es)
       (reduce + (map (lambda (x) (loop x es)) (expression-arguments x)) 0)))
   (else (fuck-up)))))

(define (clone-expression x)
 (let loop ((x x) (gss '()))
  (case (expression-kind x)
   ((null-constant)
    (let ((x (create-expression 'null-constant x #f)))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((true-constant)
    (let ((x (create-expression 'true-constant x #f)))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((false-constant)
    (let ((x (create-expression 'false-constant x #f)))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((char-constant)
    (let ((x (create-expression 'char-constant x (expression-constant x))))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((fixnum-constant)
    (let ((x (create-expression 'fixnum-constant x (expression-constant x))))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((flonum-constant)
    (let ((x (create-expression 'flonum-constant x (expression-constant x))))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((rectangular-constant)
    (let ((x (create-expression
	      'rectangular-constant x (expression-constant x))))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((string-constant)
    (let ((x (create-expression 'string-constant x (expression-constant x))))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((symbol-constant)
    (let ((x (create-expression 'symbol-constant x (expression-constant x))))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((pair-constant)
    (let ((x (create-expression
	      'pair-constant x
	      (cons (loop (car (expression-constant x)) gss)
		    (loop (cdr (expression-constant x)) gss)))))
     (set-expression-type-set! x (create-type-set x))
     (set-expression-parent! (car (expression-constant x)) x)
     (set-expression-parent! (cdr (expression-constant x)) x)
     x))
   ((vector-constant)
    (let ((x (create-expression 'vector-constant x
				(map-vector (lambda (x) (loop x gss))
					    (expression-constant x)))))
     (set-expression-type-set! x (create-type-set x))
     (for-each-vector (lambda (x1) (set-expression-parent! x1 x))
		      (expression-constant x))
     x))
   ((lambda)
    (let ((e1 (create-environment
	       (environment-name
		(narrow-prototype (expression-lambda-environment x)))
	       #f)))
     (set-environment-split! e1 #t)
     (when #f				;debugging
      (unless (null? gss)
       (notify "Deep cloning: ~a -> ~a"
	       (environment-name (expression-lambda-environment x))
	       (environment-name e1))))
     (when (null? gss)
      (set-environment-narrow-prototype!
       e1 (narrow-prototype (expression-lambda-environment x)))
      (set-environment-narrow-clones! e1 '())
      (set-environment-narrow-clones!
       (narrow-prototype (expression-lambda-environment x))
       (cons e1 (narrow-clones (expression-lambda-environment x)))))
     (set-environment-wide-prototype!
      e1 (wide-prototype (expression-lambda-environment x)))
     (let* ((gs (let loop ((gs (expression-parameters x)))
		 (cond ((null? gs) '())
		       ((pair? gs)
			(let ((g (create-variable (first gs))))
			 (set-variable-type-set! g (create-type-set g))
			 (cons g (loop (rest gs)))))
		       ((variable? gs)
			(let ((g (create-variable gs)))
			 (set-variable-type-set! g (create-type-set g))
			 g))
		       (else (fuck-up)))))
	    (x1 (create-lambda-expression
		 x e1 gs
		 (if (noop? x)
		     #f
		     (loop (expression-body x) (cons gs gss))))))
      ;; The lambda expression for narrow clones must be initialized to used
      ;; if the narrow prototype is used. The lambda expression for wide
      ;; clones will always be a narrow prototype and will be initialized to
      ;; used if it actually is reached.
      (when (null? gss) (set-expression-reached?! x1 (expression-reached? x)))
      (set-expression-type-set!
       x1
       (if (null? gss)
	   ;; The lambda expressions for narrow clones share the same type set.
	   (expression-type-set x)
	   ;; The lambda expressions for wide clones each have their own type
	   ;; set.
	   (create-type-set x1)))
      (unless (noop? x1) (set-expression-parent! (expression-body x1) x1))
      (annotate-environment-variables-with-their-environment! e1)
      (annotate-environment-expressions-with-their-environment! e1)
      x1)))
   ((converted-lambda)
    (let ((e1 (create-environment
	       (environment-name
		(narrow-prototype (expression-lambda-environment x)))
	       #f)))
     (set-environment-split! e1 #t)
     (when #f				;debugging
      (unless (null? gss)
       (notify "Deep cloning: ~a -> ~a"
	       (environment-name (expression-lambda-environment x))
	       (environment-name e1))))
     (when (null? gss)
      (set-environment-narrow-prototype!
       e1 (narrow-prototype (expression-lambda-environment x)))
      (set-environment-narrow-clones! e1 '())
      (set-environment-narrow-clones!
       (narrow-prototype (expression-lambda-environment x))
       (cons e1 (narrow-clones (expression-lambda-environment x)))))
     (set-environment-wide-prototype!
      e1 (wide-prototype (expression-lambda-environment x)))
     (let* ((gs (let loop ((gs (expression-parameters x)))
		 (cond ((null? gs) '())
		       ((pair? gs)
			(let ((g (create-variable (first gs))))
			 (set-variable-type-set! g (create-type-set g))
			 (cons g (loop (rest gs)))))
		       ((variable? gs)
			(let ((g (create-variable gs)))
			 (set-variable-type-set! g (create-type-set g))
			 g))
		       (else (fuck-up)))))
	    (x1 (create-converted-lambda-expression
		 x e1 gs
		 (if (noop? x)
		     #f
		     (loop (expression-body x) (cons gs gss))))))
      ;; The lambda expression for narrow clones must be initialized to used
      ;; if the narrow prototype is used. The lambda expression for wide
      ;; clones will always be a narrow prototype and will be initialized to
      ;; used if it actually is reached.
      (when (null? gss) (set-expression-reached?! x1 (expression-reached? x)))
      (set-expression-type-set!
       x1
       (if (null? gss)	
	   ;; The lambda expressions for narrow clones share the same type set.
	   (expression-type-set x)
	   ;; The lambda expressions for wide clones each have their own type
	   ;; set.
	   (create-type-set x1)))
      (unless (noop? x1) (set-expression-parent! (expression-body x1) x1))
      (annotate-environment-variables-with-their-environment! e1)
      (annotate-environment-expressions-with-their-environment! e1)
      x1)))
   ((converted-continuation)
    (let ((e1 (create-environment
	       (environment-name
		(narrow-prototype (expression-lambda-environment x)))
	       #f)))
     (set-environment-split! e1 #t)
     (when #f				;debugging
      (unless (null? gss)
       (notify "Deep cloning: ~a -> ~a"
	       (environment-name (expression-lambda-environment x))
	       (environment-name e1))))
     (when (null? gss)
      (set-environment-narrow-prototype!
       e1 (narrow-prototype (expression-lambda-environment x)))
      (set-environment-narrow-clones! e1 '())
      (set-environment-narrow-clones!
       (narrow-prototype (expression-lambda-environment x))
       (cons e1 (narrow-clones (expression-lambda-environment x)))))
     (set-environment-wide-prototype!
      e1 (wide-prototype (expression-lambda-environment x)))
     (let* ((gs (let loop ((gs (expression-parameters x)))
		 (cond ((null? gs) '())
		       ((pair? gs)
			(let ((g (create-variable (first gs))))
			 (set-variable-type-set! g (create-type-set g))
			 (cons g (loop (rest gs)))))
		       ((variable? gs)
			(let ((g (create-variable gs)))
			 (set-variable-type-set! g (create-type-set g))
			 g))
		       (else (fuck-up)))))
	    (x1 (create-converted-continuation-expression
		 x e1 gs
		 (if (noop? x)
		     #f
		     (loop (expression-body x) (cons gs gss))))))
      ;; The lambda expression for narrow clones must be initialized to used
      ;; if the narrow prototype is used. The lambda expression for wide
      ;; clones will always be a narrow prototype and will be initialized to
      ;; used if it actually is reached.
      (when (null? gss) (set-expression-reached?! x1 (expression-reached? x)))
      (set-expression-type-set!
       x1
       (if (null? gss)	
	   ;; The lambda expressions for narrow clones share the same type set.
	   (expression-type-set x)
	   ;; The lambda expressions for wide clones each have their own type
	   ;; set.
	   (create-type-set x1)))
      (unless (noop? x1) (set-expression-parent! (expression-body x1) x1))
      (annotate-environment-variables-with-their-environment! e1)
      (annotate-environment-expressions-with-their-environment! e1)
      x1)))
   ((set!)
    (let ((x (create-set!-expression
	      x
	      (let outer ((gss1 gss) (e (expression-environment x)))
	       (if (null? gss1)
		   (expression-variable x)
		   (let inner ((gs1 (first gss1))
			       (gs2 (expression-parameters
				     (environment-expression e))))
		    (cond ((null? gs1)
			   (unless (null? gs2) (fuck-up))
			   (outer (rest gss1) (parent e)))
			  ((null? gs2) (fuck-up))
			  ((and (pair? gs1) (pair? gs2))
			   (if (eq? (first gs2) (expression-variable x))
			       (first gs1)
			       (inner (rest gs1) (rest gs2))))
			  ((and (variable? gs1) (variable? gs2))
			   (if (eq? gs2 (expression-variable x))
			       gs1
			       (outer (rest gss1) (parent e))))
			  (else (fuck-up))))))
	      (loop (expression-source x) gss))))
     (set-expression-type-set! x (create-type-set x))
     (set-expression-parent! (expression-source x) x)
     (set-variable-assignments!
      (expression-variable x)
      (cons x (variable-assignments (expression-variable x))))
     (set-variable-references!
      (expression-variable x)
      (cons x (variable-references (expression-variable x))))
     x))
   ((if)
    (let ((x (create-if-expression
	      x
	      (loop (expression-antecedent x) gss)
	      (loop (expression-consequent x) gss)
	      (loop (expression-alternate x) gss))))
     (set-expression-type-set! x (create-type-set x))
     (set-expression-parent! (expression-antecedent x) x)
     (set-expression-parent! (expression-consequent x) x)
     (set-expression-parent! (expression-alternate x) x)
     x))
   ((primitive-procedure)
    (let ((x (create-expression
	      'primitive-procedure x (expression-constant x))))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((foreign-procedure)
    (let ((x (create-expression 'foreign-procedure x (expression-constant x))))
     (set-expression-type-set! x (create-type-set x))
     x))
   ((access)
    (let ((x (create-access-expression
	      x
	      (let outer ((gss1 gss) (e (expression-environment x)))
	       (if (null? gss1)
		   (expression-variable x)
		   (let inner ((gs1 (first gss1))
			       (gs2 (expression-parameters
				     (environment-expression e))))
		    (cond ((null? gs1)
			   (unless (null? gs2) (fuck-up))
			   (outer (rest gss1) (parent e)))
			  ((null? gs2) (fuck-up))
			  ((and (pair? gs1) (pair? gs2))
			   (if (eq? (first gs2) (expression-variable x))
			       (first gs1)
			       (inner (rest gs1) (rest gs2))))
			  ((and (variable? gs1) (variable? gs2))
			   (if (eq? gs2 (expression-variable x))
			       gs1
			       (outer (rest gss1) (parent e))))
			  (else (fuck-up)))))))))
     (set-expression-type-set! x (create-type-set x))
     (set-variable-accesses!
      (expression-variable x)
      (cons x (variable-accesses (expression-variable x))))
     (set-variable-references!
      (expression-variable x)
      (cons x (variable-references (expression-variable x))))
     x))
   ((call)
    (let ((x (create-call-expression
	      x (loop (expression-callee x) gss)
	      (map (lambda (x) (loop x gss)) (expression-arguments x)))))
     (set-expression-type-set! x (create-type-set x))
     (set-expression-parent! (expression-callee x) x)
     (for-each (lambda (x1) (set-expression-parent! x1 x))
	       (expression-arguments x))
     x))
   ((converted-call)
    (let ((x (create-converted-call-expression
	      x (loop (expression-callee x) gss)
	      (map (lambda (x) (loop x gss)) (expression-arguments x)))))
     (set-expression-type-set! x (create-type-set x))
     (set-expression-parent! (expression-callee x) x)
     (for-each (lambda (x1) (set-expression-parent! x1 x))
	       (expression-arguments x))
     x))
   (else (fuck-up)))))

(define (clone e)
 (let* ((x (environment-expression (narrow-prototype e)))
	(x1 (clone-expression x)))
  (set-expression-parent! x1 (expression-parent x))
  (expression-lambda-environment x1)))

(define (callee-environment? u y)
 (assp same-call-site?
       y
       (native-procedure-type-call-site-environment-alist u)))

(define (callee-environment u y)
 (unless (callee-environment? u y)
  (when *types-frozen?* (fuck-up))
  (let ((e (if (and (explicit-call-site? y)
		    (expression-original-expression (call-site-expression y)))
	       (let ((y1 (create-call-site (expression-original-expression
					    (call-site-expression y)))))
		(if (callee-environment? u y1)
		    (let ((e (cdr (callee-environment? u y1))))
		     (cond
		      ((nested-in?
			(expression-environment (call-site-expression y1)) e)
		       (let loop ((e1 (expression-environment
				       (call-site-expression y))))
			(if (or (eq? e e1)
				(eq? (environment-expression e)
				     (expression-original-expression
				      (environment-expression e1))))
			    e1
			    (loop (parent e1)))))
		      ((eq? (environment-split e) #t)
		       (when #f		;debugging
			(notify "Chain cloning x~s ~a->[clone ~a ~s]"
				(expression-index (call-site-expression y))
				(environment-name e)
				(environment-name (narrow-prototype e))
				*ei*)
			(notify "~s" (undecorate (call-site-expression y))))
		       (clone e))
		      (else (narrow-prototype u))))
		    (narrow-prototype u)))
	       (narrow-prototype u))))
   (set-native-procedure-type-call-site-environment-alist!
    u
    (cons (cons y e) (native-procedure-type-call-site-environment-alist u)))))
 (cdr (callee-environment? u y)))

;;; `Necessary' procedures

;;; This procedure is necessary (sic) because it is called by
;;; DEFINE-PRIMITIVE-PROCEDURE EQ? and DETERMINE-ESCAPING-TYPES! before
;;; PERFORM-LIGHTWEIGHT-CLOSURE-CONVERSION! determines FICTITIOUS?.

(define (necessarily-fictitious? u)
 (or (null-type? u)
     (true-type? u)
     (false-type? u)
     (eof-object-type? u)
     (internal-symbol-type? u)
     (primitive-procedure-type? u)
     (and (native-procedure-type? u)
	  (if *types-frozen?*
	      (native-procedure-type-necessarily-fictitious? u)
	      (noop? u)))
     (foreign-procedure-type? u)
     ;; note: This used to count a structure type as necessarily fictitious
     ;;       when every slot was necessarily fictitious and types were frozen.
     ;;       But this is unsound because uniqueness is asserted after escaping
     ;;       types are determined and uniqueness can change a type from being
     ;;       fictitious to being nonfictitious. This happens in veto.sc.
     (and (structure-type? u) (null? (structure-type-slots u)))))

(define (type-set-necessarily-fictitious? w)
 (or (void? w)
     (and (monomorphic? w) (necessarily-fictitious? (the-member w)))))

(define (determine-necessarily-fictitious-native-procedure-types!)
 (for-each
  (lambda (u) (set-native-procedure-type-necessarily-fictitious?! u #t))
  *native-procedure-types*)
 (let loop ()
  (let ((again? #f))
   (for-each
    (lambda (u)
     (unless (or (not (native-procedure-type-necessarily-fictitious? u))
		 (not (called? u))
		 (noop? u)
		 (every (lambda (e)
			 (or (not (environment-used? e))
			     (every (lambda (g)
				     (or (not (accessed? g))
					 (type-set-necessarily-fictitious?
					  (variable-type-set g))))
				    (free-variables e))))
			(narrow-clones u)))
      (set-native-procedure-type-necessarily-fictitious?! u #f)
      (set! again? #t)))
    *native-procedure-types*)
   (when again? (loop)))))

;;; Program points

(define (before x) (make-program-point #t x))

(define (after x) (make-program-point #f x))

(define (before? d) (program-point-before? d))

(define (after? d) (not (program-point-before? d)))

(define (expression d) (program-point-expression d))

(define (same-program-point? d1 d2)
 (and (eq? (before? d1) (before? d2)) (eq? (expression d1) (expression d2))))

(define (all-program-points) (append (map before *xs*) (map after *xs*)))

(define (control-directly-flows? d1 d2)
 ;; needs work: To handle calls to primitive, native, and foreign procedures
 ;;             that don't return.
 (or (and (before? d1)
	  (after? d2)
	  (eq? (expression d1) (expression d2))
	  (or (eq? (expression-kind (expression d1)) 'null-constant)
	      (eq? (expression-kind (expression d1)) 'true-constant)
	      (eq? (expression-kind (expression d1)) 'false-constant)
	      (eq? (expression-kind (expression d1)) 'char-constant)
	      (eq? (expression-kind (expression d1)) 'fixnum-constant)
	      (eq? (expression-kind (expression d1)) 'flonum-constant)
	      (eq? (expression-kind (expression d1)) 'rectangular-constant)
	      (eq? (expression-kind (expression d1)) 'string-constant)
	      (eq? (expression-kind (expression d1)) 'symbol-constant)
	      (eq? (expression-kind (expression d1)) 'pair-constant)
	      (eq? (expression-kind (expression d1)) 'vector-constant)
	      (eq? (expression-kind (expression d1)) 'lambda)
	      (eq? (expression-kind (expression d1)) 'converted-lambda)
	      (eq? (expression-kind (expression d1)) 'converted-continuation)
	      (eq? (expression-kind (expression d1)) 'primitive-procedure)
	      (eq? (expression-kind (expression d1)) 'foreign-procedure)
	      (eq? (expression-kind (expression d1)) 'access)))
     ;; From before SET! to before source.
     (and (before? d1)
	  (before? d2)
	  (eq? (expression-kind (expression d1)) 'set!)
	  (eq? (expression-source (expression d1)) (expression d2)))
     ;; From after source to after SET!.
     (and (after? d1)
	  (after? d2)
	  (eq? (expression-kind (expression d2)) 'set!)
	  (eq? (expression-source (expression d2)) (expression d1)))
     ;; From before IF to before antecedent.
     (and (before? d1)
	  (before? d2)
	  (eq? (expression-kind (expression d1)) 'if)
	  (eq? (expression-antecedent (expression d1)) (expression d2)))
     ;; From after antecedent to before consequent and alternate.
     (and
      (after? d1)
      (before? d2)
      (expression-parent (expression d1))
      (eq? (expression-kind (expression-parent (expression d1))) 'if)
      (eq? (expression-antecedent (expression-parent (expression d1)))
	   (expression d1))
      (or (and (eq? (expression-consequent (expression-parent (expression d1)))
		    (expression d2))
	       (can-be-non?
		false-type?
		(expression-type-set
		 (expression-antecedent (expression-parent (expression d1))))))
	  (and
	   (eq? (expression-alternate (expression-parent (expression d1)))
		(expression d2))
	   (can-be?
	    false-type?
	    (expression-type-set
	     (expression-antecedent (expression-parent (expression d1))))))))
     ;; From after consequent and alternate to after IF.
     (and (after? d1)
	  (after? d2)
	  (expression-parent (expression d1))
	  (eq? (expression-kind (expression-parent (expression d1))) 'if)
	  (or (eq? (expression-consequent (expression-parent (expression d1)))
		   (expression d1))
	      (eq? (expression-alternate (expression-parent (expression d1)))
		   (expression d1)))
	  (eq? (expression-parent (expression d1)) (expression d2)))
     ;; note: The following all assume a callee-first left-to-right argument
     ;;       evaluation order.
     ;; From before call to before callee.
     (and (before? d1)
	  (before? d2)
	  (or (eq? (expression-kind (expression d1)) 'call)
	      (eq? (expression-kind (expression d1)) 'converted-call))
	  (eq? (expression-callee (expression d1)) (expression d2)))
     ;; From after callee to before first argument.
     (and
      (after? d1)
      (before? d2)
      (expression-parent (expression d1))
      (or (eq? (expression-kind (expression-parent (expression d1))) 'call)
	  (eq? (expression-kind (expression-parent (expression d1)))
	       'converted-call))
      (eq? (expression-callee (expression-parent (expression d1)))
	   (expression d1))
      (not (null? (expression-arguments (expression-parent (expression d1)))))
      (eq? (first (expression-arguments (expression-parent (expression d1))))
	   (expression d2)))
     ;; From after each argument to before next argument.
     (and
      (after? d1)
      (before? d2)
      (expression-parent (expression d1))
      (or (eq? (expression-kind (expression-parent (expression d1))) 'call)
	  (eq? (expression-kind (expression-parent (expression d1)))
	       'converted-call))
      (memq (expression d1)
	    (expression-arguments (expression-parent (expression d1))))
      (< (positionq
	  (expression d1)
	  (expression-arguments (expression-parent (expression d1))))
	 (- (length (expression-arguments (expression-parent (expression d1))))
	    1))
      (eq? (list-ref
	    (expression-arguments (expression-parent (expression d1)))
	    (+ (positionq
		(expression d1)
		(expression-arguments (expression-parent (expression d1))))
	       1))
	   (expression d2)))
     ;; From after callee or last argument to before target when target is a
     ;; non-noop native procedure.
     (and
      (after? d1)
      (before? d2)
      (expression-parent (expression d1))
      (or (eq? (expression-kind (expression-parent (expression d1))) 'call)
	  (eq? (expression-kind (expression-parent (expression d1)))
	       'converted-call))
      (or (and
	   (eq? (expression-callee (expression-parent (expression d1)))
		(expression d1))
	   (null? (expression-arguments (expression-parent (expression d1)))))
	  (and
	   (memq (expression d1)
		 (expression-arguments (expression-parent (expression d1))))
	   (= (positionq
	       (expression d1)
	       (expression-arguments (expression-parent (expression d1))))
	      (- (length
		  (expression-arguments (expression-parent (expression d1))))
		 1))))
      (expression-parent (expression d2))
      (or (eq? (expression-kind (expression-parent (expression d2))) 'lambda)
	  (eq? (expression-kind (expression-parent (expression d2)))
	       'converted-lambda)
	  (eq? (expression-kind (expression-parent (expression d2)))
	       'converted-continuation))
      (called? (expression-environment (expression d2)))
      (can-be?
       (lambda (u)
	(and
	 ((truly-compatible-call? (expression-parent (expression d1))) u)
	 (or
	  (eq? u (environment-type (expression-environment (expression d2))))
	  (and ((primitive-procedure-type-named? 'apply) u)
	       (can-be?
		(lambda (u)
		 (and ((truly-compatible-call-via-apply? x) u)
		      (eq? u
			   (environment-type
			    (expression-environment (expression d2))))))
		(expression-type-set
		 (first-argument (expression-parent (expression d1))))))
	  (and
	   ((primitive-procedure-type-named? 'call-with-current-continuation)
	    u)
	   (can-be?
	    (lambda (u)
	     (and
	      ((truly-compatible-call-via-call-with-current-continuation? x) u)
	      (eq? u
		   (environment-type
		    (expression-environment (expression d2))))))
	    (expression-type-set
	     (first-argument (expression-parent (expression d1))))))
	  (and ((primitive-procedure-type-named? 'fork) u)
	       (or (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-fork1? x) u)
			  (eq? u
			       (environment-type
				(expression-environment (expression d2))))))
		    (expression-type-set
		     (first-argument (expression-parent (expression d1)))))
		   (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-fork2? x) u)
			  (eq? u
			       (environment-type
				(expression-environment (expression d2))))))
		    (expression-type-set
		     (second-argument (expression-parent (expression d1)))))))
	  (and ((primitive-procedure-type-named? 'mutex) u)
	       (can-be?
		(lambda (u)
		 (and ((truly-compatible-call-via-mutex? x) u)
		      (eq? u
			   (environment-type
			    (expression-environment (expression d2))))))
		(expression-type-set
		 (first-argument (expression-parent (expression d1)))))))))
       (expression-type-set
	(expression-callee (expression-parent (expression d1))))))
     ;; From after target to after call.
     (and
      (after? d1)
      (after? d2)
      (expression-parent (expression d1))
      ;; needs work: The following comment might not be accurate.
      ;; Converted calls and calls to converted lambdas and continuations never
      ;; return.
      (eq? (expression-kind (expression-parent (expression d1))) 'lambda)
      (eq? (expression-kind (expression d2)) 'call)
      (called? (expression-environment (expression d1)))
      (can-be?
       (lambda (u)
	(and
	 ((truly-compatible-call? (expression d2)) u)
	 (or
	  (eq? u (environment-type (expression-environment (expression d1))))
	  (and ((primitive-procedure-type-named? 'apply) u)
	       (can-be?
		(lambda (u)
		 (and ((truly-compatible-call-via-apply? x) u)
		      (eq? u
			   (environment-type
			    (expression-environment (expression d1))))))
		(expression-type-set
		 (first-argument (expression-parent (expression d2))))))
	  (and
	   ((primitive-procedure-type-named? 'call-with-current-continuation)
	    u)
	   (can-be?
	    (lambda (u)
	     (and
	      ((truly-compatible-call-via-call-with-current-continuation? x) u)
	      (eq? u
		   (environment-type
		    (expression-environment (expression d1))))))
	    (expression-type-set
	     (first-argument (expression-parent (expression d2))))))
	  (and ((primitive-procedure-type-named? 'fork) u)
	       (or (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-fork1? x) u)
			  (eq? u
			       (environment-type
				(expression-environment (expression d1))))))
		    (expression-type-set
		     (first-argument (expression-parent (expression d2)))))
		   (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-fork2? x) u)
			  (eq? u
			       (environment-type
				(expression-environment (expression d1))))))
		    (expression-type-set
		     (second-argument (expression-parent (expression d2)))))))
	  (and ((primitive-procedure-type-named? 'mutex) u)
	       (can-be?
		(lambda (u)
		 (and ((truly-compatible-call-via-mutex? x) u)
		      (eq? u
			   (environment-type
			    (expression-environment (expression d1))))))
		(expression-type-set
		 (first-argument (expression-parent (expression d2)))))))))
       (expression-type-set (expression-callee (expression d2)))))
     ;; From after callee or last argument to after call when target is a noop,
     ;; primitive procedure, or foreign procedure.
     (and
      (after? d1)
      (after? d2)
      (expression-parent (expression d1))
      (eq? (expression-parent (expression d1)) (expression d2))
      ;; needs work: The following comment might not be accurate.
      ;; Converted calls and calls to converted lambdas and continuations never
      ;; return.
      (eq? (expression-kind (expression d2)) 'call)
      (or (and (eq? (expression-callee (expression d2)) (expression d1))
	       (null? (expression-arguments (expression d2))))
	  (and (memq (expression d1) (expression-arguments (expression d2)))
	       (= (positionq (expression d1)
			     (expression-arguments (expression d2)))
		  (- (length (expression-arguments (expression d2))) 1))))
      (can-be?
       (lambda (u)
	(and
	 ((truly-compatible-call? (expression d2)) u)
	 (or
	  (and (native-procedure-type? u) (noop? u))
	  (and (primitive-procedure-type? u)
	       (not ((primitive-procedure-type-named? 'apply) u))
	       (not ((primitive-procedure-type-named?
		      'call-with-current-continuation)
		     u))
	       (not ((primitive-procedure-type-named? 'fork) u))
	       (not ((primitive-procedure-type-named? 'mutex) u)))
	  (foreign-procedure-type? u)
	  (and ((primitive-procedure-type-named? 'apply) u)
	       (can-be?
		(lambda (u)
		 (and ((truly-compatible-call-via-apply? x) u)
		      (or (and (native-procedure-type? u) (noop? u))
			  (foreign-procedure-type? u))))
		(expression-type-set (first-argument (expression d2)))))
	  (and
	   ((primitive-procedure-type-named? 'call-with-current-continuation)
	    u)
	   (can-be?
	    (lambda (u)
	     (and
	      ((truly-compatible-call-via-call-with-current-continuation? x) u)
	      (or (and (native-procedure-type? u) (noop? u))
		  (foreign-procedure-type? u))))
	    (expression-type-set (first-argument (expression d2)))))
	  (and ((primitive-procedure-type-named? 'fork) u)
	       (or (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-fork1? x) u)
			  (or (and (native-procedure-type? u) (noop? u))
			      (foreign-procedure-type? u))))
		    (expression-type-set (first-argument (expression d2))))
		   (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-fork2? x) u)
			  (or (and (native-procedure-type? u) (noop? u))
			      (foreign-procedure-type? u))))
		    (expression-type-set (second-argument (expression d2))))))
	  (and ((primitive-procedure-type-named? 'mutex) u)
	       (can-be?
		(lambda (u)
		 (and ((truly-compatible-call-via-mutex? x) u)
		      (or (and (native-procedure-type? u) (noop? u))
			  (foreign-procedure-type? u))))
		(expression-type-set (first-argument (expression d2))))))))
       (expression-type-set (expression-callee (expression d2)))))
     ;; From after callee or last argument to after call to
     ;; CALL-WITH-CURRENT-CONTINUATION when target is a continuation.
     ;; needs work: Doesn't handle implicit continuation calls.
     (and
      (after? d1)
      (after? d2)
      (expression-parent (expression d1))
      (or (eq? (expression-kind (expression-parent (expression d1))) 'call)
	  (eq? (expression-kind (expression-parent (expression d1)))
	       'converted-call))
      (or (and
	   (eq? (expression-callee (expression-parent (expression d1)))
		(expression d1))
	   (null? (expression-arguments (expression-parent (expression d1)))))
	  (and
	   (memq (expression d1)
		 (expression-arguments (expression-parent (expression d1))))
	   (= (positionq
	       (expression d1)
	       (expression-arguments (expression-parent (expression d1))))
	      (- (length
		  (expression-arguments (expression-parent (expression d1))))
		 1))))
      (can-be?
       (lambda (u)
	(and (eq? (continuation-type-allocating-expression u) (expression d2))
	     ((truly-compatible-call? (expression-parent (expression d1))) u)))
       (expression-type-set
	(expression-callee (expression-parent (expression d1))))))))

(define (control-properly-flows? d1 d2)
 (let ((ds '()))
  (some (lambda (d)
	 (let loop ((d d))
	  (or (same-program-point? d d2)
	      (and (not (memp same-program-point? d ds))
		   (begin
		    (set! ds (cons d ds))
		    (some loop (program-points-that-directly-flow-from d)))))))
	(program-points-that-directly-flow-from d1))))

(define (control-flows? d1 d2)
 (or (same-program-point? d1 d2) (control-properly-flows? d1 d2)))

(define (program-points-that-directly-flow-to d)
 ;; needs work: To handle calls to primitive, native, and foreign procedures
 ;;             that don't return.
 (if (before? d)
     (append
      ;; From before SET! to before source.
      (if (and
	   (expression-parent (expression d))
	   (eq? (expression-kind (expression-parent (expression d))) 'set!))
	  (list (before (expression-parent (expression d))))
	  '())
      ;; From before IF to before antecedent.
      (if (and
	   (expression-parent (expression d))
	   (eq? (expression-kind (expression-parent (expression d))) 'if)
	   (eq? (expression-antecedent (expression-parent (expression d)))
		(expression d)))
	  (list (before (expression-parent (expression d))))
	  '())
      ;; From after antecedent to before consequent and alternate.
      (if (and
	   (expression-parent (expression d))
	   (eq? (expression-kind (expression-parent (expression d))) 'if)
	   (or
	    (and
	     (eq? (expression-consequent (expression-parent (expression d)))
		  (expression d))
	     (can-be-non?
	      false-type?
	      (expression-type-set
	       (expression-antecedent (expression-parent (expression d))))))
	    (and
	     (eq? (expression-alternate (expression-parent (expression d)))
		  (expression d))
	     (can-be?
	      false-type?
	      (expression-type-set
	       (expression-antecedent (expression-parent (expression d))))))))
	  (list
	   (after (expression-antecedent (expression-parent (expression d)))))
	  '())
      ;; note: The following all assume a callee-first left-to-right argument
      ;;       evaluation order.
      ;; From before call to before callee.
      (if (and
	   (expression-parent (expression d))
	   (or (eq? (expression-kind (expression-parent (expression d))) 'call)
	       (eq? (expression-kind (expression-parent (expression d)))
		    'converted-call))
	   (eq? (expression-callee (expression-parent (expression d)))
		(expression d)))
	  (list (before (expression-parent (expression d))))
	  '())
      ;; From after callee to before first argument.
      (if (and
	   (expression-parent (expression d))
	   (or (eq? (expression-kind (expression-parent (expression d))) 'call)
	       (eq? (expression-kind (expression-parent (expression d)))
		    'converted-call))
	   (not
	    (null? (expression-arguments (expression-parent (expression d)))))
	   (eq?
	    (first (expression-arguments (expression-parent (expression d))))
	    (expression d)))
	  (list (after (expression-callee (expression-parent (expression d)))))
	  '())
      ;; From after each argument to before next argument.
      (if (and
	   (expression-parent (expression d))
	   (or (eq? (expression-kind (expression-parent (expression d))) 'call)
	       (eq? (expression-kind (expression-parent (expression d)))
		    'converted-call))
	   (memq (expression d)
		 (expression-arguments (expression-parent (expression d))))
	   (not
	    (eq? (expression d)
		 (first
		  (expression-arguments (expression-parent (expression d)))))))
	  (list
	   (after
	    (list-ref
	     (expression-arguments (expression-parent (expression d)))
	     (- (positionq
		 (expression d)
		 (expression-arguments (expression-parent (expression d))))
		1))))
	  '())
      ;; From after callee or last argument to before target when target is a
      ;; non-noop native procedure.
      (if
       (and
	(expression-parent (expression d))
	(or (eq? (expression-kind (expression-parent (expression d)))
		 'lambda)
	    (eq? (expression-kind (expression-parent (expression d)))
		 'converted-lambda)
	    (eq? (expression-kind (expression-parent (expression d)))
		 'converted-continuation))
	(called? (expression-environment (expression d))))
       (map
	(lambda (x)
	 (after (if (null? (expression-arguments x))
		    (expression-callee x)
		    (last (expression-arguments x)))))
	(remove-if-not
	 (lambda (x)
	  (can-be?
	   (lambda (u)
	    (and
	     ((truly-compatible-call? x) u)
	     (or
	      (eq?
	       u (environment-type (expression-environment (expression d))))
	      (and ((primitive-procedure-type-named? 'apply) u)
		   (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-apply? x) u)
			  (eq? u
			       (environment-type
				(expression-environment (expression d))))))
		    (expression-type-set (first-argument x))))
	      (and
	       ((primitive-procedure-type-named?
		 'call-with-current-continuation)
		u)
	       (can-be?
		(lambda (u)
		 (and
		  ((truly-compatible-call-via-call-with-current-continuation?
		    x)
		   u)
		  (eq? u
		       (environment-type
			(expression-environment (expression d))))))
		(expression-type-set (first-argument x))))
	      (and ((primitive-procedure-type-named? 'fork) u)
		   (or (can-be?
			(lambda (u)
			 (and ((truly-compatible-call-via-fork1? x) u)
			      (eq? u
				   (environment-type
				    (expression-environment (expression d))))))
			(expression-type-set (first-argument x)))
		       (can-be?
			(lambda (u)
			 (and ((truly-compatible-call-via-fork2? x) u)
			      (eq? u
				   (environment-type
				    (expression-environment (expression d))))))
			(expression-type-set (second-argument x)))))
	      (and ((primitive-procedure-type-named? 'mutex) u)
		   (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-mutex? x) u)
			  (eq? u
			       (environment-type
				(expression-environment (expression d))))))
		    (expression-type-set (first-argument x)))))))
	   (expression-type-set (expression-callee x))))
	 *calls*))
       '()))
     (append
      (if (or (eq? (expression-kind (expression d)) 'null-constant)
	      (eq? (expression-kind (expression d)) 'true-constant)
	      (eq? (expression-kind (expression d)) 'false-constant)
	      (eq? (expression-kind (expression d)) 'char-constant)
	      (eq? (expression-kind (expression d)) 'fixnum-constant)
	      (eq? (expression-kind (expression d)) 'flonum-constant)
	      (eq? (expression-kind (expression d)) 'rectangular-constant)
	      (eq? (expression-kind (expression d)) 'string-constant)
	      (eq? (expression-kind (expression d)) 'symbol-constant)
	      (eq? (expression-kind (expression d)) 'pair-constant)
	      (eq? (expression-kind (expression d)) 'vector-constant)
	      (eq? (expression-kind (expression d)) 'lambda)
	      (eq? (expression-kind (expression d)) 'converted-lambda)
	      (eq? (expression-kind (expression d)) 'converted-continuation)
	      (eq? (expression-kind (expression d)) 'primitive-procedure)
	      (eq? (expression-kind (expression d)) 'foreign-procedure)
	      (eq? (expression-kind (expression d)) 'access))
	  (list (before (expression d)))
	  '())
      ;; From after source to after SET!.
      (if (eq? (expression-kind (expression d)) 'set!)
	  (list (after (expression-source (expression d))))
	  '())
      ;; From after consequent and alternate to after IF.
      (if (eq? (expression-kind (expression d)) 'if)
	  (list (after (expression-consequent (expression d)))
		(after (expression-alternate (expression d))))
	  '())
      ;; From after target to after call.
      ;; needs work: The following comment might not be accurate.
      ;; Converted calls and calls to converted lambdas and continuations never
      ;; return.
      (if (eq? (expression-kind (expression d)) 'call)
	  (append
	   (map
	    (lambda (u)
	     (after
	      (expression-body
	       (environment-expression
		(callee-environment u (create-call-site (expression d)))))))
	    (members-that
	     (lambda (u)
	      (and (native-procedure-type? u)
		   (not (noop? u))
		   ((truly-compatible-call? (expression d)) u)))
	     (expression-type-set (expression-callee (expression d)))))
	   (if (can-be?
		(lambda (u)
		 (and ((primitive-procedure-type-named? 'apply) u)
		      ((truly-compatible-call? (expression d)) u)))
		(expression-type-set (expression-callee (expression d))))
	       (map
		(lambda (u)
		 (after
		  (expression-body
		   (environment-expression
		    (callee-environment
		     u
		     (recreate-call-site (create-call-site (expression d))
					 'first-argument))))))
		(members-that
		 (lambda (u)
		  (and (native-procedure-type? u)
		       (not (noop? u))
		       ((truly-compatible-call-via-apply? (expression d)) u)))
		 (expression-type-set (first-argument (expression d)))))
	       '())
	   (if (can-be?
		(lambda (u)
		 (and ((primitive-procedure-type-named?
			'call-with-current-continuation)
		       u)
		      ((truly-compatible-call? (expression d)) u)))
		(expression-type-set (expression-callee (expression d))))
	       (map
		(lambda (u)
		 (after
		  (expression-body
		   (environment-expression
		    (callee-environment
		     u
		     (recreate-call-site (create-call-site (expression d))
					 'first-argument))))))
		(members-that
		 (lambda (u)
		  (and
		   (native-procedure-type? u)
		   (not (noop? u))
		   ((truly-compatible-call-via-call-with-current-continuation?
		     (expression d))
		    u)))
		 (expression-type-set (first-argument (expression d)))))
	       '())
	   (if (can-be?
		(lambda (u)
		 (and ((primitive-procedure-type-named? 'fork) u)
		      ((truly-compatible-call? (expression d)) u)))
		(expression-type-set (expression-callee (expression d))))
	       (append
		(map
		 (lambda (u)
		  (after
		   (expression-body
		    (environment-expression
		     (callee-environment
		      u
		      (recreate-call-site (create-call-site (expression d))
					  'first-argument))))))
		 (members-that
		  (lambda (u)
		   (and (native-procedure-type? u)
			(not (noop? u))
			((truly-compatible-call-via-fork1? (expression d)) u)))
		  (expression-type-set (first-argument (expression d)))))
		(map
		 (lambda (u)
		  (after
		   (expression-body
		    (environment-expression
		     (callee-environment
		      u
		      (recreate-call-site (create-call-site (expression d))
					  'second-argument))))))
		 (members-that
		  (lambda (u)
		   (and (native-procedure-type? u)
			(not (noop? u))
			((truly-compatible-call-via-fork2? (expression d)) u)))
		  (expression-type-set (second-argument (expression d))))))
	       '())
	   (if (can-be?
		(lambda (u)
		 (and ((primitive-procedure-type-named? 'mutex) u)
		      ((truly-compatible-call? (expression d)) u)))
		(expression-type-set (expression-callee (expression d))))
	       (map
		(lambda (u)
		 (after
		  (expression-body
		   (environment-expression
		    (callee-environment
		     u
		     (recreate-call-site (create-call-site (expression d))
					 'first-argument))))))
		(members-that
		 (lambda (u)
		  (and (native-procedure-type? u)
		       (not (noop? u))
		       ((truly-compatible-call-via-mutex? (expression d)) u)))
		 (expression-type-set (first-argument (expression d)))))
	       '()))
	  '())
      ;; From after callee or last argument to after call when target is a
      ;; noop, primitive procedure, or foreign procedure.
      ;; needs work: The following comment might not be accurate.
      ;; Converted calls and calls to converted lambdas and continuations never
      ;; return.
      (if (and
	   (eq? (expression-kind (expression d)) 'call)
	   (can-be?
	    (lambda (u)
	     (and
	      ((truly-compatible-call? (expression d)) u)
	      (or
	       (and (native-procedure-type? u) (noop? u))
	       (and (primitive-procedure-type? u)
		    (not ((primitive-procedure-type-named? 'apply) u))
		    (not ((primitive-procedure-type-named?
			   'call-with-current-continuation)
			  u))
		    (not ((primitive-procedure-type-named? 'fork) u))
		    (not ((primitive-procedure-type-named? 'mutex) u)))
	       (foreign-procedure-type? u)
	       (and
		((primitive-procedure-type-named? 'apply) u)
		(can-be?
		 (lambda (u)
		  (and ((truly-compatible-call-via-apply? (expression d)) u)
		       (or (and (native-procedure-type? u) (noop? u))
			   (foreign-procedure-type? u))))
		 (expression-type-set (first-argument (expression d)))))
	       (and
		((primitive-procedure-type-named?
		  'call-with-current-continuation)
		 u)
		(can-be?
		 (lambda (u)
		  (and
		   ((truly-compatible-call-via-call-with-current-continuation?
		     (expression d))
		    u)
		   (or (and (native-procedure-type? u) (noop? u))
		       (foreign-procedure-type? u))))
		 (expression-type-set (first-argument (expression d)))))
	       (and
		((primitive-procedure-type-named? 'fork) u)
		(or
		 (can-be?
		  (lambda (u)
		   (and ((truly-compatible-call-via-fork1? (expression d)) u)
			(or (and (native-procedure-type? u) (noop? u))
			    (foreign-procedure-type? u))))
		  (expression-type-set (first-argument (expression d))))
		 (can-be?
		  (lambda (u)
		   (and ((truly-compatible-call-via-fork2? (expression d)) u)
			(or (and (native-procedure-type? u) (noop? u))
			    (foreign-procedure-type? u))))
		  (expression-type-set (second-argument (expression d))))))
	       (and
		((primitive-procedure-type-named? 'mutex) u)
		(can-be?
		 (lambda (u)
		  (and ((truly-compatible-call-via-mutex? (expression d)) u)
		       (or (and (native-procedure-type? u) (noop? u))
			   (foreign-procedure-type? u))))
		 (expression-type-set (first-argument (expression d))))))))
	    (expression-type-set (expression-callee (expression d)))))
	  (if (null? (expression-arguments (expression d)))
	      (list (after (expression-callee (expression d))))
	      (list (after (last (expression-arguments (expression d))))))
	  '())
      ;; From after callee or last argument to after call to
      ;; CALL-WITH-CURRENT-CONTINUATION when target is a continuation.
      ;; needs work: Doesn't handle implicit continuation calls.
      (if (and
	   (or (eq? (expression-kind (expression d)) 'call)
	       (eq? (expression-kind (expression d)) 'converted-call))
	   (can-be? (lambda (u)
		     (and ((primitive-procedure-type-named?
			    'call-with-current-continuation)
			   u)
			  ((truly-compatible-call? (expression d)) u)))
		    (expression-type-set (expression-callee (expression d)))))
	  (map
	   (lambda (x)
	    (after (if (null? (expression-arguments x))
		       (expression-callee x)
		       (last (expression-arguments x)))))
	   (remove-if-not
	    (lambda (x)
	     (can-be? (lambda (u)
		       (and (continuation-type? u)
			    (eq? (continuation-type-allocating-expression u)
				 (expression d))
			    ((truly-compatible-call? (expression d)) u)))
		      (expression-type-set (expression-callee x))))
	    *calls*))
	  '()))))

(define (program-points-that-properly-flow-to d)
 (let ((ds '()))
  (for-each (lambda (d)
	     (let loop ((d d))
	      (unless (memp same-program-point? d ds)
	       (set! ds (cons d ds))
	       (for-each loop (program-points-that-directly-flow-to d)))))
	    (program-points-that-directly-flow-to d))
  ds))

(define (program-points-that-flow-to d)
 (let ((ds (program-points-that-properly-flow-to d)))
  (if (memp same-program-point? d ds) ds (cons d ds))))

(define (program-points-that-directly-flow-from d)
 ;; needs work: To handle calls to primitive, native, and foreign procedures
 ;;             that don't return.
 (if (before? d)
     (cond ((or (eq? (expression-kind (expression d)) 'null-constant)
		(eq? (expression-kind (expression d)) 'true-constant)
		(eq? (expression-kind (expression d)) 'false-constant)
		(eq? (expression-kind (expression d)) 'char-constant)
		(eq? (expression-kind (expression d)) 'fixnum-constant)
		(eq? (expression-kind (expression d)) 'flonum-constant)
		(eq? (expression-kind (expression d)) 'rectangular-constant)
		(eq? (expression-kind (expression d)) 'string-constant)
		(eq? (expression-kind (expression d)) 'symbol-constant)
		(eq? (expression-kind (expression d)) 'pair-constant)
		(eq? (expression-kind (expression d)) 'vector-constant)
		(eq? (expression-kind (expression d)) 'lambda)
		(eq? (expression-kind (expression d)) 'converted-lambda)
		(eq? (expression-kind (expression d)) 'converted-continuation)
		(eq? (expression-kind (expression d)) 'primitive-procedure)
		(eq? (expression-kind (expression d)) 'foreign-procedure)
		(eq? (expression-kind (expression d)) 'access))
	    (list (after (expression d))))
	   ;; From before SET! to before source.
	   ((eq? (expression-kind (expression d)) 'set!)
	    (list (before (expression-source (expression d)))))
	   ;; From before IF to before antecedent.
	   ((eq? (expression-kind (expression d)) 'if)
	    (list (before (expression-antecedent (expression d)))))
	   ;; note: The following all assume a callee-first left-to-right
	   ;;       argument evaluation order.
	   ;; From before call to before callee.
	   ((or (eq? (expression-kind (expression d)) 'call)
		(eq? (expression-kind (expression d)) 'converted-call))
	    (list (before (expression-callee (expression d)))))
	   (else '()))
     (append
      ;; From after source to after SET!.
      (if (and
	   (expression-parent (expression d))
	   (eq? (expression-kind (expression-parent (expression d))) 'set!))
	  (list (after (expression-parent (expression d))))
	  '())
      ;; From after antecedent to before consequent and alternate.
      (if (and
	   (expression-parent (expression d))
	   (eq? (expression-kind (expression-parent (expression d))) 'if)
	   (eq? (expression-antecedent (expression-parent (expression d)))
		(expression d))
	   (can-be-non?
	    false-type?
	    (expression-type-set
	     (expression-antecedent (expression-parent (expression d))))))
	  (list
	   (before (expression-consequent (expression-parent (expression d)))))
	  '())
      (if (and
	   (expression-parent (expression d))
	   (eq? (expression-kind (expression-parent (expression d))) 'if)
	   (eq? (expression-antecedent (expression-parent (expression d)))
		(expression d))
	   (can-be?
	    false-type?
	    (expression-type-set
	     (expression-antecedent (expression-parent (expression d))))))
	  (list
	   (before (expression-alternate (expression-parent (expression d)))))
	  '())
      ;; From after consequent and alternate to after IF.
      (if (and
	   (expression-parent (expression d))
	   (eq? (expression-kind (expression-parent (expression d))) 'if)
	   (or (eq? (expression-consequent (expression-parent (expression d)))
		    (expression d))
	       (eq? (expression-alternate (expression-parent (expression d)))
		    (expression d))))
	  (list (after (expression-parent (expression d))))
	  '())
      ;; note: The following all assume a callee-first left-to-right argument
      ;;       evaluation order.
      ;; From after callee to before first argument.
      (if (and
	   (expression-parent (expression d))
	   (or (eq? (expression-kind (expression-parent (expression d))) 'call)
	       (eq? (expression-kind (expression-parent (expression d)))
		    'converted-call))
	   (eq? (expression-callee (expression-parent (expression d)))
		(expression d))
	   (not
	    (null? (expression-arguments (expression-parent (expression d))))))
	  (list
	   (before
	    (first (expression-arguments (expression-parent (expression d))))))
	  '())
      ;; From after each argument to before next argument.
      (if (and
	   (expression-parent (expression d))
	   (or (eq? (expression-kind (expression-parent (expression d))) 'call)
	       (eq? (expression-kind (expression-parent (expression d)))
		    'converted-call))
	   (memq (expression d)
		 (expression-arguments (expression-parent (expression d))))
	   (< (positionq
	       (expression d)
	       (expression-arguments (expression-parent (expression d))))
	      (- (length
		  (expression-arguments (expression-parent (expression d))))
		 1)))
	  (list
	   (before
	    (list-ref
	     (expression-arguments (expression-parent (expression d)))
	     (+ (positionq
		 (expression d)
		 (expression-arguments (expression-parent (expression d))))
		1))))
	  '())
      ;; From after callee or last argument to before target when target is a
      ;; non-noop native procedure.
      (if (and
	   (expression-parent (expression d))
	   (or (eq? (expression-kind (expression-parent (expression d))) 'call)
	       (eq? (expression-kind (expression-parent (expression d)))
		    'converted-call))
	   (or (and
		(eq? (expression-callee (expression-parent (expression d)))
		     (expression d))
		(null?
		 (expression-arguments (expression-parent (expression d)))))
	       (and
		(memq
		 (expression d)
		 (expression-arguments (expression-parent (expression d))))
		(= (positionq
		    (expression d)
		    (expression-arguments (expression-parent (expression d))))
		   (- (length (expression-arguments
			       (expression-parent (expression d))))
		      1)))))
	  (append
	   (map
	    (lambda (u)
	     (before
	      (expression-body
	       (environment-expression
		(callee-environment
		 u (create-call-site (expression-parent (expression d))))))))
	    (members-that
	     (lambda (u)
	      (and (native-procedure-type? u)
		   (not (noop? u))
		   ((truly-compatible-call? (expression-parent (expression d)))
		    u)))
	     (expression-type-set
	      (expression-callee (expression-parent (expression d))))))
	   (if (can-be?
		(lambda (u)
		 (and
		  ((primitive-procedure-type-named? 'apply) u)
		  ((truly-compatible-call? (expression-parent (expression d)))
		   u)))
		(expression-type-set
		 (expression-callee (expression-parent (expression d)))))
	       (map (lambda (u)
		     (before
		      (expression-body
		       (environment-expression
			(callee-environment
			 u
			 (recreate-call-site
			  (create-call-site (expression-parent (expression d)))
			  'first-argument))))))
		    (members-that
		     (lambda (u)
		      (and (native-procedure-type? u)
			   (not (noop? u))
			   ((truly-compatible-call-via-apply?
			     (expression-parent (expression d)))
			    u)))
		     (expression-type-set
		      (first-argument (expression-parent (expression d))))))
	       '())
	   (if (can-be?
		(lambda (u)
		 (and ((primitive-procedure-type-named?
			'call-with-current-continuation)
		       u)
		      ((truly-compatible-call?
			(expression-parent (expression d)))
		       u)))
		(expression-type-set
		 (expression-callee (expression-parent (expression d)))))
	       (map
		(lambda (u)
		 (before
		  (expression-body
		   (environment-expression
		    (callee-environment
		     u
		     (recreate-call-site
		      (create-call-site (expression-parent (expression d)))
		      'first-argument))))))
		(members-that
		 (lambda (u)
		  (and
		   (native-procedure-type? u)
		   (not (noop? u))
		   ((truly-compatible-call-via-call-with-current-continuation?
		     (expression-parent (expression d)))
		    u)))
		 (expression-type-set
		  (first-argument (expression-parent (expression d))))))
	       '())
	   (if (can-be?
		(lambda (u)
		 (and
		  ((primitive-procedure-type-named? 'fork) u)
		  ((truly-compatible-call? (expression-parent (expression d)))
		   u)))
		(expression-type-set
		 (expression-callee (expression-parent (expression d)))))
	       (append
		(map
		 (lambda (u)
		  (before
		   (expression-body
		    (environment-expression
		     (callee-environment
		      u
		      (recreate-call-site
		       (create-call-site (expression-parent (expression d)))
		       'first-argument))))))
		 (members-that
		  (lambda (u)
		   (and (native-procedure-type? u)
			(not (noop? u))
			((truly-compatible-call-via-fork1?
			  (expression-parent (expression d)))
			 u)))
		  (expression-type-set
		   (first-argument (expression-parent (expression d))))))
		(map
		 (lambda (u)
		  (before
		   (expression-body
		    (environment-expression
		     (callee-environment
		      u
		      (recreate-call-site
		       (create-call-site (expression-parent (expression d)))
		       'second-argument))))))
		 (members-that
		  (lambda (u)
		   (and (native-procedure-type? u)
			(not (noop? u))
			((truly-compatible-call-via-fork2?
			  (expression-parent (expression d)))
			 u)))
		  (expression-type-set
		   (second-argument (expression-parent (expression d)))))))
	       '())
	   (if (can-be?
		(lambda (u)
		 (and
		  ((primitive-procedure-type-named? 'mutex) u)
		  ((truly-compatible-call? (expression-parent (expression d)))
		   u)))
		(expression-type-set
		 (expression-callee (expression-parent (expression d)))))
	       (map (lambda (u)
		     (before
		      (expression-body
		       (environment-expression
			(callee-environment
			 u
			 (recreate-call-site
			  (create-call-site (expression-parent (expression d)))
			  'first-argument))))))
		    (members-that
		     (lambda (u)
		      (and (native-procedure-type? u)
			   (not (noop? u))
			   ((truly-compatible-call-via-mutex?
			     (expression-parent (expression d)))
			    u)))
		     (expression-type-set
		      (first-argument (expression-parent (expression d))))))
	       '()))
	  '())
      ;; From after target to after call.
      (if
       (and
	(expression-parent (expression d))
	;; needs work: The following comment might not be accurate.
	;; Calls to converted lambdas and continuations never return.
	(eq? (expression-kind (expression-parent (expression d))) 'lambda)
	(called? (expression-environment (expression d))))
       (map
	after
	(remove-if-not
	 (lambda (x)
	  (can-be?
	   (lambda (u)
	    (and
	     ((truly-compatible-call? x) u)
	     (or
	      (eq?
	       u (environment-type (expression-environment (expression d))))
	      (and ((primitive-procedure-type-named? 'apply) u)
		   (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-apply? x) u)
			  (eq? u
			       (environment-type
				(expression-environment (expression d))))))
		    (expression-type-set (first-argument x))))
	      (and
	       ((primitive-procedure-type-named?
		 'call-with-current-continuation)
		u)
	       (can-be?
		(lambda (u)
		 (and
		  ((truly-compatible-call-via-call-with-current-continuation?
		    x)
		   u)
		  (eq? u
		       (environment-type
			(expression-environment (expression d))))))
		(expression-type-set (first-argument x))))
	      (and ((primitive-procedure-type-named? 'fork) u)
		   (or (can-be?
			(lambda (u)
			 (and ((truly-compatible-call-via-fork1? x) u)
			      (eq? u
				   (environment-type
				    (expression-environment (expression d))))))
			(expression-type-set (first-argument x)))
		       (can-be?
			(lambda (u)
			 (and ((truly-compatible-call-via-fork2? x) u)
			      (eq? u
				   (environment-type
				    (expression-environment (expression d))))))
			(expression-type-set (second-argument x)))))
	      (and ((primitive-procedure-type-named? 'mutex) u)
		   (can-be?
		    (lambda (u)
		     (and ((truly-compatible-call-via-mutex? x) u)
			  (eq? u
			       (environment-type
				(expression-environment (expression d))))))
		    (expression-type-set (first-argument x)))))))
	   (expression-type-set (expression-callee x))))
	 *calls*))
       '())
      ;; From after callee or last argument to after call when target is a
      ;; noop, primitive procedure, or foreign procedure.
      (if (and
	   (expression-parent (expression d))
	   ;; needs work: The following comment might not be accurate.
	   ;; Converted calls never return.
	   (eq? (expression-kind (expression-parent (expression d))) 'call)
	   (or
	    (and
	     (eq? (expression-callee (expression-parent (expression d)))
		  (expression d))
	     (null?
	      (expression-arguments (expression-parent (expression d)))))
	    (and
	     (memq
	      (expression d)
	      (expression-arguments (expression-parent (expression d))))
	     (= (positionq
		 (expression d)
		 (expression-arguments (expression-parent (expression d))))
		(- (length (expression-arguments
			    (expression-parent (expression d))))
		   1)))
	    (can-be?
	     (lambda (u)
	      (and
	       ((truly-compatible-call? (expression-parent (expression d))) u)
	       (or
		(and (native-procedure-type? u) (noop? u))
		(and (primitive-procedure-type? u)
		     (not ((primitive-procedure-type-named? 'apply) u))
		     (not ((primitive-procedure-type-named?
			    'call-with-current-continuation)
			   u))
		     (not ((primitive-procedure-type-named? 'fork) u))
		     (not ((primitive-procedure-type-named? 'mutex) u)))
		(foreign-procedure-type? u)
		(and ((primitive-procedure-type-named? 'apply) u)
		     (can-be?
		      (lambda (u)
		       (and ((truly-compatible-call-via-apply?
			      (expression-parent (expression d)))
			     u)
			    (or (and (native-procedure-type? u) (noop? u))
				(foreign-procedure-type? u))))
		      (expression-type-set
		       (first-argument (expression-parent (expression d))))))
		(and
		 ((primitive-procedure-type-named?
		   'call-with-current-continuation)
		  u)
		 (can-be?
		  (lambda (u)
		   (and
		    ((truly-compatible-call-via-call-with-current-continuation?
		      (expression-parent (expression d)))
		     u)
		    (or (and (native-procedure-type? u) (noop? u))
			(foreign-procedure-type? u))))
		  (expression-type-set
		   (first-argument (expression-parent (expression d))))))
		(and
		 ((primitive-procedure-type-named? 'fork) u)
		 (or (can-be?
		      (lambda (u)
		       (and ((truly-compatible-call-via-fork1?
			      (expression-parent (expression d)))
			     u)
			    (or (and (native-procedure-type? u) (noop? u))
				(foreign-procedure-type? u))))
		      (expression-type-set
		       (first-argument (expression-parent (expression d)))))
		     (can-be?
		      (lambda (u)
		       (and ((truly-compatible-call-via-fork2?
			      (expression-parent (expression d)))
			     u)
			    (or (and (native-procedure-type? u) (noop? u))
				(foreign-procedure-type? u))))
		      (expression-type-set
		       (second-argument (expression-parent (expression d)))))))
		(and
		 ((primitive-procedure-type-named? 'mutex) u)
		 (can-be?
		  (lambda (u)
		   (and ((truly-compatible-call-via-mutex?
			  (expression-parent (expression d)))
			 u)
			(or (and (native-procedure-type? u) (noop? u))
			    (foreign-procedure-type? u))))
		  (expression-type-set
		   (first-argument (expression-parent (expression d)))))))))
	     (expression-type-set
	      (expression-callee (expression-parent (expression d)))))))
	  (list (after (expression-parent (expression d))))
	  '())
      ;; From after callee or last argument to after call to
      ;; CALL-WITH-CURRENT-CONTINUATION when target is a continuation.
      ;; needs work: Doesn't handle implicit continuations calls.
      (if (and
	   (expression-parent (expression d))
	   (or (eq? (expression-kind (expression-parent (expression d))) 'call)
	       (eq? (expression-kind (expression-parent (expression d)))
		    'converted-call))
	   (or (and
		(eq? (expression-callee (expression-parent (expression d)))
		     (expression d))
		(null?
		 (expression-arguments (expression-parent (expression d)))))
	       (and
		(memq
		 (expression d)
		 (expression-arguments (expression-parent (expression d))))
		(= (positionq
		    (expression d)
		    (expression-arguments (expression-parent (expression d))))
		   (- (length (expression-arguments
			       (expression-parent (expression d))))
		      1)))))
	  (map
	   (lambda (u) (after (continuation-type-allocating-expression u)))
	   (members-that
	    (lambda (u)
	     (and
	      (continuation-type? u)
	      ((truly-compatible-call? (expression-parent (expression d))) u)))
	    (expression-type-set
	     (expression-callee (expression-parent (expression d))))))
	  '()))))

(define (program-points-that-properly-flow-from d)
 (let ((ds '()))
  (for-each (lambda (d)
	     (let loop ((d d))
	      (unless (memp same-program-point? d ds)
	       (set! ds (cons d ds))
	       (for-each loop (program-points-that-directly-flow-from d)))))
	    (program-points-that-directly-flow-from d))
  ds))

(define (program-points-that-flow-from d)
 (let ((ds (program-points-that-properly-flow-from d)))
  (if (memp same-program-point? d ds) ds (cons d ds))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
