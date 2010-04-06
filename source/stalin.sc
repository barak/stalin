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
(MODULE
 STALIN
 (WITH
  QOBISCHEME
  XLIB
  STALIN1
  STALIN2
  STALIN3A
  STALIN3B
  STALIN4A
  STALIN4B
  STALIN4C
  STALIN4D
  STALIN5A
  STALIN5B
  STALIN5C
  STALIN5D
  STALIN5E)
 (MAIN MAIN))

(include "QobiScheme.sch")
(include "stalin.sch")
;;; End delete for Trotsky

(define-command
 (main (at-most-one ("version" version?))
       (any-number
	("I" include-path? (include-path "include-directory" string-argument)))
       (at-most-one ("s" s?) ("x" x?) ("q" q?) ("t" t?))
       (at-most-one
	("treat-all-symbols-as-external"
	 treat-all-symbols-as-external?)
	("do-not-treat-all-symbols-as-external"
	 do-not-treat-all-symbols-as-external?))
       (at-most-one
	("index-allocated-string-types-by-expression"
	 index-allocated-string-types-by-expression?)
	("do-not-index-allocated-string-types-by-expression"
	 do-not-index-allocated-string-types-by-expression?))
       (at-most-one
	("index-constant-structure-types-by-slot-types"
	 index-constant-structure-types-by-slot-types?)
	("do-not-index-constant-structure-types-by-slot-types"
	 do-not-index-constant-structure-types-by-slot-types?))
       (at-most-one
	("index-constant-structure-types-by-expression"
	 index-constant-structure-types-by-expression?)
	("do-not-index-constant-structure-types-by-expression"
	 do-not-index-constant-structure-types-by-expression?))
       (at-most-one
	("index-allocated-structure-types-by-slot-types"
	 index-allocated-structure-types-by-slot-types?)
	("do-not-index-allocated-structure-types-by-slot-types"
	 do-not-index-allocated-structure-types-by-slot-types?))
       (at-most-one
	("index-allocated-structure-types-by-expression"
	 index-allocated-structure-types-by-expression?)
	("do-not-index-allocated-structure-types-by-expression"
	 do-not-index-allocated-structure-types-by-expression?))
       (at-most-one
	("index-constant-headed-vector-types-by-element-type"
	 index-constant-headed-vector-types-by-element-type?)
	("do-not-index-constant-headed-vector-types-by-element-type"
	 do-not-index-constant-headed-vector-types-by-element-type?))
       (at-most-one
	("index-constant-headed-vector-types-by-expression"
	 index-constant-headed-vector-types-by-expression?)
	("do-not-index-constant-headed-vector-types-by-expression"
	 do-not-index-constant-headed-vector-types-by-expression?))
       (at-most-one
	("index-allocated-headed-vector-types-by-element-type"
	 index-allocated-headed-vector-types-by-element-type?)
	("do-not-index-allocated-headed-vector-types-by-element-type"
	 do-not-index-allocated-headed-vector-types-by-element-type?))
       (at-most-one
	("index-allocated-headed-vector-types-by-expression"
	 index-allocated-headed-vector-types-by-expression?)
	("do-not-index-allocated-headed-vector-types-by-expression"
	 do-not-index-allocated-headed-vector-types-by-expression?))
       (at-most-one
	("index-constant-nonheaded-vector-types-by-element-type"
	 index-constant-nonheaded-vector-types-by-element-type?)
	("do-not-index-constant-nonheaded-vector-types-by-element-type"
	 do-not-index-constant-nonheaded-vector-types-by-element-type?))
       (at-most-one
	("index-constant-nonheaded-vector-types-by-expression"
	 index-constant-nonheaded-vector-types-by-expression?)
	("do-not-index-constant-nonheaded-vector-types-by-expression"
	 do-not-index-constant-nonheaded-vector-types-by-expression?))
       (at-most-one
	("index-allocated-nonheaded-vector-types-by-element-type"
	 index-allocated-nonheaded-vector-types-by-element-type?)
	("do-not-index-allocated-nonheaded-vector-types-by-element-type"
	 do-not-index-allocated-nonheaded-vector-types-by-element-type?))
       (at-most-one
	("index-allocated-nonheaded-vector-types-by-expression"
	 index-allocated-nonheaded-vector-types-by-expression?)
	("do-not-index-allocated-nonheaded-vector-types-by-expression"
	 do-not-index-allocated-nonheaded-vector-types-by-expression?))
       (at-most-one
	("no-clone-size-limit" no-clone-size-limit?)
	("clone-size-limit"
	 clone-size-limit?
	 (clone-size-limit "number-of-expressions" integer-argument 80)))
       (at-most-one ("split-even-if-no-widening" split-even-if-no-widening?))
       (at-most-one ("fully-convert-to-CPS" fully-convert-to-CPS?)
		    ("no-escaping-continuations" no-escaping-continuations?))
       (at-most-one ("du" disable-uniqueness?))
       (at-most-one ("Ob" disable-bounds-checks?))
       (at-most-one ("Om" disable-memory-checks?))
       (at-most-one ("On" disable-overflow-checks?))
       (at-most-one ("Or" disable-runtime-checks?))
       (at-most-one ("Ot" disable-type-checks?))
       (at-most-one ("d0" p0?))
       (at-most-one ("d1" p1?))
       (at-most-one ("d2" p2?))
       (at-most-one ("d3" p3?))
       (at-most-one ("d4" p4?))
       (at-most-one ("d5" p5?))
       (at-most-one ("d6" p6?))
       (at-most-one ("d7" p7?))
       (at-most-one ("closure-conversion-statistics"
		     closure-conversion-statistics?))
       (at-most-one ("dc" disable-stack-allocation?))
       (at-most-one ("dC" disable-heap-allocation?))
       (at-most-one ("dH" disable-region-allocation?))
       (at-most-one ("dg" memory-messages?))
       (at-most-one ("dh" disable-expandable-regions?))
       (at-most-one ("d" d?))
       (at-most-one ("architecture"
		     architecture-name?
		     (architecture-name "name" string-argument "")))
       (at-most-one ("baseline" baseline?)
		    ("conventional" conventional?)
		    ("lightweight" lightweight?))
       (at-most-one ("immediate-flat" immediate-flat?)
		    ("indirect-flat" indirect-flat?)
		    ("immediate-display" immediate-display?)
		    ("indirect-display" indirect-display?)
		    ("linked" linked?))
       (at-most-one ("align-strings" align-strings?)
		    ("do-not-align-strings" do-not-align-strings?))
       (at-most-one ("de" eq?-forgery?))
       (at-most-one ("df" disable-forgery?))
       (at-most-one ("dG" globals?))
       (at-most-one ("di" type-if?))
       (at-most-one ("dI" immediate-structures?))
       (at-most-one ("dp" promote-representations?))
       (at-most-one ("dP" copy-propagation?))
       (at-most-one ("ds" disable-squeezing?))
       (at-most-one ("dS" disable-squishing?))
       (at-most-one ("Tmk" treadmarks?))
       (at-most-one
	("no-tail-call-optimization" disable-tail-call-optimization?))
       (at-most-one ("db" disable-database?))
       (at-most-one ("c" disable-run-cc?))
       (at-most-one ("k" keep-c?))
       (at-most-one ("cc" cc? (cc "C-compiler" string-argument "gcc")))
       (any-number ("copt" copt? (copts "C-compiler-options" string-argument)))
       (optional (pathname "pathname" string-argument #f)))
 ;; conventions
 (set! *program* "stalin")
 (set! *october?* #f)
 (when version? (format #t "0.11~%") (exit -1))
 (unless pathname (panic "You must specify a pathname"))
 (initialize-options!
  (append '(".") include-path '("/usr/local/stalin/include")))
 (when s? (set! *Scheme->C-compatibility?* #t))
 (when x?
  (set! *Scheme->C-compatibility?* #t)
  (set! *Xlib-and-GL?* #t))
 (when q?
  (set! *Scheme->C-compatibility?* #t)
  (set! *Xlib-and-GL?* #t)
  (set! *QobiScheme?* #t))
 (when t?
  (set! *Scheme->C-compatibility?* #t)
  (set! *Xlib-and-GL?* #t)
  (set! *QobiScheme?* #t)
  (set! *Trotsky?* #t))
 (when treat-all-symbols-as-external?
  (set! *treat-all-symbols-as-external?* #t))
 (when do-not-treat-all-symbols-as-external?
  (set! *treat-all-symbols-as-external?* #f))
 (when index-allocated-string-types-by-expression?
  (set! *index-allocated-string-types-by-expression?* #t))
 (when do-not-index-allocated-string-types-by-expression?
  (set! *index-allocated-string-types-by-expression?* #f))
 (when index-constant-structure-types-by-slot-types?
  (set! *index-constant-structure-types-by-slot-types?* #t))
 (when do-not-index-constant-structure-types-by-slot-types?
  (set! *index-constant-structure-types-by-slot-types?* #f))
 (when index-constant-structure-types-by-expression?
  (set! *index-constant-structure-types-by-expression?* #t))
 (when do-not-index-constant-structure-types-by-expression?
  (set! *index-constant-structure-types-by-expression?* #f))
 (when index-allocated-structure-types-by-slot-types?
  (set! *index-allocated-structure-types-by-slot-types?* #t))
 (when do-not-index-allocated-structure-types-by-slot-types?
  (set! *index-allocated-structure-types-by-slot-types?* #f))
 (when index-allocated-structure-types-by-expression?
  (set! *index-allocated-structure-types-by-expression?* #t))
 (when do-not-index-allocated-structure-types-by-expression?
  (set! *index-allocated-structure-types-by-expression?* #f))
 (when index-constant-headed-vector-types-by-element-type?
  (set! *index-constant-headed-vector-types-by-element-type?* #t))
 (when do-not-index-constant-headed-vector-types-by-element-type?
  (set! *index-constant-headed-vector-types-by-element-type?* #f))
 (when index-constant-headed-vector-types-by-expression?
  (set! *index-constant-headed-vector-types-by-expression?* #t))
 (when do-not-index-constant-headed-vector-types-by-expression?
  (set! *index-constant-headed-vector-types-by-expression?* #f))
 (when index-allocated-headed-vector-types-by-element-type?
  (set! *index-allocated-headed-vector-types-by-element-type?* #t))
 (when do-not-index-allocated-headed-vector-types-by-element-type?
  (set! *index-allocated-headed-vector-types-by-element-type?* #f))
 (when index-allocated-headed-vector-types-by-expression?
  (set! *index-allocated-headed-vector-types-by-expression?* #t))
 (when do-not-index-allocated-headed-vector-types-by-expression?
  (set! *index-allocated-headed-vector-types-by-expression?* #f))
 (when index-constant-nonheaded-vector-types-by-element-type?
  (set! *index-constant-nonheaded-vector-types-by-element-type?* #t))
 (when do-not-index-constant-nonheaded-vector-types-by-element-type?
  (set! *index-constant-nonheaded-vector-types-by-element-type?* #f))
 (when index-constant-nonheaded-vector-types-by-expression?
  (set! *index-constant-nonheaded-vector-types-by-expression?* #t))
 (when do-not-index-constant-nonheaded-vector-types-by-expression?
  (set! *index-constant-nonheaded-vector-types-by-expression?* #f))
 (when index-allocated-nonheaded-vector-types-by-element-type?
  (set! *index-allocated-nonheaded-vector-types-by-element-type?* #t))
 (when do-not-index-allocated-nonheaded-vector-types-by-element-type?
  (set! *index-allocated-nonheaded-vector-types-by-element-type?* #f))
 (when index-allocated-nonheaded-vector-types-by-expression?
  (set! *index-allocated-nonheaded-vector-types-by-expression?* #t))
 (when do-not-index-allocated-nonheaded-vector-types-by-expression?
  (set! *index-allocated-nonheaded-vector-types-by-expression?* #f))
 (cond (no-clone-size-limit? (set! *clone-size-limit* -1))
       (clone-size-limit?
	(when (or (not (integer? clone-size-limit))
		  (not (exact? clone-size-limit))
		  (negative? clone-size-limit))
	 (notify "Invalid clone size limit: ~a" clone-size-limit)
	 (terminate))
	(set! *clone-size-limit* clone-size-limit)))
 (when split-even-if-no-widening? (set! *split-even-if-no-widening?* #t))
 (when fully-convert-to-CPS? (set! *fully-convert-to-CPS?* #t))
 (when no-escaping-continuations? (set! *no-escaping-continuations?* #t))
 (when disable-uniqueness? (set! *uniqueness?* #f))
 (when disable-bounds-checks? (set! *bounds-checks?* #f))
 (when disable-memory-checks? (set! *memory-checks?* #f))
 (when disable-overflow-checks? (set! *overflow-checks?* #f))
 (when disable-runtime-checks? (set! *runtime-checks?* #f))
 (when disable-type-checks? (set! *type-checks?* #f))
 (when p0? (set! *panic?* #f))
 (when p1? (set! *p1?* #t))
 (when p2? (set! *p2?* #t))
 (when p3? (set! *p3?* #t))
 (when p4? (set! *p4?* #t))
 (when p5? (set! *p5?* #t))
 (when p6? (set! *p6?* #t))
 (when p7? (set! *p7?* #t))
 (when closure-conversion-statistics?
  (set! *closure-conversion-statistics?* #t))
 (when disable-stack-allocation? (set! *stack-allocation?* #f))
 (when disable-heap-allocation? (set! *heap-allocation?* #f))
 (when disable-region-allocation? (set! *region-allocation?* #f))
 (when memory-messages? (set! *memory-messages?* #t))
 (when disable-expandable-regions? (set! *expandable-regions?* #f))
 (when d? (set! *flonum-representation* 'double))
 (when architecture-name? (set! *architecture-name* architecture-name))
 (when baseline? (set! *closure-conversion-method* 'baseline))
 (when conventional? (set! *closure-conversion-method* 'conventional))
 (when lightweight? (set! *closure-conversion-method* 'lightweight))
 (when immediate-flat? (set! *closure-representation* 'immediate-flat))
 (when indirect-flat? (set! *closure-representation* 'indirect-flat))
 (when immediate-display? (set! *closure-representation* 'immediate-display))
 (when indirect-display? (set! *closure-representation* 'indirect-display))
 (when linked? (set! *closure-representation* 'linked))
 (when align-strings? (set! *align-strings?* #t))
 (when do-not-align-strings? (set! *align-strings?* #f))
 (when eq?-forgery? (set! *eq?-forgery?* #t))
 (when disable-forgery? (set! *forgery?* #f))
 (when globals? (set! *globals?* #t))
 (when type-if? (set! *type-if?* #t))
 (when immediate-structures? (set! *immediate-structures?* #t))
 (when promote-representations? (set! *promote-representations?* #t))
 (when copy-propagation? (set! *copy-propagation?* #t))
 (when disable-squeezing?
  (set! *squeeze?* #f)
  (set! *squish?* #f))
 (when disable-squishing? (set! *squish?* #f))
 (when treadmarks? (set! *treadmarks?* #t))
 (when disable-tail-call-optimization? (set! *tail-call-optimization?* #f))
 (when disable-database? (set! *database?* #f))
 (when disable-run-cc? (set! *run-cc?* #f))
 (when keep-c? (set! *keep-c?* #t))
 (when cc? (set! *cc* cc))
 (set! *copts* copts)
 (stalin pathname
	 (lambda ()
	  (unless (can-open-file-for-input? (default-extension pathname "sc"))
	   (notify "Cannot find: ~a" (default-extension pathname "sc"))
	   (terminate))
	  (let ((ss (read-s-expressions (default-extension pathname "sc"))))
	   (list (scheme-library (symbols-in (scheme-library '() ss)) ss)
		 (lambda (output-port)
		  ;; conventions: OUTPUT-PORT
		  #f))))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
