;;; LaHaShem HaAretz U'Mloah

;;; Stalin 0.10 - A global optimizing compiler for Scheme
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997, 1998, 1999, 2000, and 2001 NEC Research Institute, Inc. All
;;; rights reserved.

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
;;;    NEC Research Institute, Inc.
;;;    4 Independence Way
;;;    Princeton NJ 08540-6620 USA
;;;    voice: 609/951-2705
;;;    FAX:   609/951-2483
;;;    Qobi@research.nj.nec.com
;;;    ftp://ftp.nj.nec.com/pub/qobi
;;;    http://www.neci.nj.nec.com/homepages/qobi

;;; Stalin outputs its internal database of assertions about the program being
;;; compiled in a form that Emacs can read. Emacs commands allow one to
;;; conveniently query this database to assertain properties of the program.
;;; Stalin has its own READ procedure to keep track of the line and character
;;; positions of each S expression in the source file being compiled so that
;;; it can generate more informative error messages. These Emacs commands use
;;; this information.
;;;
;;; These commands are similar to the c-sh-A/m-. commands from the MIT Lisp
;;; Machine only much much more sophisticated. The current interface is
;;; extremely simple yet exceedingly powerful:
;;;
;;; m-sh-V  If the cursor points to a variable reference or assignment (SET!)
;;;         expression, move the cursor to the defining occurrence for that
;;;         variable (i.e. lambda expression or derivative that binds that
;;;         variable). If the cursor points to a defining occurrence move it to
;;;         one of its references (variable reference or assignment). As a
;;;         special case this encompasses the functionality of m-. but is much
;;;         more general.
;;;
;;; m-sh-N  Go to the next variable reference.
;;;
;;; m-sh-P  Go to the previous variable reference.
;;;
;;; m-sh-T  Tell me the type of the defining occurence or expression at the
;;;         cursor position.
;;;
;;; m-sh-I  Tell me all the information you know (except the type) about the
;;;         defining occurrence or expression at the cursor position.
;;;
;;; m-sh-Q  Tell me all the information you know about the type of the defining
;;;         occurrence or expression at the cursor position (not the type
;;;         itself but rather information about the type).
;;;
;;; m-sh-M  The cursor must point at a defining occurrence or expression whose
;;;         type is a native procedure monotype. Tell me all the information
;;;         you know about that procedure.
;;;
;;; m-sh-W  Go to the first compiler warning.
;;;
;;; m-sh-F  Go to the next compiler warning.
;;;
;;; m-sh-B  Go to the previous compiler warning.
;;;
;;; m-sh-D  Go to the first driver for this variable.
;;;
;;; m-sh-A  Go to the next driver for this variable.
;;;
;;; m-sh-E  Go to the previous driver for this variable.
;;;
;;; I view this as a prototype of the way programming environments ought to be.
;;; Start to finish, it took me less than two days to add this functionality to
;;; Stalin. Having done this, I now am thinking of additional functionality.
;;; One simple addition would be an ability to cycle through all assignments
;;; to a variable or all expressions that appear as parameters to that
;;; variable in a procedure call. This would allow a person to simply ask and
;;; answer the question `Why does this variable have the type that it has?'
;;; Down the road, I have a more ambitious project: I can easily augment the
;;; Stalin code generator to keep track of the bidirectional mapping between
;;; character positions in the Scheme source code and the C object code. This
;;; would allow a compiler developer to easily jump back and forth between the
;;; source and object code. But more important, it would allow building a
;;; high-level Scheme debugger on top of a low-level C debugger like gdb/gud.
;;; I don't think that it would be difficult to:
;;;
;;; - allow source-level Scheme tracing and single stepping during execution by
;;;   moving the coursor around the Scheme program
;;;
;;; - allow breakpoints
;;;
;;; - allow getting a backtrace
;;;
;;; - allow examining and changing the value of any Scheme variable, even ones
;;;   within a lexical closure
;;;
;;; The important thing is that this would be done on fully optimized compiled
;;; code, not interpreted code or half-compiled code with hooks left for the
;;; debugger (such hooks preclude many optimizations).
;;;
;;; Now I need to find someone willing to build this gdb/gud interface since I
;;; don't have the time to do it myself.
;;;
;;; TTMTTD
;;;  1. associate databases with buffers
;;;  2. call graph
;;;  3. allocation expressions
;;;  4. need to save database when writing file using WRITE-FILE-HOOKS
;;;  5. screws up on converted code
;;;  6. move point back and forth between Scheme and C code
;;;  7. interface to gdb/gud
;;;     a. single step C code should move point through Scheme
;;;     b. breakpoints
;;;        i. after expression
;;;        ii. procedure entry
;;;        iii. procedure exit
;;;     c. examine/modify variables, even slotted variables inside procedures
;;;     d. backtrace
;;;  8. m-D takes a long time
;;;  9. should allow pointing to a destination of a SET!
;;; 10. test10.sc doesn't show the DEFINE driver for *X*

;;; needs work: The following are strings and not symbols because the
;;;             allowed characters in symbols differs between Scheme and
;;;             Emacs Lisp:
;;;                internal-symbol-type-name
;;;                primitive-procedure-type-name
;;;                structure-type-name
;;;             In addition, there may be difficulty printing out types with
;;;             the following strings:
;;;                environment-name
;;;                foreign-procedure-type-name

(require 'scheme)
(require 'ilisp)
(setq ilisp-modes (cons 'scheme-mode ilisp-modes))

(defvar stalin:*db* nil)
(defvar stalin:*xs* '())
(defvar stalin:*i* 0)

(defun stalin:lookup (x y z)
 (catch 'return
   (progn (while z
	   (if (= (nth y (car z)) x) (throw 'return (car z)))
	   (setq z (cdr z)))
	  nil)))

(defun stalin:lookup-expression (x) (stalin:lookup x 5 (nth 0 stalin:*db*)))
(defun stalin:lookup-type (x) (stalin:lookup x 22 (nth 1 stalin:*db*)))
(defun stalin:lookup-type-set (x) (stalin:lookup x 1 (nth 2 stalin:*db*)))
(defun stalin:lookup-variable (x) (stalin:lookup x 4 (nth 3 stalin:*db*)))
(defun stalin:lookup-environment (x) (stalin:lookup x 0 (nth 4 stalin:*db*)))

(defun stalin:expression-kind (x) (nth 0 (stalin:lookup-expression x)))
(defun stalin:expression-pathname (x) (nth 1 (stalin:lookup-expression x)))
(defun stalin:expression-line-position (x)
 (nth 2 (stalin:lookup-expression x)))
(defun stalin:expression-character-position (x)
 (nth 3 (stalin:lookup-expression x)))
(defun stalin:expression-character-position-within-line (x)
 (nth 4 (stalin:lookup-expression x)))
(defun stalin:expression-environment (x) (nth 6 (stalin:lookup-expression x)))
(defun stalin:expression-type-set (x) (nth 7 (stalin:lookup-expression x)))
(defun stalin:expression-parent (x) (nth 8 (stalin:lookup-expression x)))
(defun stalin:expression-lambda-environment (x)
 (nth 9 (stalin:lookup-expression x)))
(defun stalin:expression-parameters (x) (nth 10 (stalin:lookup-expression x)))
(defun stalin:expression-expression (x) (nth 11 (stalin:lookup-expression x)))
(defun stalin:expression-variable (x) (nth 12 (stalin:lookup-expression x)))
(defun stalin:expression-source (x) (nth 13 (stalin:lookup-expression x)))
(defun stalin:expression-antecedent (x) (nth 14 (stalin:lookup-expression x)))
(defun stalin:expression-consequent (x) (nth 15 (stalin:lookup-expression x)))
(defun stalin:expression-alternate (x) (nth 16 (stalin:lookup-expression x)))
(defun stalin:expression-callee (x) (nth 17 (stalin:lookup-expression x)))
(defun stalin:expression-arguments (x) (nth 18 (stalin:lookup-expression x)))
(defun stalin:expression-type-allocation-alist (x)
 (nth 19 (stalin:lookup-expression x)))
(defun stalin:type-kind (u) (nth 0 (stalin:lookup-type u)))
(defun stalin:internal-symbol-type-name (u) (nth 1 (stalin:lookup-type u)))
(defun stalin:external-symbol-type-displaced-string-type (u)
 (nth 2 (stalin:lookup-type u)))
(defun stalin:primitive-procedure-type-name (u) (nth 3 (stalin:lookup-type u)))
(defun stalin:primitive-procedure-type-arguments (u)
 (nth 4 (stalin:lookup-type u)))
(defun stalin:native-procedure-type-call-site-environment-alist (u)
 (nth 5 (stalin:lookup-type u)))
(defun stalin:foreign-procedure-type-name (u) (nth 6 (stalin:lookup-type u)))
(defun stalin:foreign-procedure-type-parameters (u)
 (nth 7 (stalin:lookup-type u)))
(defun stalin:foreign-procedure-type-result (u) (nth 8 (stalin:lookup-type u)))
(defun stalin:foreign-procedure-type-calledp (u)
 (eq (nth 9 (stalin:lookup-type u)) 'T))
(defun stalin:continuation-type-allocating-expression (u)
 (nth 10 (stalin:lookup-type u)))
(defun stalin:continuation-type-calledp (u)
 (eq (nth 11 (stalin:lookup-type u)) 'T))
(defun stalin:string-type-allocating-expressions (u)
 (nth 12 (stalin:lookup-type u)))
(defun stalin:structure-type-name (u) (nth 13 (stalin:lookup-type u)))
(defun stalin:structure-type-slots (u) (nth 14 (stalin:lookup-type u)))
(defun stalin:structure-type-immediatep (u)
 (eq (nth 15 (stalin:lookup-type u)) 'T))
(defun stalin:structure-type-allocating-expressions (u)
 (nth 16 (stalin:lookup-type u)))
(defun stalin:headed-vector-type-element (u) (nth 17 (stalin:lookup-type u)))
(defun stalin:headed-vector-type-allocating-expressions (u)
 (nth 18 (stalin:lookup-type u)))
(defun stalin:nonheaded-vector-type-element (u)
 (nth 19 (stalin:lookup-type u)))
(defun stalin:nonheaded-vector-type-allocating-expressions (u)
 (nth 20 (stalin:lookup-type u)))
(defun stalin:displaced-vector-type-displaced-vector-type (u)
 (nth 21 (stalin:lookup-type u)))
(defun stalin:type-use-count (u) (nth 23 (stalin:lookup-type u)))
(defun stalin:type-fictitiousp (u) (eq (nth 24 (stalin:lookup-type u)) 'T))
(defun stalin:type-set-members (w) (nth 0 (stalin:lookup-type-set w)))
(defun stalin:type-set-fictitiousp (w)
 (eq (nth 2 (stalin:lookup-type-set w)) 'T))
(defun stalin:voidp (w) (eq (nth 3 (stalin:lookup-type-set w)) 'T))
(defun stalin:monomorphicp (w) (eq (nth 4 (stalin:lookup-type-set w)) 'T))
(defun stalin:multimorphicp (w) (eq (nth 5 (stalin:lookup-type-set w)) 'T))
(defun stalin:tag-onlyp (w) (eq (nth 6 (stalin:lookup-type-set w)) 'T))
(defun stalin:has-unionp (w) (eq (nth 7 (stalin:lookup-type-set w)) 'T))
(defun stalin:squeezedp (w) (eq (nth 8 (stalin:lookup-type-set w)) 'T))
(defun stalin:squishedp (w) (eq (nth 9 (stalin:lookup-type-set w)) 'T))
(defun stalin:variable-pathname (g) (nth 0 (stalin:lookup-variable g)))
(defun stalin:variable-line-position (g) (nth 1 (stalin:lookup-variable g)))
(defun stalin:variable-character-position (g)
 (nth 2 (stalin:lookup-variable g)))
(defun stalin:variable-character-position-within-line (g)
 (nth 3 (stalin:lookup-variable g)))
(defun stalin:variable-name (g) (nth 5 (stalin:lookup-variable g)))
(defun stalin:variable-environment (g) (nth 6 (stalin:lookup-variable g)))
(defun stalin:accessedp (g) (eq (nth 7 (stalin:lookup-variable g)) 'T))
(defun stalin:variable-type-set (g) (nth 8 (stalin:lookup-variable g)))
(defun stalin:accesses (g) (nth 9 (stalin:lookup-variable g)))
(defun stalin:assignments (g) (nth 10 (stalin:lookup-variable g)))
(defun stalin:references (g) (nth 11 (stalin:lookup-variable g)))
(defun stalin:localp (g) (eq (nth 12 (stalin:lookup-variable g)) 'T))
(defun stalin:globalp (g) (eq (nth 13 (stalin:lookup-variable g)) 'T))
(defun stalin:hiddenp (g) (eq (nth 14 (stalin:lookup-variable g)) 'T))
(defun stalin:slottedp (g) (eq (nth 15 (stalin:lookup-variable g)) 'T))
(defun stalin:environment-expression (e) (nth 1 (stalin:lookup-environment e)))
(defun stalin:environment-name (e) (nth 2 (stalin:lookup-environment e)))
(defun stalin:environment-has-regionp (e)
 (eq (nth 3 (stalin:lookup-environment e)) 'T))
(defun stalin:environment-call-sites (e) (nth 4 (stalin:lookup-environment e)))
(defun stalin:environment-allocation (e) (nth 5 (stalin:lookup-environment e)))
(defun stalin:environment-reentrantp (e)
 (eq (nth 6 (stalin:lookup-environment e)) 'T))
(defun stalin:environment-called-more-than-oncep (e)
 (eq (nth 7 (stalin:lookup-environment e)) 'T))
(defun stalin:environment-has-environment-levelp (e)
 (eq (nth 8 (stalin:lookup-environment e)) 'T))
(defun stalin:environment-unique-call-site (e)
 (nth 9 (stalin:lookup-environment e)))

(defun stalin:findable-references (g)
 (let ((xs1 (stalin:references g))
       (xs2 '()))
  (while xs1
   (cond
    ((stalin:lookup-expression (car xs1)) (setq xs2 (cons (car xs1) xs2))))
   (setq xs1 (cdr xs1)))
  (reverse xs2)))

(defun stalin:some-can-be-native-procedurep (us e)
 (and (not (null us))
      (or (and (eq (stalin:type-kind (car us)) 'NATIVE-PROCEDURE-TYPE)
	       (member e (stalin:environments (car us))))
	  (stalin:some-can-be-native-procedurep (cdr us) e))))

(defun stalin:can-be-native-procedurep (w e)
 (stalin:some-can-be-native-procedurep (stalin:type-set-members w) e))

(defun stalin:position (i is)
 (let ((n 0))
  (while (not (= (car is) i)) (setq is (cdr is)) (setq n (+ n 1)))
  n))

(defun stalin:findable-drivers (g)
 (let* ((xs1 (stalin:references g))
	(xs2 '())
	(e (stalin:variable-environment g))
	;; needs work
	;;(n (stalin:position g (siblings g)))
	(n 0)
	(xs3 (nth 0 stalin:*db*)))
  (while xs1
   (cond ((and (eq (stalin:expression-kind (car xs1)) 'SET!)
	       (stalin:lookup-expression (car xs1)))
	  (setq xs2 (cons (car xs1) xs2))))
   (setq xs1 (cdr xs1)))
  (while xs3
   (let ((x (nth 5 (car xs3))))
    (if (and (eq (stalin:expression-kind x) 'CALL)
	     ;; needs work
	     ;;(stalin:can-be-native-procedurep (stalin:expression-callee-type x) e)
	     ;; needs work: To deal with &REST arguments
	     ;; needs work
	     ;;(= (length (stalin:expression-arguments x)) (length (siblings g)))
	     (stalin:lookup-expression
	      (nth n (stalin:expression-arguments x))))
	(setq xs2 (cons (nth n (stalin:expression-arguments x)) xs2))))
   (setq xs3 (cdr xs3)))
  (reverse xs2)))

(defun stalin:copy-string (s s1 i)
 (aset s1 i (aref s i))
 (if (< i (- (length s1) 1)) (stalin:copy-string s s1 (+ i 1)) s1))

(defun stalin:remove-extension (s)
 (stalin:copy-string s (make-string (- (length s) 3) 0) 0))

(defun stalin:munge-expressions (l buffer)
 (while (not (null l))
  (setcar (cdr (cdr (cdr (car l))))
	  (set-marker (make-marker)
		      (if (car (cdr (cdr (cdr (car l)))))
			  (+ (car (cdr (cdr (cdr (car l))))) 1)
			  nil)
		      buffer))
  (setq l (cdr l))))

(defun stalin:munge-variables (l buffer)
 (while (not (null l))
  (setcar (cdr (cdr (car l)))
	  (set-marker (make-marker)
		      (if (car (cdr (cdr (car l))))
			  (+ (car (cdr (cdr (car l)))) 1)
			  nil)
		      buffer))
  (setq l (cdr l))))

(defun stalin:load-database ()
 (setq stalin:*db* nil)
 (setq stalin:*xs* '())
 (setq stalin:*i* 0)
 (cond
  ((eq major-mode 'scheme-mode)
   (let ((pathname
          (concat (stalin:remove-extension (buffer-file-name)) ".db"))
         (buffer (current-buffer)))
    (cond
     ((file-exists-p pathname)
      (save-excursion
       (find-file pathname)
       (setq stalin:*db* (read (current-buffer)))
       (kill-buffer (current-buffer))
       (stalin:munge-expressions (nth 0 stalin:*db*) buffer)
       (stalin:munge-variables (nth 3 stalin:*db*) buffer))))))))

(defun stalin:find ()
 (catch 'return
   (let ((l (nth 0 stalin:*db*)))
    (while (not (null l))
     (if (and (marker-position (car (cdr (cdr (cdr (car l))))))
	      (marker-buffer (car (cdr (cdr (cdr (car l))))))
	      (= (point) (car (cdr (cdr (cdr (car l)))))))
	 (throw 'return (list 'expression (nth 5 (car l)))))
     (setq l (cdr l)))
    (setq l (nth 3 stalin:*db*))
    (while (not (null l))
     (if (and (marker-position (car (cdr (cdr (car l)))))
	      (marker-buffer (car (cdr (cdr (car l)))))
	      (= (point) (car (cdr (cdr (car l))))))
	 (throw 'return (list 'variable (nth 4 (car l)))))
     (setq l (cdr l)))
    (save-excursion
     (backward-sexp)
     (setq l (nth 0 stalin:*db*))
     (while (not (null l))
      (if (and (marker-position (car (cdr (cdr (cdr (car l))))))
	       (marker-buffer (car (cdr (cdr (cdr (car l))))))
	       (= (point) (car (cdr (cdr (cdr (car l)))))))
	  (throw 'return (list 'expression (nth 5 (car l)))))
      (setq l (cdr l)))
     (setq l (nth 3 stalin:*db*))
     (while (not (null l))
      (if (and (marker-position (car (cdr (cdr (car l)))))
	       (marker-buffer (car (cdr (cdr (car l)))))
	       (= (point) (car (cdr (cdr (car l))))))
	  (throw 'return (list 'variable (nth 4 (car l)))))
      (setq l (cdr l))))
    nil)))

(defun stalin:environments (u)
 (mapcar (lambda (y-e) (cdr y-e))
         (stalin:native-procedure-type-call-site-environment-alist u)))

(defun stalin:null-typep (u) (eq (stalin:type-kind u) 'NULL-TYPE))

(defun stalin:pair-typep (u)
 (and (eq (stalin:type-kind u) 'STRUCTURE-TYPE)
      (equal (stalin:structure-type-name u) "PAIR")))

(defun stalin:pair-type-car (u) (car (stalin:structure-type-slots u)))

(defun stalin:pair-type-cdr (u) (car (cdr (stalin:structure-type-slots u))))

(defun stalin:list+-typep (u)
 (and
  (stalin:pair-typep u)
  (= (length (stalin:type-set-members (stalin:pair-type-cdr u))) 2)
  (or (and (stalin:null-typep
	    (car (stalin:type-set-members (stalin:pair-type-cdr u))))
	   (= (car (cdr (stalin:type-set-members (stalin:pair-type-cdr u))))
	      u))
      (and (stalin:null-typep
	    (car (cdr (stalin:type-set-members (stalin:pair-type-cdr u)))))
	   (= (car (stalin:type-set-members (stalin:pair-type-cdr u))) u)))))

(defun stalin:list-typep (u)
 (or (stalin:null-typep u)
     (and (stalin:pair-typep u)
	  (stalin:monomorphicp (stalin:pair-type-cdr u))
	  (stalin:list-typep
	   (car (stalin:type-set-members (stalin:pair-type-cdr u)))))))

(defun stalin:list-type-slots (u)
 (if (stalin:null-typep u)
     '()
     (cons (stalin:pair-type-car u)
	   (stalin:list-type-slots
	    (car (stalin:type-set-members (stalin:pair-type-cdr u)))))))

(defun stalin:list*-typep (w)
 (and
  (= (length (stalin:type-set-members w)) 2)
  (or
   (and (stalin:null-typep (car (stalin:type-set-members w)))
	(stalin:pair-typep (car (cdr (stalin:type-set-members w))))
	(= (stalin:pair-type-cdr (car (cdr (stalin:type-set-members w)))) w))
   (and (stalin:null-typep (car (cdr (stalin:type-set-members w))))
	(stalin:pair-typep (car (stalin:type-set-members w)))
	(= (stalin:pair-type-cdr (car (stalin:type-set-members w))) w)))))

(defun stalin:up (u/w u/ws)
 (if (or (equal u/w (car u/ws))
	 (and (eq (car u/w) 'w)
	      (eq (car (car u/ws)) 'u)
	      (stalin:monomorphicp (cdr u/w))
	      (= (car (stalin:type-set-members (cdr u/w))) (cdr (car u/ws)))))
     0
     (+ (stalin:up u/w (cdr u/ws)) 1)))

(defun stalin:the-structure-member (us)
 (if (eq (stalin:type-kind (car us)) 'STRUCTURE-TYPE)
     (car us)
     (stalin:the-structure-member (cdr us))))

(defun stalin:externalize-type-set-internal (w u/ws)
 (cond
  ((or (member (cons 'w w) u/ws)
       (and (stalin:monomorphicp w)
	    (member (cons 'u (car (stalin:type-set-members w))) u/ws)))
   `(UP ,(stalin:up (cons 'w w) u/ws)))
  ((stalin:voidp w) 'VOID)
  ((stalin:monomorphicp w)
   (stalin:externalize-type-internal (car (stalin:type-set-members w)) u/ws))
  ((stalin:list*-typep w)
   ;; note: Ambiguous between list* type and structure type.
   `(LIST* ,(stalin:externalize-type-set-internal
	     (stalin:pair-type-car
	      (stalin:the-structure-member (stalin:type-set-members w)))
	     (cons (cons 'w w) u/ws))))
  (t `(UNION ,@(mapcar
		(lambda (u)
		 (stalin:externalize-type-internal u (cons (cons 'w w) u/ws)))
		(stalin:type-set-members w))))))

(defun stalin:externalize-type-internal (u u/ws)
 (cond
  ((member (cons 'u u) u/ws) `(UP ,(stalin:up (cons 'u u) u/ws)))
  ((eq (stalin:type-kind u) 'NULL-TYPE) 'NULL)
  ((eq (stalin:type-kind u) 'TRUE-TYPE) 'TRUE)
  ((eq (stalin:type-kind u) 'FALSE-TYPE) 'FALSE)
  ((eq (stalin:type-kind u) 'CHAR-TYPE) 'CHAR)
  ((eq (stalin:type-kind u) 'FIXNUM-TYPE) 'FIXNUM)
  ((eq (stalin:type-kind u) 'FLONUM-TYPE) 'FLONUM)
  ((eq (stalin:type-kind u) 'RECTANGULAR-TYPE) 'RECTANGULAR)
  ((eq (stalin:type-kind u) 'INPUT-PORT-TYPE) 'INPUT-PORT)
  ((eq (stalin:type-kind u) 'OUTPUT-PORT-TYPE) 'OUTPUT-PORT)
  ((eq (stalin:type-kind u) 'EOF-OBJECT-TYPE) 'EOF-OBJECT)
  ((eq (stalin:type-kind u) 'POINTER-TYPE) 'POINTER)
  ((eq (stalin:type-kind u) 'INTERNAL-SYMBOL-TYPE)
   ;; note: This can't be `',(stalin:internal-symbol-type-name u) because that
   ;;       would use quote instead of QUOTE.
   `(QUOTE ,(stalin:internal-symbol-type-name u)))
  ((eq (stalin:type-kind u) 'EXTERNAL-SYMBOL-TYPE)
   ;; note: Ambiguous between external symbol type and structure type.
   `(EXTERNAL-SYMBOL
     ,(stalin:externalize-type-internal
       (stalin:external-symbol-type-displaced-string-type u)
       (cons (cons 'u u) u/ws))))
  ((eq (stalin:type-kind u) 'PRIMITIVE-PROCEDURE-TYPE)
   (if (null (stalin:primitive-procedure-type-arguments u))
       (stalin:primitive-procedure-type-name u)
       (cons (stalin:primitive-procedure-type-name u)
	     (stalin:primitive-procedure-type-arguments u))))
  ((eq (stalin:type-kind u) 'NATIVE-PROCEDURE-TYPE)
   `(NATIVE-PROCEDURE
     ,@(mapcar (lambda (e) (stalin:environment-name e))
	       (stalin:environments u))))
  ((eq (stalin:type-kind u) 'FOREIGN-PROCEDURE-TYPE)
   (stalin:foreign-procedure-type-name u))
  ((eq (stalin:type-kind u) 'CONTINUATION-TYPE)
   `(CONTINUATION
     ,(if (eq (stalin:continuation-type-allocating-expression u) 'NIL)
	  'TOP-LEVEL
	  (stalin:continuation-type-allocating-expression u))))
  ;; note: Ambiguous between string type and primitive-procedure type.
  ((eq (stalin:type-kind u) 'STRING-TYPE) 'STRING)
  ((eq (stalin:type-kind u) 'STRUCTURE-TYPE)
   (cond
    ((stalin:list+-typep u)
     ;; note: Ambiguous between list+ type and structure type.
     `(LIST+ ,(stalin:externalize-type-set-internal
	       (stalin:pair-type-car u) (cons (cons 'u u) u/ws))))
    ((stalin:list-typep u)
     ;; note: Ambiguous between list type and structure type.
     `(LIST
       ,@(mapcar
	  (lambda (w)
	    (stalin:externalize-type-set-internal w (cons (cons 'u u) u/ws)))
	  (stalin:list-type-slots u))))
    (t `(,(stalin:structure-type-name u)
	 ,@(mapcar
	    (lambda (w)
	      (stalin:externalize-type-set-internal w (cons (cons 'u u) u/ws)))
	    (stalin:structure-type-slots u))))))
  ((eq (stalin:type-kind u) 'HEADED-VECTOR-TYPE)
   ;; note: Ambiguous between headed-vector type and structure type.
   `(HEADED-VECTOR
     ,(stalin:externalize-type-set-internal
       (stalin:headed-vector-type-element u) (cons (cons 'u u) u/ws))))
  ((eq (stalin:type-kind u) 'NONHEADED-VECTOR-TYPE)
   ;; note: Ambiguous between nonheaded-vector type and structure type.
   `(NONHEADED-VECTOR
     ,(stalin:externalize-type-set-internal
       (stalin:nonheaded-vector-type-element u)
       (cons (cons 'u u) u/ws))))
  ((eq (stalin:type-kind u) 'DISPLACED-VECTOR-TYPE)
   ;; note: Ambiguous between displaced-vector type and structure type.
   `(DISPLACED-VECTOR
     ,(stalin:externalize-type-internal
       (stalin:displaced-vector-type-displaced-vector-type u)
       (cons (cons 'u u) u/ws))))
  (t (error "fuck up"))))

(defun stalin:externalize-type-set (w)
 (stalin:externalize-type-set-internal w '()))

(defun stalin:show-type-set (w)
 (ilisp-display-output-adaptively
  (with-output-to-string (pp (stalin:externalize-type-set w)))
  ilisp-arglist-output))

(defun stalin:goto-variable (g)
 (let ((p (stalin:variable-character-position g)))
  (if (or (not p) (not (marker-position p)) (not (marker-buffer p)))
      (error "Can't find variable"))
  (goto-char p)))

(defun stalin:goto-expression (x)
 (let ((p (stalin:expression-character-position x)))
  (if (or (not p) (not (marker-position p)) (not (marker-buffer p)))
      (error "Can't find expression"))
  (goto-char p)))

(defun stalin:next (i is)
 (let ((is (member i (append is (list 'nil)))))
  (if (null is) (error "fuck up"))
  (car (cdr is))))

(defun stalin:previous (i is)
 (let ((is (member i (reverse (append is (list 'nil))))))
  (if (null is) (error "fuck up"))
  (car (cdr is))))

(defun stalin:expression-allocation-environments (u-es)
 (if (null u-es)
     ""
     (concat
      (cond ((numberp (cdr (car u-es)))
	     (format "allocates on %s, "
		     (stalin:environment-name (cdr (car u-es)))))
	    ((eq (cdr (car u-es)) 'STACK) "allocates on the stack, ")
	    ((eq (cdr (car u-es)) 'HEAP) "allocates on the heap, ")
	    (t (error "fuck up")))
      (stalin:expression-allocation-environments (cdr u-es)))))

(defun stalin:expression-info (x)
 (concat (stalin:expression-allocation-environments
	  (stalin:expression-type-allocation-alist x))
	 "x" (number-to-string x) ", "
	 "w" (number-to-string (stalin:expression-type-set x))))

(defun stalin:type-info (u)
 ;; needs work: There is currently no command that displays this info:
 ;;               foreign-procedure-type-calledp
 ;;               continuation-type-calledp
 ;;               structure-type-immediate
 ;;               allocating-expressions
 ;;               type-use-count
 (concat (if (stalin:type-fictitiousp u) "fictitious, " "")
	 "u" (number-to-string u)))

(defun stalin:type-set-info (w)
 ;; needs work: There is currently no command that displays this info:
 ;;               type-set-has-unionp
 ;;               type-set-multimorphic
 (concat (cond ((stalin:voidp w) "void, ")
	       ((stalin:type-set-fictitiousp w) "fictitious, ")
	       ((stalin:monomorphicp w) "monomorphic, ")
	       ((stalin:tag-onlyp w) "tag only, ")
	       ((stalin:squeezedp w) "squeezed, ")
	       ((stalin:squishedp w) "squished, ")
	       (t "general case, "))
	 "w" (number-to-string w)))

(defun stalin:variable-info (g)
 (concat (if (stalin:accessedp g) "accessed, " "")
	 (if (stalin:localp g) "local, " "")
	 (if (stalin:globalp g) "global, " "")
	 (if (stalin:hiddenp g) "hidden, " "")
	 (if (stalin:slottedp g) "slotted, " "")
	 (cond ((stalin:localp g) "a")
	       ((stalin:globalp g) "a")
	       ((stalin:slottedp g) "a")
	       (t ""))
	 (cond ((stalin:localp g) (number-to-string g))
	       ((stalin:globalp g) (number-to-string g))
	       ((stalin:slottedp g) (number-to-string g))
	       (t ""))
	 (cond ((stalin:localp g) ", ")
	       ((stalin:globalp g) ", ")
	       ((stalin:slottedp g) ", ")
	       (t ""))
	 "w" (number-to-string (stalin:variable-type-set g))))

(defun stalin:environment-info (e)
 (concat
  (if (stalin:environment-has-regionp e) "has region, " "")
  (cond
   ((numberp (stalin:environment-allocation e))
    (format "allocates on %s, "
	    (stalin:environment-name (stalin:environment-allocation e))))
   ((eq (stalin:environment-allocation e) 'STACK) "allocates on the stack, ")
   ((eq (stalin:environment-allocation e) 'HEAP) "allocates on the heap, ")
   (t ""))
  (if (stalin:environment-reentrantp e) "reentrant, " "")
  (if (stalin:environment-has-environment-levelp e)
      "has environment level, "
      "")
  (if (eq (stalin:environment-unique-call-site e) 'NIL) "" "in lined, ")
  "f" (number-to-string e)))

(defun stalin:goto-variable-or-reference ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (not x/g) (error "Point not on expression or variable"))
  (if (eq (car x/g) 'expression)
      (let* ((x (car (cdr x/g)))
	     (k (stalin:expression-kind x)))
       (if (not (or (eq k 'ACCESS) (eq k 'SET!)))
	   (error "Point not on access or assignment"))
       (if (not (stalin:lookup-variable (stalin:expression-variable x)))
	   (error "Can't find variable"))
       (stalin:goto-variable (stalin:expression-variable x)))
      (let* ((g (car (cdr x/g)))
	     (xs (stalin:findable-references g)))
       (if (null xs) (error "No findable references"))
       (stalin:goto-expression (car xs))))))

(defun stalin:goto-next-reference ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (not x/g) (error "Point not on expression or variable"))
  (if (eq (car x/g) 'expression)
      (let* ((x (car (cdr x/g)))
	     (k (stalin:expression-kind x)))
       (if (not (or (eq k 'ACCESS) (eq k 'SET!)))
	   (error "Point not on access or assignment"))
       (let ((x (stalin:next
		 x
		 (stalin:findable-references (stalin:expression-variable x)))))
	(if (not x) (error "Last reference"))
	(stalin:goto-expression x)))
      (let* ((g (car (cdr x/g)))
	     (xs (stalin:findable-references g)))
       (if (null xs) (error "No findable references"))
       (stalin:goto-expression (car xs))))))

(defun stalin:goto-previous-reference ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (not x/g) (error "Point not on expression or variable"))
  (if (eq (car x/g) 'expression)
      (let* ((x (car (cdr x/g)))
	     (k (stalin:expression-kind x)))
       (if (not (or (eq k 'ACCESS) (eq k 'SET!)))
	   (error "Point not on access or assignment"))
       (let ((x (stalin:previous
		 x
		 (stalin:findable-references (stalin:expression-variable x)))))
	(if (not x) (error "First reference"))
	(stalin:goto-expression x)))
      (let* ((g (car (cdr x/g)))
	     (xs (stalin:findable-references g)))
       (if (null xs) (error "No findable references"))
       (stalin:goto-expression (car xs))))))

(defun stalin:display-type ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (not x/g) (error "Point not on expression or variable"))
  (let ((w (if (eq (car x/g) 'expression)
	       (stalin:expression-type-set (car (cdr x/g)))
	       (stalin:variable-type-set (car (cdr x/g))))))
   (stalin:show-type-set w))))

(defun stalin:display-info ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (not x/g) (error "Point not on expression or variable"))
  (if (eq (car x/g) 'expression)
      (message (stalin:expression-info (car (cdr x/g))))
      (message (stalin:variable-info (car (cdr x/g)))))))

(defun stalin:display-type-set-info ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (not x/g) (error "Point not on expression or variable"))
  (let ((w (if (eq (car x/g) 'expression)
	       (stalin:expression-type-set (car (cdr x/g)))
	       (stalin:variable-type-set (car (cdr x/g))))))
   (message (stalin:type-set-info w)))))

(defun stalin:display-environment-info ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (not x/g) (error "Point not on expression or variable"))
  (let ((w (if (eq (car x/g) 'expression)
	       (stalin:expression-type-set (car (cdr x/g)))
	       (stalin:variable-type-set (car (cdr x/g))))))
   (if (and (= (length (stalin:type-set-members w)) 1)
	    (eq (stalin:type-kind (car (stalin:type-set-members w)))
		'NATIVE-PROCEDURE-TYPE))
       (message (stalin:environment-info
		 ;; needs work: Currently only displays info for one of the
		 ;;             polyvariant environments.
		 (car (stalin:environments
		       (car (stalin:type-set-members w))))))
       (error "Not a native procedure")))))

(defun stalin:goto-first-warning ()
 (interactive)
 (if (null (nth 5 stalin:*db*)) (error "No warnings"))
 (setq stalin:*i* 0)
 (stalin:goto-expression (car (nth stalin:*i* (nth 5 stalin:*db*))))
 (message (car (cdr (cdr (nth stalin:*i* (nth 5 stalin:*db*)))))))

(defun stalin:goto-next-warning ()
 (interactive)
 (if (null (nth 5 stalin:*db*)) (error "No warnings"))
 (if (>= stalin:*i* (- (length (nth 5 stalin:*db*)) 1)) (error "Last warning"))
 (setq stalin:*i* (+ stalin:*i* 1))
 (stalin:goto-expression (car (nth stalin:*i* (nth 5 stalin:*db*))))
 (message (car (cdr (cdr (nth stalin:*i* (nth 5 stalin:*db*)))))))

(defun stalin:goto-previous-warning ()
 (interactive)
 (if (null (nth 5 stalin:*db*)) (error "No warnings"))
 (if (<= stalin:*i* 0) (error "First warning"))
 (setq stalin:*i* (- stalin:*i* 1))
 (stalin:goto-expression (car (nth stalin:*i* (nth 5 stalin:*db*))))
 (message (car (cdr (cdr (nth stalin:*i* (nth 5 stalin:*db*)))))))

(defun stalin:goto-first-driver ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (not x/g) (error "Point not on expression or variable"))
  (if (eq (car x/g) 'expression)
      (let* ((x (car (cdr x/g)))
	     (k (stalin:expression-kind x)))
       (if (not (or (eq k 'ACCESS) (eq k 'SET!)))
	   (error "Point not on access or assignment"))
       (if (not (stalin:lookup-variable (stalin:expression-variable x)))
	   (error "Can't find variable"))
       (stalin:goto-variable (stalin:expression-variable x))
       (stalin:goto-first-driver))
      (let* ((g (car (cdr x/g)))
	     (xs (stalin:findable-drivers g)))
       (if (null xs) (error "No findable drivers"))
       (setq stalin:*xs* xs)
       (let ((x (car xs)))
	(stalin:goto-expression x)
	(stalin:show-type-set
	 (if (eq (stalin:expression-kind x) 'SET!)
	     (stalin:variable-type-set (stalin:expression-variable x))
	     (stalin:expression-type-set x))))))))

(defun stalin:goto-next-driver ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (or (not x/g)
	  (not (eq (car x/g) 'expression))
	  (not (member (car (cdr x/g)) stalin:*xs*)))
      (stalin:goto-first-driver)
      (let ((x (stalin:next (car (cdr x/g)) stalin:*xs*)))
       (if (not x) (error "Last driver"))
       (stalin:goto-expression x)
       (stalin:show-type-set (stalin:expression-type-set
			      (if (eq (stalin:expression-kind x) 'SET!)
				  (stalin:expression-source x)
				  x)))))))

(defun stalin:goto-previous-driver ()
 (interactive)
 (let ((x/g (stalin:find)))
  (if (or (not x/g)
	  (not (eq (car x/g) 'expression))
	  (not (member (car (cdr x/g)) stalin:*xs*)))
      (stalin:goto-first-driver)
      (let ((x (stalin:previous (car (cdr x/g)) stalin:*xs*)))
       (if (not x) (error "First driver"))
       (stalin:goto-expression x)
       (stalin:show-type-set (stalin:expression-type-set
			      (if (eq (stalin:expression-kind x) 'SET!)
				  (stalin:expression-source x)
				  x)))))))

(add-hook 'find-file-hooks 'stalin:load-database t)

(define-key scheme-mode-map "\M-V" 'stalin:goto-variable-or-reference)
(define-key scheme-mode-map "\M-N" 'stalin:goto-next-reference)
(define-key scheme-mode-map "\M-P" 'stalin:goto-previous-reference)
(define-key scheme-mode-map "\M-T" 'stalin:display-type)
(define-key scheme-mode-map "\M-I" 'stalin:display-info)
(define-key scheme-mode-map "\M-Q" 'stalin:display-type-set-info)
(define-key scheme-mode-map "\M-M" 'stalin:display-environment-info)
(define-key scheme-mode-map "\M-W" 'stalin:goto-first-warning)
(define-key scheme-mode-map "\M-F" 'stalin:goto-next-warning)
(define-key scheme-mode-map "\M-B" 'stalin:goto-previous-warning)
(define-key scheme-mode-map "\M-D" 'stalin:goto-first-driver)
(define-key scheme-mode-map "\M-A" 'stalin:goto-next-driver)
(define-key scheme-mode-map "\M-E" 'stalin:goto-previous-driver)

;;; Tam V'Nishlam Shevah L'El Borei Olam
