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
(module stalin5d)

(include "QobiScheme.sch")
(include "stalin5d.sch")
;;; End delete for Trotsky

(define *Scheme->C-compatibility-macros*
 (list
  (list 'when
	(lambda (s)
	 (unless (>= (sx-length s) 2) (syntax-error s "Improper WHEN"))
	 `(if ,(sx-second s) (begin ,@(sx-unlist (sx-rest (sx-rest s)))))))
  (list 'unless
	(lambda (s)
	 (unless (>= (sx-length s) 2) (syntax-error s "Improper UNLESS"))
	 `(if ,(sx-second s)
	      ((lambda ()))
	      (begin ,@(sx-unlist (sx-rest (sx-rest s)))))))))

(define (valid-Xlib-and-GL-type? type)
 ;; conventions: TYPE
 (let ((type (unencapsulate type)))
  ;; conventions: TYPE
  (or (equal? type 'pointer)
      (equal? type 'void)
      (equal? type '(pointer void))
      (equal? type '(pointer (pointer void)))
      (equal? type 'char)
      (equal? type '(pointer char))
      (equal? type '(pointer (pointer char)))
      (equal? type '(pointer (pointer (pointer char))))
      (equal? type 'unsigned-char)
      (equal? type '(pointer unsigned-char))
      (equal? type '(pointer (pointer unsigned-char)))
      (equal? type '(pointer (pointer (pointer unsigned-char))))
      (equal? type 'signed-char)
      (equal? type '(pointer signed-char))
      (equal? type 'string)
      (equal? type 'short)
      (equal? type '(pointer short))
      (equal? type 'unsigned-short)
      (equal? type '(pointer unsigned-short))
      (equal? type 'int)
      (equal? type '(pointer int))
      (equal? type 'unsigned-int)
      (equal? type '(pointer unsigned-int))
      (equal? type 'long)
      (equal? type 'unsigned-long)
      (equal? type '(pointer unsigned-long))
      (equal? type '(pointer (pointer unsigned-long)))
      (equal? type 'float)
      (equal? type '(pointer float))
      (equal? type 'double)
      (equal? type '(pointer double))
      (equal? type '(pointer union))
      (equal? type '(pointer struct))
      (equal? type '(pointer (struct "GLUquadricObj")))
      (equal? type '(pointer (struct "GLUnurbsObj")))
      (equal? type '(pointer (struct "GLUtriangulatorObj")))
      (equal? type '(pointer (pointer struct)))
      (equal? type '(pointer (pointer (pointer struct))))
      (equal? type '(pointer function)))))

(define (translate-Xlib-and-GL-type type)
 ;; conventions: TYPE
 (let ((type (unencapsulate type)))
  ;; conventions: TYPE
  (cond ((equal? type 'pointer) 'void*)	;needs work
	((equal? type 'void) 'void)
	((equal? type '(pointer void)) 'void*)
	((equal? type '(pointer (pointer void))) 'void*)
	((equal? type 'char) 'char)
	((equal? type '(pointer char)) 'void*)
	((equal? type '(pointer (pointer char))) 'void*)
	((equal? type '(pointer (pointer (pointer char)))) 'void*)
	((equal? type 'unsigned-char) 'unsigned-char)
	((equal? type '(pointer unsigned-char)) 'void*)
	((equal? type '(pointer (pointer unsigned-char))) 'void*)
	((equal? type '(pointer (pointer (pointer unsigned-char)))) 'void*)
	((equal? type 'signed-char) 'signed-char)
	((equal? type '(pointer signed-char)) 'void*)
	((equal? type 'string) 'char*)
	((equal? type 'short) 'short)
	((equal? type '(pointer short)) 'void*)
	((equal? type 'unsigned-short) 'unsigned-short)
	((equal? type '(pointer unsigned-short)) 'void*)
	((equal? type 'int) 'int)
	((equal? type '(pointer int)) 'void*)
	((equal? type 'unsigned-int) 'unsigned)
	((equal? type '(pointer unsigned-int)) 'void*)
	((equal? type 'long) 'long)
	((equal? type 'unsigned-long) 'unsigned-long)
	((equal? type '(pointer unsigned-long)) 'void*)
	((equal? type '(pointer (pointer unsigned-long))) 'void*)
	((equal? type 'float) 'float)
	((equal? type '(pointer float)) 'void*)
	((equal? type 'double) 'double)
	((equal? type '(pointer double)) 'void*)
	((equal? type '(pointer union)) 'void*)
	((equal? type '(pointer struct)) 'void*)
	((equal? type '(pointer (struct "GLUquadricObj"))) 'void*)
	((equal? type '(pointer (struct "GLUnurbsObj"))) 'void*)
	((equal? type '(pointer (struct "GLUtriangulatorObj"))) 'void*)
	((equal? type '(pointer (pointer struct))) 'void*)
	((equal? type '(pointer (pointer (pointer struct)))) 'void*)
	((equal? type '(pointer function)) 'void*)
	(else (fuck-up)))))

(define *Xlib-and-GL-macros*
 (list (list 'foreign-function
	     (lambda (s)
	      (unless (and (= (sx-length s) 5)
			   (sx-symbol? (sx-second s))
			   (sx-list? (sx-third s))
			   (sx-every valid-Xlib-and-GL-type? (sx-third s))
			   (valid-Xlib-and-GL-type? (sx-fourth s))
			   (sx-string? (sx-fifth s)))
	       (syntax-error s "Improper FOREIGN-FUNCTION"))
	      `(define ,(sx-second s)
		(foreign-procedure
		 ,(sx-map translate-Xlib-and-GL-type (sx-third s))
		 ,(translate-Xlib-and-GL-type (sx-fourth s))
		 ,(sx-fifth s)))))
       (list 'foreign-define
	     (lambda (s)
	      (unless (and (= (sx-length s) 3) (sx-symbol? (sx-second s)))
	       (syntax-error s "Improper FOREIGN-DEFINE"))
	      `(define ,(sx-second s) ,(sx-third s))))
       (list 'in-package
	     (lambda (s)
	      (unless (and (= (sx-length s) 2) (sx-symbol? (sx-second s)))
	       (syntax-error s "Improper IN-PACKAGE"))
	      `(begin)))))

;;; Derived from the t21oct97 archive of QobiScheme, updated to the
;;; m2feb98, r5feb98, m7dec98, m24jan00, f10mar00, h22apr00, f5may00,
;;; m12jun00, and m25jun01 archives.

(define *QobiScheme-macros*
 (list
  (list 'define-structure
	(lambda (s)
	 (unless (and (>= (sx-length s) 3) (sx-every sx-symbol? (sx-rest s)))
	  (syntax-error s "Improper DEFINE-STRUCTURE"))
	 (let ((type (sx-datum (sx-second s)))
	       (slots (sx-unlist (sx-rest (sx-rest s)))))
	  ;; conventions: TYPE SLOTS
	  `(begin
	    (define (,(string->symbol
		       (string-append "MAKE-" (symbol->string type)))
		     ,@(map sx-datum slots))
	     ((primitive-procedure make-structure ,type ,(length slots))
	      ,@(map sx-datum slots)))
	    (define (,(string->symbol
		       (string-append (symbol->string type) "?"))
		     obj)
	     ((primitive-procedure structure? ,type) obj))
	    ,@(map-indexed
	       (lambda (slot i)
		;; conventions: SLOT I
		(let ((slot (sx-datum slot)))
		 ;; conventions: SLOT
		 `(begin
		   (define (,(string->symbol
			      (string-append (symbol->string type)
					     "-"
					     (symbol->string slot)))
			    s)
		    ((primitive-procedure structure-ref ,type ,i) s))
		   (define (,(string->symbol
			      (string-append "SET-"
					     (symbol->string type)
					     "-"
					     (symbol->string slot)
					     "!"))
			    s x)
		    ((primitive-procedure structure-set! ,type ,i) s x))
		   (define (,(string->symbol
			      (string-append "LOCAL-SET-"
					     (symbol->string type)
					     "-"
					     (symbol->string slot)
					     "!"))
			    s x)
		    (let ((p ((primitive-procedure structure-ref ,type ,i) s)))
		     ;; conventions: P
		     (upon-failure
		      ((primitive-procedure structure-set! ,type ,i) s p)))
		    ((primitive-procedure structure-set! ,type ,i) s x)))))
	       slots)
;;; Begin delete for Trotsky
	    (define ,(string->uninterned-symbol "ignore")
	     (define-write-method
	      ,(string->symbol (string-append (symbol->string type) "?"))
	      (lambda (s port)
	       (display "#S(" port)
	       (display ,(symbol->string type) port)
	       ,@(reduce
		  append
		  (map (lambda (slot)
			`((display " " port)
			  (write
			   (,(string->symbol
			      (string-append (symbol->string type)
					     "-"
					     (symbol->string (sx-datum slot))))
			    s)
			   port)))
		       slots)
		  '())
	       (display ")" port))))
	    (define ,(string->uninterned-symbol "ignore")
	     (define-display-method
	      ,(string->symbol (string-append (symbol->string type) "?"))
	      (lambda (s port)
	       (display "#S(" port)
	       (display ,(symbol->string type) port)
	       ,@(reduce
		  append
		  (map (lambda (slot)
			`((display " " port)
			  (display
			   (,(string->symbol
			      (string-append (symbol->string type)
					     "-"
					     (symbol->string (sx-datum slot))))
			    s)
			   port)))
		       slots)
		  '())
	       (display ")" port))))
;;; End delete for Trotsky
	    ))))
  (list 'while
	(lambda (s)
	 (let ((loop (gensym "loop")))	;changed
	  ;; conventions: LOOP
	  `(begin (define (,loop)
		   (when ,(sx-second s)
		    ,@(sx-unlist (sx-rest (sx-rest s))) (,loop)))
		  (,loop)))))
  (list 'either
	(lambda (s)
	 (cond ((sx-null? (sx-rest s)) '(fail))
	       ((sx-null? (sx-rest (sx-rest s))) (sx-second s))
	       (else `(if (a-boolean)
			  ,(sx-second s)
			  (either ,@(sx-unlist (sx-rest (sx-rest s)))))))))
  (list 'for-effects
	(lambda (s)
	 (let ((return (gensym "return")) ;changed
	       (old-fail (gensym "old-fail"))) ;changed
	  ;; conventions: RETURN OLD-FAIL
	  `(call-with-current-continuation
	    (lambda (,return)
	     (let ((,old-fail fail))
	      (set! fail (lambda () (set! fail ,old-fail) (,return #f)))
	      (begin ,@(sx-unlist (sx-rest s)))
	      (fail)))))))
  (list 'one-value
	(lambda (s)
	 (unless (or (= (sx-length s) 2) (= (sx-length s) 3))
	  (syntax-error s "Improper ONE-VALUE"))
	 (let ((s1 (sx-second s))
	       (s2 (if (= (sx-length s) 2) '(fail) (sx-third s)))
	       (return (gensym "return")) ;changed
	       (old-fail (gensym "old-fail"))) ;changed
	  ;; conventions: S1 S2 RETURN OLD-FAIL
	  `(call-with-current-continuation
	    (lambda (,return)
	     (let ((,old-fail fail))
	      (set! fail (lambda () (set! fail ,old-fail) (,return ,s2)))
	      (let ((v ,s1))
	       (set! fail ,old-fail)
	       v)))))))			;changed
  (list 'local-one-value
	;; needs work: *FAIL?* can potentially be captured.
	(lambda (s)
	 (unless (or (= (sx-length s) 2) (= (sx-length s) 3))
	  (syntax-error s "Improper LOCAL-ONE-VALUE"))
	 (let ((s1 (sx-second s))
	       (s2 (if (= (sx-length s) 2) '(fail) (sx-third s)))
	       (return (gensym "return")) ;changed
	       (old-fail (gensym "old-fail")) ;changed
	       (v (gensym "v")))	;changed
	  ;; conventions: S1 S2 RETURN OLD-FAIL V
	  `(call-with-current-continuation
	    (lambda (,return)
	     (let ((,v #f)
		   (,old-fail fail))
	      (set!
	       fail
	       (lambda ()
		(set! fail ,old-fail)
		(,return (cond (*fail?* ,s2) (else (set! *fail?* #t) ,v)))))
	      (set! ,v ,s1)
	      (set! *fail?* #f)
	      (fail)))))))
  (list 'all-values
	;; needs work: To eliminate REVERSE.
	(lambda (s)
	 (let ((values (gensym "values"))) ;changed
	  ;; conventions: VALUEs
	  `(let ((,values '()))
	    (for-effects
	     (set! ,values (cons (begin ,@(sx-unlist (sx-rest s))) ,values)))
	    (reverse ,values)))))
  (list 'possibly?
	(lambda (s)
	 (let ((return (gensym "return")) ;changed
	       (old-fail (gensym "old-fail")) ;changed
	       (v (gensym "v")))	;changed
	  ;; conventions: RETURN OLD-FAIL V
	  `(call-with-current-continuation
	    (lambda (,return)
	     (let ((,old-fail fail))
	      (set! fail (lambda () (set! fail ,old-fail) (,return #f)))
	      (let ((,v (begin ,@(sx-unlist (sx-rest s)))))
	       (unless ,v (fail))
	       (set! fail ,old-fail)
	       ,v)))))))		;changed
  (list 'necessarily?
	(lambda (s)
	 (let ((return (gensym "return")) ;changed
	       (old-fail (gensym "old-fail")) ;changed
	       (v (gensym "v"))		;changed
	       (u (gensym "u")))	;changed
	  ;; conventions: RETURN OLD-FAIL V U
	  `(call-with-current-continuation
	    (lambda (,return)
	     (let ((,old-fail fail)
		   (,u #t))
	      (set! fail (lambda () (set! fail ,old-fail) (,return ,u)))
	      (let ((,v (begin ,@(sx-unlist (sx-rest s)))))
	       (when ,v (set! ,u ,v) (fail))
	       (set! fail ,old-fail)
	       #f)))))))		;changed
  (list 'upon-failure
	(lambda (s)
	 (let ((old-fail (gensym "old-fail"))) ;changed
	  ;; conventions: OLD-FAIL
	  `(let ((,old-fail fail))
	    (set! fail (lambda ()
			(set! fail ,old-fail)
			,@(sx-unlist (sx-rest s))
			(fail)))))))
  (list 'local-set!
	(lambda (s)
	 (unless (= (sx-length s) 3) (syntax-error s "Improper LOCAL-SET!"))
	 (let ((p (gensym "p")))	;changed
	  ;; conventions: P
	  `(begin
	    (let ((,p ,(sx-second s))) (upon-failure (set! ,(sx-second s) ,p)))
	    (set! ,(sx-second s) ,(sx-third s))))))
  (list 'lazy
	(lambda (s)
	 (let ((args (gensym "args")))	;changed
	  ;; conventions: LAZY
	  `(lambda ,args (apply ,(sx-second s) ,args)))))
  (list 'define-toggle-button
	(lambda (s)
	 (unless (and (= (sx-length s) 6) (sx-symbol? (sx-fifth s)))
	  (syntax-error s "Improper DEFINE-TOGGLE-BUTTON"))
	 `(define-button ,(sx-second s) ,(sx-third s) ,(sx-fourth s)
	   (lambda () ,(sx-fifth s))
	   (lambda ()
	    (set! ,(sx-fifth s) (not ,(sx-fifth s)))
	    (redraw-buttons)
	    (,(sx-sixth s))))))
  (list 'define-radio-buttons
	(lambda (s)
	 (unless (and (>= (sx-length s) 3)
		      (sx-symbol? (sx-second s))
		      (sx-every (lambda (element)
				 (and (sx-list? element)
				      (= (sx-length element) 4)
				      (sx-symbol? (sx-third element))))
				(sx-rest (sx-rest (sx-rest s)))))
	  (syntax-error s "Improper DEFINE-RADIO-BUTTONS"))
	 `(begin
	   ,@(sx-map (lambda (element)
		      `(define-button ,(sx-first element) ,(sx-second element)
			,(sx-fourth element)
			(lambda () (eq? ,(sx-second s) ',(sx-third element)))
			(lambda ()
			 (set! ,(sx-second s) ',(sx-third element))
			 (redraw-buttons)
			 (,(sx-third s)))))
		     (sx-rest (sx-rest (sx-rest s)))))))
  (list 'define-cycle-button
	(lambda (s)
	 (unless (and (>= (sx-length s) 6)
		      (sx-symbol? (sx-fourth s))
		      (sx-every
		       (lambda (element)
			(and (sx-list? element)
			     (= (sx-length element) 2)
			     (sx-symbol? (sx-first element))))
		       (sx-rest (sx-rest (sx-rest (sx-rest (sx-rest s)))))))
	  (syntax-error s "Improper DEFINE-CYCLE-BUTTON"))
	 (let ((symbols
		(sx-map sx-first
			(sx-rest (sx-rest (sx-rest (sx-rest (sx-rest s))))))))
	  `(define-button ,(sx-second s) ,(sx-third s)
	    (lambda ()
	     (case ,(sx-fourth s)
	      ,@(sx-map (lambda (element)
			 `((,(sx-first element)) ,(sx-second element)))
			(sx-rest (sx-rest (sx-rest (sx-rest (sx-rest s))))))
	      (else (fuck-up))))
	    #f
	    (lambda ()
	     (set! ,(sx-fourth s)
		   (case ,(sx-fourth s)
		    ,@(map (lambda (s1 s2) `((,s1) (set! ,(sx-fourth s) ',s2)))
			   symbols
			   (append (rest symbols) (list (first symbols))))
		    (else (fuck-up))))
	     (redraw-buttons)
	     (,(sx-fifth s)))))))
  (list 'define-integer-range-buttons
	(lambda (s)
	 (unless (and (= (sx-length s) 11) (sx-symbol? (sx-sixth s)))
	  (syntax-error s "Improper DEFINE-INTEGER-RANGE-BUTTONS"))
	 `(begin (define-button ,(sx-second s) ,(sx-third s) ,(sx-ninth s)
		  #f
		  (lambda ()
		   (when (= ,(sx-sixth s) ,(sx-seventh s)) (abort))
		   (set! ,(sx-sixth s) (- ,(sx-sixth s) 1))
		   (redraw-buttons)
		   (,(sx-eleventh s))))
		 (define-button ,(sx-fourth s) ,(sx-fifth s) ,(sx-tenth s)
		  #f
		  (lambda ()
		   (when (= ,(sx-sixth s) ,(sx-eighth s)) (abort))
		   (set! ,(sx-sixth s) (+ ,(sx-sixth s) 1))
		   (redraw-buttons)
		   (,(sx-eleventh s)))))))
  (list 'define-display-pane-application
	;; (DEFINE-DISPLAY-PANE-APPLICATION
	;;  NAME
	;;  DISPLAY-PANE-WIDTH
	;;  DISPLAY-PANE-HEIGHT
	;;  PRE-INITIALIZE-PROCEDURE
	;;  POST-INITIALIZE-PROCEDURE
	;;  FINALIZE-PROCEDURE
	;;  REDRAW-PROCEDURE)
	(lambda (s)
	 `(define (,(sx-second s) arguments)
	   (let* ((stalin? #t)
		  (display-pane-width ,(sx-third s))
		  (display-pane-height ,(sx-fourth s))
		  (pre-initialize-procedure ,(sx-fifth s))
		  (post-initialize-procedure ,(sx-sixth s))
		  (finalize-procedure ,(sx-seventh s))
		  (redraw-procedure ,(sx-eighth s)))
	    (set! *post-initialize-procedure* post-initialize-procedure)
	    (set! *transcript-pane* #f)
	    (set! *echo-pane* #f)
	    (set! *status-pane* #f)
	    (set! *message-pane* #f)
	    (set! *display* (xopendisplay *display-name*))
	    ;; changed: NULL-POINTER?, FORMAT
	    (when (zero? *display*)
	     (panic (format #f "Cannot connect to X server: ~a"
			    (xdisplayname *display-name*))))
	    (set! *screen* (xdefaultscreen *display*))
	    (set! *root-window* (xrootwindow *display* *screen*))
	    (set! *button-width* 0)
	    (set! *button-height* 0)
	    (cond
	     (stalin?
	      (set! *white-pixel* (xwhitepixel *display* *screen*))
	      (set! *black-pixel* (xblackpixel *display* *screen*)))
	     (else
	      (set! *background*
		    (xallocnamedcolor *display*
				      (xdefaultcolormap *display* *screen*)
				      *background-color*))
	      (unless (= (first *background*) 1)
	       (panic "Can't allocate background colorcell"))
	      (set! *foreground*
		    (xallocnamedcolor *display*
				      (xdefaultcolormap *display* *screen*)
				      *foreground-color*))
	      (unless (= (first *foreground*) 1)
	       (panic "Can't allocate foreground colorcell"))))
	    (set! *roman-font* (xloadqueryfont *display* *roman-font-name*))
	    ;; changed: NULL-POINTER?, FORMAT
	    (when (zero? *roman-font*)
	     (panic (format #f "Cannot open font: ~a" *roman-font-name*)))
	    (set! *bold-font* (xloadqueryfont *display* *bold-font-name*))
	    ;; changed: NULL-POINTER?, FORMAT
	    (when (zero? *bold-font*)
	     (panic (format #f "Cannot open font: ~a" *bold-font-name*)))
	    (set! *roman-height*
		  (+ (xfontstruct-ascent *roman-font*)
		     (xfontstruct-descent *roman-font*)))
	    (set! *bold-height*
		  (+ (xfontstruct-ascent *bold-font*)
		     (xfontstruct-descent *bold-font*)))
	    (set! *text-height*
		  (+ (max (xfontstruct-ascent *roman-font*)
			  (xfontstruct-ascent *bold-font*))
		     (max (xfontstruct-descent *roman-font*)
			  (xfontstruct-descent *bold-font*))))
	    (set! *roman-baseline* (xfontstruct-descent *roman-font*))
	    (set! *bold-baseline* (xfontstruct-descent *bold-font*))
	    (set! *text-baseline* (max *roman-baseline* *bold-baseline*))
	    (set! *display-pane-width* display-pane-width)
	    (set! *display-pane-height* display-pane-height)
	    (set! *who-line-height* 0)
	    (set! *window*
		  (xcreatesimplewindow
		   *display* *root-window*
		   *window-position-x* *window-position-y*
		   *display-pane-width* *display-pane-height*
		   1
		   (if stalin?
		       *black-pixel*
		       (xcolor-pixel (second *foreground*)))
		   (if stalin?
		       *white-pixel*
		       (xcolor-pixel (second *background*)))))
	    (xstorename *display* *window* *program*)
	    (xseticonname *display* *window* *program*)
	    (set! *display-pane* *window*)
	    (xselectinput *display*
			  *display-pane*
			  (bit-or exposuremask
				  pointermotionmask
				  buttonpressmask
				  buttonreleasemask
				  keypressmask))
	    (set! *thin-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *thin-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *thin-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *thin-gc* 0 linesolid capround joinround)
	    (set! *thin-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *thin-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *thin-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetlineattributes
	     *display* *thin-flipping-gc* 0 linesolid capround joinround)
	    (xsetfunction *display* *thin-flipping-gc* gxxor)
	    (set! *medium-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *medium-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *medium-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *medium-gc* 2 linesolid capround joinround)
	    (set! *medium-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *medium-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *medium-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetlineattributes
	     *display* *medium-flipping-gc* 2 linesolid capround joinround)
	    (xsetfunction *display* *medium-flipping-gc* gxxor)
	    (set! *thick-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *thick-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *thick-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *thick-gc* 5 linesolid capround joinround)
	    (set! *thick-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *thick-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *thick-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetlineattributes
	     *display* *thick-flipping-gc* 5 linesolid capround joinround)
	    (xsetfunction *display* *thick-flipping-gc* gxxor)
	    (set! *dashed-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *dashed-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *dashed-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *dashed-gc* 0 lineonoffdash capround joinround)
	    (set! *dashed-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *dashed-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *dashed-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetlineattributes
	     *display* *dashed-flipping-gc* 0 lineonoffdash capround joinround)
	    (xsetfunction *display* *dashed-flipping-gc* gxxor)
	    (set! *roman-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *roman-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *roman-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetfont
	     *display* *roman-gc* (xfontstruct-fid *roman-font*))
	    (set! *bold-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *bold-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *bold-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetfont
	     *display* *bold-gc* (xfontstruct-fid *bold-font*))
	    (set! *bold-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *bold-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *bold-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetfont
	     *display* *bold-flipping-gc* (xfontstruct-fid *bold-font*))
	    (xsetlineattributes
	     *display* *bold-flipping-gc* 0 linesolid capround joinround)
	    (xsetfunction *display* *bold-flipping-gc* gxxor)
	    (unless stalin?
	     (set! *light-gray*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Light Gray"))
	     (unless (= (first *light-gray*) 1)
	      (panic "Can't allocate light gray colorcell"))
	     (set! *light-gray-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *light-gray-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *light-gray-gc*
			     (xcolor-pixel (second *light-gray*)))
	     (xsetlineattributes
	      *display* *light-gray-gc* 0 linesolid capround joinround)
	     (set! *gray*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Gray"))
	     (unless (= (first *gray*) 1)
	      (panic "Can't allocate gray colorcell"))
	     (set! *gray-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *gray-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *gray-gc*
			     (xcolor-pixel (second *gray*)))
	     (xsetlineattributes
	      *display* *gray-gc* 0 linesolid capround joinround)
	     (set! *red*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Red"))
	     (unless (= (first *red*) 1)
	      (panic "Can't allocate red colorcell"))
	     (set! *red-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *red-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *red-gc*
			     (xcolor-pixel (second *red*)))
	     (xsetfont
	      *display* *red-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *red-gc* 0 linesolid capround joinround)
	     (set! *dark-red*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Dark Red"))
	     (unless (= (first *dark-red*) 1)
	      (panic "Can't allocate dark red colorcell"))
	     (set! *dark-red-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *dark-red-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *dark-red-gc*
			     (xcolor-pixel (second *dark-red*)))
	     (xsetfont
	      *display* *dark-red-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *dark-red-gc* 0 linesolid capround joinround)
	     (set! *green*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Green"))
	     (unless (= (first *green*) 1)
	      (panic "Can't allocate green colorcell"))
	     (set! *green-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *green-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *green-gc*
			     (xcolor-pixel (second *green*)))
	     (xsetfont
	      *display* *green-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *green-gc* 0 linesolid capround joinround)
	     (set! *dark-green*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Dark Green"))
	     (unless (= (first *dark-green*) 1)
	      (panic "Can't allocate dark green colorcell"))
	     (set! *dark-green-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *dark-green-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *dark-green-gc*
			     (xcolor-pixel (second *dark-green*)))
	     (xsetfont
	      *display* *dark-green-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *dark-green-gc* 0 linesolid capround joinround)
	     (set! *blue*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Blue"))
	     (unless (= (first *blue*) 1)
	      (panic "Can't allocate blue colorcell"))
	     (set! *blue-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *blue-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *blue-gc*
			     (xcolor-pixel (second *blue*)))
	     (xsetfont
	      *display* *blue-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *blue-gc* 0 linesolid capround joinround)
	     (set! *yellow*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Yellow"))
	     (unless (= (first *yellow*) 1)
	      (panic "Can't allocate yellow colorcell"))
	     (set! *yellow-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *yellow-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *yellow-gc*
			     (xcolor-pixel (second *yellow*)))
	     (xsetfont
	      *display* *yellow-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *yellow-gc* 0 linesolid capround joinround)
	     (set! *violet*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Violet"))
	     (unless (= (first *violet*) 1)
	      (panic "Can't allocate violet colorcell"))
	     (set! *violet-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *violet-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *violet-gc*
			     (xcolor-pixel (second *violet*)))
	     (xsetfont
	      *display* *violet-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *violet-gc* 0 linesolid capround joinround)
	     (set! *orange*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Orange"))
	     (unless (= (first *orange*) 1)
	      (panic "Can't allocate orange colorcell"))
	     (set! *orange-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *orange-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *orange-gc*
			     (xcolor-pixel (second *orange*)))
	     (xsetfont
	      *display* *orange-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *orange-gc* 0 linesolid capround joinround)
	     (set! *dark-orange*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Dark Orange"))
	     (unless (= (first *dark-orange*) 1)
	      (panic "Can't allocate dark orange colorcell"))
	     (set! *dark-orange-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *dark-orange-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *dark-orange-gc*
			     (xcolor-pixel (second *dark-orange*)))
	     (xsetfont
	      *display* *dark-orange-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *dark-orange-gc* 0 linesolid capround joinround))
	    (set! *color-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *color-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *color-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *color-gc* 0 linesolid capround joinround)
	    (set! *window-methods* '())
	    (set! *abort-button* #f)
	    (set! *abort-key* #f)
	    (set! *comtab* (make-vector 256 #f))
	    (set! *help* '())
	    (define-key (control #\h) "Help" help-command)
	    (set! *help* '())
	    (define-key (control #\n) "Scroll help window down one line"
	     help-scroll-down-line-command)
	    (define-key (control #\p) "Scroll help window up one line"
	     help-scroll-up-line-command)
	    (define-key (control #\v) "Scroll help window down one page"
	     help-scroll-down-page-command)
	    (define-key (meta #\v) "Scroll help window up one page"
	     help-scroll-up-page-command)
	    (define-key (meta #\<) "Scroll help window to beginning"
	     help-scroll-beginning-command)
	    (define-key (meta #\>) "Scroll help window to end"
	     help-scroll-end-command)
	    (set! *help-comtab* *comtab*)
	    (set! *comtab* (make-vector 256 #f))
	    (set! *prefix* '())
	    (set! *status* "Tyi")
	    (set! *message* "")
	    (set! *redraw-procedure* redraw-procedure)
	    (set! *buttons* '())
	    (set! *pause?* #f)
	    (set! *help?* #f)
	    (set! *clear-display-pane?* #t)
	    (let ((hints (make-xwmhints)))
	     (set-xwmhints-input! hints 1) ;changed
	     (set-xwmhints-flags! hints inputhint) ;changed
	     (xsetwmhints *display* *window* hints))
	    (let ((hints (make-xsizehints)))
	     (when *window-position?*
	      (set-xsizehints-x! hints *window-position-x*) ;changed
	      (set-xsizehints-y! hints *window-position-y*)) ;changed
	     (set-xsizehints-min_width! hints *display-pane-width*) ;changed
	     (set-xsizehints-max_width! hints *display-pane-width*) ;changed
	     (set-xsizehints-min_height! hints *display-pane-height*) ;changed
	     (set-xsizehints-max_height! hints *display-pane-height*) ;changed
	     (set-xsizehints-flags!
	      hints
	      (if *window-position?*
		  (+ usposition pposition pminsize pmaxsize)
		  (+ pminsize pmaxsize)))
	     (xsetwmnormalhints *display* *window* hints))
	    (pre-initialize-procedure)
	    (set-window-method! *display-pane* 'expose redraw-display-pane)
	    (set-window-method! *display-pane* 'buttonpress region-handler)
	    (when *transcript-pane*
	     (set-window-method!
	      *transcript-pane* 'expose redraw-transcript-pane))
	    (when *echo-pane*
	     (set-window-method! *echo-pane* 'expose redraw-echo-pane))
	    (set!			;changed
	     kill-application
	     (lambda ()
	      (set! kill-application (lambda () #t))
	      (finalize-procedure)
	      (when *display*
	       (xfreegc *display* *thin-gc*)
	       (xfreegc *display* *thin-flipping-gc*)
	       (xfreegc *display* *medium-gc*)
	       (xfreegc *display* *medium-flipping-gc*)
	       (xfreegc *display* *thick-gc*)
	       (xfreegc *display* *thick-flipping-gc*)
	       (xfreegc *display* *dashed-gc*)
	       (xfreegc *display* *dashed-flipping-gc*)
	       (xfreegc *display* *roman-gc*)
	       (xfreegc *display* *bold-gc*)
	       (xfreegc *display* *bold-flipping-gc*)
	       (unless stalin?
		(xfreegc *display* *light-gray-gc*)
		(xfreegc *display* *gray-gc*)
		(xfreegc *display* *red-gc*)
		(xfreegc *display* *dark-red-gc*)
		(xfreegc *display* *green-gc*)
		(xfreegc *display* *dark-green-gc*)
		(xfreegc *display* *blue-gc*)
		(xfreegc *display* *yellow-gc*)
		(xfreegc *display* *violet-gc*)
		(xfreegc *display* *orange-gc*)
		(xfreegc *display* *dark-orange-gc*)
		(xfreegc *display* *color-gc*)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *background*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *foreground*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *light-gray*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *gray*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *red*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *dark-red*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *green*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *dark-green*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *blue*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *yellow*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *violet*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *orange*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *dark-orange*))))
			     1
			     0))
	       (xunloadfont *display* (xfontstruct-fid *roman-font*))
	       (xunloadfont *display* (xfontstruct-fid *bold-font*))
	       (xdestroywindow *display* *window*)
	       (xclosedisplay *display*)
	       (set! *display* #f))
	      #t))
	    (xmapsubwindows *display* *window*)
	    (xmapraised *display* *window*)
	    (process-events)
	    (kill-application)))))
  (list 'define-application
	;; (DEFINE-APPLICATION
        ;;  NAME
	;;  DISPLAY-PANE-WIDTH
	;;  DISPLAY-PANE-HEIGHT
	;;  TRANSCRIPT-LINES
	;;  BUTTON-ROWS
	;;  BUTTOM-COLUMNS
	;;  PRE-INITIALIZE-PROCEDURE
	;;  POST-INITIALIZE-PROCEDURE
	;;  FINALIZE-PROCEDURE
	;;  REDRAW-PROCEDURE
	;;  LISTENER-PROCEDURE)
	(lambda (s)
	 `(define (,(sx-second s) arguments)
	   (let* ((stalin? #t)
		  (display-pane-width ,(sx-third s))
		  (display-pane-height ,(sx-fourth s))
		  (transcript-lines ,(sx-fifth s))
		  (button-rows ,(sx-sixth s))
		  (button-columns ,(sx-seventh s))
		  (button-width
		   (if display-pane-width
		       (- (quotient (+ display-pane-width 4) button-columns)
			  4)
		       100))
		  (width (if display-pane-width
			     (+ display-pane-width 6)
			     (+ (* button-columns (+ button-width 4)) 2)))
		  (pre-initialize-procedure ,(sx-eighth s))
		  (post-initialize-procedure ,(sx-ninth s))
		  (finalize-procedure ,(sx-tenth s))
		  (redraw-procedure ,(sx-eleventh s))
		  (listener-procedure
		   ,(if (= (sx-length s) 12) (sx-twelfth s) '(lambda () #f))))
	    (set! *post-initialize-procedure* post-initialize-procedure)
	    (set! *transcript-pane* #f)
	    (set! *echo-pane* #f)
	    (set! *display* (xopendisplay *display-name*))
	    ;; changed: NULL-POINTER?, FORMAT
	    (when (zero? *display*)
	     (panic (format #f "Cannot connect to X server: ~a"
			    (xdisplayname *display-name*))))
	    (set! *screen* (xdefaultscreen *display*))
	    (set! *root-window* (xrootwindow *display* *screen*))
	    (cond
	     (stalin?
	      (set! *white-pixel* (xwhitepixel *display* *screen*))
	      (set! *black-pixel* (xblackpixel *display* *screen*)))
	     (else
	      (set! *background*
		    (xallocnamedcolor *display*
				      (xdefaultcolormap *display* *screen*)
				      *background-color*))
	      (unless (= (first *background*) 1)
	       (panic "Can't allocate background colorcell"))
	      (set! *foreground*
		    (xallocnamedcolor *display*
				      (xdefaultcolormap *display* *screen*)
				      *foreground-color*))
	      (unless (= (first *foreground*) 1)
	       (panic "Can't allocate foreground colorcell"))))
	    (set! *roman-font* (xloadqueryfont *display* *roman-font-name*))
	    ;; changed: NULL-POINTER?, FORMAT
	    (when (zero? *roman-font*)
	     (panic (format #f "Cannot open font: ~a" *roman-font-name*)))
	    (set! *bold-font* (xloadqueryfont *display* *bold-font-name*))
	    ;; changed: NULL-POINTER?, FORMAT
	    (when (zero? *bold-font*)
	     (panic (format #f "Cannot open font: ~a" *bold-font-name*)))
	    (set! *roman-height*
		  (+ (xfontstruct-ascent *roman-font*)
		     (xfontstruct-descent *roman-font*)))
	    (set! *bold-height*
		  (+ (xfontstruct-ascent *bold-font*)
		     (xfontstruct-descent *bold-font*)))
	    (set! *text-height*
		  (+ (max (xfontstruct-ascent *roman-font*)
			  (xfontstruct-ascent *bold-font*))
		     (max (xfontstruct-descent *roman-font*)
			  (xfontstruct-descent *bold-font*))))
	    (set! *roman-baseline* (xfontstruct-descent *roman-font*))
	    (set! *bold-baseline* (xfontstruct-descent *bold-font*))
	    (set! *text-baseline* (max *roman-baseline* *bold-baseline*))
	    (set! *button-width* button-width)
	    (set! *button-height* (+ *text-height* 4))
	    (set! *display-pane-width* (- width 6))
	    (set! *display-pane-height* display-pane-height)
	    (when transcript-lines
	     (unless (zero? transcript-lines)
	      (set! *transcript-pane-height*
		    (+ (* transcript-lines *text-height*) 4)))
	     (set! *echo-pane-height* (+ *text-height* 4)))
	    (set! *who-line-height* (+ *text-height* 4))
	    (set! *status-pane-width*
		  (+ (max (xtextwidth *roman-font* "Tyi" 3)
			  (xtextwidth *roman-font* "Run" 3)
			  (xtextwidth *roman-font* "Pause" 5)
			  (xtextwidth *roman-font* "Track" 5))
		     4))
	    (set! *window*
		  (xcreatesimplewindow
		   *display* *root-window*
		   *window-position-x* *window-position-y*
		   width
		   (if transcript-lines
		       (if (zero? transcript-lines)
			   (+ (* button-rows (+ *button-height* 4))
			      *display-pane-height*
			      *echo-pane-height*
			      *who-line-height*
			      14)
			   (+ (* button-rows (+ *button-height* 4))
			      *display-pane-height*
			      *transcript-pane-height*
			      *echo-pane-height*
			      *who-line-height*
			      18))
		       (+ (* button-rows (+ *button-height* 4))
			  *display-pane-height*
			  *who-line-height*
			  10))
		   1
		   (if stalin?
		       *black-pixel*
		       (xcolor-pixel (second *foreground*)))
		   (if stalin?
		       *white-pixel*
		       (xcolor-pixel (second *background*)))))
	    (xstorename *display* *window* *program*)
	    (xseticonname *display* *window* *program*)
	    (xselectinput *display*
			  *window*
			  (bit-or exposuremask
				  pointermotionmask
				  buttonpressmask
				  buttonreleasemask
				  keypressmask))
	    (set! *display-pane*
		  (xcreatesimplewindow
		   *display* *window*
		   2 (+ (* button-rows (+ *button-height* 4)) 2)
		   *display-pane-width* *display-pane-height*
		   1
		   (if stalin?
		       *black-pixel*
		       (xcolor-pixel (second *foreground*)))
		   (if stalin?
		       *white-pixel*
		       (xcolor-pixel (second *background*)))))
	    (xselectinput *display*
			  *display-pane*
			  (bit-or exposuremask
				  pointermotionmask
				  buttonpressmask
				  buttonreleasemask
				  keypressmask))
	    (when transcript-lines
	     (unless (zero? transcript-lines)
	      (set! *transcript-pane*
		    (xcreatesimplewindow
		     *display* *window*
		     2
		     (+ (* button-rows (+ *button-height* 4))
			*display-pane-height*
			6)
		     *display-pane-width* *transcript-pane-height* 1
		     (if stalin?
			 *black-pixel*
			 (xcolor-pixel (second *foreground*)))
		     (if stalin?
			 *white-pixel*
			 (xcolor-pixel (second *background*)))))
	      (xselectinput
	       *display* *transcript-pane* (bit-or exposuremask keypressmask)))
	     (set! *echo-pane*
		   (xcreatesimplewindow
		    *display* *window*
		    2
		    (if (zero? transcript-lines)
			(+ (* button-rows (+ *button-height* 4))
			   *display-pane-height*
			   6)
			(+ (* button-rows (+ *button-height* 4))
			   *display-pane-height* *transcript-pane-height* 10))
		    *display-pane-width* *echo-pane-height* 1
		    (if stalin?
			*black-pixel*
			(xcolor-pixel (second *foreground*)))
		    (if stalin?
			*white-pixel*
			(xcolor-pixel (second *background*)))))
	     (xselectinput
	      *display* *echo-pane* (bit-or exposuremask keypressmask)))
	    (set! *status-pane*
		  (xcreatesimplewindow
		   *display* *window*
		   2
		   (+ (* button-rows (+ *button-height* 4))
		      *display-pane-height*
		      (if transcript-lines
			  (if (zero? transcript-lines)
			      (+ *echo-pane-height* 10)
			      (+ *transcript-pane-height*
				 *echo-pane-height*
				 14))
			  6))
		   *status-pane-width* *who-line-height*
		   1
		   (if stalin?
		       *black-pixel*
		       (xcolor-pixel (second *foreground*)))
		   (if stalin?
		       *white-pixel*
		       (xcolor-pixel (second *background*)))))
	    (xselectinput
	     *display* *status-pane* (bit-or exposuremask keypressmask))
	    (set! *message-pane*
		  (xcreatesimplewindow
		   *display* *window*
		   (+ *status-pane-width* 6)
		   (+ (* button-rows (+ *button-height* 4))
		      *display-pane-height*
		      (if transcript-lines
			  (if (zero? transcript-lines)
			      (+ *echo-pane-height* 10)
			      (+ *transcript-pane-height*
				 *echo-pane-height*
				 14))
			  6))
		   (- width *status-pane-width* 10) *who-line-height*
		   1
		   (if stalin?
		       *black-pixel*
		       (xcolor-pixel (second *foreground*)))
		   (if stalin?
		       *white-pixel*
		       (xcolor-pixel (second *background*)))))
	    (xselectinput
	     *display* *message-pane* (bit-or exposuremask keypressmask))
	    (set! *thin-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *thin-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *thin-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *thin-gc* 0 linesolid capround joinround)
	    (set! *thin-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *thin-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *thin-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetlineattributes
	     *display* *thin-flipping-gc* 0 linesolid capround joinround)
	    (xsetfunction *display* *thin-flipping-gc* gxxor)
	    (set! *medium-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *medium-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *medium-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *medium-gc* 2 linesolid capround joinround)
	    (set! *medium-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *medium-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *medium-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetlineattributes
	     *display* *medium-flipping-gc* 2 linesolid capround joinround)
	    (xsetfunction *display* *medium-flipping-gc* gxxor)
	    (set! *thick-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *thick-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *thick-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *thick-gc* 5 linesolid capround joinround)
	    (set! *thick-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *thick-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *thick-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetlineattributes
	     *display* *thick-flipping-gc* 5 linesolid capround joinround)
	    (xsetfunction *display* *thick-flipping-gc* gxxor)
	    (set! *dashed-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *dashed-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *dashed-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *dashed-gc* 0 lineonoffdash capround joinround)
	    (set! *dashed-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *dashed-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *dashed-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetlineattributes
	     *display* *dashed-flipping-gc* 0 lineonoffdash capround joinround)
	    (xsetfunction *display* *dashed-flipping-gc* gxxor)
	    (set! *roman-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *roman-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *roman-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetfont
	     *display* *roman-gc* (xfontstruct-fid *roman-font*))
	    (set! *bold-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *bold-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *bold-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetfont
	     *display* *bold-gc* (xfontstruct-fid *bold-font*))
	    (set! *bold-flipping-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *bold-flipping-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetforeground *display* *bold-flipping-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetfont
	     *display* *bold-flipping-gc* (xfontstruct-fid *bold-font*))
	    (xsetlineattributes
	     *display* *bold-flipping-gc* 0 linesolid capround joinround)
	    (xsetfunction *display* *bold-flipping-gc* gxxor)
	    (unless stalin?
	     (set! *light-gray*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Light Gray"))
	     (unless (= (first *light-gray*) 1)
	      (panic "Can't allocate light gray colorcell"))
	     (set! *light-gray-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *light-gray-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *light-gray-gc*
			     (xcolor-pixel (second *light-gray*)))
	     (xsetlineattributes
	      *display* *light-gray-gc* 0 linesolid capround joinround)
	     (set! *gray*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Gray"))
	     (unless (= (first *gray*) 1)
	      (panic "Can't allocate gray colorcell"))
	     (set! *gray-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *gray-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *gray-gc*
			     (xcolor-pixel (second *gray*)))
	     (xsetlineattributes
	      *display* *gray-gc* 0 linesolid capround joinround)
	     (set! *red*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Red"))
	     (unless (= (first *red*) 1)
	      (panic "Can't allocate red colorcell"))
	     (set! *red-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *red-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *red-gc*
			     (xcolor-pixel (second *red*)))
	     (xsetfont
	      *display* *red-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *red-gc* 0 linesolid capround joinround)
	     (set! *dark-red*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Dark Red"))
	     (unless (= (first *dark-red*) 1)
	      (panic "Can't allocate dark red colorcell"))
	     (set! *dark-red-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *dark-red-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *dark-red-gc*
			     (xcolor-pixel (second *dark-red*)))
	     (xsetfont
	      *display* *dark-red-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *dark-red-gc* 0 linesolid capround joinround)
	     (set! *green*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Green"))
	     (unless (= (first *green*) 1)
	      (panic "Can't allocate green colorcell"))
	     (set! *green-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *green-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *green-gc*
			     (xcolor-pixel (second *green*)))
	     (xsetfont
	      *display* *green-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *green-gc* 0 linesolid capround joinround)
	     (set! *dark-green*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Dark Green"))
	     (unless (= (first *dark-green*) 1)
	      (panic "Can't allocate dark green colorcell"))
	     (set! *dark-green-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *dark-green-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *dark-green-gc*
			     (xcolor-pixel (second *dark-green*)))
	     (xsetfont
	      *display* *dark-green-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *dark-green-gc* 0 linesolid capround joinround)
	     (set! *blue*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Blue"))
	     (unless (= (first *blue*) 1)
	      (panic "Can't allocate blue colorcell"))
	     (set! *blue-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *blue-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *blue-gc*
			     (xcolor-pixel (second *blue*)))
	     (xsetfont
	      *display* *blue-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *blue-gc* 0 linesolid capround joinround)
	     (set! *yellow*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Yellow"))
	     (unless (= (first *yellow*) 1)
	      (panic "Can't allocate yellow colorcell"))
	     (set! *yellow-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *yellow-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *yellow-gc*
			     (xcolor-pixel (second *yellow*)))
	     (xsetfont
	      *display* *yellow-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *yellow-gc* 0 linesolid capround joinround)
	     (set! *violet*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Violet"))
	     (unless (= (first *violet*) 1)
	      (panic "Can't allocate violet colorcell"))
	     (set! *violet-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *violet-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *violet-gc*
			     (xcolor-pixel (second *violet*)))
	     (xsetfont
	      *display* *violet-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *violet-gc* 0 linesolid capround joinround)
	     (set! *orange*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Orange"))
	     (unless (= (first *orange*) 1)
	      (panic "Can't allocate orange colorcell"))
	     (set! *orange-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *orange-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *orange-gc*
			     (xcolor-pixel (second *orange*)))
	     (xsetfont
	      *display* *orange-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *orange-gc* 0 linesolid capround joinround)
	     (set! *dark-orange*
		   (xallocnamedcolor *display*
				     (xdefaultcolormap *display* *screen*)
				     "Dark Orange"))
	     (unless (= (first *dark-orange*) 1)
	      (panic "Can't allocate dark orange colorcell"))
	     (set! *dark-orange-gc*
		   (xcreategc *display* *window* 0 (make-xgcvalues)))
	     (xsetbackground *display* *dark-orange-gc*
			     (xcolor-pixel (second *background*)))
	     (xsetforeground *display* *dark-orange-gc*
			     (xcolor-pixel (second *dark-orange*)))
	     (xsetfont
	      *display* *dark-orange-gc* (xfontstruct-fid *roman-font*))
	     (xsetlineattributes
	      *display* *dark-orange-gc* 0 linesolid capround joinround))
	    (set! *color-gc*
		  (xcreategc *display* *window* 0 (make-xgcvalues)))
	    (xsetbackground *display* *color-gc*
			    (if stalin?
				*white-pixel*
				(xcolor-pixel (second *background*))))
	    (xsetforeground *display* *color-gc*
			    (if stalin?
				*black-pixel*
				(xcolor-pixel (second *foreground*))))
	    (xsetlineattributes
	     *display* *color-gc* 0 linesolid capround joinround)
	    (set! *window-methods* '())
	    (set! *abort-button* #f)
	    (set! *abort-key* #f)
	    (set! *comtab* (make-vector 256 #f))
	    (set! *help* '())
	    (define-key (control #\h) "Help" help-command)
	    (set! *help* '())
	    (define-key (control #\n) "Scroll help window down one line"
	     help-scroll-down-line-command)
	    (define-key (control #\p) "Scroll help window up one line"
	     help-scroll-up-line-command)
	    (define-key (control #\v) "Scroll help window down one page"
	     help-scroll-down-page-command)
	    (define-key (meta #\v) "Scroll help window up one page"
	     help-scroll-up-page-command)
	    (define-key (meta #\<) "Scroll help window to beginning"
	     help-scroll-beginning-command)
	    (define-key (meta #\>) "Scroll help window to end"
	     help-scroll-end-command)
	    (set! *help-comtab* *comtab*)
	    (set! *comtab* (make-vector 256 #f))
	    (when transcript-lines
	     (set! *transcript* '())
	     (set! *input* "")
	     (set! *input-position* 0)
	     (let ((help *help*))
	      (for-each
	       (lambda (character)
		(define-key character
		 "Enter the typed character into the echo pane"
		 (lambda () (echo-pane-insert-character-command character))))
	       (append
		(string->list
		 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
		(string->list
		 "1234567890-=\\`!@#$%^&*()_+|~[]{};':\",./<>? ")))
	      (set! *help* help))
	     (define-key (control #\a)
	      "Move the cursor to the beginning of the echo pane"
	      echo-pane-beginning-of-line-command)
	     (define-key (control #\b)
	      "Move the cursor backward one character in the echo pane"
	      echo-pane-backward-char-command)
	     (define-key (control #\d)
	      "Delete the character after the cursor in the echo pane"
	      echo-pane-delete-char-command)
	     (define-key (control #\e)
	      "Move the cursor to the end of the echo pane"
	      echo-pane-end-of-line-command)
	     (define-key (control #\f)
	      "Move the cursor forward one character in the echo pane"
	      echo-pane-forward-char-command)
	     (define-key (control #\k)
	      "Delete all characters after the cursor in the echo pane"
	      echo-pane-kill-line-command)
	     (define-key delete
	      "Delete the character before the cursor in the echo pane"
	      echo-pane-backward-delete-char-command)
	     (define-key return
	      "Process the input in the echo pane"
	      (lambda ()
	       (set! *transcript* (cons (list 'user *input*) *transcript*))
	       (listener-procedure)
	       (set! *input* "")
	       (set! *input-position* 0)
	       (redraw-transcript-pane)
	       (redraw-echo-pane)))
	     (define-key (meta #\b)
	      "Move the cursor backward one word in the echo pane"
	      echo-pane-backward-word-command)
	     (define-key (meta #\d)
	      "Delete the word after the cursor in the echo pane"
	      echo-pane-kill-word-command)
	     (define-key (meta #\f)
	      "Move the cursor forward one word in the echo pane"
	      echo-pane-forward-word-command)
	     (define-key (meta delete)
	      "Delete the word before the cursor in the echo pane"
	      echo-pane-backward-kill-word-command))
	    (set! *prefix* '())
	    (set! *status* "Tyi")
	    (set! *message* "")
	    (set! *redraw-procedure* redraw-procedure)
	    (set! *buttons* '())
	    (set! *pause?* #f)
	    (set! *help?* #f)
	    (set! *clear-display-pane?* #t)
	    (let ((hints (make-xwmhints)))
	     (set-xwmhints-input! hints 1) ;changed
	     (set-xwmhints-flags! hints inputhint) ;changed
	     (xsetwmhints *display* *window* hints))
	    (let ((hints (make-xsizehints))
		  (height (if transcript-lines
			      (if (zero? transcript-lines)
				  (+ (* button-rows (+ *button-height* 4))
				     *display-pane-height*
				     *echo-pane-height*
				     *who-line-height*
				     14)
				  (+ (* button-rows (+ *button-height* 4))
				     *display-pane-height*
				     *transcript-pane-height*
				     *echo-pane-height*
				     *who-line-height*
				     18))
			      (+ (* button-rows (+ *button-height* 4))
				 *display-pane-height*
				 *who-line-height*
				 10))))
	     (when *window-position?*
	      (set-xsizehints-x! hints *window-position-x*) ;changed
	      (set-xsizehints-y! hints *window-position-y*)) ;changed
	     (set-xsizehints-min_width! hints width) ;changed
	     (set-xsizehints-max_width! hints width) ;changed
	     (set-xsizehints-min_height! hints height) ;changed
	     (set-xsizehints-max_height! hints height) ;changed
	     (set-xsizehints-flags!
	      hints
	      (if *window-position?*
		  (+ usposition pposition pminsize pmaxsize)
		  (+ pminsize pmaxsize)))
	     (xsetwmnormalhints *display* *window* hints))
	    (pre-initialize-procedure)
	    (set-window-method! *display-pane* 'expose redraw-display-pane)
	    (set-window-method! *display-pane* 'buttonpress region-handler)
	    (when *transcript-pane*
	     (set-window-method!
	      *transcript-pane* 'expose redraw-transcript-pane))
	    (when *echo-pane*
	     (set-window-method! *echo-pane* 'expose redraw-echo-pane))
	    (set-window-method! *status-pane* 'expose redraw-status-pane)
	    (set-window-method! *message-pane* 'expose redraw-message-pane)
	    (set!			;changed
	     kill-application
	     (lambda ()
	      (set! kill-application (lambda () #t))
	      (finalize-procedure)
	      (when *display*
	       (xfreegc *display* *thin-gc*)
	       (xfreegc *display* *thin-flipping-gc*)
	       (xfreegc *display* *medium-gc*)
	       (xfreegc *display* *medium-flipping-gc*)
	       (xfreegc *display* *thick-gc*)
	       (xfreegc *display* *thick-flipping-gc*)
	       (xfreegc *display* *dashed-gc*)
	       (xfreegc *display* *dashed-flipping-gc*)
	       (xfreegc *display* *roman-gc*)
	       (xfreegc *display* *bold-gc*)
	       (xfreegc *display* *bold-flipping-gc*)
	       (unless stalin?
		(xfreegc *display* *light-gray-gc*)
		(xfreegc *display* *gray-gc*)
		(xfreegc *display* *red-gc*)
		(xfreegc *display* *dark-red-gc*)
		(xfreegc *display* *green-gc*)
		(xfreegc *display* *dark-green-gc*)
		(xfreegc *display* *blue-gc*)
		(xfreegc *display* *yellow-gc*)
		(xfreegc *display* *violet-gc*)
		(xfreegc *display* *orange-gc*)
		(xfreegc *display* *dark-orange-gc*)
		(xfreegc *display* *color-gc*)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *background*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *foreground*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *light-gray*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *gray*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *red*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *dark-red*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *green*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *dark-green*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *blue*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *yellow*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *violet*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *orange*))))
			     1
			     0)
		(xfreecolors *display*
			     (xdefaultcolormap *display* *screen*)
			     (unsigned-list->unsigneda
			      (list (xcolor-pixel (second *dark-orange*))))
			     1
			     0))
	       (xunloadfont *display* (xfontstruct-fid *roman-font*))
	       (xunloadfont *display* (xfontstruct-fid *bold-font*))
	       (xdestroywindow *display* *window*)
	       (xclosedisplay *display*)
	       (set! *display* #f))
	      #t))
	    (xmapsubwindows *display* *window*)
	    (xmapraised *display* *window*)
	    (process-events)
	    (kill-application)))))
  (list 'define-command
	(lambda (s)
	 (define (valid-command-arguments? l)
	  (define (valid-optional-parameter? l)
	   (and (sx-list? l)
		(= (sx-length l) 4)
		(sx-symbol? (sx-first l))
		(sx-string? (sx-second l))))
	  (define (valid-required-parameter? l)
	   (and (sx-list? l)
		(= (sx-length l) 3)
		(sx-symbol? (sx-first l))
		(sx-string? (sx-second l))))
	  (define (order-ok-optional? l)
	   (or (sx-null? l)
	       (and (sx-eq? (sx-first (sx-first l)) 'optional)
		    (order-ok-optional? (sx-rest l)))
	       (and (sx-eq? (sx-first (sx-first l)) 'rest)
		    (sx-null? (sx-rest l)))))
	  (define (order-ok-required? l)
	   (or (sx-null? l)
	       (and (sx-eq? (sx-first (sx-first l)) 'required)
		    (order-ok-required? (sx-rest l)))
	       (and (sx-eq? (sx-first (sx-first l)) 'optional)
		    (order-ok-optional? (sx-rest l)))
	       (and (sx-eq? (sx-first (sx-first l)) 'rest)
		    (sx-null? (sx-rest l)))))
	  (define (order-ok? l)
	   (or (sx-null? l)
	       (and (or (sx-eq? (sx-first (sx-first l)) 'any-number)
			(sx-eq? (sx-first (sx-first l)) 'at-least-one)
			(sx-eq? (sx-first (sx-first l)) 'at-most-one)
			(sx-eq? (sx-first (sx-first l)) 'exactly-one))
		    (order-ok? (sx-rest l)))
	       (and (sx-eq? (sx-first (sx-first l)) 'required)
		    (order-ok-required? (sx-rest l)))
	       (and (sx-eq? (sx-first (sx-first l)) 'optional)
		    (order-ok-optional? (sx-rest l)))
	       (and (sx-eq? (sx-first (sx-first l)) 'rest)
		    (sx-null? (sx-rest l)))))
	  (and
	   (sx-list? l)
	   (>= (sx-length l) 1)
	   (sx-symbol? (sx-first l))
	   (sx-every
	    (lambda (l)
	     (and
	      (sx-list? l)
	      (>= (sx-length l) 1)
	      (or (and (or (sx-eq? (sx-first l) 'exactly-one)
			   (sx-eq? (sx-first l) 'at-most-one))
		       (>= (sx-length l) 2)
		       (sx-every
			(lambda (l)
			 (and (sx-list? l)
			      (>= (sx-length l) 2)
			      (sx-string? (sx-first l))
			      (sx-symbol? (sx-second l))
			      (sx-every valid-optional-parameter?
					(sx-rest (sx-rest l)))))
			(sx-rest l)))
		  (and (or (sx-eq? (sx-first l) 'at-least-one)
			   (sx-eq? (sx-first l) 'any-number))
		       (>= (sx-length l) 2)
		       (sx-every
			(lambda (l)
			 (and (sx-list? l)
			      (>= (sx-length l) 2)
			      (sx-string? (sx-first l))
			      (sx-symbol? (sx-second l))
			      (sx-every valid-required-parameter?
					(sx-rest (sx-rest l)))))
			(sx-rest l)))
		  (and (or (sx-eq? (sx-first l) 'required)
			   (sx-eq? (sx-first l) 'rest))
		       (= (sx-length l) 2)
		       (valid-required-parameter? (sx-second l)))
		  (and (sx-eq? (sx-first l) 'optional)
		       (= (sx-length l) 2)
		       (valid-optional-parameter? (sx-second l))))))
	    (sx-rest l))
	   (order-ok? (sx-rest l))))
	 (define (command-usage l)
	  (define (command-usage1 l)
	   (let ((s (let loop ((l l))
		     (define (command-usage l)
		      (string-append
		       "-"
		       (sx-datum (sx-first l))
		       (let loop ((l (sx-rest (sx-rest l))))
			(cond
			 ((sx-null? l) "")
			 ((sx-null? (sx-rest l))
			  (string-append
			   " " (sx-datum (sx-second (sx-first l)))))
			 (else (string-append
				" "
				(sx-datum (sx-second (sx-first l)))
				(loop (sx-rest l))))))))
		     (if (sx-null? (sx-rest l))
			 (command-usage (sx-first l))
			 (string-append
			  (command-usage (sx-first l))
			  "|"
			  (loop (sx-rest l)))))))
	    (if (= (sx-length l) 1) s (string-append "[" s "]"))))
	  (if (sx-null? l)
	      ""
	      (case (sx-datum (sx-first (sx-first l)))
	       ((any-number)
		(string-append " ["
			       (command-usage1 (sx-rest (sx-first l)))
			       "]*"
			       (command-usage (sx-rest l))))
	       ((at-least-one)
		(string-append " ["
			       (command-usage1 (sx-rest (sx-first l)))
			       "]+"
			       (command-usage (sx-rest l))))
	       ((at-most-one)
		(string-append
		 " ["
		 (command-usage1 (sx-rest (sx-first l)))
		 "]"
		 (command-usage (sx-rest l))))
	       ((exactly-one)
		(string-append
		 " "
		 (command-usage1 (sx-rest (sx-first l)))
		 (command-usage (sx-rest l))))
	       ((required)
		(string-append " "
			       (sx-datum (sx-second (sx-second (sx-first l))))
			       (command-usage (sx-rest l))))
	       ((optional)
		(string-append " ["
			       (sx-datum (sx-second (sx-second (sx-first l))))
			       (command-usage (sx-rest l)) "]"))
	       ((rest)
		(string-append " ["
			       (sx-datum (sx-second (sx-second (sx-first l))))
			       "]*"))
	       (else (fuck-up)))))
	 (define (command-bindings l)
	  (if (sx-null? l)
	      '()
	      (case (sx-datum (sx-first (sx-first l)))
	       ((any-number at-least-one)
		(append
		 (reduce
		  append
		  (sx-map (lambda (l)
			   (cons (list (sx-second l) #f)
				 (sx-map (lambda (l) (list (sx-first l) ''()))
					 (sx-rest (sx-rest l)))))
			  (sx-rest (sx-first l)))
		  '())
		 (command-bindings (sx-rest l))))
	       ((at-most-one exactly-one)
		(append
		 (reduce
		  append
		  (sx-map (lambda (l)
			   (cons (list (sx-second l) #f)
				 (sx-map (lambda (l)
					  (list (sx-first l) (sx-fourth l)))
					 (sx-rest (sx-rest l)))))
			  (sx-rest (sx-first l)))
		  '())
		 (command-bindings (sx-rest l))))
	       ;; changed
	       ((required) (cons (sx-first (sx-second (sx-first l)))
				 (command-bindings (sx-rest l))))
	       ((optional) (cons (list (sx-first (sx-second (sx-first l)))
				       (sx-fourth (sx-second (sx-first l))))
				 (command-bindings (sx-rest l))))
	       ((rest) (cons (list (sx-first (sx-second (sx-first l))) ''())
			     (command-bindings (sx-rest l))))
	       (else (fuck-up)))))
	 (define (command-keyword-argument-parser l)
	  (cons
	   `(let loop ()
	     (unless (null? arguments)
	      (cond
	       ,@(let loop ((l l))
		  (if (sx-null? l)
		      '(((string=? (car arguments) "-usage") (usage))) ;changed
		      (case (sx-datum (sx-first (sx-first l)))
		       ((any-number at-least-one)
			(append
			 (sx-map
			  (lambda (l)
			   `((string=? (car arguments) ;changed
				       ,(string-append
					 "-" (sx-datum (sx-first l))))
			     (set! arguments (cdr arguments)) ;changed
			     (set! ,(sx-second l) #t)
			     ,@(reduce
				append
				(sx-map
				 (lambda (l)
				  `((when (null? arguments) (usage))
				    (set! ,(sx-first l)
					  (cons (,(sx-third l)
						 ;; changed
						 (car arguments) usage)
						,(sx-first l)))
				    ;; changed
				    (set! arguments (cdr arguments))))
				 (sx-rest (sx-rest l)))
				'())
			     (loop)))
			  (sx-rest (sx-first l)))
			 (loop (sx-rest l))))
		       ((at-most-one exactly-one)
			(append
			 (sx-map
			  (lambda (l1)
			   `((string=? (car arguments) ;changed
				       ,(string-append
					 "-" (sx-datum (sx-first l1))))
			     (set! arguments (cdr arguments)) ;changed
			     (when (or ,@(sx-map sx-second
						 (sx-rest (sx-first l))))
			      (usage))
			     (set! ,(sx-second l1) #t)
			     ,@(reduce
				append
				(sx-map (lambda (l)
					 `((when (null? arguments) (usage))
					   (set! ,(sx-first l)
						 (,(sx-third l)
						  ;; changed
						  (car arguments) usage))
					   ;; changed
					   (set! arguments (cdr arguments))))
					(sx-rest (sx-rest l1)))
				'())
			     (loop)))
			  (sx-rest (sx-first l)))
			 (loop (sx-rest l))))
		       ((required optional rest) (loop (sx-rest l)))
		       (else (fuck-up))))))))
	   (let loop ((l l))
	    (if (sx-null? l)
		'()
		(case (sx-datum (sx-first (sx-first l)))
		 ((at-least-one exactly-one)
		  (cons `(unless (or ,@(sx-map sx-second
					       (sx-rest (sx-first l))))
			  (usage))
			(loop (sx-rest l))))
		 ((at-most-one any-number required optional rest)
		  (loop (sx-rest l)))
		 (else (fuck-up)))))))
	 (define (command-positional-argument-parser l)
	  (let loop ((l l))
	   (if (sx-null? l)
	       '((unless (null? arguments) (usage)))
	       (case (sx-datum (sx-first (sx-first l)))
		((any-number at-least-one at-most-one exactly-one)
		 (loop (sx-rest l)))
		((required)
		 (append `((when (null? arguments) (usage))
			   (set! ,(sx-first (sx-second (sx-first l)))
				 (,(sx-third (sx-second (sx-first l)))
				  (car arguments) usage)) ;changed
			   (set! arguments (cdr arguments))) ;changed
			 (loop (sx-rest l))))
		((optional)
		 (cons `(unless (null? arguments)
			 (set! ,(sx-first (sx-second (sx-first l)))
			       (,(sx-third (sx-second (sx-first l)))
				(car arguments) usage))	;changed
			 (set! arguments (cdr arguments))) ;changed
		       (loop (sx-rest l))))
		((rest)
		 `((let loop ()
		    (unless (null? arguments)
		     (set! ,(sx-first (sx-second (sx-first l)))
			   (cons (,(sx-third (sx-second (sx-first l)))
				  (car arguments) usage) ;changed
				 ,(sx-first (sx-second (sx-first l)))))
		     (set! arguments (cdr arguments)) ;changed
		     (loop)))))
		(else (fuck-up))))))
	 (unless (and (sx-list? s)
		      (>= (sx-length s) 2)
		      (valid-command-arguments? (sx-second s)))
	  (syntax-error s "Improper DEFINE-COMMAND"))
	 ;; changed
	 `(let ((arguments (vector->list argv)))
	   (define (string-argument string usage)
	    ;; changed
	    (if (string? string) string (panic "This shouldn't happen")))
	   (define (integer-argument string usage)
	    (let ((integer (string->number string)))
	     ;; changed
	     (if (integer? integer)
		 (if (exact? integer) integer (usage))
		 (usage))))
	   (define (real-argument string usage)
	    (let ((real (string->number string)))
	     ;; changed
	     (if (real? real) (exact->inexact real) (usage))))
	   (let ((program (car arguments))) ;changed
	    (define (usage)
	     ;; removed: STDERR-PORT
	     (panic
	      (string-append
	       "usage: " program ,(command-usage (sx-rest (sx-second s))))))
	    (set! arguments (cdr arguments)) ;changed
	    (let ,(command-bindings (sx-rest (sx-second s)))
	     ,@(command-keyword-argument-parser (sx-rest (sx-second s)))
	     ,@(command-positional-argument-parser (sx-rest (sx-second s)))
	     ,@(sx-unlist (sx-rest (sx-rest s))))))))
  (list 'parallel-begin
	(lambda (s)
	 (cond
	  ((sx-null? (sx-rest s)) '((lambda ())))
	  ((sx-null? (sx-rest (sx-rest s))) (sx-second s))
	  (else `((primitive-procedure fork)
		  (lambda () ,(sx-second s))
		  (lambda ()
		   (parallel-begin ,@(sx-unlist (sx-rest (sx-rest s))))))))))
  (list 'parallel-call
	(lambda (s)
	 (unless (and (sx-list? s) (>= (sx-length s) 2))
	  (syntax-error s "Improper PARALLEL-CALL"))
	 (let ((variables
		(sx-map (lambda (s) (gensym "x")) (sx-rest (sx-rest s)))))
	  `(let ,variables
	    (parallel-begin ,@(map (lambda (variable s) `(set! ,variable ,s))
				   variables
				   (sx-unlist (sx-rest (sx-rest s)))))
	    (,(sx-second s) ,@variables)))))
  (list 'parallel-do
	(lambda (s)
	 ;; Extension to R4RS: Iterators can be empty.
	 (unless (and (>= (sx-length s) 3)
		      (sx-list? (sx-second s))
		      (sx-every (lambda (s)
				 (and (sx-list? s)
				      (or (= (sx-length s) 2)
					  (= (sx-length s) 3))))
				(sx-second s))
		      (sx-list? (sx-third s))
		      (>= (sx-length (sx-third s)) 1))
	  (syntax-error s "Improper PARALLEL-DO"))
	 (let ((loop (gensym "loop")))
	  ;; conventions: LOOP
	  `(letrec ((,loop (lambda ,(sx-map sx-first (sx-second s))
			    (if ,(sx-first (sx-third s))
				(begin ,@(sx-unlist (sx-rest (sx-third s))))
				(parallel-begin
				 ,@(sx-unlist (sx-rest (sx-rest (sx-rest s))))
				 (,loop
				  ,@(sx-map (lambda (s)
					     (if (= (sx-length s) 2)
						 (sx-first s)
						 (sx-third s)))
					    (sx-second s))))))))
	    (,loop ,@(sx-map sx-second (sx-second s)))))))
  (list 'mutex-begin
	(lambda (s)
	 `((primitive-procedure mutex)
	   (lambda () ,@(sx-unlist (sx-rest s))))))))

(define *Trotsky-macros*
 (list
  (list 'define-primitive-procedure
	(lambda (s)
	 (unless (= (sx-length s) 9)
	  (syntax-error s "Wrong number of arguments"))
	 `(set! *primitive-procedure-handlers*
		(cons
		 (cons ',(sx-second s)
		       (make-primitive-procedure
			,(sx-third s)
			(lambda (y u0 w0) ,(sx-fourth s))
			(lambda (y u0 n w0) ,(sx-fifth s))
			(lambda (y u0 n w0) ,(sx-sixth s))
			(lambda (y u0 propagate-result!
				   propagate-type-predicate! w0)
			 ,(sx-seventh s))
			(lambda (r y u0 ws w w0 w1 w2 w3) ,(sx-eighth s))
			(lambda (r y u0 ts ws t w compile-type-predicate
				   t0 w0 t1 w1 t2 w2 t3 w3)
			 ,(sx-ninth s))))
		 *primitive-procedure-handlers*))))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
