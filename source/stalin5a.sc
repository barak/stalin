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
(module stalin5a)

(include "QobiScheme.sch")
(include "stalin5a.sch")
;;; End delete for Trotsky

(define *list->vector* #f)
(define *append* #f)
(define *cons* #f)
(define *eqv?* #f)

;;; Backquote

(define (contains-unquote? s)
 (or (and (sx-list? s)
	  (= (sx-length s) 2)
	  (or (sx-eq? (sx-first s) 'unquote)
	      (sx-eq? (sx-first s) 'unquote-splicing)))
     (and (sx-vector? s) (sx-some-vector contains-unquote? s))
     (and (sx-pair? s)
	  (or (contains-unquote? (sx-car s)) (contains-unquote? (sx-cdr s))))))

(define (expand-quasiquote s)
 ;; needs work: This encapsulation loses the line and character positions of
 ;;             the quasiquote expression that is being rewritten.
 (encapsulate
  (if (contains-unquote? s)
      (cond
       ((and (sx-list? s)
	     (= (sx-length s) 2)
	     (or (sx-eq? (sx-first s) 'unquote)
		 (sx-eq? (sx-first s) 'unquote-splicing)))
	s)
       ((sx-vector? s)
	(let ((ss (map expand-quasiquote (sx-vector->list s))))
	 (if (some (lambda (s) (sx-eq? (sx-first s) 'unquote-splicing)) ss)
	     (list 'unquote
		   `(,*list->vector*
		     (,*append*
		      ,@(map (lambda (s)
			      (case (sx-datum (sx-first s))
			       ((unquote) `(,*cons* ,(sx-second s) '()))
			       ((unquote-splicing) (sx-second s))
			       ((quote) `'(,(sx-second s)))
			       (else (fuck-up))))
			     ss))))
	     (list 'unquote
		   `((primitive-procedure vector)
		     ,@(map (lambda (s)
			     (if (sx-eq? (sx-first s) 'unquote)
				 (sx-second s)
				 s))
			    ss))))))
       ((sx-pair? s)
	(let ((s1 (expand-quasiquote (sx-car s)))
	      (s2 (expand-quasiquote (sx-cdr s))))
	 (case (sx-datum (sx-first s1))
	  ((unquote)
	   (case (sx-datum (sx-first s2))
	    ((unquote)
	     (list 'unquote `(,*cons* ,(sx-second s1) ,(sx-second s2))))
	    ((unquote-splicing) (syntax-error s "Improper UNQUOTE-SPLICING"))
	    ((quote) (list 'unquote `(,*cons* ,(sx-second s1) ,s2)))
	    (else (fuck-up))))
	  ((unquote-splicing)
	   (case (sx-datum (sx-first s2))
	    ((unquote)
	     ;; needs work: This doesn't handle the case `(,@A . ,B) where B is
	     ;;             not a list.
	     (list 'unquote `(,*append* ,(sx-second s1) ,(sx-second s2))))
	    ((unquote-splicing) (syntax-error s "Improper UNQUOTE-SPLICING"))
	    ((quote)
	     ;; needs work: This doesn't handle the case `(,@A . B).
	     (list 'unquote `(,*append* ,(sx-second s1) ,s2)))
	    (else (fuck-up))))
	  ((quote)
	   (case (sx-datum (sx-first s2))
	    ((unquote) (list 'unquote `(,*cons* ,s1 ,(sx-second s2))))
	    ((unquote-splicing) (syntax-error s "Improper UNQUOTE-SPLICING"))
	    ((quote) `'(,(sx-second s1) . ,(sx-second s2)))
	    (else (fuck-up))))
	  (else (fuck-up)))))
       (else `',s))
      `',s)))

;;; DEFINEs

(define (defined-variables ss)
 (let loop ((ss ss) (ss1 '()))
  (cond
   ((null? ss) ss1)
   ((and (sx-pair? (first ss)) (sx-eq? (sx-first (first ss)) 'define))
    ;; Extension to R4RS: undefined defines.
    (cond
     ((and (or (= (sx-length (first ss)) 2) (= (sx-length (first ss)) 3))
	   (sx-symbol? (sx-second (first ss))))
      (loop (rest ss)
	    (if (memq (sx-datum (sx-second (first ss))) (map sx-datum ss1))
		ss1
		(cons (sx-second (first ss)) ss1))))
     ((and (>= (sx-length (first ss)) 2) (sx-pair? (sx-second (first ss))))
      (loop (rest ss)
	    (if (memq (sx-datum (sx-first (sx-second (first ss))))
		      (map sx-datum ss1))
		ss1
		(cons (sx-first (sx-second (first ss))) ss1))))
     (else (syntax-error (first ss) "Improper DEFINE"))))
   (else (loop (rest ss) ss1)))))

(define (body s)
 (cond ((and (sx-list? s) (sx-pair? s) (sx-eq? (sx-first s) 'begin))
	(let ((ss (body-list (sx-rest s))))
	 (if (every
	      (lambda (s)
	       (and (sx-list? s) (sx-pair? s) (sx-eq? (sx-first s) 'define)))
	      ss)
	     ss
	     ;; needs work: This encapsulation loses the line and character
	     ;;             positions of the BEGIN expression that is being
	     ;;             rewritten.
	     (list (encapsulate `(begin ,@ss))))))
       ((and (sx-list? s) (sx-pair? s) (macro? (sx-first s)))
	(body (expand-macro s)))
       (else (list s))))

(define (body-list s) (reduce append (sx-map body s) '()))

(define (macroexpand-body s)
 ;; Extension to R4RS: definitions can appear more places.
 (unless (s-expression-macroexpand-body s)
  (set-s-expression-macroexpand-body!
   s
   (let ((ss (body-list (sx-rest (sx-rest s)))))
    ;; needs work: This encapsulation loses the line and character positions of
    ;;             the lambda expression that is being rewritten.
    (encapsulate
     (if (some (lambda (s)
		(and (sx-list? s) (sx-pair? s) (sx-eq? (sx-first s) 'define)))
	       ss)
	 (let ((ss1 (defined-variables ss))
	       (ss (map (lambda (s)
			 (if (and (sx-pair? s) (sx-eq? (sx-first s) 'define))
			     (if (sx-symbol? (sx-second s))
				 `(set! ,(sx-second s) ,(sx-third s))
				 `(set! ,(sx-first (sx-second s))
					(lambda ,(sx-rest (sx-second s))
					 ,@(sx-unlist (sx-rest (sx-rest s))))))
			     s))
			(remove-if (lambda (s)
				    (and (sx-pair? s)
					 (sx-eq? (sx-first s) 'define)
					 (= (sx-length s) 2)))
				   ss))))
	  (cond
	   ((null? ss) `(,(sx-first s) ,(sx-second s)))
	   ((null? (rest ss))
	    `(,(sx-first s)
	      ,(sx-second s)
	      ((lambda ,ss1 ,(last ss))
	       ,@(map (lambda (s) '((lambda ()))) ss1))))
	   (else
	    `(,(sx-first s)
	      ,(sx-second s)
	      ((lambda ,ss1
		;; note: This transformation relies on left-to-right argument
		;;       evaluation order.
		((lambda ,(map (lambda (s) (gensym "hunoz")) (but-last ss))
		  ,(last ss))
		 ,@(but-last ss)))
	       ,@(map (lambda (s) '((lambda ()))) ss1))))))
	 (if (or (null? ss) (null? (rest ss)))
	     s
	     `(,(sx-first s)
	       ,(sx-second s)
	       ;; note: This transformation relies on left-to-right argument
	       ;;       evaluation order.
	       ((lambda ,(map (lambda (s) (gensym "hunoz")) (but-last ss))
		 ,(last ss))
		,@(but-last ss)))))))))
 (s-expression-macroexpand-body s))

;;; The macros

(define *macros* '())

(define *r4rs-macros*
 (list
  (list 'cond
	(lambda (s)
	 (unless (and (sx-every (lambda (s) (and (sx-list? s) (sx-pair? s)))
				(sx-rest s))
		      (or (= (sx-length s) 1)
			  (and (sx-every (lambda (s)
					  (or (< (sx-length s) 2)
					      (not (sx-eq? (sx-second s) '=>))
					      (= (sx-length s) 3)))
					 (sx-rest s))
			       (every
				(lambda (s) (not (sx-eq? (sx-first s) 'else)))
				(rest (reverse (rest (sx-unlist s))))))))
	  (syntax-error s "Improper COND"))
	 (cond ((= (sx-length s) 1) '((lambda ())))
	       ((sx-eq? (sx-first (sx-second s)) 'else)
		`(begin ,@(sx-unlist (sx-rest (sx-second s)))))
	       ((sx-null? (sx-rest (sx-second s)))
		`(or ,(sx-first (sx-second s))
		     (cond ,@(sx-unlist (sx-rest (sx-rest s))))))
	       ((and (= (sx-length (sx-second s)) 3)
		     (sx-eq? (sx-second (sx-second s)) '=>))
		(let ((v (gensym "v")))
		 ;; conventions: V
		 `(let ((,v ,(sx-first (sx-second s))))
		   (if ,v
		       (,(sx-third (sx-second s)) ,v)
		       (cond ,@(sx-unlist (sx-rest (sx-rest s))))))))
	       (else `(if ,(sx-first (sx-second s))
			  (begin ,@(sx-unlist (sx-rest (sx-second s))))
			  (cond ,@(sx-unlist (sx-rest (sx-rest s)))))))))
  (list 'case
	(lambda (s)
	 (unless (and (>= (sx-length s) 2)
		      (sx-every (lambda (s) (and (sx-list? s) (sx-pair? s)))
				(sx-rest (sx-rest s)))
		      (or (= (sx-length s) 2)
			  (and (every
				(lambda (s) (sx-list? (sx-first s)))
				(rest (reverse (rest (rest (sx-unlist s))))))
			       (or (sx-eq? (sx-first (sx-last s)) 'else)
				   (sx-list? (sx-first (sx-last s)))))))
	  (syntax-error s "Improper CASE"))
	 (let ((v (gensym "v")))
	  ;; conventions: V
	  `(let ((,v ,(sx-second s)))
	    ,(if (and (>= (sx-length s) 3)
		      (sx-eq? (sx-first (sx-last s)) 'else))
		 `(cond ,@(map (lambda (s)
				`((or ,@(sx-map (lambda (s) `(,*eqv?* ,v ',s))
						(sx-first s)))
				  ,@(sx-unlist (sx-rest s))))
			       (but-last (rest (rest (sx-unlist s)))))
			,(sx-last s))
		 `(cond ,@(sx-map (lambda (s)
				   `((or ,@(sx-map
					    (lambda (s) `(,*eqv?* ,v ',s))
					    (sx-first s)))
				     ,@(sx-unlist (sx-rest s))))
				  (sx-rest (sx-rest s)))))))))
  (list 'and
	(lambda (s)
	 (cond ((= (sx-length s) 1) #t)
	       ((= (sx-length s) 2) (sx-second s))
	       (else `(if ,(sx-second s)
			  (and ,@(sx-unlist (sx-rest (sx-rest s))))
			  #f)))))
  (list 'or
	(lambda (s)
	 (cond
	  ((= (sx-length s) 1) #f)
	  ((= (sx-length s) 2) (sx-second s))
	  (else (let ((v (gensym "v")))
		 ;; conventions: V
		 `(let ((,v ,(sx-second s)))
		   (if ,v ,v (or ,@(sx-unlist (sx-rest (sx-rest s)))))))))))
  (list 'let
	;; Extension to R4RS: Binding can be symbol.
	(lambda (s)
	 (unless (and (>= (sx-length s) 2)
		      (or (and (sx-list? (sx-second s))
			       (sx-every
				(lambda (s)
				 (or (sx-symbol? s)
				     (and (sx-list? s) (= (sx-length s) 2))))
				(sx-second s)))
			  (and (sx-symbol? (sx-second s))
			       (>= (sx-length s) 3)
			       (sx-list? (sx-third s))
			       (sx-every
				(lambda (s)
				 (or (sx-symbol? s)
				     (and (sx-list? s) (= (sx-length s) 2))))
				(sx-third s)))))
	  (syntax-error s "Improper LET"))
	 (if (sx-list? (sx-second s))
	     ;; note: This is more complicated than it has to be in attempt to
	     ;;       match the Scheme->C argument evaluation order.
	     `((lambda ,(map (lambda (s) (if (sx-symbol? s) s (sx-first s)))
			     (reverse (sx-unlist (sx-second s))))
		,@(sx-unlist (sx-rest (sx-rest s))))
	       ,@(map (lambda (s)
		       (if (sx-symbol? s) '((lambda ())) (sx-second s)))
		      (reverse (sx-unlist (sx-second s)))))
	     `((letrec ((,(sx-second s)
			 (lambda ,(sx-map (lambda (s)
					   (if (sx-symbol? s) s (sx-first s)))
					  (sx-third s))
			  ,@(sx-unlist (sx-rest (sx-rest (sx-rest s)))))))
		,(sx-second s))
	       ,@(sx-map (lambda (s)
			  (if (sx-symbol? s) '((lambda ())) (sx-second s)))
			 (sx-third s))))))
  (list 'let*
	;; Extension to R4RS: Binding can be symbol.
	(lambda (s)
	 (unless (and (>= (sx-length s) 2)
		      (sx-list? (sx-second s))
		      (sx-every (lambda (s)
				 (or (sx-symbol? s)
				     (and (sx-list? s) (= (sx-length s) 2))))
				(sx-second s)))
	  (syntax-error s "Improper LET*"))
	 (if (sx-null? (sx-second s))
	     `(begin ,@(sx-unlist (sx-rest (sx-rest s))))
	     `(let (,(sx-first (sx-second s)))
	       (let* ,(sx-rest (sx-second s))
		,@(sx-unlist (sx-rest (sx-rest s))))))))
  (list 'letrec
	;; Extension to R4RS: Binding can be symbol.
	(lambda (s)
	 (unless (and (>= (sx-length s) 2)
		      (sx-list? (sx-second s))
		      (sx-every (lambda (s)
				 (or (sx-symbol? s)
				     (and (sx-list? s) (= (sx-length s) 2))))
				(sx-second s)))
	  (syntax-error s "Improper LETREC"))
	 `(let ,(sx-map (lambda (s)
			 (if (sx-symbol? s)
			     `(,s ((lambda ())))
			     `(,(sx-first s) ((lambda ())))))
			(sx-second s))
	   ,@(map (lambda (s) `(set! ,(sx-first s) ,(sx-second s)))
		  (remove-if sx-symbol? (sx-unlist (sx-second s))))
	   ,@(sx-unlist (sx-rest (sx-rest s))))))
  (list 'begin (lambda (s) `((lambda () ,@(sx-unlist (sx-rest s))))))
  (list 'do
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
	  (syntax-error s "Improper DO"))
	 (let ((loop (gensym "loop")))
	  ;; conventions: LOOP
	  `(letrec ((,loop (lambda ,(sx-map sx-first (sx-second s))
			    (if ,(sx-first (sx-third s))
				(begin ,@(sx-unlist (sx-rest (sx-third s))))
				(begin
				 ,@(sx-unlist (sx-rest (sx-rest (sx-rest s))))
				 (,loop
				  ,@(sx-map (lambda (s)
					     (if (= (sx-length s) 2)
						 (sx-first s)
						 (sx-third s)))
					    (sx-second s))))))))
	    (,loop ,@(sx-map sx-second (sx-second s)))))))
  (list 'delay
	(lambda (s)
	 (unless (= (sx-length s) 2) (syntax-error s "Improper DELAY"))
	 `((lambda (proc)
	    (let ((result-ready? #f)
		  (result #f))
	     (lambda ()
	      (if result-ready?
		  result
		  (let ((x (proc)))
		   (if result-ready?
		       result
		       (begin (set! result-ready? #t)
			      (set! result x)
			      result)))))))
	   (lambda () ,(sx-second s)))))
  (list 'quasiquote
	(lambda (s)
	 (unless (= (sx-length s) 2) (syntax-error s "Improper QUASIQUOTE"))
	 (let ((s (expand-quasiquote (sx-second s))))
	  (case (sx-datum (sx-first s))
	   ((unquote) (sx-second s))
	   ((unquote-splicing) (syntax-error s "Improper UNQUOTE-SPLICING"))
	   ((quote) s)
	   (else (fuck-up))))))
  (list 'unquote (lambda (s) (syntax-error s "UNQUOTE not inside QUASIQUOTE")))
  (list 'unquote-splicing
	(lambda (s)
	 (syntax-error s "UNQUOTE-SPLICING not inside QUASIQUOTE")))))

;;; The Scheme library

(define *read*
 '(define (read . port)
   ;; needs work: Long predecimal point digit strings can overflow.
   ;; needs work: Mantissa can overflow or underflow even though exponent
   ;;             would prevent that overflow or underflow.
   ;; needs work: Can't read largest negative number.
   ;; needs work: To handle polar numbers with @.
   ;; needs work: To handle rectangular numbers with i.
   ;; needs work: To handle ratios with /.
   ;; needs work: To handle numbers with embedded #.
   ;; needs work: To handle exactness with #e and #i.
   ;; needs work: To handle structures.
   (set! port (if (null? port) (current-input-port) (car port)))
   ;; needs work: The DOT and CLOSE gensyms should be extracted and bound by a
   ;;             LET that is outside the DEFINE of READ.
   (let ((dot (string->uninterned-symbol (string-copy "dot")))
	 (close (string->uninterned-symbol (string-copy "close"))))
    (let read ((state 'object))
     (define (read-exact-binary-integer n)
      (let ((c (peek-char port)))
       (cond
	((eof-object? c) n)
	((char=? c #\0) (read-char port) (read-exact-binary-integer (* 2 n)))
	((char=? c #\1)
	 (read-char port)
	 (read-exact-binary-integer (+ (* 2 n) 1)))
	(else n))))
     (define (read-exact-octal-integer n)
      (let ((c (peek-char port)))
       (cond ((eof-object? c) n)
	     ((and (char>=? c #\0) (char<=? c #\7))
	      (read-char port)
	      (read-exact-octal-integer
	       (+ (* 8 n) (- (char->integer c) (char->integer #\0)))))
	     (else n))))
     (define (read-exact-decimal-integer n)
      (let ((c (peek-char port)))
       (cond ((eof-object? c) n)
	     ((char-numeric? c)
	      (read-char port)
	      (read-exact-decimal-integer
	       (+ (* 10 n) (- (char->integer c) (char->integer #\0)))))
	     (else n))))
     (define (read-exact-hexadecimal-integer n)
      (let ((c (peek-char port)))
       (cond ((eof-object? c) n)
	     ((char-numeric? c)
	      (read-char port)
	      (read-exact-hexadecimal-integer
	       (+ (* 16 n) (- (char->integer c) (char->integer #\0)))))
	     ((and (char>=? c #\a) (char<=? c #\f))
	      (read-char port)
	      (read-exact-hexadecimal-integer
	       (+ (* 16 n) (- (char->integer c) (char->integer #\a)) 10)))
	     ((and (char>=? c #\A) (char<=? c #\F))
	      (read-char port)
	      (read-exact-hexadecimal-integer
	       (+ (* 16 n) (- (char->integer c) (char->integer #\A)) 10)))
	     (else n))))
     (define (read-inexact-number n m)
      (let ((c1 (peek-char port)))
       (cond
	((eof-object? c1) n)
	((char-numeric? c1)
	 (read-char port)
	 (read-inexact-number
	  (+ n (/ (- (char->integer c1) (char->integer #\0)) m)) (* m 10.0)))
	((or (char=? c1 #\e) (char=? c1 #\E)
	     (char=? c1 #\s) (char=? c1 #\S)
	     (char=? c1 #\f) (char=? c1 #\F)
	     (char=? c1 #\d) (char=? c1 #\D)
	     (char=? c1 #\l) (char=? c1 #\L))
	 (read-char port)
	 (let ((c2 (read-char port)))
	  (if (eof-object? c2) (panic "EOF while reading exponent"))
	  (cond
	   ((char-numeric? c2)
	    (* n (expt 10.0
		       (read-exact-decimal-integer
			(- (char->integer c2) (char->integer #\0))))))
	   ((char=? c2 #\+)
	    (let ((c3 (read-char port)))
	     (if (eof-object? c3) (panic "EOF while reading exponent"))
	     (if (not (char-numeric? c3)) (panic "Unfinished exponent"))
	     (* n (expt 10.0
			(read-exact-decimal-integer
			 (- (char->integer c3) (char->integer #\0)))))))
	   ((char=? c2 #\-)
	    (let ((c3 (read-char port)))
	     (if (eof-object? c3) (panic "EOF while reading exponent"))
	     (if (not (char-numeric? c3)) (panic "Unfinished exponent"))
	     (* n (expt 10.0
			(- (read-exact-decimal-integer
			    (- (char->integer c3) (char->integer #\0))))))))
	   (else (panic "Unfinished exponent")))))
	(else n))))
     (define (read-number n)
      (let ((c1 (peek-char port)))
       (cond
	((eof-object? c1) n)
	((char-numeric? c1)
	 (read-char port)
	 (read-number (+ (* 10 n) (- (char->integer c1) (char->integer #\0)))))
	((char=? c1 #\.)
	 (read-char port)
	 (read-inexact-number (exact->inexact n) 10.0))
	((or (char=? c1 #\e) (char=? c1 #\E)
	     (char=? c1 #\s) (char=? c1 #\S)
	     (char=? c1 #\f) (char=? c1 #\F)
	     (char=? c1 #\d) (char=? c1 #\D)
	     (char=? c1 #\l) (char=? c1 #\L))
	 (read-char port)
	 (let ((c2 (read-char port)))
	  (if (eof-object? c2) (panic "EOF while reading exponent"))
	  (cond
	   ((char-numeric? c2)
	    (* (exact->inexact n)
	       (expt 10.0
		     (read-exact-decimal-integer
		      (- (char->integer c2) (char->integer #\0))))))
	   ((char=? c2 #\+)
	    (let ((c3 (read-char port)))
	     (if (eof-object? c3) (panic "EOF while reading exponent"))
	     (if (not (char-numeric? c3)) (panic "Unfinished exponent"))
	     (* (exact->inexact n)
		(expt 10.0
		      (read-exact-decimal-integer
		       (- (char->integer c3) (char->integer #\0)))))))
	   ((char=? c2 #\-)
	    (let ((c3 (read-char port)))
	     (if (eof-object? c3) (panic "EOF while reading exponent"))
	     (if (not (char-numeric? c3)) (panic "Unfinished exponent"))
	     (* (exact->inexact n)
		(expt 10.0
		      (- (read-exact-decimal-integer
			  (- (char->integer c3) (char->integer #\0))))))))
	   (else (panic "Unfinished exponent")))))
	(else n))))
     (define (char-initial? c)
      (or (char-alphabetic? c)
	  (char=? c #\~)
	  (char=? c #\!)
	  (char=? c #\$)
	  (char=? c #\%)
	  (char=? c #\^)
	  (char=? c #\&)
	  (char=? c #\*)
	  (char=? c #\_)
	  (char=? c #\/)
	  (char=? c #\:)
	  (char=? c #\<)
	  (char=? c #\=)
	  (char=? c #\>)
	  (char=? c #\?)))
     (define (char-subsequent? c)
      (or (char-initial? c)
	  (char-numeric? c)
	  (char=? c #\+)
	  (char=? c #\-)
	  (char=? c #\.)))
     (define (read-symbol s)
      ;; needs work: To eliminate LIST-REVERSE.
      (let ((c (peek-char port)))
       (cond ((eof-object? c) (string->symbol (list->string (list-reverse s))))
	     ((char-subsequent? c)
	      (read-char port)
	      (read-symbol (cons (char-upcase c) s)))
	     (else (string->symbol (list->string (list-reverse s)))))))
     (define (lookup-character-name s)
      (let loop ((names '(((#\e #\c #\a #\p #\s) . #\space)
			  ((#\e #\n #\i #\l #\w #\e #\n) . #\newline))))
       (if (null? names) (panic "Unrecognized character name"))
       (if (let loop? ((s s) (name (car (car names))))
	    (or (and (null? s) (null? name))
		(and (not (null? s))
		     (not (null? name))
		     (char-ci=? (car s) (car name))
		     (loop? (cdr s) (cdr name)))))
	   (cdr (car names))
	   (loop (cdr names)))))
     (define (read-character-name s)
      (let ((c (peek-char port)))
       (cond ((eof-object? c) (lookup-character-name s))
	     ((char-alphabetic? c)
	      (read-char port)
	      (read-character-name (cons c s)))
	     (else (if (and (not (null? s)) (null? (cdr s)))
		       (car s)
		       (lookup-character-name s))))))
     (let ((c1 (read-char port)))
      (cond
       ((eof-object? c1)
	;; note: Manually split EQV? here by removing CASE.
	(cond
	 ((eq? state 'object) c1)
	 ((eq? state 'list) (panic "EOF while reading list"))
	 ((eq? state 'vector) (panic "EOF while reading vector"))
	 ((eq? state 'quote) (panic "EOF while reading quoted object"))
	 ((eq? state 'quasiquote)
	  (panic "EOF while reading quasiquoted object"))
	 ((eq? state 'unquote-splicing)
	  (panic "EOF while reading unquote-slicing object"))
	 ((eq? state 'unquote) (panic "EOF while reading unquoted object"))
	 ((eq? state 'close) (panic "EOF while reading pair"))
	 (else (panic "This shouldn't happen"))))
       ((char=? c1 #\;)
	(let loop ()
	 (if (let ((c (read-char port)))
	      (and (not (eof-object? c)) (not (char=? c #\newline))))
	     (loop)))
	(read state))
       ((char=? c1 #\))
	(if (and (not (eq? state 'list))
		 (not (eq? state 'vector))
		 (not (eq? state 'close)))
	    (panic "Mismatched closing parenthesis"))
	close)
       ((char-whitespace? c1) (read state))
       ((eq? state 'close) (panic "Only one object allowed after dot"))
       ((char=? c1 #\') (list 'quote (read 'quote)))
       ((char=? c1 #\`) (list 'quasiquote (read 'quasiquote)))
       ((char=? c1 #\,)
	(let ((c2 (peek-char port)))
	 (if (eof-object? c2) (panic "EOF after dot"))
	 (cond ((char=? c2 #\@)
		(read-char port)
		(list 'unquote-splicing (read 'unquote-splicing)))
	       (else (list 'unquote (read 'unquote))))))
       ((char=? c1 #\()
	(let loop ((s '()))
	 (let ((e (read 'list)))
	  ;; needs work: To eliminate LIST-REVERSE.
	  (cond
	   ((eq? e dot)
	    (if (null? s) (panic "Dot cannot be first element of list"))
	    (let* ((e1 (read 'object))
		   (e2 (read 'close)))
	     (let loop ((s (cdr s)) (c (cons (car s) e1)))
	      (if (null? s) c (loop (cdr s) (cons (car s) c))))))
	   ((eq? e close) (list-reverse s))
	   (else (loop (cons e s)))))))
       ((char=? c1 #\#)
	(let ((c2 (read-char port)))
	 (if (eof-object? c2) (panic "EOF after sharp sign"))
	 (cond
	  ((or (char=? c2 #\t) (char=? c2 #\T)) #t)
	  ((or (char=? c2 #\f) (char=? c2 #\F)) #f)
	  ((or (char=? c2 #\b) (char=? c2 #\B))
	   (let ((c3 (read-char port)))
	    (if (eof-object? c3) (panic "EOF while reading binary number"))
	    (cond ((char=? c3 #\0) (read-exact-binary-integer 0))
		  ((char=? c3 #\1) (read-exact-binary-integer 1))
		  ((char=? c3 #\+)
		   (let ((c4 (read-char port)))
		    (if (eof-object? c4)
			(panic "EOF while reading binary number"))
		    (cond ((char=? c4 #\0) (read-exact-binary-integer 0))
			  ((char=? c4 #\1) (read-exact-binary-integer 1))
			  (else (panic "Unfinished binary number")))))
		  ((char=? c3 #\-)
		   (let ((c4 (read-char port)))
		    (if (eof-object? c4)
			(panic "EOF while reading binary number"))
		    (cond ((char=? c4 #\0) (- (read-exact-binary-integer 0)))
			  ((char=? c4 #\1) (- (read-exact-binary-integer 1)))
			  (else (panic "Unfinished binary number")))))
		  (else (panic "Unfinished binary number")))))
	  ((or (char=? c2 #\o) (char=? c2 #\O))
	   (let ((c3 (read-char port)))
	    (if (eof-object? c3) (panic "EOF while reading octal number"))
	    (cond ((and (char>=? c3 #\0) (char<=? c3 #\7))
		   (read-exact-octal-integer
		    (- (char->integer c3) (char->integer #\0))))
		  ((char=? c3 #\+)
		   (let ((c4 (read-char port)))
		    (if (eof-object? c4)
			(panic "EOF while reading octal number"))
		    (if (or (char<? c4 #\0) (char>? c4 #\7))
			(panic "Unfinished octal number"))
		    (read-exact-octal-integer
		     (- (char->integer c4) (char->integer #\0)))))
		  ((char=? c3 #\-)
		   (let ((c4 (read-char port)))
		    (if (eof-object? c4)
			(panic "EOF while reading octal number"))
		    (if (or (char<? c4 #\0) (char>? c4 #\7))
			(panic "Unfinished octal number"))
		    (- (read-exact-octal-integer
			(- (char->integer c4) (char->integer #\0))))))
		  (else (panic "Unfinished octal number")))))
	  ((or (char=? c2 #\d) (char=? c2 #\D))
	   (let ((c3 (read-char port)))
	    (if (eof-object? c3)
		(panic "EOF while reading decimal number"))
	    (cond
	     ((char=? c3 #\+)
	      (let ((c4 (read-char port)))
	       (if (eof-object? c4)
		   (panic "EOF while reading decimal number"))
	       (cond
		((char-numeric? c4)
		 (read-number (- (char->integer c4) (char->integer #\0))))
		((char=? c4 #\.)
		 (let ((c5 (read-char port)))
		  (if (eof-object? c5)
		      (panic "EOF while reading decimal number"))
		  (if (not (char-numeric? c5))
		      (panic "Unfinished decimal number"))
		  (read-inexact-number
		   (/ (- (char->integer c5) (char->integer #\0)) 10.0)
		   100.0)))
		(else (panic "Unfinished decimal number")))))
	     ((char=? c3 #\-)
	      (let ((c4 (read-char port)))
	       (if (eof-object? c4)
		   (panic "EOF while reading decimal number"))
	       (cond ((char-numeric? c4)
		      (- (read-number
			  (- (char->integer c4) (char->integer #\0)))))
		     ((char=? c4 #\.)
		      (let ((c5 (read-char port)))
		       (if (eof-object? c5)
			   (panic "EOF while reading decimal number"))
		       (if (not (char-numeric? c5))
			   (panic "Unfinished decimal number"))
		       (- (read-inexact-number
			   (/ (- (char->integer c5) (char->integer #\0))
			      10.0)
			   100.0))))
		     (else (panic "Unfinished decimal number")))))
	     ((char=? c3 #\.)
	      (let ((c4 (read-char port)))
	       (if (eof-object? c4)
		   (panic "EOF while reading decimal number"))
	       (if (not (char-numeric? c4))
		   (panic "Unfinished decimal number"))
	       (read-inexact-number
		(/ (- (char->integer c4) (char->integer #\0)) 10.0) 100.0)))
	     ((char-numeric? c3)
	      (read-number (- (char->integer c3) (char->integer #\0))))
	     (else (panic "Unfinished decimal number")))))
	  ((or (char=? c2 #\x) (char=? c2 #\X))
	   (let ((c3 (read-char port)))
	    (if (eof-object? c3)
		(panic "EOF while reading hexadecimal number"))
	    (cond
	     ((char-numeric? c3)
	      (read-exact-hexadecimal-integer
	       (- (char->integer c3) (char->integer #\0))))
	     ((and (char>=? c3 #\a) (char<=? c3 #\f))
	      (read-exact-hexadecimal-integer
	       (+ (- (char->integer c3) (char->integer #\a)) 10)))
	     ((and (char>=? c3 #\A) (char<=? c3 #\F))
	      (read-exact-hexadecimal-integer
	       (+ (- (char->integer c3) (char->integer #\A)) 10)))
	     ((char=? c3 #\+)
	      (let ((c4 (read-char port)))
	       (if (eof-object? c4)
		   (panic "EOF while reading hexadecimal number"))
	       (cond ((char-numeric? c4)
		      (read-exact-hexadecimal-integer
		       (- (char->integer c4) (char->integer #\0))))
		     ((and (char>=? c4 #\a) (char<=? c4 #\f))
		      (read-exact-hexadecimal-integer
		       (+ (- (char->integer c4) (char->integer #\a)) 10)))
		     ((and (char>=? c4 #\A) (char<=? c4 #\F))
		      (read-exact-hexadecimal-integer
		       (+ (- (char->integer c4) (char->integer #\A)) 10)))
		     (else (panic "Unfinished hexadecimal number")))))
	     ((char=? c3 #\-)
	      (let ((c4 (read-char port)))
	       (if (eof-object? c4)
		   (panic "EOF while reading hexadecimal number"))
	       (cond ((char-numeric? c4)
		      (- (read-exact-hexadecimal-integer
			  (- (char->integer c4) (char->integer #\0)))))
		     ((and (char>=? c4 #\a) (char<=? c4 #\f))
		      (- (read-exact-hexadecimal-integer
			  (+ (- (char->integer c4) (char->integer #\a)) 10))))
		     ((and (char>=? c4 #\A) (char<=? c4 #\F))
		      (- (read-exact-hexadecimal-integer
			  (+ (- (char->integer c4) (char->integer #\A)) 10))))
		     (else (panic "Unfinished hexadecimal number")))))
	     (else (panic "Unfinished hexadecimal number")))))
	  ((char=? c2 #\()
	   (let loop ((s '()))
	    (let ((e (read 'vector)))
	     ;; needs work: To eliminate LIST-REVERSE.
	     (if (eq? e close)
		 (list->vector (list-reverse s))
		 (loop (cons e s))))))
	  ((char=? c2 #\\)
	   (let ((c3 (read-char port)))
	    (if (eof-object? c3)
		(panic "EOF while reading character constant"))
	    (if (char-alphabetic? c3) (read-character-name (list c3)) c3)))
	  (else (panic "Improper character after sharp sign")))))
       ((char=? c1 #\")
	;; needs work: To eliminate LIST-REVERSE.
	(let loop ((s '()))
	 (let ((c (read-char port)))
	  (if (eof-object? c) (panic "EOF while reading string"))
	  (cond ((char=? c #\\)
		 (let ((c1 (read-char port)))
		  (if (eof-object? c1)
		      (panic "EOF after backslash in string"))
		  (loop (cons c1 s))))
		((char=? c #\") (list->string (list-reverse s)))
		(else (loop (cons c s)))))))
       ((char=? c1 #\+)
	(let ((c2 (peek-char port)))
	 (cond ((eof-object? c2) '+)
	       ((char-numeric? c2)
		(read-char port)
		(read-number (- (char->integer c2) (char->integer #\0))))
	       ((char=? c2 #\.)
		(read-char port)
		(let ((c3 (peek-char port)))
		 (cond ((eof-object? c3) '\+.)
		       ((char-numeric? c3)
			(read-char port)
			(read-inexact-number
			 (/ (- (char->integer c3) (char->integer #\0)) 10.0)
			 100.0))
		       ((char-subsequent? c3)
			(read-char port)
			(read-symbol (list (char-upcase c3)
					   (char-upcase c2)
					   (char-upcase c1))))
		       (else '\+.))))
	       ((char-subsequent? c2)
		(read-char port)
		(read-symbol (list (char-upcase c2) (char-upcase c1))))
	       (else '+))))
       ((char=? c1 #\-)
	(let ((c2 (peek-char port)))
	 (cond ((eof-object? c2) '-)
	       ((char-numeric? c2)
		(read-char port)
		(- (read-number (- (char->integer c2) (char->integer #\0)))))
	       ((char=? c2 #\.)
		(read-char port)
		(let ((c3 (peek-char port)))
		 (cond
		  ((eof-object? c3) '\-.)
		  ((char-numeric? c3)
		   (read-char port)
		   (- (read-inexact-number
		       (/ (- (char->integer c3) (char->integer #\0)) 10.0)
		       100.0)))
		  ((char-subsequent? c3)
		   (read-char port)
		   (read-symbol (list (char-upcase c3)
				      (char-upcase c2)
				      (char-upcase c1))))
		  (else '\-.))))
	       ((char-subsequent? c2)
		(read-char port)
		(read-symbol (list (char-upcase c2) (char-upcase c1))))
	       (else '-))))
       ((char=? c1 #\.)
	(let ((c2 (peek-char port)))
	 (if (eof-object? c2) (panic "EOF after dot"))
	 (cond ((char-numeric? c2)
		(read-char port)
		(read-inexact-number
		 (/ (- (char->integer c2) (char->integer #\0)) 10.0) 100.0))
	       ((char-subsequent? c2)
		(read-char port)
		(read-symbol (list (char-upcase c2) (char-upcase c1))))
	       ((eq? state 'list) dot)
	       (else (panic "Dot allowed only inside list")))))
       ((char-numeric? c1)
	(read-number (- (char->integer c1) (char->integer #\0))))
       ((char-initial? c1) (read-symbol (list (char-upcase c1))))
       (else (panic "Attempt to READ invalid character"))))))))

(define *i/o*
 ;; needs work:
 ;;  1. DISPLAY-{STRING,EXACT-INTEGER,MANTISSA-EXPONENT,INEXACT-REAL}1,
 ;;     WRITE1
 ;;  2. recursive calls to DISPLAY-INEXACT-REAL2, WRITE2, and DISPLAY2
 ;;     should be optimized to not pass PORT
 ;;  3. check implementation of remainder and modulo
 ;;  4. rectangular numbers
 ;;  5. -0.0
 ;;  6. should be able to print inexact numbers in non-scientific notation
 '(let ((buffer (make-vector 20))	;enough for 64-bit numbers
	(the-current-input-port ((primitive-procedure standard-input-port)))
	(the-current-output-port ((primitive-procedure standard-output-port)))
	(write-methods '())
	(display-methods '()))
   (define read-char1 (primitive-procedure read-char1))
   (define peek-char1 (primitive-procedure peek-char1))
   (define char-ready?1 (primitive-procedure char-ready?1))
   (define write-char2 (primitive-procedure write-char2))
   (define (display-string2 string port)
    (let ((n (string-length string)))
     (let loop ((i 0))
      (if (< i n)
	  (begin (write-char2 (string-ref string i) port) (loop (+ i 1)))))))
   (define (display-exact-integer2 number port)
    (cond
     ((positive? number)
      (let loop ((i 0) (number number))
       (if (zero? number)
	   (let loop ((i (- i 1)))
	    (write-char2
	     (integer->char (+ (char->integer #\0) (vector-ref buffer i)))
	     port)
	    (if (not (zero? i)) (loop (- i 1))))
	   (begin (vector-set! buffer i (remainder number 10))
		  (loop (+ i 1) (quotient number 10))))))
     ((negative? number)
      (write-char2 #\- port)
      (let loop ((i 0) (number number))
       (if (zero? number)
	   (let loop ((i (- i 1)))
	    (write-char2
	     (integer->char (+ (char->integer #\0) (vector-ref buffer i)))
	     port)
	    (if (not (zero? i)) (loop (- i 1))))
	   (begin (vector-set! buffer i (- (remainder number 10)))
		  (loop (+ i 1) (quotient number 10))))))
     (else (write-char2 #\0 port))))
   (define (display-mantissa-exponent2 mantissa exponent port)
    (let* ((float-digit (floor mantissa))
	   (digit (inexact->exact float-digit)))
     ;; needs work: This is a real kludge.
     (cond
      ((= digit 0)
       (display-mantissa-exponent2 (* 10.0 mantissa) (- exponent 1) port))
      ((= digit 10)
       (display-mantissa-exponent2 (* 0.1 mantissa) (+ exponent 1) port))
      (else (write-char2 (integer->char (+ (char->integer #\0) digit)) port)
	    (write-char2 #\. port)
	    (let loop ((mantissa (* 10.0 (- mantissa float-digit))))
	     (let* ((float-digit (floor mantissa))
		    (digit (inexact->exact float-digit)))
	      (write-char2 (integer->char (+ (char->integer #\0) digit)) port)
	      (let ((mantissa (* 10.0 (- mantissa float-digit))))
	       (if (not (zero? mantissa)) (loop mantissa)))))
	    (if (not (zero? exponent))
		(begin (write-char2 #\e port)
		       (display-exact-integer2 exponent port)))))))
   (define (display-inexact-real2 number port)
    (cond ((not (= number number)) (display-string2 "#*NOT-A-NUMBER*" port))
	  (((primitive-procedure infinity?) number)
	   (display-string2 "#*INFINITY*" port))
	  ((negative? number)
	   (write-char2 #\- port)
	   (display-inexact-real2 (- number) port))
	  ((zero? number) (display-string2 "0.0" port))
	  ((>= number 10.0)
	   (let loop ((mantissa (* 0.1 number)) (exponent 1))
	    (if (>= mantissa 10.0)
		(loop (* 0.1 mantissa) (+ exponent 1))
		(display-mantissa-exponent2 mantissa exponent port))))
	  ((< number 1.0)
	   (let loop ((mantissa (* 10.0 number)) (exponent -1))
	    (if (< mantissa 1.0)
		(loop (* 10.0 mantissa) (- exponent 1))
		(display-mantissa-exponent2 mantissa exponent port))))
	  (else (display-mantissa-exponent2 number 0 port))))
   (define (write2 obj port)
    (cond ((null? obj) (display-string2 "()" port))
	  ((eq? obj #t) (display-string2 "#T" port))
	  ((not obj) (display-string2 "#F" port))
	  ((char? obj)
	   ;; needs work: To handle other non printing characters.
	   (cond ((char=? obj #\space) (display-string2 "#\\Space" port))
		 ((char=? obj #\newline) (display-string2 "#\\Newline" port))
		 (else (display-string2 "#\\" port)
		       (write-char2 obj port))))
	  ((number? obj)
	   (if (exact? obj)
	       (display-exact-integer2 obj port)
	       (display-inexact-real2 obj port)))
	  ((input-port? obj) (display-string2 "#*INPUT-PORT*" port))
	  ((output-port? obj) (display-string2 "#*OUTPUT-PORT*" port))
	  ((eof-object? obj) (display-string2 "#*EOF-OBJECT*" port))
	  ((pointer? obj) (display-string2 "#*POINTER*" port))
	  ((symbol? obj)
	   ;; needs work: Should slashify.
	   (display-string2 (symbol->string obj) port))
	  ((procedure? obj) (display-string2 "#*PROCEDURE*" port))
	  ((string? obj)
	   (write-char2 #\" port)
	   (let ((n (string-length obj)))
	    (let loop ((i 0))
	     (if (< i n)
		 (let ((c (string-ref obj i)))
		  (if (or (char=? c #\\) (char=? c #\"))
		      (write-char2 #\\ port))
		  (write-char2 c port)
		  (loop (+ i 1))))))
	   (write-char2 #\" port))
	  ((pair? obj)
	   (write-char2 #\( port)
	   (let loop ((obj obj))
	    (cond ((null? (cdr obj))
		   (write2 (car obj) port)
		   (write-char2 #\) port))
		  ((pair? (cdr obj))
		   (write2 (car obj) port)
		   (write-char2 #\space port)
		   (loop (cdr obj)))
		  (else (write2 (car obj) port)
			(display-string2 " . " port)
			(write2 (cdr obj) port)
			(write-char2 #\) port)))))
	  ((vector? obj)
	   (display-string2 "#(" port)
	   (if (not (zero? (vector-length obj)))
	       (begin (write2 (vector-ref obj 0) port)
		      (let loop ((i 1))
		       (if (< i (vector-length obj))
			   (begin (write-char2 #\space port)
				  (write2 (vector-ref obj i) port)
				  (loop (+ i 1)))))))
	   (write-char2 #\) port))
	  (else (let loop ((write-methods write-methods))
		 (cond ((null? write-methods)
			(display-string2 "#*UNKNOWN*" port))
		       (((car (car write-methods)) obj)
			((cdr (car write-methods)) obj port))
		       (else (loop (cdr write-methods))))))))
   (define (display2 obj port)
    (cond ((null? obj) (display-string2 "()" port))
	  ((eq? obj #t) (display-string2 "#T" port))
	  ((not obj) (display-string2 "#F" port))
	  ((char? obj) (write-char2 obj port))
	  ((number? obj)
	   (if (exact? obj)
	       (display-exact-integer2 obj port)
	       (display-inexact-real2 obj port)))
	  ((input-port? obj) (display-string2 "#*INPUT-PORT*" port))
	  ((output-port? obj) (display-string2 "#*OUTPUT-PORT*" port))
	  ((eof-object? obj) (display-string2 "#*EOF-OBJECT*" port))
	  ((pointer? obj) (display-string2 "#*POINTER*" port))
	  ((symbol? obj) (display-string2 (symbol->string obj) port))
	  ((procedure? obj) (display-string2 "#*PROCEDURE*" port))
	  ((string? obj) (display-string2 obj port))
	  ((pair? obj)
	   (write-char2 #\( port)
	   (let loop ((obj obj))
	    (cond ((null? (cdr obj))
		   (display2 (car obj) port)
		   (write-char2 #\) port))
		  ((pair? (cdr obj))
		   (display2 (car obj) port)
		   (write-char2 #\space port)
		   (loop (cdr obj)))
		  (else (display2 (car obj) port)
			(display-string2 " . " port)
			(display2 (cdr obj) port)
			(write-char2 #\) port)))))
	  ((vector? obj)
	   (display-string2 "#(" port)
	   (if (not (zero? (vector-length obj)))
	       (begin (display2 (vector-ref obj 0) port)
		      (let loop ((i 1))
		       (if (< i (vector-length obj))
			   (begin (write-char2 #\space port)
				  (display2 (vector-ref obj i) port)
				  (loop (+ i 1)))))))
	   (write-char2 #\) port))
	  (else (let loop ((display-methods display-methods))
		 (cond ((null? display-methods)
			(display-string2 "#*UNKNOWN*" port))
		       (((car (car display-methods)) obj)
			((cdr (car display-methods)) obj port))
		       (else (loop (cdr display-methods))))))))
   (set!
    number->string
    ;; Bengt Kleberg <bengt@softwell.se> contributed the enhancements to
    ;; NUMBER->STRING to support the optional radix argument.
    (lambda (number . optional-radix)
     (let* ((radix-exact
	     (if (null? optional-radix)
		 10
		 (let ((probably-radix (car optional-radix)))
		  (if (and (integer? probably-radix)
			   (exact? probably-radix)
			   (or (= probably-radix 10)
			       (and (integer? number)
				    (exact? number)
				    (or (= probably-radix 2)
					(= probably-radix 8)
					(= probably-radix 16)))))
		      probably-radix
		      (panic
		       "Attempt to call NUMBER->STRING with invalid radix")))))
	    (radix-inexact (exact->inexact radix-exact))
	    (radix-inverted (/ radix-inexact)))
      ;; needs work: To handle optional radix argument better.
      (define (hex-digit->char digit)
       (vector-ref
	'#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F)
	digit))
      (define (mantissa-exponent->characters mantissa exponent)
       (let* ((float-digit (floor mantissa))
	      (digit (inexact->exact float-digit)))
	;; needs work: This is a real kludge.
	(cond
	 ((= digit 0)
	  (mantissa-exponent->characters
	   (* radix-inexact mantissa) (- exponent 1)))
	 ((= digit radix-exact)
	  (mantissa-exponent->characters
	   (* radix-inverted mantissa) (+ exponent 1)))
	 (else
	  (cons
	   (hex-digit->char digit)
	   (cons
	    #\.
	    (let loop ((mantissa (* radix-inexact (- mantissa float-digit))))
	     (let* ((float-digit (floor mantissa))
		    (digit (inexact->exact float-digit)))
	      (cons
	       (hex-digit->char digit)
	       (let ((mantissa (* radix-inexact (- mantissa float-digit))))
		(if (zero? mantissa)
		    (if (zero? exponent)
			'()
			(cons
			 #\e
			 (cond
			  ((positive? exponent)
			   (let loop ((exponent exponent) (characters '()))
			    (if (zero? exponent)
				characters
				(loop (quotient exponent radix-exact)
				      (cons (hex-digit->char
					     (remainder exponent radix-exact))
					    characters)))))
			  ((negative? exponent)
			   (let loop ((exponent exponent) (characters '()))
			    (if (zero? exponent)
				(cons #\- characters)
				(loop
				 (quotient exponent radix-exact)
				 (cons (hex-digit->char
					(- (remainder exponent radix-exact)))
				       characters))))))))
		    (loop mantissa))))))))))))
      ;; NUMBER->STRING body
      (cond
       ((inexact? number)
	(cond
	 ((not (= number number)) "#*NOT-A-NUMBER*")
	 (((primitive-procedure infinity?) number) "#*INFINITY*")
	 ((negative? number)
	  (let ((number (- number)))
	   (cond
	    ((not (= number number)) "-#*NOT-A-NUMBER*")
	    (((primitive-procedure infinity?) number) "-#*INFINITY*")
	    ((>= number radix-inexact)
	     (let loop ((mantissa (* radix-inverted number)) (exponent 1))
	      (if (>= mantissa radix-inexact)
		  (loop (* radix-inverted mantissa) (+ exponent 1))
		  (list->string
		   (cons
		    #\- (mantissa-exponent->characters mantissa exponent))))))
	    ((< number 1.0)
	     (let loop ((mantissa (* radix-inexact number)) (exponent -1))
	      (if (< mantissa 1.0)
		  (loop (* radix-inexact mantissa) (- exponent 1))
		  (list->string
		   (cons
		    #\- (mantissa-exponent->characters mantissa exponent))))))
	    (else (list->string
		   (cons #\- (mantissa-exponent->characters number 0)))))))
	 ((zero? number) "0.0")
	 ((>= number radix-inexact)
	  (let loop ((mantissa (* radix-inverted number)) (exponent 1))
	   (if (>= mantissa radix-inexact)
	       (loop (* radix-inverted mantissa) (+ exponent 1))
	       (list->string
		(mantissa-exponent->characters mantissa exponent)))))
	 ((< number 1.0)
	  (let loop ((mantissa (* radix-inexact number)) (exponent -1))
	   (if (< mantissa 1.0)
	       (loop (* radix-inexact mantissa) (- exponent 1))
	       (list->string
		(mantissa-exponent->characters mantissa exponent)))))
	 (else (list->string (mantissa-exponent->characters number 0)))))
       ((positive? number)
	(let loop ((number number) (characters '()))
	 (if (zero? number)
	     (list->string characters)
	     (loop (quotient number radix-exact)
		   (cons (hex-digit->char (remainder number radix-exact))
			 characters)))))
       ((negative? number)
	(let loop ((number number) (characters '()))
	 (if (zero? number)
	     (list->string (cons #\- characters))
	     (loop (quotient number radix-exact)
		   (cons (hex-digit->char (- (remainder number radix-exact)))
			 characters)))))
       (else "0")))))
   (set!
    string->number
    (lambda (string)
     ;; needs work: Long predecimal point digit strings can overflow.
     ;; needs work: Mantissa can overflow or underflow even though exponent
     ;;             would prevent that overflow or underflow.
     ;; needs work: Can't parse largest negative number.
     ;; needs work: To handle polar numbers with @.
     ;; needs work: To handle rectangular numbers with i.
     ;; needs work: To handle ratios with /.
     ;; needs work: To handle numbers with embedded #.
     ;; needs work: To handle exactness with #e and #i.
     ;; needs work: To handle optional radix argument.
     (let ((i 0)
	   (l (string-length string)))
      (define (negate n) (if n (- n) #f))
      (define (parse-exact-binary-integer n)
       (if (= i l)
	   n
	   (let ((c (string-ref string i)))
	    (set! i (+ i 1))
	    (cond ((char=? c #\0) (parse-exact-binary-integer (* 2 n)))
		  ((char=? c #\1) (parse-exact-binary-integer (+ (* 2 n) 1)))
		  (else #f)))))
      (define (parse-exact-octal-integer n)
       (if (= i l)
	   n
	   (let ((c (string-ref string i)))
	    (set! i (+ i 1))
	    (cond ((and (char>=? c #\0) (char<=? c #\7))
		   (parse-exact-octal-integer
		    (+ (* 8 n) (- (char->integer c) (char->integer #\0)))))
		  (else #f)))))
      (define (parse-exact-decimal-integer n)
       (if (= i l)
	   n
	   (let ((c (string-ref string i)))
	    (set! i (+ i 1))
	    (cond ((char-numeric? c)
		   (parse-exact-decimal-integer
		    (+ (* 10 n) (- (char->integer c) (char->integer #\0)))))
		  (else #f)))))
      (define (parse-exact-hexadecimal-integer n)
       (if (= i l)
	   n
	   (let ((c (string-ref string i)))
	    (set! i (+ i 1))
	    (cond ((char-numeric? c)
		   (parse-exact-hexadecimal-integer
		    (+ (* 16 n) (- (char->integer c) (char->integer #\0)))))
		  ((and (char>=? c #\a) (char<=? c #\f))
		   (parse-exact-hexadecimal-integer
		    (+ (* 16 n) (- (char->integer c) (char->integer #\a)) 10)))
		  ((and (char>=? c #\A) (char<=? c #\F))
		   (parse-exact-hexadecimal-integer
		    (+ (* 16 n) (- (char->integer c) (char->integer #\A)) 10)))
		  (else #f)))))
      (define (parse-inexact-number n m)
       (if (= i l)
	   n
	   (let ((c1 (string-ref string i)))
	    (set! i (+ i 1))
	    (cond
	     ((char-numeric? c1)
	      (parse-inexact-number
	       (+ n (/ (- (char->integer c1) (char->integer #\0)) m))
	       (* m 10.0)))
	     ((or (char=? c1 #\e) (char=? c1 #\E)
		  (char=? c1 #\s) (char=? c1 #\S)
		  (char=? c1 #\f) (char=? c1 #\F)
		  (char=? c1 #\d) (char=? c1 #\D)
		  (char=? c1 #\l) (char=? c1 #\L))
	      (if (= i l)
		  #f
		  (let ((c2 (string-ref string i)))
		   (set! i (+ i 1))
		   (cond
		    ((char-numeric? c2)
		     (let ((k (parse-exact-decimal-integer
			       (- (char->integer c2) (char->integer #\0)))))
		      (if k (* n (expt 10.0 k)) #f)))
		    ((char=? c2 #\+)
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (if (char-numeric? c3)
			      (let ((k (parse-exact-decimal-integer
					(- (char->integer c3)
					   (char->integer #\0)))))
			       (if k (* n (expt 10.0 k)) #f))
			      #f))))
		    ((char=? c2 #\-)
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (if (char-numeric? c3)
			      (let ((k (parse-exact-decimal-integer
					(- (char->integer c3)
					   (char->integer #\0)))))
			       (if k (* n (expt 10.0 (- k))) #f))
			      #f))))
		    (else #f)))))
	     (else #f)))))
      (define (parse-number n)
       (if (= i l)
	   n
	   (let ((c1 (string-ref string i)))
	    (set! i (+ i 1))
	    (cond
	     ((char-numeric? c1)
	      (parse-number
	       (+ (* 10 n) (- (char->integer c1) (char->integer #\0)))))
	     ((char=? c1 #\.) (parse-inexact-number (exact->inexact n) 10.0))
	     ((or (char=? c1 #\e) (char=? c1 #\E)
		  (char=? c1 #\s) (char=? c1 #\S)
		  (char=? c1 #\f) (char=? c1 #\F)
		  (char=? c1 #\d) (char=? c1 #\D)
		  (char=? c1 #\l) (char=? c1 #\L))
	      (if (= i l)
		  #f
		  (let ((c2 (string-ref string i)))
		   (set! i (+ i 1))
		   (cond
		    ((char-numeric? c2)
		     (let ((k (parse-exact-decimal-integer
			       (- (char->integer c2) (char->integer #\0)))))
		      (if k (* (exact->inexact n) (expt 10.0 k)))))
		    ((char=? c2 #\+)
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (if (char-numeric? c3)
			      (let ((k (parse-exact-decimal-integer
					(- (char->integer c3)
					   (char->integer #\0)))))
			       (if k (* (exact->inexact n) (expt 10.0 k)) #f))
			      #f))))
		    ((char=? c2 #\-)
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (if (char-numeric? c3)
			      (let ((k (parse-exact-decimal-integer
					(- (char->integer c3)
					   (char->integer #\0)))))
			       (if k
				   (* (exact->inexact n) (expt 10.0 (- k)))
				   #f))
			      #f))))
		    (else #f)))))
	     (else #f)))))
      (let loop ()
       (if (= i l)
	   #f
	   (let ((c1 (string-ref string i)))
	    (set! i (+ i 1))
	    (cond
	     ((char-whitespace? c1) (loop))
	     ((char=? c1 #\#)
	      (if (= i l)
		  #f
		  (let ((c2 (string-ref string i)))
		   (set! i (+ i 1))
		   (cond
		    ((or (char=? c2 #\b) (char=? c2 #\B))
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (cond
			   ((char=? c3 #\0) (parse-exact-binary-integer 0))
			   ((char=? c3 #\1) (parse-exact-binary-integer 1))
			   ((char=? c3 #\+)
			    (if (= i l)
				#f
				(let ((c4 (string-ref string i)))
				 (set! i (+ i 1))
				 (cond
				  ((char=? c4 #\0)
				   (parse-exact-binary-integer 0))
				  ((char=? c4 #\1)
				   (parse-exact-binary-integer 1))
				  (else #f)))))
			   ((char=? c3 #\-)
			    (if (= i l)
				#f
				(let ((c4 (string-ref string i)))
				 (set! i (+ i 1))
				 (cond
				  ((char=? c4 #\0)
				   (negate (parse-exact-binary-integer 0)))
				  ((char=? c4 #\1)
				   (negate (parse-exact-binary-integer 1)))
				  (else #f)))))
			   (else #f)))))
		    ((or (char=? c2 #\o) (char=? c2 #\O))
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (cond
			   ((and (char>=? c3 #\0) (char<=? c3 #\7))
			    (parse-exact-octal-integer
			     (- (char->integer c3) (char->integer #\0))))
			   ((char=? c3 #\+)
			    (if (= i l)
				#f
				(let ((c4 (string-ref string i)))
				 (set! i (+ i 1))
				 (if (and (char>=? c4 #\0) (char<=? c4 #\7))
				     (parse-exact-octal-integer
				      (- (char->integer c4)
					 (char->integer #\0)))
				     #f))))
			   ((char=? c3 #\-)
			    (if (= i l)
				#f
				(let ((c4 (string-ref string i)))
				 (set! i (+ i 1))
				 (if (and (char>=? c4 #\0) (char<=? c4 #\7))
				     (negate (parse-exact-octal-integer
					      (- (char->integer c4)
						 (char->integer #\0))))
				     #f))))
			   (else #f)))))
		    ((or (char=? c2 #\d) (char=? c2 #\D))
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (cond
			   ((char=? c3 #\+)
			    (if (= i l)
				#f
				(let ((c4 (string-ref string i)))
				 (set! i (+ i 1))
				 (cond
				  ((char-numeric? c4)
				   (parse-number
				    (- (char->integer c4)
				       (char->integer #\0))))
				  ((char=? c4 #\.)
				   (if (= i l)
				       #f
				       (let ((c5 (string-ref string i)))
					(set! i (+ i 1))
					(if (char-numeric? c5)
					    (parse-inexact-number
					     (/ (- (char->integer c5)
						   (char->integer #\0))
						10.0)
					     100.0)
					    #f))))
				  (else #f)))))
			   ((char=? c3 #\-)
			    (if (= i l)
				#f
				(let ((c4 (string-ref string i)))
				 (set! i (+ i 1))
				 (cond
				  ((char-numeric? c4)
				   (negate (parse-number
					    (- (char->integer c4)
					       (char->integer #\0)))))
				  ((char=? c4 #\.)
				   (if (= i l)
				       #f
				       (let ((c5 (string-ref string i)))
					(set! i (+ i 1))
					(if (char-numeric? c5)
					    (negate
					     (parse-inexact-number
					      (/ (- (char->integer c5)
						    (char->integer #\0))
						 10.0)
					      100.0))
					    #f))))
				  (else #f)))))
			   ((char=? c3 #\.)
			    (if (= i l)
				#f
				(let ((c4 (string-ref string i)))
				 (set! i (+ i 1))
				 (if (char-numeric? c4)
				     (parse-inexact-number
				      (/ (- (char->integer c4)
					    (char->integer #\0))
					 10.0)
				      100.0)
				     #f))))
			   ((char-numeric? c3)
			    (parse-number
			     (- (char->integer c3) (char->integer #\0))))
			   (else #f)))))
		    ((or (char=? c2 #\x) (char=? c2 #\X))
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (cond
			   ((char-numeric? c3)
			    (parse-exact-hexadecimal-integer
			     (- (char->integer c3) (char->integer #\0))))
			   ((and (char>=? c3 #\a) (char<=? c3 #\f))
			    (parse-exact-hexadecimal-integer
			     (+ (- (char->integer c3) (char->integer #\a))
				10)))
			   ((and (char>=? c3 #\A) (char<=? c3 #\F))
			    (parse-exact-hexadecimal-integer
			     (+ (- (char->integer c3) (char->integer #\A))
				10)))
			   ((char=? c3 #\+)
			    (if (= i l)
				#f
				(let ((c4 (string-ref string i)))
				 (set! i (+ i 1))
				 (cond
				  ((char-numeric? c4)
				   (parse-exact-hexadecimal-integer
				    (- (char->integer c4)
				       (char->integer #\0))))
				  ((and (char>=? c4 #\a) (char<=? c4 #\f))
				   (parse-exact-hexadecimal-integer
				    (+ (- (char->integer c4)
					  (char->integer #\a))
				       10)))
				  ((and (char>=? c4 #\A) (char<=? c4 #\F))
				   (parse-exact-hexadecimal-integer
				    (+ (- (char->integer c4)
					  (char->integer #\A))
				       10)))
				  (else #f)))))
			   ((char=? c3 #\-)
			    (if (= i l)
				#f
				(let ((c4 (string-ref string i)))
				 (set! i (+ i 1))
				 (cond
				  ((char-numeric? c4)
				   (negate (parse-exact-hexadecimal-integer
					    (- (char->integer c4)
					       (char->integer #\0)))))
				  ((and (char>=? c4 #\a) (char<=? c4 #\f))
				   (negate
				    (parse-exact-hexadecimal-integer
				     (+ (- (char->integer c4)
					   (char->integer #\a))
					10))))
				  ((and (char>=? c4 #\A) (char<=? c4 #\F))
				   (negate (parse-exact-hexadecimal-integer
					    (+ (- (char->integer c4)
						  (char->integer #\A))
					       10))))
				  (else #f)))))
			   (else #f)))))
		    (else #f)))))
	     ((char=? c1 #\+)
	      (if (= i l)
		  #f
		  (let ((c2 (string-ref string i)))
		   (set! i (+ i 1))
		   (cond
		    ((char-numeric? c2)
		     (parse-number (- (char->integer c2) (char->integer #\0))))
		    ((char=? c2 #\.)
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (if (char-numeric? c3)
			      (parse-inexact-number
			       (/ (- (char->integer c3) (char->integer #\0))
				  10.0)
			       100.0)
			      #f))))
		    (else #f)))))
	     ((char=? c1 #\-)
	      (if (= i l)
		  #f
		  (let ((c2 (string-ref string i)))
		   (set! i (+ i 1))
		   (cond
		    ((char-numeric? c2)
		     (negate (parse-number
			      (- (char->integer c2) (char->integer #\0)))))
		    ((char=? c2 #\.)
		     (if (= i l)
			 #f
			 (let ((c3 (string-ref string i)))
			  (set! i (+ i 1))
			  (if (char-numeric? c3)
			      (negate
			       (parse-inexact-number
				(/ (- (char->integer c3) (char->integer #\0))
				   10.0)
				100.0))
			      #f))))
		    (else #f)))))
	     ((char=? c1 #\.)
	      (if (= i l)
		  #f
		  (let ((c2 (string-ref string i)))
		   (set! i (+ i 1))
		   (if (char-numeric? c2)
		       (parse-inexact-number
			(/ (- (char->integer c2) (char->integer #\0)) 10.0)
			100.0)
		       #f))))
	     ((char-numeric? c1)
	      (parse-number (- (char->integer c1) (char->integer #\0))))
	     (else #f))))))))
   (set! current-input-port (lambda () the-current-input-port))
   (set! current-output-port (lambda () the-current-output-port))
   (set! with-input-from-file
	 (lambda (string thunk)
	  (let* ((saved-input-port the-current-input-port)
		 (result (call-with-input-file string
			  (lambda (port)
			   (set! the-current-input-port port)
			   (thunk)))))
	   (set! the-current-input-port saved-input-port)
	   result)))
   (set! with-output-to-file
	 (lambda (string thunk)
	  (let* ((saved-output-port the-current-output-port)
		 (result (call-with-output-file string
			  (lambda (port)
			   (set! the-current-output-port port)
			   (thunk)))))
	   (set! the-current-output-port saved-output-port)
	   result)))
   (set!
    read-char
    (lambda &rest
     (cond
      ((null? &rest) (read-char1 the-current-input-port))
      ((null? (cdr &rest)) (read-char1 (car &rest)))
      (else
       (panic
	"Attempt to call READ-CHAR with the wrong number of arguments")))))
   (set!
    peek-char
    (lambda &rest
     (cond
      ((null? &rest) (peek-char1 the-current-input-port))
      ((null? (cdr &rest)) (peek-char1 (car &rest)))
      (else
       (panic
	"Attempt to call PEEK-CHAR with the wrong number of arguments")))))
   (set!
    char-ready?
    (lambda &rest
     (cond
      ((null? &rest) (char-ready?1 the-current-input-port))
      ((null? (cdr &rest)) (char-ready?1 (car &rest)))
      (else
       (panic
	"Attempt to call CHAR-READY? with the wrong number of arguments")))))
   (set!
    write
    (lambda (obj . &rest)
     (cond
      ((null? &rest) (write2 obj the-current-output-port))
      ((null? (cdr &rest)) (write2 obj (car &rest)))
      (else
       (panic "Attempt to call WRITE with the wrong number of arguments")))))
   (set!
    display
    (lambda (obj . &rest)
     (cond
      ((null? &rest) (display2 obj the-current-output-port))
      ((null? (cdr &rest)) (display2 obj (car &rest)))
      (else
       (panic "Attempt to call DISPLAY with the wrong number of arguments")))))
   (set!
    newline
    (lambda &rest
     (cond
      ((null? &rest) (write-char2 #\newline the-current-output-port))
      ((null? (cdr &rest)) (write-char2 #\newline (car &rest)))
      (else
       (panic "Attempt to call NEWLINE with the wrong number of arguments")))))
   (set!
    write-char
    (lambda (char . &rest)
     (cond
      ((null? &rest) (write-char2 char the-current-output-port))
      ((null? (cdr &rest)) (write-char2 char (car &rest)))
      (else
       (panic
	"Attempt to call WRITE-CHAR with the wrong number of arguments")))))
   (set! define-write-method
	 (lambda (type? method)
	  (set! write-methods (cons (cons type? method) write-methods))))
   (set! define-display-method
	 (lambda (type? method)
	  (set! display-methods (cons (cons type? method) display-methods))))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
