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
(module stalin5e)

(include "QobiScheme.sch")
(include "stalin5e.sch")
;;; End delete for Trotsky

;;; Input/Output

(define (search-include-path-without-extension pathname)
 ;; conventions: PATHNAME
 (cond ((can-open-file-for-input? pathname) pathname)
       ((and (>= (string-length pathname) 1)
	     (char=? (string-ref pathname 0) #\/))
	(notify "Cannot find: ~a" pathname)
	(terminate))
       (else (let loop ((include-path *include-path*))
	      ;; conventions: INCLUDE-PATH
	      (cond ((null? include-path)
		     (notify "Cannot find: ~a" pathname)
		     (terminate))
		    ((can-open-file-for-input?
		      (string-append (first include-path) "/" pathname))
		     (string-append (first include-path) "/" pathname))
		    (else (loop (rest include-path))))))))

(define (search-include-path pathname)
 ;; conventions: PATHNAME
 (search-include-path-without-extension (default-extension pathname "sc")))

(define (read-s-expressions pathname)
 ;; conventions
 (let ((line-position 0)
       (character-position -1)
       (character-position-within-line -1)
       (newline? #t)
       (last-line-position 0)
       (last-character-position -1)
       (last-character-position-within-line -1)
       (last-newline? #t)
       (last-char #f)
       ;; needs work: The DOT and CLOSE gensyms should be extracted and bound
       ;;             by a LET that is outside the DEFINE of READ.
       (dot (gensym "dot"))
       (close (gensym "close")))
  (call-with-input-file pathname
   (lambda (port)
    (define (read-s-expression)
     ;; needs work: Long predecimal point digit strings can overflow.
     ;; needs work: Mantissa can overflow or underflow even though exponent
     ;;             would prevent that overflow or underflow.
     ;; needs work: Can't read largest negative number.
     ;; needs work: To handle polar numbers with @
     ;; needs work: To handle rectangular numbers with i
     ;; needs work: To handle ratios with /
     ;; needs work: To handle numbers with embedded #
     ;; needs work: To handle exactness with #e #i
     ;; needs work: To handle structures
     (define (read-error error)
      (notify "~a:~s:~a" pathname line-position error)
      (terminate))
     (define (unget-char c)
      (set! line-position last-line-position)
      (set! character-position last-character-position)
      (set! character-position-within-line last-character-position-within-line)
      (set! newline? last-newline?)
      (set! last-char c))
     (define (get-char)
      (set! last-line-position line-position)
      (set! last-character-position character-position)
      (set! last-character-position-within-line character-position-within-line)
      (set! last-newline? newline?)
      (cond (newline? (set! line-position (+ line-position 1))
		      (set! character-position-within-line 0))
	    (else (set! character-position-within-line
			(+ character-position-within-line 1))))
      (set! character-position (+ character-position 1))
      (let ((c (or last-char (read-char port))))
       (set! last-char #f)
       (set! newline? (and (not (eof-object? c)) (char=? c #\newline)))
       c))
     (let read ((state 'object) (comments '()))
      ;; conventions: STATE COMMENTS
      (define (read-exact-binary-integer n)
       (let ((c (get-char)))
	(cond ((eof-object? c) n)
	      ((char=? c #\0) (read-exact-binary-integer (* 2 n)))
	      ((char=? c #\1) (read-exact-binary-integer (+ (* 2 n) 1)))
	      (else (unget-char c) n))))
      (define (read-exact-octal-integer n)
       (let ((c (get-char)))
	(cond ((eof-object? c) n)
	      ((and (char>=? c #\0) (char<=? c #\7))
	       (read-exact-octal-integer
		(+ (* 8 n) (- (char->integer c) (char->integer #\0)))))
	      (else (unget-char c) n))))
      (define (read-exact-decimal-integer n)
       (let ((c (get-char)))
	(cond ((eof-object? c) n)
	      ((char-numeric? c)
	       (read-exact-decimal-integer
		(+ (* 10 n) (- (char->integer c) (char->integer #\0)))))
	      (else (unget-char c) n))))
      (define (read-exact-hexadecimal-integer n)
       (let ((c (get-char)))
	(cond ((eof-object? c) n)
	      ((char-numeric? c)
	       (read-exact-hexadecimal-integer
		(+ (* 16 n) (- (char->integer c) (char->integer #\0)))))
	      ((and (char>=? c #\a) (char<=? c #\f))
	       (read-exact-hexadecimal-integer
		(+ (* 16 n) (- (char->integer c) (char->integer #\a)) 10)))
	      ((and (char>=? c #\A) (char<=? c #\F))
	       (read-exact-hexadecimal-integer
		(+ (* 16 n) (- (char->integer c) (char->integer #\A)) 10)))
	      (else (unget-char c) n))))
      (define (read-inexact-number n m)
       (let ((c1 (get-char)))
	(cond
	 ((eof-object? c1) n)
	 ((char-numeric? c1)
	  (read-inexact-number
	   (+ n (/ (- (char->integer c1) (char->integer #\0)) m)) (* m 10.0)))
	 ((or (char=? c1 #\e) (char=? c1 #\E)
	      (char=? c1 #\s) (char=? c1 #\S)
	      (char=? c1 #\f) (char=? c1 #\F)
	      (char=? c1 #\d) (char=? c1 #\D)
	      (char=? c1 #\l) (char=? c1 #\L))
	  (let ((c2 (get-char)))
	   (when (eof-object? c2) (read-error "EOF while reading exponent"))
	   (cond
	    ((char-numeric? c2)
	     (* n (expt 10.0
			(read-exact-decimal-integer
			 (- (char->integer c2) (char->integer #\0))))))
	    ((char=? c2 #\+)
	     (let ((c3 (get-char)))
	      (when (eof-object? c3) (read-error "EOF while reading exponent"))
	      (unless (char-numeric? c3) (read-error "Unfinished exponent"))
	      (* n (expt 10.0
			 (read-exact-decimal-integer
			  (- (char->integer c3) (char->integer #\0)))))))
	    ((char=? c2 #\-)
	     (let ((c3 (get-char)))
	      (when (eof-object? c3) (read-error "EOF while reading exponent"))
	      (unless (char-numeric? c3) (read-error "Unfinished exponent"))
	      (* n (expt 10.0
			 (- (read-exact-decimal-integer
			     (- (char->integer c3) (char->integer #\0))))))))
	    (else (read-error "Unfinished exponent")))))
	 (else (unget-char c1) n))))
      (define (read-number n)
       (let ((c1 (get-char)))
	(cond
	 ((eof-object? c1) n)
	 ((char-numeric? c1)
	  (read-number
	   (+ (* 10 n) (- (char->integer c1) (char->integer #\0)))))
	 ((char=? c1 #\.) (read-inexact-number (exact->inexact n) 10.0))
	 ((or (char=? c1 #\e) (char=? c1 #\E)
	      (char=? c1 #\s) (char=? c1 #\S)
	      (char=? c1 #\f) (char=? c1 #\F)
	      (char=? c1 #\d) (char=? c1 #\D)
	      (char=? c1 #\l) (char=? c1 #\L))
	  (let ((c2 (get-char)))
	   (when (eof-object? c2) (read-error "EOF while reading exponent"))
	   (cond
	    ((char-numeric? c2)
	     (* (exact->inexact n)
		(expt 10.0
		      (read-exact-decimal-integer
		       (- (char->integer c2) (char->integer #\0))))))
	    ((char=? c2 #\+)
	     (let ((c3 (get-char)))
	      (when (eof-object? c3) (read-error "EOF while reading exponent"))
	      (unless (char-numeric? c3) (read-error "Unfinished exponent"))
	      (* (exact->inexact n)
		 (expt 10.0
		       (read-exact-decimal-integer
			(- (char->integer c3) (char->integer #\0)))))))
	    ((char=? c2 #\-)
	     (let ((c3 (get-char)))
	      (when (eof-object? c3) (read-error "EOF while reading exponent"))
	      (unless (char-numeric? c3) (read-error "Unfinished exponent"))
	      (* (exact->inexact n)
		 (expt 10.0
		       (- (read-exact-decimal-integer
			   (- (char->integer c3) (char->integer #\0))))))))
	    (else (read-error "Unfinished exponent")))))
	 (else (unget-char c1) n))))
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
       ;; needs work: To eliminate REVERSE.
       (let ((c (get-char)))
	(cond ((eof-object? c) (string->symbol (list->string (reverse s))))
	      ((char-subsequent? c) (read-symbol (cons (char-upcase c) s)))
	      (else (unget-char c)
		    (string->symbol (list->string (reverse s)))))))
      (define (lookup-character-name s)
       (let loop ((names '(((#\e #\c #\a #\p #\s) . #\space)
			   ((#\e #\n #\i #\l #\w #\e #\n) . #\newline))))
	(when (null? names) (read-error "Unrecognized character name"))
	(if (let loop? ((s s) (name (car (first names))))
	     (or (and (null? s) (null? name))
		 (and (not (null? s))
		      (not (null? name))
		      (char-ci=? (first s) (first name))
		      (loop? (rest s) (rest name)))))
	    (cdr (first names))
	    (loop (rest names)))))
      (define (read-character-name s)
       (let ((c (get-char)))
	(cond ((eof-object? c) (lookup-character-name s))
	      ((char-alphabetic? c) (read-character-name (cons c s)))
	      (else (unget-char c)
		    (if (and (not (null? s)) (null? (rest s)))
			(first s)
			(lookup-character-name s))))))
      (let* ((c1 (get-char))
	     (line-position line-position)
	     (character-position character-position)
	     (character-position-within-line character-position-within-line)
	     (datum
	      (cond
	       ((eof-object? c1)
		(case state
		 ((object) c1)
		 ((list) (read-error "EOF while reading list"))
		 ((vector) (read-error "EOF while reading vector"))
		 ((quote) (read-error "EOF while reading quoted object"))
		 ((quasiquote)
		  (read-error "EOF while reading quasiquoted object"))
		 ((unquote-splicing)
		  (read-error "EOF while reading unquote-slicing object"))
		 ((unquote) (read-error "EOF while reading unquoted object"))
		 ((close) (read-error "EOF while reading pair"))))
	       ((char=? c1 #\;)
		(let loop ((cs '(#\;)))
		 (let ((c1 (get-char)))
		  (if (char=? c1 #\newline)
		      (read state (cons (list->string (reverse cs)) comments))
		      (loop (cons c1 cs))))))
	       ((char=? c1 #\))
		(unless (or (eq? state 'list)
			    (eq? state 'vector)
			    (eq? state 'close))
		 (read-error "Mismatched closing parenthesis"))
		close)
	       ((char-whitespace? c1) (read state comments))
	       ((eq? state 'close)
		(read-error "Only one object allowed after dot"))
	       ((char=? c1 #\')
		(let ((s (create-s-expression
			  pathname
			  line-position
			  character-position
			  character-position-within-line
			  (reverse comments)
			  'quote)))
		 (cons s (create-anonymous-s-expression
			  (cons (read 'quote '())
				(create-anonymous-s-expression '()))))))
	       ((char=? c1 #\`)
		(let ((s (create-s-expression
			  pathname
			  line-position
			  character-position
			  character-position-within-line
			  (reverse comments)
			  'quasiquote)))
		 (cons s (create-anonymous-s-expression
			  (cons (read 'quasiquote '())
				(create-anonymous-s-expression '()))))))
	       ((char=? c1 #\,)
		(let ((c2 (get-char)))
		 (when (eof-object? c2) (read-error "EOF after dot"))
		 (cond
		  ((char=? c2 #\@)
		   (let ((s (create-s-expression
			     pathname
			     line-position
			     character-position
			     character-position-within-line
			     (reverse comments)
			     'unquote-splicing)))
		    (cons
		     s (create-anonymous-s-expression
			(cons (read 'unquote-splicing '())
			      (create-anonymous-s-expression '()))))))
		  (else (unget-char c2)
			(let ((s (create-s-expression
				  pathname
				  line-position
				  character-position
				  character-position-within-line
				  (reverse comments)
				  'unquote)))
			 (cons s (create-anonymous-s-expression
				  (cons (read 'unquote '())
					(create-anonymous-s-expression
					 '())))))))))
	       ((char=? c1 #\()
		;; needs work: Redundant consing.
		(let loop ((s '()))
		 (let ((e (read 'list '())))
		  (cond
		   ((eq? (s-expression-datum e) dot)
		    (when (null? s)
		     (read-error "Dot cannot be first element of list"))
		    (let* ((e1 (read 'object '()))
			   (e2 (read 'close '())))
		     (let loop ((s (rest s))
				(c (create-anonymous-s-expression
				    (cons (first s) e1))))
		      (if (null? s)
			  (sx-datum c)
			  (loop (rest s)
				(create-anonymous-s-expression
				 (cons (first s) c)))))))
		   ((eq? (s-expression-datum e) close)
		    (let loop ((s s) (c (create-anonymous-s-expression '())))
		     (if (null? s)
			 (sx-datum c)
			 (loop (rest s)
			       (create-anonymous-s-expression
				(cons (first s) c))))))
		   (else (loop (cons e s)))))))
	       ((char=? c1 #\#)
		(let ((c2 (get-char)))
		 (when (eof-object? c2) (read-error "EOF after sharp sign"))
		 (cond
		  ((or (char=? c2 #\t) (char=? c2 #\T)) #t)
		  ((or (char=? c2 #\f) (char=? c2 #\F)) #f)
		  ((or (char=? c2 #\b) (char=? c2 #\B))
		   (let ((c3 (get-char)))
		    (when (eof-object? c3)
		     (read-error "EOF while reading binary number"))
		    (cond
		     ((char=? c3 #\0) (read-exact-binary-integer 0))
		     ((char=? c3 #\1) (read-exact-binary-integer 1))
		     ((char=? c3 #\+)
		      (let ((c4 (get-char)))
		       (when (eof-object? c4)
			(read-error "EOF while reading binary number"))
		       (cond
			((char=? c4 #\0) (read-exact-binary-integer 0))
			((char=? c4 #\1) (read-exact-binary-integer 1))
			(else (read-error "Unfinished binary number")))))
		     ((char=? c3 #\-)
		      (let ((c4 (get-char)))
		       (when (eof-object? c4)
			(read-error "EOF while reading binary number"))
		       (cond
			((char=? c4 #\0) (- (read-exact-binary-integer 0)))
			((char=? c4 #\1) (- (read-exact-binary-integer 1)))
			(else (read-error "Unfinished binary number")))))
		     (else (read-error "Unfinished binary number")))))
		  ((or (char=? c2 #\o) (char=? c2 #\O))
		   (let ((c3 (get-char)))
		    (when (eof-object? c3)
		     (read-error "EOF while reading octal number"))
		    (cond ((and (char>=? c3 #\0) (char<=? c3 #\7))
			   (read-exact-octal-integer
			    (- (char->integer c3) (char->integer #\0))))
			  ((char=? c3 #\+)
			   (let ((c4 (get-char)))
			    (when (eof-object? c4)
			     (read-error "EOF while reading octal number"))
			    (unless (and (char>=? c4 #\0) (char<=? c4 #\7))
			     (read-error "Unfinished octal number"))
			    (read-exact-octal-integer
			     (- (char->integer c4) (char->integer #\0)))))
			  ((char=? c3 #\-)
			   (let ((c4 (get-char)))
			    (when (eof-object? c4)
			     (read-error "EOF while reading octal number"))
			    (unless (and (char>=? c4 #\0) (char<=? c4 #\7))
			     (read-error "Unfinished octal number"))
			    (- (read-exact-octal-integer
				(- (char->integer c4) (char->integer #\0))))))
			  (else (read-error "Unfinished octal number")))))
		  ((or (char=? c2 #\d) (char=? c2 #\D))
		   (let ((c3 (get-char)))
		    (when (eof-object? c3)
		     (read-error "EOF while reading decimal number"))
		    (cond
		     ((char=? c3 #\+)
		      (let ((c4 (get-char)))
		       (when (eof-object? c4)
			(read-error "EOF while reading decimal number"))
		       (cond
			((char-numeric? c4)
			 (read-number
			  (- (char->integer c4) (char->integer #\0))))
			((char=? c4 #\.)
			 (let ((c5 (get-char)))
			  (when (eof-object? c5)
			   (read-error "EOF while reading decimal number"))
			  (unless (char-numeric? c5)
			   (read-error "Unfinished decimal number"))
			  (read-inexact-number
			   (/ (- (char->integer c5) (char->integer #\0)) 10.0)
			   100.0)))
			(else (read-error "Unfinished decimal number")))))
		     ((char=? c3 #\-)
		      (let ((c4 (get-char)))
		       (when (eof-object? c4)
			(read-error "EOF while reading decimal number"))
		       (cond
			((char-numeric? c4)
			 (- (read-number
			     (- (char->integer c4) (char->integer #\0)))))
			((char=? c4 #\.)
			 (let ((c5 (get-char)))
			  (when (eof-object? c5)
			   (read-error "EOF while reading decimal number"))
			  (unless (char-numeric? c5)
			   (read-error "Unfinished decimal number"))
			  (- (read-inexact-number
			      (/ (- (char->integer c5) (char->integer #\0))
				 10.0)
			      100.0))))
			(else (read-error "Unfinished decimal number")))))
		     ((char=? c3 #\.)
		      (let ((c4 (get-char)))
		       (when (eof-object? c4)
			(read-error "EOF while reading decimal number"))
		       (unless (char-numeric? c4)
			(read-error "Unfinished decimal number"))
		       (read-inexact-number
			(/ (- (char->integer c4) (char->integer #\0)) 10.0)
			100.0)))
		     ((char-numeric? c3)
		      (read-number (- (char->integer c3) (char->integer #\0))))
		     (else (read-error "Unfinished decimal number")))))
		  ((or (char=? c2 #\x) (char=? c2 #\X))
		   (let ((c3 (get-char)))
		    (when (eof-object? c3)
		     (read-error "EOF while reading hexadecimal number"))
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
		      (let ((c4 (get-char)))
		       (when (eof-object? c4)
			(read-error "EOF while reading hexadecimal number"))
		       (cond
			((char-numeric? c4)
			 (read-exact-hexadecimal-integer
			  (- (char->integer c4) (char->integer #\0))))
			((and (char>=? c4 #\a) (char<=? c4 #\f))
			 (read-exact-hexadecimal-integer
			  (+ (- (char->integer c4) (char->integer #\a)) 10)))
			((and (char>=? c4 #\A) (char<=? c4 #\F))
			 (read-exact-hexadecimal-integer
			  (+ (- (char->integer c4) (char->integer #\A)) 10)))
			(else (read-error "Unfinished hexadecimal number")))))
		     ((char=? c3 #\-)
		      (let ((c4 (get-char)))
		       (when (eof-object? c4)
			(read-error "EOF while reading hexadecimal number"))
		       (cond
			((char-numeric? c4)
			 (- (read-exact-hexadecimal-integer
			     (- (char->integer c4) (char->integer #\0)))))
			((and (char>=? c4 #\a) (char<=? c4 #\f))
			 (- (read-exact-hexadecimal-integer
			     (+ (- (char->integer c4) (char->integer #\a))
				10))))
			((and (char>=? c4 #\A) (char<=? c4 #\F))
			 (- (read-exact-hexadecimal-integer
			     (+ (- (char->integer c4) (char->integer #\A))
				10))))
			(else (read-error "Unfinished hexadecimal number")))))
		     (else (read-error "Unfinished hexadecimal number")))))
		  ((char=? c2 #\()
		   (let loop ((s '()))
		    (let ((e (read 'vector '())))
		     ;; needs work: To eliminate REVERSE.
		     (if (eq? (s-expression-datum e) close)
			 (list->vector (reverse s))
			 (loop (cons e s))))))
		  ((char=? c2 #\\)
		   (let ((c3 (get-char)))
		    (when (eof-object? c3)
		     (read-error "EOF while reading character constant"))
		    (if (char-alphabetic? c3)
			(read-character-name (list c3))
			c3)))
		  (else (read-error "Improper character after sharp sign")))))
	       ((char=? c1 #\")
		;; needs work: To eliminate REVERSE.
		(let loop ((s '()))
		 (let ((c (get-char)))
		  (when (eof-object? c)
		   (read-error "EOF while reading string"))
		  (cond ((char=? c #\\)
			 (let ((c1 (get-char)))
			  (when (eof-object? c1)
			   (read-error "EOF after backslash in string"))
			  (loop (cons c1 s))))
			((char=? c #\") (list->string (reverse s)))
			(else (loop (cons c s)))))))
	       ((char=? c1 #\+)
		(let ((c2 (get-char)))
		 (cond
		  ((eof-object? c2) '+)
		  ((char-numeric? c2)
		   (read-number (- (char->integer c2) (char->integer #\0))))
		  ((char=? c2 #\.)
		   (let ((c3 (get-char)))
		    (cond ((eof-object? c3) '\+.)
			  ((char-numeric? c3)
			   (read-inexact-number
			    (/ (- (char->integer c3) (char->integer #\0)) 10.0)
			    100.0))
			  ((char-subsequent? c3)
			   (read-symbol (list (char-upcase c3)
					      (char-upcase c2)
					      (char-upcase c1))))
			  (else (unget-char c3) '\+.))))
		  ((char-subsequent? c2)
		   (read-symbol (list (char-upcase c2) (char-upcase c1))))
		  (else (unget-char c2) '+))))
	       ((char=? c1 #\-)
		(let ((c2 (get-char)))
		 (cond
		  ((eof-object? c2) '-)
		  ((char-numeric? c2)
		   (- (read-number
		       (- (char->integer c2) (char->integer #\0)))))
		  ((char=? c2 #\.)
		   (let ((c3 (get-char)))
		    (cond
		     ((eof-object? c3) '\-.)
		     ((char-numeric? c3)
		      (- (read-inexact-number
			  (/ (- (char->integer c3) (char->integer #\0)) 10.0)
			  100.0)))
		     ((char-subsequent? c3)
		      (read-symbol (list (char-upcase c3)
					 (char-upcase c2)
					 (char-upcase c1))))
		     (else (unget-char c3) '\-.))))
		  ((char-subsequent? c2)
		   (read-symbol (list (char-upcase c2) (char-upcase c1))))
		  (else (unget-char c2) '-))))
	       ((char=? c1 #\.)
		(let ((c2 (get-char)))
		 (when (eof-object? c2) (read-error "EOF after dot"))
		 (cond
		  ((char-numeric? c2)
		   (read-inexact-number
		    (/ (- (char->integer c2) (char->integer #\0)) 10.0) 100.0))
		  ((char-subsequent? c2)
		   (read-symbol (list (char-upcase c2) (char-upcase c1))))
		  ((eq? state 'list) (unget-char c2) dot)
		  (else (read-error "Dot allowed only inside list")))))
	       ((char-numeric? c1)
		(read-number (- (char->integer c1) (char->integer #\0))))
	       ((char-initial? c1) (read-symbol (list (char-upcase c1))))
	       (else (read-error "Attempt to READ invalid character")))))
       (if (s-expression? datum)
	   datum
	   (create-s-expression pathname
				line-position
				character-position
				character-position-within-line
				(reverse comments)
				datum)))))
    (let loop ((ss '()))
     (let ((s (read-s-expression)))
      (cond
       ((eof-object? (s-expression-datum s)) (reverse ss))
       ((and (sx-list? s)
	     (= (sx-length s) 2)
	     (sx-eq? (sx-first s) 'include)
	     (sx-string? (sx-second s)))
	(let ((pathname (search-include-path (sx-datum (sx-second s)))))
	 ;; conventions: PATHNAME
	 (cond
	  ((member pathname *includes*) (loop ss))
	  (else (set! *includes* (cons pathname *includes*))
		(loop (append (reverse (read-s-expressions pathname)) ss))))))
       (else (loop (cons s ss))))))))))

(define (generate c pathname spitter)
 ;; note: This will not handle braces inside comments.
 ;; conventions: PATHNAME SPITTER
 (call-with-output-file (replace-extension pathname "c")
  (lambda (port)
   (spitter port)
   ;; conventions: PORT
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
     (write-char c port))
    (let loop ((c c))
     (cond ((char? c)
	    (unless (char=? c #\newline) (fuck-up))
	    (unless (or newline? open?)
	     (newline port)
	     (for-each-n (lambda (i)
			  ;; conventions: I
			  (write-char #\space port))
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
	   (else (fuck-up))))))))

;;; The compiler top level

;;; Options

(define *Scheme->C-compatibility?* #f)
(define *Xlib-and-GL?* #f)
(define *QobiScheme?* #f)
(define *Trotsky?* #f)
(define *treat-all-symbols-as-external?* #f)
(define *index-allocated-string-types-by-expression?* #f)
(define *index-constant-structure-types-by-slot-types?* #f)
(define *index-constant-structure-types-by-expression?* #f)
(define *index-allocated-structure-types-by-slot-types?* #f)
(define *index-allocated-structure-types-by-expression?* #f)
(define *index-constant-headed-vector-types-by-element-type?* #f)
(define *index-constant-headed-vector-types-by-expression?* #f)
(define *index-allocated-headed-vector-types-by-element-type?* #f)
(define *index-allocated-headed-vector-types-by-expression?* #f)
(define *index-constant-nonheaded-vector-types-by-element-type?* #f)
(define *index-constant-nonheaded-vector-types-by-expression?* #f)
(define *index-allocated-nonheaded-vector-types-by-element-type?* #f)
(define *index-allocated-nonheaded-vector-types-by-expression?* #f)
(define *clone-size-limit* 0)
(define *split-even-if-no-widening?* #f)
(define *fully-convert-to-CPS?* #f)
(define *no-escaping-continuations?* #f)
(define *uniqueness?* #f)
(define *bounds-checks?* #f)
(define *memory-checks?* #f)
(define *overflow-checks?* #f)
(define *runtime-checks?* #f)
(define *type-checks?* #f)
(define *p1?* #f)
(define *p2?* #f)
(define *p3?* #f)
(define *p4?* #f)
(define *p5?* #f)
(define *p6?* #f)
(define *p7?* #f)
(define *closure-conversion-statistics?* #f)
(define *stack-allocation?* #f)
(define *heap-allocation?* #f)
(define *region-allocation?* #f)
(define *memory-messages?* #f)
(define *expandable-regions?* #f)
(define *flonum-representation* #f)
(define *architecture-name* #f)
(define *closure-conversion-method* #f)
(define *closure-representation* #f)
(define *align-strings?* #f)
(define *eq?-forgery?* #f)
(define *forgery?* #f)
(define *globals?* #f)
(define *type-if?* #f)
(define *immediate-structures?* #f)
(define *promote-representations?* #f)
(define *copy-propagation?* #f)
(define *squeeze?* #f)
(define *squish?* #f)
(define *treadmarks?* #f)
(define *tail-call-optimization?* #f)
(define *database?* #f)
(define *run-cc?* #f)
(define *keep-c?* #f)
(define *cc* "")
(define *copts* '())

;;; Global variables

(define *include-path* '())
(define *includes* '())
(define *herald* #f)
(define *heralds* '())
(define *program-has-pthreads?* #f)
(define *current-architecture-name* #f)

(define (current-architecture-name)
 (unless *current-architecture-name*
  (system
   (format #f "~a >/tmp/QobiScheme.tmp"
	   (search-include-path-without-extension "stalin-architecture-name")))
  (set! *current-architecture-name* (first (read-file "/tmp/QobiScheme.tmp")))
  (rm "/tmp/QobiScheme.tmp"))
 *current-architecture-name*)

(define (initialize-architecture!)
 (let* ((pathname (search-include-path "stalin.architectures"))
	(architecture
	 (assoc *architecture-name* (read-object-from-file pathname))))
  ;; conventions: PATHNAME ARCHITECTURE
  (unless architecture
   (notify "Unknown architecture: ~a" *architecture-name*)
   (terminate))
  (set! *char* (list-ref architecture 1))
  (set! *fixnum* (list-ref architecture 2))
  (set! *flonum*
	(case *flonum-representation*
	 ((float) (list-ref architecture 3))
	 ((double) (list-ref architecture 4))
	 (else (fuck-up))))
  (set! *length* (list-ref architecture 5))
  (set! *tag* (list-ref architecture 6))
  (set! *squished* (list-ref architecture 7))
  (set! *signed-squished* (list-ref architecture 8))
  (set! *file* (list-ref architecture 9))
  (set! *jmpbuf* (list-ref architecture 10))
  (set! *char-alignment* (list-ref architecture 11))
  (set! *fixnum-alignment* (list-ref architecture 12))
  (set! *flonum-alignment*
	(case *flonum-representation*
	 ((float) (list-ref architecture 13))
	 ((double) (list-ref architecture 14))
	 (else (fuck-up))))
  (set! *pointer-alignment* (list-ref architecture 15))
  (set! *length-alignment* (list-ref architecture 16))
  (set! *tag-alignment* (list-ref architecture 17))
  (set! *squished-alignment* (list-ref architecture 18))
  (set! *file-alignment* (list-ref architecture 19))
  (set! *jmpbuf-alignment* (list-ref architecture 20))
  (set! *char-size* (list-ref architecture 21))
  (set! *fixnum-size* (list-ref architecture 22))
  (set! *flonum-size*
	(case *flonum-representation*
	 ((float) (list-ref architecture 23))
	 ((double) (list-ref architecture 24))
	 (else (fuck-up))))
  (set! *pointer-size* (list-ref architecture 25))
  (set! *length-size* (list-ref architecture 26))
  (set! *tag-size* (list-ref architecture 27))
  (set! *squished-size* (if *squish?* (list-ref architecture 28) 0))
  (set! *include-malloc-for-alloca?* (list-ref architecture 29))))

(define (initialize-options! include-path)
 ;; conventions: INCLUDE-PATH
 (set! *include-path* include-path)
 (set! *Scheme->C-compatibility?* #f)
 (set! *Xlib-and-GL?* #f)
 (set! *QobiScheme?* #f)
 (set! *Trotsky?* #f)
 (set! *treat-all-symbols-as-external?* #f)
 (set! *index-allocated-string-types-by-expression?* #t)
 (set! *index-constant-structure-types-by-slot-types?* #f)
 (set! *index-constant-structure-types-by-expression?* #t)
 (set! *index-allocated-structure-types-by-slot-types?* #f)
 (set! *index-allocated-structure-types-by-expression?* #t)
 (set! *index-constant-headed-vector-types-by-element-type?* #f)
 (set! *index-constant-headed-vector-types-by-expression?* #t)
 (set! *index-allocated-headed-vector-types-by-element-type?* #f)
 (set! *index-allocated-headed-vector-types-by-expression?* #t)
 (set! *index-constant-nonheaded-vector-types-by-element-type?* #f)
 (set! *index-constant-nonheaded-vector-types-by-expression?* #t)
 (set! *index-allocated-nonheaded-vector-types-by-element-type?* #f)
 (set! *index-allocated-nonheaded-vector-types-by-expression?* #t)
 (set! *clone-size-limit* 80)
 (set! *split-even-if-no-widening?* #f)
 (set! *fully-convert-to-CPS?* #f)
 (set! *no-escaping-continuations?* #f)
 (set! *uniqueness?* #t)
 (set! *bounds-checks?* #t)
 (set! *memory-checks?* #t)
 (set! *overflow-checks?* #t)
 (set! *runtime-checks?* #t)
 (set! *type-checks?* #t)
 (set! *p1?* #f)
 (set! *p2?* #f)
 (set! *p3?* #f)
 (set! *p4?* #f)
 (set! *p5?* #f)
 (set! *p6?* #f)
 (set! *p7?* #f)
 (set! *closure-conversion-statistics?* #f)
 (set! *stack-allocation?* #t)
 (set! *heap-allocation?* #t)
 (set! *region-allocation?* #t)
 (set! *memory-messages?* #f)
 (set! *expandable-regions?* #t)
 (set! *flonum-representation* 'float)
 (set! *architecture-name* (current-architecture-name))
 (set! *closure-conversion-method* 'lightweight)
 (set! *closure-representation* 'linked)
 (set! *align-strings?* #t)
 (set! *eq?-forgery?* #f)
 (set! *forgery?* #t)
 (set! *globals?* #f)
 (set! *type-if?* #f)
 (set! *immediate-structures?* #f)
 (set! *promote-representations?* #f)
 (set! *copy-propagation?* #f)
 (set! *squeeze?* #t)
 (set! *squish?* #t)
 (set! *treadmarks?* #f)
 (set! *tail-call-optimization?* #t)
 (set! *database?* #t)
 (set! *run-cc?* #t)
 (set! *keep-c?* #f)
 (set! *cc* "gcc")
 (set! *copts* '()))

(define (initialize-stalin!)
 (initialize-architecture!)
 (set! *types-frozen?* #t)
 (set! *during-closure-conversion?* #f)
 (initialize-expressions!)
 (initialize-types!)
 (initialize-variables!)
 (initialize-environments!)
 (set! *abbreviate?* #f)
 (set! *worst-alignment* #f)
 (set! *allocation-alignment* #f)
 (set! *char-alignment?* #f)
 (set! *fixnum-alignment?* #f)
 (set! *flonum-alignment?* #f)
 (set! *rectangular-alignment?* #f)
 (set! *void*-alignment?* #f)
 (set! *char*-alignment?* #f)
 (set! *file*-alignment?* #f)
 (set! *jmpbuf*-alignment?* #f)
 (set! *length-alignment?* #f)
 (set! *tag-alignment?* #f)
 (set! *squished-alignment?* #f)
 (set! *file-alignment?* #f)
 (set! *jmpbuf-alignment?* #f)
 (set! *char-size?* #f)
 (set! *fixnum-size?* #f)
 (set! *flonum-size?* #f)
 (set! *rectangular-size?* #f)
 (set! *void*-size?* #f)
 (set! *char*-size?* #f)
 (set! *file*-size?* #f)
 (set! *jmpbuf*-size?* #f)
 (set! *length-size?* #f)
 (set! *tag-size?* #f)
 (set! *squished-size?* #f)
 (set! *strings* '())
 (set! *symbols* '())
 (set! *outside-main* '())
 (set! *inside-main* '())
 (set! *outside-body* '())
 (set! *discard* (create-discard-result))
 (set! *errors-used* '())
 (set! *warnings* '())
 (set! *ti* 0)
 (set! *li* 0)
 (set! *list->vector* (gensym "list->vector"))
 (set! *append* (gensym "append"))
 (set! *cons* (gensym "cons"))
 (set! *eqv?* (gensym "eqv?"))
 (set! *c:noreturn?* #f)
 (set! *c:c?* #f)
 (set! *c:panic?* #f)
 (set! *c:backtrace?* #f)
 (set! *c:backtrace-internal?* #f)
 (set! *c:ipow?* #f)
 (set! *c:input-waiting?* #f)
 (set! *c:includes* '())
 (set! *includes* '())
 (set! *herald* #f)
 (set! *heralds* '())
 (set! *program-has-pthreads?* #f)
 ;; needs work: This is the only unconditional include because
 ;;             FOREIGN-PROCEDURE doesn't (yet) allow specification of an
 ;;             include file.
 (include! "stdlib"))			;system exit

(define (herald p? text)
 ;; conventions: TEXT
 (when p? (notify text))
 (let ((t (clock-sample)))
  ;; conventions: T
  (when *herald*
   (let ((herald (find-if (lambda (herald)
			   ;; conventions: HERALD
			   (string=? (second herald) (second *herald*)))
			  *heralds*)))
    ;; conventions: HERALD
    (if herald
	(set-car! herald (+ (first herald) (- t (first *herald*))))
	(set! *heralds*
	      (cons (list (- t (first *herald*)) (second *herald*))
		    *heralds*)))))
  (set! *herald* (list t text))))

(define (display-heralds)
 (notify "Compilation time summary (in CPU seconds)")
 (let ((t (clock-sample)))
  ;; conventions: T
  (when *herald*
   (let ((herald (find-if (lambda (herald)
			   ;; conventions: HERALD
			   (string=? (second herald) (second *herald*)))
			  *heralds*)))
    ;; conventions: HERALD
    (if herald
	(set-car! herald (+ (first herald) (- t (first *herald*))))
	(set! *heralds*
	      (cons (list (- t (first *herald*)) (second *herald*))
		    *heralds*))))))
 (for-each
  (lambda (herald)
   (notify "~a - ~a% - ~a"
	   (number->string-of-length (inexact->exact (round (first herald))) 6)
	   (number->string-of-length
	    (inexact->exact
	     (round (/ (* 100 (first herald))
		       (reduce + (map first *heralds*) 0))))
	    2)
	   (second herald)))
  (reverse *heralds*)))

(define (replace-true-and-false-with-t-and-nil c) (if c 't 'nil))

(define (replace-symbols-with-strings c)
 (cond ((pair? c)
	(cons (replace-symbols-with-strings (car c))
	      (replace-symbols-with-strings (cdr c))))
       ((symbol? c) (symbol->string c))
       (else c)))

(define (type-kind u)
 (cond ((null-type? u) 'null-type)
       ((true-type? u) 'true-type)
       ((false-type? u) 'false-type)
       ((char-type? u) 'char-type)
       ((fixnum-type? u) 'fixnum-type)
       ((flonum-type? u) 'flonum-type)
       ((rectangular-type? u) 'rectangular-type)
       ((input-port-type? u) 'input-port-type)
       ((output-port-type? u) 'output-port-type)
       ((eof-object-type? u) 'eof-object-type)
       ((pointer-type? u) 'pointer-type)
       ((internal-symbol-type? u) 'internal-symbol-type)
       ((external-symbol-type? u) 'external-symbol-type)
       ((primitive-procedure-type? u) 'primitive-procedure-type)
       ((native-procedure-type? u) 'native-procedure-type)
       ((foreign-procedure-type? u) 'foreign-procedure-type)
       ((continuation-type? u) 'continuation-type)
       ((string-type? u) 'string-type)
       ((structure-type? u) 'structure-type)
       ((headed-vector-type? u) 'headed-vector-type)
       ((nonheaded-vector-type? u) 'nonheaded-vector-type)
       ((displaced-vector-type? u) 'displaced-vector-type)
       (else (fuck-up))))

(define (write-database pathname)
 ;; conventions: PATHNAME
 (define (map-variable-index gs)
  (cond
   ((pair? gs) (cons (variable-index (car gs)) (map-variable-index (cdr gs))))
   ((variable? gs) (variable-index gs))
   ((null? gs) '())
   (else (fuck-up))))
 (call-with-output-file (replace-extension pathname "db")
  (lambda (port)
   ;; conventions: PORT
   (write
    (list
     (map
      (lambda (x)
       ;; No need for: LINK,
       ;;              CONSTANT,
       ;;              INFERRED?,
       ;;              NEEDS-CONVERSION-TO-CPS?,
       ;;              RESULT,
       ;;              CONTINUATION-TYPE,
       ;;              STRING-TYPE,
       ;;              STRUCTURE-TYPES,
       ;;              HEADED-VECTOR-TYPES, and
       ;;              NONHEADED-VECTOR-TYPES.
       ;; Should add: ORIGINAL-EXPRESSION,
       ;;             REACHABLE?,
       ;;             ACCESSED?,
       ;;             RETURNS?, and
       ;;             FREE-REFERENCE? (but these only apply to references).
       (list (expression-kind x)
	     (expression-pathname x)
	     (expression-line-position x)
	     (expression-character-position x)
	     (expression-character-position-within-line x)
	     (expression-index x)
	     (if (expression-environment x)
		 (environment-index (expression-environment x))
		 'nil)
	     (type-set-index (expression-type-set x))
	     (if (expression-parent x)
		 (expression-index (expression-parent x))
		 'nil)
	     ;; This is a real kludge.
	     (cond ((eq? (expression-lambda-environment x) (unused)) 'unused)
		   ((eq? (expression-lambda-environment x) (unspecified))
		    'unspecified)
		   ((environment? (expression-lambda-environment x))
		    (environment-index (expression-lambda-environment x)))
		   (else 'nil))
	     ;; This is a real kludge.
	     (cond ((eq? (expression-parameters x) (unused)) 'unused)
		   ((eq? (expression-parameters x) (unspecified)) 'unspecified)
		   (else (map-variable-index (expression-parameters x))))
	     ;; This is a real kludge.
	     (cond ((eq? (expression-body x) (unused)) 'unused)
		   ((noop? x) 'nil)
		   (else (expression-index (expression-body x))))
	     ;; This is a real kludge.
	     (if (eq? (expression-variable x) (unused))
		 'unused
		 (variable-index (expression-variable x)))
	     ;; This is a real kludge.
	     (if (eq? (expression-source x) (unused))
		 'unused
		 (expression-index (expression-source x)))
	     ;; This is a real kludge.
	     (if (eq? (expression-antecedent x) (unused))
		 'unused
		 (expression-index (expression-antecedent x)))
	     ;; This is a real kludge.
	     (if (eq? (expression-consequent x) (unused))
		 'unused
		 (expression-index (expression-consequent x)))
	     ;; This is a real kludge.
	     (if (eq? (expression-alternate x) (unused))
		 'unused
		 (expression-index (expression-alternate x)))
	     ;; This is a real kludge.
	     (if (eq? (expression-callee x) (unused))
		 'unused
		 (expression-index (expression-callee x)))
	     ;; This is a real kludge.
	     (if (eq? (expression-arguments x) (unused))
		 'unused
		 (map expression-index (expression-arguments x)))
	     (map (lambda (u-e)
		   (cons
		    (type-index (car u-e))
		    (cond ((region-allocation? (cdr u-e))
			   (environment-index (cdr u-e)))
			  ((stack-allocation? (cdr u-e)) 'stack)
			  ((heap-allocation? (cdr u-e)) 'heap)
			  (else (fuck-up)))))
		  (expression-type-allocation-alist x))))
      (remove-if-not expression-pathname *xs*))
     (map (lambda (u)
	   ;; No need for: MARKED?,
	   ;;              USED?, and
	   ;;              LINK.
	   ;; Should add: TYPES-AND-TYPE-SETS-THAT-DIRECTLY-POINT-TO,
	   ;;             TYPE-PREDICATE-ACCESSED?,
	   ;;             EQ?-ACCESSED?,
	   ;;             SYMBOL->STRING-ACCESSED,
	   ;;             STRING-LENGTH-ACCESSED,
	   ;;             STRING-REF-ACCESSED,
	   ;;             STRUCTURE-REF-ACCESSED?,
	   ;;             VECTOR-LENGTH-ACCESSED,
	   ;;             VECTOR-REF-ACCESSED,
	   ;;             NARROW-PROTOTYPE,
	   ;;             ALIGNMENT?,
	   ;;             ALIGNMENT&?,
	   ;;             SIZE?,
	   ;;             ACCESSED-AFTER-RETURN?,
	   ;;             REFERENCED-RECURSIVELY?,
	   ;;             CALL-SITES,
	   ;;             NEVER-ALLOCATED-ON-THE-HEAP?,
	   ;;             EXTERNAL-SYMBOL-TYPE, and
	   ;;             DISPLACED-VECTOR-TYPE.
	   (list (type-kind u)
		 (if (internal-symbol-type? u)
		     (replace-symbols-with-strings
		      (internal-symbol-type-name u))
		     'unused)
		 (if (external-symbol-type? u)
		     (type-index
		      (external-symbol-type-displaced-string-type u))
		     'unused)
		 (if (primitive-procedure-type? u)
		     (replace-symbols-with-strings
		      (primitive-procedure-type-name u))
		     'unused)
		 (if (primitive-procedure-type? u)
		     (primitive-procedure-type-arguments u)
		     'unused)
		 (if (native-procedure-type? u)
		     (map
		      (lambda (y-e)
		       (cons (if (top-level-call-site? (car y-e))
				 'nil
				 (cons (expression-index
					(call-site-expression (car y-e)))
				       (call-site-offsets (car y-e))))
			     (environment-index (cdr y-e))))
		      (native-procedure-type-call-site-environment-alist u))
		     'unused)
		 (if (foreign-procedure-type? u)
		     (foreign-procedure-type-name u)
		     'unused)
		 (if (foreign-procedure-type? u)
		     (foreign-procedure-type-parameters u)
		     'unused)
		 (if (foreign-procedure-type? u)
		     (foreign-procedure-type-result u)
		     'unused)
		 (if (foreign-procedure-type? u)
		     (replace-true-and-false-with-t-and-nil
		      (foreign-procedure-type-called? u))
		     'unused)
		 (if (continuation-type? u)
		     (expression-index
		      (continuation-type-allocating-expression u))
		     'unused)
		 (if (continuation-type? u)
		     (replace-true-and-false-with-t-and-nil
		      (continuation-type-continuation-accessed? u))
		     'unused)
		 (if (string-type? u)
		     (map (lambda (x)
			   (if (expression? x) (expression-index x) 'nil))
			  (string-type-allocating-expressions u))
		     'unused)
		 (if (structure-type? u)
		     (replace-symbols-with-strings (structure-type-name u))
		     'unused)
		 (if (structure-type? u)
		     (map type-set-index (structure-type-slots u))
		     'unused)
		 (if (structure-type? u)
		     (replace-true-and-false-with-t-and-nil
		      (structure-type-immediate? u))
		     'unused)
		 (if (structure-type? u)
		     (map expression-index
			  (structure-type-allocating-expressions u))
		     'unused)
		 (if (headed-vector-type? u)
		     (type-set-index (headed-vector-type-element u))
		     'unused)
		 (if (headed-vector-type? u)
		     (map expression-index
			  (headed-vector-type-allocating-expressions u))
		     'unused)
		 (if (nonheaded-vector-type? u)
		     (type-set-index (nonheaded-vector-type-element u))
		     'unused)
		 (if (nonheaded-vector-type? u)
		     (map (lambda (x)
			   (if (expression? x) (expression-index x) 'nil))
			  (nonheaded-vector-type-allocating-expressions u))
		     'unused)
		 (if (displaced-vector-type? u)
		     (type-index
		      (displaced-vector-type-displaced-vector-type u))
		     'unused)
		 (type-index u)
		 (type-use-count u)
		 (replace-true-and-false-with-t-and-nil (fictitious? u))))
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
     (map (lambda (w)
	   ;; No need for: LINK and
	   ;;              USED?
	   ;; Should add: LOCATION,
	   ;;             MINIMAL-ALIGNMENT,
	   ;;             ALIGNMENT?,
	   ;;             SIZE?,
	   ;;             SQUEEZABLE, and
	   ;;             SQUISHABLE.
	   (list (map type-index (members w))
		 (type-set-index w)
		 (replace-true-and-false-with-t-and-nil (fictitious? w))
		 (replace-true-and-false-with-t-and-nil (void? w))
		 (replace-true-and-false-with-t-and-nil (monomorphic? w))
		 (replace-true-and-false-with-t-and-nil (multimorphic? w))
		 (replace-true-and-false-with-t-and-nil (tag-only? w))
		 (replace-true-and-false-with-t-and-nil (has-union? w))
		 (replace-true-and-false-with-t-and-nil (squeezed? w))
		 (replace-true-and-false-with-t-and-nil (squished? w))))
	  *ws*)
     (map (lambda (g)
	   ;; Should add: ASSIGNED?,
	   ;;             HAS-NON-IN-LINED-REFERENCE?,
	   ;;             REFERENCED-AFTER-RETURN?,
	   ;;             REFERENCED-RECURSIVELY?, and
	   ;;             HIDEABLE?.
	   (list (variable-pathname g)
		 (variable-line-position g)
		 (variable-character-position g)
		 (variable-character-position-within-line g)
		 (variable-index g)
		 (replace-symbols-with-strings (variable-name g))
		 (environment-index (variable-environment g))
		 (replace-true-and-false-with-t-and-nil (accessed? g))
		 (type-set-index (variable-type-set g))
		 (map expression-index (accesses g))
		 (map expression-index (assignments g))
		 (map expression-index (references g))
		 (replace-true-and-false-with-t-and-nil (local? g))
		 (replace-true-and-false-with-t-and-nil (global? g))
		 (replace-true-and-false-with-t-and-nil (hidden? g))
		 (replace-true-and-false-with-t-and-nil (slotted? g))))
	  (remove-if-not variable-pathname *gs*))
     (map (lambda (e)
	   ;; No need for: MARKED1?,
	   ;;              MARKED2?,
	   ;;              SPLIT,
	   ;; Should add: RECURSIVE?,
	   ;;             HAS-EXTERNAL-SELF-TAIL-CALL?
	   ;;             HAS-EXTERNAL-CONTINUATION-CALL?,
	   ;;             DISTANCE-FROM-ROOT,
	   ;;             FREE-VARIABLES,
	   ;;             QUICK-PARENT,
	   ;;             PARENT-PARAMETER,
	   ;;             PARENT-SLOT,
	   ;;             ANCESTORS,
	   ;;             DESCENDENTS,
	   ;;             PROPERLY-IN-LINED-ENVIRONMENTS,
	   ;;             NARROW-PROTOTYPE,
	   ;;             NARROW-CLONES
	   ;;             WIDE-PROTOTYPE,
	   ;;             DIRECT-TAIL-CALLERS,
	   ;;             DIRECT-NON-TAIL-CALLERS,
	   ;;             DIRECT-TAIL-CALLEES,
	   ;;             DIRECT-NON-TAIL-CALLEES,
	   ;;             BLOCKED-ENVIRONMENTS,
	   ;;             EXPRESSIONS,
	   ;;             CONTINUATION-CALLS,
	   ;;             ESCAPING-TYPES, and
	   ;;             NON-SELF-TAIL-CALL-SITES.
	   (list
	    (environment-index e)
	    (if (environment-expression e)
		(expression-index (environment-expression e))
		'nil)
	    (environment-name e)
	    (replace-true-and-false-with-t-and-nil (has-region? e))
	    ;; needs work: The offsets are discarded here. But it doesn't
	    ;;             matter for now since this field is not used by
	    ;;             stalin.el.
	    (map (lambda (y) (expression-index (call-site-expression y)))
		 ;; needs work: Should have abstraction for top-level call
		 ;;             site.
		 (remove-if top-level-call-site? (call-sites e)))
	    (cond ((region-allocation? (allocation e))
		   (environment-index (allocation e)))
		  ((stack-allocation? (allocation e)) 'stack)
		  ((heap-allocation? (allocation e)) 'heap)
		  (else 'nil))
	    (replace-true-and-false-with-t-and-nil (reentrant? e))
	    (replace-true-and-false-with-t-and-nil (called-more-than-once? e))
	    (if (or (empty? e) (not (called? e)))
		'nil
		(replace-true-and-false-with-t-and-nil (has-closure? e)))
	    ;; needs work: The offsets are discarded here. But it doesn't
	    ;;             matter for now since this field is not used by
	    ;;             stalin.el.
	    (if (unique-call-site? e)
		(expression-index (call-site-expression (unique-call-site e)))
		'nil)))
	  ;; note: We used to do (REMOVE-IF LET? *ES*) but then m-sh-Q gave
	  ;;       "allocates on nil" because it allocated on a let
	  ;;       environment.
	  *es*)
     (map (lambda (warning)
	   ;; conventions: WARNING
	   (list (first warning) (second warning) (third warning)))
	  *warnings*))
    port)
   (newline port))))

(define (stalin pathname thunk)
 (when *overflow-checks?*
  (unimplemented
   #f "For now, you must specify -On because safe fixnum arithmetic is not (yet) implemented"))
 (when *treadmarks?*
  (when *heap-allocation?*
   (unimplemented #f "For now, with -Tmk you must specify -dC"))
  (when *stack-allocation?*
   (unimplemented #f "With -Tmk you must specify -dc"))
  (unless *region-allocation?*
   (unimplemented #f "For now, with -Tmk you cannot specify -dH"))
  (when *globals?*
   (unimplemented #f "For now, with -Tmk you cannot specify -dG")))
 (initialize-stalin!)
 (set! *macros* *r4rs-macros*)
 (when *Scheme->C-compatibility?*
  (set! *macros* (append *macros* *Scheme->C-compatibility-macros*)))
 (when *Xlib-and-GL?* (set! *macros* (append *macros* *Xlib-and-GL-macros*)))
 (when *QobiScheme?* (set! *macros* (append *macros* *QobiScheme-macros*)))
 (when *Trotsky?* (set! *macros* (append *macros* *Trotsky-macros*)))
 (herald *p1?* "Reading source")
 (let* ((ss-spitter (thunk))
	(ss (first ss-spitter))
	(spitter (second ss-spitter)))
  ;; conventions: SPITTER
  (herald *p1?* "Expanding macros")
  (set! *x* (macroexpand ss))
  (set! *gs* (reverse *gs*))
  (herald *p1?* "Fast tree shake")
  (fast-tree-shake!)
  (when *fully-convert-to-CPS?*
   (herald *p1?* "Fully converting to CPS")
   (set! *x* (fully-convert-to-CPS *x*)))
  (let loop ((again? #f))
   (herald *p1?* "Annotating expressions with their parents")
   (annotate-expressions-with-their-parents!)
   (herald *p1?* "Annotating variables with their environments")
   (annotate-variables-with-their-environments!)
   (herald *p1?* "Annotating expressions with their environments")
   (annotate-expressions-with-their-environments!)
   (herald *p1?* "In-lining first-order calls to primitive procedures")
   (in-line-first-order-calls-to-primitive-procedures!)
   (herald *p1?* "Annotating expressions with their parents")
   (annotate-expressions-with-their-parents!)
   (herald *p1?* "Annotating variables with their environments")
   (annotate-variables-with-their-environments!)
   (herald *p1?* "Annotating expressions with their environments")
   (annotate-expressions-with-their-environments!)
   (herald *p1?* "Annotating variables with their references")
   (annotate-variables-with-their-references!)
   (let loop ((i 1))
    ;; conventions: I
    (when (>= i 3) (fuck-up))
    (herald *p1?* "Performing flow analysis")
    (perform-flow-analysis!)
    (herald *p1?* "Enumerating call sites")
    (enumerate-call-sites!)
    (herald *p1?* "Determining which types and type sets are used")
    (determine-which-types-and-type-sets-are-used!)
    ;; needs work: The next three expressions have their time assigned to
    ;;             determining which types and type sets are used.
    (remove-unused-objects! #f)
    (when *p1?*
     (print-counts)
     (print-number-of-call-sites-that-dispatch-on-clones)
     (print-maximal-non-let-lexical-nesting-depth)
     (print-maximal-clone-rate))
    (herald *p1?* "Determining which call sites to split")
    (if (and (not (zero? *clone-size-limit*))
	     (determine-which-call-sites-to-split!))
	(loop (+ i 1))
	(when *p1?*
	 (notify "~a pass~a of flow analysis" i (if (= i 1) "" "es")))))
   (set! *during-closure-conversion?* #t)
   (herald *p1?* "Computing call graph")
   (compute-call-graph! (expression-lambda-environment *x*))
   (herald *p1?* "Determining which environments are called more than once")
   (determine-which-environments-are-called-more-than-once!)
   (herald *p1?* "Determining which variables are referenced")
   (determine-which-variables-are-referenced!)
   (herald *p1?* "Determining free variables")
   (determine-free-variables!)
   (herald *p1?* "Determining necessarily-fictitious native procedure types")
   (determine-necessarily-fictitious-native-procedure-types!)
   (herald *p1?* "Annotating environments and continuation types")
   (annotate-environments-and-continuation-types!)
   (herald *p1?* "Inverting points-to relation")
   (invert-points-to-relation!)
   (herald *p1?* "Determining escaping types")
   (determine-escaping-types!)
   (herald *p1?* "Determining which environments have unique call sites")
   (determine-which-environments-have-unique-call-sites!)
   (herald *p1?* "Determining which environments are recursive")
   (determine-which-environments-are-recursive!)
   (herald *p1?* "Determining which environments are reentrant")
   (determine-which-environments-are-reentrant!)
   (when *uniqueness?*
    ;; needs work: The following comment is out of date.
    ;; I believe that it is not necessary to do this before determining which
    ;; types and type sets are used, computing the call graph, determining
    ;; which environments are called more than once, determining which
    ;; variables are accessed, determining referenced types, determining
    ;; escaping types, determining which environments have unique call sites,
    ;; and determining which environments are reentrant. But it must come
    ;; before performing lightweight closure conversion, determining parents
    ;; determining allocations, applying closed-world assumption, determining
    ;; indirect structure types, determining which environments have regions,
    ;; determining which type sets are squeezable, determining which type sets
    ;; are squishable, and determining alignments. I'm not sure about
    ;; converting to CPS, determining environment distances from root,
    ;; determining which environments have external self tail calls,
    ;; determining which environments have external continuation calls, and
    ;; determining which types are never allocated on the heap.
    (herald *p1?* "Asserting uniqueness")
    (assert-uniqueness!))
   (herald *p1?* "Performing lightweight closure conversion")
   (perform-lightweight-closure-conversion!)
   (herald *p1?* "Determining parents")
   (determine-parents!)
   (set! *during-closure-conversion?* #f)
   (unless *fully-convert-to-CPS?*
    (herald *p1?* "Determining which expressions need conversion to CPS")
    (unless *no-escaping-continuations?*
     (determine-which-expressions-need-conversion-to-CPS!))
    (when (and again?
	       (not *no-escaping-continuations?*)
	       (some expression-needs-conversion-to-CPS? *xs*))
     (notify "Warning! Double CPS conversion"))
    (when (and (not again?)
	       (not *no-escaping-continuations?*)
	       (some expression-needs-conversion-to-CPS? *xs*))
     (when again? (fuck-up))
     (when (some (lambda (e) (not (eq? e (wide-prototype e)))) *es*)
      (unimplemented
       #f "For now, this program must be compiled with -clone-size-limit 0"))
     (herald *p1?* "Converting to CPS")
     (set! *x* (nonconvert-to-CPS *x*))
     (when (converted? *y*) (fuck-up))
     ;; This is needed because ANNOTATE-EXPRESSIONS-WITH-THEIR-PARENTS! will
     ;; remove orphaned expressions from *XS* so clear out the REACHED? bits
     ;; before we lose an easy handle on such expressions.
     (for-each (lambda (x) (set-expression-reached?! x #f)) *xs*)
     (set! *es* (remove-if-not (lambda (e) (eq? e (wide-prototype e))) *es*))
     ;; This call to ANNOTATE-VARIABLES-WITH-THEIR-ENVIRONMENTS! is necessary
     ;; for VARIABLE-ENVIRONMENT to be set on variables created or moved by
     ;; conversion to CPS.
     (annotate-variables-with-their-environments!)
     (set! *gs*
	   (remove-if-not (lambda (g)
			   (eq? (variable-environment g)
				(wide-prototype (variable-environment g))))
			  *gs*))
     (for-each (lambda (e)
		(set-environment-narrow-clones! e (list e))
		(set-environment-direct-tail-callers! e (unspecified))
		(set-environment-direct-non-tail-callers! e (unspecified))
		(set-environment-direct-tail-callees! e (unspecified))
		(set-environment-direct-non-tail-callees! e (unspecified))
		(set-environment-expressions! e (unspecified))
		(set-environment-continuation-calls! e (unspecified))
		(set-environment-escaping-types! e (unspecified))
		(set-environment-non-self-tail-call-sites! e (unspecified)))
	       *es*)
     (initialize-types!)
     (loop #t))))
  ;; needs work: The next three expressions have their time assigned to
  ;;             determining environment distances from root.
  (when *closure-conversion-statistics?*
   (set! *during-closure-conversion?* #t)
   (notify
    "~s"
    (list
     'static-counts
     (length *gs*)
     (count-if assigned? *gs*)
     (count-if accessed? *gs*)
     (count-if (lambda (g) (not (fictitious? (variable-type-set g)))) *gs*)
     (count-if local? *gs*)
     (count-if global? *gs*)
     (count-if hidden? *gs*)
     (count-if slotted? *gs*)
     (length *es*)
     (count-if has-closure? *es*)
     (count-if
      (lambda (e) (and (environment-used? e) (has-parent-slot? e))) *es*)
     (count-if
      (lambda (e) (and (environment-used? e) (has-parent-parameter? e))) *es*)
     (count-if (lambda (e)
		(and (environment-used? e)
		     (has-parent-slot? e)
		     (not (eq? (parent-slot e) (parent e)))))
	       *es*)
     (count-if (lambda (e)
		(and (environment-used? e)
		     (has-parent-parameter? e)
		     (not (eq? (parent-parameter e) (parent e)))))
	       *es*)
     (let ((counts (map number-of-accessor-indirections
			(remove-if-not
			 (lambda (x)
			  (environment-used? (expression-environment x)))
			 *references*))))
      (/ (reduce + counts 0) (exact->inexact (length counts))))
     (length *accesses*)
     (count-if nontrivial-reference? *accesses*)
     (length *assignments*)
     (count-if nontrivial-reference? *assignments*)))
   (set! *during-closure-conversion?* #f))
  (remove-unused-objects! #t)
  (check-for-corruption #t)
  (when *p1?*
   (print-counts)
   (print-number-of-call-sites-that-dispatch-on-clones)
   (print-maximal-non-let-lexical-nesting-depth)
   (print-maximal-clone-rate))
  (herald *p1?* "Determining environment distances from root")
  (determine-environment-distances-from-root!)
  (herald *p1?* "Determining which environments have external self tail calls")
  (determine-which-environments-have-external-self-tail-calls!)
  (herald
   *p1?* "Determining which environments have external continuation calls")
  (determine-which-environments-have-external-continuation-calls!)
  (herald *p1?* "Determining blocked environments")
  (determine-blocked-environments!)
  (herald *p1?*
	  "Determining which environments need to pass parameters globally")
  (determine-which-environments-need-to-pass-parameters-globally!)
  (herald *p1?* "Determining allocations")
  (determine-allocations!)
  ;; needs work: The next expression has its time assigned to determining
  ;;             allocations.
  (when *p2?*
   (notify-pp "~s" (externalize-expression *x*))
   (for-each
    (lambda (w)
     (notify-pp "~s" (list (type-set-index w) (externalize-type-set w))))
    *ws*))
  (herald *p1?* "Applying closed-world assumption")
  (apply-closed-world-assumption!)
  (check-for-corruption #t)
  (herald *p1?* "Determining indirect structure types")
  (determine-indirect-structure-types!)
  (herald *p1?* "Determining which types are never allocated on the heap")
  (determine-which-types-are-never-allocated-on-the-heap!)
  (herald *p1?* "Determining which types are atomic")
  (determine-which-types-are-atomic!)
  (herald *p1?* "Determining which environments have regions")
  (determine-which-environments-have-regions!)
  ;; needs work: The next eight expressions have their time assigned to
  ;;             determining which environments have regions.
  (when *p3?*
   (notify-pp "~s" (externalize-expression *x*))
   (for-each
    (lambda (w)
     (notify-pp "~s" (list (type-set-index w) (externalize-type-set w))))
    *ws*))
  (when *p4?*
   (for-each (lambda (e)
	      (unless (and (null? (direct-tail-callees e))
			   (null? (direct-callees e))
			   (null? (proper-tail-callees e))
			   (null? (proper-callees e)))
	       (notify (environment-name e))
	       (unless (null? (direct-tail-callees e))
		(notify "  Direct tail callees:")
		(for-each (lambda (e) (notify "    ~a" (environment-name e)))
			  (direct-tail-callees e)))
	       (unless (null? (direct-callees e))
		(notify "  Direct callees:")
		(for-each (lambda (e) (notify "    ~a" (environment-name e)))
			  (direct-callees e)))
	       (unless (null? (proper-tail-callees e))
		(notify "  Proper tail callees:")
		(for-each (lambda (e) (notify "    ~a" (environment-name e)))
			  (proper-tail-callees e)))
	       (unless (null? (proper-callees e))
		(notify "  Proper callees:")
		(for-each (lambda (e) (notify "    ~a" (environment-name e)))
			  (proper-callees e)))))
	     *es*))
  (when *p5?*
   (let ((es (remove-if unique-call-site? *es*)))
    (notify
     (if (= (length es) 1)
	 "The following non-in-line native procedure will be generated:"
	 "The following non-in-line native procedures will be generated:"))
    (for-each (lambda (e)
	       (notify "  ~a~a~a~a"
		       (environment-name e)
		       (if (reentrant? e) " reentrant" "")
		       (if (converted? e) " converted" "")
		       (if (environment-passes-parameters-globally? e)
			   " passes parameters globally"
			   ""))
	       (case *closure-representation*
		((immediate-flat)
		 (unimplemented
		  #f "Immediate flat closures are not (yet) implemented"))
		((indirect-flat)
		 (unimplemented
		  #f "Indirect flat closures are not (yet) implemented"))
		((immediate-display indirect-display)
		 (when (has-parent-parameter? e)
		  (notify (if (= (length (ancestors e)) 1)
			      "    has the following ancestor:"
			      "    has the following ancestors"))
		  (for-each
		   (lambda (e) (notify "      ~a" (environment-name e)))
		   (ancestors e)))
		 (when (has-closure? e) (notify "    has closure")))
		((linked)
		 (when (has-parent-parameter? e)
		  (notify "    has parent parameter ~a"
			  (environment-name (parent-parameter e))))
		 (when (has-closure? e) (notify "    has closure"))
		 (when (has-parent-slot? e)
		  (notify "    has parent slot ~a"
			  (environment-name (parent-slot e)))))
		(else (fuck-up)))
	       (when (has-region? e) (notify "    has region"))
	       (let ((gs
		      (remove-if-not
		       (lambda (g)
			(or (local? g) (global? g) (hidden? g) (slotted? g)))
		       (variables e))))
		(unless (null? gs)
		 (notify (if (= (length gs) 1)
			     "    has the following parameter:"
			     "    has the following parameters:"))
		 (for-each (lambda (g)
			    (notify "      ~a{~s}~a~a~a~a"
				    (variable-name g)
				    (variable-index g)
				    (if (local? g) " local" "")
				    (if (global? g) " global" "")
				    (if (slotted? g) " slotted" "")
				    (if (hidden? g) " hidden as" ""))
			    (when (hidden? g)
			     (for-each (lambda (e)
					(notify "        ~a"
						(environment-name e)))
				       (narrow-clones
					(the-member (variable-type-set g))))))
			   gs)))
	       (let ((gs (remove-if-not
			  local?
			  (sort
			   (reduce append
				   (map variables
					(properly-in-lined-environments e))
				   '())
			   <
			   variable-index))))
		(unless (null? gs)
		 (notify (if (= (length gs) 1)
			     "    has the following in-lined local:"
			     "    has the following in-lined locals:"))
		 (for-each (lambda (g)
			    (notify "      ~a{~s}"
				    (variable-name g)
				    (variable-index g)))
			   gs))))
	      es))
   (let ((es
	  (remove-if-not
	   (lambda (e)
	    (and
	     (unique-call-site? e)
	     (or (reentrant? e)
		 (converted? e)
		 (case *closure-representation*
		  ((immediate-flat)
		   (unimplemented
		    #f "Immediate flat closures are not (yet) implemented"))
		  ((indirect-flat)
		   (unimplemented
		    #f "Indirect flat closures are not (yet) implemented"))
		  ((immediate-display indirect-display)
		   (or (has-parent-parameter? e) (has-closure? e)))
		  ((linked)
		   (or (has-parent-parameter? e)
		       (has-closure? e)
		       (has-parent-slot? e)))
		  (else (fuck-up)))
		 (has-region? e)
		 (some (lambda (g)
			(or (local? g) (global? g) (hidden? g) (slotted? g)))
		       (variables e)))))
	   *es*)))
    (unless (null? es)
     (notify
      (if (= (length es) 1)
	  "The following non-trivial in-line native procedure will be generated:"
	  "The following non-trivial in-line native procedures will be generated:")))
    (for-each
     (lambda (e)
      (notify "  ~a~a~a"
	      (environment-name e)
	      (if (reentrant? e) " reentrant" "")
	      (if (converted? e) " converted" ""))
      (case *closure-representation*
       ((immediate-flat)
	(unimplemented #f "Immediate flat closures are not (yet) implemented"))
       ((indirect-flat)
	(unimplemented #f "Indirect flat closures are not (yet) implemented"))
       ((immediate-display indirect-display)
	(when (has-parent-parameter? e)
	 (notify (if (= (length (ancestors e)) 1)
		     "    has the following ancestor:"
		     "    has the following ancestors:"))
	 (for-each (lambda (e) (notify "      ~a" (environment-name e)))
		   (ancestors e)))
	(when (has-closure? e) (notify "    has closure")))
       ((linked)
	(when (has-parent-parameter? e)
	 (notify "    has parent parameter ~a"
		 (environment-name (parent-parameter e))))
	(when (has-closure? e) (notify "    has closure"))
	(when (has-parent-slot? e)
	 (notify "    has parent slot ~a" (environment-name (parent-slot e)))))
       (else (fuck-up)))
      (when (has-region? e) (notify "    has region"))
      (let ((gs (remove-if-not
		 (lambda (g)
		  (or (local? g) (global? g) (hidden? g) (slotted? g)))
		 (variables e))))
       (unless (null? gs)
	(notify (if (= (length gs) 1)
		    "    has the following parameter:"
		    "    has the following parameters:"))
	(for-each (lambda (g)
		   (notify "      ~a{~s}~a~a~a~a"
			   (variable-name g)
			   (variable-index g)
			   (if (local? g) " local" "")
			   (if (global? g) " global" "")
			   (if (slotted? g) " slotted" "")
			   (if (hidden? g) " hidden as" ""))
		   (when (hidden? g)
		    (for-each
		     (lambda (e)
		      (notify "        ~a" (environment-name e)))
		     (narrow-clones (the-member (variable-type-set g))))))
		  gs))))
     es)))
  (if
   *tail-call-optimization?*
   ;; note: I believe that all call-graph cycles that consist only of tail
   ;;       calls have the property that all return types sets of procedures
   ;;       in that cycle are equal and thus EQ?. Thus there cannot be return-
   ;;       value coercion at a tail-recursive call site.
   (let ((ys (remove-if-not
	      (lambda (y)
	       (and
		(nonmerged-tail-recursive-purely-tail-call-site? y)
		(let* ((e1 (expression-environment (call-site-expression y)))
		       (e2 (home e1)))
		 ;; needs work: There is another case that foils tail-call
		 ;;             optimization in gcc that we don't (yet) check:
		 ;;             use of unary & on a local variable in the
		 ;;             caller. This is done by C:FORGERY-CAST and also
		 ;;             ZERO-VALUE and EQ? when *EQ?-FORGERY?* is true.
		 ;;             There are also cases unary & is applied to
		 ;;             a jump_buf but those are already handled by
		 ;;             HAS-SETJMP?. And MUTEX applies unary & to a
		 ;;             mutex to pass its address to pthread_mutex_lock
		 ;;             and pthread_mutex_unlock.
		 (or (and (has-region? e1) (reentrant? e1))
		     (has-alloca? e2)
		     (has-setjmp? e2)))))
	      *ys*)))
    (unless (null? ys)
     ;; needs work: These warnings should go in the warning database.
     (notify
      (if (= (reduce
	      +
	      (map
	       (lambda (y)
		(length
		 (nonmerged-tail-recursive-purely-tail-call-site-callees y)))
	       ys)
	      0)
	     1)
	  "Warning! The following tail-recursive tail call is not merged:"
	  "Warning! The following tail-recursive tail calls are not merged:"))
     (for-each
      (lambda (y)
       (cond
	((expression-pathname (call-site-expression y))
	 (notify "  From the following expression, ~a:~s:~s:"
		 (expression-pathname (call-site-expression y))
		 (expression-line-position (call-site-expression y))
		 (expression-character-position (call-site-expression y))))
	(else (notify "  From the following expression:")))
       (notify-pp3 "    ~s" (undecorate (call-site-expression y)))
       (let* ((e1 (expression-environment (call-site-expression y)))
	      (e2 (home e1)))
	(notify "    in ~a" (environment-name e2))
	(for-each (lambda (e) (notify "    to ~a" (environment-name e)))
		  (nonmerged-tail-recursive-purely-tail-call-site-callees y))
	;; You can't panic and still need to give these warning because the
	;; user might have specified -dC that forces the use of regions and
	;; alloca.
	(when (and (has-region? e1) (reentrant? e1))
	 (notify "    because the call site has a reentrant region"))
	(when (has-alloca? e2)
	 (notify "    because the call site has calls to alloca"))
	(when (has-setjmp? e2)
	 (notify "    because the call site has calls to setjmp"))))
      ys)))
   (let ((ys (remove-if-not nonmerged-tail-recursive-purely-tail-call-site?
			    *ys*)))
    (unless (null? ys)
     ;; needs work: These warnings should go in the warning database.
     (notify
      (if (= (reduce
	      +
	      (map
	       (lambda (y)
		(length
		 (nonmerged-tail-recursive-purely-tail-call-site-callees y)))
	       ys)
	      0)
	     1)
	  "Warning! The following tail-recursive tail call is not merged:"
	  "Warning! The following tail-recursive tail calls are not merged:"))
     (for-each
      (lambda (y)
       (cond
	((expression-pathname (call-site-expression y))
	 (notify "  From the following expression, ~a:~s:~s:"
		 (expression-pathname (call-site-expression y))
		 (expression-line-position (call-site-expression y))
		 (expression-character-position (call-site-expression y))))
	(else (notify "  From the following expression:")))
       (notify-pp3 "    ~s" (undecorate (call-site-expression y)))
       (notify "    in ~a"
	       (environment-name
		(home (expression-environment (call-site-expression y)))))
       (for-each (lambda (e) (notify "    to ~a" (environment-name e)))
		 (nonmerged-tail-recursive-purely-tail-call-site-callees y)))
      ys))))
  (when *p6?*
   (for-each
    (lambda (x)
     (for-each
      (lambda (u-e)
       (let ((u (car u-e))
	     (e (cdr u-e)))
	(cond
	 ((expression-pathname x)
	  (notify "The following expression, ~a:~s:~s, allocates on ~a:"
		  (expression-pathname x)
		  (expression-line-position x)
		  (expression-character-position x)
		  (cond ((region-allocation? e) (environment-name e))
			((stack-allocation? e) "the stack")
			((heap-allocation? e) "the heap")
			(else (fuck-up)))))
	 (else (notify "The following expression allocates on ~a:"
		       (cond ((region-allocation? e) (environment-name e))
			     ((stack-allocation? e) "the stack")
			     ((heap-allocation? e) "the heap")
			     (else (fuck-up))))))
	(notify-pp3 "~s" (undecorate x))))
      (expression-type-allocation-alist x)))
    *calls*)
   (for-each
    (lambda (e)
     (when (has-closure? e)
      (notify "The closure for ~a is allocated on ~a"
	      (environment-name e)
	      (cond ((region-allocation? (allocation e))
		     (environment-name (allocation e)))
		    ((stack-allocation? (allocation e)) "the stack")
		    ((heap-allocation? (allocation e)) "the heap")
		    (else (fuck-up))))))
    *es*))
  (herald *p1?* "Determining which type sets are squeezable")
  (determine-which-type-sets-are-squeezable!)
  (herald *p1?* "Determining which type sets are squishable")
  (determine-which-type-sets-are-squishable!)
  (herald *p1?* "Determining alignments")
  (determine-alignments!)
  (herald *p1?* "Assigning global squish tags")
  (assign-global-squish-tags!)
  (when *promote-representations?*
   (herald *p1?* "Promoting representations")
   (promote-representations!))
  (when *p1?* (print-reasons-why-type-sets-are-not-squishable!))
  ;; needs work: To handle fake structure slots and vector elements.
  ;;             Vector elements may be tricky because it can lead to
  ;;             degeneracy.
  (for-each
   (lambda (g)
    (when (fake? (variable-type-set g))
     (cond
      ((local? g)
       (format #t "Warning! Unlocalizing ~a{~a}:W~s because it is fake~%"
	       (variable-name g)
	       (variable-index g)
	       (type-set-index (variable-type-set g)))
       (set-variable-local?! g #f))
      ((global? g)
       (format #t "Warning! Unglobalizing ~a{~a}:W~s because it is fake~%"
	       (variable-name g)
	       (variable-index g)
	       (type-set-index (variable-type-set g)))
       (set-variable-global?! g #f))
      ;; needs work: I'm not sure about this one.
      ((hidden? g)
       (format #t "Warning! Unhidding ~a{~a}:W~s because it is fake~%"
	       (variable-name g)
	       (variable-index g)
	       (type-set-index (variable-type-set g)))
       (set-variable-hidden?! g #f))
      ((slotted? g)
       (format #t "Warning! Unslotting ~a{~a}:W~s because it is fake~%"
	       (variable-name g)
	       (variable-index g)
	       (type-set-index (variable-type-set g)))
       (set-variable-slotted?! g #f)))))
   *gs*)
  (herald *p1?* "Generating code")
  ;; needs work: To move all quote variables before any procedures.
  (let* ((c1 (compile-native-procedures))
	 (c1 (newline-between (compile-error-procedures) c1)))
   (set! *outside-body* '())
   (when (hidden? (first (variables *x*))) (fuck-up))
   (when (or (eq? *closure-conversion-method* 'baseline)
	     (eq? *closure-conversion-method* 'conventional))
    ;; needs work: needs abstraction for initialized declaration
    (outside-main
     (semicolon-after
      (space-between *fixnum*
		     (unparenthesize (c:= "fake" (c:value-offset)))))))
   (let* ((u (the-member (expression-type-set *x*)))
	  (g (first (variables *x*)))
	  (w (variable-type-set g))
	  (t (if (or (local? g) (slotted? g))
		 (allocate-temporary w)
		 (c:noop)))
	  (w1 (return-type-set (callee-environment u *y*)))
	  (c (if (and (or (local? g) (slotted? g))
		      (not (environment-passes-parameters-globally?
			    (callee-environment u *y*))))
		 (c:call (c:f (callee-environment u *y*)) t)
		 (c:call (c:f (callee-environment u *y*)))))
	  (c (newline-between
	      (if (or (local? g) (global? g) (slotted? g))
		  (move-displaced-vector
		   (if (global? g)
		       (create-accessor-result
			(variable-type-set g) (accessor g #f))
		       (create-accessor-result w t))
		   (the-member-that top-level-nonheaded-vector-type? w)
		   (c:argv)
		   (c:argc))
		  (c:noop))
	      (if (and (or (local? g) (slotted? g))
		       (environment-passes-parameters-globally?
			(callee-environment u *y*)))
		  (c::= (c:b g) t)
		  (c:noop))
	      (if (can-be? fixnum-type? w1)
		  (if (can-be-non? fixnum-type? w1)
		      (let ((t1 (allocate-temporary w1)))
		       (newline-between
			(move (create-accessor-result w1 t1) c w1)
			(type-switch
			 fixnum-type?
			 w1
			 *discard*
			 t1
			 (lambda (u1)
			  (cond (*treadmarks?*
				 (include! "Tmk") ;Tmk_exit
				 (c:gosub "Tmk_exit" (c:value t1 u1 w1)))
				(else (c:return (c:value t1 u1 w1)))))
			 (lambda (p?)
			  (cond (*treadmarks?*
				 (include! "Tmk") ;Tmk_exit
				 (c:gosub "Tmk_exit" (c:0)))
				(else (c:return (c:0))))))))
		      (c:return c))
		  (newline-between (semicolon-after c) (c:return (c:0))))))
	  (c (newline-between
	      (compile-regions)
	      (compile-closures)
	      (compile-type-declarations)
	      (compile-closure-levels)
	      (compile-global-variables)
	      (compile-error-procedure-prototypes)
	      (compile-native-procedure-prototypes)
	      (if *october?*
		  (c:noop)
		  (compile-foreign-procedure-prototypes))
	      (compile-constant-initialization-procedure-prototypes)
	      (newlines-between (reverse *outside-main*))
	      c1
	      (compile-constant-initialization-procedures)
	      ;; needs work: To use code-generation abstractions.
	      (space-between
	       "int"
	       (if (or (accessed? g)
		       (eq? *closure-conversion-method* 'baseline)
		       (eq? *closure-conversion-method* 'conventional)
		       *treadmarks?*)
		   ;; needs work: To generate a prototype.
		   (c:header (c:main)
			     (space-between "int" (c:argc))
			     (space-between
			      "char" (star-before (star-before (c:argv)))))
		   ;; needs work: To generate a prototype.
		   (c:header (c:main))))
	      (braces-around
	       (newline-between
		(newlines-between (reverse *outside-body*))
		(compile-assertions)
		(cond (*treadmarks?*
		       (include! "Tmk")	;Tmk_startup
		       (include! "unistd") ;optind
		       (include! "string") ;strcmp
		       (newline-between
			(c:while
			 (c:&& (c:< "optind" (c:argc))
			       (c:!=0
				(c:call "strcmp"
					(c:subscript (c:argv) "optind")
					"\"--\"")))
			 (semicolon-after (c:++ "optind")))
			(c:if (c:&& (c:< "optind" (c:argc))
				    (c:==0
				     (c:call "strcmp"
					     (c:subscript (c:argv) "optind")
					     "\"--\"")))
			      (semicolon-after (c:++ "optind"))
			      (c:noop)
			      #f)
			(c:gosub "Tmk_startup" (c:argc) (c:argv))))
		      (else (c:noop)))
		(compile-constant-initialization-procedure-calls)
		;; For now, we don't use GC_enable_incremental because it is
		;; flakey under Solaris 2.6 and IRIX64 6.2. Also, for some
		;; reason, self-compilation runs out of memory with
		;; GC_enable_incremental but not without it.
		(if (and #f *program-has-heap?*)
		    (c:gc-enable-incremental)
		    (c:noop))
		(if *expandable-regions?*
		    (cond
		     (*treadmarks?*
		      (include! "Tmk")	;Tmk_proc_id
		      (c:if (c:== "Tmk_proc_id" (c:0))
			    (newline-between
			     (newlines-between
			      (map (lambda (e)
				    (if (and (has-region? e) (reentrant? e))
					(c:align (c:fp e))
					(c:noop)))
				   *es*))
			     (compile-region-distribution)
			     (compile-global-variable-distribution))
			    (c:noop)
			    #f))
		     (else (newlines-between
			    (map (lambda (e)
				  (if (and (has-region? e) (reentrant? e))
				      (c:align (c:fp e))
				      (c:noop)))
				 *es*))))
		    (cond (*treadmarks?*
			   (include! "Tmk") ;Tmk_proc_id
			   (c:if (c:== "Tmk_proc_id" (c:0))
				 (newline-between
				  (compile-region-distribution)
				  (compile-global-variable-distribution))
				 (c:noop)
				 #f))
			  (else (c:noop))))
		(cond (*treadmarks?*
		       (include! "Tmk")	;Tmk_barrier
		       (c:gosub "Tmk_barrier" (c:0)))
		      (else (c:noop)))
		c)))))
    (let ((c (list (newline-between (c-library) (compile-offsets) c)
		   #\newline)))
     (when *copy-propagation?*
      (herald *p1?* "Copy propagation")
      (c:copy-propagate! c))
     (herald *p1?* "Removing unused declarations")
     (c:remove-unused-declarations! c)
     (herald *p1?* "Removing unused labels")
     (c:remove-unused-labels! c)
     (herald *p1?* "Generating C code")
     (generate c pathname spitter))))
  (when *database?*
   (herald *p1?* "Writing database")
   (write-database pathname))
  (when *run-cc?*
   (herald *p1?* "Compiling C code")
   (unless (zero?
	    (system (reduce (lambda (s1 s2)
			     ;; conventions: S1 S2
			     (string-append s1 " " s2))
			    `(,*cc*
			      ,@(reduce append
					(map (lambda (s)
					      ;; conventions: S
					      (list "-I" s))
					     *include-path*)
					'())
			      "-o"
			      ,(strip-extension pathname)
			      ,(replace-extension pathname "c")
			      ,@(reverse *copts*)
			      ,@(reduce append
					(map (lambda (s)
					      ;; conventions: S
					      (list "-L" s))
					     *include-path*)
					'())
			      "-lm"
			      "-lstalin"
			      ,@(if *program-has-heap?* '("-lgc") '())
			      ,@(if *program-has-pthreads?* '("-lpthread") '())
			      ,@(if *treadmarks?* '("-lTmk") '()))
			    "")))
    (fuck-up)))
  (when (and *run-cc?* (not *keep-c?*)) (rm (replace-extension pathname "c")))
  (when *p1?* (display-heralds))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
