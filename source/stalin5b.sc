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
(module stalin5b)

(include "QobiScheme.sch")
(include "stalin5b.sch")
;;; End delete for Trotsky

(define (scheme-library vs ss)
 ;; conventions: VS
 (encapsulate
  ;; 6. Standard procedures

  ;; 6.1 Booleans

  `(lambda (argv)

    (define (not obj) ((primitive-procedure not) obj))

    (define (boolean? obj) ((primitive-procedure boolean?) obj))

    ;; 6.2 Equivalence predicates

    (define (eqv? obj1 obj2)
     (or (eq? obj1 obj2)
	 (and (number? obj1) (number? obj2) (= obj1 obj2))
	 (and (string? obj1)
	      (string? obj2)
	      (zero? (string-length obj1))
	      (zero? (string-length obj2)))
	 (and (vector? obj1)
	      (vector? obj2)
	      (zero? (vector-length obj1))
	      (zero? (vector-length obj2)))))

    (define (eq? obj1 obj2) ((primitive-procedure eq?) obj1 obj2))

    (define (equal? obj1 obj2)
     (or (eqv? obj1 obj2)
	 (and (pair? obj1)
	      (pair? obj2)
	      (equal? (car obj1) (car obj2))
	      (equal? (cdr obj1) (cdr obj2)))
	 (and (string? obj1) (string? obj2) (string=? obj1 obj2))
	 (and (vector? obj1)
	      (vector? obj2)
	      (= (vector-length obj1) (vector-length obj2))
	      (let loop? ((k 0))
	       (or (= k (vector-length obj1))
		   (and (equal? (vector-ref obj1 k) (vector-ref obj2 k))
			(loop? (+ k 1))))))))

    ;; 6.3 Pairs and lists

    (define (pair? obj) ((primitive-procedure structure? pair) obj))

    (define (cons obj1 obj2)
     ((primitive-procedure make-structure pair 2) obj1 obj2))

    (define (car pair) ((primitive-procedure structure-ref pair 0) pair))

    (define (cdr pair) ((primitive-procedure structure-ref pair 1) pair))

    (define (set-car! pair obj)
     ((primitive-procedure structure-set! pair 0) pair obj))

    (define (set-cdr! pair obj)
     ((primitive-procedure structure-set! pair 1) pair obj))

    (define (caar pair) (car (car pair)))

    (define (cadr pair) (car (cdr pair)))

    (define (cdar pair) (cdr (car pair)))

    (define (cddr pair) (cdr (cdr pair)))

    (define (caaar pair) (car (car (car pair))))

    (define (caadr pair) (car (car (cdr pair))))

    (define (cadar pair) (car (cdr (car pair))))

    (define (caddr pair) (car (cdr (cdr pair))))

    (define (cdaar pair) (cdr (car (car pair))))

    (define (cdadr pair) (cdr (car (cdr pair))))

    (define (cddar pair) (cdr (cdr (car pair))))

    (define (cdddr pair) (cdr (cdr (cdr pair))))

    (define (caaaar pair) (car (car (car (car pair)))))

    (define (caaadr pair) (car (car (car (cdr pair)))))

    (define (caadar pair) (car (car (cdr (car pair)))))

    (define (caaddr pair) (car (car (cdr (cdr pair)))))

    (define (cadaar pair) (car (cdr (car (car pair)))))

    (define (cadadr pair) (car (cdr (car (cdr pair)))))

    (define (caddar pair) (car (cdr (cdr (car pair)))))

    (define (cadddr pair) (car (cdr (cdr (cdr pair)))))

    (define (cdaaar pair) (cdr (car (car (car pair)))))

    (define (cdaadr pair) (cdr (car (car (cdr pair)))))

    (define (cdadar pair) (cdr (car (cdr (car pair)))))

    (define (cdaddr pair) (cdr (car (cdr (cdr pair)))))

    (define (cddaar pair) (cdr (cdr (car (car pair)))))

    (define (cddadr pair) (cdr (cdr (car (cdr pair)))))

    (define (cdddar pair) (cdr (cdr (cdr (car pair)))))

    (define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))

    (define (null? obj) ((primitive-procedure null?) obj))

    (define (list? x)
     (or (null? x)
	 (and (pair? x)
	      (let loop? ((slow x) (fast (cdr x)))
	       (or (null? fast)
		   (and (pair? fast)
			(and (not (eq? fast slow))
			     (let ((fast (cdr fast)))
			      (or (null? fast)
				  (and (pair? fast)
				       (loop? (cdr slow) (cdr fast))))))))))))

    (define (list . objs) objs)

    (define (list-length list)		;Extension to R4RS.
     (let loop ((k 0))
      (cond ((null? list) k) (else (set! list (cdr list)) (loop (+ k 1))))))

    (define (length s)			;Extension to R4RS.
     (cond
      ;; note: This was changed from LIST? to PAIR?/NULL? for efficiency
      ;;       reasons. Now it will loop when given an infinite list.
      ((null? s) 0)
      ((pair? s) (list-length s))
      ((string? s) (string-length s))
      ((vector? s) (vector-length s))
      (else (panic "Argument to LENGTH is not a list, string, or vector"))))

    (define (sublist list start end)	;Extension to R4RS.
     (if (zero? start)
	 ;; needs work: To make tail recursive.
	 (let loop ((list list) (k end))
	  (if (zero? k) '() (cons (car list) (loop (cdr list) (- k 1)))))
	 (sublist (cdr list) (- start 1) (- end 1))))

    (define (sub s start end)	        ;Extension to R4RS.
     (cond
      ;; note: This was changed from LIST? to PAIR?/NULL? for efficiency
      ;;       reasons. Now it may loop when given an infinite list and may
      ;;       fail to detect an error when given a pair that is not a list.
      ((null? s)
       (cond ((and (zero? start) (zero? end)) '())
	     (else (panic "Arguments to SUB out of bounds"))))
      ((pair? s) (sublist s start end))
      ((string? s) (substring s start end))
      ((vector? s) (subvector s start end))
      (else (panic "First argument to SUB is not a list, string, or vector"))))

    (define (list-append . lists)	;Extension to R4RS.
     ;; needs work: To make tail recursive.
     ;; note: Support for multiple arguments incurs a penalty here.
     ;; note: This may loop when given an infinite list and may fail to detect
     ;;       an error when given a pair that is not a list as other than the
     ;;       last argument.
     (cond
      ((null? lists) '())
      ((null? (cdr lists)) (car lists))
      (else (let loop ((list1 (car lists))
		       (list2 (car (cdr lists)))
		       (lists (cdr (cdr lists))))
	     (if (null? list1)
		 (if (null? lists) list2 (loop list2 (car lists) (cdr lists)))
		 (cons (car list1) (loop (cdr list1) list2 lists)))))))

    (define (append . ss)		;Extension to R4RS.
     ;; note: Support for multiple arguments incurs a penalty here.
     (cond
      ((null? ss) '())
      ;; note: This was changed from LIST? to PAIR?/NULL? for efficiency
      ;;       reasons. Now it may loop when given an infinite list and may
      ;;       fail to detect an error when given a pair that is not a list as
      ;;       other than the last argument.
      ((let loop ((ss ss))
	(or (null? (cdr ss))
	    (and (or (null? (car ss)) (pair? (car ss))) (loop (cdr ss)))))
       (cond
	((null? (cdr ss)) (car ss))
	(else
	 ;; needs work: To make tail recursive.
	 (let loop ((list1 (car ss))
		    (list2 (car (cdr ss)))
		    (lists (cdr (cdr ss))))
	  (if (null? list1)
	      (if (null? lists) list2 (loop list2 (car lists) (cdr lists)))
	      (cons (car list1) (loop (cdr list1) list2 lists)))))))
      ((let loop ((ss ss))
	(or (null? ss) (and (string? (car ss)) (loop (cdr ss)))))
       (let* ((r (make-string (let loop ((k 0) (strings ss))
			       (if (null? strings)
				   k
				   (loop (+ k (string-length (car strings)))
					 (cdr strings))))))
	      (k 0))
	(let loop ((strings ss))
	 (if (not (null? strings))
	     (let ((n (string-length (car strings))))
	      (let loop ((l 0))
	       (if (not (= l n))
		   (begin (string-set! r k (string-ref (car strings) l))
			  (set! k (+ k 1))
			  (loop (+ l 1)))))
	      (loop (cdr strings)))))
	r))
      ((let loop ((ss ss))
	(or (null? ss) (and (vector? (car ss)) (loop (cdr ss)))))
       (let* ((r (make-vector (let loop ((k 0) (vectors ss))
			       (if (null? vectors)
				   k
				   (loop (+ k (vector-length (car vectors)))
					 (cdr vectors))))))
	      (k 0))
	(let loop ((vectors ss))
	 (if (not (null? vectors))
	     (begin (let loop ((l 0))
		     (if (not (= l (vector-length (car vectors))))
			 (begin (vector-set! r k (vector-ref (car vectors) l))
				(set! k (+ k 1))
				(loop (+ l 1)))))
		    (loop (cdr vectors)))))
	r))
      (else
       (panic "Arguments to APPEND are not all lists, strings, or vectors"))))

    (define (list-reverse list)		;Extension to R4RS.
     (let loop ((list list) (r '()))
      (if (null? list) r (loop (cdr list) (cons (car list) r)))))

    (define (reverse s)			;Extension to R4RS.
     (cond
      ;; note: This was changed from LIST? to PAIR?/NULL? for efficiency
      ;;       reasons. Now it will loop when given an infinite list.
      ((null? s) '())
      ((pair? s) (list-reverse s))
      ((string? s) (string-reverse s))
      ((vector? s) (vector-reverse s))
      (else (panic "Argument to REVERSE is not a list, string, or vector"))))

    (define (list-tail list k)
     (if (zero? k) list (list-tail (cdr list) (- k 1))))

    (define (list-ref list k)
     (let loop ()
      (cond ((zero? k) (car list))
	    (else (set! list (cdr list)) (set! k (- k 1)) (loop)))))

    (define (ref s k)			;Extension to R4RS.
     (cond
      ;; note: This was changed from LIST? to PAIR? for efficiency reasons.
      ;;       Now it may loop when given an infinite list and may fail to
      ;;       detect an error when given a pair that is not a list.
      ((pair? s) (list-ref s k))
      ((string? s) (string-ref s k))
      ((vector? s) (vector-ref s k))
      (else
       (panic
	"First argument to REF is not a (nonempty) list, string, or vector"))))

    (define (memq obj list)
     (and (not (null? list))
	  (if (eq? obj (car list)) list (memq obj (cdr list)))))

    (define (memv obj list)
     (and (not (null? list))
	  (if (eqv? obj (car list)) list (memv obj (cdr list)))))

    (define (member obj list)
     (and (not (null? list))
	  (if (equal? obj (car list)) list (member obj (cdr list)))))

    (define (assq obj alist)
     (and (not (null? alist))
	  (if (eq? obj (car (car alist))) (car alist) (assq obj (cdr alist)))))

    (define (assv obj alist)
     (and
      (not (null? alist))
      (if (eqv? obj (car (car alist))) (car alist) (assv obj (cdr alist)))))

    (define (assoc obj alist)
     (and
      (not (null? alist))
      (if (equal? obj (car (car alist))) (car alist) (assoc obj (cdr alist)))))

    (define (list-set! list k obj)	;Extension to R4RS.
     (let loop ()
      (cond ((zero? k) (set-car! list obj))
	    (else (set! list (cdr list)) (set! k (- k 1)) (loop)))))

    (define (ref! s k obj)		;Extension to R4RS.
     (cond
      ;; note: This was changed from LIST? to PAIR? for efficiency reasons.
      ;;       Now it may loop when given an infinite list and may fail to
      ;;       detect an error when given a pair that is not a list.
      ((pair? s) (list-set! s k obj))
      ((string? s) (string-set! s k obj))
      ((vector? s) (vector-set! s k obj))
      (else (panic "First argument to REF! is not a (nonempty) list, string, or vector"))))

    (define (list-fill! list obj)	;Extension to R4RS.
     (let loop ()
      (if (not (null? list))
	  (begin (set-car! list obj) (set! list (cdr list)) (loop)))))

    (define (fill! s obj)		;Extension to R4RS.
     (cond
      ;; note: This was changed from LIST? to PAIR?/NULL? for efficiency
      ;;       reasons. Now it will loop when given an infinite list.
      ((null? s) '())
      ((pair? s) (list-fill! s obj))
      ((string? s) (string-fill! s obj))
      ((vector? s) (vector-fill! s obj))
      (else
       (panic "First argument to FILL! is not a list, string, or vector"))))

    (define (list-copy list)		;Extension to R4RS.
     ;; needs work: To make tail recursive.
     (if (null? list) '() (cons (car list) (list-copy (cdr list)))))

    (define (copy s)			;Extension to R4RS.
     (cond
      ;; note: This was changed from LIST? to PAIR?/NULL? for efficiency
      ;;       reasons. Now it will loop when given an infinite list.
      ((null? s) '())
      ((pair? s) (list-copy s))
      ((string? s) (string-copy s))
      ((vector? s) (vector-copy s))
      (else (panic "Argument to COPY is not a list, string, or vector"))))

    ;; 6.4 Symbols

    (define (symbol? obj) ((primitive-procedure symbol?) obj))

    (define (symbol->string symbol)
     ((primitive-procedure symbol->string) symbol))

    (define (string->uninterned-symbol string) ;Extension to R4RS.
     ((primitive-procedure string->uninterned-symbol) string))

    (define string->symbol
     (let ((package '()))
      (lambda (string)
       (cond
	,@(map (lambda (v) `((string=? string ,(symbol->string v)) ',v)) vs)
	;; This formulation relies on the fact that, with the current
	;; implementation of STRING->UNINTERNED-SYMBOL, (EQ? X Y) implies
	;; (EQ? (STRING->UNINTERNED-SYMBOL X) (STRING->UNINTERNED-SYMBOL Y)).
	;; note: Manually split MEMBER here.
	(else (let ((found (let loop ((package package))
			    (and (not (null? package))
				 (if (string=? string (car package))
				     package
				     (loop (cdr package)))))))
	       (cond (found (string->uninterned-symbol (car found)))
		     (else (set! package (cons (string-copy string) package))
			   (string->uninterned-symbol (car package))))))))))

    ;; 6.5 Numbers

    ;; 6.5.5 Numerical operations

    (define (number? obj) ((primitive-procedure number?) obj))

    (define complex? number?)

    (define (real? obj) ((primitive-procedure real?) obj))

    (define rational? real?)

    (define (integer? obj) ((primitive-procedure integer?) obj))

    (define (exact? z) ((primitive-procedure exact?) z))

    (define (inexact? z) ((primitive-procedure inexact?) z))

    (define (= z1 z2 . zs)
     (and ((primitive-procedure =) z1 z2)
	  (let loop? ((z z2) (zs zs))
	   (or (null? zs)
	       (and ((primitive-procedure =) z (car zs))
		    (loop? (car zs) (cdr zs)))))))

    (define (< x1 x2 . xs)
     (and ((primitive-procedure <) x1 x2)
	  (let loop? ((x x2) (xs xs))
	   (or (null? xs)
	       (and ((primitive-procedure <) x (car xs))
		    (loop? (car xs) (cdr xs)))))))

    (define (> x1 x2 . xs)
     (and ((primitive-procedure >) x1 x2)
	  (let loop? ((x x2) (xs xs))
	   (or (null? xs)
	       (and ((primitive-procedure >) x (car xs))
		    (loop? (car xs) (cdr xs)))))))

    (define (<= x1 x2 . xs)
     (and ((primitive-procedure <=) x1 x2)
	  (let loop? ((x x2) (xs xs))
	   (or (null? xs)
	       (and ((primitive-procedure <=) x (car xs))
		    (loop? (car xs) (cdr xs)))))))

    (define (>= x1 x2 . xs)
     (and ((primitive-procedure >=) x1 x2)
	  (let loop? ((x x2) (xs xs))
	   (or (null? xs)
	       (and ((primitive-procedure >=) x (car xs))
		    (loop? (car xs) (cdr xs)))))))

    (define (zero? z) ((primitive-procedure zero?) z))

    (define (positive? x) ((primitive-procedure positive?) x))

    (define (negative? x) ((primitive-procedure negative?) x))

    (define (odd? n) (not (even? n)))

    (define (even? n) (zero? (remainder n 2)))

    (define (max x . xs)
     (if (null? xs)
	 ((primitive-procedure max) x)
	 (let loop ((xs (cdr xs)) (r ((primitive-procedure max) x (car xs))))
	  (if (null? xs)
	      r
	      (loop (cdr xs) ((primitive-procedure max) r (car xs)))))))

    (define (min x . xs)
     (if (null? xs)
	 ((primitive-procedure min) x)
	 (let loop ((xs (cdr xs)) (r ((primitive-procedure min) x (car xs))))
	  (if (null? xs)
	      r
	      (loop (cdr xs) ((primitive-procedure min) r (car xs)))))))

    (define (+ . zs)
     (if (null? zs)
	 ((primitive-procedure +))
	 (let loop ((zs (cdr zs)) (r ((primitive-procedure +) (car zs))))
	  (if (null? zs)
	      r
	      (loop (cdr zs) ((primitive-procedure +) r (car zs)))))))

    (define (* . zs)
     (if (null? zs) ((primitive-procedure *))
	 (let loop ((zs (cdr zs)) (r ((primitive-procedure *) (car zs))))
	  (if (null? zs)
	      r
	      (loop (cdr zs) ((primitive-procedure *) r (car zs)))))))

    (define (- z . zs)
     (if (null? zs)
	 ((primitive-procedure -) z)
	 (let loop ((zs (cdr zs)) (r ((primitive-procedure -) z (car zs))))
	  (if (null? zs)
	      r
	      (loop (cdr zs) ((primitive-procedure -) r (car zs)))))))

    (define (/ z . zs)
     (if (null? zs)
	 ((primitive-procedure /) z)
	 (let loop ((zs (cdr zs)) (r ((primitive-procedure /) z (car zs))))
	  (if (null? zs)
	      r
	      (loop (cdr zs) ((primitive-procedure /) r (car zs)))))))

    (define (abs x) (if (negative? x) (- x) x))

    (define (quotient n1 n2) ((primitive-procedure quotient) n1 n2))

    (define (remainder n1 n2) ((primitive-procedure remainder) n1 n2))

    (define (modulo n1 n2)
     (if (or (and (positive? n1) (negative? n2))
	     (and (negative? n1) (positive? n2)))
	 (+ n2 (remainder n1 n2))
	 (remainder n1 n2)))

    ;; needs work: NUMERATOR DENOMINATOR

    (define (gcd . ns)
     ;; note: Support for multiple arguments incurs a penalty here.
     (cond
      ((null? ns) 0)
      ((null? (cdr ns)) (abs (car ns)))
      (else
       (let loop ((n1 (abs (car ns)))
		  (n2 (abs (car (cdr ns))))
		  (ns (cdr (cdr ns)))
		  (p? (or (inexact? (car ns)) (inexact? (car (cdr ns))))))
	(if (zero? n2)
	    (if (null? ns)
		(if p? (exact->inexact n1) n1)
		(loop n1 (abs (car ns)) (cdr ns) (or p? (inexact? (car ns)))))
	    (let ((r (remainder n1 n2)))
	     (if (zero? r)
		 (if (null? ns)
		     (if p? (exact->inexact n2) n2)
		     (loop
		      n2 (abs (car ns)) (cdr ns) (or p? (inexact? (car ns)))))
		 (loop n2 r ns p?))))))))

    (define (lcm . ns)
     ;; note: Support for multiple arguments incurs a penalty here.
     (cond
      ((null? ns) 1)
      ((null? (cdr ns)) (abs (car ns)))
      (else
       (let loop ((n1 (abs (car ns)))
		  (n2 (abs (car (cdr ns))))
		  (ns (cdr (cdr ns)))
		  (p? (or (inexact? (car ns)) (inexact? (car (cdr ns))))))
	(let ((n (cond ((= n1 n2) n1)
		       ((zero? (remainder n1 n2)) n1)
		       ((zero? (remainder n2 n1)) n2)
		       (else (* (quotient n1 (gcd n1 n2)) n2)))))
	 (if (null? ns)
	     (if p? (exact->inexact n) n)
	     (loop n (abs (car ns)) (cdr ns) (or p? (inexact? (car ns))))))))))

    (define (<< n1 n2) ((primitive-procedure <<) n1 n2)) ;Extension to R4RS.

    (define (>> n1 n2) ((primitive-procedure >>) n1 n2)) ;Extension to R4RS.

    (define (bitwise-not n)		;Extension to R4RS.
     ((primitive-procedure bitwise-not) n))

    (define (bitwise-and . ns)		;Extension to R4RS.
     (if (null? ns)
	 ((primitive-procedure bitwise-and))
	 (let loop ((ns (cdr ns))
		    (r ((primitive-procedure bitwise-and) (car ns))))
	  (if (null? ns)
	      r
	      (loop (cdr ns)
		    ((primitive-procedure bitwise-and) r (car ns)))))))

    (define (bitwise-or . ns)		;Extension to R4RS.
     (if (null? ns)
	 ((primitive-procedure bitwise-or))
	 (let loop ((ns (cdr ns))
		    (r ((primitive-procedure bitwise-or) (car ns))))
	  (if (null? ns)
	      r
	      (loop (cdr ns) ((primitive-procedure bitwise-or) r (car ns)))))))

    (define (bitwise-xor . ns)		;Extension to R4RS.
     (if (null? ns)
	 ((primitive-procedure bitwise-xor))
	 (let loop ((ns (cdr ns))
		    (r ((primitive-procedure bitwise-xor) (car ns))))
	  (if (null? ns)
	      r
	      (loop (cdr ns)
		    ((primitive-procedure bitwise-xor) r (car ns)))))))

    (define (floor x) ((primitive-procedure floor) x))

    (define (ceiling x) ((primitive-procedure ceiling) x))

    (define (truncate x) ((primitive-procedure truncate) x))

    (define (round x) ((primitive-procedure round) x))

    ;; needs work: RATIONALIZE

    (define (exp z) ((primitive-procedure exp) z))

    (define (log z) ((primitive-procedure log) z))

    (define (sin z) ((primitive-procedure sin) z))

    (define (cos z) ((primitive-procedure cos) z))

    (define (tan z) ((primitive-procedure tan) z))

    (define (asin z) ((primitive-procedure asin) z))

    (define (acos z) ((primitive-procedure acos) z))

    (define (atan x . y)
     (cond
      ((null? y) ((primitive-procedure atan) x))
      ((null? (cdr y)) ((primitive-procedure atan) x (car y)))
      (else
       (panic "Attempt to call ATAN with the wrong number of arguments"))))

    (define (sqrt z) ((primitive-procedure sqrt) z))

    (define (expt z1 z2) ((primitive-procedure expt) z1 z2))

    ;; needs work: MAKE-RECTANGULAR MAKE-POLAR REAL-PART IMAG-PART MAGNITUDE
    ;;             ANGLE

    (define (exact->inexact z) ((primitive-procedure exact->inexact) z))

    (define (inexact->exact z) ((primitive-procedure inexact->exact) z))

    ;; 6.5.6 Numerical input and output

    (define number->string ((lambda ()))) ;Defined in *I/O*.

    (define string->number ((lambda ()))) ;Defined in *I/O*.

    ;; 6.6 Characters

    (define (char? obj) ((primitive-procedure char?) obj))

    (define (char=? char1 char2 . chars)
     (let loop? ((char1 char1) (char2 char2) (chars chars))
      (and (= (char->integer char1) (char->integer char2))
	   (or (null? chars) (loop? char2 (car chars) (cdr chars))))))

    (define (char<? char1 char2 . chars)
     (let loop? ((char1 char1) (char2 char2) (chars chars))
      (and (< (char->integer char1) (char->integer char2))
	   (or (null? chars) (loop? char2 (car chars) (cdr chars))))))

    (define (char>? char1 char2 . chars)
     (let loop? ((char1 char1) (char2 char2) (chars chars))
      (and (> (char->integer char1) (char->integer char2))
	   (or (null? chars) (loop? char2 (car chars) (cdr chars))))))

    (define (char<=? char1 char2 . chars)
     (let loop? ((char1 char1) (char2 char2) (chars chars))
      (and (<= (char->integer char1) (char->integer char2))
	   (or (null? chars) (loop? char2 (car chars) (cdr chars))))))

    (define (char>=? char1 char2 . chars)
     (let loop? ((char1 char1) (char2 char2) (chars chars))
      (and (>= (char->integer char1) (char->integer char2))
	   (or (null? chars) (loop? char2 (car chars) (cdr chars))))))

    (define (char-ci=? char1 char2 . chars)
     (let loop? ((char1 (char-upcase char1))
		 (char2 (char-upcase char2))
		 (chars chars))
      (and (= (char->integer char1) (char->integer char2))
	   (or (null? chars)
	       (loop? char2 (char-upcase (car chars)) (cdr chars))))))

    (define (char-ci<? char1 char2 . chars)
     (let loop? ((char1 (char-upcase char1))
		 (char2 (char-upcase char2))
		 (chars chars))
      (and (< (char->integer char1) (char->integer char2))
	   (or (null? chars)
	       (loop? char2 (char-upcase (car chars)) (cdr chars))))))

    (define (char-ci>? char1 char2 . chars)
     (let loop? ((char1 (char-upcase char1))
		 (char2 (char-upcase char2))
		 (chars chars))
      (and (> (char->integer char1) (char->integer char2))
	   (or (null? chars)
	       (loop? char2 (char-upcase (car chars)) (cdr chars))))))

    (define (char-ci<=? char1 char2 . chars)
     (let loop? ((char1 (char-upcase char1))
		 (char2 (char-upcase char2))
		 (chars chars))
      (and (<= (char->integer char1) (char->integer char2))
	   (or (null? chars)
	       (loop? char2 (char-upcase (car chars)) (cdr chars))))))

    (define (char-ci>=? char1 char2 . chars)
     (let loop? ((char1 (char-upcase char1))
		 (char2 (char-upcase char2))
		 (chars chars))
      (and (>= (char->integer char1) (char->integer char2))
	   (or (null? chars)
	       (loop? char2 (char-upcase (car chars)) (cdr chars))))))

    ;; Sven Hartrumpf <Sven.Hartrumpf@FernUni-Hagen.de> contributed variations
    ;; on the next five definitions.
    (define char-alphabetic?
     (let ((char-alphabetic?
	    '#(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
		  #t #t #t #t #t #t #t #t #t #t #t #f #f #f #f #f
		  #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
		  #t #t #t #t #t #t #t #t #t #t #t #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
		  #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t
		  #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
		  #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t)))
      (lambda (char) (vector-ref char-alphabetic? (char->integer char)))))

    (define char-numeric?
     (let ((char-numeric?
	    '#(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #t #t #t #t #t #t #t #t #t #t #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
      (lambda (char) (vector-ref char-numeric? (char->integer char)))))

    (define char-whitespace?
     (let ((char-whitespace?
	    '#(#f #f #f #f #f #f #f #f #f #t #t #f #t #t #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
      (lambda (char) (vector-ref char-whitespace? (char->integer char)))))

    (define char-upper-case?
     (let ((char-upper-case?
	    '#(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
		  #t #t #t #t #t #t #t #t #t #t #t #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
		  #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
      (lambda (letter) (vector-ref char-upper-case? (char->integer letter)))))

    (define char-lower-case?
     (let ((char-lower-case?
	    '#(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
		  #t #t #t #t #t #t #t #t #t #t #t #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t
		  #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
		  #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t)))
      (lambda (letter) (vector-ref char-lower-case? (char->integer letter)))))

    (define (char->integer char) ((primitive-procedure char->integer) char))

    (define (integer->char k) ((primitive-procedure integer->char) k))

    ;; Sven Hartrumpf <Sven.Hartrumpf@FernUni-Hagen.de> contributed variations
    ;; on the next two definitions.
    (define char-upcase
     (let ((char-upcase
	    '#(0 1 2 3 4 5 6 7
		 8 9 10 11 12 13 14 15
		 16 17 18 19 20 21 22 23
		 24 25 26 27 28 29 30 31
		 32 33 34 35 36 37 38 39
		 40 41 42 43 44 45 46 47
		 48 49 50 51 52 53 54 55
		 56 57 58 59 60 61 62 63
		 64 65 66 67 68 69 70 71
		 72 73 74 75 76 77 78 79
		 80 81 82 83 84 85 86 87
		 88 89 90 91 92 93 94 95
		 96 65 66 67 68 69 70 71
		 72 73 74 75 76 77 78 79
		 80 81 82 83 84 85 86 87
		 88 89 90 123 124 125 126 127
		 128 129 130 131 132 133 134 135
		 136 137 138 139 140 141 142 143
		 144 145 146 147 148 149 150 151
		 152 153 154 155 156 157 158 159
		 160 161 162 163 164 165 166 167
		 168 169 170 171 172 173 174 175
		 176 177 178 179 180 181 182 183
		 184 185 186 187 188 189 190 191
		 192 193 194 195 196 197 198 199
		 200 201 202 203 204 205 206 207
		 208 209 210 211 212 213 214 215
		 216 217 218 219 220 221 222 191
		 192 193 194 195 196 197 198 199
		 200 201 202 203 204 205 206 207
		 208 209 210 211 212 213 214 247
		 216 217 218 219 220 221 222 223)))
      (lambda (char)
       (integer->char (vector-ref char-upcase (char->integer char))))))

    (define char-downcase
     (let ((char-downcase
	    '#(0 1 2 3 4 5 6 7
		 8 9 10 11 12 13 14 15
		 16 17 18 19 20 21 22 23
		 24 25 26 27 28 29 30 31
		 32 33 34 35 36 37 38 39
		 40 41 42 43 44 45 46 47
		 48 49 50 51 52 53 54 55
		 56 57 58 59 60 61 62 63
		 64 97 98 99 100 101 102 103
		 104 105 106 107 108 109 110 111
		 112 113 114 115 116 117 118 119
		 120 121 122 91 92 93 94 95
		 96 97 98 99 100 101 102 103
		 104 105 106 107 108 109 110 111
		 112 113 114 115 116 117 118 119
		 120 121 122 123 124 125 126 127
		 128 129 130 131 132 133 134 135
		 136 137 138 139 140 141 142 143
		 144 145 146 147 148 149 150 151
		 152 153 154 155 156 157 158 159
		 160 161 162 163 164 165 166 167
		 168 169 170 171 172 173 174 175
		 176 177 178 179 180 181 182 183
		 184 185 186 187 188 189 190 191
		 224 225 226 227 228 229 230 231
		 232 233 234 235 236 237 238 239
		 240 241 242 243 244 245 246 215
		 248 249 250 251 252 253 254 223
		 224 225 226 227 228 229 230 231
		 232 233 234 235 236 237 238 239
		 240 241 242 243 244 245 246 247
		 248 249 250 251 252 253 254 255)))
      (lambda (char)
       (integer->char (vector-ref char-downcase (char->integer char))))))

    ;; 6.7 Strings

    (define (string? obj) ((primitive-procedure string?) obj))

    (define (make-string k . char)
     (cond
      ((null? char) ((primitive-procedure make-string) k))
      ((null? (cdr char)) ((primitive-procedure make-string) k (car char)))
      (else
       (panic
	"Attempt to call MAKE-STRING with the wrong number of arguments"))))

    (define (string . chars)
     (let ((r (make-string (list-length chars))))
      (let loop ((k 0) (chars chars))
       (if (null? chars)
	   r
	   (begin (string-set! r k (car chars)) (loop (+ k 1) (cdr chars)))))))

    (define (string-length string)
     ((primitive-procedure string-length) string))

    (define (string-ref string k) ((primitive-procedure string-ref) string k))

    (define (string-set! string k char)
     ((primitive-procedure string-set!) string k char))

    (define (string=? string1 string2 . strings)
     (let loop? ((string1 string1) (string2 string2) (strings strings))
      (let ((n (string-length string1)))
       (and (= n (string-length string2))
	    (let loop? ((k 0))
	     (or (= k n)
		 (and (char=? (string-ref string1 k) (string-ref string2 k))
		      (loop? (+ k 1)))))
	    (or (null? strings)
		(loop? string2 (car strings) (cdr strings)))))))

    (define (string-ci=? string1 string2 . strings)
     (let loop? ((string1 string1) (string2 string2) (strings strings))
      (let ((n (string-length string1)))
       (and (= n (string-length string2))
	    (let loop? ((k 0))
	     (or (= k n)
		 (and (char-ci=? (string-ref string1 k) (string-ref string2 k))
		      (loop? (+ k 1)))))
	    (or (null? strings)
		(loop? string2 (car strings) (cdr strings)))))))

    (define (string<? string1 string2 . strings)
     (let loop? ((string1 string1)
		 (n1 (string-length string1))
		 (string2 string2)
		 (n2 (string-length string2))
		 (strings strings))
      (and (let loop? ((k 0))
	    (if (= k n1)
		(< k n2)
		(and (< k n2)
		     (or (char<? (string-ref string1 k) (string-ref string2 k))
			 (and (char=?
			       (string-ref string1 k) (string-ref string2 k))
			      (loop? (+ k 1)))))))
	   (or (null? strings)
	       (loop? string2
		      n2
		      (car strings)
		      (string-length (car strings))
		      (cdr strings))))))

    (define (string>? string1 string2 . strings)
     (let loop? ((string1 string1)
		 (n1 (string-length string1))
		 (string2 string2)
		 (n2 (string-length string2))
		 (strings strings))
      (and (let loop? ((k 0))
	    (if (= k n2)
		(< k n1)
		(and (< k n1)
		     (or (char>? (string-ref string1 k) (string-ref string2 k))
			 (and (char=?
			       (string-ref string1 k) (string-ref string2 k))
			      (loop? (+ k 1)))))))
	   (or (null? strings)
	       (loop? string2
		      n2
		      (car strings)
		      (string-length (car strings))
		      (cdr strings))))))

    (define (string<=? string1 string2 . strings)
     (let loop? ((string1 string1)
		 (n1 (string-length string1))
		 (string2 string2)
		 (n2 (string-length string2))
		 (strings strings))
      (and (let loop? ((k 0))
	    (if (= k n1)
		(<= k n2)
		(and (< k n2)
		     (or (char<? (string-ref string1 k) (string-ref string2 k))
			 (and (char=?
			       (string-ref string1 k) (string-ref string2 k))
			      (loop? (+ k 1)))))))
	   (or (null? strings)
	       (loop? string2
		      n2
		      (car strings)
		      (string-length (car strings))
		      (cdr strings))))))

    (define (string>=? string1 string2 . strings)
     (let loop? ((string1 string1)
		 (n1 (string-length string1))
		 (string2 string2)
		 (n2 (string-length string2))
		 (strings strings))
      (and (let loop? ((k 0))
	    (if (= k n2)
		(<= k n1)
		(and (< k n1)
		     (or (char>? (string-ref string1 k) (string-ref string2 k))
			 (and (char=?
			       (string-ref string1 k) (string-ref string2 k))
			      (loop? (+ k 1)))))))
	   (or (null? strings)
	       (loop? string2
		      n2
		      (car strings)
		      (string-length (car strings))
		      (cdr strings))))))

    (define (string-ci<? string1 string2 . strings)
     (let loop? ((string1 string1)
		 (n1 (string-length string1))
		 (string2 string2)
		 (n2 (string-length string2))
		 (strings strings))
      (and (let loop? ((k 0))
	    (if (= k n1)
		(< k n2)
		(and (< k n2)
		     (or (char-ci<?
			  (string-ref string1 k) (string-ref string2 k))
			 (and (char-ci=?
			       (string-ref string1 k) (string-ref string2 k))
			      (loop? (+ k 1)))))))
	   (or (null? strings)
	       (loop? string2
		      n2
		      (car strings)
		      (string-length (car strings))
		      (cdr strings))))))

    (define (string-ci>? string1 string2 . strings)
     (let loop? ((string1 string1)
		 (n1 (string-length string1))
		 (string2 string2)
		 (n2 (string-length string2))
		 (strings strings))
      (and (let loop? ((k 0))
	    (if (= k n2)
		(< k n1)
		(and (< k n1)
		     (or (char-ci>?
			  (string-ref string1 k) (string-ref string2 k))
			 (and (char-ci=?
			       (string-ref string1 k) (string-ref string2 k))
			      (loop? (+ k 1)))))))
	   (or (null? strings)
	       (loop? string2
		      n2
		      (car strings)
		      (string-length (car strings))
		      (cdr strings))))))

    (define (string-ci<=? string1 string2 . strings)
     (let loop? ((string1 string1)
		 (n1 (string-length string1))
		 (string2 string2)
		 (n2 (string-length string2))
		 (strings strings))
      (and (let loop? ((k 0))
	    (if (= k n1)
		(<= k n2)
		(and (< k n2)
		     (or (char-ci<?
			  (string-ref string1 k) (string-ref string2 k))
			 (and (char-ci=?
			       (string-ref string1 k) (string-ref string2 k))
			      (loop? (+ k 1)))))))
	   (or (null? strings)
	       (loop? string2
		      n2
		      (car strings)
		      (string-length (car strings))
		      (cdr strings))))))

    (define (string-ci>=? string1 string2 . strings)
     (let loop? ((string1 string1)
		 (n1 (string-length string1))
		 (string2 string2)
		 (n2 (string-length string2))
		 (strings strings))
      (and (let loop? ((k 0))
	    (if (= k n2)
		(<= k n1)
		(and (< k n1)
		     (or (char-ci>?
			  (string-ref string1 k) (string-ref string2 k))
			 (and (char-ci=?
			       (string-ref string1 k) (string-ref string2 k))
			      (loop? (+ k 1)))))))
	   (or (null? strings)
	       (loop? string2
		      n2
		      (car strings)
		      (string-length (car strings))
		      (cdr strings))))))

    (define (substring string start end)
     (let ((r (make-string (- end start))))
      (let loop ((k start))
       (if (< k end)
	   (begin
	    (string-set! r (- k start) (string-ref string k))
	    (loop (+ k 1)))))
      r))

    (define (string-append . strings)
     ;; note: Support for multiple arguments incurs a penalty here.
     (let* ((r (make-string (let loop ((k 0) (strings strings))
			     (if (null? strings)
				 k
				 (loop (+ k (string-length (car strings)))
				       (cdr strings))))))
	    (k 0))
      (let loop ((strings strings))
       (if (not (null? strings))
	   (let ((n (string-length (car strings))))
	    (let loop ((l 0))
	     (if (not (= l n))
		 (begin (string-set! r k (string-ref (car strings) l))
			(set! k (+ k 1))
			(loop (+ l 1)))))
	    (loop (cdr strings)))))
      r))

    (define (string->list string)
     ;; needs work: To make tail recursive.
     (let ((n (string-length string)))
      (let loop ((k 0))
       (if (= k n) '() (cons (string-ref string k) (loop (+ k 1)))))))

    (define (list->string list)
     (let ((r (make-string (list-length list))))
      (let loop ((k 0))
       (if (not (null? list))
	   (begin (string-set! r k (car list))
		  (set! list (cdr list))
		  (loop (+ k 1)))))
      r))

    (define (string-copy string)
     (let* ((n (string-length string))
	    (r (make-string n)))
      (let loop ((k 0))
       (if (not (= k n))
	   (begin (string-set! r k (string-ref string k)) (loop (+ k 1)))))
      r))

    (define (string-fill! string char)
     (let ((n (string-length string)))
      (let loop ((k 0))
       (if (not (= k n))
	   (begin (string-set! string k char) (loop (+ k 1)))))))

    (define (string-reverse string)	;Extension to R4RS.
     (let* ((n (string-length string))
	    (r (make-string n)))
      (let loop ((k 0))
       (if (not (= k n))
	   (begin (string-set! r k (string-ref string (- (- n k) 1)))
		  (loop (+ k 1)))))
      r))

    ;; 6.8 Vectors

    (define (vector? obj) ((primitive-procedure vector?) obj))

    (define (make-vector k . fill)
     (cond
      ((null? fill) ((primitive-procedure make-vector) k))
      ((null? (cdr fill)) ((primitive-procedure make-vector) k (car fill)))
      (else
       (panic
	"Attempt to call MAKE-VECTOR with the wrong number of arguments"))))

    (define (make-displaced-vector vector k1 k2) ;Extension to R4RS.
     ((primitive-procedure make-displaced-vector) vector k1 k2))

    (define (vector . objs)
     (let ((r (make-vector (list-length objs))))
      (let loop ((k 0) (objs objs))
       (if (null? objs)
	   r
	   (begin (vector-set! r k (car objs)) (loop (+ k 1) (cdr objs)))))))

    (define (vector-length vector)
     ((primitive-procedure vector-length) vector))

    (define (vector-ref vector k) ((primitive-procedure vector-ref) vector k))

    (define (vector-set! vector k obj)
     ((primitive-procedure vector-set!) vector k obj))

    (define (vector->list vector)
     ;; needs work: To make tail recursive.
     (let loop ((k 0))
      (if (= k (vector-length vector))
	  '()
	  (cons (vector-ref vector k) (loop (+ k 1))))))

    (define (list->vector list)
     (let ((r (make-vector (list-length list))))
      (let loop ((k 0))
       (if (not (null? list))
	   (begin (vector-set! r k (car list))
		  (set! list (cdr list))
		  (loop (+ k 1)))))
      r))

    (define (vector-fill! vector obj)
     (let loop ((k 0))
      (if (not (= k (vector-length vector)))
	  (begin (vector-set! vector k obj) (loop (+ k 1))))))

    (define (subvector vector start end) ;Extension to R4RS.
     (let ((r (make-vector (- end start))))
      (let loop ((k 0))
       (if (< k (- end start))
	   (begin (vector-set! r k (vector-ref vector (+ k start)))
		  (loop (+ k 1)))))
      r))

    (define (vector-append . vectors)	;Extension to R4RS.
     ;; note: Support for multiple arguments incurs a penalty here.
     (let* ((r (make-vector (let loop ((k 0) (vectors vectors))
			     (if (null? vectors)
				 k
				 (loop (+ k (vector-length (car vectors)))
				       (cdr vectors))))))
	    (k 0))
      (let loop ((vectors vectors))
       (if (not (null? vectors))
	   (begin (let loop ((l 0))
		   (if (not (= l (vector-length (car vectors))))
		       (begin (vector-set! r k (vector-ref (car vectors) l))
			      (set! k (+ k 1))
			      (loop (+ l 1)))))
		  (loop (cdr vectors)))))
      r))

    (define (vector-reverse vector)	;Extension to R4RS.
     (let ((r (make-vector (vector-length vector))))
      (let loop ((k 0))
       (if (not (= k (vector-length vector)))
	   (begin (vector-set!
		   r k (vector-ref vector (- (- (vector-length vector) k) 1)))
		  (loop (+ k 1)))))
      r))

    (define (vector-copy vector)	;Extension to R4RS.
     (let ((r (make-vector (vector-length vector))))
      (let loop ((k 0))
       (if (not (= k (vector-length vector)))
	   (begin (vector-set! r k (vector-ref vector k)) (loop (+ k 1)))))
      r))

    ;; 6.9 Control features

    (define (procedure? obj) ((primitive-procedure procedure?) obj))

    (define (apply proc arg1 . args)
     (if (null? args)
	 ((primitive-procedure apply) proc arg1)
	 (let loop ((arg1 (list arg1)) (args args))
	  (if (null? (cdr args))
	      (let loop ((arg1 (car args)) (args arg1))
	       (if (null? args)
		   ((primitive-procedure apply) proc arg1)
		   (loop (cons (car args) arg1) (cdr args))))
	      (loop (cons (car args) arg1) (cdr args))))))

    (define (map proc list1 . lists)
     ;; note: Support for multiple arguments incurs a penalty here.
     (cond
      ;; note: This special-cases the one-argument case for speed.
      ((null? lists)
       (let loop ((list1 list1) (c '()))
	(if (null? list1)
	    (list-reverse c)
	    (loop (cdr list1) (cons (proc (car list1)) c)))))
      ;; note: This special-cases the two-argument case for speed.
      ((null? (cdr lists))
       (let loop ((list1 list1) (list2 (car lists)) (c '()))
	(if (null? list1)
	    (list-reverse c)
	    (loop (cdr list1)
		  (cdr list2)
		  (cons (proc (car list1) (car list2)) c)))))
      (else
       (let loop ((list1 list1) (lists lists) (c '()))
	(if (null? list1)
	    (list-reverse c)
	    (loop (cdr list1)
		  (let loop ((lists lists) (c '()))
		   (if (null? lists)
		       (list-reverse c)
		       (loop (cdr lists) (cons (cdr (car lists)) c))))
		  (cons
		   (apply proc
			  (car list1)
			  (let loop ((lists lists) (c '()))
			   (if (null? lists)
			       (list-reverse c)
			       (loop (cdr lists) (cons (car (car lists)) c)))))
		   c)))))))

    (define (for-each proc list1 . lists)
     ;; note: Support for multiple arguments incurs a penalty here.
     (cond
      ((null? lists)
       ;; note: This special-cases the one-argument case for speed.
       (let loop ((list1 list1))
	(if (not (null? list1))
	    (begin (proc (car list1)) (loop (cdr list1))))))
      ((null? (cdr lists))
       ;; note: This special-cases the two-argument case for speed.
       (let loop ((list1 list1) (list2 (car lists)))
	(if (not (null? list1))
	    (begin (proc (car list1) (car list2))
		   (loop (cdr list1) (cdr list2))))))
      (else
       (let loop ((list1 list1) (lists lists))
	(if (not (null? list1))
	    (begin
	     (apply proc
		    (car list1)
		    (let loop ((lists lists) (c '()))
		     (if (null? lists)
			 (list-reverse c)
			 (loop (cdr lists) (cons (car (car lists)) c)))))
	     (loop (cdr list1)
		   (let loop ((lists lists) (c '()))
		    (if (null? lists)
			(list-reverse c)
			(loop (cdr lists) (cons (cdr (car lists)) c)))))))))))

    (define (force promise) (promise))

    (define (call-with-current-continuation proc)
     ((primitive-procedure call-with-current-continuation) proc))

    ;; 6.10 Input and Output

    ;; 6.10.1 Ports

    (define (call-with-input-file string proc)
     (let* ((input-port (open-input-file string))
	    (r (proc input-port)))
      (close-input-port input-port)
      r))

    (define (call-with-output-file string proc)
     (let* ((output-port (open-output-file string))
	    (r (proc output-port)))
      (close-output-port output-port)
      r))

    (define (input-port? obj) ((primitive-procedure input-port?) obj))

    (define (output-port? obj) ((primitive-procedure output-port?) obj))

    (define current-input-port ((lambda ()))) ;Defined in *I/O*.

    (define current-output-port ((lambda ()))) ;Defined in *I/O*.

    (define with-input-from-file ((lambda ())))	;Defined in *I/O*.

    (define with-output-to-file ((lambda ()))) ;Defined in *I/O*.

    (define (open-input-file filename)
     ((primitive-procedure open-input-file) filename))

    (define (open-output-file filename)
     ((primitive-procedure open-output-file) filename))

    (define (close-input-port port)
     ((primitive-procedure close-input-port) port))

    (define (close-output-port port)
     ((primitive-procedure close-output-port) port))

    ;; 6.10.2 Input

    ,*read*

    (define read-char ((lambda ())))	;Defined in *I/O*.

    (define peek-char ((lambda ())))	;Defined in *I/O*.

    (define (eof-object? obj) ((primitive-procedure eof-object?) obj))

    (define char-ready? ((lambda ())))	;Defined in *I/O*.

    ;; 6.10.3 Output

    (define write ((lambda ())))	;Defined in *I/O*.

    (define display ((lambda ())))	;Defined in *I/O*.

    (define newline ((lambda ())))	;Defined in *I/O*.

    (define write-char ((lambda ())))	;Defined in *I/O*.

    ;; Extension to R4RS.
    (define define-write-method ((lambda ()))) ;Defined in *I/O*.

    ;; Extension to R4RS.
    (define define-display-method ((lambda ()))) ;Defined in *I/O*.

    ,*i/o*

    (define (panic string)		;Extension to R4RS.
     ((primitive-procedure panic) string))

    (define (pointer? obj)		;Extension to R4RS.
     ((primitive-procedure pointer?) obj))

    ;; Extension to R4RS.
    (define (integer->string k) ((primitive-procedure integer->string) k))

    (define (integer->input-port k)	;Extension to R4RS.
     ((primitive-procedure integer->input-port) k))

    (define (integer->output-port k)	;Extension to R4RS.
     ((primitive-procedure integer->output-port) k))

    ;; Extension to R4RS.
    (define (integer->pointer k) ((primitive-procedure integer->pointer) k))

    ;; 6.10.4 System interface

    ;; needs work: LOAD TRANSCRIPT-ON TRANSCRIPT-OFF

    (define ,*list->vector* list->vector)
    (define ,*append* append)
    (define ,*cons* cons)
    (define ,*eqv?* eqv?)
    (let ((not not)
	  (boolean? boolean?)
	  (eqv? eqv?)
	  (eq? eq?)
	  (equal? equal?)
	  (pair? pair?)
	  (cons cons)
	  (car car)
	  (cdr cdr)
	  (set-car! set-car!)
	  (set-cdr! set-cdr!)
	  (caar caar)
	  (cadr cadr)
	  (cdar cdar)
	  (cddr cddr)
	  (caaar caaar)
	  (caadr caadr)
	  (cadar cadar)
	  (caddr caddr)
	  (cdaar cdaar)
	  (cdadr cdadr)
	  (cddar cddar)
	  (cdddr cdddr)
	  (caaaar caaaar)
	  (caaadr caaadr)
	  (caadar caadar)
	  (caaddr caaddr)
	  (cadaar cadaar)
	  (cadadr cadadr)
	  (caddar caddar)
	  (cadddr cadddr)
	  (cdaaar cdaaar)
	  (cdaadr cdaadr)
	  (cdadar cdadar)
	  (cdaddr cdaddr)
	  (cddaar cddaar)
	  (cddadr cddadr)
	  (cdddar cdddar)
	  (cddddr cddddr)
	  (null? null?)
	  (list? list?)
	  (list list)
	  (list-length list-length)
	  (length length)
	  (sublist sublist)
	  (sub sub)
	  (list-append list-append)
	  (append append)
	  (list-reverse list-reverse)
	  (reverse reverse)
	  (list-tail list-tail)
	  (list-ref list-ref)
	  (ref ref)
	  (memq memq)
	  (memv memv)
	  (member member)
	  (assq assq)
	  (assv assv)
	  (assoc assoc)
	  (list-set! list-set!)
	  (ref! ref!)
	  (list-fill! list-fill!)
	  (fill! fill!)
	  (list-copy list-copy)
	  (copy copy)
	  (symbol? symbol?)
	  (symbol->string symbol->string)
	  (string->uninterned-symbol string->uninterned-symbol)
	  (string->symbol string->symbol)
	  (number? number?)
	  (complex? complex?)
	  (real? real?)
	  (rational? rational?)
	  (integer? integer?)
	  (exact? exact?)
	  (inexact? inexact?)
	  (= =)
	  (< <)
	  (> >)
	  (<= <=)
	  (>= >=)
	  (zero? zero?)
	  (positive? positive?)
	  (negative? negative?)
	  (odd? odd?)
	  (even? even?)
	  (max max)
	  (min min)
	  (+ +)
	  (* *)
	  (- -)
	  (/ /)
	  (abs abs)
	  (quotient quotient)
	  (remainder remainder)
	  (modulo modulo)
	  (gcd gcd)
	  (lcm lcm)
	  (<< <<)
	  (>> >>)
	  (bitwise-not bitwise-not)
	  (bitwise-and bitwise-and)
	  (bitwise-or bitwise-or)
	  (bitwise-xor bitwise-xor)
	  (floor floor)
	  (ceiling ceiling)
	  (truncate truncate)
	  (round round)
	  (exp exp)
	  (log log)
	  (sin sin)
	  (cos cos)
	  (tan tan)
	  (asin asin)
	  (acos acos)
	  (atan atan)
	  (sqrt sqrt)
	  (expt expt)
	  (exact->inexact exact->inexact)
	  (inexact->exact inexact->exact)
	  (number->string number->string)
	  (string->number string->number)
	  (char? char?)
	  (char=? char=?)
	  (char<? char<?)
	  (char>? char>?)
	  (char<=? char<=?)
	  (char>=? char>=?)
	  (char-ci=? char-ci=?)
	  (char-ci<? char-ci<?)
	  (char-ci>? char-ci>?)
	  (char-ci<=? char-ci<=?)
	  (char-ci>=? char-ci>=?)
	  (char-alphabetic? char-alphabetic?)
	  (char-numeric? char-numeric?)
	  (char-whitespace? char-whitespace?)
	  (char-upper-case? char-upper-case?)
	  (char-lower-case? char-lower-case?)
	  (char->integer char->integer)
	  (integer->char integer->char)
	  (char-upcase char-upcase)
	  (char-downcase char-downcase)
	  (string? string?)
	  (make-string make-string)
	  (string string)
	  (string-length string-length)
	  (string-ref string-ref)
	  (string-set! string-set!)
	  (string=? string=?)
	  (string-ci=? string-ci=?)
	  (string<? string<?)
	  (string>? string>?)
	  (string<=? string<=?)
	  (string>=? string>=?)
	  (string-ci<? string-ci<?)
	  (string-ci>? string-ci>?)
	  (string-ci<=? string-ci<=?)
	  (string-ci>=? string-ci>=?)
	  (substring substring)
	  (string-append string-append)
	  (string->list string->list)
	  (list->string list->string)
	  (string-copy string-copy)
	  (string-fill! string-fill!)
	  (string-reverse string-reverse)
	  (vector? vector?)
	  (make-vector make-vector)
	  (make-displaced-vector make-displaced-vector)
	  (vector vector)
	  (vector-length vector-length)
	  (vector-ref vector-ref)
	  (vector-set! vector-set!)
	  (vector->list vector->list)
	  (list->vector list->vector)
	  (vector-fill! vector-fill!)
	  (subvector subvector)
	  (vector-append vector-append)
	  (vector-reverse vector-reverse)
	  (vector-copy vector-copy)
	  (procedure? procedure?)
	  (apply apply)
	  (map map)
	  (for-each for-each)
	  (force force)
	  (call-with-current-continuation call-with-current-continuation)
	  (call-with-input-file call-with-input-file)
	  (call-with-output-file call-with-output-file)
	  (input-port? input-port?)
	  (output-port? output-port?)
	  (current-input-port current-input-port)
	  (current-output-port current-output-port)
	  (with-input-from-file with-input-from-file)
	  (with-output-to-file with-output-to-file)
	  (open-input-file open-input-file)
	  (open-output-file open-output-file)
	  (close-input-port close-input-port)
	  (close-output-port close-output-port)
	  (read read)
	  (read-char read-char)
	  (peek-char peek-char)
	  (eof-object? eof-object?)
	  (char-ready? char-ready?)
	  (write write)
	  (display display)
	  (newline newline)
	  (write-char write-char)
	  (define-write-method define-write-method)
	  (define-display-method define-display-method)
	  (panic panic)
	  (pointer? pointer?)
	  (integer->string integer->string)
	  (integer->input-port integer->input-port)
	  (integer->output-port integer->output-port)
	  (integer->pointer integer->pointer))
     ,@ss))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
