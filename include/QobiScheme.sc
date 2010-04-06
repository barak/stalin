;;; LaHaShem HaAretz U'Mloah

;;; Stalin 0.10 - A global optimizing compiler for Scheme
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997, 1998, 1999, 2000, and 2001 NEC Research Institute, Inc. All
;;; rights reserved.
;;; Copyright 2002 and 2003 Purdue University. All rights reserved.

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

;;; Derived from the t4aug98 archive of QobiScheme, updated to the m7dec98,
;;; m24jan00, f10mar00, h22apr00, f5may00, m12jun00, and m25jun01 archives.

;;; removed: MODULE

;;; TTMTTD
;;;  1. learn how to use profiler
;;;  2. self-documentation
;;;  3. ability to abort out of button presses
;;;  4. What if debugger called inside WITH-INPUT-FROM-FILE or
;;;     WITH-OUTPUT-TO-FILE? I.e. should temporarily rebind CURRENT-INPUT-PORT
;;;     and CURRENT-OUTPUT-PORT inside debugger.
;;;  5. Should catch stack overflow error and out of memory error.
;;;  6. $, $$, and $$$ only work in debugger.
;;;  7. What about errors inside debugger?
;;;  8. Breakpoints, tracing, and timeouts.
;;;  9. Should save error string and only call format once on format-string
;;;     and args.
;;; 10. Can't nest interrupts more than two deep.
;;; 11. Need to make c-z c, c-z e, c-d, c-z a, and m-TAB work.
;;; 12. Need way to set DISPLAY, SCGCINFO, SCHEAP, SCLIMIT, SCMAXHEAP,
;;;     stack (and other) limits, and cd.
;;; 13. Maybe put back checks for "SCEVAL_INTERPRETED-PROC" and
;;;     "LOOP [inside EXEC]".

(include "Scheme-to-C-compatibility")	;added
(include "xlib")			;changed

(define pp write)			;added
(define (remq! x l) (panic "REMQ! is not (yet) implemented")) ;added

;;; System Conditionalization

;;; note: The following can't use TMP since the variable *TMP* might not be
;;;       initialized yet.

(define *cpu-type* #f)

(define (cpu-type)
 (unless *cpu-type*
  (system "uname -m >/tmp/QobiScheme.tmp")
  (set! *cpu-type* (first (read-file "/tmp/QobiScheme.tmp")))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *cpu-type*)

(define *os-type* #f)

(define (os-type)
 (unless *os-type*
  (system "uname -s >/tmp/QobiScheme.tmp")
  (set! *os-type* (first (read-file "/tmp/QobiScheme.tmp")))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-type*)

(define *os-version* #f)

(define (os-version)
 (unless *os-version*
  (system "uname -r >/tmp/QobiScheme.tmp")
  (set! *os-version* (first (read-file "/tmp/QobiScheme.tmp")))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-version*)

(define *os-major-version* #f)

(define (os-major-version)
 (unless *os-major-version*
  (system "uname -r|cut -f 1 -d. >/tmp/QobiScheme.tmp")
  (set! *os-major-version*
	(string->number (first (read-file "/tmp/QobiScheme.tmp"))))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-major-version*)

(define *os-minor-version* #f)

(define (os-minor-version)
 (unless *os-minor-version*
  (system "uname -r|cut -f 2 -d. >/tmp/QobiScheme.tmp")
  (set! *os-minor-version*
	(string->number (first (read-file "/tmp/QobiScheme.tmp"))))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-minor-version*)

(define *os-sub-version* #f)

(define (os-sub-version)
 (unless *os-sub-version*
  (system "uname -r|cut -f 3 -d. >/tmp/QobiScheme.tmp")
  (set! *os-sub-version*
	(string->number (first (read-file "/tmp/QobiScheme.tmp"))))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-sub-version*)

;;; Sugar

;;; removed: EVAL-WHEN

(define first car)			;changed
(define second cadr)			;changed
(define third caddr)			;changed
(define fourth cadddr)			;changed
(define (fifth x) (car (cddddr x)))
(define (sixth x) (cadr (cddddr x)))
(define (seventh x) (caddr (cddddr x)))
(define (eighth x) (cadddr (cddddr x)))
(define (ninth x) (car (cddddr (cddddr x))))
(define (tenth x) (cadr (cddddr (cddddr x))))
(define (eleventh x) (caddr (cddddr (cddddr x))))
(define (twelfth x) (cadddr (cddddr (cddddr x))))
(define rest cdr)			;changed

(define (last x) (if (null? (rest x)) (first x) (last (rest x))))

(define (sqr x) (* x x))

(define (xor a b) (if a (not b) b))

(define (identity x) x)

(define (nan? x) (not (= x x)))

;;; removed: WHILE

;;; changed: This is just a stub.
(define *panic?* #t)

(define *program* "")			;changed

;;; removed: PANIC

(define (fuck-up) (panic "This shouldn't happen"))

;;; removed: USAGE

(define (compose . fs)
 (if (null? fs)
     identity
     (lambda (x) ((apply compose (rest fs)) ((first fs) x)))))

(define (rounded-number->string x . digits-of-precision)
 (if (null? digits-of-precision)
     (number->string (inexact->exact (round x)))
     (let* ((digits (first digits-of-precision))
	    (factor (expt 10.0 digits))
	    (n (abs (inexact->exact (round (* x factor)))))
	    (s (number->string n))
	    (l (string-length s))
	    (rs (if (< n factor)
		    (string-append "0."
				   (make-string (- digits l) #\0)
				   s)
		    (string-append (substring s 0 (- l digits))
				   "."
				   (substring s (- l digits) l)))))
      (if (< x 0) (string-append "-" rs) rs))))

(define (number->string-of-length number length)
 (let ((string (number->string number)))
  (string-append (make-string (- length (string-length string)) #\space)
		 string)))

(define (number->padded-string-of-length number length)
 (when (negative? number) (fuck-up))
 (let ((string (number->string number)))
  (string-append (make-string (- length (string-length string)) #\0) string)))

(define (number->string-of-length-and-precision number length precision)
 (let* ((negative? (negative? number))
	(integer-part (inexact->exact (floor (abs number))))
	(fraction-part
	 (inexact->exact
	  (floor (* (expt 10 precision) (- (abs number) integer-part)))))
	(integer-part-string (number->string integer-part))
	(fraction-part-string (number->string fraction-part)))
  (if negative?
      (string-append
       (make-string
	(- length (string-length integer-part-string) 2 precision) #\space)
       "-"
       integer-part-string
       "."
       (make-string (- precision (string-length fraction-part-string)) #\0)
       fraction-part-string)
      (string-append
       (make-string
	(- length (string-length integer-part-string) 1 precision) #\space)
       integer-part-string
       "."
       (make-string (- precision (string-length fraction-part-string)) #\0)
       fraction-part-string))))

(define (time format-string thunk)
 (let* ((start (clock-sample))
	(result (thunk))
	(end (clock-sample)))
  (format #t format-string
	  (number->string-of-length-and-precision (- end start) 8 2))
  result))

(define c-getenv (foreign-procedure (char*) char* "getenv" "stdlib")) ;changed

(define (getenv string)
 ;; changed
 (if (zero? (c-getenv string)) #f (c-getenv string)))

(define (archive-date)
 (rm (tmp "archive-date"))
 (system (format #f "archive-date >~a" (tmp "archive-date")))
 (let ((archive-date (read-file (tmp "archive-date"))))
  (rm (tmp "archive-date"))
  (first archive-date)))

;;; Structures

;;; removed: DEFINE-STRUCTURE

;;; removed: DEFINE-STRUCTURE-INTERNAL

;;; Sequences

(define (list-set! l i x)
 (if (zero? i) (set-car! l x) (list-set! (cdr l) (- i 1) x)))

(define (list-insert l i x)
 (if (zero? i)
     (cons x l)
     (cons (first l) (list-insert (rest l) (- i 1) x))))

(define (list-remove l i)
 (if (zero? i) (rest l) (cons (first l) (list-remove (rest l) (- i 1)))))

(define (list-replace l i x)
 (if (zero? i)
     (cons x (rest l))
     (cons (first l) (list-replace (rest l) (- i 1) x))))

(define (but-last x) (reverse (rest (reverse x))))

;;; removed: SUBLIST

;;; removed: SUBVECTOR

;;; removed: EVAL-WHEN

(define (reduce f l i)
 (cond ((null? l) i)
       ((null? (rest l)) (first l))
       (else (let loop ((l (rest l)) (c (first l)))
	      (if (null? l) c (loop (rest l) (f c (first l))))))))

(define (reduce-n f n i)
 (let loop ((i 0) (c i)) (if (>= i n) c (loop (+ i 1) (f c i)))))

(define (reduce-vector f v i)
 (let ((n (vector-length v)))
  (cond ((zero? n) i)
	((= n 1) (vector-ref v 0))
	(else (let loop ((i 1) (c (vector-ref v 0)))
	       (if (= i n) c (loop (+ i 1) (f c (vector-ref v i)))))))))

(define (sum f n)
 (let loop ((n (- n 1)) (c 0))
  (if (negative? n) c (loop (- n 1) (+ c (f n))))))

(define (product f n)
 (let loop ((n (- n 1)) (c 1))
  (if (negative? n) c (loop (- n 1) (* c (f n))))))

(define (factorial n) (product (lambda (i) (+ i 1)) n))

(define (choose n m) (product (lambda (i) (/ (+ i n (- m) 1) (+ i 1))) m))

(define (some p l . &rest)
 (let loop ((l l) (&rest &rest))
  (and (not (null? l))
       (or (apply p (first l) (map first &rest))
	   (loop (rest l) (map rest &rest))))))

(define (some-n p n)
 (let loop ((i 0)) (and (< i n) (or (p i) (loop (+ i 1))))))

(define (some-vector p v . &rest)
 (let loop ((i 0))
  (and (< i (vector-length v))
       (or (apply p
		  (vector-ref v i)
		  (map (lambda (v) (vector-ref v i)) &rest))
	   (loop (+ i 1))))))

;;; removed: EVAL-WHEN

(define (every p l . &rest)
 (let loop ((l l) (&rest &rest))
  (or (null? l)
      (and (apply p (first l) (map first &rest))
	   (loop (rest l) (map rest &rest))))))

(define (every-n p n)
 (let loop ((i 0)) (or (>= i n) (and (p i) (loop (+ i 1))))))

(define (every-vector p v . &rest)
 (let loop ((i 0))
  (or (>= i (vector-length v))
      (and (apply p
		  (vector-ref v i)
		  (map (lambda (v) (vector-ref v i)) &rest))
	   (loop (+ i 1))))))

(define (one p l . &rest)
 (let loop ((l l) (&rest &rest))
  (and (not (null? l))
       (if (apply p (first l) (map first &rest))
	   (let loop ((l (rest l)) (&rest (map rest &rest)))
	    (or (null? l)
		(and (not (apply p (first l) (map first &rest)))
		     (loop (rest l) (map rest &rest)))))
	   (loop (rest l) (map rest &rest))))))

(define (one-n p n)
 (let loop ((i 0))
  (and (< i n)
       (if (p i)
	   (let loop ((i (+ i 1)))
	    (or (>= i n) (and (not (p i)) (loop (+ i 1)))))
	   (loop (+ i 1))))))

(define (one-vector p v . &rest)
 (let loop ((i 0))
  (and (< i (vector-length v))
       (if (apply p
		  (vector-ref v i)
		  (map (lambda (v) (vector-ref v i)) &rest))
	   (let loop ((i (+ i 1)))
	    (or (>= i (vector-length v))
		(and (not (apply p
				 (vector-ref v i)
				 (map (lambda (v) (vector-ref v i)) &rest)))
		     (loop (+ i 1)))))
	   (loop (+ i 1))))))

(define (for-each-indexed f l)
 (let loop ((i 0) (l l))
  (unless (null? l) (f (first l) i) (loop (+ i 1) (rest l)))))

(define (for-each-n f n)
 (let loop ((i 0)) (when (< i n) (f i) (loop (+ i 1)))))

(define (for-each-from-a-up-to-b f a b)
 (let loop ((i a)) (when (< i b) (f i) (loop (+ i 1)))))

(define (for-each-n-decreasing f n)
 (when (> n 0) (let ((i (- n 1))) (f i) (for-each-n-decreasing f i))))

(define (for-each-vector f v . &rest)
 (for-each-n
  (lambda (i)
   (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest)))
  (vector-length v)))

;;; removed: EVAL-WHEN

(define (map-indexed f l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((i 0) (l l) (c '()))
  (if (null? l)
      (reverse c)
      (loop (+ i 1) (rest l) (cons (f (first l) i) c)))))

(define (map-n f n)
 ;; needs work: To eliminate REVERSE.
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (map-vector f v . &rest)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((u (make-vector (vector-length v))))
  (for-each-n
   (lambda (i)
    (vector-set!
     u i
     (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest))))
   (vector-length v))
  u))

(define (map-n-vector f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (when (< i n)
    (vector-set! v i (f i))
    (loop (+ i 1))))
  v))

(define (enumerate n)
 (let loop ((i (- n 1)) (c '()))
  (if (>= i 0) (loop (- i 1) (cons i c)) c)))

(define (enumerate-vector n)
 (let ((v (make-vector n)))
  (for-each-n (lambda (i) (vector-set! v i i)) n)
  v))

(define (memp p x l)
 (cond ((null? l) #f) ((p x (first l)) l) (else (memp p x (rest l)))))

(define (assp p x alist)
 (and (not (null? alist))
      (if (p x (car (first alist))) (first alist) (assp p x (rest alist)))))

(define (pairwise? p l)
 (or (null? l)
     (let loop ((l1 l) (l2 (rest l)))
      ;; needs work: To make tail recursive.
      (or (null? l2)
	  (and (p (first l1) (first l2)) (loop (rest l1) (rest l2)))))))

(define (adjoinq x l) (if (memq x l) l (cons x l)))

(define (adjoinv x l) (if (memv x l) l (cons x l)))

(define (adjoin x l) (if (member x l) l (cons x l)))

(define (adjoinp p x l) (if (memp p x l) l (cons x l)))

(define (removeq x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((eq? x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (removev x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((eqv? x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (removep p x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (remove-if p l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (remove-if-not p l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (positionq x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((eq? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (positionv x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((eqv? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (position x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((equal? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (positionp p x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (position-if p l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (position-if-not p l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p (first l)) (loop (rest l) (+ i 1)))
	(else i))))

(define (findq x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((eq? x (first l)) (first l))
	(else (loop (rest l))))))

(define (findv x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((eqv? x (first l)) (first l))
	(else (loop (rest l))))))

(define (find x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((equal? x (first l)) (first l))
	(else (loop (rest l))))))

(define (findp p x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((p x (first l)) (first l))
	(else (loop (rest l))))))

(define (find-if p l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((p (first l)) (first l))
	(else (loop (rest l))))))

(define (find-if-not p l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((p (first l)) (loop (rest l)))
	(else (first l)))))

(define (countq x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((eq? x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (countv x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((eqv? x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (count x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((equal? x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (countp p x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (count-if p l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (count-if-not p l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p (first l)) (loop (rest l) c))
	(else (loop (rest l) (+ c 1))))))

(define (subsetq? x y) (every (lambda (xe) (memq xe y)) x))

(define (subsetv? x y) (every (lambda (xe) (memv xe y)) x))

(define (subset? x y) (every (lambda (xe) (member xe y)) x))

(define (subsetp? p x y) (every (lambda (xe) (memp p xe y)) x))

(define (set-equalq? x y) (and (subsetq? x y) (subsetq? y x)))

(define (set-equalv? x y) (and (subsetv? x y) (subsetv? y x)))

(define (set-equal? x y) (and (subset? x y) (subset? y x)))

(define (set-equalp? p x y) (and (subsetp? p x y) (subsetp? p y x)))

(define (unionq x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memq (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (unionv x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memv (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (union x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((member (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (unionp p x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memp p (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (intersectionq x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memq (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (intersectionv x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memv (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (intersection x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((member (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (intersectionp p x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memp p (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (set-differenceq x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memq (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (set-differencev x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memv (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (set-difference x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((member (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (set-differencep p x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memp p (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (remove-duplicatesq x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memq (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (remove-duplicatesv x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memv (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (remove-duplicates x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((member (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (remove-duplicatesp p x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memp p (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (equivalence-classesq x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (equivalence-classesq (rest x)))
	    (z (find-if (lambda (w) (eq? y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (equivalence-classesv x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (equivalence-classesv (rest x)))
	    (z (find-if (lambda (w) (eqv? y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (equivalence-classes x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (equivalence-classes (rest x)))
	    (z (find-if (lambda (w) (equal? y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (transitive-equivalence-classesp p x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (transitive-equivalence-classesp p (rest x)))
	    (z (find-if (lambda (w) (p y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (equivalence-classesp p x)
 ;; This wrapper is necessary since P may not be transitive.
 (define (equivalence-classesp p x)
  ;; needs work: To make tail recursive.
  (if (null? x)
      '()
      (let* ((y (first x))
	     (x (equivalence-classesp p (rest x)))
	     (z (find-if (lambda (w) (some (lambda (v) (p y v)) w)) x)))
       (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))
 (let loop ((c (map list x)))
  (let ((d (map (lambda (z) (reduce append z '()))
		(equivalence-classesp
		 (lambda (x y) (some (lambda (xe) (memp p xe y)) x)) c))))
   (if (= (length d) (length c)) d (loop d)))))

(define (topological-sort p l)
 (let loop ((l l) (c '()))
  (if (null? l)
      (reverse c)
      (let ((x (find-if
		(lambda (x1)
		 (not (some (lambda (x2) (and (not (eq? x2 x1)) (p x2 x1)))
			    l)))
		l)))
       (unless x (fuck-up))
       (loop (removeq x l) (cons x c))))))

(define (every-other list)
 (cond ((null? list) '())
       ((null? (rest list)) list)
       (else (cons (first list) (every-other (rest (rest list)))))))

(define (merge list1 list2 predicate key)
 (cond ((null? list1) list2)
       ((null? list2) list1)
       ((predicate (key (first list1)) (key (first list2)))
	(cons (first list1) (merge (rest list1) list2 predicate key)))
       (else (cons (first list2) (merge list1 (rest list2) predicate key)))))

(define (sort list predicate key)
 (if (or (null? list) (null? (rest list)))
     list
     (merge (sort (every-other list) predicate key)
	    (sort (every-other (rest list)) predicate key)
	    predicate
	    key)))

(define (minp p l)
 (when (null? l) (fuck-up))
 (let loop ((x (first l)) (l (rest l)))
  (if (null? l) x (loop (if (p x (first l)) x (first l)) (rest l)))))

(define (unionvt x y) (if (or (eq? x #t) (eq? y #t)) #t (unionv x y)))

(define (intersectionvt x y)
 (cond ((eq? x #t) y) ((eq? y #t) x) (else (intersectionv x y))))

(define (set-differencevt x y)
 (cond ((eq? y #t) '()) ((eq? x #t) x) (else (set-differencev x y))))

(define (subsetvt? x y)
 (cond ((eq? y #t) #t)
       ((eq? x #t) #f)
       (else (every (lambda (xe) (memq xe y)) x))))

(define (lexicographically<? <? =?)
 ;; note: There is a bug in Scheme->C which doesn't allow < and = to shadow
 ;;       the global bindings which is why these are named <? and =?.
 (lambda (x y)
  (let loop ((x x) (y y))
   (and (not (null? x))
	(or (<? (first x) (first y))
	    (and (=? (first x) (first y)) (loop (rest x) (rest y))))))))

(define (minimal-membersp <? =? l)
 ;; note: There is a bug in Scheme->C which doesn't allow < and = to shadow
 ;;       the global bindings which is why these are named <? and =?.
 (when (null? l) (fuck-up))
 (let loop ((xs (list (first l))) (l (rest l)))
  (if (null? l)
      xs
      (loop (cond ((<? (first l) (first xs)) (list (first l)))
		  ((=? (first l) (first xs)) (cons (first l) xs))
		  (else	xs))
	    (rest l)))))

;;; Sleep

;;; removed: C-USLEEP

(define usleep (foreign-procedure (int) void "usleep" "unistd")) ;changed

;;; Random

;;; removed: C-RAND

(define rand (foreign-procedure () int "rand" "stdlib")) ;changed

(define *rand-max* ((primitive-procedure rand-max))) ;changed

(define (random-real) (/ (rand) (+ *rand-max* 1.0)))

(define (random-integer n) (inexact->exact (floor (* (random-real) n))))

(define (random-boolean) (>= (random-real) 0.5))

(define (random-member l) (list-ref l (random-integer (length l))))

(define (n-random-elements-without-replacement n x)
 (when (< (length x) n) (panic "Not enough elements"))
 (let loop ((x (map list x)) (l (length x)) (n n) (c '()))
  (if (zero? n)
      c
      (let ((e (list-ref x (random-integer l))))
       (loop (remq! e x) (- l 1) (- n 1) (cons (first e) c))))))

(define (deal x) (n-random-elements-without-replacement (length x) x))

(define (random-partition-of-size k x)
 (let ((y (deal (rest x))))
  (let loop ((u (let loop ((n (- k 1)) (x y))
		 (if (zero? n) x (loop (- n 1) (rest x)))))
	     (v (cons (list (first x))
		      (let loop ((n (- k 1)) (x y))
		       (if (zero? n)
			   '()
			   (cons (list (first x)) (loop (- n 1) (rest x))))))))
   (if (null? u)
       v
       (let* ((i (random-integer k))
	      (w (list-ref v i)))
	(loop (rest u) (cons (cons (first u) w) (removeq w v))))))))

;;; Gamma Function

(define (gamma n)
 ;; needs work: Doesn't work with n<1.
 (if (<= 1.0 n 2.0)
     ;; from CRC Standard Mathematical Tables 22nd edition
     ;; needs work: Should interpolate.
     (vector-ref '#(1.0 .99433 .98884 .98355 .97844
			.97350 .96874 .96415 .95973 .95546
			.95135 .94740 .94359 .93993 .93642
			.93304 .92980 .92670 .92373 .92089
			.91817 .91558 .91311 .91075 .90852
			.90640 .90440 .90250 .90072 .89904
			.89747 .89600 .89464 .89338 .89222
			.89115 .89018 .88931 .88854 .88785
			.88726 .88676 .88636 .88604 .88581
			.88566 .88560 .88563 .88575 .88595
			.88623 .88659 .88704 .88757 .88818
			.88887 .88964 .89049 .89142 .89243
			.89352 .89468 .89592 .89724 .89864
			.90012 .90167 .90330 .90500 .90678
			.90864 .91057 .91258 .91466 .91683
			.91906 .92137 .92376 .92623 .92877
			.93138 .93408 .93685 .93969 .94261
			.94561 .94869 .95184 .95507 .95838
			.96177 .96523 .96877 .97240 .97610
			.97988 .98374 .98768 .99171 .99581 1.0)
		 (inexact->exact (floor (* (- n 1.0) 100.0))))
     (* (- n 1) (gamma (- n 1.0)))))

(define (log-gamma n)
 ;; needs work: Doesn't work with n<1.
 (if (<= 1.0 n 2.0)
     ;; from CRC Standard Mathematical Tables 22nd edition
     ;; needs work: Should interpolate.
     ;; needs work: Should precompute the log of the table.
     (log (vector-ref '#(1.0 .99433 .98884 .98355 .97844
			     .97350 .96874 .96415 .95973 .95546
			     .95135 .94740 .94359 .93993 .93642
			     .93304 .92980 .92670 .92373 .92089
			     .91817 .91558 .91311 .91075 .90852
			     .90640 .90440 .90250 .90072 .89904
			     .89747 .89600 .89464 .89338 .89222
			     .89115 .89018 .88931 .88854 .88785
			     .88726 .88676 .88636 .88604 .88581
			     .88566 .88560 .88563 .88575 .88595
			     .88623 .88659 .88704 .88757 .88818
			     .88887 .88964 .89049 .89142 .89243
			     .89352 .89468 .89592 .89724 .89864
			     .90012 .90167 .90330 .90500 .90678
			     .90864 .91057 .91258 .91466 .91683
			     .91906 .92137 .92376 .92623 .92877
			     .93138 .93408 .93685 .93969 .94261
			     .94561 .94869 .95184 .95507 .95838
			     .96177 .96523 .96877 .97240 .97610
			     .97988 .98374 .98768 .99171 .99581 1.0)
		      (inexact->exact (floor (* (- n 1.0) 100.0)))))
     (+ (log (- n 1)) (log-gamma (- n 1.0)))))

;;; Numerical Integration

(define (integrate f a b n)
 ;; The constants are hardwired to be inexact for efficiency.
 (let ((delta (/ (- b a) n)))
  (let loop ((previous (f a)) (this (f (+ a delta))) (i 1) (s 0.0))
   (if (> i n)
       s
       (loop this
	     (f (+ a (* i delta)))
	     (+ i 1)
	     (+ s (* 0.5 (+ previous this) delta)))))))

;;; Schemer

(define *fail?* #t)

;;; removed: EITHER

;;; removed: TOP-LEVEL-FAIL

;;; changed
(define (fail)
 (when *fail?* (panic "Top-level fail"))
 (set! *fail?* #t))

;;; removed: SET-FAIL!

;;; removed: FOR-EFFECTS

;;; removed: ONE-VALUE

;;; removed: LOCAL-ONE-VALUE

;;; removed: ALL-VALUES

;;; removed: POSSIBLY?

;;; removed: NECESSARILY?

;;; removed: UPON-FAILURE

(define (unwind-trail) (set! *fail?* #f) (fail))

;;; removed: UNWEDGE-TRAIL

;;; removed: LOCAL-SET!

(define (local-set-car! x y)
 (let ((p (car x))) (upon-failure (set-car! x p)))
 (set-car! x y))

(define (local-set-cdr! x y)
 (let ((p (cdr x))) (upon-failure (set-cdr! x p)))
 (set-cdr! x y))

(define (local-string-set! s i x)
 (let ((p (string-ref s i))) (upon-failure (string-set! s i p)))
 (string-set! s i x))

(define (local-vector-set! v i x)
 (let ((p (vector-ref v i))) (upon-failure (vector-set! v i p)))
 (vector-set! v i x))

(define (a-boolean)
 ;; removed: comment
 (call-with-current-continuation
  (lambda (c)
   (let ((old-fail fail))
    ;; changed
    (set! fail (lambda () (set! fail old-fail) (if *fail?* (c #f) (fail)))))
   #t)))

(define (an-integer)
 (either 0 (let ((i (an-integer-above 1))) (either i (- i)))))

(define (an-integer-above i) (either i (an-integer-above (+ i 1))))

(define (an-integer-below i) (either i (an-integer-below (- i 1))))

(define (an-integer-between i j)
 (when (> i j) (fail))
 (either i (an-integer-between (+ i 1) j)))

(define (a-member-of s)
 (if (vector? s)
     (vector-ref s (an-integer-between 0 (- (vector-length s) 1)))
     (let loop ((l s))
      (when (null? l) (fail))
      (either (first l) (loop (rest l))))))

(define (a-subset-of l)
 (if (null? l)
     '()
     (let ((y (a-subset-of (rest l)))) (either (cons (first l) y) y))))

(define (a-split-of l)
 (let loop ((x '()) (y l))
  (if (null? y)
      (list x y)
      (either (list x y) (loop (append x (list (first y))) (rest y))))))

(define (a-permutation-of l)
 (if (null? l)
     l
     (let ((split (a-split-of (a-permutation-of (rest l)))))
      (append (first split) (cons (first l) (second split))))))

(define (a-partition-of x)
 (if (null? x)
     x
     (let ((y (a-partition-of (rest x))))
      (either (cons (list (first x)) y)
	      (let ((z (a-member-of y)))
	       (cons (cons (first x) z) (removeq z y)))))))

(define (a-partition-of-size k x)
 (when (< (length x) k) (fail))
 (let loop ((x x))
  (if (= (length x) k)
      (map list x)
      (let* ((y (loop (rest x)))
	     (z (a-member-of y)))
       (cons (cons (first x) z) (removeq z y))))))

(define-structure logic-variable binding name noticers)

(define *logic-variable-counter* -1)

(define (create-logic-variable)
 (set! *logic-variable-counter* (+ *logic-variable-counter* 1))
 (let ((v (make-logic-variable
	   #f
	   (string->uninterned-symbol
	    (format #f "?~s" *logic-variable-counter*))
	   '())))
  (set-logic-variable-binding! v v)
  v))

(define (attach-noticer! x noticer)
 (cond ((logic-variable? x)
	(cond
	 ((eq? (logic-variable-binding x) x)
	  (local-set-logic-variable-noticers!
	   x (cons noticer (logic-variable-noticers x)))
	  (noticer))
	 (else (attach-noticer! (logic-variable-binding x) noticer))))
       ((pair? x)
	(attach-noticer! (car x) noticer)
	(attach-noticer! (cdr x) noticer))
       ((vector? x)
	(for-each-n (lambda (i) (attach-noticer! (vector-ref x i) noticer))
		    (vector-length x)))))

(define (value-of x)
 (cond ((logic-variable? x)
	(if (eq? (logic-variable-binding x) x)
	    x
	    (value-of (logic-variable-binding x))))
       ((pair? x) (cons (value-of (car x)) (value-of (cdr x))))
       ((vector? x) (map-vector value-of x))
       (else x)))

(define (ground? x)
 (cond ((logic-variable? x)
	(and (not (eq? (logic-variable-binding x) x))
	     (ground? (logic-variable-binding x))))
       ((pair? x) (and (ground? (car x)) (ground? (cdr x))))
       ((vector? x)
	(every-n (lambda (i) (ground? (vector-ref x i))) (vector-length x)))
       (else #t)))

(define (known?-equalv x y)
 (or (eq? x y)
     (eqv? x y)
     (and (logic-variable? x)
	  (not (eq? (logic-variable-binding x) x))
	  (known?-equalv (logic-variable-binding x) y))
     (and (logic-variable? y)
	  (not (eq? (logic-variable-binding y) y))
	  (known?-equalv x (logic-variable-binding y)))
     (and (pair? x)
	  (pair? y)
	  (known?-equalv (car x) (car y))
	  (known?-equalv (cdr x) (cdr y)))
     (and (not (logic-variable? x))
	  (not (logic-variable? y))
	  (vector? x)
	  (vector? y)
	  (= (vector-length x) (vector-length y))
	  (every-n
	   (lambda (i) (known?-equalv (vector-ref x i) (vector-ref y i)))
	   (vector-length x)))))

(define (assert!-equalv x y)
 (cond
  ((logic-variable? x)
   (cond ((and (logic-variable? y) (not (eq? (logic-variable-binding y) y)))
	  (assert!-equalv x (logic-variable-binding y)))
	 ((eq? (logic-variable-binding x) x)
	  (let loop ((y y))
	   (when (eq? x y) (fail))
	   (cond
	    ((logic-variable? y)
	     (unless (eq? (logic-variable-binding y) y)
	      (loop (logic-variable-binding y))))
	    ((pair? y) (loop (car y)) (loop (cdr y)))
	    ((vector? y) (for-each-n (lambda (i) (loop (vector-ref y i)))
				     (vector-length y)))))
	  (local-set-logic-variable-binding! x y)
	  (for-each (lambda (noticer) (noticer) (attach-noticer! y noticer))
		    (logic-variable-noticers x)))
	 (else (assert!-equalv (logic-variable-binding x) y))))
  ((logic-variable? y) (assert!-equalv y x))
  ((and (pair? x) (pair? y))
   (assert!-equalv (car x) (car y))
   (assert!-equalv (cdr x) (cdr y)))
  ((and (vector? x) (vector? y) (= (vector-length x) (vector-length y)))
   (for-each-n (lambda (i) (assert!-equalv (vector-ref x i) (vector-ref y i)))
	       (vector-length x)))
  ((not (eqv? x y)) (fail))))

(define (assert!-notv-equalv x y)
 (when (known?-equalv x y) (fail))
 (attach-noticer! x (lambda () (when (known?-equalv x y) (fail))))
 (attach-noticer! y (lambda () (when (known?-equalv x y) (fail)))))

;;; Memoization

(define-structure entry arguments continuations results)

(define (memoize f)
 (let ((cache '()))
  (lambda arguments
   ;; removed: comment
   (call-with-current-continuation
    (lambda (continuation)
     (let ((entry (find-if (lambda (e) (equal? arguments (entry-arguments e)))
			   cache)))
      (cond
       (entry (set-entry-continuations!
	       entry (cons continuation (entry-continuations entry)))
	      (a-member-of (entry-results entry)))
       (else (set! entry (make-entry arguments (list continuation) '()))
	     (set! cache (cons entry cache))
	     (let ((result (apply f arguments)))
	      (set-entry-results! entry (cons result (entry-results entry)))
	      ((a-member-of (entry-continuations entry)) result))))))))))

;;; Strings

(define (prefix? prefix string)
 (and (<= (string-length prefix) (string-length string))
      (string=? prefix (substring string 0 (string-length prefix)))))

;;; removed: STRING-REVERSE

(define (suffix? suffix string)
 (prefix? (string-reverse suffix) (string-reverse string)))

(define (directory-prefix? prefix string)
 (or (string=? prefix string) (prefix? (string-append prefix "/") string)))

(define (string-downcase string)
 (list->string (map char-downcase (string->list string))))

(define (string-upcase string)
 (list->string (map char-upcase (string->list string))))

(define (symbol-downcase symbol)
 (string->symbol (string-downcase (symbol->string symbol))))

(define (pad-left string n)
 (string-append (make-string (- n (string-length string)) #\space) string))

(define (pad-right string n)
 (string-append string (make-string (- n (string-length string)) #\space)))

(define (substring? s1 s2)
 (let ((n (string-length s1)))
  (some-n (lambda (i)
	   (every-n (lambda (j)
		     (char=? (string-ref s1 j) (string-ref s2 (+ j i))))
		    n))
	  (+ (- (string-length s2) n) 1))))

(define (substring-ci? s1 s2)
 (let ((n (string-length s1)))
  (some-n (lambda (i)
	   (every-n (lambda (j)
		     (char-ci=? (string-ref s1 j) (string-ref s2 (+ j i))))
		    n))
	  (+ (- (string-length s2) n) 1))))

(define (slashify string)
 (let loop ((characters (string->list string)) (result '()))
  (cond
   ((null? characters) (list->string (reverse result)))
   ((char=? (first characters) #\\)
    (loop (rest characters) (cons #\\ (cons #\\ result))))
   ((char=? (first characters) #\")
    (loop (rest characters) (cons #\" (cons #\\ result))))
   ;; note: This is not really legitimate.
   ((or (char<? (first characters) #\space)
	(char>=? (first characters) (integer->char 127)))
    (loop (rest characters)
	  (cons (integer->char
		 (+ (bit-and (char->integer (first characters)) 7)
		    (char->integer #\0)))
		(cons (integer->char
		       (+ (bit-and
			   (quotient (char->integer (first characters)) 8) 7)
			  (char->integer #\0)))
		      (cons
		       (integer->char
			(+ (bit-and
			    (quotient (char->integer (first characters)) 64) 7)
			   (char->integer #\0)))
		       (cons #\\ result))))))
   (else (loop (rest characters) (cons (first characters) result))))))

(define (string-insert-character character)
 (lambda (string position)
  (list (string-append (substring string 0 position)
		       (list->string (list character))
		       (substring string position (string-length string)))
	(+ position 1))))

(define (string-beginning-of-line string position)
 (list string 0))

(define (string-backward-char string position)
 (when (zero? position) (abort))
 (list string (- position 1)))

(define (string-delete-char string position)
 (when (= position (string-length string)) (abort))
 (list (string-append
	(substring string 0 position)
	(substring string (+ position 1) (string-length string)))
       position))

(define (string-end-of-line string position)
 (list string (string-length string)))

(define (string-forward-char string position)
 (when (= position (string-length string)) (abort))
 (list string (+ position 1)))

(define (string-kill-line string position)
 (list (substring string 0 position) position))

(define (string-backward-delete-char string position)
 (when (zero? position) (abort))
 (list (string-append (substring string 0 (- position 1))
		      (substring string position (string-length string)))
       (- position 1)))

(define (char-alphanumeric? char)
 (or (char-alphabetic? char) (char-numeric? char)))

(define (beginning-of-word? string position)
 (or (zero? position)
     (and (not (= position (string-length string)))
	  (not (char-alphanumeric? (string-ref string (- position 1))))
	  (char-alphanumeric? (string-ref string position)))))

(define (end-of-word? string position)
 (or (= position (string-length string))
     (and (not (zero? position))
	  (char-alphanumeric? (string-ref string (- position 1)))
	  (not (char-alphanumeric? (string-ref string position))))))

(define (string-backward-word string position)
 (when (zero? position) (abort))
 (let loop ((position (- position 1)))
  (if (beginning-of-word? string position)
      (list string position)
      (loop (- position 1)))))

(define (string-kill-word string position)
 (when (= position (string-length string)) (abort))
 (list (string-append (substring string 0 position)
		      (substring string
				 (second (string-forward-word string position))
				 (string-length string)))
       position))

(define (string-forward-word string position)
 (when (= position (string-length string)) (abort))
 (let loop ((position (+ position 1)))
  (if (end-of-word? string position)
      (list string position)
      (loop (+ position 1)))))

(define (string-backward-kill-word string position)
 (when (zero? position) (abort))
 (let ((new-position (second (string-backward-word string position))))
  (list (string-append (substring string 0 new-position)
		       (substring string position (string-length string)))
	new-position)))

;;; Fields

(define (number-of-fields string)
 (let loop ((n 0) (chars (string->list string)))
  (if (null? chars)
      n
      (if (char-whitespace? (first chars))
	  (loop n (rest chars))
	  (loop (+ n 1)
		(let loop ((chars chars))
		 (if (or (null? chars) (char-whitespace? (first chars)))
		     chars
		     (loop (rest chars)))))))))

(define (field-ref string n)
 (let loop ((n n) (chars (string->list string)))
  (if (char-whitespace? (first chars))
      (loop n (rest chars))
      (if (zero? n)
	  (let loop ((chars chars) (field '()))
	   (if (or (null? chars) (char-whitespace? (first chars)))
	       (list->string (reverse field))
	       (loop (rest chars) (cons (first chars) field))))
	  (loop (- n 1)
		(let loop ((chars chars))
		 (if (char-whitespace? (first chars))
		     chars
		     (loop (rest chars)))))))))

(define (fields string)
 (map-n (lambda (i) (field-ref string i)) (number-of-fields string)))

;;; Context-Free Grammars

;;; removed: LAZY

(define (terminal x)
 (lambda (p) (if (not (and (pair? p) (eq? (first p) x))) (fail)) (rest p)))

(define (seq . &rest)
 (if (null? &rest) list (compose (first &rest) (apply seq (rest &rest)))))

(define (alt . &rest) (a-member-of &rest))

(define (opt a) (alt (seq) a))

(define (k* a) (opt (seq a (k* a))))

(define (recognize? s words) (possibly? (null? (s words))))

;;; Line and Whole-File I/O

(define (read-line . port)
 (if (null? port) (set! port (current-input-port)) (set! port (first port)))
 (let loop ((chars '()))
  (let ((char (read-char port)))
   (if (eof-object? char)
       (if (null? chars) char (list->string (reverse chars)))
       (if (char=? char #\newline)
	   (list->string (reverse chars))
	   (loop (cons char chars)))))))

(define (read-file pathname)
 (if (string=? pathname "-")
     (let loop ((lines '()) (line (read-line)))
      (if (eof-object? line)
	  (reverse lines)
	  (loop (cons line lines) (read-line))))
     (call-with-input-file pathname
      (lambda (port)
       (let loop ((lines '()) (line (read-line port)))
	(if (eof-object? line)
	    (reverse lines)
	    (loop (cons line lines) (read-line port))))))))

(define (write-file lines pathname)
 (if (string=? pathname "-")
     (for-each (lambda (line) (display line) (newline)) lines)
     (call-with-output-file pathname
      (lambda (port)
       (for-each (lambda (line) (display line port) (newline port)) lines)))))

(define (read-object-from-file pathname)
 (if (string=? pathname "-") (read) (call-with-input-file pathname read)))

(define (write-object-to-file object pathname)
 (cond ((string=? pathname "-") (pp object) (newline))
       (else (call-with-output-file pathname
	      (lambda (port) (pp object port) (newline port))))))

(define (read-from-string string)
 (rm (tmp "cdslib.tmp"))
 (write-file (list string) (tmp "cdslib.tmp"))
 (let ((input (call-with-input-file (tmp "cdslib.tmp") read)))
  (rm (tmp "cdslib.tmp"))
  input))

;;; Pathnames

;;; needs work: missing notions: ., .., foo~, foo.~n~, .foo, #foo#, /foo/, and
;;;             foo/

(define (has-directory? pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let loop ((l (reverse (string->list pathname))))
  (and (not (null? l)) (or (char=? (first l) #\/) (loop (rest l))))))

(define (directory pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (unless (has-directory? pathname) (panic "No directory"))
 (let ((l (string->list pathname)))
  (substring pathname 0 (- (length l) (positionv #\/ (reverse l)) 1))))

(define (strip-directory pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (if (has-directory? pathname)
     (let ((l (string->list pathname)))
      (substring pathname
		 (- (length l) (positionv #\/ (reverse l)))
		 (length l)))
     pathname))

(define (has-extension? pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let loop ((l (reverse (string->list pathname))))
  (and (not (null? l))
       (not (char=? (first l) #\/))
       (or (char=? (first l) #\.) (loop (rest l))))))

(define (extension pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (unless (has-extension? pathname) (panic "No extension"))
 (substring pathname
	    (+ (positionv #\. (string->list pathname)) 1)
	    (string-length pathname)))

(define (strip-extension pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let loop ((l (reverse (string->list pathname))))
  (if (or (null? l) (char=? (first l) #\/))
      pathname
      (if (char=? (first l) #\.)
	  (list->string (reverse (rest l)))
	  (loop (rest l))))))

(define (default-extension pathname extension)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (if (has-extension? pathname)
     pathname
     (string-append pathname "." extension)))

(define (replace-extension pathname extension)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (string-append (strip-extension pathname) "." extension))

;;; Temporary files

(define *tmp* "/tmp")

(define (tmp pathname) (string-append *tmp* "/" pathname))

;;; Directory/File operations

;;; removed: FOPEN

;;; removed: FCLOSE

;;; changed
(define (can-open-file-for-input? pathname)
 (or (string=? pathname "-")
     (let ((input-port
	    ((foreign-procedure (char* char*) input-port "fopen" "stdio")
	     pathname "r")))
      (unless (zero? input-port)
       ((foreign-procedure (file*) int "fclose" "stdio") input-port))
      (not (zero? input-port)))))

(define *system-V?* #t)

(define (quotify string)
 (let loop ((chars (string->list string)) (c '()))
  (if (null? chars)
      (list->string (reverse c))
      (loop (rest chars)
	    (cons (first chars)
		  (if (or (char=? (first chars) #\\)
			  (char=? (first chars) #\")
			  (char=? (first chars) #\$)
			  (char=? (first chars) #\&))
		      (cons #\\ c)
		      c))))))

(define (file-exists? pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (system (format #f "~als -ld ~a >~a 2>~a"
		 (if *system-V?* "" "/usr/5bin/")
		 (quotify pathname)
		 (tmp "QobiScheme.ls")
		 (tmp "QobiScheme.stderr")))
 (unless (or (eof-object?
	      (call-with-input-file (tmp "QobiScheme.stderr") read-line))
	     (string=?
	      (call-with-input-file (tmp "QobiScheme.stderr") read-line)
	      (format #f "ls: ~a: No such file or directory"
		      (quotify pathname))))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.ls")))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.stderr")))
  (fuck-up))
 (let ((result (not (eof-object?
		     (call-with-input-file (tmp "QobiScheme.ls") read-line)))))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.ls")))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.stderr")))
  result))

(define (directory-list pattern)
 (system
  (format #f "ls -A ~a >~a 2>~a"
	  (quotify pattern) (tmp "QobiScheme.ls") (tmp "QobiScheme.stderr")))
 (unless (or (eof-object?
	      (call-with-input-file (tmp "QobiScheme.stderr") read-line))
	     (string=?
	      (call-with-input-file (tmp "QobiScheme.stderr") read-line)
	      (format #f "ls: ~a: No such file or directory"
		      (quotify pattern))))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.ls")))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.stderr")))
  (fuck-up))
 (let ((result (read-file (tmp "QobiScheme.ls"))))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.ls")))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.stderr")))
  result))

(define (recursive-directory-list pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (unless (file-exists? pathname)
  (panic "Can't get recursive directory list for nonexistent file"))
 (system (format #f "find ~a -print >~a 2>~a"
		 (quotify pathname)
		 (tmp "QobiScheme.find")
		 (tmp "QobiScheme.stderr")))
 (unless (eof-object?
	  (call-with-input-file (tmp "QobiScheme.stderr") read-line))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.find")))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.stderr")))
  (fuck-up))
 (let ((result (read-file (tmp "QobiScheme.find"))))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.find")))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.stderr")))
  result))

(define (file-info pathname id?)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (unless (file-exists? pathname) (panic "Can't get info for nonexistent file"))
 (system (format #f "~als -~ad ~a >~a 2>~a"
		 (if *system-V?* "" "/usr/5bin/")
		 (if id? "n" "l")
		 (quotify pathname)
		 (tmp "QobiScheme.ls")
		 (tmp "QobiScheme.stderr")))
 (unless (eof-object?
	  (call-with-input-file (tmp "QobiScheme.stderr") read-line))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.ls")))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.stderr")))
  (fuck-up))
 (let ((result (call-with-input-file (tmp "QobiScheme.ls") read-line)))
  (when (eof-object? result) (fuck-up))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.ls")))
  (system (format #f "rm -f ~a" (tmp "QobiScheme.stderr")))
  result))

(define (file-permission-flags pathname) (field-ref (file-info pathname #f) 0))

(define (directory? pathname)
 (char=? (string-ref (file-permission-flags pathname) 0) #\d))

(define (symlink? pathname)
 (char=? (string-ref (file-permission-flags pathname) 0) #\l))

(define (file? pathname)
 (char=? (string-ref (file-permission-flags pathname) 0) #\-))

(define (file-number-of-links pathname)
 (string->number (field-ref (file-info pathname #f) 1)))

(define (file-userid pathname)
 (let ((result (field-ref (file-info pathname #f) 2)))
  (when (integer? (string->number result)) (panic "Can't get userid"))
  result))

(define (file-uid pathname)
 (let ((result (string->number (field-ref (file-info pathname #f) 2))))
  (unless (integer? result) (panic "Can't get uid"))
  result))

(define (file-groupid pathname)
 (let ((result (field-ref (file-info pathname #f) 3)))
  (when (integer? (string->number result)) (panic "Can't get groupid"))
  result))

(define (file-gid pathname)
 (let ((result (string->number (field-ref (file-info pathname #f) 3))))
  (unless (integer? result) (panic "Can't get gid"))
  result))

(define (file-length pathname)
 (string->number (field-ref (file-info pathname #f) 4)))

(define (file-mtime-month pathname)
 (+
  (position
   (field-ref (file-info pathname #f) 5)
   '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
  1))

(define (file-mtime-date pathname)
 (string->number (field-ref (file-info pathname #f) 6)))

(define (file-mtime-time/year pathname) (field-ref (file-info pathname #f) 7))

(define (symlink-target pathname)
 (unless (symlink? pathname) (panic "Not a link"))
 (field-ref (file-info pathname #f) 10))

(define (symlink target pathname)
 (when (or (string=? target "-") (string=? pathname "-"))
  (panic "Invalid pathname"))
 (unless (zero? (system (format #f "ln -s ~a ~a 2>/dev/null"
				(quotify target) (quotify pathname))))
  (panic "SYMLINK failed")))

(define (mkdir pathname)
 (unless (zero? (system (format #f "mkdir ~a 2>/dev/null" (quotify pathname))))
  (panic "MKDIR failed")))

(define (rm pathname)
 (unless (zero? (system (format #f "rm -rf ~a" (quotify pathname))))
  (panic "RM failed")))

(define (mkfifo pathname)
 (unless (zero? (system (format #f "mkfifo ~a" (quotify pathname))))
  (panic "MKFIFO failed")))

(define (create-directory-and-parents-if-necessary target)
 (let ((pathname (directory target)))
  (unless (zero? (string-length pathname))
   (cond ((file-exists? pathname)
	  (unless (directory? pathname)
	   ;; changed
	   (panic (format #f "~a exists but is not a directory" pathname))))
	 (else (create-directory-and-parents-if-necessary pathname)
	       (mkdir pathname))))))

(define (same-contents? pathname1 pathname2)
 (when (or (string=? pathname1 "-") (string=? pathname2 "-"))
  (panic "Invalid pathname"))
 (let ((result (/ (system (format #f "cmp -s ~a ~a"
				  (quotify pathname1)
				  (quotify pathname2)))
		  256)))
  (when (> result 1) (panic "SAME-CONTENTS? failed"))
  (zero? result)))

(define (compressed? pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (and (char=? (string-ref pathname (- (string-length pathname) 2)) #\.)
      (char=? (string-ref pathname (- (string-length pathname) 1)) #\Z)))

(define (compressed pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (string-append pathname ".Z"))

(define (compress pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (system (format #f "compress -f ~a" pathname)))

(define (uncompress pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (system (format #f "uncompress ~a" pathname)))

;;; removed: LD

;;; Tries

(define-structure trie n char->integer integer->char initial-value trie-node)

(define-structure trie-node table value)

(define (create-trie n char->integer integer->char . initial-value)
 (if (null? initial-value)
     (set! initial-value #f)
     (set! initial-value (first initial-value)))
 (make-trie n
	    char->integer
	    integer->char
	    initial-value
	    (make-trie-node (make-vector n #f) initial-value)))

(define (trie-ref trie string)
 (let ((m (string-length string)))
  (let loop ((trie-node (trie-trie-node trie)) (i 0))
   (if trie-node
       (if (= i m)
	   (trie-node-value trie-node)
	   (loop (vector-ref (trie-node-table trie-node)
			     ((trie-char->integer trie) (string-ref string i)))
		 (+ i 1)))
       (trie-initial-value trie)))))

(define (trie-set! trie string value)
 (let ((m (string-length string))
       (n (trie-n trie))
       (initial-value (trie-initial-value trie)))
  (let loop ((trie-node (trie-trie-node trie)) (i 0))
   (if (= i m)
       (set-trie-node-value! trie-node value)
       (let ((j ((trie-char->integer trie) (string-ref string i))))
	(unless (vector-ref (trie-node-table trie-node) j)
	 (vector-set! (trie-node-table trie-node)
		      j
		      (make-trie-node (make-vector n #f) initial-value)))
	(loop (vector-ref (trie-node-table trie-node) j) (+ i 1)))))))

(define (for-each-trie-entry p trie)
 (let loop ((trie-node (trie-trie-node trie)) (characters '()))
  (p (list->string (reverse characters)) (trie-node-value trie-node))
  (for-each-n
   (lambda (i)
    (let ((trie-node (vector-ref (trie-node-table trie-node) i)))
     (when trie-node
      (loop trie-node (cons ((trie-integer->char trie) i) characters)))))
   (vector-length (trie-node-table trie-node)))))

(define (trie->alist trie)
 (let ((alist '()))
  (for-each-trie-entry
   (lambda (key value) (set! alist (cons (cons key value) alist))) trie)
  alist))

(define (alist->trie alist n char->integer integer->char . initial-value)
 (let ((trie (if (null? initial-value)
		 (create-trie n char->integer integer->char)
		 (create-trie
		  n char->integer integer->char (first initial-value)))))
  (for-each (lambda (entry) (trie-set! trie (car entry) (cdr entry))) alist)
  trie))

;;; Vectors

(define-structure line-segment p q)

(define (p l) (line-segment-p l))

(define (q l) (line-segment-q l))

(define (x v) (vector-ref v 0))

(define (y v) (vector-ref v 1))

(define (z v) (vector-ref v 2))

(define (dot u v) (reduce-vector + (map-vector * u v) 0))

(define (cross-2d u v)			;return scalar z-component
 (- (* (x u) (y v)) (* (y u) (x v))))

(define (cross u v)
 (vector (- (* (y u) (z v)) (* (y v) (z u)))
	 (- (* (x v) (z u)) (* (x u) (z v)))
	 (- (* (x u) (y v)) (* (x v) (y u)))))

(define (v+ u v) (map-vector + u v))

(define (v- u v) (map-vector - u v))

(define (k*v k u) (map-vector (lambda (x) (* k x)) u))

(define (v= u v) (every-vector = u v))

(define (rotate-90 u) (vector (- (y u)) (x u)))

(define (rotate-180 u) (vector (- (x u)) (- (y u))))

(define (rotate-270 u) (vector (y u) (- (x u))))

(define (perpendicular? u v) (zero? (dot u v)))

(define (parallel? u v) (perpendicular? (rotate-90 u) v))

(define (magnitude-squared v) (dot v v))

(define (magnitude v) (sqrt (magnitude-squared v)))

(define (unit v) (k*v (/ (magnitude v)) v))

(define (distance-squared u v) (magnitude-squared (v- v u)))

(define (distance u v) (sqrt (distance-squared u v)))

(define (tangent l) (unit (v- (line-segment-q l) (line-segment-p l))))

(define (normal-2d l)
 (unit (vector (- (y (line-segment-p l)) (y (line-segment-q l)))
	       (- (x (line-segment-q l)) (x (line-segment-p l))))))

(define (line-segment-length l)
 (distance (line-segment-p l) (line-segment-q l)))

(define (collinear? l1 l2)
 (and (parallel? (v- (q l1) (p l1)) (v- (p l2) (p l1)))
      (parallel? (v- (q l1) (p l1)) (v- (q l2) (p l1)))
      (parallel? (v- (q l2) (p l2)) (v- (p l1) (p l2)))
      (parallel? (v- (q l2) (p l2)) (v- (q l1) (p l2)))))

(define (point-on-line-segment? r l)
 (and (parallel? (v- (q l) (p l)) (v- r (p l)))
      (<= (min (x (p l)) (x (q l))) (x r) (max (x (p l)) (x (q l))))
      (<= (min (y (p l)) (y (q l))) (y r) (max (y (p l)) (y (q l))))))

(define (intersection-point l1 l2)
 (let ((a (invert-matrix (vector (vector (- (y (p l1)) (y (q l1)))
					 (- (x (q l1)) (x (p l1))))
				 (vector (- (y (p l2)) (y (q l2)))
					 (- (x (q l2)) (x (p l2))))))))
  (and a (m*v a (vector (+ (* (- (y (p l1)) (y (q l1))) (x (p l1)))
			   (* (- (x (q l1)) (x (p l1))) (y (p l1))))
			(+ (* (- (y (p l2)) (y (q l2))) (x (p l2)))
			   (* (- (x (q l2)) (x (p l2))) (y (p l2)))))))))

(define (cross? l1 l2)
 (or (and (clockwise-angle?
	   (v- (p l2) (p l1)) (v- (q l1) (p l1)) (v- (q l2) (p l1)))
	  (clockwise-angle?
	   (v- (q l1) (p l2)) (v- (q l2) (p l2)) (v- (p l1) (p l2)))
	  (clockwise-angle?
	   (v- (q l2) (q l1)) (v- (p l1) (q l1)) (v- (p l2) (q l1)))
	  (clockwise-angle?
	   (v- (p l1) (q l2)) (v- (p l2) (q l2)) (v- (q l1) (q l2))))
     (and (clockwise-angle?
	   (v- (q l2) (p l1)) (v- (q l1) (p l1)) (v- (p l2) (p l1)))
	  (clockwise-angle?
	   (v- (p l1) (p l2)) (v- (q l2) (p l2)) (v- (q l1) (p l2)))
	  (clockwise-angle?
	   (v- (p l2) (q l1)) (v- (p l1) (q l1)) (v- (q l2) (q l1)))
	  (clockwise-angle?
	   (v- (q l1) (q l2)) (v- (p l2) (q l2)) (v- (p l1) (q l2))))))

(define (intersect? l1 l2)
 (or (point-on-line-segment? (p l1) l2)
     (point-on-line-segment? (q l1) l2)
     (cross? l1 l2)))

(define (read-line-segments-from-file pathname)
 (define (read-line-segments-from-file port)
  (let loop ((l '()))
   (let* ((x1 (read port))
	  (y1 (read port))
	  (x2 (read port))
	  (y2 (read port)))
    (if (eof-object? y2)
	(reverse l)
	(loop (cons (make-line-segment (vector x1 y1) (vector x2 y2)) l))))))
 (if (string=? pathname "-")
     (read-line-segments-from-file (current-input-port))
     (call-with-input-file (default-extension pathname "lines")
      read-line-segments-from-file)))

(define (write-line-segments-to-file line-segments pathname)
 (define (write-line-segments-to-file port)
  (for-each (lambda (l)
	     (write (x (line-segment-p l)) port)
	     (write-char #\space port)
	     (write (y (line-segment-p l)) port)
	     (write-char #\space port)
	     (write (x (line-segment-q l)) port)
	     (write-char #\space port)
	     (write (y (line-segment-q l)) port)
	     (newline port))
	    line-segments))
 (if (string=? pathname "-")
     (write-line-segments-to-file (current-output-port))
     (call-with-output-file (default-extension pathname "lines")
      write-line-segments-to-file)))

;;; Matrices

(define (make-matrix m n . &rest)
 (cond ((null? &rest) (map-n-vector (lambda (i) (make-vector n)) m))
       ((null? (rest &rest))
	(map-n-vector (lambda (i) (make-vector n (first &rest))) m))
       (else (panic "Too many arguments to MAKE-MATRIX"))))

(define (make-3-by-3-matrix a11 a12 a13 a21 a22 a23 a31 a32 a33)
 (vector (vector a11 a12 a13)
	 (vector a21 a22 a23)
	 (vector a31 a32 a33)))

(define (matrix-copy m)
 (map-vector (lambda (row) (map-vector identity row)) m))

(define (matrix-rows a) (vector-length a))

(define (matrix-columns a) (vector-length (vector-ref a 0)))

(define (matrix-ref a i j) (vector-ref (vector-ref a i) j))

(define (matrix-set! a i j x) (vector-set! (vector-ref a i) j x))

(define (matrix-row-ref a i) (vector-ref a i))

(define (matrix-column-ref a j) (map-vector (lambda (v) (vector-ref v j)) a))

(define (matrix-row-set! a i v) (vector-set! a i v))

(define (vector->row-matrix v) (vector v))

(define (vector->column-matrix v) (map-vector vector v))

(define (m+ a b) (map-vector v+ a b))

(define (m- a b) (map-vector v- a b))

(define (m*v a v) (map-vector (lambda (u) (dot u v)) a))

(define (transpose a)
 (map-n-vector (lambda (j) (matrix-column-ref a j)) (matrix-columns a)))

(define (outer-product f u v)
 (map-vector (lambda (ui) (map-vector (lambda (vj) (f ui vj)) v)) u))

(define (self-outer-product f v) (outer-product f v v))

(define (m* a b) (outer-product dot a (transpose b)))

(define (v*m v a) (m* (vector->row-matrix v) a))

(define (k*m k m)
 (map-vector (lambda (row) (map-vector (lambda (e) (* k e)) row)) m))

(define (determinant a)
 ;; The constants are hardwired to be inexact for efficiency.
 (unless (= (matrix-rows a) (matrix-columns a))
  (panic "Can only find determinant of a square matrix"))
 ;; removed: comment
 (call-with-current-continuation
  (lambda (return)
   (let* ((n (matrix-rows a))
	  (b (make-matrix n n))
	  (d 1.0))
    (for-each-n
     (lambda (i)
      (for-each-n (lambda (j) (matrix-set! b i j (matrix-ref a i j))) n))
     n)
    (for-each-n
     (lambda (i)
      ;; partial pivoting reduces rounding errors
      (let ((greatest (abs (matrix-ref b i i)))
	    (index i))
       (for-each-from-a-up-to-b
	(lambda (j)
	 (let ((x (abs (matrix-ref b j i))))
	  (when (> x greatest) (set! index j) (set! greatest x))))
	(+ i 1)
	n)
       (when (= greatest 0.0) (return 0.0))
       (unless (= index i)
	(let ((v (matrix-row-ref b i)))
	 (matrix-row-set! b i (matrix-row-ref b index))
	 (matrix-row-set! b index v)
	 (set! d (- d))))
       (let ((c (matrix-ref b i i)))
	(set! d (* d c))
	(for-each-from-a-up-to-b
	 (lambda (j) (matrix-set! b i j (/ (matrix-ref b i j) c)))
	 i
	 n)
	(for-each-from-a-up-to-b
	 (lambda (j)
	  (let ((e (matrix-ref b j i)))
	   (for-each-from-a-up-to-b
	    (lambda (k)
	     (matrix-set!
	      b j k (- (matrix-ref b j k) (* e (matrix-ref b i k)))))
	    (+ i 1)
	    n)))
	 (+ i 1)
	 n))))
     n)
    d))))

(define (invert-matrix a)
 ;; The constants are hardwired to be inexact for efficiency.
 (unless (= (matrix-rows a) (matrix-columns a))
  (panic "Can only invert a square matrix"))
 ;; removed: comment
 (call-with-current-continuation
  (lambda (abort)
   (let* ((n (matrix-rows a))
	  (c (make-matrix n n))
	  (b (make-matrix n n 0.0)))
    (for-each-n
     (lambda (i)
      (for-each-n (lambda (j) (matrix-set! c i j (matrix-ref a i j))) n))
     n)
    (for-each-n (lambda (i) (matrix-set! b i i 1.0)) n)
    (for-each-n
     (lambda (i)
      (when (zero? (matrix-ref c i i))
       (call-with-current-continuation
	(lambda (return)
	 (for-each-n (lambda (j)
		      (when (and (> j i) (not (zero? (matrix-ref c j i))))
		       (let ((e (vector-ref c i)))
			(vector-set! c i (vector-ref c j))
			(vector-set! c j e))
		       (let ((e (vector-ref b i)))
			(vector-set! b i (vector-ref b j))
			(vector-set! b j e))
		       (return #f)))
		     n)
	 (abort #f))))
      (let ((d (/ (matrix-ref c i i))))
       (for-each-n (lambda (j)
		    (matrix-set! c i j (* d (matrix-ref c i j)))
		    (matrix-set! b i j (* d (matrix-ref b i j))))
		   n)
       (for-each-n
	(lambda (k)
	 (let ((d (- (matrix-ref c k i))))
	  (unless (= k i)
	   (for-each-n
	    (lambda (j)
	     (matrix-set!
	      c k j (+ (matrix-ref c k j) (* d (matrix-ref c i j))))
	     (matrix-set!
	      b k j (+ (matrix-ref b k j) (* d (matrix-ref b i j)))))
	    n))))
	n)))
     n)
    b))))

(define *epsilon* 1e-6)

;;; changed: To use modified kilo/simplex.sc version.
(define (simplex a m1 m2 m3)
 (unless (and (>= m1 0)
	      (>= m2 0)
	      (>= m3 0)
	      (= (matrix-rows a) (+ m1 m2 m3 2)))
  (fuck-up))
 (let* ((m12 (+ m1 m2 1))
	(m (- (matrix-rows a) 2))
	(n (- (matrix-columns a) 1))
	(l1 (make-vector n))
	(l2 (make-vector m))
	(l3 (make-vector m2))
	(nl1 n)
	(iposv (make-vector m))
	(izrov (make-vector n))
	(ip 0)
	(kp 0)
	(bmax 0.0)
	(one? #f)
	(pass2? #t))
  (define (simp1 mm abs?)
   (set! kp (vector-ref l1 0))
   (set! bmax (matrix-ref a mm kp))
   (do ((k 1 (+ k 1))) ((>= k nl1))
    (when (positive?
	   (if abs?
	       (- (abs (matrix-ref a mm (vector-ref l1 k))) (abs bmax))
	       (- (matrix-ref a mm (vector-ref l1 k)) bmax)))
     (set! kp (vector-ref l1 k))
     (set! bmax (matrix-ref a mm (vector-ref l1 k))))))
  (define (simp2)
   (set! ip 0)
   (let ((q1 0.0)
	 (flag? #f))
    (do ((i 0 (+ i 1))) ((= i m))
     (if flag?
	 (when (< (matrix-ref a (vector-ref l2 i) kp) (- *epsilon*))
	  (let ((q (/ (- (matrix-ref a (vector-ref l2 i) 0))
		      (matrix-ref a (vector-ref l2 i) kp))))
	   (cond
	    ((< q q1) (set! ip (vector-ref l2 i)) (set! q1 q))
	    ((= q q1)
	     (let ((qp 0.0)
		   (q0 0.0))
	      (let loop ((k 1))
	       (when (<= k n)
		(set! qp
		      (/ (- (matrix-ref a ip k)) (matrix-ref a ip kp)))
		(set! q0 (/ (- (matrix-ref a (vector-ref l2 i) k))
			    (matrix-ref a (vector-ref l2 i) kp)))
		(when (= q0 qp) (loop (+ k 1)))))
	      (when (< q0 qp) (set! ip (vector-ref l2 i))))))))
	 (when (< (matrix-ref a (vector-ref l2 i) kp) (- *epsilon*))
	  (set! q1 (/ (- (matrix-ref a (vector-ref l2 i) 0))
		      (matrix-ref a (vector-ref l2 i) kp)))
	  (set! ip (vector-ref l2 i))
	  (set! flag? #t))))))
  (define (simp3 one?)
   (let ((piv (/ (matrix-ref a ip kp))))
    (do ((ii 0 (+ ii 1))) ((= ii (+ m (if one? 2 1))))
     (unless (= ii ip)
      (matrix-set! a ii kp (* piv (matrix-ref a ii kp)))
      (do ((kk 0 (+ kk 1))) ((= kk (+ n 1)))
       (unless (= kk kp)
	(matrix-set! a ii kk (- (matrix-ref a ii kk)
				(* (matrix-ref a ip kk)
				   (matrix-ref a ii kp))))))))
    (do ((kk 0 (+ kk 1))) ((= kk (+ n 1)))
     (unless (= kk kp)
      (matrix-set! a ip kk (* (- piv) (matrix-ref a ip kk)))))
    (matrix-set! a ip kp piv)))
  (do ((k 0 (+ k 1))) ((= k n))
   (vector-set! l1 k (+ k 1))
   (vector-set! izrov k k))
  (do ((i 0 (+ i 1))) ((= i m))
   (when (negative? (matrix-ref a (+ i 1) 0)) (fuck-up))
   (vector-set! l2 i (+ i 1))
   (vector-set! iposv i (+ n i)))
  (do ((i 0 (+ i 1))) ((= i m2)) (vector-set! l3 i #t))
  (when (positive? (+ m2 m3))
   (do ((k 0 (+ k 1))) ((= k (+ n 1)))
    (do ((i (+ m1 1) (+ i 1)) (sum 0.0 (+ sum (matrix-ref a i k))))
      ((> i m) (matrix-set! a (+ m 1) k (- sum)))))
   (let loop ()
    (simp1 (+ m 1) #f)
    (cond
     ((<= bmax *epsilon*)
      (cond
       ((< (matrix-ref a (+ m 1) 0) (- *epsilon*)) (set! pass2? #f))
       ((<= (matrix-ref a (+ m 1) 0) *epsilon*)
	(let loop ((ip1 m12))
	 (if (<= ip1 m)
	     (cond ((= (vector-ref iposv (- ip1 1)) (+ ip n -1))
		    (simp1 ip1 #t)
		    (cond ((positive? bmax) (set! ip ip1) (set! one? #t))
			  (else (loop (+ ip1 1)))))
		   (else (loop (+ ip1 1))))
	     (do ((i (+ m1 1) (+ i 1))) ((>= i m12))
	      (when (vector-ref l3 (- i m1 1))
	       (do ((k 0 (+ k 1))) ((= k (+ n 1)))
		(matrix-set! a i k (- (matrix-ref a i k)))))))))
       (else (simp2) (if (zero? ip) (set! pass2? #f) (set! one? #t)))))
     (else (simp2) (if (zero? ip) (set! pass2? #f) (set! one? #t))))
    (when one?
     (set! one? #f)
     (simp3 #t)
     (cond ((>= (vector-ref iposv (- ip 1)) (+ n m12 -1))
	    (let loop ((k 0))
	     (cond
	      ((and (< k nl1) (not (= kp (vector-ref l1 k))))
	       (loop (+ k 1)))
	      (else
	       (set! nl1 (- nl1 1))
	       (do ((is k (+ is 1))) ((>= is nl1))
		(vector-set! l1 is (vector-ref l1 (+ is 1))))
	       (matrix-set! a (+ m 1) kp (+ (matrix-ref a (+ m 1) kp) 1))
	       (do ((i 0 (+ i 1))) ((= i (+ m 2)))
		(matrix-set! a i kp (- (matrix-ref a i kp))))))))
	   ((and (>= (vector-ref iposv (- ip 1)) (+ n m1))
		 (vector-ref l3 (- (vector-ref iposv (- ip 1)) m1 n)))
	    (vector-set! l3 (- (vector-ref iposv (- ip 1)) m1 n) #f)
	    (matrix-set! a (+ m 1) kp (+ (matrix-ref a (+ m 1) kp) 1))
	    (do ((i 0 (+ i 1))) ((= i (+ m 2)))
	     (matrix-set! a i kp (- (matrix-ref a i kp))))))
     (let ((t (vector-ref izrov (- kp 1))))
      (vector-set! izrov (- kp 1) (vector-ref iposv (- ip 1)))
      (vector-set! iposv (- ip 1) t))
     (loop))))
  (and pass2?
       (let loop ()
	(simp1 0 #f)
	(cond
	 ((positive? bmax)
	  (simp2)
	  (cond
	   ((zero? ip) #t)
	   (else (simp3 #f)
		 (let ((t (vector-ref izrov (- kp 1))))
		  (vector-set! izrov (- kp 1) (vector-ref iposv (- ip 1)))
		  (vector-set! iposv (- ip 1) t))
		 (loop))))
	 (else (list iposv izrov)))))))

;;; The constants in the following are hardwired to be inexact for efficiency.

(define (quadratic1 a b c)
 (let ((d (- (* b b) (* 4.0 a c))))
  (when (and (negative? d) (< (- d) *epsilon*)) (set! d 0.0))
  (/ (+ (- b) (sqrt d)) (* 2.0 a))))

(define (quadratic2 a b c)
 (let ((d (- (* b b) (* 4.0 a c))))
  (when (and (negative? d) (< (- d) *epsilon*)) (set! d 0.0))
  (/ (- (- b) (sqrt d)) (* 2.0 a))))

(define (jacobi a)
 (unless (and (= (matrix-rows a) (matrix-columns a))
	      (every-n (lambda (i)
			(every-n (lambda (j)
				  (= (matrix-ref a i j) (matrix-ref a j i)))
				 (matrix-rows a)))
		       (matrix-rows a)))
  (panic "Can only compute eigenvalues/eigenvectors of a symmetric matrix"))
 (let* ((a (map-vector (lambda (row) (map-vector identity row)) a))
	(n (matrix-rows a))
	(d (make-vector n))
	(v (make-matrix n n 0.0))
	(b (make-vector n))
	(z (make-vector n 0.0)))
  (for-each-n (lambda (ip)
	       (matrix-set! v ip ip 1.0)
	       (vector-set! b ip (matrix-ref a ip ip))
	       (vector-set! d ip (matrix-ref a ip ip)))
	      n)
  (let loop ((i 0))
   ;; This was changed from 50 to 500 for center-surround.
   (when (> i 500) (panic "Too many iterations in JACOBI"))
   (let ((sm (sum (lambda (ip)
		   (sum (lambda (ir)
			 (let ((iq (+ ip ir 1)))
			  (abs (matrix-ref a ip iq))))
			(- n ip 1)))
		  (- n 1))))
    (unless (zero? sm)
     (let ((tresh (if (< i 3) (/ (* 0.2 sm) (* n n)) 0.0)))
      (for-each-n
       (lambda (ip)
	(for-each-n
	 (lambda (ir)
	  (let* ((iq (+ ip ir 1))
		 (g (* 100.0 (abs (matrix-ref a ip iq)))))
	   (cond
	    ((and (> i 3)
		  (= (+ (abs (vector-ref d ip)) g) (abs (vector-ref d ip)))
		  (= (+ (abs (vector-ref d iq)) g) (abs (vector-ref d iq))))
	     (matrix-set! a ip iq 0.0))
	    ((> (abs (matrix-ref a ip iq)) tresh)
	     (let* ((h (- (vector-ref d iq) (vector-ref d ip)))
		    (t (if (= (+ (abs h) g) (abs h))
			   (/ (matrix-ref a ip iq) h)
			   (let ((theta (/ (* 0.5 h) (matrix-ref a ip iq))))
			    (if (negative? theta)
				(- (/ (+ (abs theta)
					 (sqrt (+ (* theta theta) 1.0)))))
				(/ (+ (abs theta)
				      (sqrt (+ (* theta theta) 1.0))))))))
		    (c (/ (sqrt (+ (* t t) 1.0))))
		    (s (* t c))
		    (tau (/ s (+ c 1.0)))
		    (h (* t (matrix-ref a ip iq))))
	      (define (rotate a i j k l)
	       (let ((g (matrix-ref a i j))
		     (h (matrix-ref a k l)))
		(matrix-set! a i j (- g (* s (+ h (* g tau)))))
		(matrix-set! a k l (+ h (* s (- g (* h tau)))))))
	      (vector-set! z ip (- (vector-ref z ip) h))
	      (vector-set! z iq (+ (vector-ref z iq) h))
	      (vector-set! d ip (- (vector-ref d ip) h))
	      (vector-set! d iq (+ (vector-ref d iq) h))
	      (matrix-set! a ip iq 0.0)
	      (for-each-n (lambda (j)
			   (cond ((< j ip) (rotate a j ip j iq))
				 ((< ip j iq) (rotate a ip j j iq))
				 ((< iq j) (rotate a ip j iq j)))
			   (rotate v j ip j iq))
			  n))))))
	 (- n ip 1)))
       (- n 1)))
     (for-each-n (lambda (ip)
		  (vector-set! b ip (+ (vector-ref b ip) (vector-ref z ip)))
		  (vector-set! d ip (vector-ref b ip))
		  (vector-set! z ip 0.0))
		 n)
     (loop (+ i 1)))))
  (for-each-n
   (lambda (i)
    (let ((k i)
	  (p (vector-ref d i)))
     (for-each-n
      (lambda (l)
       (let* ((j (+ i l 1)))
	(when (>= (vector-ref d j) p) (set! k j) (set! p (vector-ref d j)))))
      (- n i 1))
     (unless (= k i)
      (vector-set! d k (vector-ref d i))
      (vector-set! d i p)
      (for-each-n (lambda (j)
		   (let ((p (matrix-ref v j i)))
		    (matrix-set! v j i (matrix-ref v j k))
		    (matrix-set! v j k p)))
		  n))))
   (- n 1))
  (list d (transpose v))))

(define (eigenvalues a) (first (jacobi a)))

(define (eigenvectors a) (second (jacobi a)))

(define (vector->diagonal-matrix v)
 (let ((m (make-matrix (vector-length v) (vector-length v) 0.0)))
  (for-each-n (lambda (i) (matrix-set! m i i (vector-ref v i)))
	      (vector-length v))
  m))

(define (identity-matrix n) (vector->diagonal-matrix (make-vector n 1.0)))

(define (clip-eigenvalues a v)
 (let* ((j (jacobi a))
	(e (second j)))
  (m* (transpose e)
      (m* (vector->diagonal-matrix (map-vector max v (first j))) e))))

;;; The following two routines are limited to 2-by-2 matricies.

(define (eigenvector-angle1 m)
 (if (and (< (abs (matrix-ref m 1 0)) *epsilon*)
	  (< (abs (matrix-ref m 0 1)) *epsilon*))
     (if (> (matrix-ref m 1 1) (matrix-ref m 0 0)) half-pi 0.0)
     (atan (matrix-ref m 1 0)
	   (- (vector-ref (eigenvalues m) 0) (matrix-ref m 1 1)))))

(define (eigenvector-angle2 m)
 (if (and (< (abs (matrix-ref m 1 0)) *epsilon*)
	  (< (abs (matrix-ref m 0 1)) *epsilon*))
     (if (<= (matrix-ref m 1 1) (matrix-ref m 0 0)) half-pi 0.0)
     (atan (matrix-ref m 1 0)
	   (- (vector-ref (eigenvalues m) 1) (matrix-ref m 1 1)))))

;;; Sparse Matrices

(define-structure sparse-matrix row column blank)

(define-structure sparse-matrix-row element i up down)

(define-structure sparse-matrix-column element j left right)

(define-structure sparse-matrix-element value i up down j left right)

(define (create-sparse-matrix blank)
 (make-sparse-matrix #f #f blank))

(define (sparse-matrix-ref sparse-matrix i j)
 ;; note: Could do different traversals.
 ;; note: Could terminate sooner relying upon ordering.
 ;; note: Could make equality predicate a parameter and have different values
 ;;       for rows and columns.
 (let loop ((sparse-matrix-row (sparse-matrix-row sparse-matrix)))
  (if sparse-matrix-row
      (if (= (sparse-matrix-row-i sparse-matrix-row) i)
	  (let loop ((sparse-matrix-element
		      (sparse-matrix-row-element sparse-matrix-row)))
	   (if sparse-matrix-element
	       (if (= (sparse-matrix-element-j sparse-matrix-element) j)
		   (sparse-matrix-element-value sparse-matrix-element)
		   (loop (sparse-matrix-element-right sparse-matrix-element)))
	       (sparse-matrix-blank sparse-matrix)))
	  (loop (sparse-matrix-row-down sparse-matrix-row)))
      (sparse-matrix-blank sparse-matrix))))

;;; Arrays
;;; Note: Limited error checking (although some errors will be revealed by
;;;       VECTOR addressing).

(define (make-array l . &rest)
 ;; (make-array '(m n ...) [v]) creates array with optional intial value
 (cond ((or (not (list? l)) (< (length l) 1))
	(panic "First argument to MAKE-ARRAY must be non-empty list"))
       ((> (length &rest) 1) (panic "Too many arguments to MAKE-ARRAY"))
       (else (let loop ((l l))
	      (if (null? (rest l))
		  (make-vector (first l) (if (null? &rest) '() (first &rest)))
		  (let ((v (make-vector (first l))))
		   (for-each-n (lambda (i) (vector-set! v i (loop (rest l))))
			       (first l))
		   v))))))

(define (array-ref a . &rest)
 ;; (array-ref a i j ...) returns a[i,j,...] from zero base
 (if (null? &rest)
     (panic "Too few arguments to ARRAY-REF")
     (let loop ((a a) (l &rest))
      (if (null? (rest l))
	  (vector-ref a (first l))
	  (loop (vector-ref a (first l)) (rest l))))))

(define (array-set! a v . &rest)
 ;; (array-set! a v i j ...) assigns a[i,j,...] = v.
 ;; Note: array indices are *at end* so they can have variable dimension
 (if (null? &rest)
     (panic "Too few arguments to ARRAY-SET!")
     (let loop ((a a) (l &rest))
      (if (null? (rest l))
	  (vector-set! a (first l) v)
	  (loop (vector-ref a (first l)) (rest l))))))

;;; 3D Geometry

(define-structure transform translation rotation)

(define pi (acos -1.0))

(define half-pi (/ pi 2.0))

(define two-pi (* 2.0 pi))

(define minus-pi (- pi))

(define two-pi/360 (/ two-pi 360.0))

(define three-sixty/two-pi (/ 360.0 two-pi))

(define (degrees->radians angle) (* two-pi/360 angle))

(define (radians->degrees angle) (* three-sixty/two-pi angle))

(define (normalize-rotation rotation)
 (cond ((> rotation pi) (normalize-rotation (- rotation two-pi)))
       ((<= rotation minus-pi) (normalize-rotation (+ rotation two-pi)))
       (else rotation)))

(define (rotation+ x y) (normalize-rotation (+ x y)))

(define (rotation- x y) (normalize-rotation (- x y)))

(define (angle-separation x y)
 (min (abs (rotation- x y)) (abs (rotation- y x))))

(define (rotation-matrix-2d theta)
 (let ((ct (cos theta))
       (st (sin theta)))
  (vector (vector ct (- st)) (vector st ct))))

(define (mean-angle angles)
 (atan (reduce + (map sin angles) 0) (reduce + (map cos angles) 0)))

(define (create-transform theta phi psi x y z)
 (let ((theta (degrees->radians theta))
       (phi (degrees->radians phi))
       (psi (degrees->radians psi)))
  (make-transform
   (vector x y z)
   (m* (m* (make-3-by-3-matrix 1.0 0.0 0.0
			       0.0 (cos theta) (sin theta)
			       0.0 (- (sin theta)) (cos theta))
	   (make-3-by-3-matrix (cos phi) 0.0 (sin phi)
			       0.0 1.0 0.0
			       (- (sin phi)) 0.0 (cos phi)))
       (make-3-by-3-matrix (cos psi) (sin psi) 0.0
			   (- (sin psi)) (cos psi) 0.0
			   0.0 0.0 1.0)))))

(define (compose-transforms t1 t2)
 (make-transform (v+ (m*v (transform-rotation t2) (transform-translation t1))
		     (transform-translation t2))
		 (m* (transform-rotation t2) (transform-rotation t1))))

(define (apply-transform t v)
 (v+ (transform-translation t) (m*v (transform-rotation t) v)))

(define (project v focal-length)
 (k*v (/ focal-length (z v)) (vector (x v) (y v))))

;;; Ellipses

(define-structure ellipse x0 y0 t0 a b)

(define (ellipse-center ellipse)
 (vector (ellipse-x0 ellipse) (ellipse-y0 ellipse)))

(define (ellipse-area ellipse)
 (* pi (ellipse-a ellipse) (ellipse-b ellipse)))

(define (ellipse-eccentricity ellipse)
 (/ (ellipse-a ellipse) (ellipse-b ellipse)))

(define (radial-distance theta phi) (normalize-rotation (- phi theta)))

(define (point-on-ellipse? p ellipse tolerance)
 (let* ((p0 (vector (ellipse-x0 ellipse) (ellipse-y0 ellipse)))
	(r (rotation-matrix-2d (- (ellipse-t0 ellipse))))
	(a (ellipse-a ellipse))
	(b (ellipse-b ellipse))
	(q (unit (m*v r (v- p p0)))))
  (<= (abs (- (distance p p0) (magnitude (vector (* a (x q)) (* b (y q))))))
      tolerance)))

(define (draw-ellipse display drawable gc ellipse)
 (let* ((previous-x #f)			;needs work: type pollution
	(previous-y #f)			;needs work: type pollution
	(x0 (ellipse-x0 ellipse))
	(y0 (ellipse-y0 ellipse))
	(t0 (ellipse-t0 ellipse))
	(a (ellipse-a ellipse))
	(b (ellipse-b ellipse))
	(rxx (cos t0))
	(rxy (- (sin t0)))
	(ryx (- rxy))
	(ryy rxx))
  (for-each-n
   (lambda (i)
    (let* ((ellipse-x (* a (sin (degrees->radians (* 10 i)))))
	   (ellipse-y (* b (cos (degrees->radians (* 10 i)))))
	   (this-x (+ (* rxx ellipse-x) (* rxy ellipse-y) x0))
	   (this-y (+ (* ryx ellipse-x) (* ryy ellipse-y) y0)))
     (when previous-x
      (xdrawline display drawable gc this-x this-y previous-x previous-y))
     (set! previous-x this-x)
     (set! previous-y this-y)))
   37)))

;;; Convex Hull

(define (same-angle? u v)
 ;; Returns #T if either U or V have zero magnitude.
 (or (and (zero? (x u)) (zero? (y u)))
     (and (zero? (x v)) (zero? (y v)))
     (and (eq? (negative? (x u)) (negative? (x v)))
	  (eq? (negative? (y u)) (negative? (y v)))
	  (parallel? u v))))

(define (clockwise-angle? u v w)
 (if (negative? (x u))
     (if (negative? (y u))
	 ;; U is in third quadrant
	 (clockwise-angle? (rotate-180 u) (rotate-180 v) (rotate-180 w))
	 ;; U is in second quadrant
	 (clockwise-angle? (rotate-270 u) (rotate-270 v) (rotate-270 w)))
     (if (negative? (y u))
	 ;; U is in fourth quadrant
	 (clockwise-angle? (rotate-90 u) (rotate-90 v) (rotate-90 w))
	 ;; U is in first quadrant
	 (if (negative? (x v))
	     (if (negative? (y v))
		 ;; V is in third quadrant
		 (if (negative? (x w))
		     (if (negative? (y w))
			 ;; W is in third quadrant
			 (clockwise-angle? v w u)
			 ;; W is in second quadrant
			 #t)
		     (if (negative? (y w))
			 ;; W is in fourth quadrant
			 #f
			 ;; W is in first quadrant
			 (clockwise-angle? w u v)))
		 ;; V is in second quadrant
		 (if (negative? (y w))
		     ;; W is in third or fourth quadrant
		     #f
		     (if (negative? (x w))
			 ;; W is in second quadrant
			 (clockwise-angle? v w u)
			 ;; W is in first quadrant
			 (clockwise-angle? w u v))))
	     (if (negative? (y v))
		 ;; V is in fourth quadrant
		 (if (negative? (x w))
		     ;; W is in second or third quadrant
		     #t
		     (if (negative? (y w))
			 ;; W is in fourth quadrant
			 (clockwise-angle? v w u)
			 ;; W is in first quadrant
			 (clockwise-angle? w u v)))
		 ;; V is in first quadrant
		 (if (negative? (x w))
		     ;; W is in second or third quadrant
		     (> (* (x v) (y u)) (* (x u) (y v)))
		     (if (negative? (y w))
			 ;; W is in fourth quadrant
			 (> (* (x v) (y u)) (* (x u) (y v)))
			 ;; W is in first quadrant
			 (or (and (> (* (x v) (y u)) (* (x u) (y v)))
				  (> (* (x w) (y v)) (* (x v) (y w))))
			     (and (> (* (x w) (y v)) (* (x v) (y w)))
				  (> (* (x u) (y w)) (* (x w) (y u))))
			     (and (> (* (x u) (y w)) (* (x w) (y u)))
				  (> (* (x v) (y u)) (* (x u) (y v))))))))))))

(define (clockwise-or-same-angle? u v w)
 (if (negative? (x u))
     (if (negative? (y u))
	 ;; U is in third quadrant
	 (clockwise-or-same-angle?
	  (rotate-180 u) (rotate-180 v) (rotate-180 w))
	 ;; U is in second quadrant
	 (clockwise-or-same-angle?
	  (rotate-270 u) (rotate-270 v) (rotate-270 w)))
     (if (negative? (y u))
	 ;; U is in fourth quadrant
	 (clockwise-or-same-angle? (rotate-90 u) (rotate-90 v) (rotate-90 w))
	 ;; U is in first quadrant
	 (if (negative? (x v))
	     (if (negative? (y v))
		 ;; V is in third quadrant
		 (if (negative? (x w))
		     (if (negative? (y w))
			 ;; W is in third quadrant
			 (clockwise-or-same-angle? v w u)
			 ;; W is in second quadrant
			 #t)
		     (if (negative? (y w))
			 ;; W is in fourth quadrant
			 #f
			 ;; W is in first quadrant
			 (clockwise-or-same-angle? w u v)))
		 ;; V is in second quadrant
		 (if (negative? (y w))
		     ;; W is in third or fourth quadrant
		     #f
		     (if (negative? (x w))
			 ;; W is in second quadrant
			 (clockwise-or-same-angle? v w u)
			 ;; W is in first quadrant
			 (clockwise-or-same-angle? w u v))))
	     (if (negative? (y v))
		 ;; V is in fourth quadrant
		 (if (negative? (x w))
		     ;; W is in second or third quadrant
		     #t
		     (if (negative? (y w))
			 ;; W is in fourth quadrant
			 (clockwise-or-same-angle? v w u)
			 ;; W is in first quadrant
			 (clockwise-or-same-angle? w u v)))
		 ;; V is in first quadrant
		 (if (negative? (x w))
		     ;; W is in second or third quadrant
		     (>= (* (x v) (y u)) (* (x u) (y v)))
		     (if (negative? (y w))
			 ;; W is in fourth quadrant
			 (>= (* (x v) (y u)) (* (x u) (y v)))
			 ;; W is in first quadrant
			 (or (and (>= (* (x v) (y u)) (* (x u) (y v)))
				  (>= (* (x w) (y v)) (* (x v) (y w))))
			     (and (>= (* (x w) (y v)) (* (x v) (y w)))
				  (>= (* (x u) (y w)) (* (x w) (y u))))
			     (and (>= (* (x u) (y w)) (* (x w) (y u)))
				  (>= (* (x v) (y u)) (* (x u) (y v))))))))))))

(define (convex-hull points)
 ;; This correctly handles collinear points, and coincident points as a special
 ;; case of collinear points. It always returns the minimal set of points that
 ;; constitute a convex hull. The return value constitutes a counterclockwise
 ;; traversal of the hull.
 (if (null? points)
     '()
     ;; START is the bottommost, rightmost point.
     (let ((start (minp (lambda (p q)
			 (or (< (y p) (y q))
			     (and (= (y p) (y q)) (> (x p) (x q)))))
			points)))
      (if (every (lambda (p) (v= p start)) points)
	  ;; If all points are coincident with START, then the hull consists
	  ;; of the single point START.
	  (list start)
	  ;; PREVIOUS is one unit to the right of START. Choose a point NEXT
	  ;; such that the ray from START to NEXT is minimally clockwise from
	  ;; the ray from START to PREVIOUS. There can be several such
	  ;; collinear points NEXT.
	  (let* ((next (minp
			(lambda (p q)
			 (or (same-angle? (v- q start) '#(1 0))
			     (and (not (same-angle? (v- p start) '#(1 0)))
				  (clockwise-or-same-angle?
				   '#(1 0) (v- p start) (v- q start)))))
			points))
		 ;; Choose the collinear point that is furthest from START.
		 (next (minp (lambda (p q)
			      (>= (distance p start) (distance q start)))
			     ;; Find all points that are collinear to NEXT
			     ;; along the ray from START to NEXT.
			     (remove-if-not
			      (lambda (p)
			       (same-angle? (v- p start) (v- next start)))
			      points))))
	   (let loop ((hull (list next start)))
	    (let* ((next
		    ;; Choose a point NEXT such that the ray from THIS to NEXT
		    ;; is minimally clockwise from the ray from THIS to
		    ;; PREVIOUS. There can be several such collinear points
		    ;; NEXT.
		    (minp (lambda (p q)
			   (or (same-angle?
				(v- q (first hull))
				(v- (second hull) (first hull)))
			       (and (not (same-angle?
					  (v- p (first hull))
					  (v- (second hull) (first hull))))
				    (clockwise-or-same-angle?
				     (v- (second hull) (first hull))
				     (v- p (first hull))
				     (v- q (first hull))))))
			  points))
		   ;; Choose the collinear point that is furthest from THIS.
		   (next (minp (lambda (p q)
				(>= (distance p (first hull))
				    (distance q (first hull))))
			       ;; Find all points that are collinear to NEXT
			       ;; along the ray from THIS to NEXT.
			       (remove-if-not
				(lambda (p)
				 (same-angle? (v- p (first hull))
					      (v- next (first hull))))
				points))))
	     (if (v= next start) hull (loop (cons next hull))))))))))

(define (concave-hull points delta)
 ;; This correctly handles collinear points, and coincident points as a special
 ;; case of collinear points. It always returns the minimal set of points that
 ;; constitute a `concave' hull. The return value constitutes a
 ;; counterclockwise traversal of the hull.
 ;; This assumes that POINTS is connected. If it is not, then the hull that is
 ;; returned may not surround all of the POINTS.
 (if (null? points)
     '()
     ;; START is the bottommost, rightmost point.
     (let ((start (minp (lambda (p q)
			 (or (< (y p) (y q))
			     (and (= (y p) (y q)) (> (x p) (x q)))))
			points)))
      (if (every (lambda (p) (or (> (distance p start) delta) (v= p start)))
		 points)
	  ;; If all points that are closer than DELTA to START are coincident
	  ;; with START, then the hull consists of the single point START.
	  (list start)
	  ;; PREVIOUS is one unit to the right of START. Choose a point NEXT
	  ;; such that the ray from START to NEXT is minimally clockwise from
	  ;; the ray from START to PREVIOUS. There can be several such
	  ;; collinear points NEXT.
	  (let* ((next
		  (minp (lambda (p q)
			 (or (same-angle? (v- q start) '#(1 0))
			     (and (not (same-angle? (v- p start) '#(1 0)))
				  (clockwise-or-same-angle?
				   '#(1 0) (v- p start) (v- q start)))))
			;; Remove the points that are further than DELTA away
			;; from THIS.
			(remove-if (lambda (p) (> (distance p start) delta))
				   points)))
		 ;; Choose the noncoincident close collinear point that is
		 ;; closest to START.
		 (next (minp (lambda (p q)
			      (<= (distance p start) (distance q start)))
			     ;; Find all points that are not coincident with
			     ;; START, closer than DELTA to START, and
			     ;; collinear to NEXT along the ray from START to
			     ;; NEXT.
			     (remove-if-not
			      (lambda (p)
			       (and (not (v= p start))
				    (<= (distance p start) delta)
				    (same-angle? (v- p start)
						 (v- next start))))
			      points))))
	   (let loop ((hull (list next start)))
	    (let* ((next
		    ;; Choose a point NEXT such that the ray from THIS to NEXT
		    ;; is minimally clockwise from the ray from THIS to
		    ;; PREVIOUS. There can be several such collinear points
		    ;; NEXT.
		    (minp (lambda (p q)
			   (or (same-angle? (v- q (first hull))
					    (v- (second hull) (first hull)))
			       (and (not (same-angle?
					  (v- p (first hull))
					  (v- (second hull) (first hull))))
				    (clockwise-or-same-angle?
				     (v- (second hull) (first hull))
				     (v- p (first hull))
				     (v- q (first hull))))))
			  ;; Remove the points that are further than DELTA away
			  ;; from THIS. Also remove those points P such that
			  ;; the line segment from THIS to P crosses some line
			  ;; segment in the current partial hull.
			  (remove-if
			   (lambda (p)
			    (or (> (distance p (first hull)) delta)
				(let ((this (first hull)))
				 (let loop ((hull (rest hull)))
				  (and
				   (not (null? (rest hull)))
				   (or (cross? (make-line-segment this p)
					       (make-line-segment
						(first hull) (second hull)))
				       (loop (rest hull))))))))
			   points)))
		   ;; Choose the noncoincident close collinear point that is
		   ;; closest to THIS.
		   (next (minp (lambda (p q)
				(<= (distance p (first hull))
				    (distance q (first hull))))
			       ;; Find all points that are not coincident with
			       ;; THIS, closer than DELTA to THIS, and
			       ;; collinear to NEXT along the ray from THIS to
			       ;; NEXT.
			       (remove-if-not
				(lambda (p)
				 (and (not (v= p (first hull)))
				      (<= (distance p (first hull)) delta)
				      (same-angle? (v- p (first hull))
						   (v- next (first hull)))))
				points))))
	     (if (v= next start)
		 (let loop ((hull hull))
		  (cond ((or (null? hull)
			     (null? (rest hull))
			     (null? (rest (rest hull))))
			 hull)
			((same-angle? (v- (second hull) (first hull))
				      (v- (third hull) (second hull)))
			 (loop (cons (first hull) (rest (rest hull)))))
			(else (cons (first hull) (loop (rest hull))))))
		 (loop (cons next hull))))))))))

(define (clockwise? p q r) (clockwise-angle? (v- q p) (v- r q) (v- p q)))

(define (crossing? points)
 (let ((line-segments (map make-line-segment
			   points
			   (append (rest points) (list (first points))))))
  (some (lambda (l1) (some (lambda (l2) (cross? l1 l2)) line-segments))
	line-segments)))

(define (triangulate points)
 ;; POINTS must be a counterclockwise traversal of the vertices of a polygon.
 ;; Returns a list of counterclockwise triangles.
 (cond
  ((null? points) '())
  ((null? (rest points))
   (list (list (first points) (first points) (first points))))
  ((v= (first points) (second points)) (triangulate (rest points)))
  ((null? (rest (rest points)))
   (list (list (second points) (first points) (first points))))
  ((same-angle? (v- (second points) (first points))
		(v- (third points) (second points)))
   (triangulate (cons (first points) (rest (rest points)))))
  ((same-angle? (v- (second points) (first points))
		(v- (second points) (third points)))
   (cons (list (third points) (second points) (first points))
	 (triangulate (cons (first points) (rest (rest points))))))
  ((and
    (clockwise? (third points) (second points) (first points))
    (not (some (lambda (p)
		(or (point-inside-triangle?
		     p (third points) (second points) (first points))
		    (and
		     (not (v= p (first points)))
		     (point-on-line-segment?
		      p
		      (make-line-segment (first points) (second points))))
		    (and
		     (not (v= p (third points)))
		     (point-on-line-segment?
		      p
		      (make-line-segment (second points) (third points))))))
	       (rest (rest (rest points))))))
   (cons (list (third points) (second points) (first points))
	 (triangulate (cons (first points) (rest (rest points))))))
  (else (triangulate (append (rest points) (list (first points)))))))

(define (perimeter-of-polygon points)
 (if (or (null? points) (null? (rest points)))
     0
     (let loop ((points (cons (last points) points)))
      (if (null? (rest points))
	  0
	  (+ (distance (first points) (second points))
	     (loop (rest points)))))))

(define (hero p q r)
 (let* ((a (distance p q))
	(b (distance q r))
	(c (distance r p))
	(s (/ (+ a b c) 2)))
  (sqrt (* s (- s a) (- s b) (- s c)))))

(define (area-of-polygon points)
 (reduce +
	 (map (lambda (triangle)
	       (hero (first triangle) (second triangle) (third triangle)))
	      (triangulate points))
	 0))

(define (point-inside-triangle? p u v w)
 (if (clockwise? u v w)
     (and (clockwise-angle? (v- v u) (v- p u) (v- w u))
	  (clockwise-angle? (v- w v) (v- p v) (v- u v))
	  (clockwise-angle? (v- u w) (v- p w) (v- v w)))
     (and (clockwise-angle? (v- w u) (v- p u) (v- v u))
	  (clockwise-angle? (v- u v) (v- p v) (v- w v))
	  (clockwise-angle? (v- v w) (v- p w) (v- u w)))))

(define (point-inside-or-on-triangle? p u v w)
 (if (clockwise? u v w)
     (and (clockwise-or-same-angle? (v- v u) (v- p u) (v- w u))
	  (clockwise-or-same-angle? (v- w v) (v- p v) (v- u v))
	  (clockwise-or-same-angle? (v- u w) (v- p w) (v- v w)))
     (and (clockwise-or-same-angle? (v- w u) (v- p u) (v- v u))
	  (clockwise-or-same-angle? (v- u v) (v- p v) (v- w v))
	  (clockwise-or-same-angle? (v- v w) (v- p w) (v- u w)))))

(define (outline-polygon points)
 (map make-line-segment points (append (rest points) (list (first points)))))

(define (fill-polygon points u v)
 ;; needs work: This is a kludge for now since POINT-INSIDE-OR-ON-TRIANGLE?
 ;;             returns #T for triangles with coincident vertices. And
 ;;             TRIANGULATE returns such triangles for concave hulls.
 (let ((triangles (remove-if (lambda (triangle)
			      (or (v= (first triangle) (second triangle))
				  (v= (second triangle) (third triangle))
				  (v= (third triangle) (first triangle))))
			     (triangulate points)))
       (points '()))
  (do ((y1 (y u) (+ y1 1))) ((>= y1 (y v)))
   (do ((x1 (x u) (+ x1 1))) ((>= x1 (x v)))
    (let ((point (vector x1 y1)))
     (when (some (lambda (triangle)
		  (point-inside-or-on-triangle?
		   point (first triangle) (second triangle) (third triangle)))
		 triangles)
      (set! points (cons point points))))))
  points))

;;; Log Space Addition

(define log-math-precision 35.0)

(define minus-infinity (/ -1.0 0.0))	;changed

(define infinity (/ 1.0 0.0))		;changed

(define nan (- infinity infinity))	;changed

(define (add-exp e1 e2)
 (let* ((e-max (max e1 e2))
	(e-min (min e1 e2))
	(factor (floor e-min)))
  (if (= e-max minus-infinity)
      minus-infinity
      (if (> (- e-max factor) log-math-precision)
	  e-max
	  (+ (log (+ (exp (- e-max factor)) (exp (- e-min factor))))
	     factor)))))

(define (log-sum f n)
 (if (positive? n)
     (let loop ((n (- n 2)) (c (f (- n 1))))
      (if (negative? n) c (loop (- n 1) (add-exp (f n) c))))
     minus-infinity))

;;; Images

(define *max-red* 255)
(define *max-green* 255)
(define *max-blue* 255)
(define *max-grey* 255)
(define *max-hue* 360)
(define *max-saturation* 100)
(define *max-value* 100)

(define (rgb->hsv rgb)
 ;; The constants are hardwired to be inexact for efficiency.
 (let* ((red (/ (vector-ref rgb 0) *max-red*))
	(green (/ (vector-ref rgb 1) *max-green*))
	(blue (/ (vector-ref rgb 2) *max-blue*))
	(value (max red green blue))
	(m (min red green blue))
	(saturation (if (zero? value) 0.0 (/ (- value m) value)))
	(hue (if (zero? saturation)
		 0.0
		 (/ (let ((rc (/ (- value red) (- value m)))
			  (gc (/ (- value green) (- value m)))
			  (bc (/ (- value blue) (- value m))))
		     (cond ((= red value) (- bc gc))
			   ((= green value) (+ 2.0 (- rc bc)))
			   (else (+ 4.0 (- gc rc)))))
		    6.0)))
	(hue (if (negative? hue) (+ hue 1.0) hue)))
  (vector (inexact->exact (floor (* *max-hue* hue)))
	  (inexact->exact (floor (* *max-saturation* saturation)))
	  (inexact->exact (floor (* *max-value* value))))))

(define (hsv->rgb hsv)
 ;; The constants are hardwired to be inexact for efficiency.
 (let ((hue (/ (vector-ref hsv 0) *max-hue*))
       (saturation (/ (vector-ref hsv 1) *max-saturation*))
       (value (* (max *max-red* *max-green* *max-blue*)
		 (/ (vector-ref hsv 2) *max-value*))))
  (if (zero? saturation)
      (vector (inexact->exact (floor value))
	      (inexact->exact (floor value))
	      (inexact->exact (floor value)))
      (let* ((hue (if (negative? hue) (+ hue 1.0) hue))
	     (hue (* 6.0 hue))
	     (fract (- hue (floor hue)))
	     (new1 (inexact->exact (floor (* value (- 1.0 saturation)))))
	     (new2 (inexact->exact
		    (floor (* value (- 1.0 (* saturation fract))))))
	     (new3 (inexact->exact
		    (floor (* value (- 1.0 (* saturation (- 1.0 fract)))))))
	     (value (inexact->exact (floor value))))
       (case (inexact->exact (floor hue))
	((0) (vector value new3 new1))
	((1) (vector new2 value new1))
	((2) (vector new1 value new3))
	((3) (vector new1 new2 value))
	((4) (vector new3 new1 value))
	((5) (vector value new1 new2))
	((6) (vector value new3 new1))
	(else (panic "Inappropriate hue angle")))))))

(define (rgb->cd rgb)
 (let* ((red (vector-ref rgb 0))	
	(green (vector-ref rgb 1))
	(blue (vector-ref rgb 2))
	(intensity (max (+ red green blue) 1)))
  (vector (inexact->exact (floor (* *max-red* (/ red intensity))))
	  (inexact->exact (floor (* *max-green* (/ green intensity)))))))

(define-structure pbm raw? bitmap)
(define-structure pgm raw? maxval grey)
(define-structure ppm raw? maxval red green blue)

(define (pnm-width pnm)
 (matrix-columns (cond ((pbm? pnm) (pbm-bitmap pnm))
		       ((pgm? pnm) (pgm-grey pnm))
		       ((ppm? pnm) (ppm-red pnm))
		       (else (panic "Argument not PNM")))))

(define (pnm-height pnm)
 (matrix-rows (cond ((pbm? pnm) (pbm-bitmap pnm))
		    ((pgm? pnm) (pgm-grey pnm))
		    ((ppm? pnm) (ppm-red pnm))
		    (else (panic "Argument not PNM")))))

(define (read-pnm pathname)
 (define (read-pnm port)
  (define (read-pbm raw?)
   (let* ((width (read port))
	  (height (read port))
	  (bitmap (make-matrix height width 0)))
    ;; removed: comment
    (call-with-current-continuation
     (lambda (return)
      (cond
       (raw? (panic "Cannot (yet) read a raw pbm image"))
       (else
	(for-each-n (lambda (y)
		     (for-each-n (lambda (x)
				  (let ((v (read port)))
				   (when (eof-object? v) (return #f))
				   ;; Yes, it really is the case (at least
				   ;; according to xv) that 0 means white and
				   ;; 1 means black for ascii pbm images.
				   (matrix-set! bitmap y x (zero? v))))
				 width))
		    height)))))
    (make-pbm raw? bitmap)))
  (define (read-pgm raw?)
   (let* ((width (read port))
	  (height (read port))
	  (maxval (read port))
	  (size (* width height))
	  (grey (make-matrix height width 0)))
    ;; removed: comment
    (call-with-current-continuation
     (lambda (return)
      (cond (raw? (read-char port)
		  (for-each-n
		   (lambda (y)
		    (for-each-n (lambda (x)
				 (let ((c (read-char port)))
				  (when (eof-object? c) (return #f))
				  (matrix-set! grey y x (char->integer c))))
				width))
		   height))
	    (else (for-each-n (lambda (y)
			       (for-each-n (lambda (x)
					    (let ((v (read port)))
					     (when (eof-object? v) (return #f))
					     (matrix-set! grey y x v)))
					   width))
			      height)))))
    (make-pgm raw? maxval grey)))
  (define (read-ppm raw?)
   (let* ((width (read port))
	  (height (read port))
	  (maxval (read port))
	  (size (* width height))
	  (red (make-matrix height width 0))
	  (green (make-matrix height width 0))
	  (blue (make-matrix height width 0)))
    ;; removed: comment
    (call-with-current-continuation
     (lambda (return)
      (cond (raw? (read-char port)
		  (for-each-n
		   (lambda (y)
		    (for-each-n
		     (lambda (x)
		      (let* ((c1 (read-char port))
			     (c2 (read-char port))
			     (c3 (read-char port)))
		       (when (eof-object? c1) (return #f))
		       (matrix-set! red y x (char->integer c1))
		       (matrix-set! green y x (char->integer c2))
		       (matrix-set! blue y x (char->integer c3))))
		     width))
		   height))
	    (else (for-each-n
		   (lambda (y)
		    (for-each-n (lambda (x)
				 (let* ((v1 (read port))
					(v2 (read port))
					(v3 (read port)))
				  (when (eof-object? v1) (return #f))
				  (matrix-set! red y x v1)
				  (matrix-set! green y x v2)
				  (matrix-set! blue y x v3)))
				width))
		   height)))))
    (make-ppm raw? maxval red green blue)))
  (let ((format (read port)))
   (case format
    ((p1) (read-pbm #f))
    ((p2) (read-pgm #f))
    ((p3) (read-ppm #f))
    ((p4) (read-pbm #t))
    ((p5) (read-pgm #t))
    ((p6) (read-ppm #t))
    (else (panic "Incorrect format for a pnm image")))))
 (if (string=? pathname "-")
     (read-pnm (current-input-port))
     (call-with-input-file pathname read-pnm)))

(define (write-pnm pnm pathname)
 (define (write-pnm port)
  (define (write-pbm pbm)
   (let ((width (pnm-width pbm))
	 (height (pnm-height pbm))
	 (bitmap (pbm-bitmap pbm)))
    (write (if (pbm-raw? pbm) 'p4 'p1) port)
    (newline port)
    (write width port)
    (write-char #\space port)
    (write height port)
    (newline port)
    (if (pbm-raw? pbm)
	(panic "Cannot (yet) write a raw pbm image")
	(for-each-n (lambda (y)
		     (for-each-n (lambda (x)
				  ;; Yes, it really is the case (at least
				  ;; according to xv) that 0 means white and
				  ;; 1 means black for ascii pbm images.
				  (write (if (matrix-ref bitmap y x) 0 1) port)
				  (newline port))
				 width))
		    height))))
  (define (write-pgm pgm)
   (let ((width (pnm-width pgm))
	 (height (pnm-height pgm))
	 (grey (pgm-grey pgm)))
    (when (pgm-raw? pgm)
     (for-each-n
      (lambda (y)
       (for-each-n (lambda (x)
		    (when (> (matrix-ref grey y x) 255)
		     (panic "Grey value too large for raw pgm file format")))
		   width))
      height))
    (write (if (pgm-raw? pgm) 'p5 'p2) port)
    (newline port)
    (write width port)
    (write-char #\space port)
    (write height port)
    (newline port)
    (write (pgm-maxval pgm) port)
    (newline port)
    (if (pgm-raw? pgm)
	(for-each-n
	 (lambda (y)
	  (for-each-n
	   (lambda (x) (write-char (integer->char (matrix-ref grey y x)) port))
	   width))
	 height)
	(for-each-n (lambda (y)
		     (for-each-n (lambda (x)
				  (write (matrix-ref grey y x) port)
				  (newline port))
				 width))
		    height))))
  (define (write-ppm ppm)
   (let ((width (pnm-width ppm))
	 (height (pnm-height ppm))
	 (red (ppm-red ppm))
	 (green (ppm-green ppm))
	 (blue (ppm-blue ppm)))
    (when (ppm-raw? ppm)
     (for-each-n
      (lambda (y)
       (for-each-n (lambda (x)
		    (when (or (> (matrix-ref red y x) 255)
			      (> (matrix-ref green y x) 255)
			      (> (matrix-ref blue y x) 255))
		     (panic "Color value too large for raw ppm file format")))
		   width))
      height))
    (write (if (ppm-raw? ppm) 'p6 'p3) port)
    (newline port)
    (write width port)
    (write-char #\space port)
    (write height port)
    (newline port)
    (write (ppm-maxval ppm) port)
    (newline port)
    (if (ppm-raw? ppm)
	(for-each-n
	 (lambda (y)
	  (for-each-n (lambda (x)
		       (write-char (integer->char (matrix-ref red y x)) port)
		       (write-char (integer->char (matrix-ref green y x)) port)
		       (write-char (integer->char (matrix-ref blue y x)) port))
		      width))
	 height)
	(for-each-n (lambda (y)
		     (for-each-n (lambda (x)
				  (write (matrix-ref red y x) port)
				  (newline port)
				  (write (matrix-ref green y x) port)
				  (newline port)
				  (write (matrix-ref blue y x) port)
				  (newline port))
				 width))
		    height))))
  (cond ((pbm? pnm) (write-pbm pnm))
	((pgm? pnm) (write-pgm pnm))
	((ppm? pnm) (write-ppm pnm))
	(else (panic "Non-PNM argument to WRITE-PNM"))))
 (if (string=? pathname "-")
     (write-pnm (current-output-port))
     (call-with-output-file (default-extension
			     pathname
			     (cond ((pbm? pnm) "pbm")
				   ((pgm? pnm) "pgm")
				   ((ppm? pnm) "ppm")
				   (else (fuck-up))))
      write-pnm)))

(define (pnm-movie-frame-pathname pathname i)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let ((i (+ i 1)))
  (replace-extension (string-append (strip-extension pathname)
				    "_"
				    (cond ((< i 10) "0000")
					  ((< i 100) "000")
					  ((< i 1000) "00")
					  ((< i 10000) "0")
					  (else ""))
				    (number->string i))
		     (extension pathname))))

(define (pnm-movie-length pathname)
 (let loop ((i 0))
  (if (can-open-file-for-input? (pnm-movie-frame-pathname pathname i))
      (loop (+ i 1))
      i)))

(define (read-pnm-movie pathname)
 (list->vector
  (map-n (lambda (i) (read-pnm (pnm-movie-frame-pathname pathname i)))
	 (pnm-movie-length pathname))))

(define (write-pnm-movie pnm-movie pathname)
 (for-each-indexed
  (lambda (pnm i) (write-pnm pnm (pnm-movie-frame-pathname pathname i)))
  (vector->list pnm-movie)))

(define (read-mpeg pathname)
 ;; needs work: To create unique temporary pathnames to avoid clashes between
 ;;             multiple processes.
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (system (format #f "rm -f ~a ~a"
		 (tmp "QobiScheme.mpg")
		 (tmp "QobiScheme_?????.ppm")))
 (system (format #f "cp ~a ~a"
		 (default-extension pathname "Mpeg1")
		 (tmp "QobiScheme.mpg")))
 (system
  (format #f "(cd ~a;mpeg_play -quality on -dither ppm -quiet QobiScheme.mpg)"
	  *tmp*))
 (let ((pnm-movie (read-pnm-movie (tmp "QobiScheme.ppm"))))
  (system (format #f "rm -f ~a ~a"
		  (tmp "QobiScheme.mpg")
		  (tmp "QobiScheme_?????.ppm")))
  pnm-movie))

(define (write-mpeg pnm-movie pathname)
 ;; needs work: To create unique temporary pathnames to avoid clashes between
 ;;             multiple processes.
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (system (format #f "rm -f ~a ~a"
		 (tmp "QobiScheme.param")
		 (tmp "QobiScheme_?????.pnm")))
 (call-with-output-file (tmp "QobiScheme.param")
  (lambda (port)
   (format port "PATTERN          IBBPBBPBBPBBPBBP~%")
   (format port "OUTPUT           ~a~%" (default-extension pathname "Mpeg1"))
   (format port "BASE_FILE_FORMAT PNM~%")
   (format port "INPUT_CONVERT    *~%")
   (format port "GOP_SIZE         16~%")
   (format port "SLICES_PER_FRAME 1~%")
   (format port "INPUT_DIR        ~a~%" *tmp*)
   (format port "INPUT~%")
   (format port "QobiScheme_*.pnm [00001-~a~s]~%"
	   (let ((n (vector-length pnm-movie)))
	    (cond ((< n 10) "0000")
		  ((< n 100) "000")
		  ((< n 1000) "00")
		  ((< n 10000) "0")
		  (else "")))
	   (vector-length pnm-movie))
   (format port "END_INPUT~%")
   (format port "PIXEL            HALF~%")
   (format port "RANGE            10~%")
   (format port "PSEARCH_ALG      LOGARITHMIC~%")
   (format port "BSEARCH_ALG      CROSS2~%")
   (format port "IQSCALE          8~%")
   (format port "PQSCALE          10~%")
   (format port "BQSCALE          25~%")
   (format port "REFERENCE_FRAME  ORIGINAL~%")
   (format port "BIT_RATE         1000000~%")
   (format port "BUFFER_SIZE      327680~%")
   (format port "FRAME_RATE       30~%")
   (format port "FORCE_ENCODE_LAST_FRAME~%")))
 (write-pnm-movie pnm-movie (tmp "QobiScheme.pnm"))
 (system (format #f "mpeg_encode ~a >/dev/null" (tmp "QobiScheme.param")))
 (system (format #f "rm -f ~a ~a"
		 (tmp "QobiScheme.param")
		 (tmp "QobiScheme_?????.pnm"))))

(define (clip-mpeg input-pathname output-pathname first-frame last-frame)
 ;; needs work: To create unique temporary pathnames to avoid clashes between
 ;;             multiple processes.
 (when (or (string=? input-pathname "-") (string=? output-pathname "-"))
  (panic "Invalid pathname"))
 (system (format #f "rm -f ~a ~a"
		 (tmp "QobiScheme.{mpg,param}")
		 (tmp "QobiScheme_?????.ppm")))
 (system (format #f "cp ~a ~a"
		 (default-extension input-pathname "Mpeg1")
		 (tmp "QobiScheme.mpg")))
 (system
  (format #f "(cd ~a;mpeg_play -quality on -dither ppm -quiet QobiScheme.mpg)"
	  *tmp*))
 (call-with-output-file (tmp "QobiScheme.param")
  (lambda (port)
   (format port "PATTERN          IBBPBBPBBPBBPBBP~%")
   (format port "OUTPUT           ~a~%"
	   (default-extension output-pathname "Mpeg1"))
   (format port "BASE_FILE_FORMAT PNM~%")
   (format port "INPUT_CONVERT    *~%")
   (format port "GOP_SIZE         16~%")
   (format port "SLICES_PER_FRAME 1~%")
   (format port "INPUT_DIR        ~a~%" *tmp*)
   (format port "INPUT~%")
   (format port "QobiScheme_*.ppm [~a~s-~a~s]~%"
	   (cond ((< (+ first-frame 1) 10) "0000")
		 ((< (+ first-frame 1) 100) "000")
		 ((< (+ first-frame 1) 1000) "00")
		 ((< (+ first-frame 1) 10000) "0")
		 (else ""))
	   (+ first-frame 1)
	   (cond ((< (+ last-frame 1) 10) "0000")
		 ((< (+ last-frame 1) 100) "000")
		 ((< (+ last-frame 1) 1000) "00")
		 ((< (+ last-frame 1) 10000) "0")
		 (else ""))
	   (+ last-frame 1))
   (format port "END_INPUT~%")
   (format port "PIXEL            HALF~%")
   (format port "RANGE            10~%")
   (format port "PSEARCH_ALG      LOGARITHMIC~%")
   (format port "BSEARCH_ALG      CROSS2~%")
   (format port "IQSCALE          8~%")
   (format port "PQSCALE          10~%")
   (format port "BQSCALE          25~%")
   (format port "REFERENCE_FRAME  ORIGINAL~%")
   (format port "BIT_RATE         1000000~%")
   (format port "BUFFER_SIZE      327680~%")
   (format port "FRAME_RATE       30~%")
   (format port "FORCE_ENCODE_LAST_FRAME~%")))
 (system (format #f "mpeg_encode ~a >/dev/null" (tmp "QobiScheme.param")))
 (system (format #f "rm -f ~a ~a"
		 (tmp "QobiScheme.{mpg,param}")
		 (tmp "QobiScheme_?????.ppm"))))

(define (ppm-hue ppm)
 (unless (ppm? ppm) (panic "Argument to PPM-HUE is not a PPM"))
 (make-pgm (ppm-raw? ppm)
	   *max-hue*
	   (map-vector
	    (lambda (red-row green-row blue-row)
	     (map-vector
	      (lambda (red green blue)
	       (vector-ref (rgb->hsv (vector red green blue)) 0))
	      red-row
	      green-row
	      blue-row))
	    (ppm-red ppm)
	    (ppm-green ppm)
	    (ppm-blue ppm))))

(define (ppm-saturation ppm)
 (unless (ppm? ppm) (panic "Argument to PPM-SATURATION is not a PPM"))
 (make-pgm (ppm-raw? ppm)
	   *max-saturation*
	   (map-vector
	    (lambda (red-row green-row blue-row)
	     (map-vector
	      (lambda (red green blue)
	       (vector-ref (rgb->hsv (vector red green blue)) 1))
	      red-row
	      green-row
	      blue-row))
	    (ppm-red ppm)
	    (ppm-green ppm)
	    (ppm-blue ppm))))

(define (ppm-value ppm)
 (unless (ppm? ppm) (panic "Argument to PPM-VALUE is not a PPM"))
 (make-pgm (ppm-raw? ppm)
	   *max-value*
	   (map-vector
	    (lambda (red-row green-row blue-row)
	     (map-vector
	      (lambda (red green blue)
	       (vector-ref (rgb->hsv (vector red green blue)) 2))
	      red-row
	      green-row
	      blue-row))
	    (ppm-red ppm)
	    (ppm-green ppm)
	    (ppm-blue ppm))))

(define (pbm-and pbm1 pbm2)
 (unless (and (pbm? pbm1)
	      (pbm? pbm2)
	      (eq? (pbm-raw? pbm1) (pbm-raw? pbm2))
	      (= (pnm-width pbm1) (pnm-width pbm2))
	      (= (pnm-height pbm1) (pnm-height pbm2)))
  (panic "Arguments to PBM-AND are not matching PBMs"))
 (make-pbm (pbm-raw? pbm1)
	   (map-vector
	    (lambda (row1 row2)
	     (map-vector (lambda (bit1 bit2) (and bit1 bit2)) row1 row2))
	    (pbm-bitmap pbm1)
	    (pbm-bitmap pbm2))))

(define (pbm-or pbm1 pbm2)
 (unless (and (pbm? pbm1)
	      (pbm? pbm2)
	      (eq? (pbm-raw? pbm1) (pbm-raw? pbm2))
	      (= (pnm-width pbm1) (pnm-width pbm2))
	      (= (pnm-height pbm1) (pnm-height pbm2)))
  (panic "Arguments to PBM-OR are not matching PBMs"))
 (make-pbm (pbm-raw? pbm1)
	   (map-vector
	    (lambda (row1 row2)
	     (map-vector (lambda (bit1 bit2) (or bit1 bit2)) row1 row2))
	    (pbm-bitmap pbm1)
	    (pbm-bitmap pbm2))))

(define (pbm-not pbm)
 (unless (pbm? pbm) (panic "Argument to PBM-NOT is not a PBM"))
 (make-pbm
  (pbm-raw? pbm)
  (map-vector (lambda (row) (map-vector not row)) (pbm-bitmap pbm))))

(define (pbm-xor pbm1 pbm2)
 (unless (and (pbm? pbm1)
	      (pbm? pbm2)
	      (eq? (pbm-raw? pbm1) (pbm-raw? pbm2))
	      (= (pnm-width pbm1) (pnm-width pbm2))
	      (= (pnm-height pbm1) (pnm-height pbm2)))
  (panic "Arguments to PBM-XOR are not matching PBMs"))
 (make-pbm (pbm-raw? pbm1)
	   (map-vector
	    (lambda (row1 row2)
	     (map-vector (lambda (bit1 bit2) (xor bit1 bit2)) row1 row2))
	    (pbm-bitmap pbm1)
	    (pbm-bitmap pbm2))))

(define (pgm-absolute-difference pgm1 pgm2)
 (unless (and (pgm? pgm1)
	      (pgm? pgm2)
	      (= (pgm-maxval pgm1) (pgm-maxval pgm2))
	      (eq? (pgm-raw? pgm1) (pgm-raw? pgm2))
	      (= (pnm-width pgm1) (pnm-width pgm2))
	      (= (pnm-height pgm1) (pnm-height pgm2)))
  (panic "Arguments to PGM-ABSOLUTE-DIFFERENCE are not matching PGMs"))
 (make-pgm (pgm-raw? pgm1)
	   (pgm-maxval pgm1)
	   (map-vector
	    (lambda (row1 row2)
	     (map-vector (lambda (e1 e2) (abs (- e1 e2))) row1 row2))
	    (pgm-grey pgm1)
	    (pgm-grey pgm2))))

(define (empty-pnm? pnm)
 (cond
  ((ppm? pnm)
   (and (every-vector (lambda (row) (every-vector zero? row)) (ppm-red pnm))
	(every-vector (lambda (row) (every-vector zero? row)) (ppm-green pnm))
	(every-vector (lambda (row) (every-vector zero? row)) (ppm-blue pnm))))
  ((pgm? pnm)
   (every-vector (lambda (row) (every-vector zero? row)) (pgm-grey pnm)))
  ((pbm? pnm)
   (not
    (some-vector (lambda (row) (some-vector identity row)) (pbm-bitmap pnm))))
  (else (panic "Argument to EMPTY-PNM? is not a PNM"))))

(define (pbm->pgm pbm)
 (unless (pbm? pbm) (panic "Argument to PBM->PGM is not a PBM"))
 (make-pgm
  (pbm-raw? pbm)
  *max-grey*
  (map-vector
   (lambda (row) (map-vector (lambda (bit) (if bit *max-grey* 0)) row))
   (pbm-bitmap pbm))))

(define (pgm->ppm pgm)
 (unless (pgm? pgm) (panic "Argument to PGM->PPM is not a PGM"))
 (make-ppm (pgm-raw? pgm)
	   (pgm-maxval pgm)
	   (pgm-grey pgm)
	   (pgm-grey pgm)
	   (pgm-grey pgm)))

(define (pbm->ppm pbm) (pgm->ppm (pbm->pgm pbm)))

(define (ppm->pgm ppm)
 (unless (ppm? ppm) (panic "Argument to PPM->PGM is not a PPM"))
 (make-pgm
  (ppm-raw? ppm)
  (ppm-maxval ppm)
  (map-vector
   (lambda (red-row green-row blue-row)
    (map-vector (lambda (red green blue)
		 (inexact->exact
		  (floor (+ (* 0.299 red) (* 0.587 green) (* 0.114 blue)))))
		red-row green-row blue-row))
   (ppm-red ppm) (ppm-green ppm) (ppm-blue ppm))))

(define (pgm->pbm pgm threshold)
 (unless (pgm? pgm) (panic "Argument to PGM->PBM is not a PGM"))
 (make-pbm (pgm-raw? pgm)
	   (map-vector
	    (lambda (row) (map-vector (lambda (grey) (>= grey threshold)) row))
	    (pgm-grey pgm))))

(define (ppm->pbm ppm threshold) (pgm->pbm (ppm->pgm ppm) threshold))

(define (pbm-constant width height bit)
 (make-pbm #t (make-matrix height width bit)))

(define (pgm-constant width height grey)
 (make-pgm #t *max-grey* (make-matrix height width grey)))

(define (ppm-constant width height red green blue)
 (make-ppm #t
	   (max *max-red* *max-green* *max-blue*)
	   (make-matrix height width red)
	   (make-matrix height width green)
	   (make-matrix height width blue)))

(define (pbm-left-vertical-stripe width height left)
 ;; Creates a black (#F) stripe on a white (#T) background.
 (let ((m (make-matrix height width #t)))
  (do ((y 0 (+ y 1))) ((= y height))
   (do ((x 0 (+ x 1))) ((= x left))
    (matrix-set! m y x #f)))
  (make-pbm #t m)))

(define (overlay-pbm-on-pnm pbm pnm)
 ;; The white (#T) pixels in the pbm become white in the result.
 (unless (and (pbm? pbm)
	      (or (pbm? pnm) (pgm? pnm) (ppm? pnm))
	      (eq? (pbm-raw? pbm) (pbm-raw? pnm))
	      (= (pnm-width pbm) (pnm-width pnm))
	      (= (pnm-height pbm) (pnm-height pnm)))
  (panic "Arguments to OVERLAY-PBM-ON-PNM are not a matching PBM and PNM"))
 (cond
  ((ppm? pnm)
   (make-ppm (pbm-raw? pbm)
	     (max *max-red* *max-green* *max-blue*)
	     (map-vector (lambda (bitmap-row red-row)
			  (map-vector (lambda (bit red)
				       (if bit *max-red* red))
				      bitmap-row
				      red-row))
			 (pbm-bitmap pbm)
			 (ppm-red pnm))
	     (map-vector (lambda (bitmap-row green-row)
			  (map-vector (lambda (bit green)
				       (if bit *max-green* green))
				      bitmap-row
				      green-row))
			 (pbm-bitmap pbm)
			 (ppm-green pnm))
	     (map-vector (lambda (bitmap-row blue-row)
			  (map-vector (lambda (bit blue)
				       (if bit *max-blue* blue))
				      bitmap-row
				      blue-row))
			 (pbm-bitmap pbm)
			 (ppm-blue pnm))))
  ((pgm? pnm)
   (make-pgm (pbm-raw? pbm)
	     *max-grey*
	     (map-vector (lambda (bitmap-row grey-row)
			  (map-vector (lambda (bit grey)
				       (if bit *max-grey* grey))
				      bitmap-row
				      grey-row))
			 (pbm-bitmap pbm)
			 (pgm-grey pnm))))
  ((pbm? pnm) (pbm-or pbm pnm))
  (else (fuck-up))))

(define (pgm-smooth pgm sigma)
 (unless (pgm? pgm) (panic "Argument to PGM-SMOOTH is not a PGM"))
 (let* ((height (pnm-height pgm))
	(width (pnm-width pgm))
	(grey1 (pgm-grey pgm))
	(grey2 (make-matrix height width 0)))
  (do ((y sigma (+ y 1))) ((= y (- height sigma)))
   (do ((x sigma (+ x 1))) ((= x (- width sigma)))
    (do ((i (- y sigma) (+ i 1))) ((= i (+ y sigma 1)))
     (do ((j (- x sigma) (+ j 1))) ((= j (+ x sigma 1)))
      (matrix-set!
       grey2 y x (+ (matrix-ref grey2 y x) (matrix-ref grey1 i j)))))
    (matrix-set!
     grey2 y x
     (inexact->exact
      (floor (/ (matrix-ref grey2 y x) (sqr (+ sigma sigma 1))))))))
  (make-pgm (pgm-raw? pgm) *max-grey* grey2)))

(define (normal-flow-magnitude pgm1 pgm2 epsilon sigma sensitivity)
 (unless (and (pgm? pgm1)
	      (pgm? pgm2)
	      (= (pgm-maxval pgm1) (pgm-maxval pgm2))
	      (eq? (pgm-raw? pgm1) (pgm-raw? pgm2))
	      (= (pnm-width pgm1) (pnm-width pgm2))
	      (= (pnm-height pgm1) (pnm-height pgm2)))
  (panic "Arguments to NORMAL-FLOW-MAGNITUDE are not matching PGMs"))
 (let* ((width (pnm-width pgm1))
	(height (pnm-height pgm1))
	(e1 (pgm-grey (pgm-smooth pgm1 sigma)))
	(e2 (pgm-grey (pgm-smooth pgm2 sigma)))
	(m (make-matrix height width 0)))
  (do ((i 0 (+ i 1))) ((= i (- height 1)))
   (do ((j 0 (+ j 1))) ((= j (- width 1)))
    (let* ((ex (/ (- (+ (matrix-ref e1 (+ i 1) j)
			(matrix-ref e1 (+ i 1) (+ j 1))
			(matrix-ref e2 (+ i 1) j)
			(matrix-ref e2 (+ i 1) (+ j 1)))
		     (+ (matrix-ref e1 i j)
			(matrix-ref e1 i (+ j 1))
			(matrix-ref e2 i j)
			(matrix-ref e2 i (+ j 1))))
		  4.0))
	   (ey (/ (- (+ (matrix-ref e1 i (+ j 1))
			(matrix-ref e1 (+ i 1) (+ j 1))
			(matrix-ref e2 i (+ j 1))
			(matrix-ref e2 (+ i 1) (+ j 1)))
		     (+ (matrix-ref e1 i j)
			(matrix-ref e1 (+ i 1) j)
			(matrix-ref e2 i j)
			(matrix-ref e2 (+ i 1) j)))
		  4.0))
	   (et (/ (- (+ (matrix-ref e2 i j)
			(matrix-ref e2 i (+ j 1))
			(matrix-ref e2 (+ i 1) j)
			(matrix-ref e2 (+ i 1) (+ j 1)))
		     (+ (matrix-ref e1 i j)
			(matrix-ref e1 i (+ j 1))
			(matrix-ref e1 (+ i 1) j)
			(matrix-ref e1 (+ i 1) (+ j 1))))
		  4.0))
	   (l (sqrt (+ (sqr ex) (sqr ey)))))
     (matrix-set!
      m i j
      (min *max-grey*
	   (inexact->exact
	    (floor
	     (* *max-grey*
		(/ (if (< l epsilon) 0.0 (/ (abs et) l)) sensitivity)))))))))
  (pgm-smooth (make-pgm (pgm-raw? pgm1) *max-grey* m) sigma)))

(define (threshold-normal-flow-magnitude pgm1 pgm2 epsilon sigma threshold)
 ;; The moving pixels become white (#T) in the result.
 (unless (and (pgm? pgm1)
	      (pgm? pgm2)
	      (= (pgm-maxval pgm1) (pgm-maxval pgm2))
	      (eq? (pgm-raw? pgm1) (pgm-raw? pgm2))
	      (= (pnm-width pgm1) (pnm-width pgm2))
	      (= (pnm-height pgm1) (pnm-height pgm2)))
  (panic "Arguments to THRESHOLD-NORMAL-FLOW-MAGNITUDE are not matching PGMs"))
 (let* ((width (pnm-width pgm1))
	(height (pnm-height pgm1))
	(e1 (pgm-grey (pgm-smooth pgm1 sigma)))
	(e2 (pgm-grey (pgm-smooth pgm2 sigma)))
	(m (make-matrix height width #f)))
  (do ((i 0 (+ i 1))) ((= i (- height 1)))
   (do ((j 0 (+ j 1))) ((= j (- width 1)))
    (let* ((ex (/ (- (+ (matrix-ref e1 (+ i 1) j)
			(matrix-ref e1 (+ i 1) (+ j 1))
			(matrix-ref e2 (+ i 1) j)
			(matrix-ref e2 (+ i 1) (+ j 1)))
		     (+ (matrix-ref e1 i j)
			(matrix-ref e1 i (+ j 1))
			(matrix-ref e2 i j)
			(matrix-ref e2 i (+ j 1))))
		  4.0))
	   (ey (/ (- (+ (matrix-ref e1 i (+ j 1))
			(matrix-ref e1 (+ i 1) (+ j 1))
			(matrix-ref e2 i (+ j 1))
			(matrix-ref e2 (+ i 1) (+ j 1)))
		     (+ (matrix-ref e1 i j)
			(matrix-ref e1 (+ i 1) j)
			(matrix-ref e2 i j)
			(matrix-ref e2 (+ i 1) j)))
		  4.0))
	   (et (/ (- (+ (matrix-ref e2 i j)
			(matrix-ref e2 i (+ j 1))
			(matrix-ref e2 (+ i 1) j)
			(matrix-ref e2 (+ i 1) (+ j 1)))
		     (+ (matrix-ref e1 i j)
			(matrix-ref e1 i (+ j 1))
			(matrix-ref e1 (+ i 1) j)
			(matrix-ref e1 (+ i 1) (+ j 1))))
		  4.0))
	   (l (sqrt (+ (sqr ex) (sqr ey)))))
     (matrix-set!
      m i j (and (>= l epsilon) (>= (/ (abs et) l) threshold))))))
  (make-pbm (pgm-raw? pgm1) m)))

(define (pbm-proximity-clusterer pbm threshold)
 ;; Clusters white (#T) pixels.
 (unless (pbm? pbm) (fuck-up))
 (let* ((width (pnm-width pbm))
	(height (pnm-height pbm))
	(m (map-vector (lambda (row) (map-vector identity row))
		       (pbm-bitmap pbm)))
	(i -1)
	(threshold-squared (sqr threshold)))
  (do ((y 0 (+ y 1))) ((= y height))
   (do ((x 0 (+ x 1))) ((= x width))
    (when (eq? (matrix-ref m y x) #t)
     (matrix-set! m y x i)
     (let loop ()
      (let ((again? #f))
       (do ((y1 0 (+ y1 1))) ((= y1 height))
	(do ((x1 0 (+ x1 1))) ((= x1 width))
	 (when (eqv? (matrix-ref m y1 x1) i)
	  (do ((y2 (max 0 (- y1 threshold)) (+ y2 1)))
	    ((= y2 (min height (+ y1 threshold 1))))
	   (do ((x2 (max 0 (- x1 threshold)) (+ x2 1)))
	     ((= x2 (min width (+ x1 threshold 1))))
	    (when (and (<= (+ (sqr (- x1 x2)) (sqr (- y1 y2)))
			   threshold-squared)
		       (eq? (matrix-ref m y2 x2) #t))
	     (matrix-set! m y2 x2 i)
	     (set! again? #t)))))))
       (when again? (loop))))
     (set! i (- i 1)))))
  (let ((clusters (make-vector (- (- i) 1) '())))
   (do ((y 0 (+ y 1))) ((= y height))
    (do ((x 0 (+ x 1))) ((= x width))
     (let ((i (matrix-ref m y x)))
      (when i
       (vector-set! clusters
		    (- (- i) 1)
		    (cons (vector x y) (vector-ref clusters (- (- i) 1))))))))
   (vector->list clusters))))

(define (pbm-bloat pbm n)
 ;; Bloats white (#T) pixels.
 (let* ((height (pnm-height pbm))
	(width (pnm-width pbm))
	(bitmap (pbm-bitmap pbm))
	(new (make-matrix height width #f)))
  (do ((y 0 (+ y 1))) ((>= y height))
   (do ((x 0 (+ x 1))) ((>= x width))
    (do ((y0 (- y n) (+ y0 1))) ((> y0 (+ y n)))
     (when (and (>= y0 0) (< y0 height))
      (do ((x0 (- x n) (+ x0 1))) ((> x0 (+ x n)))
       (when (and (>= x0 0) (< x0 width) (matrix-ref bitmap y0 x0))
	(matrix-set! new y x #t)))))))
  (make-pbm (pbm-raw? pbm) new)))

(define (pnm-shift pnm delta)
 (cond
  ((ppm? pnm)
   (let* ((height (pnm-height pnm))
	  (width (pnm-width pnm))
	  (red (ppm-red pnm))
	  (green (ppm-green pnm))
	  (blue (ppm-blue pnm))
	  (new-red (make-matrix height width 0))
	  (new-green (make-matrix height width 0))
	  (new-blue (make-matrix height width 0))
	  (dy (y delta))
	  (dx (x delta)))
    (do ((y 0 (+ y 1))) ((>= y height))
     (when (and (>= (- y dy) 0) (< (- y dy) height))
      (do ((x 0 (+ x 1))) ((>= x width))
       (when (and (>= (- x dx) 0) (< (- x dx) width))
	(matrix-set! new-red y x (matrix-ref red (- y dy) (- x dx)))
	(matrix-set! new-green y x (matrix-ref green (- y dy) (- x dx)))
	(matrix-set! new-blue y x (matrix-ref blue (- y dy) (- x dx)))))))
    (make-ppm (ppm-raw? pnm) (ppm-maxval pnm) new-red new-green new-blue)))
  ((pgm? pnm)
   (let* ((height (pnm-height pnm))
	  (width (pnm-width pnm))
	  (grey (pgm-grey pnm))
	  (new-grey (make-matrix height width 0))
	  (dy (y delta))
	  (dx (x delta)))
    (do ((y 0 (+ y 1))) ((>= y height))
     (when (and (>= (- y dy) 0) (< (- y dy) height))
      (do ((x 0 (+ x 1))) ((>= x width))
       (when (and (>= (- x dx) 0) (< (- x dx) width))
	(matrix-set! new-grey y x (matrix-ref grey (- y dy) (- x dx)))))))
    (make-pgm (pgm-raw? pnm) (pgm-maxval pnm) new-grey)))
  ((pbm? pnm)
   (let* ((height (pnm-height pnm))
	  (width (pnm-width pnm))
	  (bitmap (pbm-bitmap pnm))
	  (new-bitmap (make-matrix height width #f))
	  (dy (y delta))
	  (dx (x delta)))
    (do ((y 0 (+ y 1))) ((>= y height))
     (when (and (>= (- y dy) 0) (< (- y dy) height))
      (do ((x 0 (+ x 1))) ((>= x width))
       (when (and (>= (- x dx) 0) (< (- x dx) width))
	(matrix-set! new-bitmap y x (matrix-ref bitmap (- y dy) (- x dx)))))))
    (make-pbm (pbm-raw? pnm) new-bitmap)))
  (else (panic "Argument is not a PNM"))))

(define (pnm-copy pnm)
 (cond
  ((ppm? pnm)
   (make-ppm (ppm-raw? pnm)
	     (ppm-maxval pnm)
	     (matrix-copy (ppm-red pnm))
	     (matrix-copy (ppm-green pnm))
	     (matrix-copy (ppm-blue pnm))))
  ((pgm? pnm)
   (make-pgm (pgm-raw? pnm) (pgm-maxval pnm) (matrix-copy (pgm-grey pnm))))
  ((pbm? pnm) (make-pbm (pbm-raw? pnm) (matrix-copy (pbm-bitmap pnm))))
  (else (panic "Argument is not a PNM"))))

(define (pnm-black-window pnm upper-left lower-right)
 (cond ((ppm? pnm)
	(let* ((ppm (pnm-copy pnm))
	       (red (ppm-red ppm))
	       (green (ppm-green ppm))
	       (blue (ppm-blue ppm))
	       (yl (y upper-left))
	       (yh (y lower-right))
	       (xl (x upper-left))
	       (xh (x lower-right))
	       (height (pnm-height ppm))
	       (width (pnm-width ppm)))
	 (do ((y 0 (+ y 1))) ((>= y height))
	  (do ((x 0 (+ x 1))) ((>= x width))
	   (unless (and (>= y yl) (< y yh) (>= x xl) (< x xh))
	    (matrix-set! red y x 0)
	    (matrix-set! green y x 0)
	    (matrix-set! blue y x 0))))
	 ppm))
       ((pgm? pnm)
	(let* ((pgm (pnm-copy pnm))
	       (grey (pgm-grey pgm))
	       (yl (y upper-left))
	       (yh (y lower-right))
	       (xl (x upper-left))
	       (xh (x lower-right))
	       (height (pnm-height pgm))
	       (width (pnm-width pgm)))
	 (do ((y 0 (+ y 1))) ((>= y height))
	  (do ((x 0 (+ x 1))) ((>= x width))
	   (unless (and (>= y yl) (< y yh) (>= x xl) (< x xh))
	    (matrix-set! grey y x 0))))
	 pgm))
       ((pbm? pnm)
	(let* ((pbm (pnm-copy pnm))
	       (bitmap (pbm-bitmap pbm))
	       (yl (y upper-left))
	       (yh (y lower-right))
	       (xl (x upper-left))
	       (xh (x lower-right))
	       (height (pnm-height pbm))
	       (width (pnm-width pbm)))
	 (do ((y 0 (+ y 1))) ((>= y height))
	  (do ((x 0 (+ x 1))) ((>= x width))
	   (unless (and (>= y yl) (< y yh) (>= x xl) (< x xh))
	    (matrix-set! bitmap y x #f))))
	 pbm))
       (else (panic "Argument is not a PNM"))))

(define (pnm-white-window pnm upper-left lower-right)
 (cond ((ppm? pnm)
	(let* ((ppm (pnm-copy pnm))
	       (maxval (ppm-maxval ppm))
	       (red (ppm-red ppm))
	       (green (ppm-green ppm))
	       (blue (ppm-blue ppm))
	       (yl (y upper-left))
	       (yh (y lower-right))
	       (xl (x upper-left))
	       (xh (x lower-right))
	       (height (pnm-height ppm))
	       (width (pnm-width ppm)))
	 (do ((y 0 (+ y 1))) ((>= y height))
	  (do ((x 0 (+ x 1))) ((>= x width))
	   (unless (and (>= y yl) (< y yh) (>= x xl) (< x xh))
	    (matrix-set! red y x maxval)
	    (matrix-set! green y x maxval)
	    (matrix-set! blue y x maxval))))
	 ppm))
       ((pgm? pnm)
	(let* ((pgm (pnm-copy pnm))
	       (maxval (pgm-maxval pgm))
	       (grey (pgm-grey pgm))
	       (yl (y upper-left))
	       (yh (y lower-right))
	       (xl (x upper-left))
	       (xh (x lower-right))
	       (height (pnm-height pgm))
	       (width (pnm-width pgm)))
	 (do ((y 0 (+ y 1))) ((>= y height))
	  (do ((x 0 (+ x 1))) ((>= x width))
	   (unless (and (>= y yl) (< y yh) (>= x xl) (< x xh))
	    (matrix-set! grey y x maxval))))
	 pgm))
       ((pbm? pnm)
	(let* ((pbm (pnm-copy pnm))
	       (bitmap (pbm-bitmap pbm))
	       (yl (y upper-left))
	       (yh (y lower-right))
	       (xl (x upper-left))
	       (xh (x lower-right))
	       (height (pnm-height pbm))
	       (width (pnm-width pbm)))
	 (do ((y 0 (+ y 1))) ((>= y height))
	  (do ((x 0 (+ x 1))) ((>= x width))
	   (unless (and (>= y yl) (< y yh) (>= x xl) (< x xh))
	    (matrix-set! bitmap y x #t))))
	 pbm))
       (else (panic "Argument is not a PNM"))))

(define (points->pbm-of-size points height width)
 ;; Takes a list of what will be the white (#T) points in the pbm image.
 (let ((bitmap (make-matrix height width #f)))
  (for-each (lambda (point) (matrix-set! bitmap (y point) (x point) #t))
	    points)
  (make-pbm #t bitmap)))

(define (pbm-ppm-and pbm ppm)
 ;; White (#T) pixels in the PBM are kept from the PPM. Black (#F) pixels in
 ;; the PBM become white in the result.
 (unless (and (pbm? pbm)
	      (ppm? ppm)
	      (= (pnm-width pbm) (pnm-width ppm))
	      (= (pnm-height pbm) (pnm-height ppm)))
  (panic "Arguments to PBM-AND are not matching PBMs"))
 (make-ppm (ppm-raw? ppm)
	   (ppm-maxval ppm)
	   (map-vector
	    (lambda (row red)
	     (map-vector (lambda (bit red) (if bit red *max-red*))
			 row red))
	    (pbm-bitmap pbm)
	    (ppm-red ppm))
	   (map-vector
	    (lambda (row green)
	     (map-vector (lambda (bit green) (if bit green *max-green*))
			 row green))
	    (pbm-bitmap pbm)
	    (ppm-green ppm))
	   (map-vector
	    (lambda (row blue)
	     (map-vector (lambda (bit blue) (if bit blue *max-blue*))
			 row blue))
	    (pbm-bitmap pbm)
	    (ppm-blue ppm))))

(define (pnm-rotate pnm)
 (cond ((pbm? pnm)
	(make-pbm (pbm-raw? pnm) (transpose (pbm-bitmap pnm))))
       ((pgm? pnm)
	(make-pgm (pgm-raw? pnm)
		  (pgm-maxval pnm)
		  (transpose (pgm-grey pnm))))
       ((ppm? pnm)
	(make-ppm (ppm-raw? pnm)
		  (ppm-maxval pnm)
		  (transpose (ppm-red pnm))
		  (transpose (ppm-green pnm))
		  (transpose (ppm-blue pnm))))
       (else (panic "Argument is not a PNM"))))

(define (pnm-flip pnm)
 (cond ((pbm? pnm)
	(make-pbm (pbm-raw? pnm)
		  (list->vector (reverse (vector->list (pbm-bitmap pnm))))))
       ((pgm? pnm)
	(make-pgm (pgm-raw? pnm)
		  (pgm-maxval pnm)
		  (list->vector (reverse (vector->list (pgm-grey pnm))))))
       ((ppm? pnm)
	(make-ppm (ppm-raw? pnm)
		  (ppm-maxval pnm)
		  (list->vector (reverse (vector->list (ppm-red pnm))))
		  (list->vector (reverse (vector->list (ppm-green pnm))))
		  (list->vector (reverse (vector->list (ppm-blue pnm))))))
       (else (panic "Argument is not a PNM"))))

;;; An API to libjpeg.a and the UCB mpeg_encode and mpeg_play programs.

(define c-open-video-input-file
 ;; changed
 (foreign-procedure (char* int) void* "open_video_input_file"))

(define c-read-video
 ;; changed
 (foreign-procedure (void*) int "read_video"))

(define c-get-video-width
 ;; changed
 (foreign-procedure (void*) int "get_video_width"))

(define c-get-video-height
 ;; changed
 (foreign-procedure (void*) int "get_video_height"))

(define c-get-video-red
 ;; changed: used to return unsigned
 (foreign-procedure (void* int int) int "get_video_red"))

(define c-get-video-green
 ;; changed: used to return unsigned
 (foreign-procedure (void* int int) int "get_video_green"))

(define c-get-video-blue
 ;; changed: used to return unsigned
 (foreign-procedure (void* int int) int "get_video_blue"))

(define c-close-video-input-port
 ;; changed
 (foreign-procedure (void*) void "close_video_input_port"))

(define (video-type-enum video-type)
 (cond ((string=? video-type "Jpeg") 0)
       ((string=? video-type "Mpeg1") 1)
       (else (panic "Unrecognized video type"))))

(define (open-video-input-file pathname video-type)
 ;; needs work: Move error checking to here.
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (c-open-video-input-file
  (default-extension pathname video-type) (video-type-enum video-type)))

(define (get-video-width video-input-port)
 (c-get-video-width video-input-port))

(define (get-video-height video-input-port)
 (c-get-video-height video-input-port))

(define (read-ppm-from-video-input-port video-input-port)
 (if (zero? (c-read-video video-input-port))
     ;; needs work: This should return an EOF object.
     #f
     (let* ((width (c-get-video-width video-input-port))
	    (height (c-get-video-height video-input-port))
	    (red (make-matrix height width))
	    (green (make-matrix height width))
	    (blue (make-matrix height width)))
      (do ((y 0 (+ y 1))) ((= y height))
       (do ((x 0 (+ x 1))) ((= x width))
	(matrix-set! red y x (c-get-video-red video-input-port y x))
	(matrix-set! green y x (c-get-video-green video-input-port y x))
	(matrix-set! blue y x (c-get-video-blue video-input-port y x))))
      (make-ppm #t 255 red green blue))))

(define (read-pixmap-from-video-input-port video-input-port)
 (let ((ppm (read-ppm-from-video-input-port video-input-port)))
  (if ppm
      (pnm->pixmap ppm)
      ;; needs work: This should return an EOF object.
      #f)))

(define (close-video-input-port video-input-port)
 ;; changed
 (c-close-video-input-port video-input-port))

(define (call-with-video-input-file pathname video-type procedure)
 (let* ((video-input-port (open-video-input-file pathname video-type))
	(result (procedure video-input-port)))
  (close-video-input-port video-input-port)
  result))

(define (video-file-length pathname video-type)
 (call-with-video-input-file
  pathname
  video-type
  (lambda (video-input-port)
   (let loop ((i 0))
    (if (read-ppm-from-video-input-port video-input-port) (loop (+ i 1)) i)))))

(define (video-file-frame->ppm pathname video-type frame)
 (call-with-video-input-file
  pathname
  video-type
  (lambda (video-input-port)
   (for-each-n (lambda (i) (read-ppm-from-video-input-port video-input-port))
	       frame)
   (read-ppm-from-video-input-port video-input-port))))

(define (video-file-frame->pixmap pathname video-type frame)
 (call-with-video-input-file
  pathname
  video-type
  (lambda (video-input-port)
   (for-each-n (lambda (i) (read-ppm-from-video-input-port video-input-port))
	       frame)
   (read-pixmap-from-video-input-port video-input-port))))

(define (video-file->pnm-movie pathname video-type)
 (call-with-video-input-file
  pathname
  video-type
  (lambda (video-input-port)
   (let loop ((pnm-movie '()))
    (let ((ppm (read-ppm-from-video-input-port video-input-port)))
     (if ppm
	 (loop (cons ppm pnm-movie))
	 (list->vector (reverse pnm-movie))))))))

(define (video-file->pixmaps pathname video-type)
 (call-with-video-input-file
  pathname
  video-type
  (lambda (video-input-port)
   (let loop ((pixmaps '()))
    (let ((pixmap (read-pixmap-from-video-input-port video-input-port)))
     (if pixmap
	 (loop (cons pixmap pixmaps))
	 (list->vector (reverse pixmaps))))))))

(define c-open-video-output-file
 ;; changed
 (foreign-procedure (char* int int) void* "open_video_output_file"))

(define c-begin-video-frame
 ;; changed
 (foreign-procedure (int int int) void "begin_video_frame"))

(define c-put-video-red
 ;; changed
 (foreign-procedure (int int int) void "put_video_red"))

(define c-put-video-green
 ;; changed
 (foreign-procedure (int int int) void "put_video_green"))

(define c-put-video-blue
 ;; changed
 (foreign-procedure (int int int) void "put_video_blue"))

(define c-end-video-frame
 ;; changed
 (foreign-procedure () void "end_video_frame"))

(define c-close-video-output-port
 ;; changed
 (foreign-procedure (void*) void "close_video_output_port"))

(define (open-video-output-file pathname video-type frames)
 ;; needs work: Move error checking to here.
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (c-open-video-output-file
  (default-extension pathname video-type) (video-type-enum video-type) frames))

(define (write-pnm-to-video-output-port pnm video-output-port)
 (define (write-ppm-to-video-output-port ppm)
  (let ((width (pnm-width ppm))
	(height (pnm-height ppm))
	(red (ppm-red ppm))
	(green (ppm-green ppm))
	(blue (ppm-blue ppm)))
   (c-begin-video-frame width height (ppm-maxval ppm))
   (do ((y 0 (+ y 1))) ((= y height))
    (do ((x 0 (+ x 1))) ((= x width))
     (c-put-video-red y x (matrix-ref red y x))
     (c-put-video-green y x (matrix-ref green y x))
     (c-put-video-blue y x (matrix-ref blue y x)))))
  ;; changed
  (c-end-video-frame))
 (cond ((ppm? pnm) (write-ppm-to-video-output-port pnm))
       ((pgm? pnm) (write-ppm-to-video-output-port (pgm->ppm pnm)))
       ((pbm? pnm) (write-ppm-to-video-output-port (pbm->ppm pnm)))
       (else (fuck-up))))

(define (close-video-output-port video-output-port)
 ;; changed
 (c-close-video-output-port video-output-port))

(define (call-with-video-output-file pathname video-type frames procedure)
 (let* ((video-output-port (open-video-output-file pathname video-type frames))
	(result (procedure video-output-port)))
  (close-video-output-port video-output-port)
  result))

(define (pnm-movie->video-file pnm-movie pathname video-type)
 (call-with-video-output-file
  pathname
  video-type
  (vector-length pnm-movie)
  (lambda (video-output-port)
   (for-each-vector
    (lambda (pnm) (write-pnm-to-video-output-port pnm video-output-port))
    pnm-movie))))

(define (map-adjacent-video-file input-pathname
				 input-video-type
				 output-pathname
				 output-video-type
				 processor)
 (let ((frames (video-file-length input-pathname (extension input-pathname))))
  (call-with-video-input-file
   input-pathname
   input-video-type
   (lambda (video-input-port)
    (call-with-video-output-file
     output-pathname
     output-video-type
     (- frames 1)
     (lambda (video-output-port)
      (let loop ((last-ppm #f))
       (let ((ppm (read-ppm-from-video-input-port video-input-port)))
	(when ppm
	 (when last-ppm
	  (write-pnm-to-video-output-port
	   (processor last-ppm ppm) video-output-port))
	 (loop ppm))))))))))

(define (map-adjacent-video-file-to-object-file input-pathname
						input-video-type
						output-pathname
						processor)
 (let ((frames '()))
  (call-with-video-input-file
   input-pathname
   input-video-type
   (lambda (video-input-port)
    (let loop ((last-ppm #f))
     (let ((ppm (read-ppm-from-video-input-port video-input-port)))
      (when ppm
       (when last-ppm (set! frames (cons (processor last-ppm ppm) frames)))
       (loop ppm))))))
  (write-object-to-file (list->vector (reverse frames)) output-pathname)))

;;; Multivariate Nonlinear Optimization

(define *itmax-powell* 200)
(define *itmax-brent* 100)
(define *tol* 2.0e-4)
(define *glimit* 100.0)
(define *tiny* 1.0e-20)
(define *zeps* 1.0e-10)

(define (brent ax bx cx f tol k)
 (define (sign x y) (if (positive? y) (abs x) (- (abs x))))
 (let* ((cgold 0.3819660)
	(d #f)				;needs work: type pollution
	(e 0.0)
	(foo (f bx)))
  (let loop ((a (min ax cx))
	     (b (max ax cx))
	     (v bx)
	     (fv foo)
	     (w bx)
	     (fw foo)
	     (x bx)
	     (fx foo)
	     (iter 0))
   (when (= iter *itmax-brent*) (panic "Too many iterations in BRENT"))
   (let* ((xm (* 0.5 (+ a b)))
	  (tol1 (+ (* tol (abs x)) *zeps*))
	  (tol2 (* 2.0 tol1)))
    (cond ((<= (abs (- x xm)) (- tol2 (* 0.5 (- b a)))) (k fx x))
	  (else
	   (cond
	    ((> (abs e) tol1)
	     (let* ((r (* (- x w) (- fx fv)))
		    (q (* (- x v) (- fx fw)))
		    (p (- (* (- x v) q) (* (- x w) r)))
		    (q (* 2.0 (- q r)))
		    (p (if (positive? q) (- p) p))
		    (q (abs q))
		    (etemp e))
	      (cond ((or (>= (abs p) (abs (* 0.5 q etemp)))
			 (<= p (* q (- a x)))
			 (> p (* q (- b x))))
		     (set! e (if (>= x xm) (- a x) (- b x)))
		     (set! d (* cgold e)))
		    (else (set! e d)
			  (set! d (/ p q))
			  (let ((u (+ x d)))
			   (when (or (< (- u a) tol2) (< (- b u) tol2))
			    (set! d (sign tol1 (- xm x)))))))))
	    (else (set! e (if (>= x xm) (- a x) (- b x)))
		  (set! d (* cgold e))))
	   (let* ((u (if (>= (abs d) tol1) (+ x d) (+ x (sign tol1 d))))
		  (fu (f u)))
	    (if (<= fu fx)
		(if (>= u x)
		    (loop x b w fw x fx u fu (+ iter 1))
		    (loop a x w fw x fx u fu (+ iter 1)))
		(if (< u x)
		    (cond ((or (<= fu fw) (= w x))
			   (loop u b w fw u fu x fx (+ iter 1)))
			  ((or (<= fu fv) (= v x) (= v w))
			   (loop u b u fu w fw x fx (+ iter 1)))
			  (else (loop u b v fv w fw x fx (+ iter 1))))
		    (cond ((or (<= fu fw) (= w x))
			   (loop a u w fw u fu x fx (+ iter 1)))
			  ((or (<= fu fv) (= v x) (= v w))
			   (loop a u u fu w fw x fx (+ iter 1)))
			  (else (loop a u v fv w fw x fx (+ iter 1)))))))))))))

(define (mnbrak ax bx func k)
 (define (sign x y) (if (positive? y) (abs x) (- (abs x))))
 (let ((gold 1.61803399))
  (define (loop ax fa bx fb cx fc)
   (if (< fb fc)
       (k ax fa bx fb cx fc)
       (let* ((r (* (- bx ax) (- fb fc)))
	      (q (* (- bx cx) (- fb fa)))
	      (u (- bx (/ (- (* (- bx cx) q) (* (- bx ax) r))
			  (* 2.0 (sign (max (abs (- q r)) *tiny*) (- q r))))))
	      (ulim (+ bx (* *glimit* (- cx bx)))))
	(if (positive? (* (- bx u) (- u cx)))
	    (let ((fu (func u)))
	     (if (< fu fc)
		 (k bx fb u fu cx fc)
		 (if (> fu fb)
		     (k ax fa bx fb u fu)
		     (let ((vx (+ cx (* gold (- cx bx)))))
		      (loop bx fb cx fc vx (func vx))))))
	    (if (positive? (* (- cx u) (- u ulim)))
		(let ((fu (func u)))
		 (if (< fu fc)
		     (let ((vx (+ u (* gold (- u cx)))))
		      (loop cx fc u fu vx (func vx)))
		     (loop bx fb cx fc u fu)))
		(if (negative? (* (- u ulim) (- ulim cx)))
		    (let ((vx (+ cx (* gold (- cx bx)))))
		     (loop bx fb cx fc vx (func vx)))
		    (loop bx fb cx fc ulim (func ulim))))))))
  (let ((fa (func ax))
	(fb (func bx)))
   (if (> fb fa)
       (let ((vx (+ ax (* gold (- ax bx)))))
	(loop bx fb ax fa vx (func vx)))
       (let ((vx (+ bx (* gold (- bx ax)))))
	(loop ax fa bx fb vx (func vx)))))))

(define (linmin p xi func)
 (let ((n (vector-length p)))
  (mnbrak 0.0 1.0 (lambda (x) (func (v+ p (k*v x xi))))
	  (lambda (ax fa xx fx bx fb)
	   (brent ax xx bx (lambda (x) (func (v+ p (k*v x xi)))) *tol*
		  (lambda (fret xmin)
		   (do ((j 0 (+ j 1))) ((= j n))
		    (vector-set! xi j (* (vector-ref xi j) xmin))
		    (vector-set! p j (+ (vector-ref p j) (vector-ref xi j))))
		   fret))))))

(define (powell p xi ftol func)
 (let* ((n (vector-length p))
	(pt (make-vector n))
	(ptt (make-vector n))
	(xit (make-vector n))
	(fret (func p)))
  (do ((j 0 (+ j 1))) ((= j n)) (vector-set! pt j (vector-ref p j)))
  (let loop ((iter 0))
   (let ((fp fret)
	 (ibig 0)
	 (del 0.0))
    (do ((i 0 (+ i 1))) ((= i n))
     (do ((j 0 (+ j 1))) ((= j n)) (vector-set! xit j (matrix-ref xi j i)))
     (let ((fptt fret))
      (set! fret (linmin p xit func))
      (when (> (abs (- fptt fret)) del)
       (set! del (abs (- fptt fret)))
       (set! ibig i))))
    (cond
     ((<= (* 2.0 (abs (- fp fret))) (* ftol (+ (abs fp) (abs fret)))) fret)
     (else
      (when (= iter *itmax-powell*)
       (panic "Too many iterations in routine POWELL"))
      (do ((j 0 (+ j 1))) ((= j n))
       (vector-set! ptt j (* 2.0 (- (vector-ref p j) (vector-ref pt j))))
       (vector-set! xit j (- (vector-ref p j) (vector-ref pt j)))
       (vector-set! pt j (vector-ref p j)))
      (let ((fptt (func ptt)))
       (when (and
	      (< fptt fp)
	      (negative?
	       (- (* 2.0 (+ (- fp (* 2.0 fret)) fptt) (sqr (- fp fret del)))
		  (* del (sqr (- fp fptt))))))
	(set! fret (linmin p xit func))
	(do ((j 0 (+ j 1))) ((= j n))
	 (matrix-set! xi j ibig (vector-ref xit j))))
       (loop (+ iter 1)))))))))

;;; EM Clusterer

(define-structure model
 pi mu sigma log-pi sigma-inverse log-determinant-sigma)

(define (log-likelihood x model)
 ;; note: This is missing a constant factor.
 (- (model-log-pi model)
    (* 0.5
       (+ (model-log-determinant-sigma model)
	  (dot (v- x (model-mu model))
	       (m*v (model-sigma-inverse model) (v- x (model-mu model))))))))

(define (e-step x models)
 ;; The constants are hardwired to be inexact for efficiency.
 (let ((z (map-vector
	   (lambda (xi)
	    ;; Compute for each model.
	    (map-vector (lambda (model) (log-likelihood xi model)) models))
	   x)))
  ;; Normalize ownerships to sum to one.
  (let ((s (map-vector (lambda (zi) (reduce-vector add-exp zi minus-infinity))
		       z)))
   ;; Return log likelihood and ownerships matrix.
   (list (reduce-vector + s 0.0)
	 (map-vector (lambda (zi) (map-vector exp zi))
		     (m- z (transpose (make-vector (matrix-columns z) s))))))))

(define (m-step x z clip)
 ;; The constants are hardwired to be inexact for efficiency.
 ;; Returns new set of models.
 (let* ((ii (vector-length x))
	(kk (vector-length (vector-ref x 0))))
  ;; For each model, optimize parameters.
  (map-n-vector
   (lambda (j)
    (let* ((zj (matrix-column-ref z j))
	   (zj-sum (reduce-vector + zj 0.0))
	   ;; Optimize values.
	   (mu (k*v (/ zj-sum)
		    (reduce-vector
		     v+ (map-vector k*v zj x) (make-vector kk 0.0))))
	   (sigma (clip-eigenvalues
		   (k*m (/ zj-sum)
			(reduce-vector
			 m+
			 (map-vector
			  (lambda (zij xi)
			   (k*m zij (self-outer-product * (v- xi mu))))
			  zj
			  x)
			 (make-matrix kk kk 0.0)))
		   clip)))
     (make-model (/ zj-sum ii)
		 mu
		 sigma
		 (log (/ zj-sum ii))
		 (invert-matrix sigma)
		 (log (determinant sigma)))))
   (matrix-columns z))))

(define (em x pi mu sigma clip em-kick-off-tolerance em-convergence-tolerance)
 (let ((jj (vector-length mu)))
  (let loop ((models (map-n-vector (lambda (j)
				    (make-model
				     (vector-ref pi j)
				     (vector-ref mu j)
				     (vector-ref sigma j)
				     (log (vector-ref pi j))
				     (invert-matrix (vector-ref sigma j))
				     (log (determinant (vector-ref sigma j)))))
				   jj))
	     (old-log-likelihood minus-infinity)
	     (starting? #t))
   (let ((log-likelihood-z (e-step x models)))
    (if (or (and starting? (> (first log-likelihood-z) old-log-likelihood))
	    (> (first log-likelihood-z)
	       (+ old-log-likelihood em-convergence-tolerance)))
	(loop (m-step x (second log-likelihood-z) clip)
	      (first log-likelihood-z)
	      (and starting?
		   (not (= jj 1))
		   (or (= old-log-likelihood minus-infinity)
		       (< (first log-likelihood-z)
			  (+ old-log-likelihood em-kick-off-tolerance)))))
	(list old-log-likelihood models))))))

(define (noise epsilon) (- (* 2.0 epsilon (random-real)) epsilon))

(define (initial-z ii jj)
 (map-n-vector
  (lambda (i)
   (let ((zi (map-n-vector (lambda (j) (+ (/ jj) (noise (/ jj)))) jj)))
    (k*v (/ (reduce-vector + zi 0.0)) zi)))
  ii))

(define (ems x clip em-kick-off-tolerance em-convergence-tolerance
	     ems-convergence-tolerance)
 ;; needs work: type pollution
 (let loop ((jj 1) (old-log-likelihood-models (list minus-infinity #f)))
  (let* ((models (m-step x (initial-z (vector-length x) jj) clip))
	 (new-log-likelihood-models
	  (em x
	      (map-vector model-pi models)
	      (map-vector model-mu models)
	      (map-vector model-sigma models)
	      clip
	      em-kick-off-tolerance
	      em-convergence-tolerance)))
   (if (or (and (not (= (first old-log-likelihood-models) minus-infinity))
		(not (zero? (first old-log-likelihood-models)))
		(<= (/ (- (first new-log-likelihood-models)
			  (first old-log-likelihood-models))
		       (abs (first old-log-likelihood-models)))
		    ems-convergence-tolerance))
	   (some-vector
	    (lambda (model1)
	     (some-vector
	      (lambda (model2)
	       (and (not (eq? model1 model2))
		    (equal? (model-mu model1) (model-mu model2))
		    (equal? (model-sigma model1) (model-sigma model2))))
	      (second new-log-likelihood-models)))
	    (second new-log-likelihood-models)))
       (second old-log-likelihood-models)
       (loop (+ jj 1) new-log-likelihood-models)))))

(define (em-jj-clusterer
	 x jj clip em-kick-off-tolerance em-convergence-tolerance)
 (let* ((models (m-step x (initial-z (vector-length x) jj) clip))
	(z (second (e-step x (second (em x
					 (map-vector model-pi models)
					 (map-vector model-mu models)
					 (map-vector model-sigma models)
					 clip
					 em-kick-off-tolerance
					 em-convergence-tolerance)))))
	(clusters
	 (map-n (lambda (i)
		 (let ((zi (vector->list (vector-ref z i))))
		  (list i (positionv (reduce max zi minus-infinity) zi))))
		(vector-length z))))
  (map-n (lambda (j)
	  (map (lambda (cluster) (vector-ref x (first cluster)))
	       (remove-if-not (lambda (cluster) (= (second cluster) j))
			      clusters)))
	 (vector-length (vector-ref z 0)))))

(define (em-clusterer x clip em-kick-off-tolerance em-convergence-tolerance
		      ems-convergence-tolerance)
 (let* ((z (second (e-step x (ems x clip em-kick-off-tolerance
				  em-convergence-tolerance
				  ems-convergence-tolerance))))
	(clusters
	 (map-n (lambda (i)
		 (let ((zi (vector->list (vector-ref z i))))
		  (list i (positionv (reduce max zi minus-infinity) zi))))
		(vector-length z))))
  (map-n (lambda (j)
	  (map (lambda (cluster) (vector-ref x (first cluster)))
	       (remove-if-not (lambda (cluster) (= (second cluster) j))
			      clusters)))
	 (vector-length (vector-ref z 0)))))

;;; CPU time procedures courtesy of James Rootham

(define clock (foreign-procedure () int "clock" "time")) ;changed

(define *clock-time* 0.0)
(define *clock-in-time-out* #f)
(define *start-time-out* 0.0)
(define *time-out-time* 0.0)

(define fix 0.0)
(define bad #t)

;;; changed
(define *clocks-per-second* ((primitive-procedure clocks-per-second)))

;;; This wraparound fix only works if you sample cpu time at least every 35
;;; minutes when *CLOCKS-PER-SECOND* is 1000000. It is not as bad if
;;; *CLOCKS-PER-SECOND* is 100.
(define (read-clock)
 (let ((time (clock)))
  (if (< time 0)
      (when bad (set! fix (+ fix (expt 2.0 32))) (set! bad #f))
      (set! bad #t))
  (/ (+ time fix) *clocks-per-second*)))

(define (clock-reset)
 (read-clock)
 (set! *clock-time* 0.0)
 (set! *clock-in-time-out* #f)
 (set! *start-time-out* 0.0)
 (set! *time-out-time* 0.0))

(define (clock-sample)
 (if *clock-in-time-out*
     (- *clock-time* *time-out-time*)
     (let ((time (read-clock)))
      (set! *clock-time* time)
      (- *clock-time* *time-out-time*))))

(define (clock-time-out)
 (let ((time (read-clock)))
  (set! *start-time-out* time)
  (set! *clock-in-time-out* #t)))

(define (clock-time-in)
 (set! *clock-in-time-out* #f)
 (let ((time (read-clock)))
  (set! *time-out-time* (- (+ *time-out-time* time) *start-time-out*))))

;;; An API to C arrays courtesy of Richard Mann

;;; removed: The API to C arrays

;;; Sclim

(define return (integer->char 13))
(define escape (integer->char 27))
(define delete (integer->char 127))
;;; needs work: type pollution in all of the following
(define *display* #f)
(define *screen* #f)
(define *root-window* #f)
(define *button-width* #f)
(define *button-height* #f)
(define *background-color* "White")
(define *foreground-color* "Black")
(define *background* #f)
(define *foreground* #f)
(define *white-pixel* #f)
(define *black-pixel* #f)
(define *roman-font* #f)
(define *bold-font* #f)
(define *roman-height* #f)
(define *bold-height* #f)
(define *text-height* #f)
(define *roman-baseline* #f)
(define *bold-baseline* #f)
(define *text-baseline* #f)
(define *display-pane-width* #f)
(define *display-pane-height* #f)
(define *transcript-pane-height* #f)
(define *echo-pane-height* #f)
(define *who-line-height* #f)
(define *status-pane-width* #f)
(define *window* #f)
(define *buttons* #f)
(define *regions* #f)
(define *display-pane* #f)
(define *transcript-pane* #f)
(define *echo-pane* #f)
(define *status-pane* #f)
(define *message-pane* #f)
(define *thin-gc* #f)
(define *thin-flipping-gc* #f)
(define *medium-gc* #f)
(define *medium-flipping-gc* #f)
(define *thick-gc* #f)
(define *thick-flipping-gc* #f)
(define *dashed-gc* #f)
(define *dashed-flipping-gc* #f)
(define *roman-gc* #f)
(define *bold-gc* #f)
(define *bold-flipping-gc* #f)
(define *light-gray* #f)
(define *light-gray-gc* #f)
(define *gray* #f)
(define *gray-gc* #f)
(define *red* #f)
(define *red-gc* #f)
(define *dark-red* #f)
(define *dark-red-gc* #f)
(define *green* #f)
(define *green-gc* #f)
(define *dark-green* #f)
(define *dark-green-gc* #f)
(define *blue* #f)
(define *blue-gc* #f)
(define *yellow* #f)
(define *yellow-gc* #f)
(define *violet* #f)
(define *violet-gc* #f)
(define *orange* #f)
(define *orange-gc* #f)
(define *dark-orange* #f)
(define *dark-orange-gc* #f)
(define *color-gc* #f)
(define *window-methods* #f)
(define *transcript* #f)
(define *input* #f)
(define *input-position* #f)
(define *abort-button* #f)
(define *abort-key* #f)
(define *comtab* #f)
(define *help-comtab* #f)
(define *prefix* #f)
(define *status* #f)
(define *message* #f)
(define *pause?* #f)
(define *redraw-procedure* #f)
(define *quit-continuation* #f)
(define *abort-continuation* #f)
(define *color-cube* #f)
(define *reds* 4)
(define *greens* 8)
(define *blues* 4)
(define *dither?* #t)
(define *help?* #f)
(define *help* #f)
(define *first-help-line* 0)
(define *clear-display-pane?* #f)
(define *display-name* "")
(define *roman-font-name* "9x15")
(define *bold-font-name* "9x15bold")
(define *window-position?* #f)
(define *window-position-x* 0)
(define *window-position-y* 0)
(define *post-initialize-procedure* #f)

(define (character->pretty-name character)
 (let ((i (char->integer character)))
  (cond ((= i 0) "C-@")			;also C-SPC
	((= i 9) "TAB")			;also C-i
	((= i 10) "LFD")		;also C-j
	((= i 13) "RET")		;also C-m
	((= i 27) "ESC")		;also C-[
	((= i 28) "C-\\")
	((= i 29) "C-]")
	((= i 30) "C-^")
	((= i 31) "C-_")		;also C-/
	((= i 32) "SPC")
	((= i 127) "DEL")
	((= i 128) "M-C-@")		;also M-C-SPC
	((= i 137) "M-TAB")		;also M-C-i
	((= i 138) "M-LFD")		;also M-C-j
	((= i 141) "M-RET")		;also M-C-m
	((= i 155) "M-ESC")		;also M-C-[
	((= i 156) "M-C-\\")
	((= i 157) "M-C-]")
	((= i 158) "M-C-^")
	((= i 159) "M-C-_")		;also M-C-/
	((= i 160) "M-SPC")
	((= i 255) "M-DEL")
	(else (if (>= i 128)
		  (let ((i (- i 128)))
		   (if (< i 32)
		       (format #f "M-C-~a" (integer->char (+ i 96)))
		       (format #f "M-~a" (integer->char i))))
		  (if (< i 32)
		      (format #f "C-~a" (integer->char (+ i 96)))
		      (string character)))))))

(define (prefix-string prefix)
 (if (null? (rest prefix))
     (character->pretty-name (first prefix))
     (format #f "~a ~a"
	     (prefix-string (rest prefix))
	     (character->pretty-name (first prefix)))))

(define (set-window-method! window event-type method)
 (set! *window-methods*
       (cons (cons (list window event-type) method) *window-methods*)))

(define (send window event-type . &rest)
 (let ((x (assoc (list window event-type) *window-methods*)))
  (when x (apply (cdr x) &rest))))

(define (redraw-buttons)
 (for-each (lambda (button) (send button 'expose)) *buttons*)
 (xflush *display*))

(define (redraw-display-pane)
 (when *display-pane*
  (when *clear-display-pane?* (xclearwindow *display* *display-pane*))
  (set! *regions* '())
  (if *help?*
      (let* ((character-strings
	      (map (lambda (help-entry)
		    (if (list? (first help-entry))
			(reduce
			 (lambda (s t) (string-append s " " t))
			 (map character->pretty-name (first help-entry))
			 "")
			(character->pretty-name (first help-entry))))
		   (reverse *help*)))
	     (n (+ (reduce max (map string-length character-strings) 0) 1)))
       (let loop ((character-strings character-strings)
		  (documentation-strings (map second (reverse *help*)))
		  (y (+ *text-height* 2))
		  (skip *first-help-line*))
	(unless (null? character-strings)
	 (if (zero? skip)
	     (let ((line
		    (format #f "~a~a~a"
			    (first character-strings)
			    (make-string
			     (- n (string-length (first character-strings)))
			     #\space)
			    (first documentation-strings))))
	      (xdrawstring *display* *display-pane* *roman-gc*
			   5 (- y (+ *roman-baseline* 2))
			   line (string-length line))
	      (loop (rest character-strings)
		    (rest documentation-strings)
		    (+ y *text-height*)
		    skip))
	     (loop (rest character-strings)
		   (rest documentation-strings)
		   y
		   (- skip 1))))))
      (*redraw-procedure*))
  (xflush *display*)))

(define (redraw-transcript-pane)
 (when *transcript-pane*
  (xclearwindow *display* *transcript-pane*)
  (let loop ((transcript *transcript*) (y (- *transcript-pane-height* 2)))
   (unless (null? transcript)
    (let* ((line (first transcript))
	   (text-height
	    (if (eq? (first line) 'user) *roman-height* *bold-height*)))
     (when (>= y (- text-height 1))
      (xdrawstring
       *display* *transcript-pane*
       (if (eq? (first line) 'user) *roman-gc* *bold-gc*)
       5
       (- y (if (eq? (first line) 'user) *roman-baseline* *bold-baseline*))
       (second line) (string-length (second line)))
      (loop (rest transcript) (- y text-height))))))
  (xflush *display*)))

(define (redraw-echo-pane)
 (when *echo-pane*
  (xclearwindow *display* *echo-pane*)
  (let* ((n (quotient (- *display-pane-width* 10)
		      (xtextwidth *roman-font* "m" (string-length "m"))))
	 (m (max 0 (- *input-position* n)))
	 (input (substring *input* m (string-length *input*)))
	 (input (substring input 0 (min (string-length input) n))))
   (xdrawstring *display* *echo-pane* *roman-gc*
		5 (- *echo-pane-height* (+ *roman-baseline* 2))
		input (string-length input))
   (xdrawline *display* *echo-pane* *thin-gc*
	      (+ 5 (xtextwidth *roman-font* *input* (- *input-position* m)))
	      2
	      (+ 5 (xtextwidth *roman-font* *input* (- *input-position* m)))
	      (- *echo-pane-height* 3)))
  (xflush *display*)))

(define (redraw-status-pane)
 (when *status-pane*
  (xclearwindow *display* *status-pane*)
  (xdrawstring
   *display* *status-pane* *roman-gc*
   (quotient (- *status-pane-width*
		(xtextwidth *roman-font* *status* (string-length *status*)))
	     2)
   (- *who-line-height* (+ *roman-baseline* 2))
   *status* (string-length *status*))
  (xflush *display*)))

(define (redraw-message-pane)
 (when *message-pane*
  (xclearwindow *display* *message-pane*)
  (if (null? *prefix*)
      (xdrawstring *display* *message-pane* *roman-gc*
		   5
		   (- *who-line-height* (+ *roman-baseline* 2))
		   *message* (string-length *message*))
      (let ((string (prefix-string *prefix*)))
       (xdrawstring *display* *message-pane* *roman-gc*
		    5
		    (- *who-line-height* (+ *roman-baseline* 2))
		    string (string-length string))))
  (xflush *display*)))

(define-structure region button state-mask state x y width height method)

(define (define-region x y width height method)
 (set! *regions*
       (cons (make-region #f 0 0 x y width height method) *regions*)))

(define (define-button-specific-region
	 button state-mask state x y width height method)
 (set! *regions*
       (cons (make-region button state-mask state x y width height method)
	     *regions*)))

(define (region-handler x y button state)
 (let ((region
	(find-if (lambda (region)
		  (and (<= (region-x region) x)
		       (<  x (+ (region-x region) (region-width region)))
		       (<= (region-y region) y)
		       (<  y (+ (region-y region) (region-height region)))
		       (or (not (region-button region))
			   (= button (region-button region)))
		       (= (bit-and state (region-state-mask region))
			  (region-state region))))
		 *regions*)))
  (when region
   (let ((old-status *status*))
    (xselectinput *display*
		  *window*
		  (bit-or exposuremask
			  buttonpressmask
			  buttonreleasemask
			  keypressmask))
    (xselectinput *display*
		  *display-pane*
		  (bit-or exposuremask
			  buttonpressmask
			  buttonreleasemask
			  keypressmask))
    (status "Run")
    (call-with-current-continuation
     (lambda (abort-continuation)
      (set! *abort-continuation* abort-continuation)
      ;; changed
      ((region-method region) x y)))
    (xselectinput *display*
		  *window*
		  (bit-or exposuremask
			  pointermotionmask
			  buttonpressmask
			  buttonreleasemask
			  keypressmask))
    (xselectinput *display*
		  *display-pane*
		  (bit-or exposuremask
			  pointermotionmask
			  buttonpressmask
			  buttonreleasemask
			  keypressmask))
    (status old-status)))))

(define (abort?)
 (let loop ((events '()))
  (cond
   ((> (xpending *display*) 0)
    (let ((event (make-xevent)))
     (xnextevent *display* event)	;changed
     (cond
      ((or
	(and *abort-button*
	     (= (xevent-xany-type event) buttonpress)
	     (eq? (xevent-xany-window event) *abort-button*))
	(and
	 *abort-key*
	 (= (xevent-xany-type event) keypress)
	 (= (string-length (ylookupstring event)) 1)
	 (if (>= (char->integer *abort-key*) 128)
	     (and (char=?
		   (string-ref (ylookupstring event) 0)
		   (integer->char (- (char->integer *abort-key*) 128)))
		  (not (zero? (bit-and (xevent-xkey-state event) mod1mask))))
	     (and (char=? (string-ref (ylookupstring event) 0) *abort-key*)
		  (zero? (bit-and (xevent-xkey-state event) mod1mask))))))
       (for-each (lambda (event) (xputbackevent *display* event)) events)
       #t)
      (else (loop (cons event events))))))
   (else (for-each (lambda (event) (xputbackevent *display* event)) events)
	 #f))))

(define (process-events)
 (call-with-current-continuation
  (lambda (quit-continuation)
   (set! *quit-continuation* quit-continuation)
   (let ((event (make-xevent))
	 (comtab (if *help?* *help-comtab* *comtab*))
	 (n 0))
    (let loop ()
     (xnextevent *display* event)	;changed
     (let ((event-type (xevent-xany-type event))
	   (window (xevent-xany-window event)))
      (define (execute-key character)
       (let ((command (vector-ref comtab (char->integer character))))
	(cond ((vector? command)
	       (set! *prefix* (cons character *prefix*))
	       (redraw-message-pane)
	       (set! comtab command))
	      ((procedure? command)
	       (set! *prefix* '())
	       (redraw-message-pane)
	       (set! comtab (if *help?* *help-comtab* *comtab*))
	       (let ((old-status *status*))
		(xselectinput *display*
			      *window*
			      (bit-or exposuremask
				      buttonpressmask
				      buttonreleasemask
				      keypressmask))
		(xselectinput *display*
			      *display-pane*
			      (bit-or exposuremask
				      buttonpressmask
				      buttonreleasemask
				      keypressmask))
		(status "Run")
		(message "")
		(call-with-current-continuation
		 (lambda (abort-continuation)
		  (set! *abort-continuation* abort-continuation)
		  ;; changed
		  (command)))
		(xselectinput *display*
			      *window*
			      (bit-or exposuremask
				      pointermotionmask
				      buttonpressmask
				      buttonreleasemask
				      keypressmask))
		(xselectinput *display*
			      *display-pane*
			      (bit-or exposuremask
				      pointermotionmask
				      buttonpressmask
				      buttonreleasemask
				      keypressmask))
		(status old-status))
	       (set! comtab (if *help?* *help-comtab* *comtab*)))
	      (else (set! *prefix* '())
		    (redraw-message-pane)
		    (set! *help?* #f)
		    (redraw-buttons)
		    (redraw-display-pane)
		    (set! comtab (if *help?* *help-comtab* *comtab*))
		    (xbell *display* 100)))))
      (cond ((= event-type mappingnotify) (xrefreshkeyboardmapping event))
	    ((= event-type expose)
	     (set! n (+ n 1))
	     (set! *prefix* '())
	     (redraw-message-pane)
	     (set! comtab (if *help?* *help-comtab* *comtab*))
	     (send window 'expose)
	     (when (and (= n (if (eq? *display-pane* *window*)
				 1
				 (+ (length *buttons*)
				    (if *transcript-pane* 6 5))))
			*post-initialize-procedure*)
	      (*post-initialize-procedure*)
	      (set! *post-initialize-procedure* #f)))
	    ;; I don't know why these happen or what they mean.
	    ((= event-type noexpose) #f)
	    ((= event-type motionnotify) #f)
	    ((= event-type buttonpress)
	     (when *help?*
	      (set! *help?* #f)
	      (redraw-buttons)
	      (redraw-display-pane))
	     (set! *prefix* '())
	     (redraw-message-pane)
	     (set! comtab (if *help?* *help-comtab* *comtab*))
	     (let ((old-status *status*))
	      (xselectinput *display*
			    *window*
			    (bit-or exposuremask
				    buttonpressmask
				    buttonreleasemask
				    keypressmask))
	      (xselectinput *display*
			    *display-pane*
			    (bit-or exposuremask
				    buttonpressmask
				    buttonreleasemask
				    keypressmask))
	      (status "Run")
	      (call-with-current-continuation
	       (lambda (abort-continuation)
		(set! *abort-continuation* abort-continuation)
		;; changed
		(send window 'buttonpress
		      (xevent-xbutton-x event)
		      (xevent-xbutton-y event)
		      (xevent-xbutton-button event)
		      (xevent-xbutton-state event))))
	      (xselectinput *display*
			    *window*
			    (bit-or exposuremask
				    pointermotionmask
				    buttonpressmask
				    buttonreleasemask
				    keypressmask))
	      (xselectinput *display*
			    *display-pane*
			    (bit-or exposuremask
				    pointermotionmask
				    buttonpressmask
				    buttonreleasemask
				    keypressmask))
	      (status old-status))
	     (set! comtab (if *help?* *help-comtab* *comtab*)))
	    ((= event-type buttonrelease) #f)
	    ((= event-type keypress)
	     (cond
	      ((= (string-length (ylookupstring event)) 1)
	       (let ((character (string-ref (ylookupstring event) 0)))
		(unless (zero? (bit-and (xevent-xkey-state event) mod1mask))
		 (set! character (meta character)))
		(execute-key character)))
	      ((= (xlookupkeysym event 0) xk_home) (execute-key (meta #\<)))
	      ((= (xlookupkeysym event 0) xk_left) (execute-key (control #\b)))
	      ((= (xlookupkeysym event 0) xk_up) (execute-key (control #\p)))
	      ((= (xlookupkeysym event 0) xk_right)
	       (execute-key (control #\f)))
	      ((= (xlookupkeysym event 0) xk_down) (execute-key (control #\n)))
	      ((= (xlookupkeysym event 0) xk_prior) (execute-key (meta #\v)))
	      ((= (xlookupkeysym event 0) xk_next) (execute-key (control #\v)))
	      ((= (xlookupkeysym event 0) xk_end) (execute-key (meta #\>)))))
	    ;; changed
	    (else (panic (format #f "Unrecognized event: ~s" event-type))))
      (loop)))))))

(define (quit)
 (unwind-trail)
 (*quit-continuation* #f))

(define (abort)
 (xbell *display* 100)
 (set! *help?* #f)
 (redraw-buttons)
 (redraw-display-pane)
 (*abort-continuation* #f))

(define (control character)
 (if (>= (char->integer character) 128)
     (let ((character (integer->char (- (char->integer character) 128))))
      (cond ((char-alphabetic? character)
	     (if (char-lower-case? character)
		 (integer->char (+ (- (char->integer character) 96) 128))
		 (integer->char (+ (- (char->integer character) 64) 128))))
	    ((char=? character #\space) (integer->char (+ 128 0)))
	    ((char=? character #\@) (integer->char (+ 128 0)))
	    ((char=? character #\[) (integer->char (+ 128 27)))
	    ((char=? character #\\) (integer->char (+ 128 28)))
	    ((char=? character #\]) (integer->char (+ 128 29)))
	    ((char=? character #\^) (integer->char (+ 128 30)))
	    ((char=? character #\_) (integer->char (+ 128 31)))
	    ((char=? character #\/) (integer->char (+ 128 31)))
	    (else
	     (panic
	      ;; changed
	      (format #f "Can't form control character: ~s" character)))))
     (cond ((char-alphabetic? character)
	    (if (char-lower-case? character)
		(integer->char (- (char->integer character) 96))
		(integer->char (- (char->integer character) 64))))
	   ((char=? character #\space) (integer->char 0))
	   ((char=? character #\@) (integer->char 0))
	   ((char=? character #\[) (integer->char 27))
	   ((char=? character #\\) (integer->char 28))
	   ((char=? character #\]) (integer->char 29))
	   ((char=? character #\^) (integer->char 30))
	   ((char=? character #\_) (integer->char 31))
	   ((char=? character #\/) (integer->char 31))
	   (else
	    (panic
	     ;; changed
	     (format #f "Can't form control character: ~s" character))))))

(define (meta character)
 (if (>= (char->integer character) 128)
     character
     (integer->char (+ (char->integer character) 128))))

(define (define-key character documentation command)
 (set! *help*
       (cons (list character documentation)
	     (remove-if (lambda (help) (equal? character (first help)))
			*help*)))
 (when (eq? command abort-command) (set! *abort-key* character))
 (if (list? character)
     (let loop ((characters character) (comtab *comtab*))
      (cond
       ((null? (rest characters))
	(vector-set! comtab (char->integer (first characters)) command))
       (else (unless (vector? (vector-ref comtab
					  (char->integer (first characters))))
	      (vector-set! comtab
			   (char->integer (first characters))
			   (make-vector 256 #f)))
	     (loop (rest characters)
		   (vector-ref comtab (char->integer (first characters)))))))
     (vector-set! *comtab* (char->integer character) command)))

(define (define-button x y text-procedure bold?-procedure method)
 (let ((button (xcreatesimplewindow
		*display* *window* (+ (* x (+ *button-width* 4)) 2)
		(+ (* y (+ *button-height* 4)) 2)
		*button-width* *button-height* 1
		;; changed
		*black-pixel*
		;; changed
		*white-pixel*)))
  (when (eq? method abort-command) (set! *abort-button* button))
  (xselectinput
   *display* button (bit-or exposuremask buttonpressmask keypressmask))
  (set-window-method!
   button
   'expose
   (lambda ()
    (let* ((text (if (procedure? text-procedure)
		     (text-procedure)
		     text-procedure))
	   (bold? (if (procedure? bold?-procedure)
		      (bold?-procedure)
		      bold?-procedure))
	   (text-width
	    (xtextwidth
	     (if bold? *bold-font* *roman-font*) text (string-length text)))
	   (text-x (quotient (- *button-width* text-width) 2))
	   (text-y (- *button-height* (+ *text-baseline* 2))))
     (xclearwindow *display* button)
     (xdrawstring *display* button (if bold? *bold-gc* *roman-gc*)
		  text-x text-y text (string-length text)))))
  (set-window-method! button 'buttonpress (lambda (x y button state) (method)))
  (set! *buttons* (cons button *buttons*))))

;;; removed: DEFINE-BUTTON-TOGGLE

;;; removed: DEFINE-BUTTON-TUPPLE

;;; removed: DEFINE-BUTTON-CYCLE

;;; removed: DEFINE-BUTTON-INTEGER-RANGE

(define (say string)
 (set! *transcript* (cons (list 'system string) *transcript*))
 (redraw-transcript-pane))

(define (status string)
 (set! *status* string)
 (redraw-status-pane))

(define (message string)
 (set! *message* string)
 (redraw-message-pane))

(define (set-pause! p)
 (set! *pause?* p)
 (redraw-buttons))

(define (pause)
 (when *pause?*
  (let ((old-status *status*))
   (status "Pause")
   (call-with-current-continuation
    (lambda (continue)
     (let ((event (make-xevent)))
      (let loop ()
       (xnextevent *display* event)	;changed
       (let ((event-type (xevent-xany-type event))
	     (window (xevent-xany-window event)))
	(cond ((= event-type mappingnotify) (xrefreshkeyboardmapping event))
	      ((= event-type expose) (send window 'expose))
	      ;; I don't know why these happen or what they mean.
	      ((= event-type noexpose) #f)
	      ((= event-type motionnotify) #f)
	      ((= event-type buttonpress) (continue #f))
	      ((= event-type buttonrelease) #f)
	      ((= event-type keypress)
	       (when (= (string-length (ylookupstring event)) 1)
		(let ((character (string-ref (ylookupstring event) 0)))
		 (cond ((char=? character #\space) (continue #f))
		       ((char=? character #\r) (set-pause! #f) (continue #f))
		       ;; needs work: Should do by abort key and button.
		       ((char=? character #\q) (abort))))))
	      ;; changed
	      (else (panic (format #f "Unrecognized event: ~s" event))))
	(loop))))))
   (status old-status))))

(define (tracking-pointer twice? press? tracker)
 (let ((old-status *status*)
       (event (make-xevent))
       (x #f)				;needs work: type pollution
       (y #f))				;needs work: type pollution
  (xselectinput *display*
		*window*
		(bit-or exposuremask
			pointermotionmask
			buttonpressmask
			buttonreleasemask
			keypressmask))
  (xselectinput *display*
		*display-pane*
		(bit-or exposuremask
			pointermotionmask
			buttonpressmask
			buttonreleasemask
			keypressmask))
  (status "Track")
  ;; Get pointer position and call tracker to draw.
  (let ((result (yquerypointer *display* *display-pane*))) ;changed
   (set! x (first result))		;changed
   (set! y (second result)))		;changed
  (tracker x y)
  (xflush *display*)
  ;; Event loop
  (let loop ()
   ;; Call tracker to erase.
   (when twice? (tracker x y) (xflush *display*))
   ;; Get pointer position and call tracker to draw.
   (let ((result (yquerypointer *display* *display-pane*))) ;changed
    (set! x (first result))		;changed
    (set! y (second result)))		;changed
   (tracker x y)
   (xflush *display*)
   ;; Wait for next event.
   (xnextevent *display* event)		;changed
   ;; needs work: Should allow abort key and button.
   (let ((event-type (xevent-xany-type event)))
    (cond
     ;; If event is MotionNotify, gobble all following MotionNotify events.
     ((= event-type motionnotify)
      (let loop2 ()
       (when (> (xeventsqueued *display* queuedalready) 0)
	(xnextevent *display* event)	;changed
	;; needs work: Should allow abort key and button.
	(let ((event-type (xevent-xany-type event)))
	 (if (= event-type motionnotify)
	     (loop2)
	     (xputbackevent *display* event)))))
      (loop))
     ;; Ignore other events except ButtonPress/Release, which exits main loop.
     ((not (= event-type (if press? buttonpress buttonrelease))) (loop)))))
  ;; Call tracker to erase; restore status; return final pointer position.
  (when twice? (tracker x y) (xflush *display*))
  (status old-status)
  (list x y)))

;;; changed
(define (kill-application) #f)

;;; removed: SET-KILL-APPLICATION!

(define (allocate-color-cube! reds greens blues)
 (unless *color-cube*
  (set! *color-cube* (make-vector (* reds greens blues)))
  (let ((colormap (xdefaultcolormap *display* *screen*))
	(i 0))
   (for-each-n
    (lambda (red)
     (for-each-n
      (lambda (green)
       (for-each-n
	(lambda (blue)
	 (let ((xcolor (make-xcolor)))
	  ;; changed
	  (set-xcolor-red! xcolor (quotient (* 65536 red) reds))
	  ;; changed
	  (set-xcolor-green! xcolor (quotient (* 65536 green) greens))
	  ;; changed
	  (set-xcolor-blue! xcolor (quotient (* 65536 blue) blues))
	  (when (zero? (xalloccolor *display* colormap xcolor))
	   (panic "Cannot allocate sufficient colors"))
	  (vector-set! *color-cube* i xcolor)
	  (set! i (+ i 1))))
	blues))
      greens))
    reds))))

(define (draw-pixmap pixmap x y)
 (let ((geometry (xgetgeometry *display* pixmap)))
  (xcopyarea *display* pixmap *display-pane* *color-gc*
	     0 0 (fifth geometry) (sixth geometry) x y)
  (xflush *display*)))

(define *frame-rate* 30.0)

(define (draw-pixmaps pixmaps x y)
 (for-each-vector
  (lambda (pixmap)
   (draw-pixmap pixmap x y)
   (usleep (inexact->exact (round (/ 1000000.0 *frame-rate*)))))
  pixmaps))

(define (free-pixmap pixmap) (xfreepixmap *display* pixmap))

(define (free-pixmaps pixmaps) (for-each-vector free-pixmap pixmaps))

(define c-xcreateimage
 ;; changed
 (foreign-procedure
  (void* void* int int int void* int int int int) void*
  "XCreateImage" "X11/Xlib"))

(define c-malloc (foreign-procedure (int) void* "malloc" "stdlib")) ;changed

(define c-docolordither
 ;; changed
 (foreign-procedure
  (void* int int void* void* void* void* int) void* "DoColorDither"))

(define (pnm->pixmap pnm)
 ;; needs work: Why don't I get exact RGB values?
 ;; needs work: Make special PGM version.
 (cond
  ((pbm? pnm) (pnm->pixmap (pbm->ppm pnm)))
  ((pgm? pnm) (pnm->pixmap (pgm->ppm pnm)))
  ((ppm? pnm)
   (let* ((default-visual (xdefaultvisual *display* *screen*))
	  (default-visual-class (visual-class default-visual))
	  (default-depth (xdefaultdepth *display* *screen*)))
    (cond
     ((= default-visual-class staticgray)
      (panic "Cannot (yet) handle STATICGRAY visual"))
     ((= default-visual-class grayscale)
      (panic "Cannot (yet) handle GRAYSCALE visual"))
     ((= default-visual-class staticcolor)
      (panic "Cannot (yet) handle STATICCOLOR visual"))
     ((= default-visual-class pseudocolor)
      (unless (and (= (visual-map_entries default-visual) 256)
		   (= default-depth 8))
       (panic "Can (currently) only handle 8-bit PSEUDOCOLOR visual"))
      (allocate-color-cube! *reds* *greens* *blues*)
      (if *dither?*
	  (let* ((width (pnm-width pnm))
		 (height (pnm-height pnm))
		 (red (ppm-red pnm))
		 (green (ppm-green pnm))
		 (blue (ppm-blue pnm))
		 (maxval (ppm-maxval pnm))
		 (pic24 (make-string (* 3 width height)))
		 (rdisp (make-string (vector-length *color-cube*)))
		 (gdisp (make-string (vector-length *color-cube*)))
		 (bdisp (make-string (vector-length *color-cube*)))
		 (idisp (make-string (vector-length *color-cube*)))
		 (i 0))
	   (do ((y 0 (+ y 1))) ((= y height))
	    (do ((x 0 (+ x 1))) ((= x width))
	     (string-set!
	      pic24 i
	      (integer->char (quotient (* 255 (matrix-ref red y x)) maxval)))
	     (set! i (+ i 1))
	     (string-set!
	      pic24 i
	      (integer->char
	       (quotient (* 255 (matrix-ref green y x)) maxval)))
	     (set! i (+ i 1))
	     (string-set!
	      pic24 i
	      (integer->char (quotient (* 255 (matrix-ref blue y x)) maxval)))
	     (set! i (+ i 1))))
	   (do ((i 0 (+ i 1))) ((= i (vector-length *color-cube*)))
	    (let ((xcolor (vector-ref *color-cube* i)))
	     (string-set!
	      rdisp i (integer->char (quotient (xcolor-red xcolor) 256)))
	     (string-set!
	      gdisp i (integer->char (quotient (xcolor-green xcolor) 256)))
	     (string-set!
	      bdisp i (integer->char (quotient (xcolor-blue xcolor) 256)))
	     (string-set! idisp i (integer->char (xcolor-pixel xcolor)))))
	   (let* ((pic8
		   (c-docolordither
		    pic24 width height
		    rdisp gdisp bdisp idisp (vector-length *color-cube*)))
		  (ximage
		   (cons 'ximagep
			 (c-xcreateimage
			  (cdr *display*) (cdr default-visual) 8 zpixmap 0
			  pic8 width height 8 0)))
		  (pixmap
		   (xcreatepixmap *display* *display-pane* width height 8)))
	    (xputimage
	     *display* pixmap *color-gc* ximage 0 0 0 0 width height)
	    (xdestroyimage ximage)
	    pixmap))
	  (let* ((width (pnm-width pnm))
		 (height (pnm-height pnm))
		 (red (ppm-red pnm))
		 (green (ppm-green pnm))
		 (blue (ppm-blue pnm))
		 (maxval (+ (ppm-maxval pnm) 1))
		 (data (c-malloc (* width height)))
		 (ximage
		  (cons 'ximagep
			(c-xcreateimage
			 (cdr *display*) (cdr default-visual) 8 zpixmap 0
			 data width height 8 0)))
		 (pixmap
		  (xcreatepixmap *display* *display-pane* width height 8)))
	   (do ((y 0 (+ y 1))) ((= y height))
	    (do ((x 0 (+ x 1))) ((= x width))
	     (xputpixel
	      ximage x y
	      (xcolor-pixel
	       (vector-ref
		*color-cube*
		(+ (* *greens*
		      *blues*
		      (min (inexact->exact
			    (round (/ (* *reds* (matrix-ref red y x)) maxval)))
			   (- *reds* 1)))
		   (* *blues*
		      (min
		       (inexact->exact
			(round (/ (* *greens* (matrix-ref green y x)) maxval)))
		       (- *greens* 1)))
		   (min (inexact->exact
			 (round (/ (* *blues* (matrix-ref blue y x)) maxval)))
			(- *blues* 1))))))))
	   (xputimage *display* pixmap *color-gc* ximage 0 0 0 0 width height)
	   (xdestroyimage ximage)
	   pixmap)))
     ((= default-visual-class truecolor)
      (cond
       ((and (= (visual-red_mask default-visual) 63488)
	     (= (visual-green_mask default-visual) 2016)
	     (= (visual-blue_mask default-visual) 31)
	     (= default-depth 16))
	(let* ((width (pnm-width pnm))
	       (height (pnm-height pnm))
	       (red (ppm-red pnm))
	       (green (ppm-green pnm))
	       (blue (ppm-blue pnm))
	       (maxval (+ (ppm-maxval pnm) 1))
	       (data (c-malloc (* 2 width height)))
	       (ximage
		(cons 'ximagep
		      (c-xcreateimage
		       (cdr *display*) (cdr default-visual) default-depth
		       zpixmap 0 data width height default-depth 0)))
	       (pixmap
		(xcreatepixmap
		 *display* *display-pane* width height default-depth)))
	 (do ((y 0 (+ y 1))) ((= y height))
	  (do ((x 0 (+ x 1))) ((= x width))
	   (xputpixel
	    ximage x y
	    (+ (* (quotient (* 32 (matrix-ref red y x)) maxval) 2048)
	       (* (quotient (* 64 (matrix-ref green y x)) maxval) 32)
	       (quotient (* 32 (matrix-ref blue y x)) maxval)))))
	 (xputimage *display* pixmap *color-gc* ximage 0 0 0 0 width height)
	 (xdestroyimage ximage)
	 pixmap))
       ((and (= (visual-red_mask default-visual) 16711680)
	     (= (visual-green_mask default-visual) 65280)
	     (= (visual-blue_mask default-visual) 255)
	     (or (= default-depth 24) (= default-depth 32)))
	(let* ((width (pnm-width pnm))
	       (height (pnm-height pnm))
	       (red (ppm-red pnm))
	       (green (ppm-green pnm))
	       (blue (ppm-blue pnm))
	       (maxval (+ (ppm-maxval pnm) 1))
	       (data (c-malloc (* 4 width height)))
	       (ximage
		(cons 'ximagep
		      (c-xcreateimage
		       (cdr *display*) (cdr default-visual) default-depth
		       zpixmap 0 data width height 32 0)))
	       (pixmap
		(xcreatepixmap
		 *display* *display-pane* width height default-depth)))
	 (do ((y 0 (+ y 1))) ((= y height))
	  (do ((x 0 (+ x 1))) ((= x width))
	   (xputpixel
	    ximage x y
	    (+ (* 65536 (quotient (* 256 (matrix-ref red y x)) maxval))
	       (* 256 (quotient (* 256 (matrix-ref green y x)) maxval))
	       (quotient (* 256 (matrix-ref blue y x)) maxval)))))
	 (xputimage *display* pixmap *color-gc* ximage 0 0 0 0 width height)
	 (xdestroyimage ximage)
	 pixmap))
       (else
	(panic "Can (currently) only handle 16-bit (5-6-5), 24-bit, and 32-bit TRUECOLOR visuals"))))
     ((= default-visual-class directcolor)
      (panic "Cannot (yet) handle DIRECTCOLOR visual"))
     (else (panic "Unrecognized visual")))))
  (else (panic "Argument not PNM"))))

(define (pnm-movie->pixmaps pnm-movie) (map-vector pnm->pixmap pnm-movie))

(define-structure tree-node width height offset text bold? procedure daughters)

(define (tree->tree-node tree)
 ;; needs work: This doesn't do any error checking.
 (let ((daughters (map tree->tree-node (rest (rest (rest tree)))))
       (bold? (first tree))
       (text (second tree))
       (procedure (third tree)))
  (make-tree-node
   (max (reduce + (map tree-node-width daughters) 0)
	(+ (xtextwidth
	    (if bold? *bold-font* *roman-font*) text (string-length text))
	   10))
   (if (null? daughters)
       *text-height*
       (+ (reduce max (map tree-node-height daughters) 0) *text-height* 20))
   (if (or (null? daughters)
	   (< (reduce + (map tree-node-width daughters) 0)
	      (+ (xtextwidth (if bold? *bold-font* *roman-font*)
			     text (string-length text))
		 10)))
       (quotient
	(+ (xtextwidth
	    (if bold? *bold-font* *roman-font*) text (string-length text))
	   10)
	2)
       (+ (quotient
	   (- (+ (reduce + (map tree-node-width (rest (reverse daughters))) 0)
		 (tree-node-offset (last daughters)))
	      (tree-node-offset (first daughters)))
	   2)
	  (tree-node-offset (first daughters))))
   text
   bold?
   procedure
   daughters)))

(define (draw-tree-node tree-node x y)
 (let* ((width (xtextwidth
		(if (tree-node-bold? tree-node) *bold-font* *roman-font*)
		(tree-node-text tree-node)
		(string-length (tree-node-text tree-node))))
	(x0 (- (+ x (tree-node-offset tree-node)) (quotient width 2))))
  (xdrawstring *display* *display-pane*
	       (if (tree-node-bold? tree-node) *bold-gc* *roman-gc*)
	       x0
	       (+ y (- *text-height* (+ *text-baseline* -3)))
	       (tree-node-text tree-node)
	       (string-length (tree-node-text tree-node)))
  (define-region x0 y width *text-height*
   (lambda (x1 y1)
    ((tree-node-procedure tree-node))
    (redraw-display-pane)))
  (let loop ((x1 (+ x (quotient
		       (- (tree-node-width tree-node)
			  (reduce + (map tree-node-width
					 (tree-node-daughters tree-node)) 0))
		       2)))
	     (daughters (tree-node-daughters tree-node)))
   (unless (null? daughters)
    (xdrawline *display* *display-pane* *thin-gc*
	       (+ x (tree-node-offset tree-node))
	       (+ y *text-height* 2)
	       (+ x1 (tree-node-offset (first daughters)))
	       (+ y *text-height* 21))
    (draw-tree-node (first daughters) x1 (+ y *text-height* 20))
    (loop (+ x1 (tree-node-width (first daughters))) (rest daughters))))))

(define (tree-height tree) (tree-node-height (tree->tree-node tree)))

(define (draw-tree tree x y) (draw-tree-node (tree->tree-node tree) x y))

;;; needs work: To add bold face and regions to alist display.

(define-structure alist-node width height offset keys values)

(define (alist->alist-node alist)
 ;; needs work: This doesn't do any error checking.
 (if (string? alist)
     alist
     (let* ((keys (map car alist))
	    (values (map alist->alist-node (map cdr alist)))
	    (offset
	     (reduce
	      max
	      (map
	       (lambda (key) (xtextwidth *roman-font* key (string-length key)))
	       keys)
	      0)))
      (make-alist-node
       (+ 5
	  offset
	  5
	  (reduce
	   max
	   (map
	    (lambda (value)
	     (if (string? value)
		 (xtextwidth *roman-font* value (string-length value))
		 (alist-node-width value)))
	    values)
	   0)
	  5)
       (+ 1
	  (max (- *text-height* 6)
	       (reduce
		+
		(map (lambda (value)
		      (max *text-height*
			   (if (string? value)
			       *text-height*
			       (alist-node-height value))))
		     values)
		0))
	  (* 2 (max (- (length keys) 1) 0))
	  5)
       (+ offset 10)
       keys
       values))))

(define (draw-alist-node alist-node x y)
 (cond
  ((string? alist-node)
   (xdrawstring *display* *display-pane* *roman-gc*
		x
		(+ y (- *roman-height* (+ *roman-baseline* -3)))
		alist-node (string-length alist-node)))
  (else
   (xdrawline *display* *display-pane* *thin-gc* x y (+ x 5) y)
   (xdrawline *display* *display-pane* *thin-gc*
	      x y x (+ y (alist-node-height alist-node)))
   (xdrawline *display* *display-pane* *thin-gc*
	      x (+ y (alist-node-height alist-node))
	      (+ x 5) (+ y (alist-node-height alist-node)))
   (xdrawline *display* *display-pane* *thin-gc*
	      (+ x (alist-node-width alist-node)) y
	      (+ x (alist-node-width alist-node) -5) y)
   (xdrawline *display* *display-pane* *thin-gc*
	      (+ x (alist-node-width alist-node))
	      y
	      (+ x (alist-node-width alist-node))
	      (+ y (alist-node-height alist-node)))
   (xdrawline *display* *display-pane* *thin-gc*
	      (+ x (alist-node-width alist-node))
	      (+ y (alist-node-height alist-node))
	      (+ x (alist-node-width alist-node) -5)
	      (+ y (alist-node-height alist-node)))
   (let loop ((keys (alist-node-keys alist-node))
	      (values (alist-node-values alist-node))
	      (y (+ y 1)))
    (unless (null? keys)
     (cond ((string? (first values))
	    (xdrawstring *display* *display-pane* *roman-gc*
			 (+ x 5)
			 (+ y (- *roman-height* (+ *roman-baseline* -3)))
			 (first keys) (string-length (first keys)))
	    (xdrawstring *display* *display-pane* *roman-gc*
			 (+ x (alist-node-offset alist-node))
			 (+ y (- *roman-height* (+ *roman-baseline* -3)))
			 (first values) (string-length (first values)))
	    (loop (rest keys) (rest values) (+ y *text-height* 2)))
	   (else (xdrawstring *display* *display-pane* *roman-gc*
			      (+ x 5)
			      (+ y (- *roman-height* (+ *roman-baseline* -3)))
			      (first keys) (string-length (first keys)))
		 (draw-alist-node
		  (first values) (+ x (alist-node-offset alist-node)) (+ y 2))
		 (loop (rest keys)
		       (rest values)
		       (+ y (alist-node-height (first values)) 2)))))))))

(define (alist-height alist) (alist-node-height (alist->alist-node alist)))

(define (draw-alist alist x y) (draw-alist-node (alist->alist-node alist) x y))

(define (abort-command) (abort))

(define (help-command)
 (set! *help?* #t)
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-up-line-command)
 (set! *help?* #t)
 (set! *first-help-line* (max (- *first-help-line* 1) 0))
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-down-line-command)
 (set! *help?* #t)
 (set! *first-help-line*
       (max (min (+ *first-help-line* 1)
		 (- (length *help*)
		    (quotient *display-pane-height* *text-height*)))
	    0))
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-up-page-command)
 (set! *help?* #t)
 (set! *first-help-line*
       (max
	(- *first-help-line* (quotient *display-pane-height* *text-height*))
	0))
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-down-page-command)
 (set! *help?* #t)
 (set! *first-help-line*
       (max (min (+ *first-help-line*
		    (quotient *display-pane-height* *text-height*))
		 (- (length *help*)
		    (quotient *display-pane-height* *text-height*)))
	    0))
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-beginning-command)
 (set! *help?* #t)
 (set! *first-help-line* 0)
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-end-command)
 (set! *help?* #t)
 (set! *first-help-line*
       (max (- (length *help*) (quotient *display-pane-height* *text-height*))
	    0))
 (redraw-buttons)
 (redraw-display-pane))

(define (echo-pane-command editor)
 ;; needs work: This is not really a command.
 (message "")
 (let ((result (editor *input* *input-position*)))
  (set! *input* (first result))
  (set! *input-position* (second result)))
 (redraw-echo-pane))

(define (echo-pane-insert-character-command character)
 (echo-pane-command (string-insert-character character)))

(define (echo-pane-beginning-of-line-command)
 (echo-pane-command string-beginning-of-line))

(define (echo-pane-backward-char-command)
 (echo-pane-command string-backward-char))

(define (echo-pane-delete-char-command)
 (echo-pane-command string-delete-char))

(define (echo-pane-end-of-line-command)
 (echo-pane-command string-end-of-line))

(define (echo-pane-forward-char-command)
 (echo-pane-command string-forward-char))

(define (echo-pane-kill-line-command)
 (echo-pane-command string-kill-line))

(define (echo-pane-backward-delete-char-command)
 (echo-pane-command string-backward-delete-char))

(define (echo-pane-backward-word-command)
 (echo-pane-command string-backward-word))

(define (echo-pane-kill-word-command)
 (echo-pane-command string-kill-word))

(define (echo-pane-forward-word-command)
 (echo-pane-command string-forward-word))

(define (echo-pane-backward-kill-word-command)
 (echo-pane-command string-backward-kill-word))

;;; removed: DEFINE-APPLICATION

;;; Debugger

;;; removed: Debugger

;;; Command Processor

;;; removed: DEFINE-COMMAND

;;; Tam V'Nishlam Shevah L'El Borei Olam
