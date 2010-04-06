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

(include "QobiScheme")

(define (parallel-map proc list1 . lists)
 ;; note: Support for multiple arguments incurs a penalty here.
 ;; needs work: To make parallel.
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

(define (parallel-for-each proc list1 . lists)
 ;; note: Support for multiple arguments incurs a penalty here.
 (cond ((null? lists)
	;; note: This special-cases the one-argument case for speed.
	(let loop ((list1 list1))
	 (if (not (null? list1))
	     (parallel-begin (proc (car list1)) (loop (cdr list1))))))
       ((null? (cdr lists))
	;; note: This special-cases the two-argument case for speed.
	(let loop ((list1 list1) (list2 (car lists)))
	 (if (not (null? list1))
	     (parallel-begin (proc (car list1) (car list2))
			     (loop (cdr list1) (cdr list2))))))
       (else
	(let loop ((list1 list1) (lists lists))
	 (if (not (null? list1))
	     (parallel-begin
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

(define (parallel-reduce f l i)
 ;; needs work: To make parallel.
 (cond ((null? l) i)
       ((null? (rest l)) (first l))
       (else (let loop ((l (rest l)) (c (first l)))
	      (if (null? l) c (loop (rest l) (f c (first l))))))))

(define (parallel-reduce-n f n i)
 ;; needs work: To make parallel.
 (let loop ((i 0) (c i)) (if (>= i n) c (loop (+ i 1) (f c i)))))

(define (parallel-reduce-vector f v i)
 ;; needs work: To make parallel.
 (let ((n (vector-length v)))
  (cond ((zero? n) i)
	((= n 1) (vector-ref v 0))
	(else (let loop ((i 1) (c (vector-ref v 0)))
	       (if (= i n) c (loop (+ i 1) (f c (vector-ref v i)))))))))

(define (parallel-sum f n)
 ;; needs work: To make parallel.
 (let loop ((n (- n 1)) (c 0))
  (if (negative? n) c (loop (- n 1) (+ c (f n))))))

(define (parallel-product f n)
 ;; needs work: To make parallel.
 (let loop ((n (- n 1)) (c 1))
  (if (negative? n) c (loop (- n 1) (* c (f n))))))

(define (parallel-some p l . &rest)
 ;; needs work: To make parallel.
 (let loop ((l l) (&rest &rest))
  (and (not (null? l))
       (or (apply p (first l) (map first &rest))
	   (loop (rest l) (map rest &rest))))))

(define (parallel-some-n p n)
 ;; needs work: To make parallel.
 (let loop ((i 0)) (and (< i n) (or (p i) (loop (+ i 1))))))

(define (parallel-some-vector p v . &rest)
 ;; needs work: To make parallel.
 (let loop ((i 0))
  (and (< i (vector-length v))
       (or (apply p
		  (vector-ref v i)
		  (map (lambda (v) (vector-ref v i)) &rest))
	   (loop (+ i 1))))))

(define (parallel-every p l . &rest)
 ;; needs work: To make parallel.
 (let loop ((l l) (&rest &rest))
  (or (null? l)
      (and (apply p (first l) (map first &rest))
	   (loop (rest l) (map rest &rest))))))

(define (parallel-every-n p n)
 ;; needs work: To make parallel.
 (let loop ((i 0)) (or (>= i n) (and (p i) (loop (+ i 1))))))

(define (parallel-every-vector p v . &rest)
 ;; needs work: To make parallel.
 (let loop ((i 0))
  (or (>= i (vector-length v))
      (and (apply p
		  (vector-ref v i)
		  (map (lambda (v) (vector-ref v i)) &rest))
	   (loop (+ i 1))))))

(define (parallel-one p l . &rest)
 ;; needs work: To make parallel.
 (let loop ((l l) (&rest &rest))
  (and (not (null? l))
       (if (apply p (first l) (map first &rest))
	   (let loop ((l (rest l)) (&rest (map rest &rest)))
	    (or (null? l)
		(and (not (apply p (first l) (map first &rest)))
		     (loop (rest l) (map rest &rest)))))
	   (loop (rest l) (map rest &rest))))))

(define (parallel-one-n p n)
 ;; needs work: To make parallel.
 (let loop ((i 0))
  (and (< i n)
       (if (p i)
	   (let loop ((i (+ i 1)))
	    (or (>= i n) (and (not (p i)) (loop (+ i 1)))))
	   (loop (+ i 1))))))

(define (parallel-one-vector p v . &rest)
 ;; needs work: To make parallel.
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

(define (parallel-for-each-n f n)
 (let loop ((i 0)) (when (< i n) (parallel-begin (f i) (loop (+ i 1))))))

(define (parallel-for-each-from-a-up-to-b f a b)
 (let loop ((i a)) (when (< i b) (parallel-begin (f i) (loop (+ i 1))))))

(define (parallel-for-each-n-decreasing f n)
 (when (> n 0)
  (let ((i (- n 1))) (parallel-begin (f i) (for-each-n-decreasing f i)))))

(define (parallel-for-each-vector f v . &rest)
 (parallel-for-each-n
  (lambda (i)
   (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest)))
  (vector-length v)))

(define (parallel-map-n f n)
 ;; needs work: To eliminate REVERSE.
 ;; needs work: To make parallel.
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (parallel-map-vector f v . &rest)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((u (make-vector (vector-length v))))
  (parallel-for-each-n
   (lambda (i)
    (vector-set!
     u i
     (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest))))
   (vector-length v))
  u))

(define (parallel-map-n-vector f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (when (< i n) (parallel-begin (vector-set! v i (f i)) (loop (+ i 1)))))
  v))

(define (parallel-memp p x l)
 ;; needs work: To make parallel.
 (cond ((null? l) #f) ((p x (first l)) l) (else (memp p x (rest l)))))

(define (parallel-assp p x alist)
 ;; needs work: To make parallel.
 (and (not (null? alist))
      (if (p x (car (first alist))) (first alist) (assp p x (rest alist)))))

(define (parallel-pairwise? p l)
 ;; needs work: To make parallel.
 (or (null? l)
     (let loop ((l1 l) (l2 (rest l)))
      ;; needs work: To make tail recursive.
      (or (null? l2)
	  (and (p (first l1) (first l2)) (loop (rest l1) (rest l2)))))))

(define (parallel-adjoinp p x l) (if (parallel-memp p x l) l (cons x l)))

(define (parallel-removep p x l)
 ;; needs work: To eliminate REVERSE.
 ;; needs work: To make parallel.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (parallel-remove-if p l)
 ;; needs work: To eliminate REVERSE.
 ;; needs work: To make parallel.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (parallel-remove-if-not p l)
 ;; needs work: To eliminate REVERSE.
 ;; needs work: To make parallel.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (parallel-positionp p x l)
 ;; needs work: To make parallel.
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (parallel-position-if p l)
 ;; needs work: To make parallel.
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (parallel-position-if-not p l)
 ;; needs work: To make parallel.
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p (first l)) (loop (rest l) (+ i 1)))
	(else i))))

(define (parallel-findp p x l)
 ;; needs work: To make parallel.
 (let loop ((l l))
  (cond ((null? l) #f)
	((p x (first l)) (first l))
	(else (loop (rest l))))))

(define (parallel-find-if p l)
 ;; needs work: To make parallel.
 (let loop ((l l))
  (cond ((null? l) #f)
	((p (first l)) (first l))
	(else (loop (rest l))))))

(define (parallel-find-if-not p l)
 ;; needs work: To make parallel.
 (let loop ((l l))
  (cond ((null? l) #f)
	((p (first l)) (loop (rest l)))
	(else (first l)))))

(define (parallel-countp p x l)
 ;; needs work: To make parallel.
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (parallel-count-if p l)
 ;; needs work: To make parallel.
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (parallel-count-if-not p l)
 ;; needs work: To make parallel.
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p (first l)) (loop (rest l) c))
	(else (loop (rest l) (+ c 1))))))

(define (parallel-subsetp? p x y)
 (parallel-every (lambda (xe) (parallel-memp p xe y)) x))

(define (parallel-set-equalp? p x y)
 (and (parallel-subsetp? p x y) (parallel-subsetp? p y x)))

(define (parallel-unionp p x y)
 ;; needs work: To eliminate REVERSE.
 ;; needs work: To make parallel.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memp p (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (parallel-intersectionp p x y)
 ;; needs work: To eliminate REVERSE.
 ;; needs work: To make parallel.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memp p (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (parallel-set-differencep p x y)
 ;; needs work: To eliminate REVERSE.
 ;; needs work: To make parallel.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memp p (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (parallel-remove-duplicatesp p x)
 ;; needs work: To eliminate REVERSE.
 ;; needs work: To make parallel.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memp p (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (parallel-transitive-equivalence-classesp p x)
 ;; needs work: To make tail recursive.
 ;; needs work: To make parallel.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (transitive-equivalence-classesp p (rest x)))
	    (z (find-if (lambda (w) (p y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (parallel-equivalence-classesp p x)
 ;; This wrapper is necessary since P may not be transitive.
 ;; needs work: To make parallel.
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

(define (parallel-topological-sort p l)
 ;; needs work: To make parallel.
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

(define (parallel-sort list predicate key)
 (if (or (null? list) (null? (rest list)))
     list
     (parallel-call merge
		    (sort (every-other list) predicate key)
		    (sort (every-other (rest list)) predicate key)
		    predicate
		    key)))

(define (parallel-minp p l)
 ;; needs work: To make parallel.
 (when (null? l) (fuck-up))
 (let loop ((x (first l)) (l (rest l)))
  (if (null? l) x (loop (if (p x (first l)) x (first l)) (rest l)))))

(define (parallel-integrate f a b n)
 ;; The constants are hardwired to be inexact for efficiency.
 ;; needs work: To make parallel.
 (let ((delta (/ (- b a) n)))
  (let loop ((previous (f a)) (this (f (+ a delta))) (i 1) (s 0.0))
   (if (> i n)
       s
       (loop this
	     (f (+ a (* i delta)))
	     (+ i 1)
	     (+ s (* 0.5 (+ previous this) delta)))))))

(define (parallel-for-each-trie-entry p trie)
 (let loop ((trie-node (trie-trie-node trie)) (characters '()))
  (parallel-begin
   (p (list->string (reverse characters)) (trie-node-value trie-node))
   (parallel-for-each-n
    (lambda (i)
     (let ((trie-node (vector-ref (trie-node-table trie-node) i)))
      (when trie-node
       (loop trie-node (cons ((trie-integer->char trie) i) characters)))))
    (vector-length (trie-node-table trie-node))))))

(define (parallel-outer-product f u v)
 (parallel-map-vector
  (lambda (ui) (parallel-map-vector (lambda (vj) (f ui vj)) v)) u))

(define (parallel-self-outer-product f v) (parallel-outer-product f v v))

;;; Tam V'Nishlam Shevah L'El Borei Olam
