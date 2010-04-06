;;; File   : sort.scm
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;; Updated: 11 June 1991
;;; Defines: sorted?, merge, merge!, sort, sort!

;;; --------------------------------------------------------------------
;;; Many Scheme systems provide some kind of sorting functions.  They do
;;; not, however, always provide the _same_ sorting functions, and those
;;; that I have had the opportunity to test provided inefficient ones (a
;;; common blunder is to use quicksort which does not perform well).
;;; Because sort and sort! are not in the standard, there is very little
;;; agreement about what these functions look like.  For example, Dybvig
;;; says that Chez Scheme provides
;;;     (merge predicate list1 list2)
;;;     (merge! predicate list1 list2)
;;;     (sort predicate list)
;;;     (sort! predicate list),
;;; while the MIT Scheme 7.1 manual, following Common Lisp, offers
;;;     (sort list predicate),
;;; TI PC Scheme offers
;;;     (sort! list/vector predicate?)
;;; and Elk offers
;;;     (sort list/vector predicate?)
;;;     (sort! list/vector predicate?)
;;; Here is a comprehensive catalogue of the variations I have found.
;;; (1) Both sort and sort! may be provided.
;;; (2) sort may be provided without sort!
;;; (3) sort! may be provided without sort
;;; (4) Neither may be provided
;;; ---
;;; (5) The sequence argument may be either a list or a vector.
;;; (6) The sequence argument may only be a list.
;;; (7) The sequence argument may only be a vector.
;;; ---
;;; (8) The comparison function may be expected to behave like <
;;; (9) or it may be expected to behave like <=
;;; ---
;;; (10) The interface may be (sort predicate? sequence)
;;; (11) or (sort sequence predicate?)
;;; (12) or (sort sequence &optional (predicate? <))
;;; ---
;;; (13) The sort may be stable
;;; (14) or it may be unstable.
;;; ---
;;; All of this variation really does not help anybody.  A nice simple
;;; merge sort is both stable and fast (quite a lot faster than `quick'
;;; sort).
;;; I am providing this source code with no restrictions at all on its
;;; use (but please retain D.H.D.Warren's credit for the original idea).
;;; You may have to rename some of these functions in order to use them
;;; in a system which already provides incompatible or inferior sorts.
;;; For each of the functions, only the top-level define needs to be
;;; edited to do that.
;;; I could have given these functions names which would not clash with
;;; any Scheme that I know of, but I would like to encourage implementors
;;; to converge on a single interface, and this may serve as a hint.
;;; The argument order for all functions has been chosen to be as close
;;; to Common Lisp as made sense, in order to avoid NIH-itis.

;;; Each of the five functions has a required *last* parameter which is
;;; a comparison function.  A comparison function f is a function of 2
;;; arguments which acts like <.  For example,
;;;     (not (f x x))
;;;     (and (f x y) (f y z)) => (f x z)
;;; The standard functions <, >, char<?, char>?, char-ci<?, char-ci>?,
;;; string<?, string>?, string-ci<?, and string-ci>? are suitable for
;;; use as comparison functions.  Think of (less? x y) as saying when
;;; x must *not* precede y.

;;; (sorted? sequence less?)
;;;     returns #t when the sequence argument is in non-decreasing order
;;;     according to less? (that is, there is no adjacent pair ... x y ...
;;;     for which (less? y x))
;;;     returns #f when the sequence contains at least one out-of-order pair.
;;;     It is an error if the sequence is neither a list nor a vector.

;;; (ok:merge list1 list2 less?)
;;;     This merges two lists, producing a completely new list as result.
;;;     I gave serious consideration to producing a Common-Lisp-compatible
;;;     version.  However, Common Lisp's `sort' is our `sort!' (well, in
;;;     fact Common Lisp's `stable-sort' is our `sort!', merge sort is
;;;     *fast* as well as stable!) so adapting CL code to Scheme takes a
;;;     bit of work anyway.  I did, however, appeal to CL to determine
;;;     the *order* of the arguments.

;;; (ok:merge! list1 list2 less?)
;;;     merges two lists, re-using the pairs of list1 and list2 to build
;;;     the result.  If the code is compiled, and less? constructs no new
;;;     pairs, no pairs at all will be allocated.  The first pair of the
;;;     result will be either the first pair of list1 or the first pair
;;;     of list2, but you can't predict which.

;;;     The code of merge and merge! could have been quite a bit simpler,
;;;     but they have been coded to reduce the amount of work done per
;;;     iteration.  (For example, we only have one null? test per iteration.)

;;; (ok:sort sequence less?)
;;;     accepts either a list or a vector, and returns a new sequence which
;;;     is sorted.  The new sequence is the same type as the input.  Always
;;;     (sorted? (ok:sort sequence less?) less?).
;;;     The original sequence is not altered in any way.  The new sequence
;;;     shares its _elements_ with the old one; no elements are copied.

;;; (ok:sort! sequence less?)
;;;     returns its sorted result in the original boxes.  If the original
;;;     sequence is a list, no new storage is allocated at all.  If the
;;;     original sequence is a vector, the sorted elements are put back
;;;     in the same vector.

;;; Note that these functions do NOT accept a CL-style ":key" argument.
;;; A simple device for obtaining the same expressiveness is to define
;;; (define (keyed less? key) (lambda (x y) (less? (key x) (key y))))
;;; and then, when you would have written
;;;     (ok:sort a-sequence #'my-less :key #'my-key)
;;; in Common Lisp, just write
;;;     (ok:sort! a-sequence (keyed my-less? my-key))
;;; in Scheme.
;;; --------------------------------------------------------------------

;;; (sorted? sequence less?)
;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;;; such that for all 1 <= i <= m,
;;;     (not (less? (list-ref list i) (list-ref list (- i 1)))).

(define (sorted? seq less?)
 (cond ((null? seq) #t)
       ((vector? seq)
	(let ((n (vector-length seq)))
	 (if (<= n 1)
	     #t
	     (do ((i 1 (+ i 1)))
	       ((or (= i n)
		    (less? (vector-ref seq (- i 1))
			   (vector-ref seq i)))
		(= i n))))))
       (else (let loop ((last (car seq)) (next (cdr seq)))
	      (or (null? next)
		  (and (not (less? (car next) last))
		       (loop (car next) (cdr next))))))))

;;; (ok:merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (ok:merge a b less?) less?).
;;; Note:  this does _not_ accept vectors.  See below.

(define (ok:merge a b less?)
 (cond ((null? a) b)
       ((null? b) a)
       (else (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
	      ;; The loop handles the merging of non-empty lists.  It has
	      ;; been written this way to save testing and car/cdring.
	      (if (less? y x)
		  (if (null? b)
		      (cons y (cons x a))
		      (cons y (loop x a (car b) (cdr b))))
		  ;; x <= y
		  (if (null? a)
		      (cons x (cons y b))
		      (cons x (loop (car a) (cdr a) y b))))))))

;;; (ok:merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (ok:merge! a b less?)
 (define (loop r a b)
  (if (less? (car b) (car a))
      (begin (set-cdr! r b)
	     (if (null? (cdr b))
		 (set-cdr! b a)
		 (loop b a (cdr b))))
      ;; (car a) <= (car b)
      (begin (set-cdr! r a)
	     (if (null? (cdr a))
		 (set-cdr! a b)
		 (loop a (cdr a) b)))))
 (cond ((null? a) b)
       ((null? b) a)
       ((less? (car b) (car a))
	(if (null? (cdr b))
	    (set-cdr! b a)
	    (loop b a (cdr b)))
	b)
       (else				; (car a) <= (car b)
	(if (null? (cdr a))
	    (set-cdr! a b)
	    (loop a (cdr a) b))
	a)))

;;; (ok:sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (ok:sort! seq less?)
 (define (step n)
  (cond ((> n 2)
	 (let* ((j (quotient n 2))
		(a (step j))
		(k (- n j))
		(b (step k)))
	  (ok:merge! a b less?)))
	((= n 2)
	 (let ((x (car seq))
	       (y (cadr seq))
	       (p seq))
	  (set! seq (cddr seq))
	  (if (less? y x)
	      (begin (set-car! p y)
		     (set-car! (cdr p) x)))
	  (set-cdr! (cdr p) '())
	  p))
	((= n 1)
	 (let ((p seq))
	  (set! seq (cdr seq))
	  (set-cdr! p '())
	  p))
	(else
	 '())))
 (if (vector? seq)
     (let ((n (vector-length seq))
	   (vector seq))		; save original vector
      (set! seq (vector->list seq))	; convert to list
      (do ((p (step n) (cdr p))		; sort list destructively
	   (i 0 (+ i 1)))		; and store elements back
	((null? p) vector)              ; in original vector
       (vector-set! vector i (car p))))
     ;; otherwise, assume it is a list
     (step (length seq))))

;;; (ok:sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence.  My understanding is that the Standard says
;;; that the result of append is always "newly allocated" except for
;;; sharing structure with "the last argument", so (append x '()) ought
;;; to be a standard way of copying a list x.

(define (ok:sort seq less?)
 (if (vector? seq)
     (list->vector (ok:sort! (vector->list seq) less?))
     (ok:sort! (append seq '()) less?)))

;;; I want to see how well Stalin does with sort.
;;; The test is to form a list of the numbers I.0/J.0 for 1 <= I,J <= 100
;;; (10 000 floating point numbers), and N times do (sort and check result).

(define (make-test-list L U)
 (let ((R '()))
  (do ((I L (+ 1 I)))
    ((> I U) R)
   (do ((N (exact->inexact I) N)
	(J L (+ J 1)))
     ((> J U) "")
    (set! R (cons (/ N (exact->inexact J)) R))))))

(define (run-once L)
 (if (not (sorted? (ok:sort L <) <))
     (write "oops"))
 #f)

(define (run-N-times N L)
 (do ((I 0 (+ 1 I)))
   ((>= I N) #f)
  (run-once L)))

;; Timing test
(run-N-times 1000 (make-test-list 1 100))

;;; Compile:     'stalin -O2 -d -On sortim'
;;; Run          'time ./sortim'                twice
;;;  33u + 0s seconds
;;;  34u + 0s seconds

;;; Compile:	'stalin -Ot -O2 -d -On sortim'
;;; Run		'time ./sortim'			twice
;;;  19u + 0s seconds
;;;  19u + 0s seconds

;;; Compile	'gcc -DQSORT -o sortq -O2 sortq.c'
;;; Run		'time ./sortq'			twice
;;; 132u + 0s seconds
;;; 131u + 0s seconds.
;;; That wasn't a fair comparison, because qsort() is bad.

;;; Compile	'gcc -DMSORT -o sortq -O2 sortq.c'
;;; Run		'time ./sortq'			twice
;;;  38u + 0s seconds
;;;  38u + 0s seconds
;;; This is an array-based 'straight' top down merge sort.

;;; Notes.
;;;  1. I note that the format for a list of double is struct X* where
;;;	struct X { double car; struct X *cdr; };
;;;	On a VAX it was known to pay off to use the opposite order,
;;;	struct X { struct X *cdr; double car; };
;;;	This also paid off on the NS32k, MC68020, and I think the 80386.
;;;	It would be interesting to have an option for choosing the order.
;;;  2.	I don't understand why -Ot made such a big difference.  I really
;;;	hoped that this would be an example Stalin would find easy to
;;;	type check.
;;;  3.	It's a very pleasant surprise that the Scheme code munching away
;;;	on lists ended up going faster than comparable C code hacking arrays!
;;;  4. If I compile the generated code with 'gcc -O6' the time goes down
;;;	from 19 seconds to 18 seconds, but Stalin will not let me say -O6.
;;;  5. For comparison, I tried to use 'cc -xO4', but it choked on
;;;    {
;;;	struct heap {
;;;	    struct heap0 *heap;
;;;	    char data[heap_size];	/* heap_size is a variable */
;;;	};
;;;	heap = (struct heap0 *)malloc(sizeof(struct heap));
;;;    }
;;;	Is it really necessary to use that?  The generated code is pretty
;;;	unreadable anyway, and this construction doesn't _do_ anything that
;;;    {
;;;	heap = (struct heap0 *)malloc(sizeof (struct heap0 *) + heap_size);
;;;    }
;;;	wouldn't do.  I know the 'struct hack' has been disowned by X3J11,
;;;	despite being in the rationale, but it is much closer to being
;;;   	generally available than runtime sizeof.
