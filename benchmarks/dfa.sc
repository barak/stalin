;;; from s48-util.scm

(define call/cc call-with-current-continuation)

(define (box x)
 (cons 'box x))

(define (unbox x)
 (cdr x))

(define (set-box! x y)
 (set-cdr! x y))

(define (append! l1 l2)
 (append l1 l2))

(define (add-one x) (+ x 1))

(define (reverse! l)
 (cond ((or (null? l)
	    (null? (cdr l)))
	l)
       (else
	(let ((rest (cdr l)))
	 (set-cdr! l '())
	 (let loop ((l1 l) (l2 rest))
	  (cond ((null? l2)
		 l1)
		(else
		 (let ((rest (cdr l2)))
		  (set-cdr! l2 l1)
		  (loop l2 rest)))))))))

(define (pormat-function control-string . values)
 (let loop ((i 0) (values values))
  (if (not (= i (string-length control-string)))
      (let ((c (string-ref control-string i)))
       (if (eqv? c #\~)
	   (let ((c2 (string-ref control-string (+ 1 i))))
	    (cond ((eqv? c2 #\a)
		   (display (car values))
		   (loop (+ 2 i) (cdr values)))
		  ((eqv? c2 #\%)
		   (newline)
		   (loop (+ 2 i) values))
		  (else
		   (display c)
		   (loop (+ 1 i) values))))
	   (begin
	    (display c)
	    (loop (+ 1 i) values)))))))

(define scream pormat-function)

(define (sort pred l)
 (sort-list l pred))

(define (copy-vector from)
 (let* ((len (vector-length from))
	(to  (make-vector len 0)))
  (let loop ((i 0))
   (if (< i len)
       (begin (vector-set! to i (vector-ref from i))
	      (loop (+ i 1)))))
  to))

(define (make-list len val)
 (let loop ((out '()) (i 0))
  (if (< i len)
      (loop (cons val out) (+ i 1))
      out)))

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.

;;; This material was developed by the T Project at the Yale
;;; University Computer Science Department.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restric-tions and
;;; understandings.
;;; 1. Any copy made of this software must include this copyright
;;;    notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;; We gratefully acknowledge Bob Nix

;;;  SORT:ONLINE-MERGE-SORT!
;;;  =======================
;;;  On-Line Merge sort, a fast and stable algorithm for sorting a list.
;;;  This is a very neat algorithm!  Consider the following code:
;;;
;;;    (DEFINE (MERGE-SORT L)
;;;        (IF (NULL? (CDR L))
;;;            L
;;;            (MERGE (MERGE-SORT (FIRST-HALF-OF L))
;;;                   (MERGE-SORT (SECOND-HALF-OF L)))))
;;;
;;;  The nested calls to MERGE above form a binary tree, with MERGE's of
;;;  singleton lists at the leaves, and a MERGE of two lists of size N/2 at
;;;  the top.  The algorithm below traverses this MERGE-tree in post-order,
;;;  moving from the lower left hand corner to the right.
;;;
;;;  This algorithm sorts N objects with about NlgN+2N comparisons and exactly
;;;  lgN conses.  The algorithm used is a version of mergesort that is
;;;  amenable to Lisp's data accessing primitives.  The first phase of the
;;;  algorithm is an "addition" phase in which each element X is added to
;;;  a list of lists of sorted runs B in much the same manner as a one is
;;;  added to a binary number.  If the first "digit" of B is 0, i.e. the first
;;;  list in B is NIL, then the element to be added becomes the first digit
;;;  of B.  If that digit is non empty then you merge the digit with X and
;;;  recurse on the rest of B -- setting the first digit of B to be zero.
;;;  For example:
;;;
;;;   Reversed      LIST B
;;;   Binary #     Each sublist is sorted.
;;;
;;;    0000        ()
;;;    1000        ((x))
;;;    0100        (()  (x x))
;;;    1100        ((x) (x x))
;;;    0010        (()  ()    (x x x x))
;;;    1010        ((x) ()    (x x x x))
;;;    0110        (()  (x x) (x x x x))
;;;    1110        ((x) (x x) (x x x x))
;;;    0001        (()  ()    ()        (x x x x x x x x))
;;;    1001        ((x) ()    ()        (x x x x x x x x))
;;;
;;;  The algorithm then merges the sublists of these lists into
;;;  one list, and returns that list.
;;;
;;;  To see the algorithm in action, trace LIST-MERGE!.
;;;

;;; Returns list L sorted using OBJ-< for comparisons.

(define (sort-list l obj-<)
 (cond ((or (null? l)
	    (null? (cdr l)))
	l)
       (else
	(online-merge-sort! (append l '()) ; copy-list
			    obj-<))))

;;; Returns list L sorted using OBJ-< for comparisons.
;;; L is destructively altered.

(define (sort-list! l obj-<)
 (cond ((or (null? l)
	    (null? (cdr l)))
	l)
       (else
	(online-merge-sort! l obj-<))))

;;; The real sort procedure.  Elements of L are added to B, a list of sorted
;;; lists as defined above.  When all elements of L have been added to B
;;; the sublists of B are merged together to get the desired sorted list.

(define (online-merge-sort! l obj-<)
 (let ((b (cons '() '())))
  (let loop ((l l))
   (cond ((null? l)
	  (do ((c (cddr b) (cdr c))
	       (r (cadr b) (list-merge! (car c) r obj-<)))
	    ((null? c)
	     r)))
	 (else
	  (let ((new-l (cdr l)))
	   (set-cdr! l '())
	   (add-to-sorted-lists l b obj-<)
	   (loop new-l)))))))

;;; X is a list that is merged into B, the list of sorted lists.

(define (add-to-sorted-lists x b obj-<)
 (let loop ((x x) (b b))
  (let ((l (cdr b)))
   (cond ((null? l)
	  (set-cdr! b (cons x '())))
	 ((null? (car l))
	  (set-car! l x))
	 (else
	  (let ((y (list-merge! x (car l) obj-<)))
	   (set-car! l '())
	   (loop y l)))))))

;;; Does a stable side-effecting merge of L1 and L2.

(define (list-merge! l1 l2 obj-<)
 (cond ((null? l1) l2)
       ((null? l2) l1)
       ((obj-< (car l1) (car l2))
	(real-list-merge! l2 (cdr l1) obj-< l1)
	l1)
       (else
	(real-list-merge! l1 (cdr l2) obj-< l2)
	l2)))

;;; Does the real work of LIST-MERGE!.  L1 is assumed to be non-empty.

(define (real-list-merge! l1 l2 obj-< prev)
 (let loop ((a l1) (b l2) (prev prev))
  (cond ((null? b)
	 (set-cdr! prev a))
	((obj-< (car a) (car b))
	 (set-cdr! prev a)
	 (loop b (cdr a) a))
	(else
	 (set-cdr! prev b)
	 (loop a (cdr b) b)))))

;;; from learn.stack.s48

(define (second x) (cadr x))
(define (third  x) (caddr x))
(define (fourth x) (cadddr x))

(define (walk-list l k)
 (let loop ((l l))
  (if (not (null? l))
      (begin (k (car l))
	     (loop (cdr l))))))

(define (do-times-up n k)		; equivalent to (for-up 0 (- n 1) k)
 (let loop ((i 0))
  (if (< i n)
      (begin (k i)
	     (loop (+ i 1))))))

(define (read-file name)
 (let ((the-file (open-input-file name)))
  (let loop ((try (read the-file)) (accum '()))
   (if (eof-object? try)
       (begin
	(close-input-port the-file)
	(reverse accum))
       (loop (read the-file) (cons try accum))))))

(define (read-int file)
 (let ((val (read file)))
  (if (integer? val)
      val
      (scream "bogus value ~a" val))))

(define (read-cform-trainset name)
 (let* ((the-file (open-input-file name))
	(n-strings (read-int the-file))
	(new-alphabet-size (read-int the-file))
	(rb (revbuilder-new)))
  (set-box! alphabet-size new-alphabet-size)
  (do-times-up n-strings
	       (lambda (i)
		(let* ((label (read-int the-file))
		       (string-len (read-int the-file))
		       (rb2 (revbuilder-new)))
		 (do-times-up string-len
			      (lambda (j)
			       (revbuilder-add rb2 (read-int the-file))))
		 (revbuilder-add rb (cons label (revbuilder-done rb2))))))
  (revbuilder-done rb)))

(define (make-stack)
 (box '()))

(define (stack-empty? s)
 (null? (unbox s)))

(define (stack-push s obj)
 (set-box! s (cons obj (unbox s))))

(define (stack-pop s)
 (let ((l (unbox s)))
  (set-box! s (cdr l))
  (car l)))

(define (make-queue)
 (cons '() '()))

(define (queue-empty? q)
 (and (null? (car q))
      (null? (cdr q))))

(define (queue-push q obj)
 (set-car! q (cons obj (car q))))

(define (queue-pop q)
 (normalize-queue! q)
 (let ((head (car (cdr q))))
  (set-cdr! q (cdr (cdr q)))
  head))

(define (normalize-queue! q)
 (if (null? (cdr q))
     (begin (set-cdr! q (reverse (car q)))
	    (set-car! q '()))))

(define (queue-head q)
 (normalize-queue! q)
 (car (cdr q)))

(define (make-counter)
 (let ((count (box 0)))
  (lambda ()
   (let ((old-count (unbox count)))
    (set-box! count (+ 1 (unbox count)))
    old-count))))

(define (make-table) (vector 'table 0 1 (make-vector 1 '())))

(define (table-get-count    table) (vector-ref table 1))
(define (table-get-n-alists table) (vector-ref table 2))
(define (table-get-alists   table) (vector-ref table 3))
(define (table-set-count    table new) (vector-set! table 1 new))
(define (table-set-n-alists table new) (vector-set! table 2 new))
(define (table-set-alists   table new) (vector-set! table 3 new))

(define (table-test table key)
 (let* ((n-alists (table-get-n-alists table))
	(alists   (table-get-alists table))
	(hash (modulo key n-alists))
	(probe (assq key (vector-ref alists hash))))
  (if probe #t #f)))

(define (table-lookup table key)
 (let* ((n-alists (table-get-n-alists table))
	(alists   (table-get-alists table))
	(hash (modulo key n-alists))
	(probe (assq key (vector-ref alists hash))))
  (if probe
      (cdr probe)
      (scream "table-lookup failed"))))

(define (table-expand table)
 (let* ((old-n-alists (table-get-n-alists table))
	(old-alists   (table-get-alists table))
	(new-n-alists (* 10 old-n-alists))
	(new-alists   (make-vector new-n-alists '())))
  (let loop ((i 0))
   (if (< i old-n-alists)
       (begin (let loop ((a (vector-ref old-alists i)))
	       (if (not (null? a))
		   (begin
		    (let ((a (car a)))
		     (let ((new-hash (modulo (car a) new-n-alists)))
		      (vector-set! new-alists new-hash
				   (cons a (vector-ref new-alists new-hash)))))
		    (loop (cdr a)))))
	      (loop (+ i 1)))))
  (table-set-n-alists table new-n-alists)
  (table-set-alists   table new-alists)))

(define (table-set! table key value)
 (let* ((n-alists (table-get-n-alists table))
	(alists   (table-get-alists table))
	(hash (modulo key n-alists))
	(probe (assq key (vector-ref alists hash))))
  (if probe
      (set-cdr! probe value)
      (begin
       (vector-set! alists hash
		    (cons (cons key value) (vector-ref alists hash)))
       (table-set-count table (add-one (table-get-count table)))
       (if (> (table-get-count table) (* 10 n-alists))
	   (table-expand table))))))

(define (table-count table)
 (table-get-count table))

(define (walk-table k table)
 (let ((n-alists (table-get-n-alists table))
       (alists   (table-get-alists table)))
  (let loop ((i 0))
   (if (< i n-alists)
       (begin (let loop ((p (vector-ref alists i)))
	       (if (not (null? p))
		   (begin (let ((p (car p)))
			   (k (car p) (cdr p)))
			  (loop (cdr p)))))
	      (loop (+ i 1)))))))

(define (table-test-and-set! table key)
 (let ((probe (table-test table key)))
  (if (not probe)
      (table-set! table key #t))
  probe))

(define (walk-vector fun vec)
 (let ((veclen (vector-length vec)))
  (let next ((i 0))
   (if (< i veclen)
       (begin (fun (vector-ref vec i))
	      (next (add-one i)))))))

(define (walk-vector-with-offset offset fun vec)
 (let ((veclen (vector-length vec)))
  (let next ((i offset))
   (if (< i veclen)
       (begin (fun (vector-ref vec i))
	      (next (add-one i)))))))

(define (vector-replace to from len)
 (do-times-up len
	      (lambda (i)
	       (vector-set! to i (vector-ref from i)))))

(define alphabet-size (box 2))

;;; note: for now, using integers for state labels
;;; -1 unknown
;;;  0 reject
;;;  1 accept

(define (nondef) 'ndf)
(define (nondef? x) (eq? 'ndf x))

(define (gloid node)
 (vector-ref node 0))

(define (state-label node)
 (vector-ref node 1))

(define (father node)
 (vector-ref node 2))

(define (get-child node i)
 (vector-ref node (+ 3 i)))

(define (set-state-label node val)
 (vector-set! node 1 val))

(define (set-father node val)
 (vector-set! node 2 val))

(define (set-child node i val)
 (vector-set! node (+ 3 i) val))

(define (walk-children fun node)
 (walk-vector-with-offset 3 fun node))

(define global-node-counter (box 0))

(define (make-node father)
 (let ((old-counter-val (unbox global-node-counter))
       (node (make-vector (+ 3 (unbox alphabet-size)))))
  (set-box! global-node-counter (+ (unbox global-node-counter) 1))
  (vector-set!     node 0 old-counter-val)
  (set-state-label node -1)
  (set-father      node father)
  (do-times-up (unbox alphabet-size)
	       (lambda (i)
		(set-child node i (nondef))))
  node))

(define (make-duplicate-node x)
 (copy-vector x))

(define (replace-node-contents to from)
 (vector-replace to from (+ 3 (unbox alphabet-size))))

(define (augment-tree curnode string label)
 (if (null? string)
     (set-state-label curnode label)
     (let ((cursym (car string)))
      (if (nondef? (get-child curnode cursym))
	  (set-child curnode cursym (make-node curnode)))
      (augment-tree (get-child curnode cursym)
		    (cdr string) label))))

(define (new-build-tree example-list)
 (let ((root (make-node (nondef))))
  (walk-list example-list
	     (lambda (example)
	      (augment-tree root (cdr example) (car example))))
  root))

(define (build-tree-from-cform-file name)
 (let* ((the-file (open-input-file name))
	(n-strings (read-int the-file))
	(ignore
	 (begin
	  (set-box! alphabet-size (read-int the-file))
	  #f))
	(root (make-node (nondef))))
  (do-times-up n-strings
	       (lambda (i)
		(let* ((label (read-int the-file))
		       (string-len (read-int the-file))
		       (rb2 (revbuilder-new)))
		 (do-times-up string-len
			      (lambda (j)
			       (revbuilder-add rb2 (read-int the-file))))
		 (augment-tree root (revbuilder-done rb2) label))))
  root))

(define (find-incoming-token node father)
 (let next ((i 0) (n-matches 0) (matching-i -1))
  (if (= i (unbox alphabet-size))
      (if (= n-matches 1)
	  matching-i
	  (scream "horrible bug in find-incoming-token ~a ~a ~a"
		  n-matches node father))
      (next (add-one i)
	    (if (eq? (get-child father i) node)
		(add-one n-matches)
		n-matches)
	    (if (eq? (get-child father i) node)
		i
		matching-i)))))

(define operate-noisily? #f)

(define (print-tree start-node)
 (let ((seen (make-table))
       (todo (make-queue))
       (count (make-counter)))
  (queue-push todo start-node)
  (let next ()
   (let ((curnode (queue-pop todo)))
    (if (not (table-test-and-set! seen (gloid curnode)))
	(begin (pormat-function "~a ~a "
				(gloid curnode)
				(state-label curnode))
	       (walk-children
		(lambda (x) (if (nondef? x)
				(pormat-function "? ")
				(begin
				 (queue-push todo x)
				 (pormat-function "~a " (gloid x)))))
		curnode)
	       (pormat-function "~%"))))
   (if (not (queue-empty? todo)) (next)))))

(define (fancy-print-tree start-node)
 (let ((seen (make-table))
       (id-table (make-table))
       (todo (make-queue))
       (count (make-counter)))
  (queue-push todo start-node)
  (let next-node ()
   (let ((curnode (queue-pop todo)))
    (if (not (table-test id-table (gloid curnode)))
	(begin (table-set! id-table (gloid curnode) (count))
	       (walk-children
		(lambda (x) (if (not (nondef? x)) (queue-push todo x)))
		curnode)))
    (if (not (queue-empty? todo)) (next-node))))
  (pormat-function "~a ~a~%" (count) (unbox alphabet-size))
  (queue-push todo start-node)
  (let next ()
   (let ((curnode (queue-pop todo)))
    (if (not (table-test-and-set! seen (gloid curnode)))
	(begin (pormat-function "~a ~a "
				(table-lookup id-table (gloid curnode))
				(state-label curnode))
	       (walk-children
		(lambda (x) (if (nondef? x)
				(pormat-function "? ")
				(begin
				 (queue-push todo x)
				 (pormat-function "~a "
						  (table-lookup id-table
								(gloid x))))))
		curnode)
	       (pormat-function "~%"))))
   (if (not (queue-empty? todo)) (next)))))

(define (label-defined lab) (not (= -1 lab)))

(define (compare-labels tr-node graph-node backup-table)
 (if (label-defined (state-label tr-node))
     (if (label-defined (state-label graph-node))
	 (= (state-label graph-node)
	    (state-label tr-node))
	 (begin
	  (backup graph-node backup-table)
	  (set-state-label graph-node (state-label tr-node))
	  #t))
     #t))

(define (continue-merging tr-node graph-node search-stack
			  backup-table splices)
 (do-times-up (unbox alphabet-size)
	      (lambda (i)
	       (let ((graph-child (get-child graph-node i))
		     (tree-child  (get-child tr-node i)))
		(if (not (nondef? tree-child))
		    (if (not (nondef? graph-child))
			(stack-push search-stack (cons tree-child graph-child))
			(begin
			 (backup graph-node backup-table)
			 (backup tree-child backup-table)
			 (set-child graph-node i tree-child)
			 (set-father tree-child graph-node)
			 (set-box! splices
				   (cons (cons graph-node tree-child)
					 (unbox splices))))))))))

(define (try-merging curnode potential-match backup-table splices)
 (let ((search-stack (make-stack)))
  (stack-push search-stack (cons curnode potential-match))
  (let loop ()
   (let* ((p (stack-pop search-stack))
	  (t (car p))
	  (g (cdr p))
	  (result (compare-labels t g backup-table)))
    (if result
	(begin
	 (continue-merging t g search-stack backup-table splices)
	 (if (not (stack-empty? search-stack))
	     (loop)
	     #t))
	#f)))))

(define (accept-the-node curnode todo unique-table unique-nodes)
 (set-box! unique-nodes (append (unbox unique-nodes) (list curnode)))
 (table-set! unique-table (gloid curnode) #t)
 (walk-children
  (lambda (x) (if (not (nondef? x)) (queue-push todo x)))
  curnode))

(define (consider-node todo unique-table unique-nodes)
 (let* ((curnode (queue-pop todo))
	(curdad  (father curnode))
	(inchar  (if (nondef? curdad) -1 ; happens only at root
		     (find-incoming-token curnode curdad))))
  (let find-match ((potential-matches (unbox unique-nodes)))
   (if (null? potential-matches)
       (accept-the-node curnode todo unique-table unique-nodes)
       (let ((potential-match (car potential-matches))
	     (backup-table (make-table))
	     (splices (box '())))
	(set-child curdad inchar potential-match)
	(if (try-merging curnode potential-match
			 backup-table splices)
	    (walk-list (unbox splices)
		       (lambda (x)
			(if (table-test unique-table (gloid (car x)))
			    (queue-push todo (cdr x)))))
	    (begin
	     (revert-from-backups backup-table)
	     (set-child curdad inchar curnode)
	     (find-match (cdr potential-matches)))))))
  (if (not (queue-empty? todo))
      (consider-node todo unique-table unique-nodes))))

(define (greedily-collapse root)
 (let ((todo         (make-queue))
       (unique-table (make-table))
       (unique-nodes (box '())))
  (queue-push todo root)
  (consider-node todo unique-table unique-nodes)
  root))

(define (backup guy backup-table)
 (if (not (table-test backup-table (gloid guy)))
     (let ((backup (make-duplicate-node guy)))
      (table-set! backup-table (gloid guy) (cons guy backup)))))

(define (revert-from-backups backup-table)
 (if (not (zero? (table-count backup-table)))
     (walk-table (lambda (id guy-dot-backup)
		  (let ((guy    (car guy-dot-backup))
			(backup (cdr guy-dot-backup)))
		   (replace-node-contents guy backup)))
		 backup-table)))

(define (revbuilder-new)
 (box '()))

(define (revbuilder-add rb new)
 (set-box! rb (cons new (unbox rb))))

(define (revbuilder-done rb)
 (reverse (unbox rb)))

(define (arbitrary-root->canonical-form machine root-node)
 (let* ((old-transitions (list->vector (car machine)))
	(oldnew (make-table))
	(seenit (make-table))
	(count (make-counter))
	(todo  (make-queue))
	(new-transitions (revbuilder-new)))
  (queue-push todo root-node)
  (let next ()
   (let ((curnode (queue-pop todo)))
    (if (not (table-test oldnew curnode))
	(begin (table-set! oldnew curnode (count))
	       (walk-list (vector-ref old-transitions curnode)
			  (lambda (x)
			   (queue-push todo x)))))
    (if (not (queue-empty? todo)) (next))))
  (queue-push todo root-node)
  (let next ()
   (let ((curnode (queue-pop todo)))
    (if (not (table-test seenit curnode))
	(begin
	 (table-set! seenit curnode #t)
	 (let ((new-trans (revbuilder-new)))
	  (walk-list (vector-ref old-transitions curnode)
		     (lambda (x)
		      (queue-push todo x)
		      (revbuilder-add new-trans (table-lookup oldnew x))))
	  (revbuilder-add new-transitions (revbuilder-done new-trans)))))
    (if (not (queue-empty? todo)) (next))))
  (let next ((new-acceptors '()) (old-acceptors (second machine)))
   (if (null? old-acceptors)
       (list (revbuilder-done new-transitions) (sort < new-acceptors))
       (let ((check (table-test oldnew (car old-acceptors))))
	(if check
	    (let ((new (table-lookup oldnew (car old-acceptors))))
	     (next (cons new new-acceptors) (cdr old-acceptors)))
	    (next new-acceptors (cdr old-acceptors))))))))

(define (dfa-tree->canonical-form start-node)
 (let ((seen (make-table))
       (id-table (make-table))
       (todo (make-queue))
       (count (make-counter))
       (transitions (revbuilder-new))
       (acceptors (revbuilder-new)))
  ;; Allocate a universal rejecting state at position 0.
  ;; If it isn't used, it will be discarded at the end.
  (revbuilder-add transitions (make-list (unbox alphabet-size) 0))
  (count)				; the root will now be at position 1.
  (queue-push todo start-node)
  (let next-node ()
   (let ((curnode (queue-pop todo)))
    (if (not (table-test id-table (gloid curnode)))
	(begin (table-set! id-table (gloid curnode) (count))
	       (walk-children
		(lambda (x) (if (not (nondef? x)) (queue-push todo x)))
		curnode)))
    (if (not (queue-empty? todo)) (next-node))))
  (queue-push todo start-node)
  (let next ()
   (let ((curnode (queue-pop todo)))
    (if (not (table-test-and-set! seen (gloid curnode)))
	(begin
	 (if (eq? 1 (state-label curnode))
	     (revbuilder-add
	      acceptors (table-lookup id-table (gloid curnode))))
	 (let ((trans (revbuilder-new)))
	  (walk-children
	   (lambda (x)
	    (if (nondef? x)		; absent children expressed by
		(revbuilder-add trans 0) ; transitions to rejecting state
		(begin (queue-push todo x)
		       (revbuilder-add trans (table-lookup id-table
							   (gloid x))))))
	   curnode)
	  (revbuilder-add transitions (revbuilder-done trans))))))
   (if (not (queue-empty? todo)) (next)))
  ;; Now we rearrange the machine so that the
  ;; the root is at position 0.
  (arbitrary-root->canonical-form
   (list (revbuilder-done transitions)
	 (sort < (revbuilder-done acceptors)))
   1)))

(define (test file-name)
 (fancy-print-tree
  (greedily-collapse
   (build-tree-from-cform-file file-name))))

(test "train2000.cform")
