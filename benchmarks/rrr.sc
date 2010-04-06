;;; LaHaShem HaAretz U'Mloah

;;; begin stolen stuff

;;; begin Scheme->C
(define (panic s) (error 'panic s))
;;; end Scheme->C
;;; begin Gambit-C
(define (panic s) (error s))
;;; end Gambit-C
;;; begin Bigloo
(define (panic s) (error s 'panic 'panic))
;;; end Bigloo
;;; begin Chez
(define (panic s) (error 'panic s))
;;; end Chez
;;; begin Chicken
(define (panic s) (error s))
;;; end Chicken

(define (first x) (car x))

(define (rest x) (cdr x))

(define (map-n-vector f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n) (begin (vector-set! v i (f i)) (loop (+ i 1)))))
  v))

(define (every-n p n)
 (let loop ((i 0)) (or (>= i n) (and (p i) (loop (+ i 1))))))

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

(define (x v) (vector-ref v 0))

(define (y v) (vector-ref v 1))

(define (make-matrix m n . &rest)
 (cond ((null? &rest) (map-n-vector (lambda (i) (make-vector n)) m))
       ((null? (rest &rest))
	(map-n-vector (lambda (i) (make-vector n (first &rest))) m))
       (else (panic "Too many arguments to MAKE-MATRIX"))))

(define (matrix-rows a) (vector-length a))

(define (matrix-columns a) (vector-length (vector-ref a 0)))

(define (matrix-ref a i j) (vector-ref (vector-ref a i) j))

(define (matrix-set! a i j x) (vector-set! (vector-ref a i) j x))

(define (pormat control-string . values)
 (let loop ((i 0) (values values))
  (if (not (= i (string-length control-string)))
      (let ((c (string-ref control-string i)))
       (cond
	((char=? c #\~)
	 (let ((c2 (string-ref control-string (+ i 1))))
	  (cond
	   ((char=? c2 #\a) (display (car values)) (loop (+ i 2) (cdr values)))
	   ((char=? c2 #\s) (write (car values)) (loop (+ i 2) (cdr values)))
	   ((char=? c2 #\%) (newline) (loop (+ i 2) values))
	   (else (display c) (loop (+ 1 i) values)))))
	(else (write-char c) (loop (+ i 1) values)))))))

;;; end stolen stuff

;;; The vertices are s, t, and (y,x).
;;; C-RIGHT[y,x] is the capacity from (y,x) to (y,x+1) which is the same as the
;;; capacity from (y,x+1) to (y,x).
;;; C-DOWN[y,x] is the capacity from (y,x) to (y+1,x) which is the same as the
;;; capacity from (y+1,x) to (y,x).
;;; The capacity from s to (y,0), (0,x), (y,Y-1), (0,X-1) is implicitly
;;; infinite.
;;; The capacity from (x,y) to t is V*W[y,x].
;;; F-RIGHT[y,x] is the preflow from (y,x) to (y,x+1) which is the negation of
;;; the preflow from (y,x+1) to (y,x).
;;; F-DOWN[y,x] is the preflow from (y,x) to (y+1,x) which is the negation of
;;; the preflow from (y+1,x) to (y,x).
;;; We do not record the preflow from s to (y,X-1), (y,0), (Y-1,x), and (0,x)
;;; and from (y,X-1), (y,0), (Y-1,x), and (0,x) to s.
;;; F-T[y,x] is the preflow from (y,x) to t.
;;; We do not record the preflow from t to (y,x).
;;; {C,F}-RIGHT[0:Y-1,0:X-2].
;;; {C,F}-DOWN[0:Y-2,0:X-1].
;;; F-T[0:Y-1,0:X-1]
;;; For now, we will keep all capacities (and thus all preflows) as integers.
;;; (CF-RIGHT y x) is the residual capacity from (y,x) to (y,x+1).
;;; (CF-LEFT y x) is the residual capacity from (y,x) to (y,x-1).
;;; (CF-DOWN y x) is the residual capacity from (y,x) to (y+1,x).
;;; (CF-UP y x) is the residual capacity from (y,x) to (y-1,x).
;;; We do not compute the residual capacities from s to (y,X-1), (y,0),
;;; (Y-1,x), and (0,x) because they are all infinite.
;;; We do not compute the residual capacities from (y,X-1), (y,0), (Y-1,x),
;;; and (0,x) to s because they will never be used.
;;; (CF-T y x) is the residual capacity from (y,x) to t.
;;; We do not compute the residual capacity from t to (y,x) because it will
;;; be used.
;;; (EF-RIGHT? y x) is true if there is an edge from (y,x) to (y,x+1) in the
;;; residual network.
;;; (EF-LEFT? y x) is true if there is an edge from (y,x) to (y,x-1) in the
;;; residual network.
;;; (EF-DOWN? y x) is true if there is an edge from (y,x) to (y+1,x) in the
;;; residual network.
;;; (EF-UP? y x) is true if there is an edge from (y,x) to (y-1,x) in the
;;; residual network.
;;; (EF-T? y x) is true if there is an edge from (y,x) to t in the
;;; residual network.
;;; There are always edges in the residual network from s to (y,X-1), (y,0),
;;; (Y-1,x), and (0,x).
;;; We don't care whether there are edges in the residual network from
;;; (y,X-1), (y,0), (Y-1,x), and (0,x) to s because they will never be used.
;;; We don't care whether there are edges in the residual network from t to
;;; (y,x) because they will never be used.

(define (positive-min x y) (if (negative? x) y (min x y)))

(define (positive- x y) (if (negative? x) x (- x y)))

(define (positive+ x y) (if (negative? x) x (+ x y)))

(define (rao-ratio-region c-right c-down w lg-max-v)
 (let* ((height (matrix-rows w))
	(width (matrix-columns w))
	(f-right (make-matrix height (- width 1) 0))
	(f-down (make-matrix (- height 1) width 0))
	(f-t (make-matrix height width 0))
	(h (make-matrix height width 0))
	(e (make-matrix height width 0))
	(marked? (make-matrix height width #f))
	(m1 (+ (* height width) 2))
	(m2 (+ (* 2 height width) 2))
	(q (make-vector (+ (* 2 height width) 3) '())))
  (define (cf-right y x)
   (- (matrix-ref c-right y x) (matrix-ref f-right y x)))
  (define (cf-left y x)
   (+ (matrix-ref c-right y (- x 1)) (matrix-ref f-right y (- x 1))))
  (define (cf-down y x)
   (- (matrix-ref c-down y x) (matrix-ref f-down y x)))
  (define (cf-up y x)
   (+ (matrix-ref c-down (- y 1) x) (matrix-ref f-down (- y 1) x)))
  (define (ef-right? y x) (positive? (cf-right y x)))
  (define (ef-left? y x) (positive? (cf-left y x)))
  (define (ef-down? y x) (positive? (cf-down y x)))
  (define (ef-up? y x) (positive? (cf-up y x)))
  (define (preflow-push! v)
   (define (enqueue! y x)
    (if (not (matrix-ref marked? y x))
	(begin
	 (vector-set! q
		      (matrix-ref h y x)
		      (cons (vector x y) (vector-ref q (matrix-ref h y x))))
	 (matrix-set! marked? y x #t))))
   (define (cf-t y x) (- (* v (matrix-ref w y x)) (matrix-ref f-t y x)))
   (define (ef-t? y x) (positive? (cf-t y x)))
   (define (can-push-right? y x)
    (and (< x (- width 1))
	 (not (zero? (matrix-ref e y x)))
	 (ef-right? y x)
	 (= (matrix-ref h y x) (+ (matrix-ref h y (+ x 1)) 1))))
   (define (can-push-left? y x)
    (and (> x 0)
	 (not (zero? (matrix-ref e y x)))
	 (ef-left? y x)
	 (= (matrix-ref h y x) (+ (matrix-ref h y (- x 1)) 1))))
   (define (can-push-down? y x)
    (and (< y (- height 1))
	 (not (zero? (matrix-ref e y x)))
	 (ef-down? y x)
	 (= (matrix-ref h y x) (+ (matrix-ref h (+ y 1) x) 1))))
   (define (can-push-up? y x)
    (and (> y 0)
	 (not (zero? (matrix-ref e y x)))
	 (ef-up? y x)
	 (= (matrix-ref h y x) (+ (matrix-ref h (- y 1) x) 1))))
   (define (can-push-t? y x)
    (and (not (zero? (matrix-ref e y x)))
	 (ef-t? y x)
	 (= (matrix-ref h y x) 1)))
   (define (can-lift? y x)
    (and (not (zero? (matrix-ref e y x)))
	 (if (= x (- width 1))
	     (<= (matrix-ref h y x) m1)
	     (or (not (ef-right? y x))
		 (<= (matrix-ref h y x) (matrix-ref h y (+ x 1)))))
	 (if (= x 0)
	     (<= (matrix-ref h y x) m1)
	     (or (not (ef-left? y x))
		 (<= (matrix-ref h y x) (matrix-ref h y (- x 1)))))
	 (if (= y (- height 1))
	     (<= (matrix-ref h y x) m1)
	     (or (not (ef-down? y x))
		 (<= (matrix-ref h y x) (matrix-ref h (+ y 1) x))))
	 (if (= y 0)
	     (<= (matrix-ref h y x) m1)
	     (or (not (ef-up? y x))
		 (<= (matrix-ref h y x) (matrix-ref h (- y 1) x))))
	 (or (not (ef-t? y x)) (= (matrix-ref h y x) 0))))
   (define (push-right! y x)
    ;;(pormat "Push right ~s ~s~%" y x)
    (let ((df-u-v (positive-min (matrix-ref e y x) (cf-right y x))))
     (matrix-set! f-right y x (+ (matrix-ref f-right y x) df-u-v))
     (matrix-set! e y x (positive- (matrix-ref e y x) df-u-v))
     (matrix-set! e y (+ x 1) (positive+ (matrix-ref e y (+ x 1)) df-u-v))
     (enqueue! y (+ x 1))))
   (define (push-left! y x)
    ;;(pormat "Push left ~s ~s~%" y x)
    (let ((df-u-v (positive-min (matrix-ref e y x) (cf-left y x))))
     (matrix-set! f-right y (- x 1) (- (matrix-ref f-right y (- x 1)) df-u-v))
     (matrix-set! e y x (positive- (matrix-ref e y x) df-u-v))
     (matrix-set! e y (- x 1) (positive+ (matrix-ref e y (- x 1)) df-u-v))
     (enqueue! y (- x 1))))
   (define (push-down! y x)
    ;;(pormat "Push down ~s ~s~%" y x)
    (let ((df-u-v (positive-min (matrix-ref e y x) (cf-down y x))))
     (matrix-set! f-down y x (+ (matrix-ref f-down y x) df-u-v))
     (matrix-set! e y x (positive- (matrix-ref e y x) df-u-v))
     (matrix-set! e (+ y 1) x (positive+ (matrix-ref e (+ y 1) x) df-u-v))
     (enqueue! (+ y 1) x)))
   (define (push-up! y x)
    ;;(pormat "Push up ~s ~s~%" y x)
    (let ((df-u-v (positive-min (matrix-ref e y x) (cf-up y x))))
     (matrix-set! f-down (- y 1) x (- (matrix-ref f-down (- y 1) x) df-u-v))
     (matrix-set! e y x (positive- (matrix-ref e y x) df-u-v))
     (matrix-set! e (- y 1) x (positive+ (matrix-ref e (- y 1) x) df-u-v))
     (enqueue! (- y 1) x)))
   (define (push-t! y x)
    ;;(pormat "Push t ~s ~s~%" y x)
    (let ((df-u-v (positive-min (matrix-ref e y x) (cf-t y x))))
     (matrix-set! f-t y x (+ (matrix-ref f-t y x) df-u-v))
     (matrix-set! e y x (positive- (matrix-ref e y x) df-u-v))))
   (define (lift! y x)
    ;;(pormat "Lift ~s ~s~%" y x)
    (matrix-set! h y x
		 (+ 1
		    (min (if (= x (- width 1))
			     m1
			     (if (ef-right? y x) (matrix-ref h y (+ x 1)) m2))
			 (if (= x 0)
			     m1
			     (if (ef-left? y x) (matrix-ref h y (- x 1)) m2))
			 (if (= y (- height 1))
			     m1
			     (if (ef-down? y x) (matrix-ref h (+ y 1) x) m2))
			 (if (= y 0)
			     m1
			     (if (ef-up? y x) (matrix-ref h (- y 1) x) m2))
			 (if (ef-t? y x) 0 m2)))))
   (define (relabel!)
    ;;(pormat "Relabel~%")
    (let ((q '())
	  (tail #f))
     (define (enqueue! y x value)
      (if (< value (matrix-ref h y x))
	  (begin (matrix-set! h y x value)
		 (if (not (matrix-ref marked? y x))
		     (begin
		      (matrix-set! marked? y x #t)
		      (cond ((eq? tail #f)
			     (set! tail (cons (vector x y) '()))
			     (set! q tail))
			    (else (set-cdr! tail (cons (vector x y) '()))
				  (set! tail (cdr tail)))))))))
     (define (dequeue!)
      (let ((p (first q)))
       (matrix-set! marked? (y p) (x p) #f)
       (set! q (rest q))
       (if (null? q) (set! tail #f))
       p))
     (do ((y 0 (+ y 1))) ((>= y height))
      (do ((x 0 (+ x 1))) ((>= x width))
       (matrix-set! h y x m1)
       (matrix-set! marked? y x #f)))
     (do ((y 0 (+ y 1))) ((>= y height))
      (do ((x 0 (+ x 1))) ((>= x width))
       (if (and (ef-t? y x) (> (matrix-ref h y x) 1)) (enqueue! y x 1))))
     (let loop ()
      (if (not (null? q))
	  (begin (let* ((p (dequeue!))
			(x (x p))
			(y (y p))
			(value (+ (matrix-ref h y x) 1)))
		  (if (and (> x 0) (ef-right? y (- x 1)))
		      (enqueue! y (- x 1) value))
		  (if (and (< x (- width 1)) (ef-left? y (+ x 1)))
		      (enqueue! y (+ x 1) value))
		  (if (and (> y 0) (ef-down? (- y 1) x))
		      (enqueue! (- y 1) x value))
		  (if (and (< y (- height 1)) (ef-up? (+ y 1) x))
		      (enqueue! (+ y 1) x value)))
		 (loop))))))
   (do ((y 0 (+ y 1))) ((>= y height))
    (do ((x 0 (+ x 1))) ((>= x width))
     (matrix-set! e y x 0)
     (matrix-set! f-t y x 0)))
   (do ((y 0 (+ y 1))) ((>= y height))
    (do ((x 0 (+ x 1))) ((>= x (- width 1)))
     (matrix-set! f-right y x 0)))
   (do ((y 0 (+ y 1))) ((>= y (- height 1)))
    (do ((x 0 (+ x 1))) ((>= x width))
     (matrix-set! f-down y x 0)))
   (do ((y 0 (+ y 1))) ((>= y height))
    (matrix-set! e y (- width 1) -1)
    (matrix-set! e y 0 -1))
   (do ((x 1 (+ x 1))) ((>= x (- width 1)))
    (matrix-set! e (- height 1) x -1)
    (matrix-set! e 0 x -1))
   (let ((pushes 0)
	 (lifts 0)
	 (relabels 0))
    (let loop ((i 0) (p? #f))
     (cond
      ((and (zero? (modulo i 6)) (not p?))
       (relabel!)
       (set! relabels (+ relabels 1))
       (cond
	((every-n (lambda (y)
		   (every-n (lambda (x)
			     (or (zero? (matrix-ref e y x))
				 (= (matrix-ref h y x) m1)))
			    width))
		  height)
	 ;; Every vertex with excess capacity is not reachable from the sink in
	 ;; the inverse residual network. So terminate early because we have
	 ;; already found a min cut. In this case, the preflows and excess
	 ;; capacities will not be correct. But the cut is indicated by the
	 ;; heights. Vertices reachable from the source have height
	 ;; (+ (* HEIGHT WIDTH) 2) while vertices reachable from the sink have
	 ;; smaller height. Early termination is necessary with relabeling to
	 ;; prevent an infinite loop. The loop arises because vertices that are
	 ;; not reachable from the sink in the inverse residual network have
	 ;; their height reset to (+ (* HEIGHT WIDTH) 2) by the relabeling
	 ;; process. If there are such vertices with excess capacity, this is
	 ;; not high enough for the excess capacity to be pushed back to the
	 ;; perimeter. So after relabeling, vertices get lifted to try to push
	 ;; excess capacity back to the perimeter but then a relabeling happens
	 ;; to soon and foils this lifting. Terminating when all vertices with
	 ;; excess capacity are not reachable from the sink in the inverse
	 ;; residual network eliminates this problem.
	 (pormat
	  "~s push~a, ~s lift~a, ~s relabel~a, ~s wave~a, terminated early~%"
	  pushes
	  (if (= pushes 1) "" "es")
	  lifts
	  (if (= lifts 1) "" "s")
	  relabels
	  (if (= relabels 1) "" "s")
	  i
	  (if (= i 1) "" "s")))
	(else
	 ;; We need to rebuild the priority queue after relabeling since the
	 ;; heights might have changed and the priority queue is indexed by
	 ;; height. This also assumes that a relabel is done before any pushes
	 ;; or lifts.
	 (do ((k 0 (+ k 1))) ((>= k (vector-length q))) (vector-set! q k '()))
	 (do ((y 0 (+ y 1))) ((>= y height))
	  (do ((x 0 (+ x 1))) ((>= x width))
	   (matrix-set! marked? y x #f)))
	 (do ((y 0 (+ y 1))) ((>= y height))
	  (do ((x 0 (+ x 1))) ((>= x width))
	   (if (not (zero? (matrix-ref e y x))) (enqueue! y x))))
	 (loop i #t))))
      ((some-vector (lambda (ps)
		     (some (lambda (p)
			    (let ((x (x p))
				  (y (y p)))
			     (or (can-push-right? y x)
				 (can-push-left? y x)
				 (can-push-down? y x)
				 (can-push-up? y x)
				 (can-push-t? y x)
				 (can-lift? y x))))
			   ps))
		    q)
       (let loop ((k (- (vector-length q) 1)))
	(if (not (negative? k))
	    (begin
	     (let ((ps (vector-ref q k)))
	      (vector-set! q k '())
	      (for-each (lambda (p) (matrix-set! marked? (y p) (x p) #f)) ps)
	      (for-each (lambda (p)
			 (let ((x (x p))
			       (y (y p)))
			  (if (can-push-right? y x)
			      (begin (set! pushes (+ pushes 1))
				     (push-right! y x)))
			  (if (can-push-left? y x)
			      (begin (set! pushes (+ pushes 1))
				     (push-left! y x)))
			  (if (can-push-down? y x)
			      (begin (set! pushes (+ pushes 1))
				     (push-down! y x)))
			  (if (can-push-up? y x)
			      (begin (set! pushes (+ pushes 1))
				     (push-up! y x)))
			  (if (can-push-t? y x)
			      (begin (set! pushes (+ pushes 1))
				     (push-t! y x)))
			  (if (can-lift? y x)
			      (begin (set! lifts (+ lifts 1))
				     (lift! y x)))
			  (if (not (zero? (matrix-ref e y x)))
			      (enqueue! y x))))
			ps))
	     (loop (- k 1)))))
       (loop (+ i 1) #f))
      (else
       ;; This is so MIN-CUT and MIN-CUT-INCLUDES-EVERY-EDGE-TO-T? work.
       (relabel!)
       (set! relabels (+ relabels 1))
       (pormat "~s push~a, ~s lift~a, ~s relabel~a, ~s wave~a~%"
	       pushes
	       (if (= pushes 1) "" "es")
	       lifts
	       (if (= lifts 1) "" "s")
	       relabels
	       (if (= relabels 1) "" "s")
	       i
	       (if (= i 1) "" "s")))))))
  (define (min-cut-includes-every-edge-to-t?)
   ;; This requires that a relabel was done immediately before returning from
   ;; PREFLOW-PUSH!.
   (every-n (lambda (y) (every-n (lambda (x) (= (matrix-ref h y x) m1)) width))
	    height))
  (define (min-cut)
   ;; This requires that a relabel was done immediately before returning from
   ;; PREFLOW-PUSH!.
   (map-n-vector
    (lambda (y)
     (map-n-vector (lambda (x) (not (= (matrix-ref h y x) m1))) width))
    height))
  (let loop ((lg-v lg-max-v) (v-max 0))
   (cond
    ((negative? lg-v)
     (pormat "V-MAX=~s~%" v-max)
     (preflow-push! (+ v-max 1))
     (min-cut))
    (else
     (let ((v (+ v-max
		 (let loop ((i lg-v) (c 1))
		  (if (zero? i) c (loop (- i 1) (+ c c)))))))
      (pormat "LG-V=~s, V-MAX=~s, V=~s~%" lg-v v-max v)
      (preflow-push! v)
      (loop (- lg-v 1) (if (min-cut-includes-every-edge-to-t?) v v-max))))))))

(let* ((height 512)
       (width 512)
       (lg-max-v 15)
       (c-right (make-matrix height (- width 1)))
       (c-down (make-matrix (- height 1) width)))
 (do ((y 0 (+ y 1))) ((>= y height))
  (do ((x 0 (+ x 1))) ((>= x (- width 1)))
   (matrix-set! c-right y x
		(if (and (>= y (quotient height 4))
			 (< y (quotient (* 3 height) 4))
			 (or (= x (- (quotient width 4) 1))
			     (= x (- (quotient (* 3 width) 4) 1))))
		    1
		    128))))
 (do ((y 0 (+ y 1))) ((>= y (- height 1)))
  (do ((x 0 (+ x 1))) ((>= x width))
   (matrix-set! c-down y x
		(if (and (>= x (quotient width 4))
			 (< x (quotient (* 3 width) 4))
			 (or (= y (- (quotient height 4) 1))
			     (= y (- (quotient (* 3 height) 4) 1))))
		    1
		    128))))
 (rao-ratio-region c-right c-down (make-matrix height width 1) lg-max-v))

;;; Tam V'Nishlam Shevah L'El Borei Olam
