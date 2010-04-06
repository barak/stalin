;;; The constants are hardwired to be inexact for efficiency.

;;; begin Stalin
(define make-model (primitive-procedure make-structure model 6))
(define model-pi (primitive-procedure structure-ref model 0))
(define set-model-pi! (primitive-procedure structure-set! model 0))
(define model-mu (primitive-procedure structure-ref model 1))
(define model-sigma (primitive-procedure structure-ref model 2))
(define model-log-pi (primitive-procedure structure-ref model 3))
(define set-model-log-pi! (primitive-procedure structure-set! model 3))
(define model-sigma-inverse (primitive-procedure structure-ref model 4))
(define model-log-determinant-sigma
 (primitive-procedure structure-ref model 5))
(define set-model-log-determinant-sigma!
 (primitive-procedure structure-set! model 5))
(define (void) ((lambda ())))
;;; end Stalin
;;; begin Scheme->C
(define make-model vector)
(define (model-pi model) (vector-ref model 0))
(define (set-model-pi! model pi) (vector-set! model 0 pi))
(define (model-mu model) (vector-ref model 1))
(define (model-sigma model) (vector-ref model 2))
(define (model-log-pi model) (vector-ref model 3))
(define (set-model-log-pi! model log-pi) (vector-set! model 3 log-pi))
(define (model-sigma-inverse model) (vector-ref model 4))
(define (model-log-determinant-sigma model) (vector-ref model 5))
(define (set-model-log-determinant-sigma! model log-determinant-sigma)
 (vector-set! model 5 log-determinant-sigma))
(define (panic s) (error 'panic s))
(define (void) #f)
;;; end Scheme->C
;;; begin Gambit-C
(define-structure model
 pi mu sigma log-pi sigma-inverse log-determinant-sigma)
(define set-model-pi! model-pi-set!)
(define set-model-log-pi! model-log-pi-set!)
(define set-model-log-determinant-sigma! model-log-determinant-sigma-set!)
(define (panic s) (error s))
;;; end Gambit-C
;;; begin Bigloo
(define make-model vector)
(define (model-pi model) (vector-ref model 0))
(define (set-model-pi! model pi) (vector-set! model 0 pi))
(define (model-mu model) (vector-ref model 1))
(define (model-sigma model) (vector-ref model 2))
(define (model-log-pi model) (vector-ref model 3))
(define (set-model-log-pi! model log-pi) (vector-set! model 3 log-pi))
(define (model-sigma-inverse model) (vector-ref model 4))
(define (model-log-determinant-sigma model) (vector-ref model 5))
(define (set-model-log-determinant-sigma! model log-determinant-sigma)
 (vector-set! model 5 log-determinant-sigma))
(define (panic s) (error s 'panic 'panic))
(define (void) #f)
;;; end Bigloo
;;; begin Chez
(define make-model vector)
(define (model-pi model) (vector-ref model 0))
(define (set-model-pi! model pi) (vector-set! model 0 pi))
(define (model-mu model) (vector-ref model 1))
(define (model-sigma model) (vector-ref model 2))
(define (model-log-pi model) (vector-ref model 3))
(define (set-model-log-pi! model log-pi) (vector-set! model 3 log-pi))
(define (model-sigma-inverse model) (vector-ref model 4))
(define (model-log-determinant-sigma model) (vector-ref model 5))
(define (set-model-log-determinant-sigma! model log-determinant-sigma)
 (vector-set! model 5 log-determinant-sigma))
(define (panic s) (error 'panic s))
;;; end Chez
;;; begin Chicken
(define-record-type model
 pi mu sigma log-pi sigma-inverse log-determinant-sigma)
(define (panic s) (error s))
;;; end Chicken

(define (hex-string->number s)
 (let loop ((s (string->list s)) (c 0))
  (if (null? s)
      c
      (loop (cdr s) (+ (* 16 c)
		       (if (char-numeric? (car s))
			   (- (char->integer (car s)) (char->integer #\0))
			   (+ (- (char->integer (car s)) (char->integer #\a))
			      10)))))))

;;; The following code is a modified version of code taken from SLIB.
;;; Copyright (C) 1991, 1993 Aubrey Jaffer.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define *most-positive-fixnum* 65535)

(define (logical:logxor n1 n2)
 (cond ((= n1 n2) 0)
       ((zero? n1) n2)
       ((zero? n2) n1)
       (else (+ (* (logical:logxor (logical:ash-4 n1) (logical:ash-4 n2)) 16)
		(vector-ref (vector-ref logical:boole-xor (modulo n1 16))
			    (modulo n2 16))))))

(define (logical:ash-4 x)
 (if (negative? x) (+ -1 (quotient (+ 1 x) 16)) (quotient x 16)))

(define logical:boole-xor
 '#(#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    #(1 0 3 2 5 4 7 6 9 8 11 10 13 12 15 14)
    #(2 3 0 1 6 7 4 5 10 11 8 9 14 15 12 13)
    #(3 2 1 0 7 6 5 4 11 10 9 8 15 14 13 12)
    #(4 5 6 7 0 1 2 3 12 13 14 15 8 9 10 11)
    #(5 4 7 6 1 0 3 2 13 12 15 14 9 8 11 10)
    #(6 7 4 5 2 3 0 1 14 15 12 13 10 11 8 9)
    #(7 6 5 4 3 2 1 0 15 14 13 12 11 10 9 8)
    #(8 9 10 11 12 13 14 15 0 1 2 3 4 5 6 7)
    #(9 8 11 10 13 12 15 14 1 0 3 2 5 4 7 6)
    #(10 11 8 9 14 15 12 13 2 3 0 1 6 7 4 5)
    #(11 10 9 8 15 14 13 12 3 2 1 0 7 6 5 4)
    #(12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3)
    #(13 12 15 14 9 8 11 10 5 4 7 6 1 0 3 2)
    #(14 15 12 13 10 11 8 9 6 7 4 5 2 3 0 1)
    #(15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))

(define random:tap 24)

(define random:size 55)

(define (random:size-int l)
 (let ((trial (hex-string->number (make-string l #\f))))
  (if (and (exact? trial) (positive? trial) (>= *most-positive-fixnum* trial))
      l
      (random:size-int (- l 1)))))

(define random:chunk-size (* 4 (random:size-int 8)))

(define random:MASK
 (hex-string->number (make-string (quotient random:chunk-size 4) #\f)))

(define *random-state* '#())

(let ((random-strings
       '#("d909ef3e" "fd330ab3" "e33f7843" "76783fbd" "f3675fb3"
		     "b54ef879" "0be45590" "a6794679" "0bcd56d3" "fabcdef8"
		     "9cbd3efd" "3fd3efcd" "e064ef27" "dddecc08" "34444292"
		     "85444454" "4c519210" "c0366273" "54734567" "70abcddc"
		     "1bbdac53" "616c5a86" "a982efa9" "105996a0" "5f0cccba"
		     "1ea055e1" "fe2acd8d" "1891c1d4" "e6690270" "6912bccc"
		     "2678e141" "61222224" "907abcbb" "4ad6829b" "9cdd1404"
		     "57798841" "5b892496" "871c9cd1" "d1e67bda" "8b0a3233"
		     "578ef23f" "28274ef6" "823ef5ef" "845678c5" "e67890a5"
		     "5890abcb" "851fa9ab" "13efa13a" "b12278d6" "daf805ab"
		     "a0befc36" "0068a7b5" "e024fd90" "a7b690e2" "27f3571a"
		     0)))
 (set! *random-state* (make-vector (+ random:size 1) 0))
 (let ((nibbles (quotient random:chunk-size 4)))
  (do ((i 0 (+ i 1))) ((= i random:size))
   (vector-set!
    *random-state* i
    (hex-string->number
     (substring (vector-ref random-strings i) 0 nibbles))))))

;;; random:chunk returns an integer in the range of
;;; 0 to (- (expt 2 random:chunk-size) 1)
(define (random:chunk v)
 (let* ((p (vector-ref v random:size))
	(ans (logical:logxor
	      (vector-ref v (modulo (- p random:tap) random:size))
	      (vector-ref v p))))
  (vector-set! v p ans)
  (vector-set! v random:size (modulo (- p 1) random:size))
  ans))

(define (rand)
 (do ((ilen 0 (+ 1 ilen))
      (s random:MASK (+ random:MASK (* (+ 1 random:MASK) s))))
   ((>= s (- *most-positive-fixnum* 1))
    (let ((slop (modulo (+ s (- 1 *most-positive-fixnum*))
			*most-positive-fixnum*)))
     (let loop ((n ilen) (r (random:chunk *random-state*)))
      (cond ((not (zero? n))
	     (loop (+ -1 n)
		   (+ (* r (+ 1 random:MASK)) (random:chunk *random-state*))))
	    ((>= r slop) (modulo r *most-positive-fixnum*))
	    (else (loop ilen (random:chunk *random-state*)))))))))

;;; End of code taken from SLIB

(define log-math-precision 35.0)

(define minus-infinity (log 0.0))

(define first car)

(define (second1 x) (car (cdr x)))

(define (second2 x) (car (cdr x)))

(define (second3 x) (car (cdr x)))

(define rest cdr)

(define (list-two1 x y) (cons x (cons y '())))

(define (list-two2 x y) (cons x (cons y '())))

(define (list-two3 x y) (cons x (cons y '())))

(define (reduce f l i)
 (cond ((null? l) i)
       ((null? (rest l)) (first l))
       (else (let loop ((l (rest l)) (c (first l)))
	      (if (null? l) c (loop (rest l) (f c (first l))))))))

(define (every-n1 p n)
 (let loop ((i 0)) (or (>= i n) (and (p i) (loop (+ i 1))))))

(define (every-n2 p n)
 (let loop ((i 0)) (or (>= i n) (and (p i) (loop (+ i 1))))))

(define (sum1 f n)
 (let loop ((n (- n 1)) (c 0.0))
  (if (negative? n) c (loop (- n 1) (+ c (f n))))))

(define (sum2 f n)
 (let loop ((n (- n 1)) (c 0.0))
  (if (negative? n) c (loop (- n 1) (+ c (f n))))))

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

(define (map-n1 f n)
 ;; needs work: To eliminate REVERSE.
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (map-n2 f n)
 ;; needs work: To eliminate REVERSE.
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (map-n-vector1 f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n)
       (begin (vector-set! v i (f i))
	      (loop (+ i 1)))))
  v))

(define (map-n-vector2 f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n)
       (begin (vector-set! v i (f i))
	      (loop (+ i 1)))))
  v))

(define (map-n-vector3 f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n)
       (begin (vector-set! v i (f i))
	      (loop (+ i 1)))))
  v))

(define (map-n-vector4 f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n)
       (begin (vector-set! v i (f i))
	      (loop (+ i 1)))))
  v))

(define (map-n-vector5 f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n)
       (begin (vector-set! v i (f i))
	      (loop (+ i 1)))))
  v))

(define (map-n-vector6 f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n)
       (begin (vector-set! v i (f i))
	      (loop (+ i 1)))))
  v))

(define (map-n-vector7 f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n)
       (begin (vector-set! v i (f i))
	      (loop (+ i 1)))))
  v))

(define (map-n-vector8 f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n)
       (begin (vector-set! v i (f i))
	      (loop (+ i 1)))))
  v))

(define (remove-if-not p l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (positionv x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((eqv? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (make-matrix1 m n) (map-n-vector1 (lambda (i) (make-vector n)) m))

(define (make-matrix2 m n) (map-n-vector2 (lambda (i) (make-vector n)) m))

(define (make-matrix3 m n) (map-n-vector3 (lambda (i) (make-vector n)) m))

(define (make-matrix4 m n) (map-n-vector4 (lambda (i) (make-vector n)) m))

(define (make-matrix-initial m n initial)
 (map-n-vector5 (lambda (i) (make-vector n initial)) m))

(define (matrix-rows a) (vector-length a))

(define (matrix-columns a) (vector-length (vector-ref a 0)))

(define (matrix-ref a i j) (vector-ref (vector-ref a i) j))

(define (matrix-set! a i j x) (vector-set! (vector-ref a i) j x))

(define (matrix-row-ref a i) (vector-ref a i))

(define (matrix-row-set! a i v) (vector-set! a i v))

(define (determinant a)
 (if (not (= (matrix-rows a) (matrix-columns a)))
     (panic "Can only find determinant of a square matrix"))
 (call-with-current-continuation
  (lambda (return)
   (let* ((n (matrix-rows a))
	  (b (make-matrix1 n n))
	  (d 1.0))
    (do ((i 0 (+ i 1))) ((= i n))
     (do ((j 0 (+ j 1))) ((= j n)) (matrix-set! b i j (matrix-ref a i j))))
    (do ((i 0 (+ i 1))) ((= i n))
     ;; partial pivoting reduces rounding errors
     (let ((greatest (abs (matrix-ref b i i)))
	   (index i))
      (do ((j (+ i 1) (+ j 1))) ((= j n))
       (let ((x (abs (matrix-ref b j i))))
	(if (> x greatest) (begin (set! index j) (set! greatest x)))))
      (if (= greatest 0.0) (return 0.0))
      (if (not (= index i))
	  (let ((v (matrix-row-ref b i)))
	   (matrix-row-set! b i (matrix-row-ref b index))
	   (matrix-row-set! b index v)
	   (set! d (- d))))
      (let ((c (matrix-ref b i i)))
       (set! d (* d c))
       (do ((j i (+ j 1))) ((= j n))
	(matrix-set! b i j (/ (matrix-ref b i j) c)))
       (do ((j (+ i 1) (+ j 1))) ((= j n))
	(let ((e (matrix-ref b j i)))
	 (do ((k (+ i 1) (+ k 1))) ((= k n))
	  (matrix-set!
	   b j k (- (matrix-ref b j k) (* e (matrix-ref b i k))))))))))
    d))))

(define (invert-matrix! a b)
 (if (not (= (matrix-rows a) (matrix-columns a)))
     (panic "Can only invert a square matrix"))
 (let* ((n (matrix-rows a))
	(c (make-matrix2 n n)))
  (do ((i 0 (+ i 1))) ((= i n))
   (do ((j 0 (+ j 1))) ((= j n))
    (matrix-set! b i j 0.0)
    (matrix-set! c i j (matrix-ref a i j))))
  (do ((i 0 (+ i 1))) ((= i n)) (matrix-set! b i i 1.0))
  (do ((i 0 (+ i 1))) ((= i n))
   (if (zero? (matrix-ref c i i))
       (call-with-current-continuation
	(lambda (return)
	 (do ((j 0 (+ j 1))) ((= j n))
	  (if (and (> j i) (not (zero? (matrix-ref c j i))))
	      (begin (let ((e (vector-ref c i)))
		      (vector-set! c i (vector-ref c j))
		      (vector-set! c j e))
		     (let ((e (vector-ref b i)))
		      (vector-set! b i (vector-ref b j))
		      (vector-set! b j e))
		     (return (void)))))
	 (panic "Matrix is singular"))))
   (let ((d (/ (matrix-ref c i i))))
    (do ((j 0 (+ j 1))) ((= j n))
     (matrix-set! c i j (* d (matrix-ref c i j)))
     (matrix-set! b i j (* d (matrix-ref b i j))))
    (do ((k 0 (+ k 1))) ((= k n))
     (let ((d (- (matrix-ref c k i))))
      (if (not (= k i))
	  (do ((j 0 (+ j 1))) ((= j n))
	   (matrix-set!
	    c k j (+ (matrix-ref c k j) (* d (matrix-ref c i j))))
	   (matrix-set!
	    b k j (+ (matrix-ref b k j) (* d (matrix-ref b i j))))))))))))

(define (jacobi! a)
 (if (not (and (= (matrix-rows a) (matrix-columns a))
	       (every-n1 (lambda (i)
			  (every-n2 (lambda (j)
				     (= (matrix-ref a i j) (matrix-ref a j i)))
				    (matrix-rows a)))
			 (matrix-rows a))))
     (panic "Can only compute eigenvalues/eigenvectors of a symmetric matrix"))
 (let* ((n (matrix-rows a))
	(d (make-vector n))
	(v (make-matrix-initial n n 0.0))
	(b (make-vector n))
	(z (make-vector n 0.0)))
  (do ((ip 0 (+ ip 1))) ((= ip n))
   (matrix-set! v ip ip 1.0)
   (vector-set! b ip (matrix-ref a ip ip))
   (vector-set! d ip (matrix-ref a ip ip)))
  (let loop ((i 0))
   (if (> i 50) (panic "Too many iterations in JACOBI!"))
   (let ((sm (sum1 (lambda (ip)
		    (sum2 (lambda (ir)
			   (let ((iq (+ ip ir 1)))
			    (abs (matrix-ref a ip iq))))
			  (- n ip 1)))
		   (- n 1))))
    (if (not (zero? sm))
	(begin
	 (let ((tresh (if (< i 3) (/ (* 0.2 sm) (* n n)) 0.0)))
	  (do ((ip 0 (+ ip 1))) ((= ip (- n 1)))
	   (do ((ir 0 (+ ir 1))) ((= ir (- n ip 1)))
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
		(do ((j 0 (+ j 1))) ((= j n))
		 (cond ((< j ip) (rotate a j ip j iq))
		       ((< ip j iq) (rotate a ip j j iq))
		       ((< iq j) (rotate a ip j iq j)))
		 (rotate v j ip j iq)))))))))
	 (do ((ip 0 (+ ip 1))) ((= ip n))
	  (vector-set! b ip (+ (vector-ref b ip) (vector-ref z ip)))
	  (vector-set! d ip (vector-ref b ip))
	  (vector-set! z ip 0.0))
	 (loop (+ i 1))))))
  (do ((i 0 (+ i 1))) ((= i (- n 1)))
   (let ((k i)
	 (p (vector-ref d i)))
    (do ((l 0 (+ l 1))) ((= l (- n i 1)))
     (let* ((j (+ i l 1)))
      (if (>= (vector-ref d j) p)
	  (begin (set! k j) (set! p (vector-ref d j))))))
    (if (not (= k i))
	(begin (vector-set! d k (vector-ref d i))
	       (vector-set! d i p)
	       (do ((j 0 (+ j 1))) ((= j n))
		(let ((p (matrix-ref v j i)))
		 (matrix-set! v j i (matrix-ref v j k))
		 (matrix-set! v j k p)))))))
  (list-two1 d v)))

(define (clip-eigenvalues! a v)
 (let* ((j (jacobi! a))
	(l (first j))
	(e (second1 j)))
  (do ((k1 0 (+ k1 1))) ((= k1 (vector-length a)))
   (let ((a-k1 (vector-ref a k1))
	 (e-k1 (vector-ref e k1)))
    (do ((k2 0 (+ k2 1))) ((= k2 (vector-length a-k1)))
     (let ((e-k2 (vector-ref e k2))
	   (s 0.0))
      (do ((k 0 (+ k 1))) ((= k (vector-length a)))
       (set! s (+ s (* (vector-ref e-k1 k)
		       (max (vector-ref v k) (vector-ref l k))
		       (vector-ref e-k2 k)))))
      (vector-set! a-k1 k2 s)))))))

;;; EM

(define (e-step! x z models)
 (do ((i 0 (+ i 1))) ((= i (vector-length x)))
  (let ((xi (vector-ref x i))
	(zi (vector-ref z i)))
   (do ((j 0 (+ j 1))) ((= j (vector-length models)))
    ;; Compute for each model.
    (let* ((model (vector-ref models j))
	   (log-pi (model-log-pi model))
	   (mu (model-mu model))
	   (sigma-inverse (model-sigma-inverse model))
	   (log-determinant-sigma (model-log-determinant-sigma model))
	   (t 0.0))
     ;; Compute likelihoods (note: up to constant for all models).
     (set! t 0.0)
     (do ((k1 0 (+ k1 1))) ((= k1 (vector-length xi)))
      (let ((sigma-inverse-k1 (vector-ref sigma-inverse k1)))
       (do ((k2 0 (+ k2 1))) ((= k2 (vector-length xi)))
	(set! t (+ t (* (- (vector-ref xi k1) (vector-ref mu k1))
			(vector-ref sigma-inverse-k1 k2)
			(- (vector-ref xi k2) (vector-ref mu k2))))))))
     (vector-set! zi j (- log-pi (* 0.5 (+ log-determinant-sigma t))))))))
 (let ((l 0.0))
  (do ((i 0 (+ i 1))) ((= i (vector-length x)))
   (let ((s minus-infinity)
	 (zi (vector-ref z i)))
    ;; Normalize ownerships to sum to one.
    (do ((j 0 (+ j 1))) ((= j (vector-length models)))
     (set! s (add-exp s (vector-ref zi j))))
    (do ((j 0 (+ j 1))) ((= j (vector-length models)))
     (vector-set! zi j (exp (- (vector-ref zi j) s))))
    (set! l (+ l s))))
  ;; Return log likelihood.
  l))

(define (m-step! x models z clip)
 (let ((kk (vector-length (vector-ref x 0))))
  ;; For each model, optimize parameters.
  (do ((j 0 (+ j 1))) ((= j (vector-length models)))
   (let* ((model (vector-ref models j))
	  (mu (model-mu model))
	  (sigma (model-sigma model))
	  (s 0.0))
    ;; Optimize values.
    (do ((i 0 (+ i 1))) ((= i (vector-length x)))
     (set! s (+ s (vector-ref (vector-ref z i) j))))
    (let ((si (/ s)))
     (do ((k 0 (+ k 1))) ((= k kk))
      (let ((m 0.0))
       (do ((i 0 (+ i 1))) ((= i (vector-length x)))
	(set! m (+ m (* (vector-ref (vector-ref z i) j)
			(vector-ref (vector-ref x i) k)))))
       (vector-set! mu k (* si m)))))
    (let ((si (/ s)))
     (do ((k1 0 (+ k1 1))) ((= k1 kk))
      (let ((sigma-k1 (vector-ref sigma k1))
	    (mu-k1 (vector-ref mu k1)))
       (do ((k2 0 (+ k2 1))) ((= k2 kk))
	(let ((mu-k2 (vector-ref mu k2))
	      (m 0.0))
	 (do ((i 0 (+ i 1))) ((= i (vector-length x)))
	  (set! m (+ m (* (vector-ref (vector-ref z i) j)
			  (* (- (vector-ref (vector-ref x i) k1) mu-k1)
			     (- (vector-ref (vector-ref x i) k2) mu-k2))))))
	 (vector-set! sigma-k1 k2 (* si m)))))))
    (clip-eigenvalues! sigma clip)
    (set-model-pi! model (/ s (vector-length x)))
    (set-model-log-pi! model (log (/ s (vector-length x))))
    (invert-matrix! sigma (model-sigma-inverse model))
    (set-model-log-determinant-sigma! model (log (determinant sigma)))))))

(define (em! x z models clip em-kick-off-tolerance em-convergence-tolerance)
 (let loop ((old-log-likelihood minus-infinity) (starting? #t))
  (let ((log-likelihood (e-step! x z models)))
   (cond
    ((or (and starting? (> log-likelihood old-log-likelihood))
	 (> log-likelihood (+ old-log-likelihood em-convergence-tolerance)))
     (m-step! x models z clip)
     (loop log-likelihood
	   (and starting?
		(not (= (vector-length models) 1))
		(or (= old-log-likelihood minus-infinity)
		    (< log-likelihood
		       (+ old-log-likelihood em-kick-off-tolerance))))))
    (else old-log-likelihood)))))

(define (noise epsilon)
 (- (* 2.0 epsilon (/ (exact->inexact (rand)) *most-positive-fixnum*))
    epsilon))

(define (initial-z ii jj)
 (map-n-vector6
  (lambda (i)
   (let ((zi (map-n-vector7 (lambda (j)
			     (+ (/ (exact->inexact jj))
				(noise (/ (exact->inexact jj)))))
			    jj))
	 (s 0.0))
    (do ((j 0 (+ j 1))) ((= j jj)) (set! s (+ s (vector-ref zi j))))
    (let ((si (/ s)))
     (do ((j 0 (+ j 1))) ((= j jj))
      (vector-set! zi j (* si (vector-ref zi j)))))
    zi))
  ii))

(define (ems x clip em-kick-off-tolerance em-convergence-tolerance
	     ems-convergence-tolerance)
 (let loop ((jj 1)
	    (old-z (void))
	    (old-models (void))
	    (old-log-likelihood minus-infinity))
  (let* ((kk (vector-length (vector-ref x 0)))
	 (z (initial-z (vector-length x) jj))
	 (models (map-n-vector8
		  (lambda (j)
		   ;; needs work: Should replace 0.0 with ((LAMBDA ())).
		   (make-model 0.0
			       (make-vector kk)
			       (make-matrix3 kk kk)
			       0.0
			       (make-matrix4 kk kk)
			       0.0))
		  jj)))
   (m-step! x models z clip)
   (let ((new-log-likelihood
	  (em!
	   x z models clip em-kick-off-tolerance em-convergence-tolerance)))
    (if (> (- (/ old-log-likelihood new-log-likelihood) 1.0)
	   ems-convergence-tolerance)
	(loop (+ jj 1) z models new-log-likelihood)
	(list-two2 old-z old-models))))))

(define (em-clusterer x clip em-kick-off-tolerance em-convergence-tolerance
		      ems-convergence-tolerance)
 (let* ((z-models (ems x clip em-kick-off-tolerance
		       em-convergence-tolerance
		       ems-convergence-tolerance))
	(z (first z-models))
	(models (second2 z-models)))
  (e-step! x z models)
  (let ((clusters
	 (map-n1 (lambda (i)
		  (let ((zi (vector->list (vector-ref z i))))
		   (list-two3
		    i (positionv (reduce max zi minus-infinity) zi))))
		 (vector-length z))))
   (map-n2 (lambda (j)
	    (map (lambda (cluster) (vector-ref x (first cluster)))
		 (remove-if-not (lambda (cluster) (= (second3 cluster) j))
				clusters)))
	   (vector-length (vector-ref z 0))))))

(do ((i 0 (+ i 1))) ((= i 100))
 (write
  (em-clusterer
   '#(#(1.0) #(2.0) #(3.0) #(11.0) #(12.0) #(13.0)) '#(1.0) 10.0 1.0 0.01))
 (newline))
