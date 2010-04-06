;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:         fft.sc
;;; Description:  FFT benchmark from the Gabriel tests.
;;; Author:       Harry Barrow
;;; Created:      8-Apr-85
;;; Modified:     6-May-85 09:29:22 (Bob Shaw)
;;;               11-Aug-87 (Will Clinger)
;;;               16-Nov-94 (Qobi)
;;;               31-Mar-98 (Qobi)
;;; Language:     Scheme
;;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi (atan 0 -1))

;;; FFT -- This is an FFT benchmark written by Harry Barrow.
;;; It tests a variety of floating point operations,
;;; including array references.

(define *re* (make-vector 1025 0.0))

(define *im* (make-vector 1025 0.0))

(define (fft areal aimag)
 (let ((ar areal)			;Qobi
       (ai aimag)			;Qobi
       (i 0)
       (j 0)
       (k 0)
       (m 0)
       (n 0)
       (le 0)
       (le1 0)
       (ip 0)
       (nv2 0)
       (nm1 0)
       (ur 0.0)				;Qobi
       (ui 0.0)				;Qobi
       (wr 0.0)				;Qobi
       (wi 0.0)				;Qobi
       (tr 0.0)				;Qobi
       (ti 0.0))			;Qobi
  ;; initialize
  (set! ar areal)
  (set! ai aimag)
  (set! n (vector-length ar))
  (set! n (- n 1))
  (set! nv2 (quotient n 2))
  (set! nm1 (- n 1))
  (set! m 0)				;compute m = log(n)
  (set! i 1)
  (let loop ()
   (if (< i n)
       (begin (set! m (+ m 1))
	      (set! i (+ i i))
	      (loop))))
  (cond ((not (= n (let loop ((i m) (p 1)) ;Qobi
		    (if (zero? i) p (loop (- i 1) (* 2 p))))))
	 (display "array size not a power of two.")
	 (newline)))
  ;; interchange elements in bit-reversed order
  (set! j 1)
  (set! i 1)
  (let l3 ()
   (cond ((< i j)
	  (set! tr (vector-ref ar j))
	  (set! ti (vector-ref ai j))
	  (vector-set! ar j (vector-ref ar i))
	  (vector-set! ai j (vector-ref ai i))
	  (vector-set! ar i tr)
	  (vector-set! ai i ti)))
   (set! k nv2)
   (let l6 ()
    (cond ((< k j)
	   (set! j (- j k))
	   (set! k (quotient k 2))	;Qobi: was / but this violates R4RS
	   (l6))))
   (set! j (+ j k))
   (set! i (+ i 1))
   (cond ((< i n) (l3))))
  ;; loop thru stages (syntax converted from old MACLISP style \bs)
  (do ((l 1 (+ l 1))) ((> l m))
   (set! le (let loop ((i l) (p 1))	;Qobi
	     (if (zero? i) p (loop (- i 1) (* 2 p)))))
   (set! le1 (quotient le 2))
   (set! ur 1.0)
   (set! ui 0.0)
   (set! wr (cos (/ pi le1)))
   (set! wi (sin (/ pi le1)))
   ;; loop thru butterflies
   (do ((j 1 (+ j 1))) ((> j le1))
    ;; do a butterfly
    (do ((i j (+ i le))) ((> i n))
     (set! ip (+ i le1))
     (set! tr (- (* (vector-ref ar ip) ur) (* (vector-ref ai ip) ui)))
     (set! ti (+ (* (vector-ref ar ip) ui) (* (vector-ref ai ip) ur)))
     (vector-set! ar ip (- (vector-ref ar i) tr))
     (vector-set! ai ip (- (vector-ref ai i) ti))
     (vector-set! ar i (+ (vector-ref ar i) tr))
     (vector-set! ai i (+ (vector-ref ai i) ti))))
   (set! tr (- (* ur wr) (* ui wi)))
   (set! ti (+ (* ur wi) (* ui wr)))
   (set! ur tr)
   (set! ui ti))
  #t))

;;; the timer which does 10 calls on fft

(define (fft-bench)
 (do ((ntimes 0 (+ ntimes 1))) ((= ntimes 10))
  (fft *re* *im*)))

;;; note: The MAKE-VECTOR is not done multiple times.
(do ((i 0 (+ i 1))) ((= i 1000)) (fft-bench))
