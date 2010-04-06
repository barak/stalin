(include "QobiScheme")

(define *line-segments* #f)
(define *ellipses* #f)
(define *object* #f)
(define *flag?* #f)
(define *mode* #f)
(define *k* #f)

(define-application main #f 480 5 2 6
 (lambda ()
  (set! *line-segments* '())
  (set! *ellipses* '())
  (set! *object* 'line-segment)
  (set! *flag?* #f)
  (set! *mode* 'a)
  (set! *k* 0)
  (define-button 0 0 "Help" #f help-command)
  (define-radio-buttons *object* (lambda () #f)
   (1 0 line-segment "Line")
   (2 0 ellipse "Ellipse"))
  (define-button 5 0 "Quit" #f quit)
  (define-toggle-button 0 1 "Flag" *flag?*
   (lambda () (say (format #f "Flag=~s" *flag?*))))
  (define-cycle-button 1 1 *mode*
   (lambda () (say (format #f "Mode=~s" *mode*)))
   (a "A")
   (b "B")
   (c "C"))
  (define-integer-range-buttons 2 1 3 1 *k* 0 9
   (lambda () (format #f "-K ~s" *k*))
   (lambda () (format #f "+K ~s" *k*))
   (lambda () (say (format #f "K=~s" *k*))))
  (define-key (list (control #\x) (control #\c)) "Quit" quit)
  (define-key (control #\h) "Help" help-command))
 (lambda () #f)
 (lambda () #f)
 (lambda ()
  (for-each (lambda (l)
	     (xdrawline *display* *display-pane* *thin-gc*
			(x (line-segment-p l))
			(y (line-segment-p l))
			(x (line-segment-q l))
			(y (line-segment-q l))))
	    *line-segments*)
  (for-each (lambda (c)
	     (xdrawarc *display* *display-pane* *thin-gc*
		       (x (line-segment-p c))
		       (y (line-segment-p c))
		       (- (x (line-segment-q c)) (x (line-segment-p c)))
		       (- (y (line-segment-q c)) (y (line-segment-p c)))
		       0 (* 360 64)))
	    *ellipses*)
  (define-region 0 0 *display-pane-width* *display-pane-height*
   (lambda (x1 y1)
    (case *object*
     ((line-segment)
      (let* ((result (tracking-pointer
		      #t
		      (lambda (x2 y2)
		       (xdrawline *display* *display-pane* *thin-flipping-gc*
				  x1 y1 x2 y2))))
	     (x2 (first result))
	     (y2 (second result)))
       (set! *line-segments*
	     (cons (make-line-segment (vector x1 y1) (vector x2 y2))
		   *line-segments*))
       (redraw-display-pane)))
     ((ellipse)
      (let* ((result (tracking-pointer
		      #t
		      (lambda (x2 y2)
		       (xdrawarc *display* *display-pane* *thin-flipping-gc*
				 x1 y1 (- x2 x1) (- y2 y1)
				 0 (* 360 64)))))
	     (x2 (first result))
	     (y2 (second result)))
       (set! *ellipses*
	     (cons (make-line-segment (vector x1 y1) (vector x2 y2))
		   *ellipses*))
       (redraw-display-pane)))
     (else (fuck-up)))))))

(main '())
