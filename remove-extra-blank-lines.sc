;;; LaHaShem HaAretz U'Mloah

(module main (main main))

(define (read-line . port)
 (let loop ((chars '()))
  (let ((char (read-char)))
   (if (eof-object? char)
       (if (null? chars) char (list->string (reverse chars)))
       (if (char=? char #\newline)
	   (list->string (reverse chars))
	   (loop (cons char chars)))))))

(define (main)
 (let loop ((flag? #t))
  (let ((line (read-line)))
   (unless (eof-object? line)
    (cond ((string=? line "")
	   (unless flag?
	    (display line)
	    (newline))
	   (loop #t))
	  (else (display line)
		(newline)
		(loop #f)))))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
