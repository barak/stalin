;;; LaHaShem HaAretz U'Mloah

(include "QobiScheme")

(define (read-benchmark-file pathname)
 (let ((benchmarks '())
       (current-benchmark #f))
  (for-each
   (lambda (line)
    (cond
     ((and (= (number-of-fields line) 2)
	   (string=? (field-ref line 0) "run"))
      (when current-benchmark
       (set! benchmarks (cons (reverse current-benchmark) benchmarks)))
      (let ((name (field-ref line 1)))
       (set! current-benchmark
	     ;; Change 0 to 5 to strip off "kilo/".
	     (list (substring name 0 (string-length name))))))
     ;; To handle tcsh version of `time'.
     ((and (= (number-of-fields line) 7) (string=? (field-ref line 4) "0+0k"))
      (set! current-benchmark
	    (cons
	     (+ (string->number
		 (list->string
		  (reverse
		   (rest (reverse (string->list (field-ref line 0)))))))
		(string->number
		 (list->string
		  (reverse
		   (rest (reverse (string->list (field-ref line 1))))))))
	     current-benchmark)))
     ;; To handle /usr/bin/time version of `time'.
     ((and (= (number-of-fields line) 6)
	   (string=? (field-ref line 5) "0maxresident)k"))
      (set! current-benchmark
	    (cons
	     (+ (string->number
		 (list->string
		  (reverse
		   (rest
		    (rest
		     (rest
		      (rest
		       (reverse (string->list (field-ref line 0))))))))))
		(string->number
		 (list->string
		  (reverse
		   (rest
		    (rest
		     (rest
		      (rest
		       (rest
			(rest
			 (reverse (string->list (field-ref line 1)))))))))))))
	     current-benchmark)))))
   (read-file pathname))
  (when current-benchmark
   (set! benchmarks (cons (reverse current-benchmark) benchmarks)))
  (reverse benchmarks)))

(define (best-results benchmarks)
 (map (lambda (benchmark)
       (if (null? (rest benchmark))
	   benchmark
	   (list (first benchmark) (reduce min (rest benchmark) #f))))
      benchmarks))

(define (format-absolute-results Stalin-benchmarks
				 Scheme->C-benchmarks
				 Gambit-C-benchmarks
				 Bigloo-benchmarks
				 Chez-benchmarks
				 Chicken-benchmarks
				 latex-port)
 (format latex-port "\\begin{tabular}{|l|r|r|r|r|r|r|}~%")
 (format latex-port "\\hline~%")
 (format latex-port
	 "&\\Stalin&\\SchemeToC&\\GambitC&\\Bigloo&\\Chez&\\Chicken\\\\~%")
 (format latex-port "\\hline~%")
 (for-each
  (lambda (Stalin-benchmark
	   Scheme->C-benchmark
	   Gambit-C-benchmark
	   Bigloo-benchmark
	   Chez-benchmark
	   Chicken-benchmark)
   (unless (and (string=? (first Stalin-benchmark) (first Scheme->C-benchmark))
		(string=? (first Stalin-benchmark) (first Gambit-C-benchmark))
		(string=? (first Stalin-benchmark) (first Bigloo-benchmark))
		(string=? (first Stalin-benchmark) (first Chez-benchmark))
		(string=? (first Stalin-benchmark) (first Chicken-benchmark)))
    (fuck-up))
   (format latex-port "{\\tt ~a}&~a&~a&~a&~a&~a&~a\\\\~%"
	   (first Stalin-benchmark)
	   (if (null? (rest Stalin-benchmark))
	       ""
	       (rounded-number->string (second Stalin-benchmark) 3))
	   (if (null? (rest Scheme->C-benchmark))
	       ""
	       (rounded-number->string (second Scheme->C-benchmark) 3))
	   (if (null? (rest Gambit-C-benchmark))
	       ""
	       (rounded-number->string (second Gambit-C-benchmark) 3))
	   (if (null? (rest Bigloo-benchmark))
	       ""
	       (rounded-number->string (second Bigloo-benchmark) 3))
	   (if (null? (rest Chez-benchmark))
	       ""
	       (rounded-number->string (second Chez-benchmark) 3))
	   (if (null? (rest Chicken-benchmark))
	       ""
	       (rounded-number->string (second Chicken-benchmark) 3))))
  (best-results Stalin-benchmarks)
  (best-results Scheme->C-benchmarks)
  (best-results Gambit-C-benchmarks)
  (best-results Bigloo-benchmarks)
  (best-results Chez-benchmarks)
  (best-results Chicken-benchmarks))
 (format latex-port "\\hline~%")
 (format latex-port "\\end{tabular}~%"))

(define (format-relative-results Stalin-benchmarks
				 Scheme->C-benchmarks
				 Gambit-C-benchmarks
				 Bigloo-benchmarks
				 Chez-benchmarks
				 Chicken-benchmarks
				 latex-port)
 (format latex-port "\\begin{tabular}{|l|r|r|r|r|r|}~%")
 (format latex-port "\\hline~%")
 (format latex-port "&$\\displaystyle\\frac{\\mbox{\\SchemeToC}}{\\mbox{\\Stalin}}$&$\\displaystyle\\frac{\\mbox{\\GambitC}}{\\mbox{\\Stalin}}$&$\\displaystyle\\frac{\\mbox{\\Bigloo}}{\\mbox{\\Stalin}}$&$\\displaystyle\\frac{\\mbox{\\Chez}}{\\mbox{\\Stalin}}$&$\\displaystyle\\frac{\\mbox{\\Chicken}}{\\mbox{\\Stalin}}$\\\\~%")
 (format latex-port "\\hline~%")
 (for-each
  (lambda (Stalin-benchmark
	   Scheme->C-benchmark
	   Gambit-C-benchmark
	   Bigloo-benchmark
	   Chez-benchmark
	   Chicken-benchmark)
   (unless (and (string=? (first Stalin-benchmark) (first Scheme->C-benchmark))
		(string=? (first Stalin-benchmark) (first Gambit-C-benchmark))
		(string=? (first Stalin-benchmark) (first Bigloo-benchmark))
		(string=? (first Stalin-benchmark) (first Chez-benchmark))
		(string=? (first Stalin-benchmark) (first Chicken-benchmark)))
    (fuck-up))
   (format latex-port "{\\tt ~a}&~a&~a&~a&~a&~a\\\\~%"
	   (first Stalin-benchmark)
	   (if (or (null? (rest Stalin-benchmark))
		   (null? (rest Scheme->C-benchmark)))
	       ""
	       (if (< (second Scheme->C-benchmark) (second Stalin-benchmark))
		   (string-append
		    "{\\bf "
		    (rounded-number->string (/ (second Scheme->C-benchmark)
					       (second Stalin-benchmark))
					    3)
		    "}")
		   (rounded-number->string (/ (second Scheme->C-benchmark)
					      (second Stalin-benchmark))
					   3)))
	   (if (or (null? (rest Stalin-benchmark))
		   (null? (rest Gambit-C-benchmark)))
	       ""
	       (if (< (second Gambit-C-benchmark) (second Stalin-benchmark))
		   (string-append
		    "{\\bf "
		    (rounded-number->string (/ (second Gambit-C-benchmark)
					       (second Stalin-benchmark))
					    3)
		    "}")
		   (rounded-number->string (/ (second Gambit-C-benchmark)
					      (second Stalin-benchmark))
					   3)))
	   (if (or (null? (rest Stalin-benchmark))
		   (null? (rest Bigloo-benchmark)))
	       ""
	       (if (< (second Bigloo-benchmark) (second Stalin-benchmark))
		   (string-append
		    "{\\bf "
		    (rounded-number->string (/ (second Bigloo-benchmark)
					       (second Stalin-benchmark))
					    3)
		    "}")
		   (rounded-number->string (/ (second Bigloo-benchmark)
					      (second Stalin-benchmark))
					   3)))
	   (if (or (null? (rest Stalin-benchmark))
		   (null? (rest Chez-benchmark)))
	       ""
	       (if (< (second Chez-benchmark) (second Stalin-benchmark))
		   (string-append
		    "{\\bf "
		    (rounded-number->string (/ (second Chez-benchmark)
					       (second Stalin-benchmark))
					    3)
		    "}")
		   (rounded-number->string (/ (second Chez-benchmark)
					      (second Stalin-benchmark))
					   3)))
	   (if (or (null? (rest Stalin-benchmark))
		   (null? (rest Chicken-benchmark)))
	       ""
	       (if (< (second Chicken-benchmark) (second Stalin-benchmark))
		   (string-append
		    "{\\bf "
		    (rounded-number->string (/ (second Chicken-benchmark)
					       (second Stalin-benchmark))
					    3)
		    "}")
		   (rounded-number->string (/ (second Chicken-benchmark)
					      (second Stalin-benchmark))
					   3)))))
  (best-results Stalin-benchmarks)
  (best-results Scheme->C-benchmarks)
  (best-results Gambit-C-benchmarks)
  (best-results Bigloo-benchmarks)
  (best-results Chez-benchmarks)
  (best-results Chicken-benchmarks))
 (format latex-port "\\hline~%")
 (format latex-port "\\end{tabular}~%"))

(let ((Stalin-benchmarks (read-benchmark-file "stalin.results"))
      (Scheme->C-benchmarks (read-benchmark-file "s2c.results"))
      (Gambit-C-benchmarks (read-benchmark-file "gambit.results"))
      (Bigloo-benchmarks (read-benchmark-file "bigloo.results"))
      (Chez-benchmarks (read-benchmark-file "chez.results"))
      (Chicken-benchmarks (read-benchmark-file "chicken.results")))
 (call-with-output-file "results.tex"
  (lambda (latex-port)
   (format latex-port "\\documentclass{article}~%")
   (format latex-port "\\usepackage{fullpage}~%")
   (format latex-port "\\begin{document}~%")
   (format latex-port "\\newcommand{\\Stalin}{{\\mbox{\\sc Stalin}}}~%")
   (format latex-port
	   "\\newcommand{\\SchemeToC}{{\\mbox{\\sc Scheme-\\symbol{62}C}}}~%")
   (format latex-port "\\newcommand{\\GambitC}{{\\mbox{\\sc Gambit-C}}}~%")
   (format latex-port "\\newcommand{\\Bigloo}{{\\mbox{\\sc Bigloo}}}~%")
   (format latex-port "\\newcommand{\\Chez}{{\\mbox{\\sc Chez}}}~%")
   (format latex-port "\\newcommand{\\Chicken}{{\\mbox{\\sc Chicken}}}~%")
   (format latex-port "\\begin{center}~%")
   (format-absolute-results Stalin-benchmarks
			    Scheme->C-benchmarks
			    Gambit-C-benchmarks
			    Bigloo-benchmarks
			    Chez-benchmarks
			    Chicken-benchmarks
			    latex-port)
   (format latex-port "\\end{center}~%")
   (format latex-port "\\begin{center}~%")
   (format-relative-results Stalin-benchmarks
			    Scheme->C-benchmarks
			    Gambit-C-benchmarks
			    Bigloo-benchmarks
			    Chez-benchmarks
			    Chicken-benchmarks
			    latex-port)
   (format latex-port "\\end{center}~%")
   (format latex-port "\\end{document}~%"))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
