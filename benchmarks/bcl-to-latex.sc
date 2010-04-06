;;; LaHaShem HaAretz U'Mloah

(include "QobiScheme")

;;; note: There is no notion of VARIABLE-USED?, PROCEDURE-USED?, and
;;;       REACHABLE?

(define (best-run-time times)
 (unless (= (length times) 3) (fuck-up))
 (reduce min times infinity))

(define (last-pair list)
 (if (null? (rest list)) list (last-pair (rest list))))

(let ((mode #f)
      (running? #f)
      (baseline '())
      (conventional '())
      (lightweight '())
      (benchmark-name #f))
 (define (baseline-number-of-variables benchmark)
  (list-ref (assoc benchmark baseline) 1))
 (define (baseline-number-of-assigned-variables benchmark)
  (list-ref (assoc benchmark baseline) 2))
 (define (baseline-number-of-accessed-variables benchmark)
  (list-ref (assoc benchmark baseline) 3))
 (define (baseline-number-of-nonfictitious-variables benchmark)
  (list-ref (assoc benchmark baseline) 4))
 (define (baseline-number-of-local-variables benchmark)
  (list-ref (assoc benchmark baseline) 5))
 (define (baseline-number-of-global-variables benchmark)
  (list-ref (assoc benchmark baseline) 6))
 (define (baseline-number-of-hidden-variables benchmark)
  (list-ref (assoc benchmark baseline) 7))
 (define (baseline-number-of-slotted-variables benchmark)
  (list-ref (assoc benchmark baseline) 8))
 (define (baseline-number-of-procedures benchmark)
  (list-ref (assoc benchmark baseline) 9))
 (define (baseline-number-of-procedures-with-closures benchmark)
  (list-ref (assoc benchmark baseline) 10))
 (define (baseline-number-of-procedures-with-parent-slots benchmark)
  (list-ref (assoc benchmark baseline) 11))
 (define (baseline-number-of-procedures-with-parent-parameters benchmark)
  (list-ref (assoc benchmark baseline) 12))
 (define (baseline-number-of-procedures-with-parent-slot-compression
	  benchmark)
  (list-ref (assoc benchmark baseline) 13))
 (define (baseline-number-of-procedures-with-parent-parameter-compression
	  benchmark)
  (list-ref (assoc benchmark baseline) 14))
 (define (baseline-average-number-of-reference-indirections benchmark)
  (list-ref (assoc benchmark baseline) 15))
 (define (baseline-number-of-accesses benchmark)
  (list-ref (assoc benchmark baseline) 16))
 (define (baseline-number-of-nontrivial-accesses benchmark)
  (list-ref (assoc benchmark baseline) 17))
 (define (baseline-number-of-assignments benchmark)
  (list-ref (assoc benchmark baseline) 18))
 (define (baseline-number-of-nontrivial-assignments benchmark)
  (list-ref (assoc benchmark baseline) 19))
 (define (baseline-run-time benchmark)
  (best-run-time
   (rest
    (rest
     (rest
      (rest
       (rest
	(rest
	 (rest
	  (rest
	   (rest
	    (rest
	     (rest
	      (rest
	       (rest
		(rest
		 (rest
		  (rest
		   (rest
		    (rest
		     (rest
		      (rest
		       (assoc benchmark baseline)))))))))))))))))))))))
 (define (conventional-number-of-variables benchmark)
  (list-ref (assoc benchmark conventional) 1))
 (define (conventional-number-of-assigned-variables benchmark)
  (list-ref (assoc benchmark conventional) 2))
 (define (conventional-number-of-accessed-variables benchmark)
  (list-ref (assoc benchmark conventional) 3))
 (define (conventional-number-of-nonfictitious-variables benchmark)
  (list-ref (assoc benchmark conventional) 4))
 (define (conventional-number-of-local-variables benchmark)
  (list-ref (assoc benchmark conventional) 5))
 (define (conventional-number-of-global-variables benchmark)
  (list-ref (assoc benchmark conventional) 6))
 (define (conventional-number-of-hidden-variables benchmark)
  (list-ref (assoc benchmark conventional) 7))
 (define (conventional-number-of-slotted-variables benchmark)
  (list-ref (assoc benchmark conventional) 8))
 (define (conventional-number-of-procedures benchmark)
  (list-ref (assoc benchmark conventional) 9))
 (define (conventional-number-of-procedures-with-closures benchmark)
  (list-ref (assoc benchmark conventional) 10))
 (define (conventional-number-of-procedures-with-parent-slots benchmark)
  (list-ref (assoc benchmark conventional) 11))
 (define (conventional-number-of-procedures-with-parent-parameters benchmark)
  (list-ref (assoc benchmark conventional) 12))
 (define (conventional-number-of-procedures-with-parent-slot-compression
	  benchmark)
  (list-ref (assoc benchmark conventional) 13))
 (define (conventional-number-of-procedures-with-parent-parameter-compression
	  benchmark)
  (list-ref (assoc benchmark conventional) 14))
 (define (conventional-average-number-of-reference-indirections benchmark)
  (list-ref (assoc benchmark conventional) 15))
 (define (conventional-number-of-accesses benchmark)
  (list-ref (assoc benchmark conventional) 16))
 (define (conventional-number-of-nontrivial-accesses benchmark)
  (list-ref (assoc benchmark conventional) 17))
 (define (conventional-number-of-assignments benchmark)
  (list-ref (assoc benchmark conventional) 18))
 (define (conventional-number-of-nontrivial-assignments benchmark)
  (list-ref (assoc benchmark conventional) 19))
 (define (conventional-run-time benchmark)
  (best-run-time
   (rest
    (rest
     (rest
      (rest
       (rest
	(rest
	 (rest
	  (rest
	   (rest
	    (rest
	     (rest
	      (rest
	       (rest
		(rest
		 (rest
		  (rest
		   (rest
		    (rest
		     (rest
		      (rest
		       (assoc benchmark conventional)))))))))))))))))))))))
 (define (lightweight-number-of-variables benchmark)
  (list-ref (assoc benchmark lightweight) 1))
 (define (lightweight-number-of-assigned-variables benchmark)
  (list-ref (assoc benchmark lightweight) 2))
 (define (lightweight-number-of-accessed-variables benchmark)
  (list-ref (assoc benchmark lightweight) 3))
 (define (lightweight-number-of-nonfictitious-variables benchmark)
  (list-ref (assoc benchmark lightweight) 4))
 (define (lightweight-number-of-local-variables benchmark)
  (list-ref (assoc benchmark lightweight) 5))
 (define (lightweight-number-of-global-variables benchmark)
  (list-ref (assoc benchmark lightweight) 6))
 (define (lightweight-number-of-hidden-variables benchmark)
  (list-ref (assoc benchmark lightweight) 7))
 (define (lightweight-number-of-slotted-variables benchmark)
  (list-ref (assoc benchmark lightweight) 8))
 (define (lightweight-number-of-procedures benchmark)
  (list-ref (assoc benchmark lightweight) 9))
 (define (lightweight-number-of-procedures-with-closures benchmark)
  (list-ref (assoc benchmark lightweight) 10))
 (define (lightweight-number-of-procedures-with-parent-slots benchmark)
  (list-ref (assoc benchmark lightweight) 11))
 (define (lightweight-number-of-procedures-with-parent-parameters benchmark)
  (list-ref (assoc benchmark lightweight) 12))
 (define (lightweight-number-of-procedures-with-parent-slot-compression
	  benchmark)
  (list-ref (assoc benchmark lightweight) 13))
 (define (lightweight-number-of-procedures-with-parent-parameter-compression
	  benchmark)
  (list-ref (assoc benchmark lightweight) 14))
 (define (lightweight-average-number-of-reference-indirections benchmark)
  (list-ref (assoc benchmark lightweight) 15))
 (define (lightweight-number-of-accesses benchmark)
  (list-ref (assoc benchmark lightweight) 16))
 (define (lightweight-number-of-nontrivial-accesses benchmark)
  (list-ref (assoc benchmark lightweight) 17))
 (define (lightweight-number-of-assignments benchmark)
  (list-ref (assoc benchmark lightweight) 18))
 (define (lightweight-number-of-nontrivial-assignments benchmark)
  (list-ref (assoc benchmark lightweight) 19))
 (define (lightweight-run-time benchmark)
  (best-run-time
   (rest
    (rest
     (rest
      (rest
       (rest
	(rest
	 (rest
	  (rest
	   (rest
	    (rest
	     (rest
	      (rest
	       (rest
		(rest
		 (rest
		  (rest
		   (rest
		    (rest
		     (rest
		      (rest
		       (assoc benchmark lightweight)))))))))))))))))))))))
 (for-each
  (lambda (line)
   (cond
    ((and (= (number-of-fields line) 1)
	  (string=? (field-ref line 0) "baseline"))
     (set! mode 'baseline))
    ((and (= (number-of-fields line) 1)
	  (string=? (field-ref line 0) "conventional"))
     (set! mode 'conventional))
    ((and (= (number-of-fields line) 1)
	  (string=? (field-ref line 0) "lightweight"))
     (set! mode 'lightweight))
    ((and (= (number-of-fields line) 2)
	  (string=? (field-ref line 0) "compile"))
     (set! running? #f)
     (set! benchmark-name (field-ref line 1)))
    ((and (= (number-of-fields line) 2)
	  (string=? (field-ref line 0) "run"))
     (set! running? #t)
     (set! benchmark-name (field-ref line 1)))
    ((and (= (number-of-fields line) 20)
	  (string=? (field-ref line 0) "(STATIC-COUNTS"))
     (case mode
      ((baseline)
       (set! baseline
	     (cons (cons benchmark-name (rest (read-from-string line)))
		   baseline)))
      ((conventional)
       (set! conventional
	     (cons (cons benchmark-name (rest (read-from-string line)))
		   conventional)))
      ((lightweight)
       (set! lightweight
	     (cons (cons benchmark-name (rest (read-from-string line)))
		   lightweight)))
      (else (fuck-up))))
    ((and running?
	  (= (number-of-fields line) 6)
	  (>= (string-length (field-ref line 0)) 4)
	  (string=? (substring (field-ref line 0)
			       (- (string-length (field-ref line 0)) 4)
			       (string-length (field-ref line 0)))
		    "user"))
     (set-cdr!
      (last-pair (assoc benchmark-name
			(case mode
			 ((baseline) baseline)
			 ((conventional) conventional)
			 ((lightweight) lightweight)
			 (else (fuck-up)))))
      (list (+ (string->number
		(list->string
		 (reverse
		  (rest
		   (rest
		    (rest
		     (rest
		      (reverse
		       (string->list (field-ref line 0))))))))))
	       (string->number
		(list->string
		 (reverse
		  (rest
		   (rest
		    (rest
		     (rest
		      (rest
		       (rest
			(reverse
			 (string->list (field-ref line 1))))))))))))))))))
  (read-file "bcl.text"))
 (call-with-output-file "bcl.tex"
  (lambda (latex-port)
   (format latex-port "\\documentclass{article}~%")
   (format latex-port "\\usepackage{fullpage}~%")
   (format latex-port "\\begin{document}~%")
   (format latex-port "\\begin{tiny}~%")
   (format latex-port "\\begin{center}~%")
   (format latex-port "\\begin{tabular}{|l|r|r|r|r|r|r|r|r|r|}~%")
   (format latex-port "\\hline~%")
   (format latex-port "&\\multicolumn{1}{|l}{variables}&\\multicolumn{1}{|l}{$\\neg$assigned}&\\multicolumn{1}{|l}{$\\neg$accessed}&\\multicolumn{1}{|l}{fictitious}&\\multicolumn{1}{|l}{eliminated}&\\multicolumn{1}{|l}{local}&\\multicolumn{1}{|l}{global}&\\multicolumn{1}{|l}{hidden}&\\multicolumn{1}{|l|}{slotted}\\\\~%")
   (format latex-port "\\hline~%")
   (for-each
    (lambda (benchmark)
     (format latex-port "\\hline~%")
     (format
      latex-port "{\\tt ~a}&~a&~a&~a&~a&~a&~a&~a&~a&~a\\\\~%"
      benchmark
      (number->string (baseline-number-of-variables benchmark))
      (number->string
       (- (baseline-number-of-variables benchmark)
	  (baseline-number-of-assigned-variables benchmark)))
      (number->string
       (- (baseline-number-of-variables benchmark)
	  (baseline-number-of-accessed-variables benchmark)))
      (number->string
       (- (baseline-number-of-variables benchmark)
	  (baseline-number-of-nonfictitious-variables benchmark)))
      (number->string
       (- (baseline-number-of-variables benchmark)
	  (+ (baseline-number-of-local-variables benchmark)
	     (baseline-number-of-global-variables benchmark)
	     (baseline-number-of-hidden-variables benchmark)
	     (baseline-number-of-slotted-variables benchmark))))
      (number->string (baseline-number-of-local-variables benchmark))
      (number->string (baseline-number-of-global-variables benchmark))
      (number->string (baseline-number-of-hidden-variables benchmark))
      (number->string (baseline-number-of-slotted-variables benchmark)))
     (format
      latex-port "&~a&~a&~a&~a&~a&~a&~a&~a&~a\\\\~%"
      (number->string (conventional-number-of-variables benchmark))
      (number->string
       (- (conventional-number-of-variables benchmark)
	  (conventional-number-of-assigned-variables benchmark)))
      (number->string
       (- (conventional-number-of-variables benchmark)
	  (conventional-number-of-accessed-variables benchmark)))
      (number->string
       (- (conventional-number-of-variables benchmark)
	  (conventional-number-of-nonfictitious-variables benchmark)))
      (number->string
       (- (conventional-number-of-variables benchmark)
	  (+ (conventional-number-of-local-variables benchmark)
	     (conventional-number-of-global-variables benchmark)
	     (conventional-number-of-hidden-variables benchmark)
	     (conventional-number-of-slotted-variables benchmark))))
      (number->string (conventional-number-of-local-variables benchmark))
      (number->string (conventional-number-of-global-variables benchmark))
      (number->string (conventional-number-of-hidden-variables benchmark))
      (number->string (conventional-number-of-slotted-variables benchmark)))
     (format
      latex-port "&~a&~a&~a&~a&~a&~a&~a&~a&~a\\\\~%"
      (number->string (lightweight-number-of-variables benchmark))
      (number->string
       (- (lightweight-number-of-variables benchmark)
	  (lightweight-number-of-assigned-variables benchmark)))
      (number->string
       (- (lightweight-number-of-variables benchmark)
	  (lightweight-number-of-accessed-variables benchmark)))
      (number->string
       (- (lightweight-number-of-variables benchmark)
	  (lightweight-number-of-nonfictitious-variables benchmark)))
      (number->string
       (- (lightweight-number-of-variables benchmark)
	  (+ (lightweight-number-of-local-variables benchmark)
	     (lightweight-number-of-global-variables benchmark)
	     (lightweight-number-of-hidden-variables benchmark)
	     (lightweight-number-of-slotted-variables benchmark))))
      (number->string (lightweight-number-of-local-variables benchmark))
      (number->string (lightweight-number-of-global-variables benchmark))
      (number->string (lightweight-number-of-hidden-variables benchmark))
      (number->string (lightweight-number-of-slotted-variables benchmark))))
    (reverse (remove-duplicates (append (map first lightweight)
					(map first baseline)
					(map first conventional)))))
   (format latex-port "\\hline~%")
   (format latex-port "\\end{tabular}~%")
   (format latex-port "\\end{center}~%")
   (format latex-port "\\end{tiny}~%")
   (format latex-port "\\begin{tiny}~%")
   (format latex-port "\\begin{center}~%")
   (format latex-port "\\begin{tabular}{|l|r|r|r|r|r|r|}~%")
   (format latex-port "\\hline~%")
   (format latex-port "&\\multicolumn{1}{|l}{procedures}&\\multicolumn{1}{|l}{closures}&\\multicolumn{1}{|l}{parent}&\\multicolumn{1}{|l}{parent}&\\multicolumn{1}{|l}{parent}&\\multicolumn{1}{|l|}{parent}\\\\~%")
   (format latex-port "&&&\\multicolumn{1}{|l}{slots}&\\multicolumn{1}{|l}{parameters}&\\multicolumn{1}{|l}{slot}&\\multicolumn{1}{|l|}{parameter}\\\\~%")
   (format latex-port "&&&&&\\multicolumn{1}{|l}{compression}&\\multicolumn{1}{|l|}{compression}\\\\~%")
   (format latex-port "\\hline~%")
   (for-each
    (lambda (benchmark)
     (format latex-port "\\hline~%")
     (format
      latex-port "{\\tt ~a}&~a&~a&~a&~a&~a&~a\\\\~%"
      benchmark
      (number->string (baseline-number-of-procedures benchmark))
      (number->string
       (baseline-number-of-procedures-with-closures benchmark))
      (number->string
       (baseline-number-of-procedures-with-parent-slots benchmark))
      (number->string
       (baseline-number-of-procedures-with-parent-parameters benchmark))
      (number->string
       (baseline-number-of-procedures-with-parent-slot-compression
	benchmark))
      (number->string
       (baseline-number-of-procedures-with-parent-parameter-compression
	benchmark)))
     (format
      latex-port "&~a&~a&~a&~a&~a&~a\\\\~%"
      (number->string (conventional-number-of-procedures benchmark))
      (number->string
       (conventional-number-of-procedures-with-closures benchmark))
      (number->string
       (conventional-number-of-procedures-with-parent-slots benchmark))
      (number->string
       (conventional-number-of-procedures-with-parent-parameters benchmark))
      (number->string
       (conventional-number-of-procedures-with-parent-slot-compression
	benchmark))
      (number->string
       (conventional-number-of-procedures-with-parent-parameter-compression
	benchmark)))
     (format
      latex-port "&~a&~a&~a&~a&~a&~a\\\\~%"
      (number->string (lightweight-number-of-procedures benchmark))
      (number->string
       (lightweight-number-of-procedures-with-closures benchmark))
      (number->string
       (lightweight-number-of-procedures-with-parent-slots benchmark))
      (number->string
       (lightweight-number-of-procedures-with-parent-parameters benchmark))
      (number->string
       (lightweight-number-of-procedures-with-parent-slot-compression
	benchmark))
      (number->string
       (lightweight-number-of-procedures-with-parent-parameter-compression
	benchmark))))
    (reverse (remove-duplicates (append (map first lightweight)
					(map first baseline)
					(map first conventional)))))
   (format latex-port "\\hline~%")
   (format latex-port "\\end{tabular}~%")
   (format latex-port "\\end{center}~%")
   (format latex-port "\\end{tiny}~%")
   (format latex-port "\\begin{tiny}~%")
   (format latex-port "\\begin{center}~%")
   (format latex-port "\\begin{tabular}{|l|r|r|r|r|r|r|}~%")
   (format latex-port "\\hline~%")
   (format latex-port "&\\multicolumn{1}{|l}{average}&\\multicolumn{1}{|l}{accesses}&\\multicolumn{1}{|l}{nontrivial}&\\multicolumn{1}{|l}{assignments}&\\multicolumn{1}{|l}{nontrivial}&\\multicolumn{1}{|l|}{run}\\\\~%")
   (format latex-port "&\\multicolumn{1}{|l|}{indirections}&&\\multicolumn{1}{|l|}{accesses}&&\\multicolumn{1}{|l}{assignments}&\\multicolumn{1}{|l|}{time}\\\\~%")
   (format latex-port "&\\multicolumn{1}{|l|}{per reference}&&&&&\\\\~%")
   (format latex-port "\\hline~%")
   (for-each
    (lambda (benchmark)
     (format latex-port "\\hline~%")
     (format
      latex-port "{\\tt ~a}&~a&~a&~a&~a&~a&~a\\\\~%"
      benchmark
      (rounded-number->string
       (baseline-average-number-of-reference-indirections benchmark) 3)
      (number->string (baseline-number-of-accesses benchmark))
      (number->string (baseline-number-of-nontrivial-accesses benchmark))
      (number->string (baseline-number-of-assignments benchmark))
      (number->string (baseline-number-of-nontrivial-assignments benchmark))
      (rounded-number->string (baseline-run-time benchmark) 3))
     (format
      latex-port "&~a&~a&~a&~a&~a&~a\\\\~%"
      (rounded-number->string
       (conventional-average-number-of-reference-indirections benchmark) 3)
      (number->string (conventional-number-of-accesses benchmark))
      (number->string (conventional-number-of-nontrivial-accesses benchmark))
      (number->string (conventional-number-of-assignments benchmark))
      (number->string
       (conventional-number-of-nontrivial-assignments benchmark))
      (rounded-number->string (conventional-run-time benchmark) 3))
     (format
      latex-port "&~a&~a&~a&~a&~a&~a\\\\~%"
      (rounded-number->string
       (lightweight-average-number-of-reference-indirections benchmark) 3)
      (number->string (lightweight-number-of-accesses benchmark))
      (number->string (lightweight-number-of-nontrivial-accesses benchmark))
      (number->string (lightweight-number-of-assignments benchmark))
      (number->string (lightweight-number-of-nontrivial-assignments benchmark))
      (rounded-number->string (lightweight-run-time benchmark) 3)))
    (reverse (remove-duplicates (append (map first lightweight)
					(map first baseline)
					(map first conventional)))))
   (format latex-port "\\hline~%")
   (format latex-port "\\end{tabular}~%")
   (format latex-port "\\end{center}~%")
   (format latex-port "\\end{tiny}~%")
   (format latex-port "\\end{document}~%"))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
