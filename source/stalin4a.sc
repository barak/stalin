;;; LaHaShem HaAretz U'Mloah

;;; Stalin 0.11 - A global optimizing compiler for Scheme
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997, 1998, 1999, 2000, and 2001 NEC Research Institute, Inc. All
;;; rights reserved.
;;; Copyright 2002, 2003, 2004, 2005, and 2006 Purdue University. All rights
;;; reserved.

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
;;;    School of Electrical and Computer Engineering
;;;    Purdue University
;;;    Electrical Engineering Building, Room 330
;;;    465 Northwestern Avenue
;;;    West Lafayette IN 47907-2035 USA
;;;    voice: 765/496-3197
;;;    fax: 765/494-6440
;;;    qobi@purdue.edu
;;;    http://www.ece.purdue.edu/~qobi

;;; Begin delete for Trotsky
(module stalin4a)

(include "QobiScheme.sch")
(include "stalin4a.sch")
;;; End delete for Trotsky

;;; Architecture parameters

;;; needs work: Character constants, string constants, and ARGV will screw up
;;;             if *CHAR* is not "char". Exact integer constants will screw up
;;;             if *FIXNUM* is not "int". Inexact constants will screw up if
;;;             *FLONUM* is not "double". Subscript constants will screw up if
;;;             *LENGTH* is not "int". Type constants will screw up if *TAG* is
;;;             not "int".
(define *char* #f)
(define *fixnum* #f)
(define *flonum* #f)
(define *length* #f)
(define *tag* #f)
(define *squished* #f)
(define *signed-squished* #f)
(define *file* #f)
(define *jmpbuf* #f)
(define *char-alignment* #f)
(define *fixnum-alignment* #f)
(define *flonum-alignment* #f)
;;; This is a limitation. We can only generate code on architectures where
;;; all pointers have the same alignment.
(define *pointer-alignment* #f)
(define *length-alignment* #f)
(define *tag-alignment* #f)
(define *squished-alignment* #f)
(define *file-alignment* #f)
(define *jmpbuf-alignment* #f)
(define *char-size* #f)
(define *fixnum-size* #f)
(define *flonum-size* #f)
;;; This is a limitation. We can only generate code on architectures where
;;; all pointers have the same size.
(define *pointer-size* #f)
(define *length-size* #f)
(define *tag-size* #f)
(define *squished-size* #f)
;;; For AIX
(define *include-malloc-for-alloca?* #f)

;;; Derived alignments

(define *worst-alignment* #f)
(define *allocation-alignment* #f)

;;; Alignment check flags

(define *char-alignment?* #f)
(define *fixnum-alignment?* #f)
(define *flonum-alignment?* #f)
(define *rectangular-alignment?* #f)
(define *void*-alignment?* #f)
(define *char*-alignment?* #f)
(define *file*-alignment?* #f)
(define *jmpbuf*-alignment?* #f)
(define *length-alignment?* #f)
(define *tag-alignment?* #f)
(define *squished-alignment?* #f)
(define *file-alignment?* #f)
(define *jmpbuf-alignment?* #f)

;;; Size check flags

(define *char-size?* #f)
(define *fixnum-size?* #f)
(define *flonum-size?* #f)
(define *rectangular-size?* #f)
(define *void*-size?* #f)
(define *char*-size?* #f)
(define *file*-size?* #f)
(define *jmpbuf*-size?* #f)
(define *length-size?* #f)
(define *tag-size?* #f)
(define *squished-size?* #f)

(define *uss* #f)
(define *strings* #f)
(define *symbols* #f)
(define *outside-main* #f)
(define *inside-main* #f)
(define *outside-body* #f)
(define *discard* #f)

;;; C surface-syntax predicates and accessors

;;; needs work: To replace the calls to LIST? and LENGTH in the following with
;;;             more efficient code:

(define (c:noop? c)
 (or (and (pair? c) (c:noop? (car c)) (c:noop? (cdr c)))
     (null? c)
     (and (string? c) (zero? (string-length c)))))

(define (c:whitespace? c)
 (or (and (pair? c) (c:whitespace? (car c)) (c:whitespace? (cdr c)))
     (null? c)
     (and (char? c) (char=? c #\newline))
     (and (string? c) (or (string=? c "") (string=? c " ")))))

(define (c:/**/? c)
 (or (c:whitespace? c)
     (and (pair? c) (c:/**/? (car c)) (c:/**/? (cdr c)))
     (and (list? c)
	  (= (length c) 3)
	  (string? (first c))
	  (string=? (first c) "/*")
	  (string? (second c))
	  (string=? (second c) " ")
	  (list? (third c))
	  (= (length (third c)) 3)
	  (string? (first (third c)))
	  (string? (second (third c)))
	  (string=? (second (third c)) " ")
	  (string? (third (third c)))
	  (string=? (third (third c)) "*/"))))

(define (c:declaration? c)
 (and (list? c) (= (length c) 4) (eq? (first c) 'c:declaration)))

(define (c:protect? c)
 (and (list? c) (= (length c) 2) (eq? (first c) 'c:protect)))

(define (c:protected? c)
 (or (eq? c 'c:protect)
     (and (pair? c) (or (c:protected? (car c)) (c:protected? (cdr c))))))

(define (c:no-return? c)
 (and (list? c) (= (length c) 2) (eq? (first c) 'c:no-return)))

(define (c:parentheses? c)
 (or (and (c:no-return? c) (c:parentheses? (second c)))
     (and (c:protect? c) (c:parentheses? (second c)))
     (and (list? c)
	  (= (length c) 3)
	  (string? (first c))
	  (string=? (first c) "(")
	  (string? (third c))
	  (string=? (third c) ")"))))

(define (c:strip c)
 (cond ((c:no-return? c) (c:strip (second c)))
       ((c:protect? c) (c:strip (second c)))
       ((c:parentheses? c) (c:strip (second c)))
       (else c)))

(define (c:match? c1 c2)
 (define (c:match c1 c2)
  (define (augment variable variables)
   ;; conventions: VARIABLE VARIABLES
   (cond
    ((eq? variables #f) #f)
    ((member variable variables) variables)
    ((some (lambda (other-variable)
	    ;; conventions: OTHER-VARIABLE
	    (equal? (car variable) (car other-variable)))
	   variables)
     #f)
    (else (cons variable variables))))
  (define (merge variables1 variables2)
   ;; conventions: VARIABLE1 VARIABLE2
   (cond
    ((eq? variables1 #f) #f)
    ((null? variables1) variables2)
    (else (merge (rest variables1) (augment (first variables1) variables2)))))
  (cond ((or (and (char? c1) (char? c2) (char=? c1 c2))
	     (and (string? c1) (string? c2) (string=? c1 c2))
	     (and (number? c1) (number? c2) (= c1 c2))
	     (and (null? c1) (null? c2))
	     (and (symbol? c1) (symbol? c2) (eq? c1 c2)))
	 '())
	((and (pair? c1) (pair? c2))
	 (if (and (string? (car c1))
		  (string? (car c2))
		  (string=? (car c1) "t")
		  (string=? (car c2) "t")
		  (pair? (cdr c1))
		  (pair? (cdr c2))
		  (integer? (cadr c1))
		  (integer? (cadr c2))
		  (null? (cddr c1))
		  (null? (cddr c2)))
	     (list (cons c1 c2))
	     (merge (c:match (car c1) (car c2)) (c:match (cdr c1) (cdr c2)))))
	(else #f)))
 (not (eq? (c:match c1 c2) #f)))

(define (c:assignment? c)
 (and (list? c)
      (= (length c) 2)
      (list? (first c))
      (= (length (first c)) 2)
      (eq? (first (first c)) 'c:protect)
      (list? (second (first c)))
      (= (length (second (first c))) 3)
      (string? (second (second (first c))))
      (string=? (second (second (first c))) " ")
      (list? (third (second (first c))))
      (= (length (third (second (first c)))) 3)
      (string? (first (third (second (first c)))))
      (string=? (first (third (second (first c)))) "=")
      (string? (second (third (second (first c)))))
      (string=? (second (third (second (first c)))) " ")
      (string? (second c))
      (string=? (second c) ";")))

(define (c:atomic-t? c)
 (and (list? c)
      (= (length c) 2)
      (string? (first c))
      (string=? (first c) "t")
      (string? (second c))))

(define (c:t? c)
 (or (and (list? c)
	  (= (length c) 2)
	  (string? (first c))
	  (string=? (first c) "t")
	  (string? (second c)))
     (and (list? c)
	  (= (length c) 3)
	  (c:t? (first c))
	  (string? (second c))
	  (string=? (second c) "."))))

(define (c:atomic-t c)
 (if (and (list? c)
	  (= (length c) 3)
	  (c:t? (first c))
	  (string? (second c))
	  (string=? (second c) "."))
     (c:atomic-t (first c))
     c))

(define (c:assignment-to-temporary? c)
 (and (c:assignment? c) (c:t? (first (second (first c))))))

(define (c:unprotected-assignment-to-atomic-temporary? c)
 (and (c:assignment? c)
      (c:atomic-t? (first (second (first c))))
      (not (c:protected? (third (third (second (first c))))))))

(define (c:label? c)
 (and (list? c)
      (= (length c) 2)
      (list? (first c))
      (= (length (first c)) 2)
      (string? (first (first c)))
      (or (string=? (first (first c)) "h")
	  (string=? (first (first c)) "l")
	  (string=? (first (first c)) "x"))
      (string? (second (first c)))
      (string? (second c))
      (string=? (second c) ":")))

;;; C surface-syntax constructors

(define (outside-main c)
 (unless (c:noop? c) (set! *outside-main* (cons c *outside-main*))))

(define (inside-main c)
 (unless (c:noop? c)
  ;; needs work: To replace the call to LIST? and LENGTH in the following with
  ;;             more efficient code:
  (cond ((and (list? c)
	      (= (length c) 3)
	      (char? (second c))
	      (char=? (second c) #\newline))
	 (inside-main (first c))
	 (inside-main (third c)))
	(else (set! *inside-main* (cons c *inside-main*))))))

(define (outside-body c)
 (unless (c:noop? c) (set! *outside-body* (cons c *outside-body*))))

(define (c:protect c) (list 'c:protect c))

(define (c:no-return c) (list 'c:no-return c))

(define (spaces-between cs)
 (define (space-between c1 c2)
  (cond ((c:noop? c1) c2) ((c:noop? c2) c1) (else (list c1 " " c2))))
 (if (null? cs) "" (space-between (first cs) (spaces-between (rest cs)))))

(define (space-between . cs) (spaces-between cs))

(define (commas-between cs)
 (define (comma-between c1 c2)
  (cond ((c:noop? c1) c2) ((c:noop? c2) c1) (else (list c1 ", " c2))))
 (if (null? cs) "" (comma-between (first cs) (commas-between (rest cs)))))

(define (comma-between . cs) (commas-between cs))

(define (newlines-between cs)
 (define (newline-between c1 c2)
  (cond ((c:noop? c1) c2) ((c:noop? c2) c1) (else (list c1 #\newline c2))))
 (if (null? cs) "" (newline-between (first cs) (newlines-between (rest cs)))))

(define (newline-between . cs) (newlines-between cs))

(define (braces-around c)
 (define (c:ends-in-colon? c)
  (or (and (pair? c)
	   (or (c:ends-in-colon? (cdr c))
	       (and (c:ends-in-colon? (car c)) (c:/**/? (cdr c)))))
      (and (string? c) (string=? c ":"))))
 ;; ANSI C doesn't allow "label:}"
 (list "{" (if (c:ends-in-colon? c) (semicolon-after c) c) "}"))

(define (unparenthesize c)
 (cond ((c:no-return? c) (c:no-return (unparenthesize (second c))))
       ((c:protect? c) (c:protect (unparenthesize (second c))))
       ((c:parentheses? c) (unparenthesize (second c)))
       (else c)))

(define (parentheses-around c) (list "(" (unparenthesize c) ")"))

(define (semicolon-after c) (list (unparenthesize c) ";"))

(define (colon-after c) (list c ":"))

(define (star-before c) (list "*" c))

;;; C declaration constructors

(define (c:declaration w c c1)
 (let ((c (if (c:protect? c) (second c) c)))
  (list 'c:declaration
	c
	(if (c:noop? c1)
	    (semicolon-after (c:type-set w c))
	    (space-between (semicolon-after (c:type-set w c)) (c:/**/ c1)))
	#f)))

;;; C expression constructors

(define (c:initialize-constants i)
 (list "initialize_constants" (number->string i)))

(define (c:main) "main")

(define (c:argc) "argc")

(define (c:argv) "argv")

(define (c:escape c)
 (cond ((char=? c #\newline) "\\n")
       ((char=? c #\') "\\'")
       ((char=? c #\") "\\\"")
       ((char=? c #\\) "\\\\")
       (else (string c))))

(define (c:character c) (list "'" (c:escape c) "'"))

(define (c:fixnum c) (number->string c))

(define (c:flonum c) (number->string c))

(define (c:string c)
 (cond
  (*align-strings?*
   (unless (memp string=? c *strings*)
    (set! *strings* (append *strings* (list c)))
    (outside-main
     ;; needs work: needs abstraction for initialized declaration
     (semicolon-after
      (space-between
       "union"
       (braces-around
	(space-between
	 (semicolon-after
	  (space-between
	   *char*
	   (c:raw-subscript
	    "string" (number->string (+ (string-length c) 1)))))
	 (semicolon-after (space-between *fixnum* "align"))))
       (unparenthesize
	(c:= (list "string"
		   (number->string (positionp string=? c *strings*)))
	     (braces-around
	      (braces-around
	       (commas-between
		(append (map (lambda (c) (number->string (char->integer c)))
			     (string->list c))
			(list (c:0))))))))))))
   (c:& (c:. (list "string"
		   (number->string (positionp string=? c *strings*)))
	     (c:raw-subscript "string" (c:0)))))
  (else (list "\"" (map c:escape (string->list c)) "\""))))

(define (c:subscript c1 c2)
 (parentheses-around (c:protect (list c1 "[" (unparenthesize c2) "]"))))

(define (c:raw-subscript c1 c2) (list c1 "[" (unparenthesize c2) "]"))

(define (c:= c1 c2)
 (parentheses-around
  (c:protect (space-between (unparenthesize c1) "=" (unparenthesize c2)))))

(define (c:call c . cs) (list c (parentheses-around (commas-between cs))))

(define (c:sizeof c) (c:call "sizeof" c))

(define (c:alignof c)
 (include! "stddef")			;offsetof
 (c:call "offsetof"
	 (list "struct"
	       (braces-around
		(space-between (semicolon-after (space-between "char" "dummy"))
			       (semicolon-after (c "probe")))))
	 "probe"))

(define (c:cast c1 c2) (parentheses-around (list (parentheses-around c1) c2)))

(define (c:unsigned-char-cast c) (c:cast (space-between "unsigned" "char") c))

;;; needs work: Calls to this might need checks for -On.
(define (c:fixnum-cast c) (c:cast *fixnum* c))

(define (c:flonum-cast c) (c:cast *flonum* c))

(define (c:void*-cast c) (c:cast (space-between "void" "*") c))

(define (c:fixnum*-cast c) (c:cast (space-between *fixnum* "*") c))

(define (c:char*-cast c) (c:cast (space-between *char* "*") c))

(define (c:file*-cast c)
 (include! "stdio")			;FILE
 (c:cast (space-between *file* "*") c))

(define (c:boolean-or . cs)
 ;; note: This can't be c:|| becaust this is not allowed by the Scheme reader.
 (parentheses-around
  (let loop ((cs cs))
   (cond ((null? cs) (fuck-up))
	 ((null? (rest cs)) (first cs))
	 (else (list (first cs) "||" (loop (rest cs))))))))

(define (c:&& . cs)
 (parentheses-around
  (let loop ((cs cs))
   (cond ((null? cs) (fuck-up))
	 ((null? (rest cs)) (first cs))
	 (else (list (first cs) "&&" (loop (rest cs))))))))

(define (unary c1 c2) (parentheses-around (list c1 c2)))

(define (binary c1 c2 c3) (parentheses-around (list c1 c2 c3)))

(define (c:?: c1 c2 c3) (parentheses-around (list c1 "?" c2 ":" c3)))

;; needs work: To distinguish between signed and unsigned and int and long int.
(define (c:0) "0")

;; needs work: To distinguish between float, double, and long double.
(define (c:0.0) "0.0")

;; needs work: To distinguish between signed and unsigned and int and long int.
(define (c:1) "1")

;; needs work: To distinguish between float, double, and long double.
(define (c:1.0) "1.0")

;; needs work: To distinguish between signed and unsigned and int and long int.
(define (c:256) "256")

;; needs work: To distinguish between char and wchar_t.
(define (c:nul) "'\\0'")

(define (c:null)
 (include! "stdlib")			;NULL
 "NULL")

(define (c:eof)
 (include! "stdio")			;EOF
 "EOF")

(define (c:. c1 c2)
 (if (let ((c1 (c:strip c1)))
      ;; needs work: To replace the calls to LIST? and LENGTH in the
      ;;             following with more efficient code:
      (and (list? c1)
	   (= (length c1) 3)
	   (string? (second c1))
	   (or (string=? (second c1) ".") (string=? (second c1) "->"))))
     (binary (unparenthesize c1) "." c2)
     (binary c1 "." c2)))

(define (c:-> c1 c2)
 (c:protect
  (if (let ((c1 (c:strip c1)))
       ;; needs work: To replace the calls to LIST? and LENGTH in the
       ;;             following with more efficient code:
       (and (list? c1)
	    (= (length c1) 3)
	    (string? (second c1))
	    (or (string=? (second c1) ".") (string=? (second c1) "->"))))
      (binary (unparenthesize c1) "->" c2)
      (binary c1 "->" c2))))

(define (c:== c1 c2) (binary c1 "==" c2))

(define (c:==0 c) (c:== c (c:0)))

(define (c:==0.0 c) (c:== c (c:0.0)))

(define (c:==infinity c)
 (include! "math")			;HUGE_VAL
 (c:== c "HUGE_VAL"))

(define (c:==null c) (c:== c (c:null)))

(define (c:==eof c) (c:== c (c:eof)))

(define (c:!= c1 c2) (binary c1 "!=" c2))

(define (c:!=0 c) (c:!= c (c:0)))

(define (c:!=0.0 c) (c:!= c (c:0.0)))

(define (c:< c1 c2) (binary c1 "<" c2))

(define (c:<0 c) (c:< c (c:0)))

(define (c:<0.0 c) (c:< c (c:0.0)))

(define (c:> c1 c2) (binary c1 ">" c2))

(define (c:>0 c) (c:> c (c:0)))

(define (c:>0.0 c) (c:> c (c:0.0)))

(define (c:<= c1 c2) (binary c1 "<=" c2))

(define (c:>= c1 c2) (binary c1 ">=" c2))

;;; needs work: Calls to this might need checks for -On.
(define (c:+ c1 c2) (if (equal? c2 "0") c1 (binary c1 "+" c2)))

;;; needs work: Calls to this might need checks for -On.
(define (c:- c . cs)
 (when (> (length cs) 1) (fuck-up))
 (cond ((null? cs) (unary "-" c))
       ((equal? (first cs) "0") c)
       (else (binary c "-" (first cs)))))

;;; needs work: Calls to this might need checks for -On.
(define (c:* c . cs)
 (when (> (length cs) 1) (fuck-up))
 (if (null? cs) (unary "*" c) (binary c "*" (first cs))))

(define (c:/ c1 c2) (binary c1 "/" c2))

(define (c:% c1 c2)
 ;; needs work: The % operator in C is implementation dependent for negative
 ;;             arguments.
 (binary c1 "%" c2))

;;; needs work: Calls to this might need checks for -On.
(define (c:<< c1 c2) (if (equal? c2 "0") c1 (binary c1 "<<" c2)))

;;; needs work: Calls to this might need checks for -On.
(define (c:>> c1 c2)
 ;; needs work: The >> operator in C is implementation dependent for negative
 ;;             arguments.
 (if (equal? c2 "0") c1 (binary c1 ">>" c2)))

(define (c:& c . cs)
 (when (> (length cs) 1) (fuck-up))
 (if (null? cs) (unary "&" c) (binary c "&" (first cs))))

(define (c:bitwise-or c1 c2)
 ;; note: This can't be c:| because this is not allowed by the Scheme reader.
 (if (equal? c2 "0") c1 (binary c1 "|" c2)))

(define (c:^ c1 c2) (binary c1 "^" c2))

(define (c:~ c) (unary "~" c))

(define (c:! c) (unary "!" c))

;;; needs work: Calls to this might need checks for -On.
(define (c:++ c) (parentheses-around (c:protect (list c "++"))))

;;; C statement constructors

(define (c:/**/ c) (space-between "/*" c "*/"))

(define (c:define c1 c2) (space-between "#define" c1 c2))

(define (c:noop) "")

(define (c::= c1 c2) (semicolon-after (c:= c1 c2)))

;;; needs work: Calls to this might need checks for -On.
(define (c:+= c1 c2)
 (semicolon-after
  (c:protect (space-between (unparenthesize c1) "+=" (unparenthesize c2)))))

(define (c:: c) (colon-after c))

(define (c:goto c) (c:no-return (semicolon-after (space-between "goto" c))))

(define (c:header c . cs)
 (if (null? cs)
     (list c (parentheses-around "void"))
     (list c (parentheses-around (commas-between cs)))))

(define (c:prototype c . cs) (semicolon-after (apply c:header c cs)))

(define (c:noreturn-prototype c . cs)
 (set! *c:noreturn?* #t)
 (semicolon-after (space-between (apply c:header c cs) "NORETURN")))

(define (c:gosub c . cs) (semicolon-after (c:protect (apply c:call c cs))))

(define (c:return . cs)
 (when (> (length cs) 1) (fuck-up))
 (c:no-return
  (semicolon-after (if (null? cs)
		       "return"
		       (space-between "return" (unparenthesize (first cs)))))))

(define (strict-operator c1 c2)
 (if (let loop ((c2 c2))
      (cond ((c:no-return? c2) (loop (second c2)))
	    ((c:protect? c2) (loop (second c2)))
	    ;; needs work: To replace the calls to LIST? and LENGTH in the
	    ;;             following with more efficient code:
	    (else (or (and (list? c2)
			   (= (length c2) 3)
			   (char? (second c2))
			   (char=? (second c2) #\newline))
		      (and (list? c2)
			   (list? (first c2))
			   (string? (first (first c2)))
			   (or (string=? (first (first c2)) "if")
			       (string=? (first (first c2)) "switch")))))))
     (newline-between c1 (braces-around c2))
     (space-between c1 c2)))

(define (operator c1 c2)
 (if (let loop ((c2 c2))
      (cond
       ((c:no-return? c2) (loop (second c2)))
       ((c:protect? c2) (loop (second c2)))
       ;; needs work: To replace the calls to LIST? and LENGTH in the
       ;;             following with more efficient code:
       (else (and (list? c2)
		  (= (length c2) 3)
		  (char? (second c2))
		  (char=? (second c2) #\newline)
		  (or (not (list? (first c2)))
		      (not (string? (first (first c2))))
		      (and (not (string=? (first (first c2)) "if"))
			   (not (string=? (first (first c2)) "switch"))))))))
     (newline-between c1 (braces-around c2))
     (space-between c1 c2)))

(define (c:while c1 c2)
 (operator (space-between "while" (parentheses-around c1)) c2))

(define (c:for c1 c2 c3 c4)
 (operator (space-between
	    "for"
	    (parentheses-around
	     (space-between (semicolon-after (unparenthesize c1))
			    (semicolon-after (unparenthesize c2))
			    (unparenthesize c3))))
	   c4))

(define (c:if c1 c2 c3 p?)
 (if (c:match? c2 c3)
     (if p? (newline-between (semicolon-after c1) c2) c2)
     (if (c:/**/? c2)
	 (if (c:/**/? c3)
	     (if p? (semicolon-after c1) (c:noop))
	     ;; note: This assumes that C:! wraps in parentheses.
	     (operator (space-between "if" (c:! (parentheses-around c1))) c3))
	 (if (c:/**/? c3)
	     (operator (space-between "if" (parentheses-around c1)) c2)
	     (if (c:no-return? c2)
		 (if (c:no-return? c3)
		     (c:no-return
		      (newline-between
		       (strict-operator
			(space-between "if" (parentheses-around c1)) c2)
		       (operator "else" c3)))
		     (newline-between
		      (strict-operator
		       (space-between "if" (parentheses-around c1)) c2)
		      c3))
		 (if (c:no-return? c3)
		     (newline-between
		      (strict-operator
		       ;; note: This assumes that C:! wraps in parentheses.
		       (space-between "if" (c:! (parentheses-around c1))) c3)
		      c2)
		     (if (and (c:assignment? c2)
			      (c:assignment? c3)
			      (equal? (first (second (first c2)))
				      (first (second (first c3)))))
			 (c::= (first (second (first c2)))
			       (c:?: c1
				     (third (third (second (first c2))))
				     (third (third (second (first c3))))))
			 (newline-between
			  (strict-operator
			   (space-between "if" (parentheses-around c1)) c2)
			  (operator "else" c3)))))))))

(define (equate-cases cs1 cs2)
 (transitive-equivalence-classesp
  (lambda (pair1 pair2)
   ;; conventions: PAIR1 PAIR2
   (c:match? (second pair1) (second pair2)))
  (map list cs1 cs2)))

(define (c:default c) (newline-between (colon-after "default") c))

(define (c:switch c1 cs2 cs3 c4 p?)
 (let ((cases (transitive-equivalence-classesp
	       (lambda (pair1 pair2)
		;; conventions: PAIR1 PAIR2
		(c:match? (second pair1) (second pair2)))
	       (cons (list (colon-after "default") c4)
		     (map (lambda (c2 c3)
			   (list (colon-after (space-between "case" c2)) c3))
			  cs2 cs3)))))
  ;; conventions: CASES
  (when (null? cases) (fuck-up))
  (if (null? (rest cases))
      ;; note: Technically we would need to prepend an evaluation of C1 but
      ;;       we don't since SWITCH is always called as a type switch and the
      ;;       antecedent can't do any side effects, diverge, or cause an
      ;;       error.
      (second (first (first cases)))
      (let ((the-case
	     (find-if (lambda (pairs)
		       ;; conventions: PAIRS
		       (some (lambda (pair)
			      ;; conventions: PAIR
			      (equal? (first pair) (colon-after "default")))
			     pairs))
		      cases)))
       ;; conventions: THE-CASE
       (if (and (null? (rest (removeq the-case cases)))
		(null? (rest (first (removeq the-case cases)))))
	   (if p?
	       (c:if
		(c:==
		 c1
		 (third
		  (first (first (first (first (removeq the-case cases)))))))
		(second (first (first (removeq the-case cases))))
		(second (first the-case))
		#f)
	       (newline-between
		(c:if
		 (c:==
		  c1
		  (third
		   (first (first (first (first (removeq the-case cases)))))))
		 (second (first (first (removeq the-case cases))))
		 (c:noop)
		 #f)
		(second (first the-case))))
	   (newline-between
	    (space-between "switch" (parentheses-around c1))
	    (braces-around
	     (newline-between
	      (newlines-between
	       (map (lambda (pairs)
		     ;; conventions: PAIRS
		     (newline-between
		      (newlines-between (map first pairs))
		      (second (first pairs))
		      (if p? (semicolon-after "break") (c:noop))))
		    (removeq the-case cases)))
	      (c:default (second (first the-case)))))))))))

(define (c:defaultless-switch c1 cs2 cs3 p?)
 ;; Defaultless here means not that the default is a noop but rather that the
 ;; compiler guarantees that the default will never be taken so that the
 ;; default can be reallocated to one of the cases.
 (let ((cases (transitive-equivalence-classesp
	       (lambda (pair1 pair2)
		;; conventions: PAIR1 PAIR2
		(c:match? (second pair1) (second pair2)))
	       (map (lambda (c2 c3)
		     (list (colon-after (space-between "case" c2)) c3))
		    cs2 cs3))))
  ;; conventions: CASES
  (when (null? cases) (fuck-up))
  (if (null? (rest cases))
      ;; note: Technically we would need to prepend an evaluation of C1 but
      ;;       we don't since SWITCH is always called as a type switch and the
      ;;       antecedent can't do any side effects, diverge, or cause an
      ;;       error.
      (second (first (first cases)))
      ;; Choose the case with the greatest number of pairs to be the default.
      ;; Note that MINP is being used to compute the maximal member here.
      (let ((the-case (minp (lambda (case1 case2)
			     ;; conventions: CASE1 CASE2
			     (> (length case1) (length case2)))
			    cases)))
       ;; conventions: THE-CASE
       (if (and (null? (rest (removeq the-case cases)))
		(null? (rest (first (removeq the-case cases)))))
	   (if p?
	       (c:if
		(c:==
		 c1
		 (third
		  (first (first (first (first (removeq the-case cases)))))))
		(second (first (first (removeq the-case cases))))
		(second (first the-case))
		#f)
	       (newline-between
		(c:if
		 (c:==
		  c1
		  (third
		   (first (first (first (first (removeq the-case cases)))))))
		 (second (first (first (removeq the-case cases))))
		 (c:noop)
		 #f)
		(second (first the-case))))
	   (newline-between
	    (space-between "switch" (parentheses-around c1))
	    (braces-around
	     (newline-between
	      (newlines-between
	       (map (lambda (pairs)
		     ;; conventions: PAIRS
		     (newline-between
		      (newlines-between (map first pairs))
		      (second (first pairs))
		      (if p? (semicolon-after "break") (c:noop))))
		    (removeq the-case cases)))
	      (c:default (second (first the-case)))))))))))

;;; C type constructors

(define (c:byte) "char")

;;; C function call constructors

(define (c:rint c)
 (include! "math")			;rint
 (c:call "rint" c))

(define (c:floor c)
 (include! "math")			;floor
 (c:call "floor" c))

(define (c:ceil c)
 (include! "math")			;ceil
 (c:call "ceil" c))

(define (c:exp c)
 (include! "math")			;exp
 (c:call "exp" c))

(define (c:log c)
 (include! "math")			;log
 (c:call "log" c))

(define (c:sin c)
 (include! "math")			;sin
 (c:call "sin" c))

(define (c:cos c)
 (include! "math")			;cos
 (c:call "cos" c))

(define (c:tan c)
 (include! "math")			;tan
 (c:call "tan" c))

(define (c:asin c)
 (include! "math")			;asin
 (c:call "asin" c))

(define (c:acos c)
 (include! "math")			;acos
 (c:call "acos" c))

(define (c:atan c)
 (include! "math")			;atan
 (c:call "atan" c))

(define (c:atan2 c1 c2)
 (include! "math")			;atan2
 (c:call "atan2" c1 c2))

(define (c:sqrt c)
 (include! "math")			;sqrt
 (c:call "sqrt" c))

(define (c:pow c1 c2)
 (include! "math")			;pow
 (c:call "pow" c1 c2))

(define (c:setjmp c)
 (include! "setjmp")			;setjmp
 (c:protect (c:call "setjmp" c)))

(define (c:longjmp c1 c2)
 (include! "setjmp")			;longjmp
 (c:gosub "longjmp" c1 c2))

(define (c:fopen c1 c2)
 (include! "stdio")			;fopen
 (c:protect (c:call "fopen" c1 c2)))

(define (c:fclose c)
 (include! "stdio")			;fclose
 (c:protect (c:call "fclose" c)))

(define (c:getc c)
 (include! "stdio")			;getc
 (c:protect (c:call "getc" c)))

(define (c:ungetc c1 c2)
 (include! "stdio")			;ungetc
 (c:protect (c:call "ungetc" c1 c2)))

(define (c:putc c1 c2)
 (include! "stdio")			;putc
 (c:gosub "putc" c1 c2))

(define (c:printf . cs)
 (include! "stdio")			;printf
 (apply c:gosub "printf" cs))

(define (c:malloc c p?)
 (cond (*treadmarks?*
	(include! "Tmk")		;Tmk_malloc
	(c:protect (c:call "Tmk_malloc" c)))
       (*program-has-heap?*
	(cond (p? (include! "gc")	;GC_malloc_uncollectable
		  (c:protect (c:call "GC_malloc_uncollectable" c)))
	      (else (include! "gc")	;GC_malloc_atomic_uncollectable
		    (c:protect (c:call "GC_malloc_atomic_uncollectable" c)))))
       (else (include! "malloc")	;malloc
	     (c:protect (c:call "malloc" c)))))

(define (c:gc-malloc c)
 (when *treadmarks?* (fuck-up))
 (include! "gc")			;GC_malloc
 (c:protect (c:call "GC_malloc" c)))

(define (c:gc-malloc-atomic c)
 (when *treadmarks?* (fuck-up))
 (include! "gc")			;GC_malloc_atomic
 (c:protect (c:call "GC_malloc_atomic" c)))

(define (c:free c p?)
 (cond (*treadmarks?*
	(include! "Tmk")		;Tmk_free
	(c:gosub "Tmk_free" c))
       (*program-has-heap?*
	(cond (p? (include! "gc")	;GC_free
		  (c:gosub "GC_free" c))
	      (else (include! "gc")	;GC_free
		    (c:gosub "GC_free" c))))
       (else (include! "malloc")	;free
	     (c:gosub "free" c))))

(define (c:alloca c)
 (when *treadmarks?* (fuck-up))
 (include! (if *include-malloc-for-alloca?* "malloc" "alloca"))	;alloca
 (c:protect (c:call "alloca" c)))

(define (c:gc-enable-incremental)
 (when *treadmarks?* (fuck-up))
 (include! "gc")			;GC_enable_incremental
 (c:gosub "GC_enable_incremental"))

(define (c:strlen c)
 (include! "string")			;strlen
 (c:call "strlen" c))

(define (c:exit c)
 (include! "stdlib")			;exit
 (c:gosub "exit" c))

(define (c:assert c)
 (include! "assert")			;assert
 (c:gosub "assert" c))

;;; Stalin-specific code generation utilities

(define (c:==struct c1 c2 u)
 (cond ((rectangular-type? u)
        (c:&& (c:== (c:r c1) (c:r c2)) (c:== (c:i c1) (c:i c2))))
       ((native-procedure-type? u)
        (case *closure-representation*
         ((immediate-flat immediate-display)
          (apply c:&&
	         (map (lambda (e) (c:== (c:. c1 (c:e e)) (c:. c2 (c:e e))))
		      (ancestors u))))
         ((indirect-flat indirect-display linked) (c:== c1 c2))
         (else (fuck-up))))
       ;; Immediate structures should never be compared for EQ?-ness.
       ((and (or (nonheaded-vector-type? u) (displaced-vector-type? u))
             (not (degenerate-vector-type? u)))
        (c:&& (c:== (c:. c1 "length") (c:. c2 "length"))
              (c:== (c:. c1 "elements") (c:. c2 "elements"))))
       (else (c:== c1 c2))))

(define (tag-only? w)
 (must-be? (lambda (u) (or (char-type? u) (fictitious? u))) w))

(define (has-union? w)
 (> (count-if-not (lambda (u) (or (char-type? u) (fictitious? u))) (members w))
    1))

(define (determine-which-type-sets-are-squeezable!)
 ;; needs work: This really depends on the architecture parameters.
 (for-each
  (lambda (w)
   (set-type-set-squeezable?!
    w
    (and (not (can-be? fixnum-type? w))
	 (not (can-be? flonum-type? w))
	 (not (can-be? rectangular-type? w))
	 (not (can-be? pointer-type? w))
	 (case *closure-representation*
	  ((immediate-flat immediate-display)
	   ;; needs work: Can be extended to allow squeezing a native procedure
	   ;;             with a closure that has a single slot or environment.
	   (not (can-be?
		 (lambda (u)
		  (and (native-procedure-type? u) (not (fictitious? u))))
		 w)))
	  ((indirect-flat indirect-display linked) #t)
	  (else (fuck-up)))
	 ;; needs work: Can be extended to allow squeezing a singleton
	 ;;             immediate structure if its slot is squeezed.
	 (not (can-be? (lambda (u)
			(and (structure-type? u)
			     (structure-type-immediate? u)))
		       w))
	 (not (can-be? (lambda (u)
			(and (headed-vector-type? u)
			     (degenerate-vector-type? u)))
		       w))
	 (not (can-be? nonheaded-vector-type? w))
	 (not (can-be? displaced-vector-type? w))
	 (one (lambda (u) (and (not (char-type? u)) (not (fictitious? u))))
	      (members w)))))
  *ws*))

(define (squeezable? w)
 ;; needs work: This really depends on the architecture parameters.
 (for-each-member
  (lambda (u)
   (cond ((null-type? u) (set! *tag-size?* #t))
	 ((true-type? u) (set! *tag-size?* #t))
	 ((false-type? u) (set! *tag-size?* #t))
	 ((char-type? u) (set! *char-size?* #t))
	 ((fixnum-type? u) #f)
	 ((flonum-type? u) #f)
	 ((rectangular-type? u) #f)
	 ((input-port-type? u)
	  (include! "stdio")		;FILE
	  (set! *file*-size?* #t))
	 ((output-port-type? u)
	  (include! "stdio")		;FILE
	  (set! *file*-size?* #t))
	 ((eof-object-type? u) (set! *tag-size?* #t))
	 ((pointer-type? u) (set! *void*-size?* #t))
	 ((internal-symbol-type? u) (set! *tag-size?* #t))
	 ((external-symbol-type? u) (set! *char*-size?* #t))
	 ((primitive-procedure-type? u) (set! *tag-size?* #t))
	 ((native-procedure-type? u)
	  (if (fictitious? u)
	      (set! *tag-size?* #t)
	      (set-native-procedure-type-size?! u #t)))
	 ((foreign-procedure-type? u) (set! *tag-size?* #t))
	 ((continuation-type? u)
	  (cond ((fictitious? u) (set! *tag-size?* #t))
		(else (include! "setjmp") ;jmp_buf
		      (set! *jmpbuf*-size?* #t))))
	 ((string-type? u) (set! *char*-size?* #t))
	 ((structure-type? u)
	  (cond ((fictitious? u) (set! *tag-size?* #t))
		(else (unless (structure-type-immediate? u)
		       (set-structure-type-size?! u #t)))))
	 ((headed-vector-type? u)
	  (unless (degenerate-vector-type? u)
	   (set-headed-vector-type-size?! u #t)))
	 ((nonheaded-vector-type? u) #f)
	 ((displaced-vector-type? u) #f)
	 (else (fuck-up))))
  w)
 (type-set-squeezable? w))

(define (squeezed-member w)
 (unless (squeezed? w) (fuck-up))
 (the-member-that
  (lambda (u) (and (not (char-type? u)) (not (fictitious? u)))) w))

(define (squeezed? w)
 (and *squeeze?*
      (not (fictitious? w))
      (not (monomorphic? w))
      (not (tag-only? w))
      (squeezable? w)))

(define (type-alignment& u)
 ;; This is only defined where U is a pointer type. This gives the alignment
 ;; of the object pointed to by U. In other words, this number is the minimum
 ;; number of low-order zeros bits in pointers of type U.
 (cond
  ((input-port-type? u)
   (set! *file-alignment?* #t)
   *file-alignment*)
  ((output-port-type? u)
   (set! *file-alignment?* #t)
   *file-alignment*)
  ((pointer-type? u)
   ;; This is worst case because you don't know what the pointer is pointing
   ;; to.
   (set! *char-alignment?* #t)
   *char-alignment*)
  ((external-symbol-type? u)
   (cond (*align-strings?*
	  (set! *fixnum-alignment?* #t)
	  *fixnum-alignment*)
	 (else (set! *char-alignment?* #t)
	       *char-alignment*)))
  ((native-procedure-type? u)
   (when (fictitious? u) (fuck-up))
   (case *closure-representation*
    ((immediate-flat immediate-display) (fuck-up))
    ((indirect-flat indirect-display linked)
     (set-native-procedure-type-alignment&?! u #t)
     (cond
      ((and (or (eq? *closure-conversion-method* 'baseline)
		(eq? *closure-conversion-method* 'conventional))
	    (or (not (environment? (native-procedure-type-narrow-prototype u)))
		(not (environment-used? (narrow-prototype u)))
		(not (environment? (parent-parameter u)))
		(not (environment-used? (parent-parameter u)))))
       (set! *fixnum-alignment?* #t)
       *fixnum-alignment*)
      (else
       (let ((e (parent-parameter u)))
	(if (has-parent-slot? e)
	    ;; note: See the note in TYPE-ALIGNMENT.
	    (max *pointer-alignment*
		 (reduce
		  ;; note: See the note in TYPE-ALIGNMENT.
		  max
		  (map (lambda (g) (type-set-alignment (variable-type-set g)))
		       (remove-if-not slotted? (variables e)))
		  ;; This should never happen.
		  #f))
	    (reduce
	     ;; note: See the note in TYPE-ALIGNMENT.
	     max
	     (map (lambda (g) (type-set-alignment (variable-type-set g)))
		  (remove-if-not slotted? (variables e)))
	     ;; This should never happen.
	     #f))))))
    (else (fuck-up))))
  ((continuation-type? u)
   (when (fictitious? u) (fuck-up))
   (set! *jmpbuf-alignment?* #t)
   *jmpbuf-alignment*)
  ((string-type? u)
   (cond (*align-strings?*
	  (set! *fixnum-alignment?* #t)
	  *fixnum-alignment*)
	 (else (set! *char-alignment?* #t)
	       *char-alignment*)))
  ((structure-type? u)
   (when (fictitious? u) (fuck-up))
   ;; needs work: Can be extended to allow squishing a singleton immediate
   ;;             structure if its slot is squished.
   (when (structure-type-immediate? u) (fuck-up))
   (set-structure-type-alignment&?! u #t)
   (reduce
    ;; note: See the note in TYPE-ALIGNMENT.
    max
    (map type-set-alignment (remove-if fictitious? (structure-type-slots u)))
    ;; This should never happen.
    #f))
  ((headed-vector-type? u)
   (when (degenerate-vector-type? u) (fuck-up))
   (set-headed-vector-type-alignment&?! u #t)
   ;; note: See the note in TYPE-ALIGNMENT.
   (max *length-alignment*
	(type-set-alignment (headed-vector-type-element u))))
  (else (fuck-up))))

(define (pointer-member? u)
 (and (not (char-type? u))
      (not (fictitious? u))
      (not (fixnum-type? u))
      ;; needs work: Can be extended to allow squishing a singleton
      ;;             immediate structure if its slot is squished.
      (not (degenerate-vector-type? u))))

(define (non-pointer-member? u) (not (pointer-member? u)))

(define (no-pointer-members? w) (must-be? non-pointer-member? w))

(define (squish-alignment w)
 (if (no-pointer-members? w)
     ;; Type sets that do not have pointer members have no limit on the number
     ;; of members that are not fictitious because we tolerate an arbitrary
     ;; loss in range for non-pointer values. Remember that CHAR is fictitious.
     ;; In this case, allocate just enough squish tag bits to encode all of
     ;; the members that are not fictitious. But if there are a power-of-two
     ;; such members, allocate an extra squish tag bit because we can't assign
     ;; squish tag zero to a fixnum or a degenerate vector.
     (max (if (can-be-non? (lambda (u) (or (char-type? u) (fictitious? u))) w)
	      (inexact->exact
	       (ceiling
		(+ (/ (log (count-if
			    (lambda (u)
			     (or (fixnum-type? u)
				 ;; needs work: Can be extended to allow
				 ;;             squishing a singleton
				 ;;             immediate structure if its
				 ;;             slot is squished.
				 (degenerate-vector-type? u)))
			    (members w)))
		      (log 2.0))
		   0.1)))
	      0)
	  (type-set-minimal-alignment w))
     ;; Type sets that do have pointer members have a limit on the number of
     ;; squish tag bits because pointers aren't shifted. The number of squish
     ;; tag bits is limited to the smallest number over all of the pointer
     ;; members. The reason is that there are ways that pointers can be
     ;; created where we have no control over the alignment. For example,
     ;; string, pair, and vector constants, alloca, malloc, GC_malloc, fopen,
     ;; stdin, and stdout. In fact, the only place where we do have control
     ;; over alignment is in the region allocator.
     (reduce min
	     (map type-alignment& (members-that pointer-member? w))
	     ;; This can't happen if the type set is squished.
	     #f)))

(define (determine-which-type-sets-are-squishable!)
 ;; needs work: This really depends on the architecture parameters.
 ;; note: Squishing reduces the maximum magnitude of fixnums and the maximum
 ;;       length of degenerate vectors.
 (for-each (lambda (w) (set-type-set-squishable?! w #t)) *ws*)
 (let loop ()
  (let ((again? #f))
   (for-each
    (lambda (w)
     (unless (and
	      ;; A flonum can't be squished because it would result in a loss
	      ;; of precision and a double might not fit in a pointer.
	      ;; needs work: Floats can be squished on 64-bit architectures.
	      (not (can-be? flonum-type? w))
	      ;; A rectangular can't be squished because it won't fit in a
	      ;; pointer.
	      (not (can-be? rectangular-type? w))
	      (case *closure-representation*
	       ((immediate-flat immediate-display)
		;; A native procedure with a closure can't be squished because
		;; it might not fit in a pointer.
		;; needs work: Can be extended to allow squishing a native
		;;             procedure with a closure that has a single
		;;             slot or environment.
		(not (can-be?
		      (lambda (u)
		       (and (native-procedure-type? u) (not (fictitious? u))))
		      w)))
	       ((indirect-flat indirect-display linked) #t)
	       (else (fuck-up)))
	      ;; An immediate structure can't be squished because it might not
	      ;; fit in a pointer and because some of its components might not
	      ;; be squished.
	      ;; needs work: Can be extended to allow squishing a singleton
	      ;;             immediate structure if its slot is squished.
	      (not (can-be? (lambda (u)
			     (and (structure-type? u)
				  (structure-type-immediate? u)))
			    w))
	      ;; A nondegenerate nonheaded vector can't be squished because it
	      ;; might not fit in a pointer.
	      (not (can-be? (lambda (u)
			     (and (nonheaded-vector-type? u)
				  (not (degenerate-vector-type? u))))
			    w))
	      ;; A nondegenerate displaced vector can't be squished because it
	      ;; might not fit in a pointer.
	      (not (can-be? (lambda (u)
			     (and (displaced-vector-type? u)
				  (not (degenerate-vector-type? u))))
			    w))
	      ;; There must be enough squish tag bits.
	      (<= (count-if-not
		   (lambda (u) (or (char-type? u) (fictitious? u)))
		   (members w))
		  (expt 2 (squish-alignment w))))
      (when (type-set-squishable? w)
       (set-type-set-squishable?! w #f)
       (set! again? #t))))
    *ws*)
   (when again? (loop)))))

(define (print-reasons-why-type-sets-are-not-squishable!)
 ;; needs work: This really depends on the architecture parameters.
 (for-each
  (lambda (w)
   (unless (squishable? w)
    (notify
     "W~a is general case for the following reasons:" (type-set-index w))
    (when (can-be? flonum-type? w) (notify "  flonum"))
    (when (can-be? rectangular-type? w) (notify "  rectangular"))
    (when (and (or (eq? *closure-representation* 'immediate-flat)
		   (eq? *closure-representation* 'immediate-display))
	       (can-be?
		(lambda (u)
		 (and (native-procedure-type? u) (not (fictitious? u))))
		w))
     (notify "  immediate native procedure"))
    (when (can-be? (lambda (u)
		    (and (structure-type? u) (structure-type-immediate? u)))
		   w)
     (notify "  immediate structure"))
    (when (can-be? (lambda (u)
		    (and (nonheaded-vector-type? u)
			 (not (degenerate-vector-type? u))))
		   w)
     (notify "  nondegenerate nonheaded vector"))
    (when (can-be? (lambda (u)
		    (and (displaced-vector-type? u)
			 (not (degenerate-vector-type? u))))
		   w)
     (notify "  nondegenerate displaced vector"))
    (when (and
	   (not (can-be? flonum-type? w))
	   (not (can-be? rectangular-type? w))
	   (not (and (or (eq? *closure-representation* 'immediate-flat)
			 (eq? *closure-representation* 'immediate-display))
		     (can-be?
		      (lambda (u)
		       (and (native-procedure-type? u) (not (fictitious? u))))
		      w)))
	   (not (can-be? (lambda (u)
			  (and (structure-type? u)
			       (structure-type-immediate? u)))
			 w))
	   (not (can-be? (lambda (u)
			  (and (nonheaded-vector-type? u)
			       (not (degenerate-vector-type? u))))
			 w))
	   (not (can-be? (lambda (u)
			  (and (displaced-vector-type? u)
			       (not (degenerate-vector-type? u))))
			 w))
	   (> (count-if-not (lambda (u) (or (char-type? u) (fictitious? u)))
			    (members w))
	      (expt 2 (squish-alignment w))))
     (notify "  insufficient squish tag bits (~s needed ~s available)"
	     (inexact->exact
	      (ceiling
	       (/ (log (count-if-not
			(lambda (u) (or (char-type? u) (fictitious? u)))
			(members w)))
		  (log 2))))
	     (squish-alignment w)))))
  *ws*))

(define (squishable? w)
 (for-each-member
  (lambda (u)
   (cond ((null-type? u) (set! *tag-size?* #t))
	 ((true-type? u) (set! *tag-size?* #t))
	 ((false-type? u) (set! *tag-size?* #t))
	 ((char-type? u) (set! *char-size?* #t))
	 ((fixnum-type? u) (set! *fixnum-size?* #t))
	 ((flonum-type? u) #f)
	 ((rectangular-type? u) #f)
	 ((input-port-type? u)
	  (include! "stdio")		;FILE
	  (set! *file*-size?* #t))
	 ((output-port-type? u)
	  (include! "stdio")		;FILE
	  (set! *file*-size?* #t))
	 ((eof-object-type? u) (set! *tag-size?* #t))
	 ((pointer-type? u) (set! *void*-size?* #t))
	 ((internal-symbol-type? u) (set! *tag-size?* #t))
	 ((external-symbol-type? u) (set! *char*-size?* #t))
	 ((primitive-procedure-type? u) (set! *tag-size?* #t))
	 ((native-procedure-type? u)
	  (if (fictitious? u)
	      (set! *tag-size?* #t)
	      (set-native-procedure-type-size?! u #t)))
	 ((foreign-procedure-type? u) (set! *tag-size?* #t))
	 ((continuation-type? u)
	  (cond ((fictitious? u) (set! *tag-size?* #t))
		(else (include! "setjmp") ;jmp_buf
		      (set! *jmpbuf*-size?* #t))))
	 ((string-type? u) (set! *char*-size?* #t))
	 ((structure-type? u)
	  (cond ((fictitious? u) (set! *tag-size?* #t))
		(else (unless (structure-type-immediate? u)
		       (set-structure-type-size?! u #t)))))
	 ((headed-vector-type? u)
	  (if (degenerate-vector-type? u)
	      (set! *length-size?* #t)
	      (set-headed-vector-type-size?! u #t)))
	 ((nonheaded-vector-type? u)
	  (when (degenerate-vector-type? u)
	   (set! *length-size?* #t)))
	 ((displaced-vector-type? u)
	  (when (degenerate-vector-type? u)
	   (set! *length-size?* #t)))
	 (else (fuck-up))))
  w)
 (set! *squished-size?* #t)
 (type-set-squishable? w))

(define (squished? w)
 (and (not (zero? *squished-size*))
      (not (fictitious? w))
      (not (monomorphic? w))
      (not (tag-only? w))
      (not (squeezed? w))
      (squishable? w)))

(define (general? w)
 (and (not (fictitious? w))
      (not (monomorphic? w))
      (not (tag-only? w))
      (not (squeezed? w))
      (not (squished? w))))

(define (determine-alignments!)
 (set! *worst-alignment*
       ;; note: See the note in TYPE-ALIGNMENT.
       (reduce max (map squish-alignment (remove-if-not squished? *ws*)) 0))
 (set! *allocation-alignment*
       ;; note: See the note in TYPE-ALIGNMENT.
       (max
	;; note: See the note in TYPE-ALIGNMENT.
	;; At this point we no longer know which strings are non-reclaimable.
	(reduce max (map type-alignment& *string-types*) 0)
	(reduce
	 ;; note: See the note in TYPE-ALIGNMENT.
	 max
	 (map type-alignment&
	      (remove-if
	       fictitious?
	       (remove-if structure-type-immediate? *structure-types*)))
	 0)
	(reduce
	 ;; note: See the note in TYPE-ALIGNMENT.
	 max
	 (map type-alignment&
	      (remove-if degenerate-vector-type? *headed-vector-types*))
	 0))))

(define (type-alignment u)
 (cond
  ((fixnum-type? u)
   (set! *fixnum-alignment?* #t)
   *fixnum-alignment*)
  ((flonum-type? u)
   (set! *flonum-alignment?* #t)
   *flonum-alignment*)
  ((rectangular-type? u)
   (set! *rectangular-alignment?* #t)
   ;; needs work: See the needs work below.
   ;; note: See the note below.
   *flonum-alignment*)
  ((input-port-type? u)
   (set! *file*-alignment?* #t)
   *pointer-alignment*)
  ((output-port-type? u)
   (set! *file*-alignment?* #t)
   *pointer-alignment*)
  ((pointer-type? u)
   (set! *void*-alignment?* #t)
   *pointer-alignment*)
  ((external-symbol-type? u)
   (set! *char*-alignment?* #t)
   *pointer-alignment*)
  ((native-procedure-type? u)
   (when (fictitious? u) (fuck-up))
   (set-native-procedure-type-alignment?! u #t)
   ;; needs work: See the needs work below.
   ;; note: See the note below.
   *pointer-alignment*)
  ((continuation-type? u)
   (when (fictitious? u) (fuck-up))
   (set! *jmpbuf*-alignment?* #t)
   *pointer-alignment*)
  ((string-type? u)
   (set! *char*-alignment?* #t)
   *pointer-alignment*)
  ((structure-type? u)
   (when (fictitious? u) (fuck-up))
   (set-structure-type-alignment?! u #t)
   (if (structure-type-immediate? u)
       ;; needs work: I'm not sure but it may be the case that
       ;;             struct {tau s0;} has different alignment and size
       ;;             than tau. It also may be the case that something like
       ;;             struct {char s0; char s1;} has different alignment
       ;;             than char and different size than twice char.
       ;; note: (= (LG (LCM (EXPT 2 X) (EXPT 2 Y))) (MAX X Y))
       (reduce max
	       (map type-set-alignment
		    (remove-if fictitious? (structure-type-slots u)))
	       ;; This can't happen if the structure type is not fictitious.
	       #f)
       *pointer-alignment*))
  ((headed-vector-type? u)
   (cond ((degenerate-vector-type? u)
	  (set! *length-alignment?* #t)
	  *length-alignment*)
	 (else (set-headed-vector-type-alignment?! u #t)
	       *pointer-alignment*)))
  ((nonheaded-vector-type? u)
   (cond ((degenerate-vector-type? u)
	  (set! *length-alignment?* #t)
	  *length-alignment*)
	 (else (set-nonheaded-vector-type-alignment?! u #t)
	       ;; note: See the note above.
	       (max *length-alignment* *pointer-alignment*))))
  ((displaced-vector-type? u)
   (cond ((degenerate-vector-type? u)
	  (set! *length-alignment?* #t)
	  *length-alignment*)
	 (else (set-displaced-vector-type-alignment?! u #t)
	       ;; note: See the note above.
	       (max *length-alignment* *pointer-alignment*))))
  (else (case *closure-conversion-method*
	 ((baseline conventional)
	  (set! *fixnum-alignment?* #t)
	  *fixnum-alignment*)
	 ((lightweight) (fuck-up))
	 (else (fuck-up))))))

(define (type-set-alignment w)
 (cond ((fictitious? w) (fuck-up))
       ((monomorphic? w)
	(cond ((char-type? (the-member w))
	       (set! *char-alignment?* #t)
	       *char-alignment*)
	      (else (type-alignment (the-member w)))))
       ((tag-only? w)
	(set! *tag-alignment?* #t)
	*tag-alignment*)
       ((squeezed? w) (type-alignment (squeezed-member w)))
       ((squished? w)
	(set! *squished-alignment?* #t)
	*squished-alignment*)
       (else (set-type-set-alignment?! w #t)
	     (set! *tag-alignment?* #t)
	     ;; needs work: See the needs work in TYPE-ALIGNMENT.
	     ;; note: See the note in TYPE-ALIGNMENT.
	     (max *tag-alignment*
		  (reduce
		   max
		   (map type-alignment
			(members-that
			 (lambda (u)
			  (and (not (char-type? u)) (not (fictitious? u))))
			 w))
		   ;; This can't happen if the type set isn't fictitious,
		   ;; monomorphic, or tag only.
		   #f)))))

(define (align s a)
 ;; conventions: S A
 ;; This adds the appropriate padding to S so that it can be followed by an
 ;; object with alignment A.
 (* (+ (quotient s (expt 2 a)) (if (zero? (remainder s (expt 2 a))) 0 1))
    (expt 2 a)))

(define (type-size u)
 (cond
  ((fixnum-type? u)
   (set! *fixnum-size?* #t)
   *fixnum-size*)
  ((flonum-type? u)
   (set! *flonum-size?* #t)
   *flonum-size*)
  ((rectangular-type? u)
   (set! *rectangular-size?* #t)
   ;; needs work: See the needs work in TYPE-ALIGNMENT.
   (align (+ (align *flonum-size* *flonum-alignment*) *flonum-size*)
	  (type-alignment u)))
  ((input-port-type? u)
   (set! *file*-size?* #t)
   *pointer-size*)
  ((output-port-type? u)
   (set! *file*-size?* #t)
   *pointer-size*)
  ((pointer-type? u)
   (set! *void*-size?* #t)
   *pointer-size*)
  ((external-symbol-type? u)
   (set! *char*-size?* #t)
   *pointer-size*)
  ((native-procedure-type? u)
   (when (fictitious? u) (fuck-up))
   (case *closure-representation*
    ((immediate-flat)
     (unimplemented #f "Immediate flat closures are not (yet) implemented"))
    ((immediate-display)
     (set-native-procedure-type-size?! u #t)
     (let loop ((s 0) (n (length (ancestors u))))
      ;; conventions: S N
      (if (zero? n)
	  (align s (type-alignment u))
	  (loop (+ (align s *pointer-alignment*) *pointer-size*)
		(- n 1)))))
    ((indirect-flat indirect-display linked)
     (set-native-procedure-type-size?! u #t)
     *pointer-size*)
    (else (fuck-up))))
  ((continuation-type? u)
   (when (fictitious? u) (fuck-up))
   (set! *jmpbuf*-size?* #t)
   *pointer-size*)
  ((string-type? u)
   (set! *char*-size?* #t)
   *pointer-size*)
  ((structure-type? u)
   (set-structure-type-size?! u #t)
   (if (structure-type-immediate? u)
       (let loop ((s 0) (ws (structure-type-slots u)))
	;; conventions: S
	(if (null? ws)
	    (align s (type-alignment u))
	    (loop (if (fictitious? (first ws))
		      s
		      (+ (align s (type-set-alignment (first ws)))
			 (type-set-size (first ws))))
		  (rest ws))))
       *pointer-size*))
  ((headed-vector-type? u)
   (cond ((degenerate-vector-type? u)
	  (set! *length-size?* #t)
	  *length-size*)
	 (else (set-headed-vector-type-size?! u #t)
	       *pointer-size*)))
  ((nonheaded-vector-type? u)
   (cond ((degenerate-vector-type? u)
	  (set! *length-size?* #t)
	  *length-size*)
	 (else
	  (set-nonheaded-vector-type-size?! u #t)
	  (align (+ (align *length-size* *pointer-alignment*) *pointer-size*)
		 (type-alignment u)))))
  ((displaced-vector-type? u)
   (cond ((degenerate-vector-type? u)
	  (set! *length-size?* #t)
	  *length-size*)
	 (else
	  (set-displaced-vector-type-size?! u #t)
	  (align (+ (align *length-size* *pointer-alignment*) *pointer-size*)
		 (type-alignment u)))))
  (else (fuck-up))))

(define (type-set-size w)
 (cond
  ((fictitious? w) (fuck-up))
  ((monomorphic? w)
   (cond ((char-type? (the-member w))
	  (set! *char-size?* #t)
	  *char-size*)
	 (else (type-size (the-member w)))))
  ((tag-only? w)
   (set! *tag-size?* #t)
   *tag-size*)
  ((squeezed? w) (type-size (squeezed-member w)))
  ((squished? w)
   (set! *squished-size?* #t)
   *squished-size*)
  (else
   (set! *tag-size?* #t)
   (set-type-set-size?! w #t)
   (align
    (+ (align *tag-size*
	      ;; needs work: I'm not sure but it may be the case that
	      ;;             union {tau s0;} has different alignment and size
	      ;;             than tau. It also may be the case that something
	      ;;             like union {char s0; short s1;} has different
	      ;;             alignment than char or short and different size
	      ;;             than char max short.
	      ;; note: See the note in TYPE-ALIGNMENT.
	      (reduce max
		      (map type-alignment
			   (members-that
			    (lambda (u)
			     (and (not (char-type? u)) (not (fictitious? u))))
			    w))
		      ;; This can't happen if the type set isn't fictitious,
		      ;; monomorphic, or tag only.
		      #f))
       (reduce max
	       (map type-size
		    (members-that
		     (lambda (u)
		      (and (not (char-type? u)) (not (fictitious? u))))
		     w))
	       ;; This can't happen if the type set isn't fictitious,
	       ;; monomorphic, or tag only.
	       #f))
    (type-set-alignment w)))))

(define (determine-which-types-are-atomic!)
 ;; A type is atomic if it cannot contain pointers that point directly or
 ;; indirectly to heap-allocated data. A heap-allocated type can be atomic.
 ;; needs work: There is a potential optimization here. One can treat pointers
 ;; to stack-allocated objects as atomic because the stack is scavanged. And
 ;; one can treat pointers to objects allocated on nonatomic regions as atomic
 ;; because regions allocated with GC_malloc_uncollectable are also scavanged.
 (for-each (lambda (u) (set-native-procedure-type-atomic?! u #t))
	   *native-procedure-types*)
 (for-each (lambda (u) (set-structure-type-atomic?! u #t)) *structure-types*)
 (for-each (lambda (u) (set-headed-vector-type-atomic?! u #t))
	   *headed-vector-types*)
 (for-each (lambda (u) (set-nonheaded-vector-type-atomic?! u #t))
	   *nonheaded-vector-types*)
 (let loop ()
  (let ((again? #f))
   (for-each
    (lambda (u)
     (when (type-atomic? u)
      (unless (or
	       (fictitious? u)
	       (every
		(lambda (y-e)
		 (let ((e (cdr y-e)))
		  (case *closure-representation*
		   ((immediate-flat)
		    (unimplemented
		     #f "Immediate flat closures are not (yet) implemented"))
		   ((indirect-flat)
		    (unimplemented
		     #f "Indirect flat closures are not (yet) implemented"))
		   ((immediate-display)
		    (every (lambda (e)
			    (and (not (heap-allocation? (allocation e)))
				 (environment-atomic? e)))
			   (ancestors e)))
		   ((indirect-display)
		    (unimplemented
		     #f "Indirect display closures are not (yet) implemented"))
		   ((linked)
		    (or (not (has-parent-parameter? e))
			(and (not (heap-allocation?
				   (allocation (parent-parameter e))))
			     (environment-atomic? (parent-parameter e)))))
		   (else (fuck-up)))))
		(native-procedure-type-call-site-environment-alist u)))
       (set-native-procedure-type-atomic?! u #f)
       (set! again? #t))))
    *native-procedure-types*)
   (for-each
    (lambda (u)
     (when (type-atomic? u)
      (unless (if (structure-type-immediate? u)
		  (every (lambda (w) (must-be? type-atomic? w))
			 (structure-type-slots u))
		  (every (lambda (w)
			  (must-be? (lambda (u)
				     (and (never-allocated-on-the-heap? u)
					  (type-atomic? u)))
				    w))
			 (structure-type-slots u)))
       (set-structure-type-atomic?! u #f)
       (set! again? #t))))
    *structure-types*)
   (for-each
    (lambda (u)
     (when (type-atomic? u)
      (unless (or (degenerate-vector-type? u)
		  (must-be?
		   (lambda (u)
		    (and (never-allocated-on-the-heap? u) (type-atomic? u)))
		   (headed-vector-type-element u)))
       (set-headed-vector-type-atomic?! u #f)
       (set! again? #t))))
    *headed-vector-types*)
   (for-each
    (lambda (u)
     (when (type-atomic? u)
      (unless (or (degenerate-vector-type? u)
		  (must-be?
		   (lambda (u)
		    (and (never-allocated-on-the-heap? u) (type-atomic? u)))
		   (nonheaded-vector-type-element u)))
       (set-nonheaded-vector-type-atomic?! u #f)
       (set! again? #t))))
    *nonheaded-vector-types*)
   (when again? (loop)))))

(define (environment-atomic? e)
 (case *closure-representation*
  ((immediate-flat)
   (unimplemented #f "Immediate flat closures are not (yet) implemented"))
  ((indirect-flat)
   (unimplemented #f "Indirect flat closures are not (yet) implemented"))
  ((immediate-display indirect-display)
   ;; With display closures, closures don't have parent slots. The closure is
   ;; atomic if none of its slots point to heap-allocated data.
   (every (lambda (g)
	   (or (not (slotted? g))
	       (must-be?
		(lambda (u)
		 (and (never-allocated-on-the-heap? u) (type-atomic? u)))
		(variable-type-set g))))
	  (variables e)))
  ((linked)
   ;; With linked closures, closures do have parent slots. The closure is
   ;; atomic if none of its slots, including its parent slot, if it has one,
   ;; point to heap-allocated data.
   (let loop ((e e))
    (and (every (lambda (g)
		 (or (not (slotted? g))
		     (must-be?
		      (lambda (u)
		       (and (never-allocated-on-the-heap? u) (type-atomic? u)))
		      (variable-type-set g))))
		(variables e))
	 (or (not (has-parent-slot? e))
	     (and (not (heap-allocation? (allocation (parent-slot e))))
		  (loop (parent-slot e)))))))
  (else (fuck-up))))

(define (type-atomic? u)
 (cond ((null-type? u) #t)
       ((true-type? u) #t)
       ((false-type? u) #t)
       ((char-type? u) #t)
       ((fixnum-type? u) #t)
       ((flonum-type? u) #t)
       ((rectangular-type? u) #t)
       ((input-port-type? u) #t)
       ((output-port-type? u) #t)
       ((eof-object-type? u) #t)
       ((pointer-type? u) #t)
       ((internal-symbol-type? u) #t)
       ((external-symbol-type? u)
	(type-atomic? (external-symbol-type-displaced-string-type u)))
       ((primitive-procedure-type? u) #t)
       ((native-procedure-type? u) (native-procedure-type-atomic? u))
       ((foreign-procedure-type? u) #t)
       ((continuation-type? u) #t)
       ((string-type? u) #t)
       ((structure-type? u) (structure-type-atomic? u))
       ((headed-vector-type? u) (headed-vector-type-atomic? u))
       ((nonheaded-vector-type? u) (nonheaded-vector-type-atomic? u))
       ((displaced-vector-type? u)
	(type-atomic? (displaced-vector-type-displaced-vector-type u)))
       (else (fuck-up))))

;;; Stalin-specific C constructors

(define (c:a g)
 (if (or (assigned? g) (has-self-tail-call? (variable-environment g)))
     (c:protect (list "a" (number->string (variable-index g))))
     (list "a" (number->string (variable-index g)))))

(define (c:b g) (list "b" (number->string (variable-index g))))

(define (c:c)
 (set! *c:c?* #t)
 "c")

(define (c:d e) (list "d" (number->string (environment-index e))))

(define (c:e e) (list "e" (number->string (environment-index e))))

(define (c:f e) (list "f" (number->string (environment-index e))))

(define (c:h e) (list "h" (number->string (environment-index e))))

(define (c:i c) (c:. c "i"))

(define (c:j x) (list "j" (number->string (expression-index x))))

(define (c:l i) (list "l" (number->string i)))

(define (c:p u/e)
 ;; note: This must use the narrow notion of clone because different wide
 ;;       clones can have different parent parameters.
 (cond ((native-procedure-type? u/e) (c:p (narrow-prototype u/e)))
       ((environment? u/e) (list "p" (number->string (environment-index u/e))))
       (else (fuck-up))))

(define (c:q i) (list "q" (number->string i)))

(define (c:r c/e)
 (if (environment? c/e)
     (list "r" (number->string (environment-index c/e)))
     (c:. c/e "r")))

(define (c:s i) (list "s" (number->string i)))

(define (c:t i) (list "t" (number->string i)))

(define (c:u u)
 (cond ((null-type? u) (fuck-up))
       ((true-type? u) (fuck-up))
       ((false-type? u)	(fuck-up))
       ((char-type? u) (fuck-up))
       ((fixnum-type? u) "fixnum_type")
       ((flonum-type? u) "flonum_type")
       ((rectangular-type? u) "rectangular_type")
       ((input-port-type? u) "input_port_type")
       ((output-port-type? u) "output_port_type")
       ((eof-object-type? u) (fuck-up))
       ((pointer-type? u) "pointer_type")
       ((internal-symbol-type? u) (fuck-up))
       ;; note: There should be only one external symbol type since there
       ;;       should be only one string type after
       ;;       APPLY-CLOSED-WORLD-ASSUMPTION!.
       ((external-symbol-type? u) "external_symbol_type")
       ((primitive-procedure-type? u) (fuck-up))
       ((native-procedure-type? u)
	(list "native_procedure_type" (number->string (type-index u))))
       ((foreign-procedure-type? u) (fuck-up))
       ((continuation-type? u)
	(unless (continuation-type-allocating-expression u) (fuck-up))
	(list "continuation_type"
	      (number->string
	       (expression-index
		(continuation-type-allocating-expression u)))))
       ;; note: There should be only one string type after
       ;;       APPLY-CLOSED-WORLD-ASSUMPTION!.
       ((string-type? u) "string_type")
       ((structure-type? u)
	(list "structure_type" (number->string (structure-type-index u))))
       ((headed-vector-type? u)
	(list "headed_vector_type"
	      (number->string (headed-vector-type-index u))))
       ((nonheaded-vector-type? u)
	(list "nonheaded_vector_type"
	      (number->string (nonheaded-vector-type-index u))))
       ((displaced-vector-type? u)
	(list "displaced_vector_type"
	      (number->string (displaced-vector-type-index u))))
       (else (fuck-up))))

(define (c:v x) (c:protect (list "v" (number->string (expression-index x)))))

(define (c:w w) (list "w" (number->string (type-set-index w))))

(define (c:x x) (list "x" (number->string (expression-index x))))

(define (c:mutex x) (list "mutex" (number->string (expression-index x))))

(define (c:error c) (list c "_error"))

(define (c:fp e)
 (c:protect (list "fp" (number->string (environment-index e)))))

(define (c:sfp e) (list "sfp" (number->string (environment-index e))))

(define (c:data) "data")

(define (c:region . es)
 (when (> (length es) 1) (fuck-up))
 (if (null? es)
     "region"
     (list "region" (number->string (environment-index (first es))))))

(define (c:region-size . es)
 (when (> (length es) 1) (fuck-up))
 (if (null? es)
     "region_size"
     (list "region_size" (number->string (environment-index (first es))))))

(define (c:initial-region e)
 (list "initial_region" (number->string (environment-index e))))

(define (c:big-region-size e)
 (list "REGION_SIZE" (number->string (environment-index e))))

(define (c:stdin)
 (include! "stdio")			;stdin
 "stdin")

(define (c:stdout)
 (include! "stdio")			;stdout
 "stdout")

(define (c:clocks-per-second)
 (include! "time")			;CLOCKS_PER_SEC
 "CLOCKS_PER_SEC")

(define (c:rand-max)
 (include! "stdlib")			;RAND_MAX
 "RAND_MAX")

(define (c:pointer-size) (c:sizeof (space-between "void" "*")))

(define (c:imax c1 c2) (c:call "IMAX" c1 c2))

(define (c:rmax c1 c2) (c:call "RMAX" c1 c2))

(define (c:imin c1 c2) (c:call "IMIN" c1 c2))

(define (c:rmin c1 c2) (c:call "RMIN" c1 c2))

(define (c:pluscc c1 c2) (c:call "PLUSCC" c1 c2))

(define (c:pluscr c1 c2) (c:call "PLUSCR" c1 c2))

(define (c:plusrc c1 c2) (c:call "PLUSRC" c1 c2))

(define (c:negc c) (c:call "NEGC" c))

(define (c:minuscc c1 c2) (c:call "MINUSCC" c1 c2))

(define (c:minuscr c1 c2) (c:call "MINUSCR" c1 c2))

(define (c:minusrc c1 c2) (c:call "MINUSRC" c1 c2))

(define (c:timescc c1 c2) (c:call "TIMESCC" c1 c2))

(define (c:timescr c1 c2) (c:call "TIMESCR" c1 c2))

(define (c:timesrc c1 c2) (c:call "TIMESRC" c1 c2))

(define (c:recipc c) (c:call "RECIPC" c))

(define (c:dividecc c1 c2) (c:call "DIVIDECC" c1 c2))

(define (c:dividecr c1 c2) (c:call "DIVIDECR" c1 c2))

(define (c:dividerc c1 c2) (c:call "DIVIDERC" c1 c2))

(define (c:ipow c1 c2)
 (set! *c:ipow?* #t)
 (c:call "ipow" c1 c2))

(define (c:input-waiting c)
 (set! *c:input-waiting?* #t)
 (c:protect (c:call "input_waiting" c)))

(define (c:panic c)
 (set! *c:panic?* #t)
 (c:gosub "stalin_panic" c))

(define (c:backtrace c1 c2 c3)
 (set! *c:backtrace?* #t)
 (c:gosub "backtrace" c1 c2 c3))

(define (c:backtrace-internal c)
 (set! *c:backtrace-internal?* #t)
 (c:gosub "backtrace_internal" c))

(define (c:align c)
 (if (positive? *allocation-alignment*) (c:gosub "ALIGN" c) (c:noop)))

(define (c:value-offset) "VALUE_OFFSET")

(define (c:char-offset) "CHAR_OFFSET")

(define (c:type u c)
 (cond
  ((char-type? u) (space-between *char* c))
  ((fixnum-type? u) (space-between *fixnum* c))
  ((flonum-type? u) (space-between *flonum* c))
  ((rectangular-type? u) (space-between "struct" "rectangular" c))
  ((input-port-type? u)
   (include! "stdio")			;FILE
   (space-between *file* (star-before c)))
  ((output-port-type? u)
   (include! "stdio")			;FILE
   (space-between *file* (star-before c)))
  ((pointer-type? u) (space-between "void" (star-before c)))
  ((external-symbol-type? u) (space-between *char* (star-before c)))
  ((native-procedure-type? u)
   (case *closure-representation*
    ((immediate-flat immediate-display) (space-between "struct" (c:p u) c))
    ((indirect-flat indirect-display)
     (space-between "struct" (c:p u) (star-before c)))
    ((linked)
     (if (and (or (eq? *closure-conversion-method* 'baseline)
		  (eq? *closure-conversion-method* 'conventional))
	      (or (not (environment?
			(native-procedure-type-narrow-prototype u)))
		  (not (environment-used? (narrow-prototype u)))
		  (not (environment? (parent-parameter u)))
		  (not (environment-used? (parent-parameter u)))))
	 (space-between (c:/**/ "fake") *fixnum* c)
	 (space-between "struct" (c:p (parent-parameter u)) (star-before c))))
    (else (fuck-up))))
  ((continuation-type? u)
   (include! "setjmp")			;jmp_buf
   (space-between *jmpbuf* (star-before c)))
  ((string-type? u) (space-between *char* (star-before c)))
  ((structure-type? u)
   (if (structure-type-immediate? u)
       (space-between "struct" (c:u u) c)
       (space-between "struct" (c:u u) (star-before c))))
  ((headed-vector-type? u)
   (if (degenerate-vector-type? u)
       (space-between *length* c)
       (space-between "struct" (c:u u) (star-before c))))
  ((nonheaded-vector-type? u)
   (if (degenerate-vector-type? u)
       (space-between *length* c)
       (space-between "struct" (c:u u) c)))
  ((displaced-vector-type? u)
   (if (degenerate-vector-type? u)
       (space-between *length* c)
       (space-between "struct" (c:u u) c)))
  (else (case *closure-conversion-method*
	 ((baseline conventional) (space-between (c:/**/ "fake") *fixnum* c))
	 ((lightweight) (fuck-up))
	 (else (fuck-up))))))

(define (c:type& u c)
 (unless (and (structure-type? u)
	      (not (structure-type-immediate? u))
	      (not (every fictitious? (structure-type-slots u))))
  (fuck-up))
 (space-between "struct" (c:u u) c))

(define (c:type-set w c)
 (cond ((fictitious? w) (fuck-up))
       ((monomorphic? w) (c:type (the-member w) c))
       ((tag-only? w) (space-between *tag* c))
       ((squeezed? w) (c:type (squeezed-member w) c))
       ((squished? w) (space-between *squished* c))
       (else (space-between "struct" (c:w w) c))))

(define (c:type-cast c u) (c:cast (c:type u "") c))

(define (c:type-set-cast c w) (c:cast (c:type-set w "") c))

(define (c:squished-cast c)
 (set! *squished-size?* #t)
 (c:cast *squished* c))

(define (c:signed-squished-cast c)
 (set! *squished-size?* #t)
 (c:cast *signed-squished* c))

(define (c:tag-cast c) (c:cast *tag* c))

(define (c:tag->squeezed-cast c w1 w2)
 (set! *tag-size?* #t)
 (if (= (type-set-size w2) *tag-size*)
     (c:type-set-cast (c:tag c w1) w2)
     ;; The squished cast is needed because *POINTER-SIZE* is not the same as
     ;; *TAG-SIZE*.
     (c:type-set-cast (c:squished-cast (c:tag c w1)) w2)))

(define (c:squeezed->tag-cast c w)
 (set! *tag-size?* #t)
 (if (= (type-set-size w) *tag-size*)
     (c:tag-cast c)
     ;; The squished cast is needed because *POINTER-SIZE* is not the same as
     ;; *TAG-SIZE*.
     (c:tag-cast (c:squished-cast c))))

(define (c:forgery-cast c w) (c:* (c:cast (c:type-set w "*") (c:& c))))

(define (c:type-tag u)
 (cond ((null-type? u)
	(set! *null-type-use-count* (+ *null-type-use-count* 1))
	"NULL_TYPE")
       ((true-type? u)
	(set! *true-type-use-count* (+ *true-type-use-count* 1))
	"TRUE_TYPE")
       ((false-type? u)
	(set! *false-type-use-count* (+ *false-type-use-count* 1))
	"FALSE_TYPE")
       ((char-type? u) (fuck-up))
       ((fixnum-type? u)
	(set! *fixnum-type-use-count* (+ *fixnum-type-use-count* 1))
	"FIXNUM_TYPE")
       ((flonum-type? u)
	(set! *flonum-type-use-count* (+ *flonum-type-use-count* 1))
	"FLONUM_TYPE")
       ((rectangular-type? u)
	(set! *rectangular-type-use-count* (+ *rectangular-type-use-count* 1))
	"RECTANGULAR_TYPE")
       ((input-port-type? u)
	(set! *input-port-type-use-count* (+ *input-port-type-use-count* 1))
	"INPUT_PORT_TYPE")
       ((output-port-type? u)
	(set! *output-port-type-use-count* (+ *output-port-type-use-count* 1))
	"OUTPUT_PORT_TYPE")
       ((eof-object-type? u)
	(set! *eof-object-type-use-count* (+ *eof-object-type-use-count* 1))
	"EOF_OBJECT_TYPE")
       ((pointer-type? u)
	(set! *pointer-type-use-count* (+ *pointer-type-use-count* 1))
	"POINTER_TYPE")
       ((internal-symbol-type? u)
	(set-internal-symbol-type-use-count!
	 u (+ (internal-symbol-type-use-count u) 1))
	;; note: We can't use the symbol name without mangling it.
	(list "INTERNAL_SYMBOL_TYPE"
	      (number->string (internal-symbol-type-index u))))
       ((external-symbol-type? u)
	(set-external-symbol-type-use-count!
	 u (+ (external-symbol-type-use-count u) 1))
	;; note: There should be only one external symbol type since there
	;;       should be only one string type after
	;;       APPLY-CLOSED-WORLD-ASSUMPTION!.
	"EXTERNAL_SYMBOL_TYPE")
       ((primitive-procedure-type? u)
	(set-primitive-procedure-type-use-count!
	 u (+ (primitive-procedure-type-use-count u) 1))
	;; note: We can't use the primitive procedure name (and arguments)
	;;       without mangling it.
	(list "PRIMITIVE_PROCEDURE_TYPE"
	      (number->string (primitive-procedure-type-index u))))
       ((native-procedure-type? u)
	(set-native-procedure-type-use-count!
	 u (+ (native-procedure-type-use-count u) 1))
	(list "NATIVE_PROCEDURE_TYPE" (number->string (type-index u))))
       ((foreign-procedure-type? u)
	(set-foreign-procedure-type-use-count!
	 u (+ (foreign-procedure-type-use-count u) 1))
	(list "FOREIGN_PROCEDURE_" (foreign-procedure-type-name u) "_TYPE"))
       ((continuation-type? u)
	(set-continuation-type-use-count!
	 u (+ (continuation-type-use-count u) 1))
	(if (continuation-type-allocating-expression u)
	    (list "CONTINUATION_TYPE"
		  (number->string
		   (expression-index
		    (continuation-type-allocating-expression u))))
	    "TOP_LEVEL_CONTINUATION_TYPE"))
       ((string-type? u)
	(set-string-type-use-count! u (+ (string-type-use-count u) 1))
	;; note: There should be only one string type after
	;;       APPLY-CLOSED-WORLD-ASSUMPTION!.
	"STRING_TYPE")
       ((structure-type? u)
	(set-structure-type-use-count! u (+ (structure-type-use-count u) 1))
	(list "STRUCTURE_TYPE" (number->string (structure-type-index u))))
       ((headed-vector-type? u)
	(set-headed-vector-type-use-count!
	 u (+ (headed-vector-type-use-count u) 1))
	(list "HEADED_VECTOR_TYPE"
	      (number->string (headed-vector-type-index u))))
       ((nonheaded-vector-type? u)
	(set-nonheaded-vector-type-use-count!
	 u (+ (nonheaded-vector-type-use-count u) 1))
	(list "NONHEADED_VECTOR_TYPE"
	      (number->string (nonheaded-vector-type-index u))))
       ((displaced-vector-type? u)
	(set-displaced-vector-type-use-count!
	 u (+ (displaced-vector-type-use-count u) 1))
	(list "DISPLACED_VECTOR_TYPE"
	      (number->string (displaced-vector-type-index u))))
       (else (fuck-up))))

(define (c:tag c w)
 (cond ((fictitious? w) (fuck-up))
       ((monomorphic? w) (fuck-up))
       ((tag-only? w) c)
       ((squeezed? w) (fuck-up))
       ((squished? w) (fuck-up))
       (else (c:. c "tag"))))

(define (c:value c u w)
 (unless (and (member? u w)
	      (or (eq? *closure-conversion-method* 'baseline)
		  (eq? *closure-conversion-method* 'conventional)
		  (not (fictitious? u))))
  (fuck-up))
 (cond
  ((and (or (eq? *closure-conversion-method* 'baseline)
	    (eq? *closure-conversion-method* 'conventional))
	(fictitious? u))
   "fake")
  ((fictitious? w) (fuck-up))
  ((monomorphic? w) c)
  ((tag-only? w)
   ;; note: Converting from tag-only to character used to be free but now
   ;;       requires a right shift when there is some squishing. This is the
   ;;       price to pay for universal type tags.
   (unless (char-type? u) (fuck-up))
   ;; This assumes that *TAG* is unsigned so that >> does a logical shift.
   ;; This also assumes that casting a *TAG* to a *CHAR* does not modify the
   ;; bit pattern except for truncation.
   (c:type-cast (c:>> (c:tag c w) (c:fixnum *worst-alignment*)) u))
  ((squeezed? w)
   ;; note: Unsqueezing characters used to be free but now requires a right
   ;;       shift when there is some squishing. This is the price to pay for
   ;;       universal type tags.
   (if (char-type? u)
       ;; The C:SQUISHED-CAST is needed to convert the pointer to an integer.
       ;; This assumes that *SQUISHED* is unsigned so that >> does a logical
       ;; shift. This also assumes that casting a *SQUISHED* to a *CHAR* does
       ;; not modify the bit pattern except for truncation.
       (c:type-cast (c:>> (c:squished-cast c) (c:fixnum *worst-alignment*)) u)
       c))
  ((squished? w)
   (cond
    ;; This assumes that *SQUISHED* is unsigned so that >> does a logical
    ;; shift. This also assumes that casting a *SQUISHED* to a *CHAR* does
    ;; not modify the bit pattern except for truncation.
    ((char-type? u) (c:type-cast (c:>> c (c:fixnum *worst-alignment*)) u))
    ((fixnum-type? u)
     ;; This assumes that *SQUISHED* is unsigned and *FIXNUM* is signed so
     ;; that C:SIGNED-SQUISHED-CAST is needed to cause >> to do an arithmetic
     ;; shift to preserve the sign bit. This assumes that *SIGNED-SQUISHED* is
     ;; signed. This also assumes that casting a *SQUISHED* to a
     ;; *SIGNED-SQUISHED* does not modify the bit pattern. The final cast from
     ;; *SIGNED-SQUISHED* to *FIXNUM* may change the size and thus may require
     ;; sign extension.
     (c:type-cast
      (c:>> (c:signed-squished-cast c) (c:fixnum (squish-alignment w))) u))
    ((degenerate-vector-type? u)
     ;; This assumes that both *SQUISHED* and *LENGTH* are unsigned so that >>
     ;; does a logical shift. The final cast from *SQUISHED* to *LENGTH* may
     ;; change the size but should not modify the bit pattern.
     (c:type-cast (c:>> c (c:fixnum (squish-alignment w))) u))
    (else (c:type-cast (strip-known-squish-tag c u w) u))))
  (else
   ;; note: Converting from general to character used to be free but now
   ;;       requires a right shift when there is some squishing. This is the
   ;;       price to pay for universal type tags.
   (if (char-type? u)
       ;; This assumes that *TAG* is unsigned so that >> does a logical shift.
       ;; If *CHAR* is signed then this also assumes that casting an unsigned
       ;; expression to a signed char does not modify the bit pattern except
       ;; for truncation.
       (c:type-cast (c:>> (c:tag c w) (c:fixnum *worst-alignment*)) u)
       (if (has-union? w) (c:. (c:. c "value") (c:u u)) (c:. c "value"))))))

(define (c:foreign-type f c)
 (case f
  ((char) (space-between "char" c))
  ((signed-char) (space-between "signed" "char" c))
  ((unsigned-char) (space-between "unsigned" "char" c))
  ((short) (space-between "short" c))
  ((unsigned-short) (space-between "unsigned" "short" c))
  ((int) (space-between "int" c))
  ((unsigned) (space-between "unsigned" c))
  ((long) (space-between "long" c))
  ((unsigned-long) (space-between "unsigned" "long" c))
  ((float) (space-between "float" c))
  ((double) (space-between "double" c))
  ((long-double) (space-between "long" "double" c))
  ((char*) (space-between "char" (star-before c)))
  ((file* input-port output-port)
   (include! "stdio")			;FILE
   (space-between *file* (star-before c)))
  ((void*) (space-between "void" (star-before c)))
  ((void) (space-between "void" c))
  ((no-return) (space-between "void" c))
  (else (fuck-up))))

;;; End of C constructors

(define (squeeze c u w)
 (unless (and (member? u w) (squeezed? w)) (fuck-up))
 (cond
  ((char-type? u)
   ;; note: Squeezing characters used to be free but now requires a left shift
   ;;       when there is some squishing. This is the price to pay for
   ;;       universal type tags.
   ;; This assumes that *SQUISHED* is unsigned so that << does a logical
   ;; shift. The call to C:UNSIGNED-CHAR-CAST is in case *CHAR* is signed to
   ;; force << to be a logical shift without a prior sign extend. The call to
   ;; C:SQUISHED-CAST is to prevent any overflow in the logical shift.
   (c:type-set-cast (c:<< (c:squished-cast (c:unsigned-char-cast c))
			  (c:fixnum *worst-alignment*))
		    w))
  ((fictitious? u) (c:type-set-cast (c:type-tag u) w))
  (else c)))

(define (squeeze-tag-test c u w)
 (unless (and (member? u w) (squeezed? w)) (fuck-up))
 (cond ((char-type? u) (c:< c (c:type-set-cast (c:char-offset) w)))
       ((fictitious? u) (c:== c (c:type-set-cast (c:type-tag u) w)))
       (else (c:>= c (c:type-set-cast (c:value-offset) w)))))

(define (assign-global-squish-tags!)
 ;; This is a special case for when the type set members are sorted.
 (define (unionq us1 us2)
  (cond ((null? us1) us2)
	((null? us2) us1)
	((eq? (first us1) (first us2))
	 (cons (first us1) (unionq (rest us1) (rest us2))))
	((< (type-index (first us1)) (type-index (first us2)))
	 (cons (first us1) (unionq (rest us1) us2)))
	(else (cons (first us2) (unionq us1 (rest us2))))))
 (set! *uss*
       (let ((uss (let ((ws (remove-if-not squished? *ws*)))
		   (equivalence-classesp
		    (lambda (u1 u2)
		     (some (lambda (w) (and (member? u1 w) (member? u2 w)))
			   ws))
		    (remove-if (lambda (u) (or (char-type? u) (fictitious? u)))
			       (reduce unionq (map members ws) '()))))))
	(map
	 (lambda (us)
	  (if (every (lambda (u)
		      (or (fixnum-type? u)
			  ;; needs work: Can be extended to allow squishing a
			  ;;             singleton immediate structure if its
			  ;;             slot is squished.
			  (degenerate-vector-type? u)))
		     us)
	      ;; In this case, there are no pointer members to get squish tag
	      ;; zero.
	      (cons 'foo us)
	      ;; This guarantees that a pointer member is assigned squish tag
	      ;; zero.
	      (sort us
		    (lambda (u1 u2)
		     ;; needs work: Can be extended to allow squishing a
		     ;;             singleton immediate structure if its slot
		     ;;             is squished.
		     (or (fixnum-type? u2) (degenerate-vector-type? u2)))
		    identity)))
	 uss))))

(define (squish-tag u w)
 (unless (and (member? u w) (squished? w)) (fuck-up))
 ;; note: This is complicated by the need to guarantee that fixnums and
 ;;       degenerate vectors aren't assigned squish tag zero since they
 ;;       aren't sparse.
 ;; needs work: Actually, can assign squish tag zero to a fixnum or degenerate
 ;;             vector when the type set has no members that are fictitious.
 ;;             Would also need to modify SQUISH-ALIGNMENT and
 ;;             ASSIGN-GLOBAL-SQUISH-TAGS! as well.
 ;; note: Since squish tag zero is more costly to test than other squish
 ;;       tags one might try to assign it only when necessary. But on the
 ;;       other hand squish tag zero is less costly to squish so it is not
 ;;       clear whether this would be a useful optimization.
 (let ((us (find-if (lambda (us) (memq u us)) *uss*)))
  (cond ((char-type? u) 0)
	((fictitious? u) 0)
	((<= (length us) (expt 2 (squish-alignment w))) (positionq u us))
	((can-be? pointer-member? w)
	 ;; This guarantees that a pointer member is assigned squish tag zero.
	 (positionq
	  u
	  (sort (members-that
		 (lambda (u) (and (not (char-type? u)) (not (fictitious? u))))
		 w)
		(lambda (u1 u2)
		 ;; needs work: Can be extended to allow squishing a singleton
		 ;;             immediate structure if its slot is squished.
		 (or (fixnum-type? u2) (degenerate-vector-type? u2)))
		identity)))
	(else
	 ;; In this case, there are no pointer members to get squish tag zero.
	 (+ (positionq
	     u (members-that
		(lambda (u) (and (not (char-type? u)) (not (fictitious? u))))
		w))
	    1)))))

(define (squish c u w)
 (unless (and (member? u w) (squished? w)) (fuck-up))
 (cond
  ;; This assumes that *SQUISHED* is unsigned so that << does a logical shift.
  ;; The call to C:UNSIGNED-CHAR-CAST is in case *CHAR* is signed to force <<
  ;; to be a logical shift without a prior sign extend. The call to
  ;; C:TYPE-SET-CAST is to prevent any overflow in the logical shift.
  ((char-type? u)
   (c:<< (c:type-set-cast (c:unsigned-char-cast c) w)
	 (c:fixnum *worst-alignment*)))
  ((fictitious? u) (c:type-tag u))
  (else
   (c:+ (cond
	 ((or (fixnum-type? u) (degenerate-vector-type? u))
	  (when *overflow-checks?*
	   (unimplemented #f "Safe exact arithmetic is not (yet) implemented"))
	  (c:<< (c:type-set-cast c w) (c:fixnum (squish-alignment w))))
	 (else (c:type-set-cast c w)))
	(c:fixnum (squish-tag u w))))))

(define (strip-squish-tag c w)
 (c:& c (c:~ (c:fixnum (- (expt 2 (squish-alignment w)) 1)))))

(define (strip-known-squish-tag c u w) (c:- c (c:fixnum (squish-tag u w))))

(define (extract-squish-tag c w)
 (c:& c (c:fixnum (- (expt 2 (squish-alignment w)) 1))))

(define (squish-tag-test c u w)
 (unless (and (member? u w) (squished? w)) (fuck-up))
 (cond ((char-type? u) (c:< c (c:char-offset)))
       ((fictitious? u) (c:== c (c:type-tag u)))
       ((and (zero? (squish-tag u w))
	     (can-be? (lambda (u) (or (char-type? u) (fictitious? u))) w))
	;; note: Squish tag zero is more costly to test than other squish tags
	;;       because of squeezing.
	(c:&& (c:>= c (c:value-offset)) (c:==0 (extract-squish-tag c w))))
       (else (c:== (extract-squish-tag c w) (c:fixnum (squish-tag u w))))))

(define (compile-squeezed-type-if us w c1 c2 c3)
 (let ((cs2 (map c2 us)))
  (let loop ((cases (equate-cases
		     (map (lambda (u) (squeeze-tag-test c1 u w)) us) cs2)))
   ;; conventions: CASES
   (if (null? cases)
       (c3 #f)
       (c:if (apply c:boolean-or (map first (first cases)))
	     (second (first (first cases)))
	     (loop (rest cases))
	     #f)))))

(define (compile-squeezed-defaultless-type-if us w c1 c2)
 ;; This has the semantics that the behaviour is undefined when C1 takes on
 ;; a type that is not in US.
 (let loop ((cases
	     (equate-cases (map (lambda (u) (squeeze-tag-test c1 u w)) us)
			   (map c2 us))))
  ;; conventions: CASES
  (cond ((null? cases) (c:noop))
	((null? (rest cases)) (second (first (first cases))))
	(else (c:if (apply c:boolean-or (map first (first cases)))
		    (second (first (first cases)))
		    (loop (rest cases))
		    #f)))))

(define (compile-squeezed-type-switch us w c1 c2 c3 p?)
 (define (c u) (if (memq u us) (c2 u) (c3 #f)))
 (if (and (not (some fictitious? us)) (can-be? fictitious? w))
     (if (some char-type? us)
	 (if (memq (squeezed-member w) us)
	     (c:if (c:< c1 (c:type-set-cast (c:char-offset) w))
		   (c2 (the-member-that char-type? w))
		   (c:if (c:>= c1 (c:type-set-cast (c:value-offset) w))
			 (c2 (squeezed-member w))
			 (c3 #f)
			 #f)
		   #f)
	     (c:if (c:< c1 (c:type-set-cast (c:char-offset) w))
		   (c2 (the-member-that char-type? w))
		   (c3 #f)
		   #f))
	 (if (memq (squeezed-member w) us)
	     (c:if (c:>= c1 (c:type-set-cast (c:value-offset) w))
		   (c2 (squeezed-member w))
		   (c3 #f)
		   #f)
	     (c3 #f)))
     (let ((us (members-that fictitious? w)))
      (c:switch (c:squeezed->tag-cast c1 w)
		(map c:type-tag us)
		(map c us)
		(if (can-be? char-type? w)
		    (c:if (c:< c1 (c:type-set-cast (c:char-offset) w))
			  (c (the-member-that char-type? w))
			  (c (squeezed-member w))
			  #f)
		    (c (squeezed-member w)))
		p?))))

(define (compile-squeezed-defaultless-type-switch us w c1 c2 p?)
 ;; This has the semantics that the behaviour is undefined when C1 takes on
 ;; a type that is not in US.
 (let ((us1 (remove-if-not fictitious? us)))
  (if (or (some char-type? us) (memq (squeezed-member w) us))
      (c:switch (c:squeezed->tag-cast c1 w)
		(map c:type-tag us1)
		(map c2 us1)
		(if (some char-type? us)
		    (if (memq (squeezed-member w) us)
			(c:if (c:< c1 (c:type-set-cast (c:char-offset) w))
			      (c2 (the-member-that char-type? w))
			      (c2 (squeezed-member w))
			      #f)
			(c2 (the-member-that char-type? w)))
		    (c2 (squeezed-member w)))
		p?)
      (c:defaultless-switch
       (c:squeezed->tag-cast c1 w) (map c:type-tag us1) (map c2 us1) p?))))

(define (compile-squished-type-if us w c1 c2 c3)
 (let ((cs2 (map c2 us)))
  (let loop ((cases (equate-cases
		     (map (lambda (u) (squish-tag-test c1 u w)) us) cs2)))
   ;; conventions: CASES
   (if (null? cases)
       (c3 #f)
       (c:if (apply c:boolean-or (map first (first cases)))
	     (second (first (first cases)))
	     (loop (rest cases))
	     #f)))))

(define (compile-squished-defaultless-type-if us w c1 c2)
 ;; This has the semantics that the behaviour is undefined when C1 takes on
 ;; a type that is not in US.
 (let loop ((cases (equate-cases (map (lambda (u) (squish-tag-test c1 u w)) us)
				 (map c2 us))))
  ;; conventions: CASES
  (cond ((null? cases) (c:noop))
	((null? (rest cases)) (second (first (first cases))))
	(else (c:if (apply c:boolean-or (map first (first cases)))
		    (second (first (first cases)))
		    (loop (rest cases))
		    #f)))))

(define (compile-squished-type-switch us w c1 c2 c3 p?)
 ;; We dispatch on C1 of type W. If the type U of C1 is in US we generate
 ;; code by calling (C2 U). Otherwise we generate it with (C3 #f) because the
 ;; error might occur.
 ;; In our case W should be (union fixnum char) and US should be {char}.
 ;; Neither fixnum nor char are fictitious.
 ;; If W can be both a character and a nonfictitous noncharacter, then US2 is
 ;; is the set of all nonfictitous noncharacter types in W. Otherwise US2 is
 ;; the set of all nonfictitious types in W. I think that this means that US2
 ;; is the set of all nonfictitous noncharacter types in W except when W has
 ;; char but no other nonfictitious types in which case it is just char.
 ;; In our case US2 should be {FIXNUM} and US1 should be empty.
 (define (c u) (if (memq u us) (c2 u) (c3 #f)))
 (if (and (not (some fictitious? us)) (can-be? fictitious? w))
     ;; This case is taken when some fictitious types take the C3 branch.
     (let ((us2 (if (can-be?
		     (lambda (u)
		      (and (pointer-member? u) (zero? (squish-tag u w))))
		     w)
		    (members-that
		     (lambda (u)
		      (and (not (char-type? u)) (not (fictitious? u)))) w)
		    (members-that (lambda (u) (not (fictitious? u))) w))))
      ;; This holds when W contains a fictitious type or a nonfictitious type
      ;; with the same squish tag.
      (if (can-be? (lambda (u)
		    (or (fictitious? u)
			(some (lambda (u1)
			       (and (not (fictitious? u1))
				    (= (squish-tag u w) (squish-tag u1 w))))
			      us)))
		   w)
	  (c:switch
	   (extract-squish-tag c1 w)
	   (map (lambda (u) (c:fixnum (squish-tag u w))) us2)
	   (map
	    (lambda (u)
	     (if (zero? (squish-tag u w))
		 (if (and (can-be? char-type? w) (not (char-type? u)))
		     (c:if (c:< c1 (c:type-set-cast (c:char-offset) w))
			   (c (the-member-that char-type? w))
			   (c:if (c:>= c1 (c:type-set-cast (c:value-offset) w))
				 (c u)
				 (c3 #f)
				 #f)
			   #f)
		     (c:if (c:>= c1 (c:type-set-cast (c:value-offset) w))
			   (c u)
			   (c3 #f)
			   #f))
		 (c u)))
	    us2)
	   (c3 #f)
	   p?)
	  (c:defaultless-switch
	   (extract-squish-tag c1 w)
	   (map (lambda (u) (c:fixnum (squish-tag u w))) us2)
	   (map
	    (lambda (u)
	     (if (zero? (squish-tag u w))
		 (if (and (can-be? char-type? w) (not (char-type? u)))
		     (c:if (c:< c1 (c:type-set-cast (c:char-offset) w))
			   (c (the-member-that char-type? w))
			   (c:if (c:>= c1 (c:type-set-cast (c:value-offset) w))
				 (c u)
				 (c3 #f)
				 #f)
			   #f)
		     (c:if (c:>= c1 (c:type-set-cast (c:value-offset) w))
			   (c u)
			   (c3 #f)
			   #f))
		 (c u)))
	    us2)
	   p?)))
     (let ((us1 (members-that fictitious? w))
	   ;; Exclude char if it can have a pointer member.
	   (us2 (if (can-be?
		     (lambda (u)
		      (and (pointer-member? u) (zero? (squish-tag u w))))
		     w)
		    (members-that
		     (lambda (u)
		      (and (not (char-type? u)) (not (fictitious? u)))) w)
		    (members-that (lambda (u) (not (fictitious? u))) w))))
      (c:switch
       c1
       (map c:type-tag us1)
       (map c us1)
       ;; This test is used only to decide if there needs to be a default in
       ;; the switch for C3. It appears to be bogus.
       (if (can-be? (lambda (u)
		     (or (fictitious? u)
			 (some (lambda (u1)
				(and (not (fictitious? u1))
				     (= (squish-tag u w) (squish-tag u1 w))))
			       us)))
		    w)
	   (c:switch
	    (extract-squish-tag c1 w)
	    (map (lambda (u) (c:fixnum (squish-tag u w))) us2)
	    (map (lambda (u)
		  (if (and (zero? (squish-tag u w))
			   (can-be? char-type? w)
			   (not (char-type? u)))
		      (c:if (c:< c1 (c:type-set-cast (c:char-offset) w))
			    (c (the-member-that char-type? w))
			    (c u)
			    #f)
		      (c u)))
		 us2)
	    (c3 #f)
	    p?)
	   (c:defaultless-switch
	    (extract-squish-tag c1 w)
	    (map (lambda (u) (c:fixnum (squish-tag u w))) us2)
	    (map (lambda (u)
		  (if (and (zero? (squish-tag u w))
			   (can-be? char-type? w)
			   (not (char-type? u)))
		      (c:if (c:< c1 (c:type-set-cast (c:char-offset) w))
			    (c (the-member-that char-type? w))
			    (c u)
			    #f)
		      (c u)))
		 us2)
	    p?))
       p?))))

(define (compile-squished-defaultless-type-switch us w c1 c2 p?)
 ;; This has the semantics that the behaviour is undefined when C1 takes on
 ;; a type that is not in US.
 (if (every fictitious? us)
     (c:defaultless-switch c1 (map c:type-tag us) (map c2 us) p?)
     (let ((us1 (remove-if-not fictitious? us))
	   ;; US1 are all of the fictitious members. US2 are all of the
	   ;; nonfictitious members. US2 only contains char is US contains
	   ;; char and does not contain some other nonfictitous nonchar.
	   (us2 (if (some (lambda (u)
			   (and (pointer-member? u) (zero? (squish-tag u w))))
			  us)
		    (remove-if
		     (lambda (u) (or (char-type? u) (fictitious? u))) us)
		    (remove-if fictitious? us))))
      (c:switch c1
		(map c:type-tag us1)
		(map c2 us1)
		(c:defaultless-switch
		 (extract-squish-tag c1 w)
		 (map (lambda (u) (c:fixnum (squish-tag u w))) us2)
		 (map (lambda (u)
		       (if (and (zero? (squish-tag u w))
				(some char-type? us)
				(not (char-type? u)))
			   (c:if (c:< c1 (c:type-set-cast (c:char-offset) w))
				 (c2 (the-member-that char-type? w))
				 (c2 u)
				 #f)
			   (c2 u)))
		      us2)
		 p?)
		p?))))

(define (compile-type-switch us w c1 c2 c3 p?)
 (define (c u) (if (memq u us) (c2 u) (c3 #f)))
 (if (and (every char-type? us) (can-be-non? char-type? w))
     (if (some char-type? us)
	 (c:if (c:< (c:tag c1 w) (c:char-offset))
	       (c2 (the-member-that char-type? w))
	       (c3 #f)
	       #f)
	 (c3 #f))
     (let ((us (members-that (lambda (u) (not (char-type? u))) w)))
      (if (can-be? char-type? w)
	  (c:switch (c:tag c1 w)
		    (map c:type-tag us)
		    (map c us)
		    (c (the-member-that char-type? w))
		    p?)
	  (c:defaultless-switch
	   (c:tag c1 w) (map c:type-tag us) (map c us) p?)))))

(define (compile-defaultless-type-switch us w c1 c2 p?)
 ;; This has the semantics that the behaviour is undefined when C1 takes on
 ;; a type that is not in US.
 (if (some char-type? us)
     (let ((us (remove-if char-type? us)))
      (c:switch (c:tag c1 w)
		(map c:type-tag us)
		(map c2 us)
		(c2 (the-member-that char-type? w))
		p?))
     (c:defaultless-switch (c:tag c1 w) (map c:type-tag us) (map c2 us) p?)))

(define (break? r)
 ;; needs work: Should also eliminate the break when the code generated by a
 ;;             particular C2 doesn't return as would be the case if it were a
 ;;             call to a nonconverted continuation.
 (or (accessor? r) (discard? r) (antecedent? r)))

(define (type-switch m w r c1 c2 c3)
 ;; This dispatches on the type W of C1. All members U of W that satisfy M
 ;; have code generated by C2 applied to U. Members that don't have code
 ;; generated by C3. If *TYPE-CHECKS?* is false then code is not generated for
 ;; members that don't satisfy M. Thus C3 should only generate error code.
 ;; C3 is called with #T if the error must occur and #F if the error might
 ;; occur.
 (cond ((squeezed? w)
	(if (can-be? m w)
	    (if (and *type-checks?* (can-be-non? m w))
		(if *type-if?*
		    (compile-squeezed-type-if (members-that m w) w c1 c2 c3)
		    (compile-squeezed-type-switch
		     (members-that m w) w c1 c2 c3 (break? r)))
		;; note: -Ot will also eliminate warnings.
		(if *type-if?*
		    (compile-squeezed-defaultless-type-if
		     (members-that m w) w c1 c2)
		    (compile-squeezed-defaultless-type-switch
		     (members-that m w) w c1 c2 (break? r))))
	    (if (can-be-non? m w) (c3 #t) (fuck-up))))
       ((squished? w)
	(if (can-be? m w)
	    (if (and *type-checks?* (can-be-non? m w))
		(if *type-if?*
		    (compile-squished-type-if (members-that m w) w c1 c2 c3)
		    (compile-squished-type-switch
		     (members-that m w) w c1 c2 c3 (break? r)))
		;; note: -Ot will also eliminate warnings.
		(if *type-if?*
		    (compile-squished-defaultless-type-if
		     (members-that m w) w c1 c2)
		    (compile-squished-defaultless-type-switch
		     (members-that m w) w c1 c2 (break? r))))
	    (if (can-be-non? m w) (c3 #t) (fuck-up))))
       ;; needs work: There is no COMPILE-TYPE-IF and
       ;;             COMPILE-DEFAULTLESS-TYPE-IF.
       (else (if (can-be? m w)
		 (if (and *type-checks?* (can-be-non? m w))
		     (compile-type-switch
		      (members-that m w) w c1 c2 c3 (break? r))
		     ;; note: -Ot will also eliminate warnings.
		     (if (null? (rest (members-that m w)))
			 (c2 (first (members-that m w)))
			 (compile-defaultless-type-switch
			  (members-that m w) w c1 c2 (break? r))))
		 (if (can-be-non? m w) (c3 #t) (fuck-up))))))

(define (nonchecking-type-switch m w r c1 c2)
 ;; This dispatches on the type W of C1. Some member U of W must satisfy M.
 ;; All members U of W that satisfy M have code generated by C2 applied to U.
 ;; Code is not generated for members that don't satisfy W.
 (unless (can-be? m w) (fuck-up))
 (cond ((squeezed? w)
	(if *type-if?*
	    (compile-squeezed-defaultless-type-if (members-that m w) w c1 c2)
	    (compile-squeezed-defaultless-type-switch
	     (members-that m w) w c1 c2 (break? r))))
       ((squished? w)
	(if *type-if?*
	    (compile-squished-defaultless-type-if (members-that m w) w c1 c2)
	    (compile-squished-defaultless-type-switch
	     (members-that m w) w c1 c2 (break? r))))
       ;; needs work: There is no COMPILE-DEFAULTLESS-TYPE-IF.
       (else (compile-defaultless-type-switch
	      (members-that m w) w c1 c2 (break? r)))))

(define (nonerror-type-switch m w r c1 c2 c3)
 ;; This dispatches on the type W of C1. All members U of W that satisfy M
 ;; have code generated by C2 applied to U. Members that don't have code
 ;; generated by C3. Code is generated for the members that don't satisfy W
 ;; irrespective of the setting of *TYPE-CHECKS?*. C3 is called with #T if
 ;; no member of W satisfies M and #F otherwise.
 (cond ((squeezed? w)
	(if (can-be? m w)
	    (if (can-be-non? m w)
		(if *type-if?*
		    (compile-squeezed-type-if (members-that m w) w c1 c2 c3)
		    (compile-squeezed-type-switch
		     (members-that m w) w c1 c2 c3 (break? r)))
		(if *type-if?*
		    (compile-squeezed-defaultless-type-if
		     (members-that m w) w c1 c2)
		    (compile-squeezed-defaultless-type-switch
		     (members-that m w) w c1 c2 (break? r))))
	    (if (can-be-non? m w) (c3 #t) (fuck-up))))
       ((squished? w)
	(if (can-be? m w)
	    (if (can-be-non? m w)
		(if *type-if?*
		    (compile-squished-type-if (members-that m w) w c1 c2 c3)
		    (compile-squished-type-switch
		     (members-that m w) w c1 c2 c3 (break? r)))
		(if *type-if?*
		    (compile-squished-defaultless-type-if
		     (members-that m w) w c1 c2)
		    (compile-squished-defaultless-type-switch
		     (members-that m w) w c1 c2 (break? r))))
	    (if (can-be-non? m w) (c3 #t) (fuck-up))))
       ;; needs work: There is no COMPILE-TYPE-IF and
       ;;             COMPILE-DEFAULTLESS-TYPE-IF.
       (else (if (can-be? m w)
		 (if (can-be-non? m w)
		     (compile-type-switch
		      (members-that m w) w c1 c2 c3 (break? r))
		     (if (null? (rest (members-that m w)))
			 (c2 (first (members-that m w)))
			 (compile-defaultless-type-switch
			  (members-that m w) w c1 c2 (break? r))))
		 (if (can-be-non? m w) (c3 #t) (fuck-up))))))

(define (return-true r) (widen r 'void1 true-type?))

(define (return-false r) (widen r 'void2 false-type?))

(define (compile-test r c) (c:if c (return-true r) (return-false r) #t))

(define (compile-time-test r p?) (if p? (return-true r) (return-false r)))

(define (compile-predicate m r w c)
 (nonerror-type-switch
  m w r c (lambda (u) (return-true r)) (lambda (p?) (return-false r))))

(define (structure-ref-accessor c u i)
 (unless (and (integer? i)
	      (exact? i)
	      (>= i 0)
	      (< i (length (structure-type-slots u))))
  (fuck-up))
 (cond ((fictitious? (list-ref (structure-type-slots u) i)) 'void3)
       ((structure-type-immediate? u) (c:. c (c:s i)))
       (else (c:-> c (c:s i)))))

(define (car-accessor c u)
 (unless (pair-type? u) (fuck-up))
 (structure-ref-accessor c u 0))

(define (cdr-accessor c u)
 (unless (pair-type? u) (fuck-up))
 (structure-ref-accessor c u 1))

(define (string-length-accessor c) (c:strlen c))

(define (string-ref-accessor c1 c2) (c:subscript c1 c2))

(define (vector-length-accessor c u)
 ;; needs work: To use code-generation abstractions.
 (if (degenerate-vector-type? u)
     c
     (cond ((headed-vector-type? u) (c:-> c "length"))
	   ((nonheaded-vector-type? u) (c:. c "length"))
	   ((displaced-vector-type? u) (c:. c "length"))
	   (else (fuck-up)))))

(define (vector-elements-accessor c u)
 ;; needs work: To use code-generation abstractions.
 (if (degenerate-vector-type? u)
     'void4
     (cond ((headed-vector-type? u) (c:-> c "element"))
	   ((nonheaded-vector-type? u) (c:. c "element"))
	   ((displaced-vector-type? u) (c:. c "element"))
	   (else (fuck-up)))))

(define (vector-ref-accessor c1 u1 c2)
 ;; needs work: To use code-generation abstractions.
 (if (degenerate-vector-type? u1)
     'void5
     (cond ((headed-vector-type? u1) (c:subscript (c:-> c1 "element") c2))
	   ((nonheaded-vector-type? u1) (c:subscript (c:. c1 "element") c2))
	   ((displaced-vector-type? u1) (c:subscript (c:. c1 "element") c2))
	   (else (fuck-up)))))

(define (value-structure-ref c u w i)
 (if (fictitious? u) 'void6 (structure-ref-accessor (c:value c u w) u i)))

(define (value-car c u w)
 (if (fictitious? u) 'void7 (car-accessor (c:value c u w) u)))

(define (value-cdr c u w)
 (if (fictitious? u) 'void8 (cdr-accessor (c:value c u w) u)))

(define (value-string-length c u w) (string-length-accessor (c:value c u w)))

(define (value-string-ref c1 u1 w1 c2)
 (string-ref-accessor (c:value c1 u1 w1) c2))

(define (value-vector-length c u w) (vector-length-accessor (c:value c u w) u))

(define (value-vector-elements c u w)
 (vector-elements-accessor (c:value c u w) u))

(define (value-vector-ref c1 u1 w1 c2)
 (vector-ref-accessor (c:value c1 u1 w1) u1 c2))

(define (accessor g e)
 (cond ((local? g) (c:a g))
       ((global? g) (c:a g))
       ((hidden? g)
	(case *closure-representation*
	 ((immediate-flat indirect-flat immediate-display indirect-display)
	  (fuck-up))
	 ((linked)
	  (let ((e1 (parent-parameter (the-member (variable-type-set g)))))
	   (if (eq? e e1)
	       (c:e e)
	       (let loop ((e (parent-parameter e)) (c (c:p e)))
		(if (eq? e e1)
		    c
		    (loop (parent-slot e) (c:-> c (c:p (parent-slot e)))))))))
	 (else (fuck-up))))
       ((slotted? g)
	(case *closure-representation*
	 ((immediate-flat)
	  (star-before (if (eq? (variable-environment g) e)
			   (c:. (c:e e) (c:a g))
			   (c:. (c:p e) (c:a g)))))
	 ((indirect-flat)
	  (star-before (if (eq? (variable-environment g) e)
			   (c:-> (c:e e) (c:a g))
			   (c:-> (c:p e) (c:a g)))))
	 ((immediate-display)
	  (if (eq? (variable-environment g) e)
	      (c:-> (c:e e) (c:a g))
	      (c:-> (c:. (c:p e) (c:e (variable-environment g))) (c:a g))))
	 ((indirect-display)
	  (if (eq? (variable-environment g) e)
	      (c:-> (c:e e) (c:a g))
	      (c:-> (c:-> (c:p e) (c:e (variable-environment g))) (c:a g))))
	 ((linked)
	  (let ((e1 (variable-environment g)))
	   (if (eq? e e1)
	       (c:-> (c:e e) (c:a g))
	       (let loop ((e (parent-parameter e)) (c (c:p e)))
		(if (eq? e e1)
		    (c:-> c (c:a g))
		    (loop (parent-slot e) (c:-> c (c:p (parent-slot e)))))))))
	 (else (fuck-up))))
       (else (format #t "Warning! Variable ~a{~a}:W~s is fake~%"
		     (variable-name g)
		     (variable-index g)
		     (type-set-index (variable-type-set g)))
	     "fake")))

(define (number-of-accessor-indirections x)
 (let ((g (expression-variable x))
       (e (expression-environment x)))
  (cond ((not (accessed? g)) 0)
	((fictitious? (variable-type-set g)) 0)
	((local? g) 0)
	((global? g) 0)
	((hidden? g)
	 (case *closure-representation*
	  ((immediate-flat indirect-flat immediate-display indirect-display)
	   (fuck-up))
	  ((linked)
	   (let ((e1 (parent-parameter (the-member (variable-type-set g)))))
	    (if (eq? e e1)
		0
		(let loop ((e (parent-parameter e)) (c 0))
		 (if (eq? e e1) c (loop (parent-slot e) (+ c 1)))))))
	  (else (fuck-up))))
	((slotted? g)
	 (case *closure-representation*
	  ((immediate-flat) 0)
	  ((indirect-flat) 1)
	  ((immediate-display) 1)
	  ((indirect-display) 1)
	  ((linked)
	   (let ((e1 (variable-environment g)))
	    (if (eq? e e1)
		1
		(let loop ((e (parent-parameter e)) (c 0))
		 (if (eq? e e1) (+ c 1) (loop (parent-slot e) (+ c 1)))))))
	  (else (fuck-up))))
	(else (fuck-up)))))

(define (lambda-accessor u e)
 (case *closure-representation*
  ((immediate-flat indirect-flat immediate-display indirect-display) (fuck-up))
  ((linked)
   (if (and (or (eq? *closure-conversion-method* 'baseline)
		(eq? *closure-conversion-method* 'conventional))
	    (or (not (environment? (native-procedure-type-narrow-prototype u)))
		(not (environment-used? (narrow-prototype u)))
		(not (environment? (parent-parameter u)))
		(not (environment-used? (parent-parameter u)))))
       "fake"
       (let ((e1 (parent-parameter u)))
	(if (eq? e e1)
	    (c:e e)
	    (let loop ((e (parent-parameter e)) (c (c:p e)))
	     (if (eq? e e1)
		 c
		 (loop (parent-slot e) (c:-> c (c:p (parent-slot e))))))))))
  (else (fuck-up))))

(define (parent-accessor e)
 (case *closure-representation*
  ((immediate-flat indirect-flat immediate-display indirect-display) (fuck-up))
  ((linked)
   (let ((e1 (parent-slot e)))
    (let loop ((e (parent-parameter e)) (c (c:p e)))
     (if (eq? e e1) c (loop (parent-slot e) (c:-> c (c:p (parent-slot e))))))))
  (else (fuck-up))))

;;; Representation Promotion

(define (promote! r w w2)
 (unless (or (discard? r) (antecedent? r))
  (let* ((w1 (result-type-set r))
	 (us (intersectionq (members w) (members w2)))
	 ;; note: This was added because of uniqueness. With uniqueness, it is
	 ;;       possible for the source to be widened and contain members
	 ;;       that the more precise analysis determines can't really occur
	 ;;       so that they may be absent from the destination.
	 (us
	  (if *uniqueness?* (intersectionq (members w1) us) us)))
   (unless (null? us)
    (cond
     ((and (general? w) (squeezed? w1))
      (let ((u1 (squeezed-member w1)))
       (when (and (some (lambda (u) (eq? u u1)) us)
		  (not (every (lambda (u) (eq? u u1)) us)))
	(when #t			;debugging
	 (notify "Promoting W~s from squeezed to avoid" (type-set-index w1))
	 (notify "   MOVE: branching general (W~s) to squeezed (W~s)"
		 (type-set-index w) (type-set-index w1)))
	(set-type-set-squeezable?! w1 #f)
	(determine-alignments!)
	(assign-global-squish-tags!)
	(set! *again?* #t))))
     ((and (squeezed? w) (squished? w1))
      (when (and
	     (not (every (lambda (u) (or (char-type? u) (fictitious? u))) us))
	     (not (zero? (squish-tag (squeezed-member w) w1)))
	     (some (lambda (u) (or (char-type? u) (fictitious? u))) us))
       (when #t				;debugging
	(notify "Promoting W~s from squeezed to avoid" (type-set-index w))
	(notify "   MOVE: branching squeezed (W~s) to squished (W~s)"
		(type-set-index w) (type-set-index w1)))
       ;; needs work: Could try to promote W1 to general. Could also try to
       ;;             reassign squish tags of W1 so that the squeezed member of
       ;;             W has squish tag zero.
       (set-type-set-squeezable?! w #f)
       (determine-alignments!)
       (assign-global-squish-tags!)
       (set! *again?* #t)))
     ((and (squished? w) (squished? w1))
      (when (and
	     (or (some (lambda (u)
			(and (not (char-type? u))
			     (not (fictitious? u))
			     (not (= (squish-tag u w) (squish-tag u w1)))))
		       us)
		 (not (= (squish-alignment w) (squish-alignment w1))))
	     (some (lambda (u) (or (char-type? u) (fictitious? u))) us)
	     (or (not (= (squish-alignment w) (squish-alignment w1)))
		 (not (every (lambda (u)
			      (or (not (zero? (squish-tag u w)))
				  (zero? (squish-tag u w1))))
			     us))))
       (cond
	((and
	  (every (lambda (u)
		  (or (char-type? u)
		      (fictitious? u)
		      (= (squish-tag u w) (squish-tag u w1))))
		 us)
	  (every (lambda (u)
		  (or (not (zero? (squish-tag u w)))
		      (zero? (squish-tag u w1))))
		 us)
	  (if (> (squish-alignment w) (squish-alignment w1))
	      (no-pointer-members? w1)
	      (no-pointer-members? w)))
	 (cond
	  ((> (squish-alignment w) (squish-alignment w1))
	   (when #t			;debugging
	    (notify "Increasing alignment of W~s from ~a to ~a to avoid"
		    (type-set-index w1)
		    (squish-alignment w1)
		    (squish-alignment w))
	    (notify "   MOVE: branching squished (W~s) to squished (W~s)"
		    (type-set-index w) (type-set-index w1)))
	   (set-type-set-minimal-alignment! w1 (squish-alignment w))
	   (determine-alignments!)
	   (assign-global-squish-tags!)
	   (set! *again?* #t))
	  (else
	   (when #t			;debugging
	    (notify "Increasing alignment of W~s from ~a to ~a to avoid"
		    (type-set-index w)
		    (squish-alignment w)
		    (squish-alignment w1))
	    (notify "   MOVE: branching squished (W~s) to squished (W~s)"
		    (type-set-index w) (type-set-index w1)))
	   (set-type-set-minimal-alignment! w (squish-alignment w1))
	   (determine-alignments!)
	   (assign-global-squish-tags!)
	   (set! *again?* #t))))
	(else
	 (when #t			;debugging
	  (notify "Promoting W~s from squished to avoid" (type-set-index w1))
	  (notify "   MOVE: branching squished (W~s) to squished (W~s)"
		  (type-set-index w) (type-set-index w1)))
	 ;; needs work: Could try to promote W to general.
	 (set-type-set-squishable?! w1 #f)
	 (determine-alignments!)
	 (assign-global-squish-tags!)
	 (set! *again?* #t)))))
     ((and (general? w) (squished? w1))
      (unless (or (every (lambda (u) (or (char-type? u) (fictitious? u))) us)
		  (null? (rest us)))
       (when #t				;debugging
	(notify "Promoting W~s from squished to avoid" (type-set-index w1))
	(notify "   MOVE: dispatching general (W~s) to squished (W~s)"
		(type-set-index w) (type-set-index w1)))
       (set-type-set-squishable?! w1 #f)
       (determine-alignments!)
       (assign-global-squish-tags!)
       (set! *again?* #t)))
     ((and (squeezed? w) (general? w1))
      (when (and
	     (not (every (lambda (u) (or (char-type? u) (fictitious? u))) us))
	     (some (lambda (u) (or (char-type? u) (fictitious? u))) us))
       (when #t				;debugging
	(notify "Promoting W~s from squeezed to avoid" (type-set-index w))
	(notify "   MOVE: branching squeezed (W~s) to general (W~s)"
		(type-set-index w) (type-set-index w1)))
       (set-type-set-squeezable?! w #f)
       (determine-alignments!)
       (assign-global-squish-tags!)
       (set! *again?* #t)))
     ((and (squished? w) (general? w1))
      (unless (or (every (lambda (u) (or (char-type? u) (fictitious? u))) us)
		  (null? (rest us)))
       (when #t				;debugging
	(notify "Promoting W~s from squished to avoid" (type-set-index w))
	(notify "   MOVE: dispatching squished (W~s) to general (W~s)"
		(type-set-index w) (type-set-index w1)))
       (set-type-set-squishable?! w #f)
       (determine-alignments!)
       (assign-global-squish-tags!)
       (set! *again?* #t)))
     ((and (general? w) (general? w1))
      (unless (or (eq? w w1)
		  (and *forgery?* (= (type-set-size w) (type-set-size w1)))
		  (every (lambda (u) (or (char-type? u) (fictitious? u))) us)
		  (null? (rest us)))
       (when #t				;debugging
	(notify "MOVE: dispatching general to general")
	(notify "   because of mismatched type set sizes ~a vs. ~a"
		(type-set-size w) (type-set-size w1))))))))))

(define (promote-pair+! r y ws w)
 (cond
  ((discard? r) #f)
  ((antecedent? r) #f)
  ((null? ws) (promote! r w w))
  (else
   (let loop ((uss (map members ws))
	      (w1 (result-type-set r))
	      (us1 '())
	      (ws1 '()))
    (if (null? uss)
	(let loop ((ws (reverse ws))
		   (us1 us1)
		   (ws1 ws1)
		   (w w))
	 (unless (null? ws)
	  (let ((w2 (first ws))
		(u1 (first us1))
		(w1 (first ws1)))
	   (unless (or (fictitious? w1)
		       (and (return? r) (not (result-accessed? r))))
	    (cond
	     ((fictitious? u1)
	      (let ((w (create-anonymous-type-set u1)))
	       (set-type-set-fictitious?!
		w
		(case *closure-conversion-method*
		 ((baseline conventional) #f)
		 ((lightweight)
		  (or (void? w)
		      (and (not (multimorphic? w)) (must-be? fictitious? w))))
		 (else (fuck-up))))
	       (promote! (create-accessor-result w1 #f) w w)))
	     (else (promote! (create-accessor-result (pair-type-car u1) #f)
			     w2
			     w2)
		   (promote! (create-accessor-result (pair-type-cdr u1) #f)
			     w
			     w))))
	   (loop (rest ws) (rest us1) (rest ws1) w1))))
	(let ((u1 (the-member-that
		   (pair+-type? uss (members w) (call-site-expression y)) w1)))
	 (loop (rest uss) (pair-type-cdr u1) (cons u1 us1) (cons w1 ws1))))))))

(define (promote-gather! e0 y ws w gs)
 (let loop ((ws ws) (w w) (gs gs))
  (unless (null? gs)
   (let* ((g (first gs)))
    (if (null? ws)
	(if (and (rest? e0) (null? (rest gs)))
	    (when (or (local? g) (global? g) (slotted? g))
	     (promote! (create-accessor-result (variable-type-set g) #f) w w))
	    (for-each (lambda (u)
		       (when (or (local? g) (global? g) (slotted? g))
			(promote!
			 (create-accessor-result (variable-type-set g) #f)
			 (pair-type-car u)
			 (pair-type-car u)))
		       (loop ws (pair-type-cdr u) (rest gs)))
		      (members-that pair-type? w)))
	(cond ((and (rest? e0) (null? (rest gs)))
	       (when (or (local? g) (global? g) (slotted? g))
		(promote-pair+!
		 (create-accessor-result (variable-type-set g) #f) y ws w)))
	      (else (when (or (local? g) (global? g) (slotted? g))
		     (promote!
		      (create-accessor-result (variable-type-set g) #f)
		      (first ws)
		      (first ws)))
		    (loop (rest ws) w (rest gs)))))))))

(define (promote-converted-call! r y u0 ws w)
 (unless (procedure-type? u0) (fuck-up))
 (when (continuation-type? u0) (fuck-up))
 (cond
  ;; CALL/CC==(LAMBDA (C X) (C (X C)))
  (((primitive-procedure-type-named? 'call-with-current-continuation) u0)
   (when (can-be-non? null-type? w) (fuck-up))
   (for-each
    (lambda (u2)
     (promote-converted-call! r (recreate-call-site y 'first-argument)
			      u2 (list (first ws) (first ws)) *null*))
    (members-that
     (compatible-procedure? (list (first ws) (first ws))
			    *null*
			    (recreate-call-site y 'first-argument))
     (second ws))))
  ((and (native-procedure-type? u0) (converted? (callee-environment u0 y)))
   (promote-call! r y u0 ws w))
  (((needs-implicit-continuation-call? ws w y) u0)
   (let ((w1 (minp subtype-set?
		   (map (lambda (u) (continuation-argument-type-set u y))
			(members (first ws))))))
    (when (can-be-non?
	   (lambda (u) (subtype-set? w1 (continuation-argument-type-set u y)))
	   (first ws))
     (fuck-up))
    (promote-call!
     (if (fictitious? w1) *discard* (create-accessor-result w1 #f))
     y u0 (rest ws) w)
    ;; This relies on the fact that the implicit continuation call is never
    ;; done through APPLY.
    (for-each
     (lambda (u1)
      (promote-call!
       r (recreate-call-site y 'continuation-argument) u1 (list w1) *null*))
     (members-that
      (compatible-procedure?
       (list w1) *null* (recreate-call-site y 'continuation-argument))
      (first ws)))))
  (else (promote-call! r y u0 (rest ws) w))))

(define (promote-call! r y u0 ws w)
 (cond
  ((primitive-procedure-type? u0)
   (when (can-be-non? null-type? w) (fuck-up))
   (unless (some void? ws)
    ((primitive-procedure-promote!
      (cdr (assq (primitive-procedure-type-name u0)
		 *primitive-procedure-handlers*)))
     r y u0 ws w
     (if (converted? y)
	 (expression-type-set (continuation-argument (call-site-expression y)))
	 #f)
     (if (>= (length ws) 1) (first ws) #f)
     (if (>= (length ws) 2) (second ws) #f)
     (if (>= (length ws) 3) (third ws) #f))))
  ((native-procedure-type? u0)
   (let* ((e (expression-environment (call-site-expression y)))
	  (e0 (callee-environment u0 y))
	  (x0 (environment-expression e0))
	  (gs (variables e0)))
    (unless (called? e0) (fuck-up))
    (cond ((noop? e0) #f)
	  ((can-be-self-tail-call-to? y e0) (promote-gather! e0 y ws w gs))
	  ((unique-call-site? e0)
	   (promote-gather! e0 y ws w gs)
	   (promote-expression! r (expression-body x0)))
	  (else (promote-gather! e0 y ws w gs)
		(promote! r (return-type-set e0) (return-type-set e0))))))
  ((foreign-procedure-type? u0) #f)
  ((continuation-type? u0)
   (when (can-be-non? null-type? w)
    (unimplemented y "APPLY of a continuation is not (yet) implemented"))
   (unless (continuation-type-continuation-accessed? u0) (fuck-up))
   (if (goto? y u0)
       (promote!
	(expression-result (continuation-type-allocating-expression u0))
	(first ws)
	(first ws))
       (promote! (if (fictitious?
		      (expression-type-set
		       (continuation-type-allocating-expression u0)))
		     *discard*
		     (create-accessor-result
		      (expression-type-set
		       (continuation-type-allocating-expression u0))
		      #f))
		 (first ws)
		 (first ws))))
  (else (fuck-up))))

(define (promote-antecedent! x)
 (promote-expression!
  (create-antecedent-result (expression-type-set x) #f #f #f) x))

(define (promote-expression! r x)
 (set-expression-result! x r)
 (if (and (not (antecedent? r))
	  (must-be? boolean-type? (expression-type-set x))
	  (can-be-non? true-type? (expression-type-set x))
	  (can-be-non? false-type? (expression-type-set x))
	  (or (and-expression? x) (or-expression? x) (not-expression? x)))
     (promote-antecedent! x)
     (case (expression-kind x)
      ((null-constant) #f)
      ((true-constant) #f)
      ((false-constant) #f)
      ((char-constant) #f)
      ((fixnum-constant) #f)
      ((flonum-constant) #f)
      ((rectangular-constant) #f)
      ((string-constant) #f)
      ((symbol-constant) #f)
      ((pair-constant) #f)
      ((vector-constant) #f)
      ((lambda converted-lambda converted-continuation) #f)
      ((set!)
       (promote-expression!
	(if (and (or (local? (expression-variable x))
		     (global? (expression-variable x))
		     (slotted? (expression-variable x)))
		 (nontrivial-reference? x)
		 (executed? x))
	    (create-accessor-result
	     (variable-type-set (expression-variable x)) #f)
	    *discard*)
	(expression-source x)))
      ((if)
       (promote-antecedent! (expression-antecedent x))
       (cond ((and (antecedent? r) (and-expression? x))
	      (when (reached? (expression-consequent x))
	       (promote-antecedent! (expression-consequent x))))
	     (else (when (reached? (expression-consequent x))
		    (promote-expression! r (expression-consequent x)))
		   (when (reached? (expression-alternate x))
		    (promote-expression! r (expression-alternate x))))))
      ((primitive-procedure) #f)
      ((foreign-procedure) #f)
      ((access)
       (when (and (accessed? (expression-variable x))
		  (or (not (hidden? (expression-variable x)))
		      (discard? r)
		      (antecedent? r)))
	(promote! r
		  (variable-type-set (expression-variable x))
		  (expression-type-set x))))
      ((call converted-call)
       (cond
	((and (antecedent? r) (or-expression? x))
	 (let* ((u (the-member (expression-type-set (expression-callee x))))
		(e0 (callee-environment u (create-call-site x)))
		(x0 (environment-expression e0))
		(x1 (expression-body x0))
		(x2 (first (expression-arguments x))))
	  (promote-antecedent! x2)
	  (when (reached? (expression-alternate x1))
	   (promote-antecedent! (expression-alternate x1)))))
	((and (antecedent? r) (not-expression? x))
	 (promote-antecedent! (first (expression-arguments x))))
	(else
	 (let* ((w0 (expression-type-set (expression-callee x)))
		(ws (map expression-type-set (expression-arguments x))))
	  (promote-expression!
	   (create-accessor-result w0 #f) (expression-callee x))
	  (for-each
	   (lambda (w x) (promote-expression! (create-accessor-result w #f) x))
	   ws (expression-arguments x))
	  (when (and (executed? x) (not (void? w0)))
	   (for-each
	    (lambda (u0)
	     (if (converted? x)
		 (promote-converted-call! r (create-call-site x) u0 ws *null*)
		 (promote-call! r (create-call-site x) u0 ws *null*)))
	    (members-that (compatible-call? x) w0)))))))
      (else (fuck-up)))))

(define (promote-representations!)
 (let loop ()
  (set! *again?* #f)
  (for-each (lambda (e)
	     (unless (unique-call-site? e)
	      (promote-expression!
	       (create-return-result
		e
		(expression-type-set
		 (expression-body (environment-expression e))))
	       (expression-body (environment-expression e)))))
	    *es*)
  (when *again?* (loop))))

;;; Copy Propagation

(define (maybe-parentheses-around c)
 ;; needs work: To replace the calls to LIST? and LENGTH in the following with
 ;;             more efficient code:
 (if (or (and (string? c)
	      (or (string=? c "argc")
		  (string=? c "argv")
		  (string=? c "'\\0'")
		  (string=? c "NULL")
		  (string=? c "EOF")
		  (string=? c "data")
		  (string=? c "region")
		  (string=? c "region_size")
		  (string=? c "stdin")
		  (string=? c "stdout")
		  (string=? c "CLOCKS_PER_SEC")
		  (string=? c "RAND_MAX")
		  (string=? c "VALUE_OFFSET")
		  (string=? c "CHAR_OFFSET")
		  (string->number c)))
	 (and (list? c)
	      (= (length c) 3)
	      (string? (first c))
	      (string=? (first c) "'")
	      (string? (second c))
	      (string? (third c))
	      (string=? (third c) "'"))
	 (and (list? c)
	      (= (length c) 3)
	      (string? (first c))
	      (string=? (first c) "\"")
	      (string? (third c))
	      (string=? (third c) "\""))
	 (and (list? c)
	      (= (length c) 2)
	      (string? (first c))
	      (or (string=? (first c) "a")
		  (string=? (first c) "e")
		  (string=? (first c) "j")
		  (string=? (first c) "p")
		  (string=? (first c) "q")
		  (string=? (first c) "r")
		  (string=? (first c) "t")
		  (string=? (first c) "v")
		  (string=? (first c) "fp")
		  (string=? (first c) "sfp")
		  (string=? (first c) "region")
		  (string=? (first c) "region_size")
		  (string=? (first c) "initial_region")
		  (string=? (first c) "REGION_SIZE"))
	      (string? (second c))))
     c
     (parentheses-around c)))

(define (c:copy-propagate! c)
 (let ((cs1 '())
       (cs2 '()))
  (let loop ((c c))
   (when (c:assignment-to-temporary? c) (set! cs1 (cons c cs1)))
   (when (and (pair? c) (not (c:declaration? c)))
    (loop (car c))
    (loop (cdr c))))
  (let loop ((c c))
   (when (c:unprotected-assignment-to-atomic-temporary? c)
    (set! cs2 (cons c cs2)))
   (when (and (pair? c) (not (c:declaration? c)))
    (loop (car c))
    (loop (cdr c))))
  (let ((cs2 (remove-if-not
	      (lambda (c2)
	       (one (lambda (c1)
		     (equal? (c:atomic-t (first (second (first c1))))
			     (first (second (first c2)))))
		    cs1))
	      cs2)))
   (let loop ((c c))
    (when (and (pair? c) (not (c:declaration? c)))
     (cond
      ((and (c:assignment-to-temporary? (car c)) (memq (car c) cs2))
       (set-car! c '()))
      ((c:atomic-t? (car c))
       (let ((c2 (find-if
		  (lambda (c2) (equal? (first (second (first c2))) (car c)))
		  cs2)))
	(when c2
	 (set-car! c (maybe-parentheses-around
		      (third (third (second (first c2))))))))))
     (cond
      ((and (c:assignment-to-temporary? (cdr c)) (memq (cdr c) cs2))
       (set-cdr! c '()))
      ((c:atomic-t? (cdr c))
       (let ((c2 (find-if
		  (lambda (c2) (equal? (first (second (first c2))) (cdr c)))
		  cs2)))
	(when c2
	 (set-cdr! c (maybe-parentheses-around
		      (third (third (second (first c2))))))))))
     (loop (car c))
     (loop (cdr c)))))))

;;; Removing Unused Declarations

(define (c:remove-unused-declarations! c)
 (let ((a-trie (create-trie
		10
		;; conventions: CHAR
		(lambda (char) (- (char->integer char) (char->integer #\0)))
		;; conventions: K
		(lambda (k) (integer->char (+ k (char->integer #\0))))
		#f))
       (r-trie (create-trie
		10
		;; conventions: CHAR
		(lambda (char) (- (char->integer char) (char->integer #\0)))
		;; conventions: K
		(lambda (k) (integer->char (+ k (char->integer #\0))))
		#f))
       (t-trie (create-trie
		10
		;; conventions: CHAR
		(lambda (char) (- (char->integer char) (char->integer #\0)))
		;; conventions: K
		(lambda (k) (integer->char (+ k (char->integer #\0))))
		#f))
       (v-trie (create-trie
		10
		;; conventions: CHAR
		(lambda (char) (- (char->integer char) (char->integer #\0)))
		;; conventions: K
		(lambda (k) (integer->char (+ k (char->integer #\0))))
		#f)))
  ;; conventions: A-TRIE, R-TRIE, T-TRIE, V-TRIE
  (let loop ((c c))
   (when (c:declaration? c)
    (when (and (list? (second c))
	       (= (length (second c)) 2)
	       (string? (first (second c)))
	       (string? (second (second c))))
     (cond ((string=? (first (second c)) "a")
	    (when (trie-ref a-trie (second (second c))) (fuck-up))
	    (trie-set! a-trie (second (second c)) c))
	   ((string=? (first (second c)) "r")
	    (when (trie-ref r-trie (second (second c))) (fuck-up))
	    (trie-set! r-trie (second (second c)) c))
	   ((string=? (first (second c)) "t")
	    (when (trie-ref t-trie (second (second c))) (fuck-up))
	    (trie-set! t-trie (second (second c)) c))
	   ((string=? (first (second c)) "v")
	    (when (trie-ref v-trie (second (second c))) (fuck-up))
	    (trie-set! v-trie (second (second c)) c)))))
   (when (and (pair? c) (not (c:declaration? c)))
    (loop (car c))
    (loop (cdr c))))
  (let loop ((c c))
   ;; needs work: To replace the calls to LIST? and LENGTH in the following
   ;;             with more efficient code:
   (when (and (list? c)
	      (= (length c) 2)
	      (string? (first c))
	      (or (string=? (first c) "a")
		  (string=? (first c) "r")
		  (string=? (first c) "t")
		  (string=? (first c) "v"))
	      (string? (second c))
	      (every char-numeric? (string->list (second c))))
    (let ((c (cond ((string=? (first c) "a") (trie-ref a-trie (second c)))
		   ((string=? (first c) "r") (trie-ref r-trie (second c)))
		   ((string=? (first c) "t") (trie-ref t-trie (second c)))
		   ((string=? (first c) "v")  (trie-ref v-trie (second c)))
		   (else (fuck-up)))))
     (when c (set-car! (cdddr c) #t))))
   (when (and (pair? c) (not (c:declaration? c)))
    (loop (car c))
    (loop (cdr c)))))
 (let loop ((c c))
  (when (and (pair? c) (not (c:declaration? c)))
   (when (and (c:declaration? (car c)) (not (fourth (car c))))
    (set-car! c '()))
   (when (and (c:declaration? (cdr c)) (not (fourth (cdr c))))
    (set-cdr! c '()))
   (loop (car c))
   (loop (cdr c)))))

;;; Removing Unused Labels

(define (c:remove-unused-labels! c)
 (let ((cs '()))
  (let loop ((c c))
   ;; needs work: To replace the calls to LIST? and LENGTH in the following
   ;;             with more efficient code:
   (when (and (list? c)
	      (= (length c) 2)
	      (list? (first c))
	      (= (length (first c)) 3)
	      (string? (first (first c)))
	      (string=? (first (first c)) "goto")
	      (string? (second (first c)))
	      (string=? (second (first c)) " ")

	      (list? (third (first c)))
	      (= (length (third (first c))) 2)
	      (string? (first (third (first c))))
	      (or (string=? (first (third (first c))) "h")
		  (string=? (first (third (first c))) "l")
		  (string=? (first (third (first c))) "x"))
	      (string? (second (third (first c))))
	      (not (member (third (first c)) cs)))
    (set! cs (cons (third (first c)) cs)))
   (when (and (pair? c) (not (c:declaration? c)))
    (loop (car c))
    (loop (cdr c))))
  (let loop ((c c))
   (when (and (pair? c) (not (c:declaration? c)))
    (when (and (c:label? (car c)) (not (member (first (car c)) cs)))
     (set-car! c '()))
    (when (and (c:label? (cdr c)) (not (member (first (cdr c)) cs)))
     (set-cdr! c '()))
    (loop (car c))
    (loop (cdr c))))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
