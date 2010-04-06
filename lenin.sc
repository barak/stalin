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

(include "QobiScheme")

;;; GENSYM

(define (gensym string) (string->uninterned-symbol (string-copy string)))

(define (no-cursor) #f)

(define (no-version) #f)

(define (notify format-string . args)
 ;; conventions: FORMAT-STRING ARGS
 (let ((string (apply format #f format-string args)))
  (display string)
  (newline)))

(define (split-into-lines s)
 ;; conventions: S
 (let loop ((characters (string->list s)) (lines '("")))
  ;; conventions: CHARACTERS LINES
  (cond ((null? characters) (reverse lines))
	((char=? (first characters) #\newline)
	 (loop (rest characters) (cons "" lines)))
	(else (loop (rest characters)
		    (cons (string-append (first lines)
					 (string (first characters)))
			  (rest lines)))))))

(define (notify-pp format-string . args)
 ;; conventions: FORMAT-STRING ARGS
 (let ((pretty? (write-pretty)))
  ;; conventions: PRETTY?
  (set-write-pretty! #t)
  (apply format #t format-string args)
  (set-write-pretty! pretty?))
 (newline))

(define (notify-pp3 format-string . args)
 ;; conventions: FORMAT-STRING ARGS
 (let ((level (write-level))
       (pretty? (write-pretty)))
  ;; conventions: LEVEL PRETTY?
  (set-write-level! 3)
  (set-write-pretty! #t)
  (apply format #t format-string args)
  (set-write-level! level)
  (set-write-pretty! pretty?))
 (newline))

(define (terminate) (exit -1))

;;; Tam V'Nishlam Shevah L'El Borei Olam
