;;; LaHaShem HaAretz U'Mloah

;;; Stalin 0.10 - A global optimizing compiler for Scheme
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997, 1998, 1999, 2000, and 2001 NEC Research Institute, Inc. All
;;; rights reserved.
;;; Copyright 2002 and 2003 Purdue University. All rights reserved.

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
;;;    NEC Research Institute, Inc.
;;;    4 Independence Way
;;;    Princeton NJ 08540-6620 USA
;;;    voice: 609/951-2705
;;;    FAX:   609/951-2483
;;;    Qobi@research.nj.nec.com
;;;    ftp://ftp.nj.nec.com/pub/qobi
;;;    http://www.neci.nj.nec.com/homepages/qobi

(include "xlib-original")

(define (unsigned-list->unsigneda list)
 (panic "UNSIGNED-LIST->UNSIGNEDA is not (yet) implemented"))

(define ylookupstring ((lambda ())))
(let ((xlookupstring-buffer (make-string 50)))
 (set! ylookupstring
       (lambda (event)
	(let ((result (xlookupstring event
				     xlookupstring-buffer
				     50
				     (integer->pointer 0)
				     (integer->pointer 0))))
	 (substring xlookupstring-buffer 0 result)))))

(define (xdestroyimage image) (panic "XDESTROYIMAGE is not (yet) implemented"))

(define (xputpixel ximage x y pixel)
 (panic "XPUTPIXEL is not (yet) implemented"))

(define xa_point_size 59)

(define allocate-int (foreign-procedure () void* "alloc_int"))
(define free-int (foreign-procedure (void*) void "free_int"))
(define get-int (foreign-procedure (void*) int "get_int"))
(define set-int (foreign-procedure (void* int) void "set_int"))

(define (yquerypointer display w)
 (let ((root-return (allocate-int))
       (child-return (allocate-int))
       (root-x-return (allocate-int))
       (root-y-return (allocate-int))
       (win-x-return (allocate-int))
       (win-y-return (allocate-int))
       (mask-return (allocate-int)))
  (xquerypointer display
		 w
		 root-return
		 child-return
		 root-x-return
		 root-y-return
		 win-x-return
		 win-y-return
		 mask-return)
  (let ((x (get-int win-x-return))	
	(y (get-int win-y-return)))
   (free-int root-return)
   (free-int child-return)
   (free-int root-x-return)
   (free-int root-y-return)
   (free-int win-x-return)
   (free-int win-y-return)
   (free-int mask-return)
   (list x y))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
