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
(module stalin5c)

(include "QobiScheme.sch")
(include "stalin5c.sch")
;;; End delete for Trotsky

;;; The C library

(define *c:noreturn?* #f)
(define *c:c?* #f)
(define *c:panic?* #f)
(define *c:backtrace?* #f)
(define *c:backtrace-internal?* #f)
(define *c:ipow?* #f)
(define *c:input-waiting?* #f)

(define *c:includes* #f)

(define (include! include)
 (unless (member include *c:includes*)
  (set! *c:includes* (cons include *c:includes*))))

(define (c-library)
 ;; needs work: To use code-generation abstractions.
 (when *c:panic?* (set! *c:noreturn?* #t))
 (when *c:panic?*
  (include! "stdio")			;fprintf stderr
  (include! "stdlib"))			;exit
 (when *c:backtrace?*
  (include! "stdio"))			;fprintf stderr
 (when *c:backtrace-internal?*
  (include! "stdio"))			;fprintf stderr
 (when *c:input-waiting?*
  (include! "sys/time")			;timeval
  (include! "unistd")			;fd_set FD_ZERO FD_SET select
  (include! "stdlib")			;NULL
  (include! "stdio"))			;FILE feof fileno
 (newline-between
  (newlines-between
   (map (lambda (include) (list "#include <" include ".h>")) *c:includes*))
  (if *c:noreturn?*
      "#ifdef __GNUC__
#define NORETURN __attribute__ ((noreturn))
#else
#define NORETURN
#endif"
      "")
  (if (positive? *allocation-alignment*)
      (list "#define ALIGN(p) if (((" *squished* ")p)%" (c:fixnum (expt 2 *allocation-alignment*)) "!=0) p += " (c:fixnum (expt 2 *allocation-alignment*)) "-(((" *squished* ")p)%" (c:fixnum (expt 2 *allocation-alignment*)) ")")
      "")
  (list "#define IMIN(x,y) (x<y?x:y)
#define IMAX(x,y) (x>y?x:y)
#define RMIN(x,y) (x<y?x:y)
#define RMAX(x,y) (x>y?x:y)
struct rectangular {" *flonum* " r; " *flonum* " i;};")
  (if *c:c?* "int c;" "")
  (if *c:panic?*
      "void stalin_panic(char *message) NORETURN;
void stalin_panic(char *message)
{fprintf(stderr, \"%s\\n\", message); exit(-1);}"
      "")
  (if *c:backtrace?*
      "void backtrace(char *file_name, unsigned int line_number, unsigned int character_number);
void backtrace(char *file_name, unsigned int line_number, unsigned int character_number)
{fprintf(stderr, \"\\n%s:%d:%d:\", file_name, line_number, character_number);}"
      "")
  (if *c:backtrace-internal?*
      "void backtrace_internal(char *name);
void backtrace_internal(char *name) {fprintf(stderr, \"\\nIn %s\\n\", name);}"
      "")
  (if *c:ipow?*
      (list "int ipow(int x, int y);
int ipow(int x, int y)
{int i, r = 1;
 for (i = 0; i<y; i++) r *= x;
 return r;}")
      "")
  (if *c:input-waiting?*
      (list "int input_waiting(" *file* " *f);
int input_waiting(" *file* " *f)
/* Returns 0 if no input, >0 if there is input, -1 if error */
{fd_set rfds;
 struct timeval tv;
 /* check stdio buffer first */
 if (feof(f)) return 1;
 #ifdef __linux__
 if ((f->_IO_read_end)>(f->_IO_read_ptr)) return 1;
 #else
 if ((f->_cnt)>0) return 1;
 #endif
 /* watch fd to see when it has input */
 FD_ZERO(&rfds);
 FD_SET(fileno(f), &rfds);
 /* do not wait */
 tv.tv_sec = 0;
 tv.tv_usec = 0;
 return select(fileno(f)+1, &rfds, NULL, NULL, &tv);}")
      "")
  (if *treadmarks?*
      "void Tmk_distribute_hack(const void *ptr, int size)
{ const char *p = ptr;
  while (size>0)
  { Tmk_distribute((void *)p, (size>65460)?65460:size);
    p += 65460;
    size -= 65460;}}
int Tmk_get_NBARRIERS(void) {return TMK_NBARRIERS;}
int Tmk_get_NPROCS(void) {return TMK_NPROCS;}
int Tmk_get_proc_id(void) {return Tmk_proc_id;}
int Tmk_get_NLOCKS(void) {return TMK_NLOCKS;}"
      "")))

;;; Tam V'Nishlam Shevah L'El Borei Olam
