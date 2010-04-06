;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:         div.sc
;;; Description:  DIV benchmarks
;;; Author:       Richard Gabriel
;;; Created:      8-Apr-85
;;; Modified:     19-Jul-85 18:28:01 (Bob Shaw)
;;;               23-Jul-87 (Will Clinger)
;;;               11-Apr-94 (Qobi)
;;;               31-Mar-98 (Qobi)
;;; Language:     Scheme
;;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DIV2 -- Benchmark which divides by 2 using lists of n ()'s.
;;; This file contains a recursive as well as an iterative test.

(define (create-n n) (do ((n n (- n 1)) (a '() (cons '() a))) ((= n 0) a)))

(define *ll* (create-n 200))

(define (recursive-div2 l)
 (cond ((null? l) '()) (else (cons (car l) (recursive-div2 (cddr l))))))

(define (test-2 l)
 (do ((i 300 (- i 1))) ((= i 0))
  (recursive-div2 l)
  (recursive-div2 l)
  (recursive-div2 l)
  (recursive-div2 l)))

;;; note: The CREATE-N is not done multiple times.
(do ((i 0 (+ i 1))) ((= i 1000)) (test-2 *ll*))
