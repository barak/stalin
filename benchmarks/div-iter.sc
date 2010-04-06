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

(define (iterative-div2 l)
 (do ((l l (cddr l)) (a '() (cons (car l) a))) ((null? l) a)))

(define (test-1 l)
 (do ((i 300 (- i 1))) ((= i 0))
  (iterative-div2 l)
  (iterative-div2 l)
  (iterative-div2 l)
  (iterative-div2 l)))

;;; note: The CREATE-N is not done multiple times.
(do ((i 0 (+ i 1))) ((= i 1000)) (test-1 *ll*))
