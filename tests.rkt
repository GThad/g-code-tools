#lang racket/base

(require rackunit
         racket/function
         racket/contract
         "g-code-tools.rkt")

(provide (all-defined-out))

;; -------------------- TESTS

(define-test-suite
  test:g-code-sym?
  (test-case
   "Produce #t on valid symbol?"
   (check-true (g-code-sym? 'G))
   (check-true (g-code-sym? 'M))
   (check-true (g-code-sym? 'F))
   (check-true (g-code-sym? 'S))
   (check-true (g-code-sym? 'R))
   (check-true (g-code-sym? 'P))
   (check-true (g-code-sym? 'X))
   (check-true (g-code-sym? 'Y))
   (check-true (g-code-sym? 'Z))
   (check-true (g-code-sym? 'I))
   (check-true (g-code-sym? 'J))
   (check-true (g-code-sym? 'K)))
  
  (test-case
   "Produce #t on non-symbol?"
   (check-false (g-code-sym? "G"))
   (check-false (g-code-sym? 20)))
  
  (test-case
   "Produce #f on non-valid symbol?"
   (check-false (g-code-sym? 'hello)))
  
  (test-case
   "Produce #f on lowercase version of valid letter?"
   (check-false (g-code-sym? 'g))))

(define-test-suite
 test:g-code?
 (check-true (g-code? (code 'G 0)))
 (check-false (g-code? (code 'X 0))))

(define-test-suite
 test:m-code?
 (check-true (m-code? (code 'M 0)))
 (check-false (m-code? (code 'X 0))))

(define-test-suite
 test:f-code?
 (check-true (f-code? (code 'F 0)))
 (check-false (f-code? (code 'X 0))))

(define-test-suite
 test:s-code?
 (check-true (s-code? (code 'S 0)))
 (check-false (s-code? (code 'X 0))))

(define-test-suite
 test:r-code?
 (check-true (r-code? (code 'R 0)))
 (check-false (r-code? (code 'X 0))))

(define-test-suite
 test:p-code?
 (check-true (p-code? (code 'P 0)))
 (check-false (p-code? (code 'X 0))))

(define-test-suite
 test:x-code?
 (check-true (x-code? (code 'X 0)))
 (check-false (x-code? (code 'F 0))))

(define-test-suite
 test:y-code?
 (check-true (y-code? (code 'Y 0)))
 (check-false (y-code? (code 'X 0))))

(define-test-suite
 test:z-code?
 (check-true (z-code? (code 'Z 0)))
 (check-false (z-code? (code 'X 0))))

(define-test-suite
 test:i-code?
 (check-true (i-code? (code 'I 0)))
 (check-false (i-code? (code 'X 0))))

(define-test-suite
 test:j-code?
 (check-true (j-code? (code 'J 0)))
 (check-false (j-code? (code 'X 0))))

(define-test-suite
 test:k-code?
 (check-true (k-code? (code 'K 0)))
 (check-false (k-code? (code 'X 0))))