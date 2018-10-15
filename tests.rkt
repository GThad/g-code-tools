#lang racket/base

(require rackunit
         racket/function
         racket/contract
         "g-code-tools.rkt")

(provide (all-defined-out))

;; -------------------- TESTS

(define-test-suite
  test:g-code-letter?
 (check-true (g-code-letter? 'G))
 (check-true (g-code-letter? 'M))
 (check-true (g-code-letter? 'F))
 (check-true (g-code-letter? 'S))
 (check-true (g-code-letter? 'R))
 (check-true (g-code-letter? 'P))
 (check-true (g-code-letter? 'X))
 (check-true (g-code-letter? 'Y))
 (check-true (g-code-letter? 'Z))
 (check-true (g-code-letter? 'I))
 (check-true (g-code-letter? 'J))
 (check-true (g-code-letter? 'K))
 (check-false (g-code-letter? 'hello))
 (check-false (g-code-letter? "G"))
 (check-false (g-code-letter? 'g))
 (check-false (g-code-letter? 20)))

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