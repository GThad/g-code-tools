#lang racket/base

(require rackunit
         racket/function
         racket/contract
         "g-code-tools.rkt")

;; -------------------- TESTS

(test-suite
 "Test g-code?"
 (check-true (g-code? (code 'G 0)))
 (check-false (g-code? (code 'X 0))))

(test-suite
 "Test m-code?"
 (check-true (m-code? (code 'M 0)))
 (check-false (m-code? (code 'X 0))))

(test-suite
 "Test f-code?"
 (check-true (f-code? (code 'F 0)))
 (check-false (f-code? (code 'X 0))))

(test-suite
 "Test s-code?"
 (check-true (s-code? (code 'S 0)))
 (check-false (s-code? (code 'X 0))))

(test-suite
 "Test r-code?"
 (check-true (r-code? (code 'R 0)))
 (check-false (r-code? (code 'X 0))))

(test-suite
 "Test p-code?"
 (check-true (p-code? (code 'P 0)))
 (check-false (p-code? (code 'X 0))))

(test-suite
 "Test x-code?"
 (check-true (x-code? (code 'X 0)))
 (check-false (x-code? (code 'F 0))))

(test-suite
 "Test y-code?"
 (check-true (y-code? (code 'Y 0)))
 (check-false (y-code? (code 'X 0))))

(test-suite
 "Test z-code?"
 (check-true (z-code? (code 'Z 0)))
 (check-false (z-code? (code 'X 0))))

(test-suite
 "Test i-code?"
 (check-true (i-code? (code 'I 0)))
 (check-false (i-code? (code 'X 0))))

(test-suite
 "Test j-code?"
 (check-true (j-code? (code 'J 0)))
 (check-false (j-code? (code 'X 0))))

(test-suite
 "Test k-code?"
 (check-true (k-code? (code 'K 0)))
 (check-false (k-code? (code 'X 0))))