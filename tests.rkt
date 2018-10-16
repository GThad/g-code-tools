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

(define-test-suite
  test:empty-coord?
  (check-true (empty-coord? (vector)))
  (check-false (empty-coord? (vector (code 'G 2))))
  (check-false (empty-coord? (vector (code 'X 10))))
  (check-false (empty-coord? (vector (code 'X 10) (code 'Y 10)))))

(define-test-suite
  test:x-coord?
  (check-true (x-coord? (vector (code 'X 10))))
  (check-false (x-coord? (vector)))
  (check-false (x-coord? (vector (code 'G 2))))
  (check-false (x-coord? (vector (code 'Y 10))))
  (check-false (x-coord? (vector (code 'X 10) (code 'Y 10)))))

(define-test-suite
  test:y-coord?
  (check-true (y-coord? (vector (code 'Y 10))))
  (check-false (y-coord? (vector)))
  (check-false (y-coord? (vector (code 'G 2))))
  (check-false (y-coord? (vector (code 'X 10))))
  (check-false (y-coord? (vector (code 'X 10) (code 'Y 10)))))

(define-test-suite
  test:z-coord?
  (check-true (z-coord? (vector (code 'Z 10))))
  (check-false (z-coord? (vector)))
  (check-false (z-coord? (vector (code 'G 2))))
  (check-false (z-coord? (vector (code 'Y 10))))
  (check-false (z-coord? (vector (code 'X 10) (code 'Y 10)))))

(define-test-suite
  test:xy-coord?
  (check-true (xy-coord? (vector (code 'X 10) (code 'Y 10))))
  (check-false (xy-coord? (vector)))
  (check-false (xy-coord? (vector (code 'Y 10))))
  (check-false (xy-coord? (vector (code 'G 2) (code 'M 3))))
  (check-false (xy-coord? (vector (code 'Y 10) (code 'X 10))))
  (check-false (xy-coord? (vector (code 'X 10) (code 'Y 10) (code 'Z 10)))))

(define-test-suite
  test:xz-coord?
  (check-true (xz-coord? (vector (code 'X 10) (code 'Z 10))))
  (check-false (xz-coord? (vector)))
  (check-false (xz-coord? (vector (code 'Y 10))))
  (check-false (xz-coord? (vector (code 'G 2) (code 'M 3))))
  (check-false (xz-coord? (vector (code 'Z 10) (code 'X 10))))
  (check-false (xz-coord? (vector (code 'X 10) (code 'Y 10) (code 'Z 10)))))

(define-test-suite
  test:yz-coord?
  (check-true (yz-coord? (vector (code 'Y 10) (code 'Z 10))))
  (check-false (yz-coord? (vector)))
  (check-false (yz-coord? (vector (code 'Y 10))))
  (check-false (yz-coord? (vector (code 'G 2) (code 'M 3))))
  (check-false (yz-coord? (vector (code 'Z 10) (code 'Y 10))))
  (check-false (yz-coord? (vector (code 'X 10) (code 'Y 10) (code 'Z 10)))))

(define-test-suite
  test:xyz-coord?
  (check-true (xyz-coord? (vector (code 'X 10) (code 'Y 10) (code 'Z 10))))
  (check-false (xyz-coord? (vector)))
  (check-false (xyz-coord? (vector (code 'Y 10))))
  (check-false (xyz-coord? (vector (code 'G 2) (code 'M 3) (code 'Z 3))))
  (check-false (xyz-coord? (vector (code 'X 10) (code 'Z 10))))
  (check-false (xyz-coord? (vector (code 'Z 10) (code 'X 10) (code 'Y 10))))
  (check-false (xyz-coord? (vector (code 'X 10) (code 'Y 10) (code 'Z 10) (code 'I 10)))))

(define-test-suite
  test:i-coord?
  (check-true (i-coord? (vector (code 'I 10))))
  (check-false (i-coord? (vector)))
  (check-false (i-coord? (vector (code 'G 2))))
  (check-false (i-coord? (vector (code 'Y 10))))
  (check-false (i-coord? (vector (code 'I 10) (code 'J 10)))))

(define-test-suite
  test:j-coord?
  (check-true (j-coord? (vector (code 'J 10))))
  (check-false (j-coord? (vector)))
  (check-false (j-coord? (vector (code 'G 2))))
  (check-false (j-coord? (vector (code 'X 10))))
  (check-false (j-coord? (vector (code 'I 10) (code 'J 10)))))

(define-test-suite
  test:k-coord?
  (check-true (k-coord? (vector (code 'K 10))))
  (check-false (k-coord? (vector)))
  (check-false (k-coord? (vector (code 'G 2))))
  (check-false (k-coord? (vector (code 'Y 10))))
  (check-false (k-coord? (vector (code 'I 10) (code 'K 10)))))

(define-test-suite
  test:ij-coord?
  (check-true (ij-coord? (vector (code 'I 10) (code 'J 10))))
  (check-false (ij-coord? (vector)))
  (check-false (ij-coord? (vector (code 'G 2) (code 'J 5))))
  (check-false (ij-coord? (vector (code 'I 10))))
  (check-false (ij-coord? (vector (code 'J 10) (code 'I 10))))
  (check-false (ij-coord? (vector (code 'I 10) (code 'J 10) (code 'K 10)))))

(define-test-suite
  test:ik-coord?
  (check-true (ik-coord? (vector (code 'I 10) (code 'K 10))))
  (check-false (ik-coord? (vector)))
  (check-false (ik-coord? (vector (code 'I 10))))
  (check-false (ik-coord? (vector (code 'G 2) (code 'K 5))))
  (check-false (ik-coord? (vector (code 'K 10) (code 'I 10))))
  (check-false (ik-coord? (vector (code 'I 10) (code 'K 10) (code 'K 10)))))

(define-test-suite
  test:jk-coord?
  (check-true (jk-coord? (vector (code 'J 10) (code 'K 10))))
  (check-false (jk-coord? (vector)))
  (check-false (jk-coord? (vector (code 'J 10))))
  (check-false (jk-coord? (vector (code 'G 2) (code 'J 5))))
  (check-false (jk-coord? (vector (code 'K 10) (code 'J 10))))
  (check-false (jk-coord? (vector (code 'I 10) (code 'J 10) (code 'K 10)))))

(define-test-suite
  test:ijk-coord?
  (check-true (ijk-coord? (vector (code 'I 10) (code 'J 10) (code 'K 10))))
  (check-false (ijk-coord? (vector)))
  (check-false (ijk-coord? (vector (code 'Y 10))))
  (check-false (ijk-coord? (vector (code 'I 10) (code 'J 10))))
  (check-false (ijk-coord? (vector (code 'G 2) (code 'J 5) (code 'M 3))))
  (check-false (ijk-coord? (vector (code 'K 10) (code 'J 10) (code 'I 10))))
  (check-false (ijk-coord? (vector (code 'I 10) (code 'J 10) (code 'K 10) (code 'X 10)))))

(define-test-suite
  test:coord?
  (check-true (coord? (vector)))
  (check-true (coord? (vector (code 'I 10))))
  (check-true (coord? (vector (code 'Z 10))))
  (check-true (coord? (vector (code 'I 10) (code 'J 10) (code 'K 10))))
  (check-true (coord? (vector (code 'I 10) (code 'J 10))))
  (check-false (coord? (vector (code 'K 10) (code 'J 10) (code 'I 10))))
  (check-false (coord? (vector (code 'G 10))))
  (check-false (coord? (vector (code 'G 10))))
  (check-false (coord? (vector 1 2 3)))
  (check-false (coord? 10))
  (check-false (coord? "hello")))

