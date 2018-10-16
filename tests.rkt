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
  (check-true (empty-coord? null))
  (check-false (empty-coord? (list (code 'X 10))))
  (check-false (empty-coord? (list (code 'X 10) (code 'Y 10)))))

(define-test-suite
  test:x-coord?
  (check-true (x-coord? (list (code 'X 10))))
  (check-false (x-coord? null))
  (check-false (x-coord? (list (code 'Y 10))))
  (check-false (x-coord? (list (code 'X 10) (code 'Y 10)))))

(define-test-suite
  test:y-coord?
  (check-true (y-coord? (list (code 'Y 10))))
  (check-false (y-coord? null))
  (check-false (y-coord? (list (code 'X 10))))
  (check-false (y-coord? (list (code 'X 10) (code 'Y 10)))))

(define-test-suite
  test:z-coord?
  (check-true (z-coord? (list (code 'Z 10))))
  (check-false (z-coord? null))
  (check-false (z-coord? (list (code 'Y 10))))
  (check-false (z-coord? (list (code 'X 10) (code 'Y 10)))))

(define-test-suite
  test:xy-coord?
  (check-true (xy-coord? (list (code 'X 10) (code 'Y 10))))
  (check-false (xy-coord? null))
  (check-false (xy-coord? (list (code 'Y 10))))
  (check-false (xy-coord? (list (code 'X 10) (code 'Y 10) (code 'Z 10)))))

(define-test-suite
  test:xz-coord?
  (check-true (xz-coord? (list (code 'X 10) (code 'Z 10))))
  (check-false (xz-coord? null))
  (check-false (xz-coord? (list (code 'Y 10))))
  (check-false (xz-coord? (list (code 'X 10) (code 'Y 10) (code 'Z 10)))))

(define-test-suite
  test:yz-coord?
  (check-true (yz-coord? (list (code 'Y 10) (code 'Z 10))))
  (check-false (yz-coord? null))
  (check-false (yz-coord? (list (code 'Y 10))))
  (check-false (yz-coord? (list (code 'X 10) (code 'Y 10) (code 'Z 10)))))

(define-test-suite
  test:xyz-coord?
  (check-true (xyz-coord? (list (code 'X 10) (code 'Y 10) (code 'Z 10))))
  (check-false (xyz-coord? null))
  (check-false (xyz-coord? (list (code 'Y 10))))
  (check-false (xyz-coord? (list (code 'X 10) (code 'Z 10)))))

(define-test-suite
  test:i-coord?
  (check-true (i-coord? (list (code 'I 10))))
  (check-false (i-coord? null))
  (check-false (i-coord? (list (code 'Y 10))))
  (check-false (i-coord? (list (code 'I 10) (code 'J 10)))))

(define-test-suite
  test:j-coord?
  (check-true (j-coord? (list (code 'J 10))))
  (check-false (j-coord? null))
  (check-false (j-coord? (list (code 'X 10))))
  (check-false (j-coord? (list (code 'I 10) (code 'J 10)))))

(define-test-suite
  test:k-coord?
  (check-true (k-coord? (list (code 'K 10))))
  (check-false (k-coord? null))
  (check-false (k-coord? (list (code 'Y 10))))
  (check-false (k-coord? (list (code 'I 10) (code 'K 10)))))

(define-test-suite
  test:ij-coord?
  (check-true (ij-coord? (list (code 'I 10) (code 'J 10))))
  (check-false (ij-coord? null))
  (check-false (ij-coord? (list (code 'I 10))))
  (check-false (ij-coord? (list (code 'I 10) (code 'J 10) (code 'K 10)))))

(define-test-suite
  test:ik-coord?
  (check-true (ik-coord? (list (code 'I 10) (code 'K 10))))
  (check-false (ik-coord? null))
  (check-false (ik-coord? (list (code 'I 10))))
  (check-false (ik-coord? (list (code 'I 10) (code 'J 10) (code 'K 10)))))

(define-test-suite
  test:jk-coord?
  (check-true (jk-coord? (list (code 'J 10) (code 'K 10))))
  (check-false (jk-coord? null))
  (check-false (jk-coord? (list (code 'J 10))))
  (check-false (jk-coord? (list (code 'I 10) (code 'J 10) (code 'K 10)))))

(define-test-suite
  test:ijk-coord?
  (check-true (ijk-coord? (list (code 'I 10) (code 'J 10) (code 'K 10))))
  (check-false (ijk-coord? null))
  (check-false (ijk-coord? (list (code 'Y 10))))
  (check-false (ijk-coord? (list (code 'I 10) (code 'J 10)))))

(define-test-suite
  test:coord?
  (check-true (coord? null))
  (check-true (coord? (list (code 'I 10))))
  (check-true (coord? (list (code 'Z 10))))
  (check-true (coord? (list (code 'I 10) (code 'J 10) (code 'K 10))))
  (check-true (coord? (list (code 'I 10) (code 'J 10))))
  (check-false (coord? (list (code 'K 10) (code 'J 10) (code 'I 10))))
  (check-false (coord? (list (code 'G 10))))
  (check-false (coord? (list (code 'G 10))))
  (check-false (coord? (list 1 2 3)))
  (check-false (coord? 10))
  (check-false (coord? "hello")))

