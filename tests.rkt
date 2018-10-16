#lang racket/base

(require rackunit
         racket/function
         racket/contract
         "g-code-tools.rkt")

(provide (all-defined-out))

;; -------------------- CODE TESTS

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
  (test-case
   "Produce #t on a G-code?"
   (check-true (g-code? (code 'G 0))))
  
  (test-case
   "Produce #f on a non-G-code?"
   (check-false (g-code? (code 'X 0)))))

(define-test-suite
  test:m-code?
  (test-case
   "Produce #t on a M-code?"
   (check-true (m-code? (code 'M 0))))
  
  (test-case
   "Produce #f on a non-M-code?"
   (check-false (m-code? (code 'X 0)))))

(define-test-suite
  test:f-code?
  (test-case
   "Produce #t on a F-code?"
   (check-true (f-code? (code 'F 0))))
  
  (test-case
   "Produce #f on a non-F-code?"
   (check-false (f-code? (code 'X 0)))))

(define-test-suite
  test:s-code?
  (test-case
   "Produce #t on a S-code?"
   (check-true (s-code? (code 'S 0))))
  
  (test-case
   "Produce #f on a non-S-code?"
   (check-false (s-code? (code 'X 0)))))

(define-test-suite
  test:r-code?
  (test-case
   "Produce #t on a R-code?"
   (check-true (r-code? (code 'R 0))))
  
  (test-case
   "Produce #f on a non-R-code?"
   (check-false (r-code? (code 'X 0)))))

(define-test-suite
  test:p-code?
  (test-case
   "Produce #t on a P-code?"
   (check-true (p-code? (code 'P 0))))
  
  (test-case
   "Produce #f on a non-P-code?"
   (check-false (p-code? (code 'X 0)))))

(define-test-suite
  test:x-code?
  (test-case
   "Produce #t on a X-code?"
   (check-true (x-code? (code 'X 0))))
  
  (test-case
   "Produce #f on a non-X-code?"
   (check-false (x-code? (code 'F 0)))))

(define-test-suite
  test:y-code?
  (test-case
   "Produce #t on a Y-code?"
   (check-true (y-code? (code 'Y 0))))
  
  (test-case
   "Produce #f on a non-Y-code?"
   (check-false (y-code? (code 'X 0)))))

(define-test-suite
  test:z-code?
  (test-case
   "Produce #t on a Z-code?"
   (check-true (z-code? (code 'Z 0))))
  
  (test-case
   "Produce #f on a non-Z-code?"
   (check-false (z-code? (code 'X 0)))))

(define-test-suite
  test:i-code?
  (test-case
   "Produce #t on a I-code?"
   (check-true (i-code? (code 'I 0))))
  
  (test-case
   "Produce #f on a non-I-code?"
   (check-false (i-code? (code 'X 0)))))

(define-test-suite
  test:j-code?
  (test-case
   "Produce #t on a J-code?"
   (check-true (j-code? (code 'J 0))))
  
  (test-case
   "Produce #f on a non-J-code?"
   (check-false (j-code? (code 'X 0)))))

(define-test-suite
  test:k-code?
  (test-case
   "Produce #t on a K-code?"
   (check-true (k-code? (code 'K 0))))
  
  (test-case
   "Produce #f on a non-K-code?"
   (check-false (k-code? (code 'X 0)))))

;; -------------------- COMMAND TESTS

(define-test-suite
  test:g-command?
  (test-case
   "Produce #t on a G-command?"
   (check-true (g-command? (command (code 'G 0) null))))
  
  (test-case
   "Produce #f on a non-G-command?"
   (check-false (g-command? (command (code 'X 0) null)))))

(define-test-suite
  test:m-command?
  (test-case
   "Produce #t on a M-command?"
   (check-true (m-command? (command (code 'M 0) null))))
  
  (test-case
   "Produce #f on a non-M-command?"
   (check-false (m-command? (command (code 'X 0) null)))))

(define-test-suite
  test:f-command?
  (test-case
   "Produce #t on a F-command?"
   (check-true (f-command? (command (code 'F 0) null))))
  
  (test-case
   "Produce #f on a non-F-command?"
   (check-false (f-command? (command (code 'X 0) null)))))

(define-test-suite
  test:s-command?
  (test-case
   "Produce #t on a S-command?"
   (check-true (s-command? (command (code 'S 0) null))))
  
  (test-case
   "Produce #f on a non-S-command?"
   (check-false (s-command? (command (code 'X 0) null)))))

(define-test-suite
  test:r-command?
  (test-case
   "Produce #t on a R-command?"
   (check-true (r-command? (command (code 'R 0) null))))
  
  (test-case
   "Produce #f on a non-R-command?"
   (check-false (r-command? (command (code 'X 0) null)))))

(define-test-suite
  test:p-command?
  (test-case
   "Produce #t on a P-command?"
   (check-true (p-command? (command (code 'P 0) null))))
  
  (test-case
   "Produce #f on a non-P-command?"
   (check-false (p-command? (command (code 'X 0) null)))))

(define-test-suite
  test:x-command?
  (test-case
   "Produce #t on a X-command?"
   (check-true (x-command? (command (code 'X 0) null))))
  
  (test-case
   "Produce #f on a non-X-command?"
   (check-false (x-command? (command (code 'F 0) null)))))

(define-test-suite
  test:y-command?
  (test-case
   "Produce #t on a Y-command?"
   (check-true (y-command? (command (code 'Y 0) null))))
  
  (test-case
   "Produce #f on a non-Y-command?"
   (check-false (y-command? (command (code 'X 0) null)))))

(define-test-suite
  test:z-command?
  (test-case
   "Produce #t on a Z-command?"
   (check-true (z-command? (command (code 'Z 0) null))))
  
  (test-case
   "Produce #f on a non-Z-command?"
   (check-false (z-command? (command (code 'X 0) null)))))

(define-test-suite
  test:i-command?
  (test-case
   "Produce #t on a I-command?"
   (check-true (i-command? (command (code 'I 0) null))))
  
  (test-case
   "Produce #f on a non-I-command?"
   (check-false (i-command? (command (code 'X 0) null)))))

(define-test-suite
  test:j-command?
  (test-case
   "Produce #t on a J-command?"
   (check-true (j-command? (command (code 'J 0) null))))
  
  (test-case
   "Produce #f on a non-J-command?"
   (check-false (j-command? (command (code 'X 0) null)))))

(define-test-suite
  test:k-command?
  (test-case
   "Produce #t on a K-command?"
   (check-true (k-command? (command (code 'K 0) null))))
  
  (test-case
   "Produce #f on a non-K-command?"
   (check-false (k-command? (command (code 'X 0) null)))))

(define-test-suite
  test:param-in-command?
  (test-case
   "Produces #f when params is empty."
   (check-false (param-in-command? (code 'X 20)
                                   (command (code 'G 0)
                                            null))))

  (test-case
   "Produces #t when code is in params."
   (check-true (param-in-command? (code 'X 20)
                                   (command (code 'G 0)
                                            (list (code 'X 20)
                                                  (code 'Y 20))))))

  (test-case
   "Produces #f when code is not in params."
   (check-false (param-in-command? (code 'Z 20)
                                   (command (code 'G 0)
                                            (list (code 'X 20)
                                                  (code 'Y 20))))))

  (test-case
   "Produces #f when code with same sym but different num is in params."
   (check-false (param-in-command? (code 'X 10)
                                   (command (code 'G 0)
                                            (list (code 'X 20)
                                                  (code 'Y 20)))))))

(define-test-suite
  test:param-by-sym?
  (test-case
   "Produces #f when params is empty."
   (check-false (param-by-sym 'X
                              (command (code 'G 0)
                                       null))))

  (test-case
   "Produces the code when code with sym is in params."
   (check-equal? (code 'X 20) (param-by-sym 'X
                                            (command (code 'G 0)
                                                     (list (code 'X 20)
                                                           (code 'Y 20))))))

  (test-case
   "Produces #f when code with sym is not in params."
   (check-false (param-by-sym 'Z
                              (command (code 'G 0)
                                       (list (code 'X 20)
                                             (code 'Y 20)))))))

;; -------------------- COORDINATE TESTS

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

(define-test-suite
  test:coord-code?
  (check-true (coord-code? (code 'X 10)))
  (check-true (coord-code? (code 'Y 10)))
  (check-true (coord-code? (code 'Z 10)))
  (check-true (coord-code? (code 'I 10)))
  (check-true (coord-code? (code 'J 10)))
  (check-true (coord-code? (code 'K 10)))
  (check-false (coord-code? (code 'G 10)))
  (check-false (coord-code? (code 'M 10))))
