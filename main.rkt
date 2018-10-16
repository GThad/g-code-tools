#lang racket/base

(module+ test
  (require rackunit
           rackunit/text-ui
           "tests.rkt"))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

(require "g-code-tools.rkt")
(provide (all-from-out "g-code-tools.rkt"))

(module+ test
  ;; Tests to be run with raco test
  (displayln "Running Tests.")
  
  (run-tests test:g-code-sym? 'verbose)
  
  (run-tests test:g-code? 'verbose)
  (run-tests test:m-code? 'verbose)
  (run-tests test:f-code? 'verbose)
  (run-tests test:m-code? 'verbose)
  (run-tests test:s-code? 'verbose)
  (run-tests test:r-code? 'verbose)
  (run-tests test:p-code? 'verbose)
  (run-tests test:x-code? 'verbose)
  (run-tests test:y-code? 'verbose)
  (run-tests test:z-code? 'verbose)
  (run-tests test:i-code? 'verbose)
  (run-tests test:j-code? 'verbose)
  (run-tests test:k-code? 'verbose)

  (run-tests test:empty-coord? 'verbose)
  (run-tests test:x-coord? 'verbose)
  (run-tests test:y-coord? 'verbose)
  (run-tests test:z-coord? 'verbose)
  (run-tests test:xy-coord? 'verbose)
  (run-tests test:xz-coord? 'verbose)
  (run-tests test:yz-coord? 'verbose)
  (run-tests test:xyz-coord? 'verbose)
  (run-tests test:i-coord? 'verbose)
  (run-tests test:j-coord? 'verbose)
  (run-tests test:k-coord? 'verbose)
  (run-tests test:ij-coord? 'verbose)
  (run-tests test:ik-coord? 'verbose)
  (run-tests test:jk-coord? 'verbose)
  (run-tests test:ijk-coord? 'verbose)
  (run-tests test:coord? 'verbose)
  (run-tests test:coord-code? 'verbose)
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  ;; Do nothing.
  )
