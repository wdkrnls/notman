(use-modules (guix packages)
             (guix gexp)
             (guix utils)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (srfi srfi-1))

(define-public notman
  (package
    (name "notman")
    (version "0.1.0")
    (source (local-file (dirname (current-filename))
                        "notman-source" #:recursive? #t))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f
           ;; No configure script, so inject the prefix via make instead.
           #:make-flags #~(list (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        ;; The default `build' runs `make', i.e. `make check',
                        ;; which shells out to `guix shell' -- impossible in
                        ;; the sandbox.  The install phase still runs.
                        (delete 'build))))
    (propagated-inputs (list guile-3.0 guile-xapian))
    (home-page "https://example.invalid/notman")
    (synopsis "Search man pages with Xapian")
    (description
     "notman indexes the section headings and body text of the manual pages
found on the system into a Xapian database, then searches it with a small
query language.")
    (license license:gpl3+)))

notman
