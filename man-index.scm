;;; man-index.scm -- index man pages into a Xapian database.
;;;
;;; usage: guile man-index.scm [--rebuild] [--lang=LANG] [--man-root=PATH]
;;;                            [--man-roots] [--file=PATH[,PATH...]] DBPATH
;;;        guile man-index.scm --render PATH      (the canonical renderer)
;;;
;;; LANG is unset (English, top-level man* dirs only), a language tag such
;;; as "de" or "de_DE.UTF-8" (that subtree as well), or "all" (every locale,
;;; indexed without stemming).
;;;
;;; Section headings (NAME, SYNOPSIS, DESCRIPTION, "SEE ALSO", ...) become
;;; search fields: NAME -> XSname, "SEE ALSO" -> XSsee_also.  Each heading
;;; also yields an XH<name> term, so "has:see_also" finds pages containing
;;; that section.  The whole page is indexed unprefixed.  Document data
;;; records name, section number, path, render width, and the character
;;; offset of each section in the rendered text.
;;;
;;; The manifest is a memo of what is already in the database, so it is
;;; stamped with index-version: change how documents are built (prefixes,
;;; XH terms, heading detection) and bump that number, which discards the
;;; memo and reindexes everything once.
;;;
;;; Pages slower than a couple of seconds print a per-phase breakdown to
;;; stderr: render (the man/groff subprocess), headings (section scan),
;;; index (term generation), store (the Xapian write).

(use-modules (ice-9 format)
             (ice-9 ftw)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-14)
             (srfi srfi-26)
             (xapian xapian)
             (xapian wrap))

(define man-width 80)                   ; offsets depend on this

;;; ---- purging -----------------------------------------------------------

;; guile-xapian 0.6 exports no delete-document!, but (xapian wrap) does, under
;; the SWIG name; in git the former is a one-line alias for the latter.  Takes
;; the same Q-prefixed unique term index-page wrote, so no docid is needed.
(define delete-document! WritableDatabase-delete-document)

(define (purge-stale! db stale)
  (for-each
   (lambda (path)
     (catch #t
       (lambda () (delete-document! db (string-append "Q" path)))
       (lambda (key . args)
         (format (current-error-port) "could not purge ~a: ~a ~s~%" path key args))))
   stale))

;;; ---- rendering ---------------------------------------------------------

(define sgr-param-chars (string->char-set "0123456789;"))

;; Index just past the SGR sequence starting at I (S[I] is ESC), or N.
(define (skip-sgr s i n)
  (if (and (< (+ i 1) n) (char=? (string-ref s (+ i 1)) #\[))
      (let loop ((j (+ i 2)))
        (cond ((>= j n) n)
              ((char-set-contains? sgr-param-chars (string-ref s j)) (loop (+ j 1)))
              (else (+ j 1))))          ; final byte of the sequence
      (+ i 1)))

;; Drop the character preceding each backspace: "N\bNA\bAM\bME\bE" -> "NAME".
;; Single pass, no regexp -- regexp-substitute/global is effectively quadratic
;; on a multi-megabyte page with tens of thousands of matches, which is what
;; made the GCC manual take four minutes.
(define (deformat s)
  (let* ((n (string-length s))
         (out (open-output-string)))
    (let loop ((i 0))
      (cond
       ((>= i n) (get-output-string out))
       ((char=? (string-ref s i) #\x1b) (loop (skip-sgr s i n)))
       ((and (< (+ i 1) n) (char=? (string-ref s (+ i 1)) #\backspace))
        (loop (+ i 2)))                 ; the duplicate follows and is emitted
       (else (write-char (string-ref s i) out) (loop (+ i 1)))))))

(define (shell-quote s)
  (string-append "'"
                 (regexp-substitute/global #f (make-regexp "'") s
                                           'pre "'\\''" 'post)
                 "'"))

(define (slurp-lines port)
  (let loop ((acc '()) (line (read-line port)))
    (if (eof-object? line) (reverse acc) (loop (cons line acc) (read-line port)))))

(define (slurp port) (string-join (slurp-lines port) "\n"))

(define (basename path)
  (let ((i (string-rindex path #\/))) (if i (substring path (+ i 1)) path)))

;; The one and only way a page is rendered.  Every offset in the database is
;; an offset into exactly this text.
(define (render path)
  (let ((port (open-input-pipe
               (format #f "MANWIDTH=~a man --nh -Tutf8 -l ~a 2>/dev/null"
                       man-width (shell-quote path)))))
    (let ((text (deformat (slurp port)))) (close-pipe port) text)))

;;; ---- which files to index ----------------------------------------------

(define (env-nonempty name)
  (let ((v (getenv name))) (and v (not (string-null? v)) v)))

;; Split S on CH, keeping empty fields so we can drop them deliberately.
(define (split-on-char s ch)
  (let loop ((start 0) (acc '()))
    (let ((i (string-index s ch start)))
      (if i
          (loop (+ i 1) (cons (substring s start i) acc))
          (reverse (cons (substring s start) acc))))))

;; man-db's own view of the search path, if the binary is reachable.
(define (manpath-roots)
  (let ((port (open-input-pipe "manpath 2>/dev/null")))
    (let ((line (read-line port)))
      (close-pipe port)
      (if (eof-object? line)
          '()
          (filter (lambda (d) (not (string-null? d))) (split-on-char line #\:))))))

;; Directories to scan, in priority order.  EXTRA comes from --man-root= and
;; wins outright; then the environment, which is how a `guix shell` profile
;; announces its pages; then manpath(1); then the two usual roots.
(define (man-roots extra)
  (let* ((home (getenv "HOME"))
         (candidates
          (append extra
                  (if (env-nonempty "MANPATH")
                      (filter (lambda (d) (not (string-null? d)))
                              (split-on-char (getenv "MANPATH") #\:))
                      '())
                  (if (env-nonempty "GUIX_ENVIRONMENT")
                      (list (string-append (getenv "GUIX_ENVIRONMENT") "/share/man"))
                      '())
                  (manpath-roots)
                  (list "/usr/share/man")
                  (if home (list (string-append home "/.guix-profile/share/man")) '()))))
    (delete-duplicates
     (filter (lambda (d)
               (and (not (string-null? d))
                    (file-exists? d)            ; #f for missing, never raises
                    (file-is-directory? d)))
             candidates)
     string=?)))

;; Name of the directory directly under ROOT, or #f.
(define (man-subdir root path)
  (let ((prefix (if (string-suffix? "/" root) root (string-append root "/"))))
    (and (string-prefix? prefix path)
         (> (string-length path) (string-length prefix))
         (let* ((rest (substring path (string-length prefix)))
                (slash (string-index rest #\/)))
           (and slash (substring rest 0 slash))))))

(define (wanted-page? lang root path)
  (let ((dir (man-subdir root path)))
    (and dir
         (or (string-prefix? "man" dir)
             (and lang
                  (or (string=? lang "all")
                      (string=? dir lang)
                      (string-prefix? (string-append lang ".") dir)
                      (string-prefix? (string-append lang "_") dir)))))))

;; -L so that Guix's symlink farms resolve; find still reports names under
;; ROOT, so the recorded paths stay stable as long as ROOT does.
(define (man-files lang roots)
  (sort
   (append-map
    (lambda (root)
      (let ((port (open-input-pipe
                   (format #f "find -L ~a -type f 2>/dev/null" (shell-quote root)))))
        (let ((files (slurp-lines port)))
          (close-pipe port)
          (filter (lambda (p) (wanted-page? lang root p)) files))))
    roots)
   string<?))

;;; ---- language and stemming ---------------------------------------------

;; "en_US.UTF-8" -> "en"; "de" -> "de"; "all" -> "all"
(define (lang-base lang)
  (let loop ((i 0))
    (cond ((>= i (string-length lang)) lang)
          ((memv (string-ref lang i) '(#\_ #\.)) (substring lang 0 i))
          (else (loop (+ i 1))))))

;; The name written to <db>.stem, so search can rebuild the same stemmer.
(define (stem-name lang)
  (cond ((or (not lang) (string=? lang "en")) "en")
        ((string=? lang "all") "none")
        (else (let ((base (lang-base lang)))
                (if (string=? base "en") "en" base)))))

;; Xapian rejects languages it doesn't know; fall back to no stemming rather
;; than aborting the run.  Search does the same, so the two agree.
(define (stemmer-for lang)
  (catch #t
    (lambda () (make-stem (stem-name lang)))
    (lambda _ (make-stem "none"))))

;;; ---- section splitting -------------------------------------------------

(define heading-chars (char-set-union char-set:upper-case (string->char-set " ")))

(define (heading? line next-line)
  (and (> (string-length line) 0)
       ;; column 0: first character is a capital, never a space
       (char-set-contains? char-set:upper-case (string-ref line 0))
       ;; only capitals and spaces -- no digits, no punctuation
       (string-every (cut char-set-contains? heading-chars <>) line)
       ;; previous line blank is checked by the caller
       ;; the next line exists, is non-blank, and is indented
       next-line
       (> (string-length next-line) 0)
       (not (string-null? (string-trim-both next-line)))
       (char-set-contains? char-set:whitespace (string-ref next-line 0))))

(define (lines-of text)                 ; list of (OFFSET . LINE)
  (let loop ((start 0) (acc '()))
    (let ((nl (string-index text #\newline start)))
      (if nl
          (loop (+ nl 1) (cons (cons start (substring text start nl)) acc))
          (reverse (cons (cons start (substring text start)) acc))))))

(define (headings text)
  (let loop ((ls (lines-of text)) (prev-blank? #t) (acc '()))
    (match ls
      (() (reverse acc))
      (((off . line) . rest)
       (let ((blank? (string-null? (string-trim-both line))))
         (loop rest blank?
               (if (and prev-blank?
                        (heading? line (and (pair? rest) (cdar rest))))
                   (cons (cons line off) acc)
                   acc)))))))

(define (normalize-section heading)
  (string-downcase
   (string-join (string-tokenize heading char-set:letter+digit) "_")))

(define (section-prefix n) (string-append "XS" n))     ; no upcasing

;;; ---- timing ------------------------------------------------------------

(define (make-timer) (vector '()))

(define (timed timer label thunk)
  (let* ((t0 (get-internal-real-time))
         (value (thunk))
         (dt (/ (- (get-internal-real-time) t0)
                internal-time-units-per-second)))
    (vector-set! timer 0 (cons (cons label dt) (vector-ref timer 0)))
    value))

(define (report-slow path timer)
  (let* ((phases (reverse (vector-ref timer 0)))
         (total (apply + (map cdr phases))))
    (when (> total 2.0)
      (format (current-error-port) "\nslow ~,1fs: ~a~%" total path)
      (for-each (lambda (p)
                  (format (current-error-port) "    ~10a ~,1fs~%" (car p) (cdr p)))
                phases)
      (force-output (current-error-port)))))

;;; ---- indexing ----------------------------------------------------------

;; Returns the list of normalized section names found, or #f if not a page.
(define (index-page db stem path)
  (let ((timer (make-timer)))
    (let* ((file (basename path))
           (file (if (string-suffix? ".gz" file)
                     (substring file 0 (- (string-length file) 3)) file))
           (m (string-match "^(.+)\\.([0-9][a-zA-Z]*)$" file)))
      (and m
           (let* ((pagename (match:substring m 1))
                  (section (match:substring m 2))
                  (text (timed timer "render" (lambda () (render path)))))
             (and (not (string-null? (string-trim-both text)))
                  (let* ((hs (timed timer "headings" (lambda () (headings text))))
                         (offsets (filter-map (lambda (h)
                                                (let ((n (normalize-section (car h))))
                                                  (and (not (string-null? n))
                                                       (cons n (cdr h)))))
                                              hs))
                         (norms (delete-duplicates (map car offsets)))
                         (idterm (string-append "Q" path))
                         (record `((name . ,pagename) (section . ,section)
                                   (path . ,path) (width . ,man-width)
                                   (sections . ,offsets)))
                         (doc (make-document
                               #:data (call-with-output-string (cut write record <>))
                               #:terms (append
                                        (map (lambda (n) (cons (string-append "XH" n) 0))
                                             norms)
                                        (list (cons idterm 0)
                                              (cons (string-append "XSEC" section) 0)
                                              (cons (string-append "XSEC"
                                                                   (substring section 0 1))
                                                    0)))))
                         (generator (make-term-generator #:stem stem
                                                         #:document doc)))
                    (timed timer "index"
                           (lambda ()
                             (let loop ((hs hs))
                               (unless (null? hs)
                                 (let* ((h (car hs))
                                        (start (cdr h))
                                        (end (if (null? (cdr hs))
                                                 (string-length text)
                                                 (cdr (cadr hs))))
                                        (nl (string-index text #\newline start))
                                        (body-start (if (and nl (< nl end)) (+ nl 1) end))
                                        (norm (normalize-section (car h))))
                                   (unless (string-null? norm)
                                     (index-text! generator
                                                  (substring text body-start end)
                                                  #:prefix (section-prefix norm)))
                                   (loop (cdr hs)))))
                             (index-text! generator text)))
                    (timed timer "store"
                           (lambda () (replace-document! db idterm doc)))
                    (report-slow path timer)
                    (map car offsets))))))))

;;; ---- manifest ----------------------------------------------------------

;; Bump whenever the terms a document carries change: new or renamed
;; prefixes, added or removed XH terms, changed heading detection.  The
;; manifest is a memo of what is in the database, so a change that alters the
;; terms has to invalidate it -- otherwise .sections keeps advertising
;; sections that no document carries a has: term for.
(define index-version 2)

(define (manifest-file dbpath) (string-append dbpath ".manifest"))

;; The entries alist, or '() if the manifest is missing or was written by a
;; different version of this program.  Entries are (path size cksum sections).
(define (read-manifest dbpath)
  (let ((f (manifest-file dbpath)))
    (if (file-exists? f)
        (let ((data (catch #t
                      (lambda () (call-with-input-file f read))
                      (lambda _ #f))))
          (cond ((not (and (pair? data) (pair? (cdr data)))) '())
                ((equal? (car data) index-version) (cadr data))
                (else
                 (format (current-error-port)
                         "index format changed; reindexing everything~%")
                 '())))
        '())))

;; Written as (VERSION ENTRIES), so a stale manifest is recognisable.
(define (write-manifest dbpath entries)
  (let ((tmp (string-append (manifest-file dbpath) ".new")))
    (call-with-output-file tmp
      (lambda (port) (write (list index-version entries) port) (newline port)))
    (rename-file tmp (manifest-file dbpath))))

(define (write-nul-separated port paths)
  (for-each (lambda (p) (display p port) (write-char #\nul port)) paths))

;; path -> (size . cksum) for PATHS, from a single cksum invocation.
;; Content-based: a reinstall that only touches timestamps changes nothing,
;; and same-size edits are still caught.  The path list goes through a temp
;; file so ARG_MAX never matters.
(define (file-hashes paths)
  (let* ((tmp (format #f "/tmp/notman-cksum-~a" (getpid)))
         (port (begin
                 (call-with-output-file tmp
                   (lambda (out) (write-nul-separated out paths)))
                 (open-input-pipe
                  (format #f "xargs -0 -a ~a cksum 2>/dev/null" (shell-quote tmp))))))
    (let loop ((acc '()) (line (read-line port)))
      (if (eof-object? line)
          (begin (close-pipe port)
                 (false-if-exception (delete-file tmp))
                 acc)
          (let ((m (string-match "^([0-9]+) ([0-9]+) (.*)$" line)))
            (loop (if m
                      (acons (match:substring m 3)
                             (cons (string->number (match:substring m 2))
                                   (match:substring m 1))
                             acc)
                      acc)
                  (read-line port)))))))

;; Index one page, returning its manifest entry or #f.
;; counters is #(new unchanged)
(define (index-one db old stem hashes path counters)
  (let ((sig (assoc-ref hashes path)))
    (and sig
         (let* ((size (car sig))
                (hash (cdr sig))
                (prev (assoc path old))
                (unchanged? (and prev
                                 (= size (cadr prev))
                                 (string=? hash (caddr prev)))))
           (cond
            (unchanged?
             (vector-set! counters 1 (+ 1 (vector-ref counters 1)))
             prev)
            (else
             (vector-set! counters 0 (+ 1 (vector-ref counters 0)))
             (let ((found (catch #t
                                 (lambda () (index-page db stem path))
                                 (lambda (key . args)
                                   (format (current-error-port)
                                           "skip ~a: ~a ~s~%" path key args)
                                   #f))))
               (list path size hash (or found '())))))))))

(define (stale-ids old entries)
  (filter (lambda (path) (not (assoc path entries))) (map car old)))

(define (rm-rf path)
  (cond ((not (file-exists? path)) #t)
        ((file-is-directory? path)
         (for-each (lambda (f)
                     (rm-rf (string-append path "/" f)))
                   (scandir path (lambda (f) (not (member f '("." ".."))))))
         (rmdir path))
        (else (delete-file path))))

;;; ---- driver ------------------------------------------------------------

;; PATHS is #f for a full run, or an explicit list of files to (re)index.
(define (index-all dbpath lang extra-roots paths)
  (let* ((lang (and lang (if (string=? lang "all") "all" (lang-base lang))))
         (tty? (isatty? (current-error-port)))
         (roots (man-roots extra-roots))
         (files (or paths (man-files lang roots)))
         (hashes (begin
                   (when tty?
                     (display "Hashing files... " (current-error-port))
                     (force-output (current-error-port)))
                   (let ((h (file-hashes files)))
                     (when tty? (display "done\n" (current-error-port)))
                     h)))
         (stem (stemmer-for lang))
         (total (length files))
         (old (read-manifest dbpath))
         (counters (vector 0 0))
         (entries '())
         (n 0))
    (when (and tty? (not paths))
      (format (current-error-port) "Roots: ~a~%" (string-join roots ", ")))
    (call-with-writable-database dbpath
      (lambda (db)
        (for-each
         (lambda (path)
           (set! n (+ 1 n))
           (let ((entry (index-one db old stem hashes path counters)))
             (when entry (set! entries (cons entry entries))))
           (when tty?
             (format (current-error-port)
                     "\rIndexing: ~a/~a (~a new, ~a unchanged)"
                     n total (vector-ref counters 0) (vector-ref counters 1))
             (force-output (current-error-port))))
         files)
        ;; Purge documents whose files are gone.  Must follow the loop, since
        ;; entries is not complete until then, and must be skipped for a
        ;; --file= run, which has not looked at the rest of the tree.
        (unless paths
          (let ((stale (stale-ids old entries)))
            (unless (null? stale)
              (when tty? (format (current-error-port) "~%"))
              (format (current-error-port)
                      "purging ~a stale document(s)~%" (length stale))
              (force-output (current-error-port))
              (purge-stale! db stale))))))
    (when tty? (format (current-error-port) "~%"))
    (let ((entries (if paths
                       (append entries
                               (filter (lambda (e) (not (member (car e) files))) old))
                       entries)))
      (write-manifest dbpath entries)
      (call-with-output-file (string-append dbpath ".stem")
        (lambda (port) (display (stem-name lang) port) (newline port)))
      (call-with-output-file (string-append dbpath ".sections")
        (lambda (port)
          (for-each (lambda (s) (display s port) (newline port))
                    (sort (delete-duplicates
                           (append-map (lambda (e) (list-ref e 3)) entries))
                          string<?)))))))

(define (rebuild dbpath lang extra-roots paths)
  (rm-rf dbpath)
  (rm-rf (string-append dbpath ".manifest"))
  (rm-rf (string-append dbpath ".sections"))
  (rm-rf (string-append dbpath ".stem"))
  (index-all dbpath lang extra-roots paths))

(define (usage program)
  (format (current-error-port)
          "Usage: ~a [--rebuild] [--lang=LANG] [--man-root=PATH] [--man-roots]~%\
       ~a [--file=PATH[,PATH...]] DBPATH~%\
       ~a --render PATH~%"
          program program program))

;; Flags may appear anywhere; the one positional argument is the database.
(define (parse-args args)
  (let loop ((as args) (rebuild? #f) (render? #f) (show-roots? #f)
             (lang #f) (files '()) (roots '()) (pos '()))
    (match as
      (() (values rebuild? render? show-roots? lang
                  (if (null? files) #f (reverse files))
                  (reverse roots)
                  (reverse pos)))
      ((a . rest)
       (cond
        ((string=? a "--rebuild") (loop rest #t render? show-roots? lang files roots pos))
        ((string=? a "--render") (loop rest rebuild? #t show-roots? lang files roots pos))
        ((string=? a "--man-roots") (loop rest rebuild? render? #t lang files roots pos))
        ((string-match "^--lang=(.*)$" a)
         => (lambda (m) (loop rest rebuild? render? show-roots?
                              (match:substring m 1) files roots pos)))
        ((string-match "^--man-root=(.*)$" a)
         => (lambda (m) (loop rest rebuild? render? show-roots? lang files
                              (cons (match:substring m 1) roots) pos)))
        ((string-match "^--file=(.*)$" a)
         => (lambda (m)
              (let ((ps (filter (lambda (s) (not (string-null? s)))
                                (split-on-char (match:substring m 1) #\,))))
                (loop rest rebuild? render? show-roots? lang
                      (append (reverse ps) files) roots pos))))
        (else (loop rest rebuild? render? show-roots? lang files roots (cons a pos))))))))

(match (command-line)
  ((_ . args)
   (call-with-values (lambda () (parse-args args))
     (lambda (rebuild? render? show-roots? lang files roots positional)
       (cond
        (show-roots?
         (for-each (lambda (r) (display r) (newline)) (man-roots roots)))
        (render?
         (if (pair? positional)
             (display (render (car positional)))
             (usage (car (command-line)))))
        ((null? positional) (usage (car (command-line))))
        (else
         (let ((dbpath (car positional)))
           (if rebuild?
               (rebuild dbpath lang roots files)
               (index-all dbpath lang roots files))))))))
  ((program . _) (usage program)))
