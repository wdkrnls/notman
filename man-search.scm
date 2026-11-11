;;; man-search.scm -- usage: guile man-search.scm DBPATH [FLAGS] QUERY...
;;;
;;; Flags:
;;;   --debug          extra information on stderr
;;;   --max-width=N    context width in characters (default 80);
;;;                    N of -1/inf/unlimited means the whole paragraph
;;;   --point          print the character offset instead of the context
;;;   --file-path      label hits by file path rather than NAME(SECTION)
;;;
;;; One line per hit.  Normal query:
;;;   pthread_create(3): ... create a new thread ...
;;; with --point:
;;;   pthread_create(3) 428
;;; with --file-path --point:
;;;   /usr/share/man/man3/pthread_create.3.gz 428
;;;
;;; A query naming only sections (has:reporting_bugs, section:3) has no text
;;; to show, so hits are reported with the page's NAME line instead:
;;;   pthread_create(3): create a new thread
;;; and with --file-path, the whole NAME section:
;;;   /usr/share/man/man3/pthread_create.3.gz: pthread_create - create a new thread
;;;
;;; Offsets are character positions in the text from
;;;   guile man-index.scm --render PATH
;;; so (goto-char (1+ offset)) in Emacs lands on the match.

(use-modules (ice-9 format)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-13)
             (xapian xapian))

;;; ---- sidecars ----------------------------------------------------------

(define (slurp-lines port)
  (let loop ((acc '()) (l (read-line port)))
    (if (eof-object? l) (reverse acc) (loop (cons l acc) (read-line port)))))

;; Non-blank, trimmed lines of FILE, or '() if it doesn't exist.
(define (read-lines file)
  (if (file-exists? file)
      (call-with-input-file file
        (lambda (port)
          (filter (lambda (s) (not (string-null? s)))
                  (map string-trim-both (slurp-lines port)))))
      '()))

(define (section-fields dbpath)
  (read-lines (string-append dbpath ".sections")))

(define (stem-name dbpath)
  (let ((names (read-lines (string-append dbpath ".stem"))))
    (if (null? names) "en" (car names))))

;; Must match index-all's stemmer-for: same name, and "none" if Xapian
;; doesn't know the language the indexer recorded.
(define (stemmer-for name)
  (catch #t (lambda () (make-stem name)) (lambda _ (make-stem "none"))))

(define (prefix-for field) (string-append "XS" field))

;;; ---- rendering, delegated so offsets match the indexer -----------------

(define (shell-quote s)
  (string-append "'"
                 (regexp-substitute/global #f (make-regexp "'") s
                                           'pre "'\\''" 'post)
                 "'"))

(define (script-dir)
  (let* ((self (car (command-line)))
         (i (string-rindex self #\/)))
    (if i (substring self 0 i) ".")))

(define (render path)
  (let* ((indexer (string-append (script-dir) "/man-index.scm"))
         (port (open-input-pipe
                (format #f "guile ~a --render ~a 2>/dev/null"
                        (shell-quote indexer) (shell-quote path)))))
    (let ((text (string-join (slurp-lines port) "\n")))
      (close-pipe port)
      text)))

;;; ---- finding words in text --------------------------------------------

;; First index of NEEDLE in TEXT at or after START, or #f.  string-contains-ci
;; may return two values, so normalise to one.
(define (first-index text needle start)
  (call-with-values
      (lambda () (string-contains-ci text needle start))
    (lambda args (and (pair? args) (car args)))))

;; The word, then progressively shorter prefixes: the stemmer matched them
;; even though the literal strings differ.
(define (word-candidates w)
  (let ((n (string-length w)))
    (append (list w)
            (if (>= n 7) (list (substring w 0 6)) '())
            (if (>= n 6) (list (substring w 0 5)) '()))))

;; The query minus any "field:" prefixes; hyphenated words also yield their
;; first piece, and longer words are tried first so "thread-based" beats
;; "thread".
(define (query-words querystring)
  (let* ((raw (string-tokenize querystring char-set:graphic))
         (stripped (map (lambda (w)
                          (let ((i (string-index w #\:)))
                            (if i (substring w (+ i 1)) w)))
                        raw))
         (words (append-map (lambda (w)
                              (let ((i (string-index w #\-)))
                                (if i (list w (substring w 0 i)) (list w))))
                            stripped)))
    (sort (delete-duplicates
           (filter (lambda (w) (>= (string-length w) 2)) words))
          (lambda (a b) (> (string-length a) (string-length b))))))

;; Which section did the query name, if any?  "reporting_bugs:foo" -> that.
(define (queried-section querystring fields)
  (let loop ((pos 0))
    (let ((m (string-match "[a-zA-Z][a-zA-Z0-9_]*:" querystring pos)))
      (cond ((not m) #f)
            (else
             (let ((f (string-drop-right (match:substring m) 1)))
               (if (member f fields) f (loop (match:end m)))))))))

;; #t if the query is nothing but section filters, e.g. "has:reporting_bugs"
;; or "section:3 has:bugs" -- there is no text to locate, only pages to list.
(define (section-only-query? querystring)
  (let ((tokens (string-tokenize querystring char-set:graphic)))
    (and (pair? tokens)
         (every (lambda (t)
                  (or (string-prefix? "has:" t)
                      (string-prefix? "section:" t)))
                tokens))))

;;; ---- context -----------------------------------------------------------

(define no-limit 100000000)             ; "absurdly high"

;; "80" -> 80; "-1", "inf", "infinity", "infinite", "unlimited" -> no-limit.
(define (parse-max-width s)
  (cond ((member s '("inf" "infinity" "infinite" "unlimited")) no-limit)
        ((string->number s)
         => (lambda (n) (let ((n (inexact->exact (truncate n))))
                          (if (<= n 0) no-limit n))))
        (else (format (current-error-port)
                      "warning: bad --max-width=~a; using 80~%" s)
              80)))

;; Accepts N, inf, infinity, infinite, unlimited.  Zero or negative means no
;; limit: a search for zero matches is never what you want.
(define (parse-limit s)
  (cond ((member s '("inf" "infinity" "infinite" "unlimited")) no-limit)
        ((string->number s)
         => (lambda (n) (let ((n (inexact->exact (truncate n))))
                          (if (<= n 0) no-limit n))))
        (else (format (current-error-port)
                      "warning: bad --limit=~a; showing all matches~%" s)
              no-limit)))

;; Collapse runs of whitespace to single spaces so a paragraph fits one line.
(define (flatten s)
  (string-trim-both
   (regexp-substitute/global #f (make-regexp "[ \t\n\r]+") s 'pre " " 'post)))

(define (blank-before text pos)
  (let loop ((i (min pos (string-length text))))
    (cond ((<= i 0) 0)
          ((and (> i 1)
                (char=? (string-ref text (- i 1)) #\newline)
                (char=? (string-ref text (- i 2)) #\newline))
           i)
          (else (loop (- i 1))))))

(define (blank-after text pos)
  (let ((n (string-length text)))
    (let loop ((i pos))
      (cond ((>= i (- n 1)) n)
            ((and (char=? (string-ref text i) #\newline)
                  (char=? (string-ref text (+ i 1)) #\newline))
             (+ i 1))
            (else (loop (+ i 1)))))))

;; Paragraph around POS, flattened to one line.  At most MAX-WIDTH characters
;; unless MAX-WIDTH is no-limit.
(define (context text pos max-width)
  (let* ((n (string-length text))
         (lo0 (max (blank-before text pos) 0))
         (hi0 (min (blank-after text pos) n)))
    (if (>= max-width no-limit)
        (flatten (substring text lo0 hi0))
        (let* ((half (quotient max-width 2))
               (lo (max lo0 (- pos half)))
               (hi (min hi0 (max (+ pos 1) (+ lo max-width)))))
          (string-append
           (if (> lo lo0) "..." "")
           (flatten (substring text lo hi))
           (if (< hi hi0) "..." ""))))))

;;; ---- NAME section ------------------------------------------------------

;; Body of the section beginning at OFFSET: the paragraph after the heading
;; line, flattened to one line.
(define (section-text text offset)
  (let* ((n (string-length text))
         (nl (string-index text #\newline (min offset n)))
         (start (if nl (min (+ nl 1) n) n))
         (end (let loop ((i start))
                (cond ((>= i n) n)
                      ((and (char=? (string-ref text i) #\newline)
                            (< (+ i 1) n)
                            (char=? (string-ref text (+ i 1)) #\newline))
                       i)
                      (else (loop (+ i 1)))))))
    (flatten (substring text start end))))

;; "pthread_create - create a new thread" -> "create a new thread"
(define (whatis-after-hyphen s)
  (let ((i (first-index s " - " 0)))
    (if i (string-trim-both (substring s (+ i 3))) s)))

;;; ---- search ------------------------------------------------------------

(define symbol-chars
  (char-set-union char-set:letter+digit (string->char-set "_-/()")))

;; Bounds of the symbol containing POS, as (start . end).
(define (symbol-bounds text pos)
  (let* ((n (string-length text))
         (lo (let loop ((i (min pos n)))
               (if (and (> i 0)
                        (char-set-contains? symbol-chars (string-ref text (- i 1))))
                   (loop (- i 1))
                   i)))
         (hi (let loop ((i (min pos n)))
               (if (and (< i n)
                        (char-set-contains? symbol-chars (string-ref text i)))
                   (loop (+ i 1))
                   i))))
    (cons lo hi)))

;; Start of the first symbol strictly before POS, or 0.
(define (prev-symbol-start text pos)
  (let loop ((i (min pos (string-length text))))
    (cond ((<= i 0) 0)
          ((char-set-contains? symbol-chars (string-ref text (- i 1)))
           (car (symbol-bounds text (- i 1))))
          (else (loop (- i 1))))))

;; End of the first symbol strictly after POS, or the length of TEXT.
(define (next-symbol-end text pos)
  (let ((n (string-length text)))
    (let loop ((i (max pos 0)))
      (cond ((>= i n) n)
            ((char-set-contains? symbol-chars (string-ref text i))
             (cdr (symbol-bounds text i)))
            (else (loop (+ i 1)))))))

;; N symbols either side of the match, whitespace collapsed to one line.
(define (words-around text start end n)
  (let ((lo (let loop ((i start) (k n))
              (if (<= k 0) i (loop (prev-symbol-start text i) (- k 1)))))
        (hi (let loop ((i end) (k n))
              (if (<= k 0) i (loop (next-symbol-end text i) (- k 1))))))
    (flatten (substring text lo hi))))

;; First match of NEEDLE in TEXT at or after START, as (start . end), or #f.
;; string-contains-ci may return one or two values, so normalise.
(define (first-span text needle start)
  (call-with-values
      (lambda () (string-contains-ci text needle start))
    (lambda args
      (cond ((null? args) #f)
            ((not (car args)) #f)
            ((pair? (cdr args)) (cons (car args) (cadr args)))
            (else (cons (car args) (+ (car args) (string-length needle))))))))

(define (first-index text needle start)
  (let ((s (first-span text needle start))) (and s (car s))))

;; The first match of any query term at or after START, as a span, or #f.
;; Longer words are tried first, then shorter prefixes, so a stemmed match
;; still lands somewhere sensible.
(define (find-match text terms start)
  (any (lambda (w)
         (any (lambda (c) (first-span text c start))
              (word-candidates w)))
       terms))

;; Paragraph around POS, flattened to one line.  At most MAX-WIDTH characters
;; unless MAX-WIDTH is no-limit.
(define (paragraph text pos max-width)
  (let* ((n (string-length text))
         (lo0 (max (blank-before text pos) 0))
         (hi0 (min (blank-after text pos) n)))
    (if (>= max-width no-limit)
        (flatten (substring text lo0 hi0))
        (let* ((half (quotient max-width 2))
               (lo (max lo0 (- pos half)))
               (hi (min hi0 (max (+ pos 1) (+ lo max-width)))))
          (string-append
           (if (> lo lo0) "..." "")
           (flatten (substring text lo hi))
           (if (< hi hi0) "..." ""))))))

(define* (search dbpath querystring #:key (limit no-limit) (debug? #f)
                 (max-width 80) (point? #f) (file-path? #f) (context #f))
  (let* ((fields (section-fields dbpath))
         (prefixes (map (lambda (f) (cons f (prefix-for f))) fields))
         (boolean-prefixes '(("section" . "XSEC") ("has" . "XH")))
         (sname (stem-name dbpath))
         (terms (query-words querystring))
         (anchor (queried-section querystring fields))
         (section-only? (section-only-query? querystring)))
    (when debug?
      (format (current-error-port)
              "stemmer: ~a; ~a fields; query: ~s; terms: ~s; context: ~a; section-only: ~a~%"
              sname (length fields) querystring terms context section-only?))
    (call-with-database dbpath
      (lambda (db)
        (let* ((query (parse-query querystring
                                   #:stemmer (stemmer-for sname)
                                   #:prefixes prefixes
                                   #:boolean-prefixes boolean-prefixes))
               (mset (enquire-mset (enquire db query) #:maximum-items limit))
               (hits
                (reverse
                 (mset-fold
                  (lambda (item acc)
                    (let* ((record (call-with-input-string
                                       (document-data (mset-item-document item)) read))
                           (name (assoc-ref record 'name))
                           (section (assoc-ref record 'section))
                           (path (assoc-ref record 'path))
                           (sections (assoc-ref record 'sections))
                           (label (if file-path? path
                                      (format #f "~a(~a)" name section))))
                      (cons (list label sections path) acc)))
                  '()
                  mset)))
               (width (if (null? hits) 0
                          (apply max (map (lambda (h) (string-length (car h))) hits)))))
          (for-each
           (lambda (hit)
             (match-let (((label sections path) hit))
               (if section-only?
                   (let* ((text (render path))
                          (noff (assoc-ref sections "name"))
                          (ntext (if noff (section-text text noff) "")))
                     (format #t "~va: ~a~%" width label
                             (if file-path? ntext (whatis-after-hyphen ntext))))
                   (let* ((text (render path))
                          (start (or (and anchor (assoc-ref sections anchor)) 0))
                          (span (find-match text terms start))
                          (found (or (and span (car span)) start)))
                     (if point?
                         (format #t "~va ~a~%" width label found)
                         (format #t "~va: ~a~%" width label
                                 (if (and context span)
                                     (words-around text (car span) (cdr span) context)
                                     (paragraph text found max-width))))))))
           hits)
          (when debug?
            (format (current-error-port) "~a hits~%" (length hits))))))))

;;; ---- command line ------------------------------------------------------

;; Flags may appear anywhere in the argument list.
(define (split-flags args)
  (let loop ((as args) (debug? #f) (max-width 80) (point? #f) (file-path? #f)
             (context #f) (limit no-limit) (terms '()))
    (match as
      (() (values debug? max-width point? file-path? context limit (reverse terms)))
      ((a . rest)
       (cond ((string=? a "--debug")
              (loop rest #t max-width point? file-path? context limit terms))
             ((string=? a "--point")
              (loop rest debug? max-width #t file-path? context limit terms))
             ((string=? a "--file-path")
              (loop rest debug? max-width point? #t context limit terms))
             ((string=? a "--context")
              (loop rest debug? max-width point? file-path? 1 limit terms))
             ((string-match "^--context=(.*)$" a)
              => (lambda (m)
                   (let ((k (string->number (match:substring m 1))))
                     (loop rest debug? max-width point? file-path?
                           (if (and k (> k 0)) (inexact->exact (truncate k)) 1)
                           limit terms))))
             ((string-match "^--limit=(.*)$" a)
              => (lambda (m)
                   (loop rest debug? max-width point? file-path? context
                         (parse-limit (match:substring m 1)) terms)))
             ((string-match "^--max-width=(.*)$" a)
              => (lambda (m)
                   (loop rest debug? (parse-max-width (match:substring m 1))
                         point? file-path? context limit terms)))
             (else (loop rest debug? max-width point? file-path? context limit
                         (cons a terms))))))))

(let ((usage "Usage: ~a DBPATH [--debug] [--max-width=N] [--point] [--file-path] [--context[=N]] QUERY...~%"))
  (match (command-line)
    ((_ dbpath . rest)
     (if (not (file-exists? dbpath))
         (format (current-error-port)
                 "no database at ~a~%run `notman index` to build it"
                 dbpath)
         (call-with-values (lambda () (split-flags rest))
           (lambda (debug? max-width point? file-path? context limit terms)
             (if (null? terms)
                 (format (current-error-port)
                         usage
                         (car (command-line)))
                 (search dbpath (string-join terms)
                         #:debug? debug? #:max-width max-width
                         #:point? point? #:file-path? file-path?
                         #:context context #:limit limit))))))
    ((program . _)
     (format (current-error-port)
             usage
             program))))
