;; -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1"))

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'seq)
(require 's)

(when load-file-name
  (setq default-directory (file-name-directory load-file-name)))

(defvar d/titles/look-up-table (make-hash-table :test #'equal)
  "A look up table for titles.")

(defun d/titles/to-look-up-table (titles)
  "Turn TITLES, a sequence of strings, into a look up table."
  (let ((lut (make-hash-table :test #'equal
                              :size (length titles))))
    (seq-doseq (title titles)
      (puthash title t lut))
    lut))

(defun k/hash-update (table key fn &optional inexistence)
  "Update the value of KEY in TABLE with FN.

If KEY is not associated with anything, do nothing. This is
normally done by checking that the value is non-nil, but if nil
is a valid value in TABLE, pass a value guaranteed to not be in
TABLE as INEXISTENCE. This is equivalent to the DFLT argument of
`gethash'.

Writes into TABLE. Returns the new value associated with KEY."
  (declare (indent 2))
  (let ((v (gethash key table inexistence)))
    (unless (equal v inexistence)
      (puthash key
               (funcall fn v)
               table))))

(defun k/collect-pronunciations (het)
  "Collect pronunciations from heteronym object HET."
  (let ((ret nil))
    (dolist (key (list
                  ;; kemdict-data-ministry-of-education
                  "bopomofo" "pinyin"
                  ;; moedict-twblg
                  "trs"
                  ;; kisaragi-dict
                  "pronunciation"))
      (when-let (p (gethash key het))
        (dolist (p (k/normalize-pronunciation p))
          (push p ret))))
    ret))

(defun k/normalize-pronunciation (p)
  "Normalize pronunciation string P.

Return a list of normalized strings. This is because some
pronunciation strings include multiple pronunciations."
  (--> p
       (s-replace "　" " " it)
       (s-replace "（變）" "/" it)
       (s-split "/" it t)
       (-map #'s-trim it)))

(defun k/process-title (title)
  "Process TITLE to replace problematic characters, and so on."
  (->> title
       (replace-regexp-in-string "'" "’")
       (replace-regexp-in-string (rx "?") (rx "？"))))

(defun k/extract-development-version (word file output-path)
  "Read FILE and write its definition of WORD to OUTPUT-PATH.

The structure in FILE is preserved in OUTPUT-PATH.

This allows for not having to load everything when I'm only
iterating on one page.

Does nothing if OUTPUT-PATH already exists as a file."
  (declare (indent 1))
  (let (parsed)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (setq parsed (json-parse-buffer)))
    (with-temp-file output-path
      (insert
       (json-encode
        (vconcat
         (seq-filter
          (lambda (it)
            (equal word (gethash "title" it)))
          parsed)))))))

(when nil
  (k/extract-development-version "入"
    "ministry-of-education/dict_revised.json" "dev-dict_revised.json")
  (k/extract-development-version "出"
    "ministry-of-education/hakkadict.json" "dev-hakkadict.json")
  (k/extract-development-version "出"
    "moedict-data-twblg/dict-twblg.json" "dev-dict-twblg.json")
  (k/extract-development-version "出"
    "moedict-data-twblg/dict-twblg-ext.json" "dev-dict-twblg-ext.json")
  (k/extract-development-version "赫茲"
    "ministry-of-education/dict_concised.json" "dev-dict_concised.json")
  (k/extract-development-version "一枕南柯"
    "ministry-of-education/dict_idioms.json" "dev-dict_idioms.json"))

(defun k/parse-and-shape (&rest files)
  "Parse FILES and return a shaped version of it.

Parsed arrays from FILES are concatenated before shaping."
  (let ((shaped (make-hash-table :test #'equal))
        (raw-dict
         (with-temp-buffer
           (cl-loop for f in files
                    vconcat
                    (progn
                      (erase-buffer)
                      (insert-file-contents f)
                      (json-parse-buffer))))))
    ;; [{:title "title"
    ;;   :heteronyms (...)
    ;;   ... ...}
    ;;  ...]
    ;; -> {"title" {heteronyms (...)}
    ;;     ...}
    ;;
    ;; For entries without heteronyms:
    ;; [{:title "title"
    ;;   :definition "def"
    ;;   ... ...}
    ;;  ...]
    ;; -> {"title" {heteronyms [{definition "def" ...}]}
    ;;     ...}
    (cl-loop
     for entry being the elements of raw-dict
     do (let* ((title (k/process-title (gethash "title" entry)))
               ;; If the dictionary does not declare heteronyms in a
               ;; key, we set the heteronyms to a list with the
               ;; entry itself.
               (heteronyms (or (gethash "heteronyms" entry)
                               (vector entry)))
               (tmp (make-hash-table :test #'equal)))
          ;; If an entry with the title already exists, insert into
          ;; its heteronyms.
          (when-let (existing (gethash title shaped))
            (setq heteronyms
                  (vconcat (gethash "heteronyms" existing)
                           heteronyms)))
          ;; Sort the heteronyms according to the het_sort key.
          (when (and
                 ;; Skip checking the rest if the first already
                 ;; doesn't have it.
                 (gethash "het_sort" (elt heteronyms 0))
                 (seq-every-p (lambda (it) (gethash "het_sort" it))
                              heteronyms))
            (setq heteronyms
                  ;; This happens to work on vectors.
                  (--sort
                   (< (string-to-number
                       (gethash "het_sort" it))
                      (string-to-number
                       (gethash "het_sort" other)))
                   heteronyms)))
          (puthash "heteronyms" heteronyms tmp)
          (when-let (added (gethash "added" entry))
            (puthash "added" added tmp))
          (puthash title tmp shaped))
     finally return shaped)))

(defun d/process-def/dict_concised (def)
  "Process DEF for dict_concised."
  (->> def
       (s-replace-regexp (rx (or (seq bol (+ digit) ".")
                                 ;; This means "this definition has an image".
                                 "　◎"))
                         "")
       (s-replace-regexp (rx (group (or "△" "]" "、"))
                             (group (+ (not (any "、。")))))
                         (lambda (str)
                           (concat (match-string 1 str)
                                   (d/links/link-to-word (match-string 2 str)))))
       ;; These are the only types that exist.
       ;; ...plus CJK COMPATIBILITY IDEOGRAPH-F9B5. (Fixed in
       ;; upstream already, should be available next time concised
       ;; dict makes a data release.)
       (s-replace-regexp (rx "[" (group (any "例似反")) "]")
                         "<br><m>\\1</m>")
       (s-replace-regexp (rx "§" (group "英") (group (+ (any "a-zA-Z "))))
                         "<br><m>\\1</m>\\2")
       (s-replace "△" "<br><m title=\"參考詞\">△</m>")
       d/links/linkify-brackets))

(defun k/process-heteronym (het dict)
  "Process the heteronym object HET that belongs to DICT.

This is a separate step from shaping."
  (dolist (key (list "definition" "source_comment" "典故說明"))
    (k/hash-update het key
      #'d/links/linkify-brackets))
  (dolist (key (list "近義同" "近義反"))
    (k/hash-update het key
      #'d/links/comma-word-list))
  (k/hash-update het "word_ref"
    #'d/links/link-to-word)
  (pcase dict
    ("dict_concised" (k/hash-update het "definition"
                       #'d/process-def/dict_concised))
    ("dict_idioms" (k/hash-update het "definition"
                     (lambda (def)
                       ;; There is often an anchor at the end of
                       ;; dict_idioms definitions that's not
                       ;; displayed. Getting rid of it here allows
                       ;; shredding them from the database.
                       (s-replace-regexp "<a name.*" "" def)))))
  het)

(cl-defun d/links/link-to-word (target &optional (desc target))
  "Create an HTML link with DESC to TARGET when appropriate.

Just return TARGET if TARGET does not exist in `d/titles/look-up-table', or
if TARGET already looks like an HTML link."
  ;; This is /way/ faster than using `member' to test a list.
  (if (and (gethash target d/titles/look-up-table)
           (not (s-contains? "<a" target t)))
      (s-lex-format "<a href=\"/word/${target}\">${desc}</a>")
    target))

(defun d/links/linkify-brackets (str)
  "Create links in STR for all brackets."
  (when str
    (->> str
         (s-replace-regexp
          (rx (group (or "「" "【"))
              (group (*? any))
              (group (or "」" "】")))
          (lambda (str)
            (concat
             (match-string 1 str)
             (d/links/link-to-word
              (match-string 2 str))
             (match-string 3 str)))))))

(ert-deftest d/links/linkify-brackets ()
  (should
   (let ((d/titles/look-up-table
          (d/titles/to-look-up-table (list "a"))))
     (and (equal (d/links/linkify-brackets "「a」、「b」")
                 "「<a href=\"/word/a\">a</a>」、「b」")
          (equal (d/links/linkify-brackets "a, b")
                 "a, b")))))

(defun d/links/comma-word-list (str)
  "Add links to a string STR containing a comma-separated list of words."
  (->> (split-string str "[,、]" t)
       (-map #'d/links/link-to-word)
       (s-join "、")))

(ert-deftest d/links/comma-word-list ()
  (should
   (let ((d/titles/look-up-table
          (d/titles/to-look-up-table (list "敵意"))))
     (and (equal (d/links/comma-word-list "敵意、仇隙")
                 "<a href=\"/word/敵意\">敵意</a>、仇隙")
          (equal (d/links/comma-word-list "敵意,仇隙")
                 "<a href=\"/word/敵意\">敵意</a>、仇隙")
          (equal (d/links/comma-word-list "交情。")
                 "交情。")))))

(defun main ()
  (let* ((merged-result (make-hash-table :test #'equal))
         (dictionaries
          (if (and (or (not noninteractive)
                       (getenv "DEV"))
                   (-all? #'file-exists-p
                          '("dev-dict_revised.json"
                            "dev-dict-twblg.json"
                            "dev-dict-twblg-ext.json"
                            "dev-dict_concised.json"
                            "dev-dict_idioms.json"
                            "dev-hakkadict.json")))
              [("moedict_twblg" . ("dev-dict-twblg.json"
                                   "dev-dict-twblg-ext.json"))
               ("dict_revised" . "dev-dict_revised.json")
               ("dict_concised" . "dev-dict_concised.json")
               ("dict_idioms" . "dev-dict_idioms.json")
               ("hakkadict" . "dev-hakkadict.json")
               ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")]
            [("moedict_twblg" . ("moedict-data-twblg/dict-twblg.json"
                                 "moedict-data-twblg/dict-twblg-ext.json"))
             ("dict_revised" . "ministry-of-education/dict_revised.json")
             ("dict_concised" . "ministry-of-education/dict_concised.json")
             ("dict_idioms" . "ministry-of-education/dict_idioms.json")
             ("hakkadict" . "ministry-of-education/hakkadict.json")
             ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")]))
         (dict-count (length dictionaries))
         (shaped-dicts (make-vector dict-count nil))
         (het-number 0)
         (all-titles nil))
    (dotimes (i dict-count)
      (message "Parsing and shaping %s (%s/%s)..."
               (car (aref dictionaries i))
               (1+ i) dict-count)
      (let ((files (cdr (aref dictionaries i))))
        (when (stringp files)
          (setq files (list files)))
        (aset shaped-dicts i
              (apply #'k/parse-and-shape files))))
    (dotimes (i dict-count)
      (message "Collecting titles (%s/%s)..." (1+ i) dict-count)
      (cl-loop
       for k being the hash-keys of (aref shaped-dicts i)
       do (push k all-titles)))
    (message "Removing duplicate titles...")
    (setq d/titles/look-up-table (d/titles/to-look-up-table all-titles))
    (setq all-titles (hash-table-keys d/titles/look-up-table))
    (message "Merging...")
    (seq-doseq (title all-titles)
      (let ((entry (make-hash-table :test #'equal))
            (pronunciations nil))
        (puthash "title" title entry)
        (dotimes (i dict-count)
          (let ((dict-name (car (aref dictionaries i))))
            ;; each individual entry becomes the value of the main
            ;; entry, with the key being the dictionary name
            (when-let (idv-entry (gethash title (aref shaped-dicts i)))
              ;; collect pronunciations
              (let ((heteronyms (gethash "heteronyms" idv-entry)))
                (seq-doseq (het heteronyms)
                  (dolist (p (k/collect-pronunciations het))
                    (push p pronunciations))))
              ;; Process heteronyms
              (k/hash-update idv-entry "heteronyms"
                (lambda (heteronyms)
                  (--> heteronyms
                       (seq-map (lambda (het)
                                  (cl-incf het-number)
                                  (when (or (= het-number 1)
                                            (= 0 (% het-number 1000)))
                                    (message "Processing heteronym (#%s)"
                                             het-number))
                                  (k/process-heteronym het dict-name))
                                it)
                       (seq-into it 'vector))))
              ;; put the individual entry into the main entry
              (puthash dict-name idv-entry entry))))
        (puthash "pronunciations" pronunciations entry)
        (puthash title entry merged-result)))
    (message "Writing result out to disk...")
    (let ((json-encoding-pretty-print t))
      (with-temp-file "titles.json"
        (insert (json-encode (vconcat all-titles))))
      (with-temp-file "combined.json"
        (insert (json-encode merged-result))))
    (message "Done")))

(let ((comp (if (and (fboundp #'native-comp-available-p)
                     (native-comp-available-p))
                #'native-compile
              #'byte-compile)))
  (mapc comp (list #'main
                   #'d/links/comma-word-list
                   #'d/links/link-to-word
                   #'d/links/linkify-brackets
                   #'d/titles/to-look-up-table
                   #'k/hash-update
                   #'k/parse-and-shape
                   #'k/process-heteronym)))

(main)
(when noninteractive
  (kill-emacs))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
