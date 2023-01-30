;; -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1"))

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'seq)
(require 's)
(require 'ol)

(when load-file-name
  (setq default-directory (file-name-directory load-file-name)))

(defun d/extract-development-version (word file output-path)
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
  (d/extract-development-version "入"
    "ministry-of-education/dict_revised.json" "dev-dict_revised.json")
  (d/extract-development-version "出"
    "ministry-of-education/hakkadict.json" "dev-hakkadict.json")
  (d/extract-development-version "㨻魚"
    "moedict-data-twblg/dict-twblg.json" "dev-dict-twblg.json")
  (d/extract-development-version "無妨"
    "moedict-data-twblg/dict-twblg-ext.json" "dev-dict-twblg-ext.json")
  (d/extract-development-version "赫茲"
    "ministry-of-education/dict_concised.json" "dev-dict_concised.json")
  (d/extract-development-version "一枕南柯"
    "ministry-of-education/dict_idioms.json" "dev-dict_idioms.json")
  (d/extract-development-version "白漆"
    "chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json" "dev-chhoetaigi-itaigi.json")
  (d/extract-development-version "無妨"
    "chhoetaigi/ChhoeTaigi_TaijitToaSutian.json" "dev-chhoetaigi-taijittoasutian.json"))

(defvar d/titles/look-up-table (make-hash-table :test #'equal)
  "A look up table for titles.")

(defun d/titles/to-look-up-table (titles)
  "Turn TITLES, a sequence of strings, into a look up table."
  (let ((lut (make-hash-table :test #'equal
                              :size (length titles))))
    (seq-doseq (title titles)
      (puthash title t lut))
    lut))

(defvar d/links/from nil
  "Used to mark where links are coming from to register to the links table.")

(defvar d/links nil
  "The links alist.")

(cl-defun d/links/link-to-word (target
                                &key
                                (desc target)
                                (href target))
  "Create an HTML link with DESC to TARGET when appropriate.

Just return DESC if TARGET does not exist in
`d/titles/look-up-table', or if DESC already looks like an HTML
link.

If HREF is a string, the HTML will link to HREF instead. This is
useful for creating links to \"/word/word#heading\" while still
only checking for the word's existence with \"word\". Use it like
this:

  (d/links/link-to-word \"word\" :href \"word#heading\")"
  (if (or (equal target d/links/from)
          ;; This is /way/ faster than using `member' to test a list.
          (not (gethash target d/titles/look-up-table))
          (s-contains? "<a" desc t))
      desc
    (when d/links/from
      (push `((from . ,d/links/from)
              (to . ,target))
            d/links))
    (s-lex-format "<a href=\"/word/${href}\">${desc}</a>")))

(defun d/links/linkify-first-phrase (str)
  "Try to create a link for the first phrase in STR."
  (when str
    (->> str
         (s-replace-regexp
          (rx bos
              (group (+? any))
              (group (or "，" "。")))
          (lambda (str)
            (concat
             (d/links/link-to-word
              (match-string 1 str))
             (match-string 2 str)))))))

(ert-deftest d/links/linkify-first-phrase ()
  (should
   (let ((d/titles/look-up-table
          (d/titles/to-look-up-table (list "a"))))
     (and (equal (d/links/linkify-first-phrase "a。")
                 "<a href=\"/word/a\">a</a>。")
          (equal (d/links/linkify-brackets "b。")
                 "b。")))))

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

(defun d/links/org-style (str)
  "Linkify Org-style links in STR."
  (when str
    (->> str
         (s-replace-regexp
          org-link-bracket-re
          (lambda (str)
            (let* ((target (match-string 1 str))
                   (description (or (match-string 2 str)
                                    target)))
              (cond
               ((s-matches? (rx bos (or "//" "https://" "http://"))
                            target)
                (format "<a href=\"%s\">%s</a>" target description))
               (t
                (d/links/link-to-word (car (s-split "#" target))
                                      :desc description
                                      :href target)))))))))

(ert-deftest d/links/org-style ()
  (let ((d/titles/look-up-table
         (d/titles/to-look-up-table (list "嗨"))))
    (should
     (equal (d/links/org-style "精神很好的樣子。同「[[嗨#kisaragi_dict][嗨]]」。")
            "精神很好的樣子。同「<a href=\"/word/嗨#kisaragi_dict\">嗨</a>」。"))
    (should
     (equal (d/links/org-style "hello")
            "hello"))))

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

(defun d::hash-update (table key fn &optional inexistence)
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

(defun d/pn-collect (het)
  "Collect pronunciations from heteronym object HET."
  (let ((ret nil))
    (dolist (key (list
                  ;; kemdict-data-ministry-of-education
                  "bopomofo" "pinyin"
                  ;; moedict-twblg
                  "trs"
                  ;; kisaragi-dict
                  "pronunciation"
                  ;; hakkadict
                  "p_四縣" "p_海陸" "p_大埔" "p_饒平" "p_詔安" "p_南四縣"
                  ;; chhoetaigi-itaigi (keys are defined in Makefile
                  ;; in this repository)
                  "poj" "kip"))
      (when-let (p (gethash key het))
        (dolist (p (d/pn-normalize p))
          (push p ret))))
    ret))

(defvar d/pn-normalize/buffer (get-buffer-create " process-data"))
(defun d/pn-normalize (p &optional one)
  "Normalize pronunciation string P.

Return a list of normalized strings. This is because some
pronunciation strings include multiple pronunciations. If ONE is
non-nil, don't do pronunciation splitting and return a string
instead."
  (with-current-buffer d/pn-normalize/buffer
    (erase-buffer)
    (insert p)
    (ucs-normalize-NFC-region
     (point-min) (point-max))
    (if one
        (->> (buffer-string)
             (s-replace "　" " ")
             s-trim)
      (->> (buffer-string)
           (s-replace "　" " ")
           (s-replace "（變）" "/")
           (s-split "/")
           (-map #'s-trim)
           (remove "")))))

(defun d/parse-and-shape (&rest files)
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
     do (let* ((title (d/process-title (gethash "title" entry)))
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
          ;; TODO: sort with the "id" key if het_sort isn't present
          (when (and
                 ;; Skip checking the rest if the first already
                 ;; doesn't have it.
                 (gethash "het_sort" (elt heteronyms 0))
                 ;; FIXME: do we actually need to check this? The data
                 ;; we're working with should be well-formed enough.
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
          (dolist (extra-entry-prop (list "added" "vogue"))
            (when-let (v (gethash extra-entry-prop entry))
              (puthash extra-entry-prop v tmp)))
          (puthash title tmp shaped))
     finally return shaped)))

(defun d/process-title (title)
  "Process TITLE to replace problematic characters, and so on."
  (->> title
       (replace-regexp-in-string "'" "’")
       (replace-regexp-in-string (rx "?") (rx "？"))))

(defun d/process-def/dict_concised (def)
  "Process DEF for dict_concised."
  (->> def
       (s-replace "。。" "。")
       ;; A "例" was CJK COMPATIBILITY IDEOGRAPH-F9B5. This has
       ;; already been fixed in upstream, and should be available next
       ;; time concised dict makes a data release.
       (s-replace "例" "例")
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
       (s-replace-regexp (rx "[" (group (any "例似反")) "]")
                         "<br><m>\\1</m> ")
       (s-replace-regexp (rx "§" (group "英") (group (+ (any "a-zA-Z "))))
                         "<br><m>\\1</m> \\2")
       (s-replace-regexp (rx (opt "　") "△")
                         "<br><m title=\"參考詞\">△</m> ")
       d/links/linkify-brackets))

(defun d/process-heteronym (het title dict)
  "Process the heteronym object HET.

HET describes TITLE and belongs to DICT. These are needed for
some processing.

This is a separate step from shaping."
  (let ((d/links/from title))
    (dolist (key (list "definition" "source_comment" "典故說明"))
      (d::hash-update het key
        #'d/links/linkify-brackets))
    (dolist (key (list "trs" "poj" "kip"))
      (d::hash-update het key
        (lambda (pn)
          (d/pn-normalize pn :one))))
    (dolist (key (list "近義同" "近義反"))
      (d::hash-update het key
        #'d/links/comma-word-list))
    (d::hash-update het "word_ref"
      #'d/links/link-to-word)
    (d::hash-update het "definitions"
      (lambda (defs)
        (seq-doseq (def defs)
          (d::hash-update def "def"
            (-compose
             #'d/links/linkify-brackets
             #'d/links/linkify-first-phrase)))))
    (pcase dict
      ("kisaragi_dict"
       (d::hash-update het "definitions"
         (lambda (defs)
           (seq-doseq (def defs)
             (d::hash-update def "def"
               (lambda (d)
                 (->> d
                      (s-replace "#+begin_quote" "<blockquote>")
                      (s-replace "#+end_quote" "</blockquote>")
                      ;; No need to apply linkify-brackets again
                      d/links/org-style)))))))
      ("chhoetaigi_itaigi"
       (d::hash-update het "definition"
         #'d/links/link-to-word))
      ("chhoetaigi_taijittoasutian"
       (d::hash-update het "example"
         ;; This makes it more readable. Is it a good idea though?
         ;; Before: "An ~ is red." (in page "apple")
         ;; After: "An apple is red."
         (lambda (str)
           (s-replace-regexp (rx (opt " ")
                                 ;; this is #x223c, TILDE OPERATOR,
                                 ;; not "~", TILDE.
                                 (repeat 1 4 "∼")
                                 (opt " "))
                             d/links/from
                             str))))
      ("dict_concised"
       (d::hash-update het "definition"
         #'d/process-def/dict_concised))
      ("dict_idioms"
       (d::hash-update het "definition"
         (lambda (def)
           ;; There is often an anchor at the end of
           ;; dict_idioms definitions that's not
           ;; displayed. Getting rid of it here allows
           ;; shredding them from the database.
           (s-replace-regexp "<a name.*" "" def)))))
    het))

(defun main ()
  (setq d/links nil)
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
                            "dev-hakkadict.json"
                            "dev-chhoetaigi-itaigi.json")))
              [("moedict_twblg" . ("dev-dict-twblg.json"
                                   "dev-dict-twblg-ext.json"))
               ("chhoetaigi_itaigi" . "dev-chhoetaigi-itaigi.json")
               ("chhoetaigi_taijittoasutian" . "dev-chhoetaigi-taijittoasutian.json")
               ("dict_revised" . "dev-dict_revised.json")
               ("dict_concised" . "dev-dict_concised.json")
               ("dict_idioms" . "dev-dict_idioms.json")
               ("hakkadict" . "dev-hakkadict.json")
               ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")]
            [("moedict_twblg" . ("moedict-data-twblg/dict-twblg.json"
                                 "moedict-data-twblg/dict-twblg-ext.json"))
             ("chhoetaigi_itaigi" . "chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json")
             ("chhoetaigi_taijittoasutian" . "chhoetaigi/ChhoeTaigi_TaijitToaSutian.json")
             ("dict_revised" . "ministry-of-education/dict_revised.json")
             ("dict_concised" . "ministry-of-education/dict_concised.json")
             ("dict_idioms" . "ministry-of-education/dict_idioms.json")
             ("hakkadict" . "ministry-of-education/hakkadict.json")
             ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")]))
         (dict-count (length dictionaries))
         (shaped-dicts (make-vector dict-count nil))
         (het-number 0)
         (all-titles nil))
    (cl-loop
     for (dict . files) being the elements of dictionaries
     using (index i)
     do
     (progn
       (message "Parsing and shaping %s (%s/%s)..."
                dict (1+ i) dict-count)
       (let* ((files (if (stringp files)
                         (list files)
                       files))
              (shaped (apply #'d/parse-and-shape files)))
         ;; Collect titles
         (cl-loop
          for k being the hash-keys of shaped
          do (push k all-titles))
         (aset shaped-dicts i shaped))))
    (message "Removing duplicate titles...")
    (setq d/titles/look-up-table (d/titles/to-look-up-table all-titles))
    (setq all-titles (hash-table-keys d/titles/look-up-table))
    (message "Merging...")
    (seq-doseq (title all-titles)
      (let ((entry (make-hash-table :test #'equal))
            (pronunciations nil))
        (puthash "title" title entry)
        (cl-loop
         for shaped being the elements of shaped-dicts
         using (index i)
         do
         (let ((dict-name (car (aref dictionaries i))))
           ;; each individual entry becomes the value of the main
           ;; entry, with the key being the dictionary name
           (when-let (idv-entry (gethash title shaped))
             ;; collect pronunciations
             (let ((heteronyms (gethash "heteronyms" idv-entry)))
               (seq-doseq (het heteronyms)
                 (dolist (pn (d/pn-collect het))
                   (push pn pronunciations))))
             ;; Process heteronyms
             (d::hash-update idv-entry "heteronyms"
               (lambda (heteronyms)
                 (--> heteronyms
                      (seq-map (lambda (het)
                                 (cl-incf het-number)
                                 (when (or (= het-number 1)
                                           (= 0 (% het-number 10000)))
                                   (message "Processing heteronym (#%s)"
                                            het-number))
                                 (d/process-heteronym het title dict-name))
                               it)
                      (seq-into it 'vector))))
             ;; put the individual entry into the main entry
             (puthash dict-name idv-entry entry))))
        (puthash "pronunciations" (-uniq pronunciations) entry)
        (puthash title entry merged-result)))
    (message "Writing result out to disk...")
    (let ((json-encoding-pretty-print (not noninteractive))
          ;; This tells `json-encode' to use the same false as
          ;; `json-parse-buffer''s default, because there are false
          ;; values from there.
          ;;
          ;; I'm using nil as null on the other hand.
          (json-false :false))
      (with-temp-file "links.json"
        (insert (json-encode d/links)))
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
                   #'d::hash-update
                   #'d/parse-and-shape
                   #'d/process-heteronym)))

(main)
(when noninteractive
  (kill-emacs))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
