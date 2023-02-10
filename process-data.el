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

(defvar d::ucs-NFC::buffer (get-buffer-create " process-data"))
(defun d::ucs-NFC (str)
  "Like `ucs-normalize-NFC-string' but keeps reusing the same temp buffer."
  (with-current-buffer d::ucs-NFC::buffer
    (erase-buffer)
    (insert str)
    (ucs-normalize-NFC-region
     (point-min) (point-max))
    (buffer-string)))

(defun d:latin-only (str)
  "Whether STR contains only characters from the latin and symbol scripts."
  (seq-every-p
   (lambda (ch)
     (memq (aref char-script-table ch)
           '(latin symbol)))
   str))

(defun d::dev:extract-development-version (word file output-path)
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
            (or (equal word (gethash "title" it))
                (equal word (gethash "poj" it))))
          parsed)))))))

(when nil
  (d::dev:extract-development-version "無妨"
    "ministry-of-education/dict_revised.json" "dev-dict_revised.json")
  (d::dev:extract-development-version "無妨"
    "ministry-of-education/hakkadict.json" "dev-hakkadict.json")
  (d::dev:extract-development-version "無妨"
    "moedict-data-twblg/dict-twblg.json" "dev-dict-twblg.json")
  (d::dev:extract-development-version "無妨"
    "moedict-data-twblg/dict-twblg-ext.json" "dev-dict-twblg-ext.json")
  (d::dev:extract-development-version "無妨"
    "ministry-of-education/dict_concised.json" "dev-dict_concised.json")
  (d::dev:extract-development-version "一枕南柯"
    "ministry-of-education/dict_idioms.json" "dev-dict_idioms.json")
  (d::dev:extract-development-version "無妨"
    "chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json" "dev-chhoetaigi-itaigi.json")
  (d::dev:extract-development-version "無妨"
    "chhoetaigi/ChhoeTaigi_TaijitToaSutian.json" "dev-chhoetaigi-taijittoasutian.json")
  (d::dev:extract-development-version "bûn-ha̍k"
    "chhoetaigi/ChhoeTaigi_TaioanPehoeKichhooGiku.json" "dev-chhoetaigi-taioanpehoekichhoogiku.json"))

(defvar d:titles:look-up-table (make-hash-table :test #'equal)
  "A look up table for titles.")

(defun d:titles:to-look-up-table (titles)
  "Turn TITLES, a sequence of strings, into a look up table."
  (let ((lut (make-hash-table :test #'equal
                              :size (length titles))))
    (seq-doseq (title titles)
      (puthash title t lut))
    lut))

(defvar d:links:from nil
  "Used to mark where links are coming from to register to the links table.")

(defvar d:links nil
  "The links alist.")

(cl-defun d:links:link-to-word (target
                                &key
                                (desc target)
                                (href target))
  "Create an HTML link with DESC to TARGET when appropriate.

Just return DESC if TARGET does not exist in
`d:titles:look-up-table', or if DESC already looks like an HTML
link.

If HREF is a string, the HTML will link to HREF instead. This is
useful for creating links to \"/word/word#heading\" while still
only checking for the word's existence with \"word\". Use it like
this:

  (d:links:link-to-word \"word\" :href \"word#heading\")"
  (if (or (equal target d:links:from)
          ;; This is /way/ faster than using `member' to test a list.
          (not (gethash target d:titles:look-up-table))
          (s-contains? "<a" desc t))
      desc
    (when d:links:from
      (push `((from . ,d:links:from)
              (to . ,target))
            d:links))
    (s-lex-format "<a href=\"/word/${href}\">${desc}</a>")))

(defun d:links:linkify-first-phrase (str)
  "Try to create a link for the first phrase in STR."
  (when str
    (->> str
         (s-replace-regexp
          (rx bos
              (group (+? any))
              (group (or "，" "。")))
          (lambda (str)
            (concat
             (d:links:link-to-word
              (match-string 1 str))
             (match-string 2 str)))))))

(ert-deftest d:links:linkify-first-phrase ()
  (should
   (let ((d:titles:look-up-table
          (d:titles:to-look-up-table (list "a"))))
     (and (equal (d:links:linkify-first-phrase "a。")
                 "<a href=\"/word/a\">a</a>。")
          (equal (d:links:linkify-brackets "b。")
                 "b。")))))

(defun d:links:linkify-brackets (str &optional open close)
  "Create links in STR for all brackets."
  (when str
    (->> str
         (s-replace-regexp
          ;; Presumably this is faster. Haven't timed it though.
          (if (and open close)
              (rx-to-string
               `(seq (group (any ,open))
                     (group (*? any))
                     (group (any ,close)))
               :no-group)
            (rx (group (any "「【"))
                (group (*? any))
                (group (any "」】"))))
          (lambda (str)
            (concat
             (match-string 1 str)
             (d:links:link-to-word
              (match-string 2 str))
             (match-string 3 str)))))))

(ert-deftest d:links:linkify-brackets ()
  (should
   (let ((d:titles:look-up-table
          (d:titles:to-look-up-table (list "a"))))
     (and (equal (d:links:linkify-brackets "「a」、「b」")
                 "「<a href=\"/word/a\">a</a>」、「b」")
          (equal (d:links:linkify-brackets "a, b")
                 "a, b")))))

(defun d:links:org-style (str)
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
                (d:links:link-to-word (car (s-split "#" target))
                                      :desc description
                                      :href target)))))))))

(ert-deftest d:links:org-style ()
  (let ((d:titles:look-up-table
         (d:titles:to-look-up-table (list "嗨"))))
    (should
     (equal (d:links:org-style "精神很好的樣子。同「[[嗨#kisaragi_dict][嗨]]」。")
            "精神很好的樣子。同「<a href=\"/word/嗨#kisaragi_dict\">嗨</a>」。"))
    (should
     (equal (d:links:org-style "hello")
            "hello"))))

(defun d:links:comma-word-list (str)
  "Add links to a string STR containing a comma-separated list of words."
  (->> (split-string str "[,、]" t)
       (-map #'d:links:link-to-word)
       (s-join "、")))

(ert-deftest d:links:comma-word-list ()
  (should
   (let ((d:titles:look-up-table
          (d:titles:to-look-up-table (list "敵意"))))
     (and (equal (d:links:comma-word-list "敵意、仇隙")
                 "<a href=\"/word/敵意\">敵意</a>、仇隙")
          (equal (d:links:comma-word-list "敵意,仇隙")
                 "<a href=\"/word/敵意\">敵意</a>、仇隙")
          (equal (d:links:comma-word-list "交情。")
                 "交情。")))))

(defconst d::pn-keys
  (list
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

(defun d:pn-collect (het &optional table?)
  "Collect pronunciations from heteronym object HET.
When TABLE? is non-nil, return a hash table mapping the
pronunciation key to the corresponding value."
  (let ((ret))
    (dolist (key d::pn-keys)
      (when-let (p (gethash key het))
        (dolist (p (d:pn-normalize p))
          (push (cons key p)
                ret))))
    (if table? ret (mapcar #'cdr ret))))

(defun d:pn-normalize (p &optional one)
  "Normalize pronunciation string P.

Return a list of normalized strings. This is because some
pronunciation strings include multiple pronunciations. If ONE is
non-nil, don't do pronunciation splitting and return a string
instead."
  (if one
      (->> p
           d::ucs-NFC
           (s-replace "　" " ")
           s-trim)
    (->> p
         d::ucs-NFC
         (s-replace "　" " ")
         (s-replace "（變）" "/")
         (s-split "/")
         (-map #'s-trim)
         (remove ""))))

(defun d:process-title (title)
  "Process TITLE to replace problematic characters, and so on.
If TITLE is empty, return nil so the call site can decide what to
do."
  (let ((title (s-trim title)))
    (unless (equal "" title)
      (->> title
           (replace-regexp-in-string "'"
                                     "’")
           (replace-regexp-in-string (rx "?")
                                     "？")
           ;; Work around chhoetaigi_taijittoasutian entries like
           ;; "(**裝)模做樣". I don't think the title is supposed to
           ;; be like that.
           (replace-regexp-in-string (rx "(**" (group (+ any)) ")")
                                     "\\1")
           (replace-regexp-in-string (rx (any "[" "]"))
                                     "")))))

(defun d:process-def:dict_concised (def)
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
                                   (d:links:link-to-word (match-string 2 str)))))
       ;; These are the only types that exist.
       (s-replace-regexp (rx "[" (group (any "例似反")) "]")
                         "<br><m>\\1</m> ")
       (s-replace-regexp (rx "§" (group "英") (group (+ (any "a-zA-Z "))))
                         "<br><m>\\1</m> \\2")
       (s-replace-regexp (rx (opt "　") "△")
                         "<br><m title=\"參考詞\">△</m> ")
       d:links:linkify-brackets))

(defun d:process-props (props title dict)
  "Process the heteronym props object PROPS.

PROPS describes TITLE and belongs to DICT. These are needed for
some processing.

This is a separate step from shaping."
  (let ((d:links:from title))
    (dolist (key (list "definition" "source_comment" "典故說明"))
      (d::hash-update props key
        #'d:links:linkify-brackets))
    (dolist (key (list "trs" "poj" "kip"))
      (d::hash-update props key
        (lambda (pn)
          (d:pn-normalize pn :one))))
    (dolist (key (list "近義同" "近義反"))
      (d::hash-update props key
        #'d:links:comma-word-list))
    (d::hash-update props "word_ref"
      #'d:links:link-to-word)
    (d::hash-update props "definitions"
      (lambda (defs)
        (seq-doseq (def defs)
          (d::hash-update def "quote"
            #'d::ucs-NFC)
          (d::hash-update def "example"
            (lambda (v)
              (cond
               ((stringp v)
                (d::ucs-NFC v))
               ((seqp v)
                (seq-map #'d::ucs-NFC v))
               (t (error "%s: het.props.definitions.example is neither a string or a sequence"
                         title)))))
          (d::hash-update def "def"
            (-compose
             #'d:links:linkify-brackets
             #'d:links:linkify-first-phrase)))))
    (pcase dict
      ("kisaragi_dict"
       (d::hash-update props "definitions"
         (lambda (defs)
           (seq-doseq (def defs)
             (d::hash-update def "def"
               (lambda (d)
                 (->> d
                      (s-replace "#+begin_quote" "<blockquote>")
                      (s-replace "#+end_quote" "</blockquote>")
                      ;; No need to apply linkify-brackets again
                      d:links:org-style)))))))
      ("chhoetaigi_itaigi"
       (d::hash-update props "definition"
         #'d:links:link-to-word))
      ("chhoetaigi_taioanpehoekichhoogiku"
       ;; Ensure the entry title and the props title are the same
       (unless (equal title (gethash "title" props))
         (d::hash-update props "title"
           (lambda (_title)
             title))))
      ("chhoetaigi_taijittoasutian"
       ;; Ensure the entry title and the props title are the same
       (unless (equal title (gethash "title" props))
         (d::hash-update props "title"
           (lambda (_title)
             title)))
       (d::hash-update props "definition"
         (lambda (def)
           (d:links:linkify-brackets def "[" "]")))
       (d::hash-update props "example"
         ;; This makes it more readable. Is it a good idea though?
         ;; Before: "An ~ is red." (in page "apple")
         ;; After: "An apple is red."
         (lambda (str)
           (s-replace-regexp (rx (opt " ")
                                 ;; this is #x223c, TILDE OPERATOR,
                                 ;; not "~", TILDE.
                                 (repeat 1 4 "∼")
                                 (opt " "))
                             d:links:from
                             str))))
      ("dict_concised"
       (d::hash-update props "definition"
         #'d:process-def:dict_concised))
      ("dict_idioms"
       (d::hash-update props "definition"
         (lambda (def)
           ;; There is often an anchor at the end of
           ;; dict_idioms definitions that's not
           ;; displayed. Getting rid of it here allows
           ;; shredding them from the database.
           (s-replace-regexp "<a name.*" "" def)))))
    props))

(defun d::dictionaries (&optional dev?)
  "Return definitions of dictionaries.

The value is a vector:
    [(ID . FILE) ; either this form
     (ID . (FILE1 FILE2 ...)) ; or this form
     ...]

Development versions are returned when applicable if DEV? is
non-nil."
  (if (and dev?
           (-all? #'file-exists-p
                  '("dev-dict_revised.json"
                    "dev-dict-twblg.json"
                    "dev-dict-twblg-ext.json"
                    "dev-dict_concised.json"
                    "dev-dict_idioms.json"
                    "dev-hakkadict.json"
                    "dev-chhoetaigi-itaigi.json"
                    "dev-chhoetaigi-taijittoasutian.json"
                    "dev-chhoetaigi-taioanpehoekichhoogiku.json")))
      [
       ("dict_idioms" . "dev-dict_idioms.json")
       ("hakkadict" . "dev-hakkadict.json")
       ("chhoetaigi_taioanpehoekichhoogiku" . "dev-chhoetaigi-taioanpehoekichhoogiku.json")
       ("chhoetaigi_itaigi" . "dev-chhoetaigi-itaigi.json")
       ("moedict_twblg" . ("dev-dict-twblg.json"
                           "dev-dict-twblg-ext.json"))
       ("chhoetaigi_taijittoasutian" . "dev-chhoetaigi-taijittoasutian.json")
       ("dict_revised" . "dev-dict_revised.json")
       ("dict_concised" . "dev-dict_concised.json")
       ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")]
    ;; The order here, reversed, defines the order they will appear in
    ;; the word pages.
    [("dict_idioms" . "ministry-of-education/dict_idioms.json")
     ("hakkadict" . "ministry-of-education/hakkadict.json")
     ("chhoetaigi_taioanpehoekichhoogiku" . "chhoetaigi/ChhoeTaigi_TaioanPehoeKichhooGiku.json")
     ("chhoetaigi_itaigi" . "chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json")
     ("moedict_twblg" . ("moedict-data-twblg/dict-twblg.json"
                         "moedict-data-twblg/dict-twblg-ext.json"))
     ("chhoetaigi_taijittoasutian" . "chhoetaigi/ChhoeTaigi_TaijitToaSutian.json")
     ("dict_revised" . "ministry-of-education/dict_revised.json")
     ("dict_concised" . "ministry-of-education/dict_concised.json")
     ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")]))

;; For entries with heteronyms:
;;   [{:title "title"
;;     :heteronyms (...)
;;     ... ...}
;;    ...]
;;   ->
;;   [{:title "title"
;;     :from "dictA"
;;     :pns {...}
;;     :props ...}])
;; Also works for entries that are themselves heteronyms
(defun d:parse-and-shape (dict files)
  "Return (heteronyms . titles) in FILES.
DICT is the dictionary ID to associate with them."
  (let* ((files (-list files))
         (raw-dict (with-temp-buffer
                     (cl-loop for f in files
                              vconcat
                              (progn
                                (erase-buffer)
                                (insert-file-contents f)
                                (json-parse-buffer)))))
         (heteronyms nil)
         (titles nil))
    (seq-doseq (entry raw-dict)
      (let ((orig-hets (or (gethash "heteronyms" entry)
                           (vector entry))))
        ;; Sort them according to the "het_sort" key, or if that's not
        ;; present, the "id" key.
        ;; TODO: maybe we want IDs in the DB as well
        ;; idea: add 1000000 to IDs from the first dict, 2000000 to
        ;; IDs from the second, and so on, plus ensuring we don't
        ;; exceed the maximum allowed word count in this scheme
        (when (or (seq-every-p (-partial #'gethash "het_sort")
                               orig-hets)
                  (seq-every-p (-partial #'gethash "id")
                               orig-hets))
          (setq orig-hets (--sort
                           (< (string-to-number
                               (or (gethash "het_sort" it)
                                   (gethash "id" it)))
                              (string-to-number
                               (or (gethash "het_sort" other)
                                   (gethash "id" other))))
                           orig-hets)))
        (seq-doseq (orig-het orig-hets)
          (let* ((shaped-het (make-hash-table :test #'equal))
                 ;; title can be empty. We'll read from .poj just below.
                 (title (-some-> (gethash "title" entry)
                          d:process-title)))
            ;; - work around some entries in chhoetaigi_taijittoasutian
            ;;   having empty titles
            ;; - also allows working with chhoetaigi_taioanpehoekichhoogiku
            (unless title
              (setq title (gethash "poj" orig-het)))
            ;; chhoetaigi_taijittoasutian:
            ;; work around some incorrectly formatted titles, like
            ;; "a-a cham-cham" being formatted as "a-acham-cham" in
            ;; the title field
            (when (and (equal dict "chhoetaigi_taijittoasutian")
                       ;; This means we know it's safe to substitude het.poj
                       (d:latin-only title)
                       (not (equal (downcase title)
                                   (downcase (gethash "poj" orig-het)))))
              (setq title (gethash "poj" orig-het)))
            (puthash "title" title shaped-het)
            (puthash "from" dict shaped-het)
            ;; We can't run d:process-props just yet, as that requires
            ;; the list of all titles to work correctly.
            (puthash "props" orig-het shaped-het)
            (dolist (extra-prop (list "added" "vogue"))
              ;; These props can be added on the heteronym or on the
              ;; entire word.
              (when-let (v (or (gethash extra-prop entry)
                               (gethash extra-prop orig-het)))
                (puthash extra-prop v shaped-het)))
            (puthash "pns"
                     (-uniq (d:pn-collect orig-het))
                     shaped-het)
            (push shaped-het heteronyms)
            (push title titles)))))
    (cons heteronyms titles)))

(defun d:main ()
  (setq d:links nil)
  (setq d:titles:look-up-table nil)
  (let* ((dictionaries
          (d::dictionaries (or (not noninteractive)
                               (getenv "DEV"))))
         (dict-count (length dictionaries))
         (heteronyms nil)
         (all-titles nil))
    ;; Step 1
    (cl-loop
     for (dict . files) being the elements of dictionaries
     using (index i)
     do
     (progn
       (message "Collecting heteronyms and titles from %s (%s/%s)..."
                dict (1+ i) dict-count)
       (let ((result (d:parse-and-shape dict files)))
         (cl-loop
          for het in (car result)
          do (push het heteronyms))
         ;; Collect titles
         (cl-loop
          for k in (cdr result)
          do (push k all-titles)))))
    ;; Step 2
    (message "Removing duplicate titles...")
    (setq d:titles:look-up-table (d:titles:to-look-up-table all-titles))
    (setq all-titles (hash-table-keys d:titles:look-up-table))
    ;; Step 3
    (cl-loop
     for het being the elements of heteronyms
     using (index i)
     do
     (progn
       (when (or (= (1+ i) 1)
                 (= 0 (% (1+ i) 10000)))
         (message "Processing heteronyms (#%s)..." (1+ i)))
       (d::hash-update het "props"
         (lambda (props)
           (d:process-props
            props
            (gethash "title" het)
            (gethash "from" het))))))
    ;; Step 4
    (message "Writing result out to disk...")
    (let ((json-encoding-pretty-print t)
          ;; This tells `json-encode' to use the same false as
          ;; `json-parse-buffer''s default, because there are false
          ;; values from there.
          ;;
          ;; I'm using nil as null on the other hand.
          (json-false :false))
      (setq d:links (-uniq d:links))
      (with-temp-file "links.json"
        (insert (json-encode d:links)))
      (with-temp-file "heteronyms.json"
        (insert (json-encode heteronyms))))
    (message "Done")))

(let ((comp (if (and (fboundp #'native-comp-available-p)
                     (native-comp-available-p))
                #'native-compile
              #'byte-compile)))
  (-each (list #'d:main
               #'d:links:comma-word-list
               #'d:links:link-to-word
               #'d:links:linkify-brackets
               #'d:titles:to-look-up-table
               #'d::hash-update
               #'d:process-props)
    comp))

(d:main)

(when noninteractive
  (kill-emacs))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp-package)
;; End:
