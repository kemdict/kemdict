;; -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1"))

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'seq)
(require 's)
(require 'ol)
(require 'ht)

(when load-file-name
  (setq default-directory (file-name-directory load-file-name)))

(put 'ht-update-with! 'lisp-indent-function 2)

(defun d::hash-prune (table value)
  "Remove all entries in TABLE that are associated with VALUE."
  (cl-loop for k in (hash-table-keys table)
           when (equal value (gethash k table))
           do (remhash k table))
  table)

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
  (let
      ;; HACK: some phrases contain a period. Treat would-be
      ;; references to it without a period as actual references.
      ((period nil))
    (if (or (equal target d:links:from)
            ;; This is /way/ faster than using `member' to test a list.
            (not (or (gethash target d:titles:look-up-table)
                     (and (gethash (concat target "。") d:titles:look-up-table)
                          (setq period t))))
            (s-contains? "<a" desc t))
        desc
      (when period
        (setq target (concat target "。")
              href (concat href "。")))
      (when d:links:from
        (push `((from . ,d:links:from)
                (to . ,target))
              d:links))
      (s-lex-format "<a href=\"/word/${href}\">${desc}</a>"))))

(defun d:links:linkify-arrow (str)
  "Try to create a link for synonym arrows in STR."
  (when str
    (->> str
         (s-replace-regexp
          (rx "→"
              (group (+ any))
              (or "。" eol))
          (lambda (str)
            (format
             "→%s%s"
             (d:links:link-to-word
              (match-string 1 str))
             ;; this is nil in the eol case
             (or (match-string 2 str) "")))))))

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
          (equal (d:links:linkify-first-phrase "b。")
                 "b。")))))

(defun d:links:linkify-brackets (str &optional open close)
  "Create links in STR for all brackets."
  (when str
    (->> str
         (s-replace-regexp
          ;; Presumably this is faster. Haven't timed it though.
          (if (and open close)
              (rx-to-string
               `(seq (group ,open)
                     (group (*? any))
                     (group ,close))
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
                 "交情。")
          ;; Should also work for a single word
          (equal (d:links:comma-word-list "敵意")
                 "<a href=\"/word/敵意\">敵意</a>")))))

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
         ;; The replacement character, which appears in one entry in
         ;; itaigi.
         ;; https://itaigi.tw/k/%E5%8D%88%E5%AE%89/
         ;; I'm pretty sure it's not supposed to be there.
         (s-replace (string #xFFFD) "")
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
           (s-replace "'" "’")
           (s-replace "?" "？")

           ;; A "省" in dict_concised is CJK COMPATIBILITY
           ;; IDEOGRAPH-F96D. I've reported the error.
           (s-replace "省" "省")
           ;; This has a few uses in itaigi.
           (s-replace "⿸疒哥" "𰣻")

           ;; This is only used once in itaigi:
           ;; https://itaigi.tw/k/%EF%97%AA%E8%8A%B3%E6%B0%B4/
           ;;
           ;; The problem is that it's in the private use area, and so
           ;; even though it's covered by HanaMinA I still am not
           ;; quite sure what it is actually supposed to look like.
           ;;
           ;; Given its pronunciation / meaning has another matching
           ;; Han character, this seems more correct.
           (s-replace (string 62954) "𫝺")

           ;; Work around chhoetaigi_taijittoasutian entries like
           ;; "(**裝)模做樣". I don't think the title is supposed to
           ;; be like that.
           (s-replace-regexp (rx "(**" (group (+ any)) ")")
                             "\\1")
           (s-replace-regexp (rx (any "[" "]"))
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
                         "<br><m>［\\1］</m>")
       (s-replace-regexp (rx "§" (group "英") (group (+ (any "a-zA-Z "))))
                         "<br><m>［\\1］</m>\\2")
       (s-replace-regexp (rx (opt "　") "△")
                         "<br><m title=\"參考詞\">［△］</m> ")
       d:links:linkify-brackets
       d:links:linkify-arrow
       (s-replace-regexp
        (rx (group "相對於")
            (group (*? any))
            (group "而言"))
        (lambda (str)
          (concat
           (match-string 1 str)
           (d:links:link-to-word
            (match-string 2 str))
           (match-string 3 str))))))

(defun d:hakkadict:pn (pn dialect)
  "Turn numeric tones in PN into Unicode.

DIALECT is the dialect, because that matters: 31 is \"ˋ\" in 四縣
and 南四縣, \"^\" in 大埔 and 詔安.

According to 客家語拼音方案. See:
https://language.moe.gov.tw/result.aspx?classify_sn=&subclassify_sn=447&content_sn=12"
  (->> pn
       (s-replace-all '(("113" . "ˇ")
                        ("11" . "ˇ")
                        ("21" . "^")
                        ("24" . "ˊ")
                        ("33" . "+")
                        ("35" . "ˊ")
                        ("43" . "ˋ")
                        ("53" . "ˋ")
                        ("54" . "ˋ")
                        ("55" . "")
                        ("2" . "ˋ")
                        ("5" . "")))
       ;; 四縣, 南四縣 is "ˋ", 大埔, 詔安 is "^"
       (s-replace "31" (if (member dialect '("四縣" "南四縣"))
                           "ˋ"
                         "^"))))

(defun d:process-props (props title dict)
  "Process the heteronym props object PROPS.

PROPS describes TITLE and belongs to DICT. These are needed for
some processing.

This is a separate step from shaping."
  (let ((d:links:from title))
    (dolist (key (list "definition" "source_comment" "典故說明"))
      (ht-update-with! props key
        #'d:links:linkify-brackets))
    (dolist (key (list "trs" "poj" "kip"))
      (ht-update-with! props key
        (lambda (pn)
          (d:pn-normalize pn :one))))
    (dolist (key (list "radical" "v_type" "v_pinyin"))
      (ht-update-with! props key
        #'s-trim))
    (dolist (key (list "近義同" "近義反"))
      (ht-update-with! props key
        #'d:links:comma-word-list))
    (dolist (key (list "antonyms" "synonyms"))
      (ht-update-with! props key
        (lambda (words)
          (->> (d:links:linkify-brackets words "【" "】")
               (s-replace-regexp
                (rx "[" (group (any "似反")) "]"
                    (group (* any)))
                (lambda (str)
                  (format
                   "<m>［%s］</m>%s"
                   (match-string 1 str)
                   (save-match-data
                     (d:links:comma-word-list
                      (match-string 2 str))))))))))
    (ht-update-with! props "word_ref"
      #'d:links:link-to-word)
    (ht-update-with! props "definitions"
      (lambda (defs)
        (seq-doseq (def defs)
          (ht-update-with! def "quote"
            #'d::ucs-NFC)
          (ht-update-with! def "example"
            (lambda (v)
              (cond
               ((stringp v)
                (d::ucs-NFC v))
               ((seqp v)
                (seq-map #'d::ucs-NFC v))
               (t (error "%s: het.props.definitions.example is neither a string or a sequence"
                         title)))))
          (ht-update-with! def "def"
            (-compose
             #'d:links:linkify-brackets
             #'d:links:linkify-first-phrase)))))
    ;; Just remove the title prop. It's already in het.title.
    (ht-remove! props "title")
    ;; The length prop is kind of pointless: just use [...str].length.
    (ht-remove! props "length")
    (pcase dict
      ("hakkadict"
       (dolist (p_name '("四縣" "海陸" "大埔" "饒平" "詔安" "南四縣"))
         (ht-update-with! props (format "p_%s" p_name)
           (lambda (str)
             (d:hakkadict:pn str p_name)))))
      ("kisaragi_dict"
       (ht-update-with! props "definitions"
         (lambda (defs)
           (seq-doseq (def defs)
             (ht-update-with! def "def"
               (lambda (d)
                 (->> d
                      (s-replace "#+begin_quote" "<blockquote>")
                      (s-replace "#+end_quote" "</blockquote>")
                      ;; No need to apply linkify-brackets again
                      d:links:org-style)))))))
      ("chhoetaigi_itaigi"
       (ht-update-with! props "definition"
         #'d:links:link-to-word))
      ("chhoetaigi_taioanpehoekichhoogiku"
       (dolist (key (list "en" "zh"))
         (ht-update-with! props key
           #'d:links:comma-word-list))
       (dolist (key (list "examplePOJ" "exampleEn" "exampleZh"))
         (ht-update-with! props key
           (lambda (example)
             (-> example
                 (d:links:linkify-brackets "[“" "”]")
                 (d:links:linkify-brackets "“[" "]”"))))))
      ("chhoetaigi_taijittoasutian"
       (ht-update-with! props "definition"
         (lambda (def)
           (d:links:linkify-brackets def "[" "]")))
       (ht-update-with! props "example"
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
       (ht-update-with! props "definition"
         #'d:process-def:dict_concised))
      ("dict_idioms"
       (ht-update-with! props "definition"
         (lambda (def)
           ;; There is often an anchor at the end of
           ;; dict_idioms definitions that's not
           ;; displayed. Getting rid of it here allows
           ;; shredding them from the database.
           (s-replace-regexp "<a name.*" "" def)))))
    ;; If a prop is empty, just don't include it.
    ;; Saves on parsing time and memory, I think.
    ;; Needs the client to check for `undefined' though.
    ;; heteronyms.json: 249MiB -> 202MiB
    ;; entries.db:      189MiB -> 152MiB
    (d::hash-prune props "")))

(defun d::dictionaries (&optional dev?)
  "Return definitions of dictionaries.

The value is a vector:
    [(ID . FILE) ; either this form
     (ID . (FILE1 FILE2 ...)) ; or this form
     ...]

Development versions are returned when applicable if DEV? is
non-nil.

An ID of nil means the entries are included but will not be shown
by default. This is added for assigning extra stroke count
information."
  (cond
   ((and dev?
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
    [("dict_idioms" . "dev-dict_idioms.json")
     ("hakkadict" . "dev-hakkadict.json")
     ("chhoetaigi_taioanpehoekichhoogiku" . "dev-chhoetaigi-taioanpehoekichhoogiku.json")
     ("chhoetaigi_itaigi" . "dev-chhoetaigi-itaigi.json")
     ("moedict_twblg" . ("dev-dict-twblg.json"
                         "dev-dict-twblg-ext.json"))
     ("chhoetaigi_taijittoasutian" . "dev-chhoetaigi-taijittoasutian.json")
     ("dict_revised" . "dev-dict_revised.json")
     ("dict_concised" . "dev-dict_concised.json")
     ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")
     (nil . "kisaragi/extra-strokes.json")])
   ;; For testing on my phone
   ((getenv "ANDROID_DATA")
    [("dict_idioms" . "ministry-of-education/dict_idioms.json")
     ("moedict_twblg" . ("moedict-data-twblg/dict-twblg.json"
                         "moedict-data-twblg/dict-twblg-ext.json"))
     ("chhoetaigi_taijittoasutian" . "chhoetaigi/ChhoeTaigi_TaijitToaSutian.json")
     ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")])
   (t
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
     ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")
     (nil . "kisaragi/extra-strokes.json")])))

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
                (or dict files) (1+ i) dict-count)
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
     with total = (length heteronyms)
     do
     (progn
       (when (or (= (1+ i) 1)
                 (= 0 (% (1+ i) 10000))
                 (= (1+ i) total))
         (message "Processing heteronyms (%s/%s)..." (1+ i) total))
       (ht-update-with! het "props"
         (lambda (props)
           (d:process-props
            props
            (gethash "title" het)
            (gethash "from" het))))))
    ;; Step 4
    (message "Writing result out to disk...")
    (let ((json-encoding-pretty-print (not noninteractive))
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
               #'d:process-props)
    comp))

(d:main)

(when noninteractive
  (kill-emacs))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp-package)
;; End:
