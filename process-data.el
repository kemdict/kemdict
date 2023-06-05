;; -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.1"))

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'seq)
(require 's)
(require 'ol)
(require 'ht)
(require 'ucs-normalize)
(require 'jieba)

(defconst d:debug? nil)

(defmacro d::debug (fmt &rest args)
  "Pass FMT and ARGS to `message' if `d:debug?' is non-nil."
  (when d:debug?
    `(message ,fmt ,@args)))

(defconst d:abc-han-ht
  (ht (?a "日") (?b "月") (?c "金") (?d "木") (?e "水") (?f "火")
      (?g "土") (?h "竹") (?i "戈") (?j "十") (?k "大") (?l "中")
      (?m "一") (?n "弓") (?o "人") (?p "心") (?q "手") (?r "口")
      (?s "尸") (?t "廿") (?u "山") (?v "女") (?w "田") (?x "難")
      (?y "卜") (?z "重")))

(defun d:cangjie-abc-to-han (abc)
  (let* ((seq (downcase abc))
         (idx 0)
         (c nil)
         (str ""))
    (while (< idx (length seq))
      (setq c (aref seq idx))
      (setq str (concat str (gethash c d:abc-han-ht)))
      (cl-incf idx))
    str))

(when load-file-name
  (setq default-directory (file-name-directory load-file-name)))

(put 'ht-update-with! 'lisp-indent-function 2)

(defmacro with-syms (symbols &rest body)
  "Bind SYMBOLS to uninterned symbols, then run BODY."
  (declare (debug (sexp body))
           (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (make-symbol ,(format "--%s--" (symbol-name s)))))
                 symbols)
     ,@body))

(defun d::hash-rename (table from to)
  "Rename the key FROM to TO in TABLE."
  (-when-let (value (ht-get table from))
    (ht-set! table to value)
    (ht-remove! table from)))

(defun d::hash-prune (table value)
  "Remove all entries in TABLE that are associated with VALUE."
  (with-syms (dflt)
    (cl-loop for k in (hash-table-keys table)
             when (equal value (gethash k table dflt))
             do (remhash k table))
    table))

(defun d:radical-id-to-char (radical-id)
  "Return the normalized radical character for RADICAL-ID.
For example, 1 is 一, 213 is 龜."
  (ucs-normalize-NFKC-string
   (string (+ #x2f00 (1- radical-id)))))

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
  (d::dev:extract-development-version "雙數"
    "ministry-of-education/dict_revised.json" "dev-dict_revised.json")
  (d::dev:extract-development-version "無妨"
    "ministry-of-education/hakkadict.json" "dev-hakkadict.json")
  (d::dev:extract-development-version "無妨"
    "moedict-data-twblg/dict-twblg.json" "dev-dict-twblg.json")
  (d::dev:extract-development-version "無妨"
    "moedict-data-twblg/dict-twblg-ext.json" "dev-dict-twblg-ext.json")
  (d::dev:extract-development-version "單數"
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
  "A list of link objects.")

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
    (if (or (not target)
            (eq target :null)
            (equal target d:links:from)
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
      (format "<a href=\"/word/%s\">%s</a>"
              href desc))))

(defun d:links:linkify-keywords (str)
  "Extract 5 keywords in STR and attempt to make them links."
  ;; Estimate the amount of information in the string and guess a
  ;; possibly appropriate number of keywords to extract.
  ;; Use bytes to treat Han characters as having more information.
  (let ((count (round (/ (string-bytes str) 50.0))))
    (dolist (keyword (jieba-extract-keywords str count "n,v"))
      (setq str
            (s-replace-regexp
             ;; HACK: avoid replacing a link.
             ;; This detection handles "/word/KEY" and <a>KEY</a>
             ;; TODO: maybe we can get rid of this if we just make
             ;; sure to linkify keywords first before applying other
             ;; linkification functions?
             (rx (group (not ">"))
                 (literal keyword)
                 (group (not (any "\"" "<"))))
             (lambda (str)
               (concat
                (match-string 1 str)
                (d:links:link-to-word keyword)
                (match-string 2 str)))
             str)))
    str))

(ert-deftest d:links:linkify-keywords ()
  (should
   (let ((d:titles:look-up-table
          (d:titles:to-look-up-table '("塞責" "空頭支票"))))
     (and (equal (d:links:linkify-keywords
                  "他無論做什麼事都按照規定一板一眼的，絕不馬虎塞責。")
                 "他無論做什麼事都按照規定一板一眼的，絕不馬虎<a href=\"/word/塞責\">塞責</a>。")
          (equal (d:links:linkify-keywords "開立空頭支票是一種詐欺的行為。")
                 "開立<a href=\"/word/空頭支票\">空頭支票</a>是一種詐欺的行為。")
          ;; Idempotent
          (equal (d:links:linkify-keywords
                  (d:links:linkify-keywords
                   "他無論做什麼事都按照規定一板一眼的，絕不馬虎塞責。"))
                 (d:links:linkify-keywords
                  "他無論做什麼事都按照規定一板一眼的，絕不馬虎塞責。"))))))

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
              (rx
               (group (literal open))
               (group (*? any))
               (group (literal close)))
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
                (d:links:link-to-word (car (s-split (rx (or "#" "?"))
                                                    target))
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
  '(;; kemdict-data-ministry-of-education
    "bopomofo" ; "pinyin"
    ;; moedict-twblg
    "trs"
    ;; kisaragi-dict
    "pronunciation"
    ;; hakkadict
    "p_四縣" "p_海陸" "p_大埔" "p_饒平" "p_詔安" "p_南四縣"
    ;; chhoetaigi-itaigi (keys are defined in Makefile
    ;; in this repository)
    "poj" "kip"
    "kMandarin"))

;; FIXME: there is one entry in TaijitToaSutian that uses a slash to
;; indicate multiple different sets of Han characters.
(defun d:process-title (title)
  "Process TITLE to replace problematic characters, and so on.
If TITLE is empty, return nil so the call site can decide what to
do."
  (let ((title (s-trim title)))
    (unless (equal "" title)
      (->> title
           ;; This was from when titles were filenames.
           ;; (s-replace "'" "’")
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

(defun d:links:簡編本:近義反義 (str)
  "Linkify antonyms and synonyms STR for dict_concised."
  (save-match-data
    (->> str
         d:links:comma-word-list
         (s-replace-regexp
          (rx (group digit ".")
              (group (+? any))
              (group (or (+ " ") eos)))
          (lambda (s)
            (save-match-data
              (concat
               (match-string 1 s)
               (d:links:link-to-word
                (match-string 2 s))
               ;; Has separator: replace it with list comma, otherwise
               ;; just keep it empty
               (if (equal "" (match-string 3 s))
                   ""
                 "、"))))))))

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
    (dolist (key '("definition" "source_comment" "典故說明"))
      (ht-update-with! props key
        #'d:links:linkify-brackets))
    (--each '("definition" "source_comment" "典故說明" "用法例句")
      (ht-update-with! props it
        #'d:links:linkify-keywords))
    (dolist (key '("trs" "poj" "kip"))
      (ht-update-with! props key
        (lambda (pn)
          (->> pn
               ucs-normalize-NFC-string
               (s-replace "　" " ")
               s-trim))))
    (dolist (key '("radical" "v_type" "v_pinyin"))
      (ht-update-with! props key
        #'s-trim))
    (dolist (key '("近義同" "近義反"))
      (ht-update-with! props key
        #'d:links:comma-word-list))
    (dolist (key '("antonyms" "synonyms"))
      (ht-update-with! props key
        (lambda (words)
          (->>
           (d:links:linkify-brackets words "【" "】")
           (s-replace-regexp
            (rx "[" (group (any "似反")) "]"
                (group (* any)))
            (lambda (str)
              (format
               "<m>［%s］</m>%s"
               (match-string 1 str)
               (d:links:簡編本:近義反義
                (match-string 2 str)))))))))
    (ht-update-with! props "word_ref"
      #'d:links:link-to-word)
    (ht-update-with! props "definitions"
      (lambda (defs)
        (seq-doseq (def defs)
          (ht-update-with! def "quote"
            #'ucs-normalize-NFC-string)
          (ht-update-with! def "example"
            (lambda (v)
              (cond
               ((stringp v)
                (ucs-normalize-NFC-string v))
               ((seqp v)
                (seq-map #'ucs-normalize-NFC-string v))
               (t (error "%s: het.props.definitions.example is neither a string or a sequence"
                         title)))))
          (ht-update-with! def "def"
            (lambda (str)
              (-> str
                  d:links:linkify-first-phrase
                  d:links:linkify-brackets
                  d:links:linkify-keywords))))))
    ;; Just remove the title prop. It's already in het.title.
    (ht-remove! props "title")
    ;; The length prop is kind of pointless: just use [...str].length.
    (ht-remove! props "length")
    (pcase dict
      ("unihan"
       (ht-set! props "pinyin"
                (-some-> props
                  (ht-get "kMandarin")
                  (ht-get "zh-Hant")))
       (when-let* ((hashes (ht-get props "kRSUnicode"))
                   ;; There can be multiple radicals because it's
                   ;; ambiguous for some characters.
                   ;; HACK: we'll just take the first one.
                   (hash (elt hashes 0))
                   (radical (ht-get hash "radical"))
                   (non-radical-stroke (ht-get hash "strokes"))
                   (total-stroke (-some-> props
                                   (ht-get "kTotalStrokes")
                                   (ht-get "zh-Hant"))))
         (ht-set! props "radical" (d:radical-id-to-char radical))
         (ht-set! props "sc" total-stroke)
         (ht-set! props "nrsc"
                  non-radical-stroke))
       (ht-update-with! props "kCangjie"
         #'d:cangjie-abc-to-han)
       (d::hash-rename props "kCangjie" "cangjie")
       (d::hash-rename props "kDefinition" "defs")
       (d::hash-rename props "kTraditionalVariant" "varT")
       (d::hash-rename props "kSimplifiedVariant" "varS")
       ;; Purge these currently unused fields until I know what to do
       ;; with them
       (--each '("char" "ucn" "kRSUnicode"
                 "kMandarin" "kTotalStrokes"
                 "kPhonetic"
                 "kSemanticVariant" "kSpecializedSemanticVariant"
                 "kAccountingNumeric" "kOtherNumeric" "kPrimaryNumeric")
         (ht-remove! props it)))
      ("hakkadict"
       (dolist (p_name '("四縣" "海陸" "大埔" "饒平" "詔安" "南四縣"))
         (ht-update-with! props (format "p_%s" p_name)
           (lambda (str)
             (d:hakkadict:pn str p_name)))))
      ((pred (s-prefix? "kisaragi"))
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
       (dolist (key '("en" "zh"))
         (ht-update-with! props key
           #'d:links:comma-word-list))
       (dolist (key '("examplePOJ" "exampleEn" "exampleZh"))
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
                                 (** 1 4 (or
                                          "∼" ; #x223c, TILDE OPERATOR
                                          "~"))
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
           (s-replace-regexp "<a name.*" "" def))))
      ((pred (s-prefix? "ilrdf"))
       (ht-update-with! props "ref"
         #'d:links:link-to-word)
       (ht-update-with! props "def"
         (lambda (str)
           (->> str
                (s-replace-regexp
                 (rx (group (not ">"))
                     (group (+ (any "a-zA-Z'^:ṟéɨʉ-"))))
                 (lambda (s)
                   (concat
                    (match-string 1 s)
                    (d:links:link-to-word
                     (match-string 2 s)))))
                d:links:linkify-keywords)))))
    (d::hash-rename props "non_radical_stroke_count" "nrsc")
    (d::hash-rename props "stroke_count" "sc")
    (d::hash-rename props "definition" "def")
    (d::hash-rename props "definitions" "defs")
    ;; If a prop is empty, just don't include it.
    ;; Saves on parsing time and memory, I think.
    ;; Needs the client to check for `undefined' though.
    ;; heteronyms.json: 249MiB -> 202MiB
    ;; entries.db:      189MiB -> 152MiB
    (d::hash-prune props "")
    (d::hash-prune props nil)
    (d::hash-prune props :null)))

(defun d::dictionaries ()
  "Return definitions of dictionaries.

The value is a vector:
    [(ID . FILE) ; either this form
     (ID . (FILE1 FILE2 ...)) ; or this form
     ...]

An ID of nil means the entries are included but will not be shown
by default. This is added for assigning extra stroke count
information."
  (cond
   ;; For testing on my phone
   ((getenv "ANDROID_DATA")
    [("dict_idioms" "zh_TW" "ministry-of-education/dict_idioms.json")
     ("moedict_twblg" "nan_TW" ("moedict-data-twblg/dict-twblg.json"
                                "moedict-data-twblg/dict-twblg-ext.json"))
     ("chhoetaigi_taijittoasutian" "nan_TW" "chhoetaigi/ChhoeTaigi_TaijitToaSutian.json")
     ("dict_concised" "zh_TW" "ministry-of-education/dict_concised.json")
     ("kisaragi_dict" "zh_TW" "kisaragi/kisaragi_dict.json")])
   (t
    ;; The order here defines the order they will appear in the word
    ;; pages.
    (--filter
     (-all? #'file-exists-p (ensure-list (elt it 2)))
     '(("unihan" "han" "unihan.json")
       ("kisaragi_dict" "zh_TW" "kisaragi/kisaragi_dict.json")
       ("dict_concised" "zh_TW" "ministry-of-education/dict_concised.json")
       ("dict_revised" "zh_TW" "ministry-of-education/dict_revised.json")
       ("kisaragi_taigi" "nan_TW" "kisaragi/kisaragi_taigi.json")
       ("chhoetaigi_taijittoasutian" "nan_TW" "chhoetaigi/ChhoeTaigi_TaijitToaSutian.json")
       ("moedict_twblg" "nan_TW" ("moedict-data-twblg/dict-twblg.json"
                                  "moedict-data-twblg/dict-twblg-ext.json"))
       ("chhoetaigi_itaigi" "nan_TW" "chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json")
       ("chhoetaigi_taioanpehoekichhoogiku" "nan_TW" "chhoetaigi/ChhoeTaigi_TaioanPehoeKichhooGiku.json")
       ("hakkadict" "hak_TW" "ministry-of-education/hakkadict.json")
       ("ilrdf_ais" "ais" "ilrdf/ais.json")
       ("ilrdf_ami" "ami" "ilrdf/ami.json")
       ("ilrdf_bnn" "bnn" "ilrdf/bnn.json")
       ("ilrdf_ckv" "ckv" "ilrdf/ckv.json")
       ("ilrdf_dru" "dru" "ilrdf/dru.json")
       ("ilrdf_pwn" "pwn" "ilrdf/pwn.json")
       ("ilrdf_pyu" "pyu" "ilrdf/pyu.json")
       ("ilrdf_ssf" "ssf" "ilrdf/ssf.json")
       ("ilrdf_sxr" "sxr" "ilrdf/sxr.json")
       ("ilrdf_tao" "tao" "ilrdf/tao.json")
       ("ilrdf_tay" "tay" "ilrdf/tay.json")
       ("ilrdf_trv" "trv" "ilrdf/trv.json")
       ("ilrdf_sdq" "sdq" "ilrdf/sdq.json")
       ("ilrdf_tsu" "tsu" "ilrdf/tsu.json")
       ("ilrdf_xnb" "xnb" "ilrdf/xnb.json")
       ("ilrdf_xsy" "xsy" "ilrdf/xsy.json")
       ("dict_idioms" "zh_TW" "ministry-of-education/dict_idioms.json"))))))

(defun d:sort-orig-hets (orig-hets)
  "Sort ORIG-HETS according to their het_sort or id keys.

They are sorted according to the \"het_sort\" key, or if that's
not present, the \"id\" key.

ORIG-HETS are props that will be used to construct heteronyms."
  (if (or (seq-every-p (-partial #'gethash "het_sort")
                       orig-hets)
          (seq-every-p (-partial #'gethash "id")
                       orig-hets))
      (sort
       orig-hets
       (lambda (it other)
         (< (string-to-number
             (or (gethash "het_sort" it)
                 (gethash "id" it)))
            (string-to-number
             (or (gethash "het_sort" other)
                 (gethash "id" other))))))
    orig-hets))

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
(defun d:parse-and-shape (dict lang files msg)
  "Return heteronyms in FILES.
DICT is the dictionary ID to associate with them.
LANG is the language of DICT.
Titles are written to `d:titles:look-up-table'."
  (let* ((files (-list files))
         (raw-dict (with-temp-buffer
                     (cl-loop for f in files
                              vconcat
                              (progn
                                (erase-buffer)
                                (insert-file-contents f)
                                (json-parse-buffer)))))
         (heteronyms nil)
         (rep (make-progress-reporter msg 0 (length raw-dict)))
         (i 0))
    (seq-doseq (entry raw-dict)
      (progress-reporter-update rep i)
      (cl-incf i)
      (let ((orig-hets (or (gethash "heteronyms" entry)
                           (vector entry))))
        (d::debug "%s - applying het_sort" dict)
        ;; TODO: maybe we want IDs in the DB as well
        ;; idea: add 1000000 to IDs from the first dict, 2000000 to
        ;; IDs from the second, and so on, plus ensuring we don't
        ;; exceed the maximum allowed word count in this scheme
        (when (> (length orig-hets) 1)
          (setq orig-hets (d:sort-orig-hets orig-hets)))
        (seq-doseq (orig-het orig-hets)
          (d::debug "%s - processing title" dict)
          (let* ((shaped-het (make-hash-table :test #'equal))
                 (title (or (-some-> (gethash "title" entry)
                              d:process-title)
                            ;; 臺日大辭典 and 臺灣白話基礎語句
                            (gethash "poj" orig-het)
                            ;; unihan
                            (gethash "char" orig-het))))
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
            (puthash "lang" lang shaped-het)
            ;; We can't run d:process-props just yet, as that requires
            ;; the list of all titles to work correctly.
            (puthash "props" orig-het shaped-het)
            (push shaped-het heteronyms)
            (puthash title t d:titles:look-up-table)))))
    heteronyms))

(defun d:main ()
  (setq d:links nil)
  (setq d:titles:look-up-table (ht))
  (let* ((heteronyms nil))
    (let* ((dictionaries (d::dictionaries))
           (dict-count (length dictionaries)))
      ;; Step 1
      (cl-loop
       for (dict lang files) being the elements of dictionaries
       using (index i)
       do
       (progn
         (setq heteronyms
               (nconc (d:parse-and-shape dict lang files
                                         (format "Collecting heteronyms and titles from %s (%s/%s)..."
                                                 (or dict files) (1+ i) dict-count))
                      heteronyms))
         (garbage-collect))))
    (garbage-collect)
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
         (message "Processing heteronyms (%s/%s)..." (1+ i) total)
         (garbage-collect))
       (ht-update-with! het "props"
         (lambda (props)
           (d:process-props
            props
            (gethash "title" het)
            (gethash "from" het))))))
    (garbage-collect)
    ;; Step 4
    (message "Writing result out to disk...")
    (let ((json-encoding-pretty-print (not noninteractive))
          ;; This tells `json-encode' to use the same false / null as
          ;; `json-parse-buffer''s default, because there are false
          ;; values from there.
          (json-false :false)
          (json-null :null))
      (setq d:links (-uniq d:links))
      ;; TODO: we can probably just make a CSV instead.
      (with-temp-file "links.json"
        (insert (json-encode d:links)))
      (with-temp-file "heteronyms.json"
        (insert (json-encode heteronyms))))
    (message "Done")))


(when noninteractive
  (jieba-reset 'big)
  (jieba-add-word "物件" "n")
  (let ((comp (if (and (fboundp #'native-comp-available-p)
                       (native-comp-available-p))
                  #'native-compile
                #'byte-compile)))
    (-each (list #'d:main
                 #'d:links:comma-word-list
                 #'d:links:link-to-word
                 #'d:links:linkify-brackets
                 #'d:process-props
                 #'d:parse-and-shape)
      comp))
  ;; We're holding all dictionary data in memory, so if this is too
  ;; low we'll be GC'ing all the time without being able to free any
  ;; memory.
  (let ((gc-cons-threshold 100000000))
    (d:main))
  (kill-emacs))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp-package)
;; End:
