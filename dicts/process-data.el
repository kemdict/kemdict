;; -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'json)
(require 'ol)
(require 's)
(require 'seq)
(require 'sqlite)
(require 'ucs-normalize)
(require 'parse-time)

(require 'jieba)

(defalias 'd::NFD #'ucs-normalize-NFD-string)

(defmacro d::for (spec &rest body)
  "Wrapper around `seq-doseq' but with a `catch' blocks around.

A `catch' block called `continue' wraps BODY, and another `catch'
block called `break' wraps the whole thing, such that
\(throw \\='`continue' nil) and (throw \\='`continue' nil) works as in
other programming languages.

VAR and SEQUENCE are as in `seq-doseq', ie. VAR is bound to each
member of SEQUENCE."
  (declare (indent 1))
  `(catch 'break
     (seq-doseq ,spec
       (catch 'continue
         ,@body))))

(defun d::dictionaries ()
  "Return definitions of dictionaries.

The value is a list:
    [(ID LANG FILE)  ; either this form
     (ID LANG FILES) ; or this form
     ...]

An ID of nil means the entries are included but will not be shown
by default."
  (cond
   ;; (t
   ;;  '(("kisaragi_dict" "zh_TW" "kisaragi/kisaragi_dict.json")))
   (t
    ;; The order here defines the order they will appear in the word
    ;; pages.
    (--filter
     (and
      ;; All files of the dictionary exist
      (-all? #'file-exists-p (ensure-list (elt it 2))))
     '(("unihan" "han" "unihan.json")
       ("kisaragi_dict" "zh_TW" "kisaragi/kisaragi_dict.json")
       ("dict_concised" "zh_TW" "ministry-of-education/dict_concised.json")
       ("dict_revised" "zh_TW" "ministry-of-education/dict_revised.json")
       ("kisaragi_taigi" "nan_TW" "kisaragi/kisaragi_taigi.json")
       ("pts-taigitv" "nan_TW" "pts-taigitv/data/scrape-20250928T154651Z.json")
       ;; FIXME 同義詞 etc. aren't included yet
       ("kautian" "nan_TW" "ministry-of-education/kautian.json")
       ("chhoetaigi_taijittoasutian" "nan_TW" "chhoetaigi/ChhoeTaigi_TaijitToaSutian.json")
       ("chhoetaigi_itaigi" "nan_TW" "chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json")
       ("chhoetaigi_taioanpehoekichhoogiku" "nan_TW" "chhoetaigi/ChhoeTaigi_TaioanPehoeKichhooGiku.json")
       ("chhoetaigi_maryknoll1976" "nan_TW" "chhoetaigi/ChhoeTaigi_MaryknollTaiengSutian.json")
       ("hakkadict" "hak_TW" "ministry-of-education/hakkadict.json")
       ("lopof-taigi" "nan_TW" "lopof-nan_TW.json")
       ("lopof-hakka" "hak_TW" "lopof-hak_TW.json")
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

(defun d::langs ()
  "Return all languages."
  (let ((available-codes (->> (d::dictionaries)
                              (--map (cadr it))
                              -uniq)))
    (->> '(("zh_TW" . "華語")
           ("nan_TW" . "台語")
           ("hak_TW" . "客語")
           ("han" . "漢字")
           ("ais" . "撒奇萊雅語")
           ("ami" . "阿美語")
           ("bnn" . "布農語")
           ("ckv" . "噶瑪蘭語")
           ("dru" . "魯凱語")
           ("pwn" . "排灣語")
           ("pyu" . "卑南語")
           ("ssf" . "邵語")
           ("sxr" . "拉阿魯哇語")
           ("tao" . "雅美語")
           ("tay" . "泰雅語")
           ("trv" . "太魯閣語")
           ("sdq" . "賽德克語")
           ("tsu" . "鄒語")
           ("xnb" . "卡那卡那富語")
           ("xsy" . "賽夏語"))
         (--filter (member (car it) available-codes)))))

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

(defun d::hash-copy (table from to)
  "Copy the value of key FROM to TO in TABLE."
  (-when-let (value (ht-get table from))
    (ht-set! table to value)))

(defun d::hash-rename (table from to)
  "Rename the key FROM to TO in TABLE."
  (-when-let (value (ht-get table from))
    (ht-set! table to value)
    (ht-remove! table from)))

(defun d::hash-prune (table value)
  "Remove all entries in TABLE that are associated with VALUE."
  (cl-with-gensyms (dflt)
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

;; Use a list because I expect the number of items to be fairly low.
(defvar d:links:linked nil
  "A list of targets that have already been linked.

`d:links:linkify-keywords' uses this to avoid replacing links multiple times.")
(defvar d:links:lang nil
  "Used to override the language of a link.")

(defvar d:links nil
  "A list of link objects.
Link objects are alists with two keys, `to' and `from'.")

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
        (push target d:links:linked)
        (push `((from . ,d:links:from)
                (to . ,target))
              d:links))
      (if d:links:lang
          ;; If the word doesn't exist in the language we ask for here, the word
          ;; page just removes the query param and does a redirect as if it's
          ;; not passed in. So this is fine.
          (format "<a href=\"/word/%s?lang=%s\">%s</a>"
                  href d:links:lang desc)
        (format "<a href=\"/word/%s\">%s</a>"
                href desc)))))

(defun d:links:linkify-keywords (str)
  "Extract 5 keywords in STR and attempt to make them links."
  ;; Estimate the amount of information in the string and guess a
  ;; possibly appropriate number of keywords to extract.
  ;; Use bytes to treat Han characters as having more information.
  (let ((count (round (/ (string-bytes str) 50.0))))
    (if (= count 0)
        str
      (with-temp-buffer
        (insert str)
        (dolist (keyword (jieba-extract-keywords str count "n,v"))
          (goto-char (point-min))
          ;; Replace just the first one.
          (when
              (and
               (not (--any?
                     (s-contains? keyword it)
                     d:links:linked))
               (re-search-forward
                ;; HACK: avoid replacing a link.
                ;; This detection handles "/word/KEY" and <a>KEY</a>
                ;; FIXME: there are still at least 1000 entries where the href
                ;; is incorrectly replaced.
                ;; NOTE: 2025-12-23T05:59:45+0900 What do you mean?!?!
                (rx (group (not ">"))
                    (literal keyword)
                    (group (not (any "\"" "<"))))
                nil t))
            (replace-match
             (concat
              (match-string 1)
              (d:links:link-to-word keyword)
              (match-string 2))
             :fixedcase
             :literal)))
        (buffer-string)))))

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

(defun d:links:comma-word-list (str &optional new-separator)
  "Add links to a string STR containing a comma-separated list of words.
If NEW-SEPARATOR is non-nil, use it as the new comma; otherwise use \"、\"."
  (->> (split-string str "[,、]" t)
       (-map #'d:links:link-to-word)
       (s-join (or new-separator "、"))))

(ert-deftest d:links:comma-word-list ()
  (should
   (let ((d:titles:look-up-table
          (d:titles:to-look-up-table (list "敵意"))))
     (and (equal (d:links:comma-word-list "敵意、仇隙")
                 "<a href=\"/word/敵意\">敵意</a>、仇隙")
          (equal (d:links:comma-word-list "敵意,仇隙")
                 "<a href=\"/word/敵意\">敵意</a>、仇隙")
          (equal (d:links:comma-word-list "敵意,仇隙" ",")
                 "<a href=\"/word/敵意\">敵意</a>,仇隙")
          (equal (d:links:comma-word-list "交情。")
                 "交情。")
          ;; Should also work for a single word
          (equal (d:links:comma-word-list "敵意")
                 "<a href=\"/word/敵意\">敵意</a>")))))

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

           ;; Get rid of the "替" marker from Kautian.
           (s-replace "【替】" "")

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
  (let ((d:links:from title)
        (d:links:linked nil)
        (d:links:lang (when (equal dict "chhoetaigi_taijittoasutian")
                        "nan_TW")))
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
      ("kautian"
       (ht-update-with! props "def"
         #'d:links:linkify-keywords))
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
       (ht-update-with! props "corr_zh"
         (lambda (str)
           (d:links:link-to-word str)))
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
      ("pts-taigitv"
       ;; Keep a copy that doesn't have markup
       (d::hash-copy props "zh" "zh-plain")
       (ht-update-with! props "zh"
         #'d:links:link-to-word))
      ("chhoetaigi_itaigi"
       (ht-update-with! props "definition"
         #'d:links:link-to-word))
      ("chhoetaigi_maryknoll1976"
       ;; Keep a copy that doesn't have markup
       (d::hash-copy props "zh" "zh-plain")
       (ht-update-with! props "zh"
         #'d:links:comma-word-list))
      ("chhoetaigi_taioanpehoekichhoogiku"
       (ht-update-with! props "en"
         (lambda (s)
           (d:links:comma-word-list s ", ")))
       (ht-update-with! props "zh"
         #'d:links:comma-word-list)
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
                             str)))
       (ht-update-with! props "examplePoj"
         (lambda (str)
           (s-replace-regexp (rx (opt " ")
                                 (** 1 4 (or
                                          "∼"
                                          "~"))
                                 (opt " "))
                             (or (ht-get props "titlePoj")
                                 d:links:from)
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

(defun d:ensure-number (string-or-number)
  "Make sure STRING-OR-NUMBER is a number.
If it is a string, run `string-to-number' on it, otherwise leave it as is."
  (if (stringp string-or-number)
      (string-to-number string-or-number)
    string-or-number))

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
         (< (d:ensure-number
             (or (gethash "het_sort" it)
                 (gethash "id" it)))
            (d:ensure-number
             (or (gethash "het_sort" other)
                 (gethash "id" other))))))
    orig-hets))

(defun d::split-titles (titles)
  "Split TITLES, a string, into a list containing titles."
  ;; Lots of special cases. But there's only 45 entries that this does anything
  ;; with, so special casing is fine.
  (pcase titles
    ("cheh/choeh仔" '("cheh仔" "choeh仔"))
    ("kiàu-kiàu/kiauh-kiauh叫" '("kiàu-kiàu叫" "kiauh-kiauh叫"))
    ("sir/su-lài-tah" '("sir-lài-tah" "su-lài-tah"))
    ("三iān/uán" '("三iān" "三uán"))
    ("二彼/比" '("二彼" "二比"))
    ("小辦/扮" '("小辦" "小扮"))
    ("挽me̍h/mi̍h" '("挽me̍h" "挽mi̍h"))
    ("揚phòng-phòng/phōng-phōng/pòng-pòng" '("揚phòng-phòng" "揚phōng-phōng/揚pòng-pòng"))
    ("枯sau/tsuâ" '("枯sau" "枯tsuâ"))
    ("查bāi/māi" '("查bāi" "查māi"))
    ("水khiō/khiò" '("水khiō" "查khiò"))
    ("澹tsiu̍h-tsiu̍h/chiuh-chiuh" '("澹tsiu̍h-tsiu̍h" "澹chiuh-chiuh"))
    ("爛he̍h/le̍h" '("爛he̍h" "爛le̍h"))
    ("田phe̍h/phue̍h" '("田phe̍h" "田phue̍h"))
    ("田tìm/tòm" '("田tìm" "田tòm"))
    ("田土phe̍h/phue̍h" '("田土phe̍h" "田土phue̍h"))
    ("相khoeh/kheh" '("相khoeh" "相kheh"))
    ("相食ànn/ānn" '("相食ànn" "相食ānn"))
    ("石phè/phuê/phèr" '("石phè" "石phuê/石phèr"))
    ("緊pia̍k-pia̍k/piak-piak" '("緊pia̍k-pia̍k" "緊piak-piak"))
    ("草sannh/sah" '("草sannh" "草sah"))
    ("衝jip/chip" '("衝jip" "衝chip"))
    ("走sau/sàu" '("走sau" "走sàu"))
    ("跟tuè/tè" '("跟tuè" "跟tè"))
    ("雄/狠鬼鬼" '("雄鬼鬼" "狠鬼鬼"))
    (_ (split-string titles "/"))))

(defvar d:db nil
  "Holds the DB connection.")

(defun d:main ()
  (setq d:links nil)
  (setq d:titles:look-up-table (ht))
  (let* ((heteronyms nil))
    (let* ((dictionaries (d::dictionaries))
           (dict-count (length dictionaries)))
      ;; Step 1: collect hets and titles
      (cl-loop
       for (dict lang files)
       being the elements of dictionaries
       using (index i)
       do
       (let* ((files (-list files))
              (raw-dict (with-temp-buffer
                          (cl-loop for f in files
                                   vconcat
                                   (progn
                                     (message "JSON parsing %s..." f)
                                     (erase-buffer)
                                     (insert-file-contents f)
                                     (json-parse-buffer)))))
              (rep (make-progress-reporter
                    (format "Collecting heteronyms and titles from %s (%s/%s)..."
                            (or dict files) (1+ i) dict-count)
                    0 (length raw-dict) nil
                    ;; 5 percent / 4 seconds
                    5 4))
              (j 0))
         (seq-doseq (entry raw-dict)
           (progress-reporter-update rep j)
           (cl-incf j)
           (let ((orig-hets
                  (or
                   ;; If the original is a structure of heteronyms, just grab
                   ;; the heteronym.
                   ;;
                   ;; This works fine for moedict-data-twblg, because it only
                   ;; keeps "title", "radical", "stroke_count",
                   ;; "non_radical_stroke_count" on the word, which can be
                   ;; discarded.
                   ;;
                   ;; For kisaragi-dict, we're only putting "added" and "eq-*"
                   ;; on the word, which we copy to each heteronym, so this is
                   ;; also fine.
                   ;;
                   ;; For kautian, that doesn't work well because there are lots
                   ;; of data attached to the word.
                   (and (not (equal dict "kautian"))
                        (gethash "heteronyms" entry))
                   (vector entry))))
             (d::debug "%s - applying het_sort" dict)
             (when (> (length orig-hets) 1)
               (setq orig-hets (d:sort-orig-hets orig-hets)))
             (d::for (orig-het orig-hets)
               ;; {title,from,lang,props}
               ;; Get the main title from original heteronyms
               (let ((titles (or (-some-> (gethash "title" entry)
                                   d:process-title)
                                 ;; What I defined kautian to be
                                 (-some->> entry
                                   (gethash "han")
                                   (gethash "main")
                                   d:process-title)
                                 ;; 臺日大辭典 and 臺灣白話基礎語句
                                 (gethash "kip" orig-het)
                                 ;; unihan
                                 (gethash "char" orig-het))))
                 ;; Taijit has some titles that should expand into many words
                 (d::for (title (d::split-titles titles))
                   (let ((shaped-het (make-hash-table :test #'equal)))
                     ;; Skip invalid titles
                     (when (or (not (stringp title))
                               (string-empty-p title))
                       (throw 'continue nil))
                     ;; chhoetaigi_taijittoasutian:
                     ;; work around some incorrectly formatted titles, like
                     ;; "a-a cham-cham" being formatted as "a-acham-cham" in
                     ;; the title field
                     (when (and (equal dict "chhoetaigi_taijittoasutian")
                                ;; This means we know it's safe to substitude het.kip
                                (d:latin-only title)
                                (not (equal (downcase title)
                                            (downcase (gethash "kip" orig-het)))))
                       (setq title (gethash "kip" orig-het)))
                     (puthash "title" title shaped-het)
                     (puthash "from" dict shaped-het)
                     (puthash "lang" lang shaped-het)
                     ;; Copy some top-level props to each heteronym.
                     (--each '("eq-en" "eq-ja" "added")
                       (puthash it (gethash it entry) orig-het))
                     ;; We can't run d:process-props just yet, as that requires
                     ;; the list of all titles to work correctly.
                     (puthash "props" orig-het shaped-het)
                     (push shaped-het heteronyms)
                     (puthash title t d:titles:look-up-table)))))))
         (garbage-collect))))
    (garbage-collect)
    (setq heteronyms (nreverse heteronyms))
    ;; Step 2: do transformations
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
    ;; Step 3: insert them into the database
    (d:db-insert heteronyms (-uniq d:links))
    (message "Done")))

(defun d:db-insert (heteronyms links)
  "Insert all data into the database."
  (message "Initializing database...")
  (d:db-init)
  (message "Preparing langs and dicts...")
  (with-sqlite-transaction d:db
    (cl-loop for (id . name) in (d::langs)
             do (sqlite-execute
                 d:db
                 "INSERT INTO langs (\"id\", \"name\") VALUES (?, ?)"
                 (list id name)))
    (cl-loop for (id lang) in (d::dictionaries)
             do (sqlite-execute
                 d:db
                 "INSERT INTO dicts (\"id\", \"lang\") VALUES (?, ?)"
                 (list id lang)))
    (message "Preparing langs and dicts...done"))
  ;; (message "Inserting heteronyms...")
  (let* (;; This tells `json-encode' to use the same false / null as
         ;; `json-parse-buffer''s default, because there are false
         ;; values from there.
         (json-false :false)
         (json-null :null)
         (zh-plain-aliases-success nil)
         (len (length heteronyms))
         (rep (make-progress-reporter
               "Inserting heteronyms..."
               1 len
               nil
               ;; 5 percent / 4 seconds
               5 4)))
    (with-sqlite-transaction d:db
      (cl-loop
       for het being the elements of heteronyms
       using (index i)
       do
       ;; Ignore heteronyms with an empty title.
       ;; This happens for a few entries in dict_revised that appear to have
       ;; empty titles in the JSON.
       (when (gethash "title" het)
         (progress-reporter-update rep (1+ i) (format "(%s/%s)" (1+ i) len))
         (sqlite-execute
          d:db
          "
INSERT INTO
  heteronyms (\"title\",\"from\",\"lang\",\"props\")
VALUES
  (?,?,?,?)"
          (list (d::NFD (gethash "title" het))
                (d::NFD (gethash "from" het))
                (d::NFD (gethash "lang" het))
                (d::NFD (json-encode (gethash "props" het)))))
         ;; SQLite integer primary key is 1-based
         (let ((het-id (1+ i))
               (het.title (d::NFD (gethash "title" het)))
               ;; (het.from (d::NFD (gethash "from" het)))
               (alias-stmt "
INSERT INTO
  aliases (\"het_id\",\"alias\",\"exact\")
VALUES
  (?,?,?)"))
           (sqlite-execute d:db alias-stmt (list het-id het.title 1))
           ;; This is only used in Kautian
           (seq-doseq (alt (-some->> het
                             (gethash "props")
                             (gethash "han")
                             (gethash "alt")))
             (sqlite-execute d:db alias-stmt (list het-id alt 1)))
           (dolist (pn (map-values (d:pn-collect het)))
             (sqlite-execute d:db alias-stmt (list het-id pn 1))
             ;; Input versions.
             ;; - don't duplicate if equal to original
             ;; - don't bother for some dictionaries)
             (when (member (gethash "from" het)
                           '("kautian"
                             "chhoetaigi_itaigi"
                             "chhoetaigi_taioanpehoekichhoogiku"
                             "chhoetaigi_taijittoasutian"
                             "pts-taigitv"
                             "lopof-taigi"
                             "lopof-hakka"))
               (let ((input-form (d:pn-to-input-form pn)))
                 (unless (equal input-form pn)
                   (sqlite-execute d:db alias-stmt (list het-id input-form nil))))))
           ;; For these two, set the zh version as an alias
           (when (member (gethash "from" het)
                         '("chhoetaigi_maryknoll1976"
                           "pts-taigitv"))
             (when-let ((zh (gethash "zh-plain" (gethash "props" het))))
               (unless zh-plain-aliases-success
                 (setq zh-plain-aliases-success t))
               (sqlite-execute d:db alias-stmt (list het-id zh nil))))
           (when (member (gethash "from" het)
                         '("chhoetaigi_maryknoll1976"))
             (when-let ((en (gethash "en" (gethash "props" het))))
               (sqlite-execute d:db alias-stmt (list het-id en nil)))))))
      (unless zh-plain-aliases-success
        (message "WARNING: zh-plain aliases from pts-taigitv and chhoetaigi_maryknoll1976 are not present"))))
  ;; (message "Inserting links...")
  (with-sqlite-transaction d:db
    (let* ((len (length links))
           (rep (make-progress-reporter
                 "Inserting links..."
                 1 len nil
                 ;; 5 percent / 4 seconds
                 5 4)))
      (cl-loop
       for link being the elements of links
       using (index i)
       do
       (progn
         (progress-reporter-update rep (1+ i) (format "(%s/%s)" (1+ i) len))
         (sqlite-execute
          d:db
          "INSERT INTO links (\"from\",\"to\") VALUES (?,?)"
          (list (d::NFD (alist-get 'from link))
                (d::NFD (alist-get 'to link))))))))
  (message "Calculating the \"han\" table...")
  (with-sqlite-transaction d:db
    (sqlite-execute d:db "
CREATE TABLE a AS
SELECT DISTINCT
  title,
  json_tree.value AS radical
FROM heteronyms, json_tree(heteronyms.props)
WHERE json_tree.key = 'radical'
  AND length(title) = 1;")
    (sqlite-execute d:db "
CREATE TABLE b AS
SELECT DISTINCT
  title,
  cast(json_tree.value as integer) AS sc
FROM heteronyms, json_tree(heteronyms.props)
WHERE json_tree.key = 'sc'
  AND length(title) = 1;")
    (sqlite-execute d:db "
CREATE TABLE c AS
SELECT DISTINCT
  title,
  cast(json_tree.value as integer) AS nrsc
FROM heteronyms, json_tree(heteronyms.props)
WHERE json_tree.key = 'nrsc'
  AND length(title) = 1;")
    (sqlite-execute d:db "
CREATE TABLE han AS
SELECT DISTINCT
  heteronyms.title,
  a.radical,
  b.sc,
  c.nrsc
FROM heteronyms
LEFT JOIN a ON a.title = heteronyms.title
LEFT JOIN b ON b.title = heteronyms.title
LEFT JOIN c ON c.title = heteronyms.title
WHERE length(heteronyms.title) = 1
  AND a.radical    IS NOT NULL
  AND b.sc         IS NOT NULL
  AND c.nrsc       IS NOT NULL
ORDER BY b.sc;")
    (sqlite-execute d:db "DROP TABLE a;")
    (sqlite-execute d:db "DROP TABLE b;")
    (sqlite-execute d:db "DROP TABLE c;"))
  (sqlite-execute d:db "VACUUM")
  (let ((words
         ;; The "added" field only exists for kisaragi-dict entries
         (sqlite-select d:db
                        "
SELECT
  heteronyms.title AS 'title',
  cast(json_tree.value as integer) AS 'time',
  heteronyms.\"from\" AS 'from'
FROM heteronyms, json_tree(heteronyms.props)
WHERE \"from\" LIKE 'kisaragi%'
  AND json_tree.key = 'added'
")))
    (dolist (f (directory-files
                "./ministry-of-education/diff/" t "added\\.json$"))
      (catch 'continue
        (let* ((parts (s-split " - "
                               (file-name-nondirectory
                                (directory-file-name f))))
               (dict-id (elt parts 0)))
          ;; It would be more correct to check if dictId is present, but
          ;; this also works.
          (when (equal dict-id "dict_mini")
            (throw 'continue nil))
          (let* ((added-date
                  (-some->> (elt parts 1) ; "2014_20220928-2014_20230112"
                    (s-split "-") ; ("2014_20220928" "2014_20230112")
                    (nth 1) ; "2014_20230112"
                    (s-split "_") ; ("2014" "20230112")
                    (nth 1) ; "20230112"
                    (s-match (rx (group (= 4 any))
                                 (group (= 2 any))
                                 (group (= 2 any))))
                    cdr ; ("2023" "01" "12")
                    (s-join "-"))))
            (unless added-date
              (message "Invalid date in %s" f)
              (throw 'continue nil))
            ;; We assume that they're all arrays of strings.
            (let ((titles (with-temp-buffer
                            (insert-file-contents f)
                            (ucs-normalize-NFD-region (point-min) (point-max))
                            (goto-char (point-min))
                            (json-parse-buffer :array-type 'list))))
              (d::for (title titles)
                (when (string-empty-p title)
                  (throw 'continue nil))
                (push
                 (list title
                       (-> (format "%sT00:00:00Z" added-date)
                           parse-iso8601-time-string
                           (time-convert 'integer))
                       dict-id)
                 words)))))))
    (setq words
          (let ((-compare-fn
                 (lambda (a b)
                   (equal (concat (elt a 0) (elt a 2))
                          (concat (elt b 0) (elt b 2))))))
            (->> words
                 (--sort (< (elt it 1)
                            (elt other 1)))
                 -uniq)))
    (with-sqlite-transaction d:db
      (pcase-dolist (`(,title ,time ,from) words)
        (sqlite-execute
         d:db "
INSERT INTO
  newwords (\"title\",\"time\",\"from\")
VALUES
  (?,?,?)"
         (list title time from))))))

(defun d:db-init ()
  "Create and initialize a new entries.db including its tables."
  (when (file-exists-p "entries.db")
    (delete-file "entries.db"))
  (setq d:db (sqlite-open "entries.db"))
  (sqlite-pragma d:db "user_version = 4")
  (sqlite-execute d:db "
CREATE TABLE langs (
  \"id\" TEXT PRIMARY KEY,
  \"name\" TEXT NOT NULL
);")
  (sqlite-execute d:db "
CREATE TABLE dicts (
  \"id\" TEXT PRIMARY KEY,
  \"lang\" TEXT REFERENCES langs(\"id\")
);")
  (sqlite-execute d:db "
CREATE TABLE heteronyms (
  \"id\" INTEGER PRIMARY KEY,
  \"title\" TEXT NOT NULL,
  \"from\" TEXT REFERENCES dicts(\"id\"),
  \"lang\" TEXT REFERENCES langs(\"id\"),
  \"props\" TEXT NOT NULL
);")
  ;; An example of an inexact alias is removing diacritics to be searchable with
  ;; an ASCII keyboard. If "góa" is reduced to "goa", matches of the latter is
  ;; always going to be inexact, therefore the alias itself is inexact.
  (sqlite-execute d:db "
CREATE TABLE aliases (
  \"het_id\" INTEGER REFERENCES heteronyms(\"id\"),
  \"alias\" TEXT NOT NULL,
  \"exact\" INTEGER
);")
  (sqlite-execute d:db "
CREATE TABLE links (
  \"from\" TEXT NOT NULL,
  \"to\" TEXT NOT NULL
);")
  ;; New words, sorted by date/time added
  (sqlite-execute d:db "
CREATE TABLE newwords (
  \"title\" TEXT NOT NULL,
  \"time\" TEXT NOT NULL,
  \"from\" TEXT REFERENCES dicts(\"id\")
);"))

(defun d:pn-normalize (pn)
  "Normalize pronunciation PN.

PN can be a list, string, vector, or hash table.

If PN is a hash table, get the string from its value for \"zh-Hant\".

Return a list of normalized strings. This is because some
pronunciation strings include multiple pronunciations."
  (let ((pns (cond
              ((stringp pn)
               (list pn))
              ((hash-table-p pn)
               (gethash "zh-Hant" pn))
              ((vectorp pn)
               (cl-coerce pn 'list))
              ((listp pn)
               pn))))
    (->> pns
         (--map
          (->> it
               d::NFD
               (s-replace "　" " ")
               ;; The replacement character, which appears in one entry in
               ;; itaigi.
               ;; https://itaigi.tw/k/%E5%8D%88%E5%AE%89/
               ;; I'm pretty sure it's not supposed to be there.
               (s-replace "\uFFFD" "")
               (s-replace "（變）" "/")
               ;; Remove the in-band type tag from kautian.
               ;; It would be better if we have some way to keep this information.
               (s-replace "【白】" "")
               (s-replace "【文】" "")
               ;; Finally split by the slash, used by multiple dictionaries to
               ;; denote multiple alternatives
               (s-split "[ \t\n\r]*/[ \t\n\r]*")))
         (-flatten-n 1)
         (--remove (equal it "")))))

(defun d:pn-collect (het)
  "Collect pronunciations from HET."
  (let ((props (gethash "props" het))
        (keys '(;; kemdict-data-ministry-of-education
                "bopomofo"
                ;; "pinyin"

                ;; kisaragi-dict
                "pronunciation"

                ;; hakkadict
                "p_四縣" "p_海陸" "p_大埔" "p_饒平" "p_詔安" "p_南四縣"

                ;; what I chose for the pts-taigitv copy
                "pn"

                ;; chhoetaigi-itaigi (keys are defined in Makefile
                ;; in this repository)
                "poj" "kip"
                "pojInput" "kipInput"
                "pojInputOthers" "kipInputOthers"

                "kMandarin"))
        (ret (make-hash-table :test #'equal)))
    (dolist (key keys)
      (when-let (value (gethash key props))
        (dolist (p (d:pn-normalize value))
          (puthash key p ret))))
    ;; What I chose for kautian
    (when-let (tl (gethash "tl" props))
      (dolist (key '("main" "colloquial" "alt" "otherMerged"))
        (when-let (value (gethash key props))
          (dolist (p (d:pn-normalize value))
            (puthash key p ret)))))
    ret))

(defun d:pn-to-input-form (pn)
  "Normalize PN such that it is searchable with an ASCII keyboard."
  (->> pn
       (s-replace "ⁿ" "nn")
       ucs-normalize-NFKD-string
       string-to-list
       (--filter (< it 128))
       (apply #'string)))

(when noninteractive
  (jieba-reset 'big)
  (jieba-add-word "物件" "n")
  (--each '(d:titles:to-look-up-table
            d:sort-orig-hets d:radical-id-to-char
            d:process-title d:process-props d:process-def:dict_concised
            d:pn-to-input-form d:pn-normalize d:pn-collect
            d:main
            d:links:簡編本:近義反義 d:links:org-style d:links:linkify-keywords
            d:links:linkify-first-phrase d:links:linkify-brackets
            d:links:linkify-arrow d:links:link-to-word d:links:comma-word-list
            d:latin-only d:hakkadict:pn
            d:db-insert d:db-init
            d:cangjie-abc-to-han
            d::langs d::hash-rename d::hash-prune
            d::dictionaries)
    (byte-compile it))
  ;; We're holding all dictionary data in memory, so if this is too
  ;; low we'll be GC'ing all the time without being able to free any
  ;; memory.
  (let ((gc-cons-threshold 100000000)
        (debug-on-error nil))
    (d:main))
  (kill-emacs))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp-package)
;; End:
