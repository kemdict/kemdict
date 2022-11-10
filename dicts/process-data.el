;; -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1"))

(require 'cl-lib)
(require 'dash)
(require 'seq)

(when load-file-name
  (setq default-directory (file-name-directory load-file-name)))

(defun k/process-title (title)
  "Process TITLE to replace problematic characters, and so on."
  ;; Mainly to normalize to half-width characters.
  (->> title
       ucs-normalize-NFKC-string
       (replace-regexp-in-string "'" "’")
       (replace-regexp-in-string (rx "?") (rx "？"))))

(defun k/extract-development-version (word file output-path)
  "Read FILE and write its definition of WORD to OUTPUT-PATH.

The structure in FILE is preserved in OUTPUT-PATH.

This allows for not having to load everything when I'm only
iterating on one page.

Does nothing if OUTPUT-PATH already exists as a file."
  (declare (indent 1))
  (unless (file-exists-p output-path)
    (let (parsed)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (setq parsed (json-parse-buffer)))
      (with-temp-file output-path
        (insert
         (json-serialize
          (vconcat
           (seq-filter
            (lambda (it)
              (equal word (gethash "title" it)))
            parsed))))))))

(unless noninteractive
  (k/extract-development-version "挨"
    "ministry-of-education/dict_revised.json" "dev-dict_revised.json")
  (k/extract-development-version "挨"
    "moedict-data-twblg/dict-twblg.json" "dev-dict-twblg.json")
  (k/extract-development-version "挨"
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
    (seq-doseq (entry raw-dict)
      (let* ((title (k/process-title (gethash "title" entry)))
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
        (puthash title tmp shaped)))
    shaped))

(defun main ()
  (let* ((all-titles (list))
         (merged-result (make-hash-table :test #'equal))
         (dictionaries
          (if (and (or (not noninteractive)
                       (getenv "DEV"))
                   (file-exists-p "dev-dict_revised.json")
                   (file-exists-p "dev-dict-twblg.json")
                   (file-exists-p "dev-dict_concised.json")
                   (file-exists-p "dev-dict_idioms.json"))
              [("moedict_twblg" . "dev-dict-twblg.json")
               ("dict_revised" . "dev-dict_revised.json")
               ("dict_concised" . "dev-dict_concised.json")
               ("dict_idioms" . "dev-dict_idioms.json")
               ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")]
            [("moedict_twblg" . ("moedict-data-twblg/dict-twblg.json"
                                 "moedict-data-twblg/dict-twblg-ext.json"))
             ("dict_revised" . "ministry-of-education/dict_revised.json")
             ("dict_concised" . "ministry-of-education/dict_concised.json")
             ("dict_idioms" . "ministry-of-education/dict_idioms.json")
             ("kisaragi_dict" . "kisaragi/kisaragi_dict.json")]))
         (dict-count (length dictionaries))
         ;; A list of the original parsed dictionary data
         (raw-dicts (make-vector dict-count nil))
         (shaped-dicts (make-vector dict-count nil)))
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
    (setq all-titles (-uniq all-titles))
    (message "Merging...")
    (dolist (title all-titles)
      (let ((entry (make-hash-table :test #'equal)))
        (puthash "title" title entry)
        (dotimes (i dict-count)
          (when-let (v (gethash title (aref shaped-dicts i)))
            (puthash (car (aref dictionaries i)) v
                     entry)))
        (puthash title entry merged-result)))
    (message "Writing result out to disk...")
    (with-temp-file "titles.json"
      (insert (json-serialize (vconcat all-titles))))
    (with-temp-file "combined.json"
      (insert (json-serialize merged-result)))
    (message "Done")))

(if (and (fboundp #'native-comp-available-p)
         (native-comp-available-p))
    (native-compile #'main)
  (byte-compile #'main))
(main)
(when noninteractive
  (kill-emacs))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
