;; -*- lexical-binding: t; -*-

(require 'json)
(require 'cl-lib)
(require 'dash)

(defun k/process-title (title)
  "Process TITLE to replace problematic characters, and so on."
  ;; Mainly to normalize to half-width characters.
  (thread-last
    title
    ucs-normalize-NFKC-string
    (replace-regexp-in-string "'" "’")
    (replace-regexp-in-string (rx "?") (rx "？"))))

(defun main ()
  (let* ((all-titles (list))
         (merged-result (make-hash-table :test #'equal))
         (dictionaries
          (if (and (or (not noninteractive)
                       (getenv "DEV"))
                   (file-exists-p "a.json")
                   (file-exists-p "b.json"))
              [("moedict_zh" . "a.json")
               ("moedict_twblg" . "b.json")
               ("kisaragi_dict" . "dicts/kisaragi-dict/kisaragi_dict.json")]
            [("moedict_zh" . "dicts/moedict-data/dict-revised.json")
             ("moedict_twblg" . "dicts/moedict-data-twblg/dict-twblg.json")
             ("kisaragi_dict" . "dicts/kisaragi-dict/kisaragi_dict.json")]))
         (dict-count (length dictionaries))
         ;; A list of the original parsed dictionary data
         (raw-dicts (make-vector dict-count nil))
         (shaped-dicts (make-vector dict-count nil)))
    (dotimes (i dict-count)
      (with-temp-buffer
        (message "Parsing (%s/%s)..." (1+ i) dict-count)
        (insert-file-contents (cdr (aref dictionaries i)))
        (goto-char (point-min))
        (aset raw-dicts i (json-parse-buffer :array-type 'list))))
    ;; [{:title "title"
    ;;   :heteronyms (...)
    ;;   ... ...}
    ;;  ...]
    ;; -> {"title" {heteronyms (...)}
    ;;     "title2" {heteronyms (...)
    ;;     ...}
    (dotimes (i dict-count)
      (message "Shaping dictionary data (%s/%s)..." (1+ i) dict-count)
      (let ((shaped (make-hash-table :test #'equal)))
        (dolist (entry (aref raw-dicts i))
          (let ((title (k/process-title (gethash "title" entry))))
            (let ((tmp (make-hash-table :test #'equal)))
              (puthash "heteronyms" (gethash "heteronyms" entry) tmp)
              (puthash title tmp shaped))))
        (aset shaped-dicts i shaped)))
    (dotimes (i dict-count)
      (message "Collecting titles (%s/%s)..." (1+ i) dict-count)
      (cl-loop
       for k being the hash-keys of (aref shaped-dicts i)
       do (push k all-titles)))
    (message "Removing duplicate titles...")
    (setq all-titles (-uniq all-titles))
    (message "Merging...")
    (dolist (title all-titles)
      (let ((hash-table (make-hash-table :test #'equal)))
        (puthash "title" title hash-table)
        (dotimes (i dict-count)
          (when-let (v (gethash title (aref shaped-dicts i)))
            (puthash (car (aref dictionaries i)) v
                     hash-table)))
        (puthash title hash-table merged-result)))
    (message "Writing result out to disk...")
    (make-directory "src/_data" t)
    (with-temp-file "src/titles.json"
      (let ((json-encoding-pretty-print (not noninteractive)))
        (insert (json-encode all-titles))))
    (with-temp-file "src/_data/combined.json"
      (let ((json-encoding-pretty-print (not noninteractive)))
        (insert (json-encode merged-result))))
    (message "Done")))

(if (featurep 'comp)
    (native-compile #'main)
  (byte-compile #'main))
(main)
(when noninteractive
  (kill-emacs))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
