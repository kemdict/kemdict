;; -*- lexical-binding: t; -*-

(require 'json)
(require 'seq)

(defun uniq (sequence)
  "Return a list of the elements of SEQUENCE with duplicates removed.

This is a copy of dash.el's `-uniq', except that the fallback
case delegates to `seq-uniq' and it works with any sequence.

<rant>Really, considering dash.el is part of GNU ELPA and thus
part of GNU Emacs, seq-uniq should just copy the optimization
over.</rant>"
  (let* ((len (length sequence))
         (lookup-table (and (> len 32)
                            (make-hash-table :test #'equal
                                             :size len))))
    (if lookup-table
        (seq-filter
         (lambda (it)
           (or (gethash it lookup-table)
               (and (puthash it t lookup-table) nil)))
         sequence)
      (seq-uniq sequence))))

(defun main ()
  (let* ((all-titles (list))
         (merged-result (list))
         (dictionaries [("moedict_zh" . "moedict-data/dict-revised.json")
                        ("moedict_twblg" . "moedict-data-twblg/dict-twblg.json")])
         ;; (dictionaries [("moedict_zh" . "a.json")
         ;;                ("moedict_twblg" . "b.json")])
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
    (dotimes (i dict-count)
      (message "Collecting titles (%s/%s)..." (1+ i) dict-count)
      (dolist (entry (aref raw-dicts i))
        (push (gethash "title" entry) all-titles)))
    (message "Removing duplicate titles...")
    (setq all-titles (uniq all-titles))
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
          (let ((title (gethash "title" entry)))
            (let ((tmp (make-hash-table :test #'equal)))
              (puthash "heteronyms" (gethash "heteronyms" entry) tmp)
              (puthash title tmp shaped))))
        (aset shaped-dicts i shaped)))
    (message "Merging...")
    (dolist (title all-titles)
      (let ((hash-table (make-hash-table :test #'equal)))
        (puthash "title" title hash-table)
        (dotimes (i dict-count)
          (when-let (v (gethash title (aref shaped-dicts i)))
            (puthash (car (aref dictionaries i)) v
                     hash-table)))
        (push hash-table merged-result)))
    (message "Writing result out to disk...")
    (make-directory "src/_data" t)
    (with-temp-file "src/_data/combined.json"
      (let ((json-encoding-pretty-print (not noninteractive)))
        (insert (json-encode merged-result))))
    (message "Done")))

(cond ((featurep 'comp)
       (native-compile #'uniq)
       (native-compile #'main))
      (t
       (byte-compile #'uniq)
       (byte-compile #'main)))
(main)
(when noninteractive
  (kill-emacs))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
