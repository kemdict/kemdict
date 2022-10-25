;; -*- lexical-binding: t; -*-

(require 'json)

;; Copied from ht.el, Copyright (C) 2013 Wilfred Hughes
;; https://github.com/Wilfred/ht.el
(defmacro ht (&rest pairs)
  "Create a hash table with the key-value pairs given.
Keys are compared with `equal'.

\(fn (KEY-1 VALUE-1) (KEY-2 VALUE-2) ...)"
  (let* ((table-symbol (make-symbol "ht-temp"))
         (assignments
          (mapcar
           (lambda (pair) `(ht-set! ,table-symbol ,@pair))
           pairs)))
    `(let ((,table-symbol (ht-create)))
       ,@assignments
       ,table-symbol)))

(defvar k/tmp1 (make-hash-table))
(defvar k/tmp2 (make-hash-table))
(defvar k/merged-result (list))
(defvar k/all-titles (list))

(let ((moedict-zh (with-temp-buffer
                    (insert-file-contents "moedict-data/dict-revised.json")
                    (goto-char (point-min))
                    (json-parse-buffer :array-type 'list)))
      (moedict-twblg (with-temp-buffer
                       (insert-file-contents "moedict-data-twblg/dict-twblg.json")
                       (goto-char (point-min))
                       (json-parse-buffer :array-type 'list))))
  (message "%s" "Finished parsing")
  (dolist (entry moedict-zh)
    (push (gethash "title" entry) k/all-titles))
  (dolist (entry moedict-twblg)
    (push (gethash "title" entry) k/all-titles))
  (setq k/all-titles (cl-remove-duplicates k/all-titles))
  (message "%s" "Finished collecting all titles")
  (dolist (entry moedict-zh)
    (let ((title (gethash "title" entry)))
      (puthash title
               (ht
                ("title" title)
                ("moedict_zh" (ht ("heteronyms" (gethash "heteronyms" entry)))))
               k/tmp1)))
  (message "%s" "Finished reforming dictionary (1/2)")
  (dolist (entry moedict-twblg)
    (let ((title (gethash "title" entry)))
      (puthash title
               (ht
                ("title" title)
                ("moedict_twblg" (ht ("heteronyms" (gethash "heteronyms" entry)))))
               k/tmp2)))
  (message "%s" "Finished reforming dictionary (2/2)")
  (dolist (title k/all-titles)
    (let ((hash-table (ht ("title" title))))
      (when-let (v (gethash title k/tmp1))
        (puthash "moedict_zh" v hash-table))
      (when-let (v (gethash title k/tmp2))
        (puthash "moedict_twblg" v hash-table))
      (push hash-table k/merged-result)))
  (message "%s" "Finished merging")
  (make-directory "src/_data" t)
  (with-temp-file "src/_data/combined.json"
    (insert (json-encode k/merged-result)))
  (message "%s" "Done"))

(kill-emacs)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
