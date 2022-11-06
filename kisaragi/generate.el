;;; generate.el --- Process kisaragi-dict.org into JSON -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu

;;; Commentary:

;; kisaragi-dict.org's format is described within. This file
;; implements the format itself.
;;
;; Essentially entries are headings under the "Words" heading,
;; structured in a particular way, and this file converts that to JSON
;; in a schema similar to that used by Moedict.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'parse-time)
(require 'json)
(require 'dash)

(when load-file-name
  (setq default-directory (file-name-directory load-file-name)))

(defun kisaragi-dict/elem-title (elem)
  "Return the only title of ELEM."
  (org-no-properties
   (org-element-interpret-data
    (org-element-property :title elem))))

(defun kisaragi-dict/timestamp-to-unix (timestamp)
  "Convert TIMESTAMP (in yyyy-mm-ddThh:mm:ssZ) to unix time."
  (float-time (parse-iso8601-time-string timestamp)))

(defun kisaragi-dict/elements-to-json (elems)
  "Process ELEMS to JSON for kisaragi-dict."
  (cl-loop
   for elem in (-sort (-on #'string< #'kisaragi-dict/elem-title) elems)
   collect
   ;; title
   (list
    (cons "title" (kisaragi-dict/elem-title elem))
    ;; Use unix time so it's easier to compare
    (cons "added" (kisaragi-dict/timestamp-to-unix
                   (org-element-property :ADDED elem)))
    (cons "heteronyms"
          (cl-loop
           for het in (org-element-contents elem)
           when (eq 'headline (org-element-type het))
           collect
           ;; pronunciation
           (list (cons "pronunciation" (kisaragi-dict/elem-title het))
                 (cons "definitions"
                       (cl-loop
                        for definition in (org-element-contents het)
                        when (eq 'headline (org-element-type definition))
                        collect
                        (let* ((type+def
                                (split-string (kisaragi-dict/elem-title definition) "|"))
                               (def
                                (list (cons "type" (elt type+def 0))
                                      (cons "def" (elt type+def 1)))))
                          (dolist (prop-headline (org-element-contents definition))
                            (pcase (kisaragi-dict/elem-title prop-headline)
                              ("例"
                               (push (cons "example"
                                           (string-trim
                                            (org-element-interpret-data
                                             (org-element-contents prop-headline))))
                                     def))
                              ("語源"
                               (push (cons "etymology"
                                           (string-trim
                                            (org-element-interpret-data
                                             (org-element-contents prop-headline))))
                                     def))))
                          def)))))))))

(defun kisaragi-dict/parse-elements ()
  "Parse elements in this buffer."
  (with-temp-buffer
    (insert-file-contents "kisaragi-dict.org")
    (org-mode)
    (goto-char (point-min))
    (re-search-forward (rx bol "* Words") nil t)
    (forward-line)
    (beginning-of-line)
    (narrow-to-region (point) (point-max))
    (org-element-contents (org-element-parse-buffer))))

(cond ((and (fboundp #'native-comp-available-p)
            (native-comp-available-p))
       (native-compile #'kisaragi-dict/elements-to-json)
       (native-compile #'kisaragi-dict/parse-elements))
      (t
       (byte-compile #'kisaragi-dict/elements-to-json)
       (byte-compile #'kisaragi-dict/parse-elements)))

(let ((json-encoding-pretty-print t))
  (with-temp-file "kisaragi_dict.json"
    (message "Generating kisaragi_dict.json...")
    (insert (json-encode
             (kisaragi-dict/elements-to-json
              (kisaragi-dict/parse-elements)))
            "\n")
    (message "Generating kisaragi_dict.json...done")))

;;; generate.el ends here
