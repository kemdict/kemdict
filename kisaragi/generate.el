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

(defvar kisaragi-dict/current-title nil)

(defun kisaragi-dict/elem-title (elem)
  "Return the only title of ELEM."
  (org-no-properties
   (org-element-interpret-data
    (org-element-property :title elem))))

(defun kisaragi-dict/timestamp-to-unix (timestamp)
  "Convert TIMESTAMP (in yyyy-mm-ddThh:mm:ssZ) to unix time."
  (float-time (parse-iso8601-time-string timestamp)))

(defun kisaragi-dict/quote-block-interpreter (_ contents)
  "Interpret a quote block in the syntax we want.
CONTENTS is the element contents."
  (format "<blockquote>%s</blockquote>" contents))

(defun kisaragi-dict/element-to-json (elem)
  "Process ELEM to JSON for kisaragi-dict."
  (let ((original (--map
                   (cons it (symbol-function it))
                   '(org-element-quote-block-interpreter))))
    ;; I don't know why `cl-letf' doesn't work for me. But this does.
    ;;
    ;; This would be terrible if this code is supposed to run in an
    ;; existing Emacs, but it's not. It's a one-time script.
    (fset #'org-element-quote-block-interpreter
          (symbol-function #'kisaragi-dict/quote-block-interpreter))
    (prog1
        ;; title
        (let ((kisaragi-dict/current-title (kisaragi-dict/elem-title elem)))
          (list
           (cons "title" kisaragi-dict/current-title)
           ;; Use unix time so it's easier to compare
           (cons "added" (-> (org-element-property :ADDED elem)
                             parse-iso8601-time-string
                             float-time))
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
                                       (-> (kisaragi-dict/elem-title definition)
                                           (split-string "|")))
                                      ;; type+def is (def) or (type def ...)
                                      ;; so to detect if type is present we
                                      ;; check if the second element exists
                                      ;; or not.
                                      (has-type (and (cadr type+def) t))
                                      (type (and has-type (car type+def)))
                                      (def (if has-type
                                               (cadr type+def)
                                             (car type+def)))
                                      (content (string-trim
                                                (org-element-interpret-data
                                                 (org-element-contents definition))))
                                      definition)
                                 (when type
                                   (push (cons "type" type) definition))
                                 (unless (equal content "")
                                   (setq def (format "%s\n%s" def content)))
                                 (push (cons "def" def) definition)
                                 definition))))))))
      (cl-loop for (sym . orig) in original
               do (fset sym orig)))))

(defun kisaragi-dict/file-to-json (file)
  "Convert FILE to a structure ready to be written to JSON."
  (let ((ret))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      ;; Only search under "* Words"
      (re-search-forward (rx bol "* Words") nil t)
      (org-narrow-to-subtree)
      (org-map-region
       (lambda ()
         (when (org-entry-get nil "added")
           (save-restriction
             (org-narrow-to-subtree)
             (push (kisaragi-dict/element-to-json
                    (-> (org-element-parse-buffer) ; (org-data ...)
                        org-element-contents ; children of org-data
                        ;; first child, which is the headline element
                        car))
                   ret))))
       (point-min) (point-max)))
    ret))

(let ((json-encoding-pretty-print t))
  (with-temp-file "kisaragi_dict.json"
    (message "Generating kisaragi_dict.json...")
    (insert (->> (kisaragi-dict/file-to-json "kisaragi-dict.org")
                 (--sort (> (cdr (assoc "added" it))
                            (cdr (assoc "added" other))))
                 json-encode)
            "\n")
    (message "Generating kisaragi_dict.json...done")))

;;; generate.el ends here
