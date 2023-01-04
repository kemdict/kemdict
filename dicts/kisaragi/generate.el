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
(defvar kisaragi-dict/links nil)

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

(defun kisaragi-dict/link-interpreter (link contents)
  "Interpret LINK object as the syntax we want.
This is a copy of `org-element-link-interpreter'.
CONTENTS is the element contents."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         ;; Hacky support for both internal and external links. Yes.
         (url? (string-prefix-p "//" path)))
    (if (string= type "radio")
        path
      (let ((fmt (pcase (org-element-property :format link)
                   ((guard contents)
                    (format (if url?
                                "<a href=\"%%s\">%s</a>"
                              "<a href=\"/word/%%s\">%s</a>")
                            (replace-regexp-in-string "%" "%%" contents)))
                   ((or `bracket
                        `nil
                        (guard (member type '("coderef" "custom-id" "fuzzy"))))
                    (if url?
                        "<a href=\"%1$s\">%1$s</a>"
                      "<a href=\"/word/%1$s\">%1$s</a>"))
                   (`angle "<%s>")
                   (`plain "%s")
                   (f (error "Wrong `:format' value: %s" f))))
            (thing (replace-regexp-in-string
                    (rx bos "/word/") ""
                    (pcase type
                      ("coderef" (format "(%s)" path))
                      ("custom-id" (concat "#" path))
                      ("file"
                       (let ((app (org-element-property :application link))
                             (opt (org-element-property :search-option link)))
                         (concat type (and app (concat "+" app)) ":"
                                 path
                                 (and opt (concat "::" opt)))))
                      ("fuzzy" path)
                      (_ (concat type ":" path))))))
        (unless url?
          (push `((from . ,kisaragi-dict/current-title)
                  (to . ,(replace-regexp-in-string "#.*" "" path)))
                kisaragi-dict/links))
        (format fmt thing)))))

(defun kisaragi-dict/elements-to-json (elems)
  "Process ELEMS to JSON for kisaragi-dict."
  (let ((original (--map
                   (cons it (symbol-function it))
                   '(org-element-link-interpreter
                     org-element-quote-block-interpreter))))
    ;; I don't know why `cl-letf' doesn't work for me. But this does.
    ;;
    ;; This would be terrible if this code is supposed to run in an
    ;; existing Emacs, but it's not. It's a one-time script.
    (fset #'org-element-link-interpreter
          (symbol-function #'kisaragi-dict/link-interpreter))
    (fset #'org-element-quote-block-interpreter
          (symbol-function #'kisaragi-dict/quote-block-interpreter))
    (prog1 (cl-loop
            for elem in elems
            collect
            ;; title
            (let ((kisaragi-dict/current-title (kisaragi-dict/elem-title elem)))
              (list
               (cons "title" kisaragi-dict/current-title)
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
                                     definition)))))))))
      (cl-loop for (sym . orig) in original
               do (fset sym orig)))))

(defun kisaragi-dict/parse-elements (file)
  "Return the word elements from FILE."
  (with-temp-buffer
    (insert-file-contents file)
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
  (setq kisaragi-dict/links nil)
  (with-temp-file "kisaragi_dict.json"
    (message "Generating kisaragi_dict.json...")
    (insert (->> (kisaragi-dict/parse-elements "kisaragi-dict.org")
                 kisaragi-dict/elements-to-json
                 (--sort (> (cdr (assoc "added" it))
                            (cdr (assoc "added" other))))
                 json-encode)
            "\n")
    (message "Generating kisaragi_dict.json...done"))
  (with-temp-file "links.json"
    (insert (json-encode kisaragi-dict/links))))

;;; generate.el ends here
