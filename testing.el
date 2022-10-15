;;; -*- lexical-binding: t -*-

(defvar kemdict/dict-revised.json)

(with-temp-buffer
  (insert-file-contents "moedict-data/dict-revised.json")
  (goto-char (point-min))
  (setq kemdict/dict-revised.json (json-parse-buffer)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
