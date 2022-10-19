;; -*- lexical-binding: t; -*-

;; This is wrong.
(with-temp-file "test-site/_data/moedict.json"
  (insert-file-contents "moedict-data/dict-revised.json")
  (goto-char (point-min))
  (while (re-search-forward (rx "{[" (group (= 4 alnum)) "]}")
                            nil t)
    (let ((code (match-string 1)))
      (thread-first (string-to-number code 16)
                    string
                    replace-match))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
