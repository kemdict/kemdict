;;; -*- lexical-binding: t -*-

(defvar kemdict/dict-revised.json)

(with-temp-buffer
  (insert-file-contents "moedict-data/dict-revised.json")
  (goto-char (point-min))
  (setq kemdict/dict-revised.json
        (json-parse-buffer :object-type 'alist))
  nil)

(let ((n 0))
  (catch 'ret
    (while t
      (let-alist (elt kemdict/dict-revised.json n)
        (when (equal "向" .title)
          (throw 'ret n))
        (cl-incf n)))))

(let-alist (elt kemdict/dict-revised.json 27046)
  (elt .heteronyms 1))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
