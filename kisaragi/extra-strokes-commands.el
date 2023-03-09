;;; extra-strokes-commands.el --- Commands for editing extra-strokes.json -*- lexical-binding: t -*-


(defun k/add-new-entry ()
  ""
  (interactive)
  (catch 'exit
    (while t
      (let ((input (read-string "title,radical,stroke,non-rad: ")))
        (when (s-blank? (s-trim input))
          (throw 'exit nil))
        (-let (((a c b d) (s-split "," input)))
          (goto-char (point-max))
          (forward-line -2)
          (end-of-line)
          (insert (s-lex-format ",{
\"title\": \"${a}\",
\"stroke_count\": ${b},
\"radical\": \"${c}\",
\"non_radical_stroke_count\": ${d}
}")))
        (save-buffer)))))

;; Local Variables:
;; mode: lisp-interaction
;; End:
