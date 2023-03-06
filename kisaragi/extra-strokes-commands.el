;;; extra-strokes-commands.el --- Commands for editing extra-strokes.json -*- lexical-binding: t -*-


(defun k/add-new-entry ()
  ""
  (interactive)
  (let ((input (read-string "title|radical|stroke|non-rad: ")))
    (-let (((a c b d) (s-split "|" input)))
      (goto-char (point-max))
      (forward-line -2)
      (end-of-line)
      (insert (s-lex-format "
,{
\"title\": \"${a}\",
\"props\": {
  \"stroke_count\": ${b},
  \"radical\": \"${c}\",
  \"non_radical_stroke_count\": ${d}
}
}
")))))

;; Local Variables:
;; mode: lisp-interaction
;; End:
