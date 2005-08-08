;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Functions to manipulate C/C++ and MODEL comments.
;
;
;  last modified 8/99            (jpw)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Put the entire current line in quotes.
;
(defun stringify-entire-line ()
  "Put an entire line in quotes, including any leading whitespace. [jpw: 8/99]"
  (interactive)
  (save-excursion
    (end-of-line)
    (if (not (equal (char-to-string (preceding-char)) " "))
        (insert " ")
      )
    (insert "\"")
    (beginning-of-line)
    (insert "\"")
    )
  )


; Put the entire current line in quotes, ignoring any leading whitespace
;
(defun stringify-indented-line ()
  "Put an entire line in quotes, ignoring any leading whitespace. [jpw: 8/99]"
  (interactive)
  (save-excursion
    (end-of-line)
    (if (not (equal (char-to-string (preceding-char)) " "))
        (insert " ")
      )
    (insert "\"")
    (back-to-indentation)
    (insert "\"")
    )
  )


; Convert a block of C++ comments to strings.
;
(defun stringify-cpp-comments (startpos endpos) 
  "Convert C++ comments in the region to a string. [jpw: 8/99]."
  (interactive "r")
  (save-excursion
    (goto-char startpos)
    (while (re-search-forward "\"" endpos t)
      (replace-match "'" nil nil)
      )
    (goto-char startpos)
    (while (re-search-forward "^\\([ \t]*\\)//" endpos t)
      (progn
        (replace-match "  \\1" nil nil)
        (stringify-indented-line)
        )
      )
    )
  )


; Convert a block of "string-lines" to a C++ comment block.
;
(defun comment-strings (startpos endpos)
  "Convert a block of strings to a C++ comment block. [jpw: 8/99]"
  (interactive "r")
  (save-excursion
    (goto-char startpos)
    (while (re-search-forward "^\\([ 	]*\\)\\\"" endpos t)
        (progn
          (replace-match "\\1" nil nil)
          (if (re-search-forward "\\\"[ 	]*$" endpos t)
              (replace-match " " nil nil)
            )
          (beginning-of-line)
          (if (re-search-forward "^\\([	]*\\) \\([	]*\\) " endpos t)
              (replace-match "//\\1\\2" nil nil)
            (progn
              (beginning-of-line)
              (insert "//")
              ) ;end else
            ) ;endif
          )
        ) ;endwhile
    )
  )


;---------------------------------------------------------------------------

;;;; END OF FILE ;;;;
