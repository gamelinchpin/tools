;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My custom indentation tools.
;;
;;  Copyright © 2005 John P. Weiss
;;  
;;  This package is free software; you can redistribute it and/or modify
;;  it under the terms of the Artistic License, included as the file
;;  "LICENSE" in the source code archive.
;;
;;  This package is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;
;;  You should have received a copy of the file "LICENSE", containing
;;  the License John Weiss originally placed this program under.
;;
;;  
;; A package of my indentation tools.  Almost all of these are `defsubst', so
;; that you can define mode-specific versions of them without loss of
;; performance.
;;
;;  
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;------------------------------------------------------------
;;
;; Utility Functions
;; 


(defsubst jpw-next-nonblank-line (&optional direction)
  ;; Moves forward to the next non-blank line.  The sign of the arg DIRECTION
  ;; determines whether we move forward (> 0) or backward (< 0).
  (let ((sgn (cond ((not direction) 1)
                   ((> direction 0) 1) 
                   ((< direction 0) -1) 
                   (t 0))))
    (forward-line sgn)
    (while (looking-at "^$")
      (forward-line sgn)
      )
    )
  )


(defsubst jpw-comment-internal-indentation ()
  ;; Return the indentation of the body of a comment.  For this to work,
  ;; `comment-start-skip' and `comment-end-skip' must be set correctly.
  ;; Leaves `point' at the start of the comment body
  (if (or (comment-beginning)
          (looking-at comment-start-skip))
      (progn (skip-syntax-forward " <")
             (if (looking-at comment-end-skip)
                 0
               ;;else
               (current-column)))
    );;end outer if
  )


(defsubst jpw-last-line-indentation ()
  ;; Return the indentation of the last non-blank line.
  ;; Returns 0 if the previous line is the first in the buffer.
  (save-excursion
    (jpw-next-nonblank-line -1)
    (current-indentation)
    )
  )


(defsubst jpw-prev-comment-internal-indentation ()
  ;; Return the indentation of the body of the comment in the preceding line,
  ;; or nil of the preceding line isn't a comment.
  (save-excursion
    (forward-line -1)
    (jpw-comment-internal-indentation)
    )
  )


;;------------------------------------------------------------
;;
;; Comment Indentation Functions
;; 


(defun jpw-indent-comment (&optional comment-offset)
  "If the current line is a comment, indent it according to a set of rules
\(see below\).  Evals to `nil' if the current line is not a comment or if
indenting fails.

The indentation rules are as follows:
- If the current comment line is indented to the same column as the
  previous comment line, then do one of the following:
  1. Do nothing if the previous line isn't a comment.
  2. Indent the body to the same level as the body of the previous comment
     line.  See below for the rules governing comment body indentation.
  3. If unable to do #2, then indent the body relatively.

- If the current comment line and the preceding comment line have different
  indentation, do each of the following:
  1. Indent the entire comment line.  The current line is
     indented to the same column as the previous line \(whether it was a
     comment or not\), plus anything specified in the optional arg
     COMMENT-OFFSET.
  2. Indent the body to the same level as the body of the previous comment
     line.
     The body isn't indented if:
     a. The previous line isn't a comment.
     b. The previous line is an empty comment.
     c. The this line's \"body\" is already indented more deeply than the
        previous line's.

In both cases, if the previous line wasn't a comment, or is a comment with *no*
body \(not even whitespace\), body indentation doesn't occur.  You'll have to
add whitespace to the comment manually for that line.

{jpw: 7/06}"
  ;; Indenting comments:
  ;;
  ;; Evals to `nil' if not in a comment.
  ;; Leaves `point' at arbitrary location on the current line.
  (let* (;; `jpw-comment-internal-indentation' leaves point at the start of
         ;; the body, so make sure these two are always grouped together, in
         ;; order.
         (body-indent  (jpw-comment-internal-indentation))
         (body-pos (point))
         (last-indent (jpw-last-line-indentation))
         (last-body-indent (jpw-prev-comment-internal-indentation))
         (last-comment-has-body (> last-body-indent 0))
         (new-indent (if comment-offset 
                         (+ last-indent comment-offset)
                       last-indent))
         );; end vars
    (and 
     ;; A two-fer:  It not only tells us where the comment body's indented to,
     ;; but if we're even in a comment (non-nil value).
     body-indent
     (if (= (current-indentation) last-indent)
         (if (and last-body-indent
                  last-comment-has-body)
              ;; Indent the body only, using indent-relative.
             (progn
               (goto-char body-pos)
               (if (> last-body-indent body-indent)
                   (indent-to last-body-indent)
                 ;;else
                 (indent-relative)
                 )
               t);; end progn
           )
       ;; else: Indent the comment itself
       (indent-line-to new-indent)
       ;; Indent the comment body (which may have moved as a result of the
       ;; indent.
       ;; (back-to-indentation)  ;; Done by `indent-line-to'
       (skip-syntax-forward " <")
       (if (and last-body-indent
                last-comment-has-body
                (> last-body-indent body-indent))
           (indent-to last-body-indent))
       t);; end if
     ;; Move `point' to the body indentation, ensuring that the defun evals to
     ;; `t'.
     (or (jpw-comment-internal-indentation)
         t)
     );;end and
    );;end let
  )


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Some library functions that may be of use for indentation.

;;  Works, but is a tad top-heavy.
;;  (how-many regex start-pt end-pt)
;; thing-at-point
;; indent-to
;; current-indentation
;; back-to-indentation ;; intra-line
;; (backward-to-indentation arg) ;; find indentation ARG lines back
;; forward-to-indentation 
;; indent-relative ;; Good fallback fn.
;; Moving by balanced parens:
;; forward-sexp
;; backward-sexp
;; backward-list
;; forward-list
;; backward-up-list
;; down-list

;; Causes the sexp/list fns to skip comments.
;; (setq parse-sexp-ignore-comments t)


;; Most of the Emacs internal indentation functions use the
;; `indent-line-function' var.


(provide 'jpw-indent-tools)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;