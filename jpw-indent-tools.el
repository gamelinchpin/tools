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
  ;; `comment-start-skip' must be set correctly.
  ;; Leaves `point' at the start of the comment body
  (if (comment-beginning)
      (progn (re-search-forward "[ \t]*" (line-end-position) t)
             (current-column))
    )
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


(defsubst jpw-comment-indent (&optional comment-offset indent-style)
  ;; Indenting comments:
  ;; 1. If the current comment line isn't indented to the same column as the
  ;;    previous line, indent the entire comment line.  The current line is
  ;;    indented to the same column as the previous one, plus anything
  ;;    specified in the optional arg COMMENT-OFFSET.
  ;; 2. Otherwise, indent relative within the comment.
  ;;
  ;; Evals to `nil' if not in a comment.
  ;; Leaves `point' at arbitrary location on the current line.
  (let* ((last-indent (jpw-last-line-indentation))
         (new-indent (if comment-offset 
                         (+ last-indent comment-offset)
                       last-indent)
         (body-indent  (jpw-comment-internal-indentation))
         );; end vars
    (and 
     ;; A two-fer:  tells us if we're in a comment (non-nil) and where the
     ;; comment body's indented to.
     body-indent
     (if (not (= (current-indentation) new-indent))
         (indent-line-to new-indent)
       ;; else:  Indent the body
       (indent-relative)
       );; end if
     t
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