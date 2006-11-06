;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My custom indentation tools.
;;
;;  Copyright © 2005-2006 John P. Weiss
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


(require 'newcomment)


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


(defsubst jpw-last-line-indentation ()
  ;; Return the indentation of the last non-blank line.
  ;; Returns 0 if the previous line is the first in the buffer.
  (save-excursion
    (jpw-next-nonblank-line -1)
    (current-indentation)
    )
  )


(defsubst jpw-last-line-column-of (re)
  ;; Searches backwards for the last non-blank line, then returns the column
  ;; of the first instance of RE on that line **after** any leading
  ;; whitespace.
  ;; Returns nil if RE didn't match anything.
  (save-excursion
    (jpw-next-nonblank-line -1)
    (back-to-indentation)
    (and (re-search-forward re (line-end-position) t)
         (goto-char (match-beginning 0))
         (current-column))
    )
  )


(defun jpw-back-to-matching (delim inv-delim &optional pos boundpos)
  "Move backward until `delim' is found, ignoring any intermediate
\"`delim' ... `inv-delim'\" in the buffer.

At present, both `delim' and `inv-delim' must be characters.

Normally, you'd use `backward-list' or `backward-up-list' instead of this
function.  Those two functions operate on any character pairs with parenthesis
syntax.  From the Emacs-Lisp manual:
   | In English text, and in C code, the parenthesis pairs are `\(\)',
   | `[]', and `{}'.  In Emacs Lisp, the delimiters for lists and
   | vectors \(`\(\)' and `[]'\) are classified as parenthesis characters.
Other modes may not be so generous.  TCL mode, for example, only defines `\(\)'
as parenthesis characters.

The search begins at either the optional arg `pos' or at `point', and
continues until `boundpos' or a match is reached.  The optional `boundpos'
defaults to the beginning of buffer.  If no match is found before `boundpos',
the function returns `nil'.

{jpw: 7/06}"
  (if pos (goto-char pos))
  (let* ((nested-delim-count 1)
         (delim-re (concat "[" 
                           (char-to-string delim)
                           (char-to-string inv-delim)
                           "]"))
         );; end bindings

    (while (and (> nested-delim-count 0)
                (re-search-backward delim-re boundpos t))
      (cond 
       ((eq (following-char) delim)
        (setq nested-delim-count (1- nested-delim-count))
        )
       ((eq (following-char) inv-delim)
        (setq nested-delim-count (1+ nested-delim-count))
        )
       );;end cond
      );;end while

    (= nested-delim-count 0)
    );;end let
  )


(defun jpw-backward-extended-line ()
  "Move backward to the beginning of an extended line.  If the current line
isn't an extension of the previous one, no motion occurs at all.  Otherwise,
`point' is left at the end of the first line in the chain.

Assumes that the line-continuation delimiter is the single char \"\\\".

Evaulates to 'nil' if the current line isn't a continuation of a previous
one.
{jpw: 7/06}"
  (let ((pos-stack (list (point)))
        (not-done t)
        );; end bindings

    (save-excursion
      (while not-done
        (end-of-line 0) ;; moves to end of previous line
        (nconc pos-stack (list (point)))
        (skip-syntax-backward " ")
        (setq not-done (eq (preceding-char) '?\\))
        );;end while
      );;end excursion

    (goto-char (cadr (reverse pos-stack)))
    (> (length pos-stack) 2)
    );;end let
  )


;;------------------------------------------------------------
;;
;; Comment Indentation Functions
;; 


(defsubst jpw-back-to-comment-body ()
  ;; Moves to the beginning of the comment body, only using syntax checking if
  ;; `comment-use-syntax' is explicitly `t'.
  (if (< (current-column) (current-indentation))
      (back-to-indentation))
  (let (comment-start-pos)
    (if (or (looking-at comment-start-skip)
            (setq comment-start-pos (comment-beginning)))
        ;; Only reach here if we're in a comment.
        ;; We're also at the start of the comment.
        (if (eq comment-use-syntax t)
            (skip-syntax-forward " <")
          ;;else
          (if comment-start-pos (goto-char comment-start-pos))
          (and (looking-at comment-start-skip)
               ;; Only goto the match-end if we're looking at the comment
               ;; start.
               (goto-char (match-end 0)))
          )
      );;end if
    );;end let
  )


(defsubst jpw-comment-internal-indentation (&optional empty-body-ok)
  ;; Return the indentation of the body of a comment.  For this to work,
  ;; `comment-start-skip' and `comment-end-skip' must be set correctly.
  ;; Leaves `point' at the start of the comment body.
  ;; Returns nil if the current line isn't a comment.
  (if (jpw-back-to-comment-body)
      (if (and (looking-at comment-end-skip)
               (not empty-body-ok))
          0
        ;;else
        (current-column))
    )
  )


(defsubst jpw-prev-comment-internal-indentation (&optional empty-body-ok)
  ;; Return the indentation of the body of the comment in the preceding line,
  ;; or nil of the preceding line isn't a comment.
  (save-excursion
    (forward-line -1)
    (jpw-comment-internal-indentation empty-body-ok)
    )
  )


(defun jpw-indent-comment (&optional comment-offset)
  "If the current line is a comment, indent it according to a set of rules
\(see below\).  Evals to `nil' if the current line is not a comment or if
no indentation whatesoever occurs.

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
     `comment-offset'.
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

When using this within a programming mode, you may want to call the
mode-specific indent command before or after this one.

Leaves `point' unchanged if no indentation took place.  Otherwise, `point'
will move to the start of the comment \"body\".

{jpw: 7/06}"
  ;; Indenting comments:
  ;;
  ;; Evals to `nil' if not in a comment.
  ;; Leaves `point' at arbitrary location on the current line.
  (let* ((orig-pos (point))
         ;; `jpw-comment-internal-indentation' leaves point at the start of
         ;; the body, so make sure these two are always grouped together, in
         ;; order.
         (body-indent (jpw-comment-internal-indentation))
         (body-pos (point))
         (last-indent (jpw-last-line-indentation))
         (last-body-indent (jpw-prev-comment-internal-indentation t))
         (last-comment-has-body (if last-body-indent
                                    (> last-body-indent 0)))
         (new-indent (if comment-offset 
                         (+ last-indent comment-offset)
                       last-indent))
         );; end vars

    ;; Before doing anything, back up to where `point' was when this function
    ;; was called.
    ;; This way, if no indentation takes place, we leave `point' unchanged. 
    (goto-char orig-pos)

    (and 
     ;; A two-fer:  It not only tells us where the comment body's indented to,
     ;; but if we're even in a comment (non-nil value).
     body-indent
     (if (= (current-indentation) last-indent)
         (if last-comment-has-body
              ;; Indent the body only, using indent-relative.
             (progn
               (goto-char body-pos)
               (if (> last-body-indent body-indent)
                   (indent-to last-body-indent)
                 ;;else
                 (indent-relative)
                 )
               t);; end progn
           ;; else:  Last comment either (a) had no body; or (b) didn't indent
           ;;        its body relative to the comment starter.  This isn't an
           ;;        error, just a no-op.
           t)
       ;; else: Indent the comment itself
       (indent-line-to new-indent)
       ;; Indent the comment body (which may have moved as a result of the
       ;; indent.
       (jpw-back-to-comment-body)
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
;;
;; Causes the sexp/list fns to skip comments.
;; (setq parse-sexp-ignore-comments t)
;;
;; Most of the Emacs internal indentation functions use the
;; `indent-line-function' var.

;;_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._
;;
;; Unit Tests
;;
(defun utest-jpw-back-to-matching () 
  (interactive) 
  (message "%S" (jpw-back-to-matching '?\{ '?\})))
(defun utest-jpw-backward-extended-line ()
  (interactive) 
  (message "%S" (jpw-backward-extended-line)))


(provide 'jpw-indent-tools)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;