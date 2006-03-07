;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A major mode for editing LiveJournal entries.
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
;; You can enter this mode using `\M-p\M-j'.
;;
;;  
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'sgml-mode)
(require 'custom-defuns)


;;------------------------------------------------------------
;;
;; User Customizations
;; 


(defgroup jpw-lj nil
  "Customizable aspects of jpw-lj-mode.
{jpw: 03/06}"
  :group 'hypermedia
  :group 'local)


(defface jpw-lj-bold-face 
  '((t (:inherit bold)))
  "How to color [b]...[/b] tags."
  :group 'jpw-lj)

(defface jpw-lj-italic-face
  '((t (:inherit italic)))
  "How to color [i]...[/i] tags."
  :group 'jpw-lj)

(defface jpw-lj-underline-face
  '((t (:inherit underline)))
  "How to color [u]...[/u] tags."
  :group 'jpw-lj)

(defface jpw-lj-tag-face 
  '((t (:inherit font-lock-function-name-face)))
  "The face to use for non-markup jpw-lj tags."
  :group 'jpw-lj)

(defface jpw-lj-size-face 
  '((t (:inherit font-lock-type-face)))
  "The face to use for size markup."
  :group 'jpw-lj)

(defface jpw-lj-color-face 
  '((t (:foreground "magenta" :background "PaleGreen")))
  "The face to use for color markup."
  :group 'jpw-lj)

(defface jpw-lj-code-face 
  '((t (:inherit font-lock-constant-face)))
  "The face to use for code markup."
  :group 'jpw-lj)

(defface jpw-lj-url-face 
  '((t (:inherit font-lock-keyword-face
                 :underline t :bold t)))
  "The face to use for URLs."
  :group 'jpw-lj)


;;------------------------------------------------------------
;;
;; Variables
;; 


(defvar jpw-lj-tag-analysis-cache '()
  "Contains the most recent result of a call to `jpw-lj-tag-analysis'.
{jpw: 03/06}")


(defvar jpw-lj-t-a-c-l-u-buffer-state '(t 0 0)
  "Internal variable used by `jpw-lj-tag-analysis'.
{jpw: 03/06}")


;;------------------------------------------------------------
;;
;; Tag-Type-Specific Constants
;; 


(defconst jpw-tag-finder-re "<.+>")
(defconst jpw-tag-end-re "</")
(defconst jpw-tag-full-startend-re "</?\\([^>=]+\\)\\(=[^>]+\\)?>")


;;------------------------------------------------------------
;;
;; Utility Functions
;; 


(defsubst jpw-lj-tag-find-last (&optional pos boundpos)
  "Searches backward from `pos' (defaults to `point') for the nearest HTML
tag.  Returns a list of the form:

\(START-TAG END-TAG\)

Either START-TAG or END-TAG will be nil, indicating the type of tag in that
direction.  If both are nil (i. e. no tags were found), returns nil instead of
a list.

The optional `boundpos' is an integer buffer position.  It stops the search at
that position.  If no tag is found before `boundpos', the function returns
nil.  The default is to search backward to the beginning of the buffer.

Alters `point'.  Be sure to call this only from within a `save-excursion'.
{jpw: 03/06}"
  (if pos (goto-char pos))
  (let (last-start-tag
        last-end-tag
        the-tag) ;; end var defs
    (if (re-search-backward jpw-tag-finder-re boundpos t)
        (if (looking-at jpw-tag-end-re)
            (setq last-end-tag (point))
          ;;else
          (setq last-start-tag (point))
          )
      ) ;; end if
    (if (or last-start-tag last-end-tag)
        (list last-start-tag last-end-tag))
    ) ;;end let
  )


(defsubst jpw-lj-tag-find-next (&optional pos boundpos)
  "Searches forward from `pos' (defaults to `point') for the nearest HTML
tag.  Returns a list of the form:

\(START-TAG END-TAG\)

Either START-TAG or END-TAG will be nil, indicating the type of tag in that
direction.  If both are nil (i. e. no tags were found), returns nil instead of
a list.

The optional `boundpos' is an integer buffer position.  It stops the search at
that position.  If no tag is found before `boundpos', the function returns
nil.  The default is to search forward to the end of the buffer.

Alters `point'.  Be sure to call this only from within a `save-excursion'.
{jpw: 03/06}"
  (if pos (goto-char pos))
  (let (next-start-tag
        next-end-tag) ;; end var defs
    (if (re-search-forward jpw-tag-finder-re boundpos t)
        (progn
          (goto-char (match-beginning 0))
          (if (looking-at jpw-tag-end-re)
              (setq next-end-tag (point))
            ;;else
            (setq next-start-tag (point))
            )
          )
      ) ;; end if
    (if (or next-start-tag next-end-tag)
        (list next-start-tag next-end-tag))
    ) ;;end let
  )


(defsubst jpw-lj-get-tag-at (&optional pos)
  "Extract the name of the HTML tag at `pos', or `point' if pos is omitted.
If `pos' is a list, uses the first non-nil element.  (Actually, it checks only
 the `car' or `cadr'.)

Alters `point'.  Be sure to call this only from within a `save-excursion'.
{jpw 03/05}"
  (if pos 
      (if (listp pos)
          (goto-char (cond ((car pos)) ((cadr pos))))
        ) ;; end if listp
    ;; else:  scalar
    (goto-char pos)
    )
  (if (looking-at jpw-tag-full-startend-re)
      (match-string-no-properties 1)) 
  )


(defsubst jpw-lj-tag-pos-to-syntactic (search-result)
  "Used by several of the defun's below.  Converts `search-result' the list
returned by `jpw-lj-tag-find-last' or `jpw-lj-tag-find-next' to a list
of the form:

\(POS IS_ENDTAG TAG_NAME\)

This defun *greatly* simplifies all of the subsequent syntactic analysis code.
{jpw: 03/06}"
  (if search-result
      (let ((is-end-tag (and (cadr search-result) t))
            (tag-pos (or (car search-result) (cadr search-result)))
            ) ;;end var defs
        (list tag-pos is-end-tag (jpw-lj-get-tag-at tag-pos))
        ) ;;end let
    ) ;; end if
  )


(defsubst jpw-lj-tag-find-nearest (&optional pos-last-start 
                                             pos-next-end
                                             from-pos
                                             to-pos)
  "Used by `jpw-lj-tag-analysis'.  Returns a list of the form:

\(LAST NEXT\)

...or nil if no tags were found.  LAST and NEXT are both lists describing the
preceding and next nearest tags, respectively.  The lists both take the form:

\(POS IS_ENDTAG TAG_NAME\)

...or `nil' if no tag was found in that direction.

The optional argument `pos-last-start' is the buffer position of the nearest
start-tag found.  The search for the next start-tag will start here.  (Default
is to start at `point'.)  Similarly, `pos-next-end' is the position of the
nearest end-tag found, and will be used as the starting point for the next end
tag.  (Again, the default is to start searching at `point'.)

`from-pos' and `to-pos' are, respectively, the upper and lower bounds of the
search.  This lets you narrow the tag search to the region between these two
positions without having to call `narrow-to-region'.  The default is to search
the entire buffer.

Alters `point'.  Be sure to call this only from within a `save-excursion'.
{jpw: 03/06}"
  (if (not pos-last-start) (setq pos-last-start (point)))
  (if (not pos-next-end) (setq pos-next-end pos-last-start))
  (let* ((last (jpw-lj-tag-pos-to-syntactic
                (jpw-lj-tag-find-last pos-last-start from-pos)))
         (next (jpw-lj-tag-pos-to-syntactic
                (jpw-lj-tag-find-next pos-next-end to-pos)))
         ) ;; end var defs    

    (if (or last next)
        (list last next))
    ) ;;end let
  )


(defsubst jpw-lj-tag-analysis-ignore (tag)
  "Returns `t' when syntactic tag analysis should ignore `tag', `nil'
otherwise.
{jpw: 03/06}"
  (equal tag "*")
  )


(defsubst jpw-lj-tag-skip-ignorable (nearest
                                     &optional from-pos to-pos)
  "Extension of `jpw-lj-tag-find-nearest' which skips tags that
`jpw-lj-tag-analysis-ignore' returns `t' for.  

`nearest' should be the a list of the same form returned by
`jpw-lj-tag-find-nearest', which is what this defun evals to, as well.

`from-pos' and `to-pos' are, respectively, the upper and lower bounds of the
search.  This lets you narrow the tag search to the region between these two
positions without having to call `narrow-to-region'.  The default is to search
the entire buffer.

Alters `point'.  Be sure to call this only from within a `save-excursion'.
{jpw: 03/06}"
  (and 
   nearest
   (let* ((info-last-tag (car nearest))
          (info-next-tag (cadr nearest))
          ) ;;end var defs

     ;; Check the preceding tag
     (while (and info-last-tag
                 (jpw-lj-tag-analysis-ignore (caddr info-last-tag)))
       (setq info-last-tag 
             (jpw-lj-tag-pos-to-syntactic
              (jpw-lj-tag-find-last (car info-last-tag) from-pos)))
       ) ;; end backward search

     ;; Check the following tag
     (while (and info-next-tag
                 (jpw-lj-tag-analysis-ignore (caddr info-next-tag)))
       (setq info-next-tag 
             (jpw-lj-tag-pos-to-syntactic
              (jpw-lj-tag-find-next (1+ (car info-next-tag)) to-pos)))
       ) ;; end forward search

     ;; The new positions, free of non-syntactic tags.
     (and info-last-tag info-next-tag
          (list info-last-tag info-next-tag))
     ) ;; end let*
   ) ;; end and
  )


(defun jpw-lj-tag-find-enclosing (&optional pos-last-start 
                                            pos-next-end
                                            skip-unanalyzable-tags
                                            from-pos
                                            to-pos)
  "Search forward and backward to see what tags we're immediately
enclosed in.  Returns a list of the form:

\(TAG \(START END\)\)

START and END denote the locations of the 1st character of the start-tag and
end-tag, respectively.

The backward search begins from `pos-last-start', or `point' if not set.  The
forward search begins from `pos-next-end', or `pos-last-start' if not set.
\(`pos-last-start' is checked first.\)

`from-pos' and `to-pos' are, respectively, the upper and lower bounds of the
search.  This lets you narrow the tag search to the region between these two
positions without having to call `narrow-to-region'.  The default is to search
the entire buffer.

Returns nil if `point' is not enclosed in any HTML tags whatsoever.  

Returns a `nil' TAG when the nearest tags aren't a matching \"[TAG]
... [/TAG]\" pair.
{jpw: 03/06}"
  (if (not pos-last-start) (setq pos-last-start (point)))
  (if (not pos-next-end) (setq pos-next-end pos-last-start))
  (save-excursion
    (let* ((tmp-nearest-pair
            (if skip-unanalyzable-tags
                (jpw-lj-tag-skip-ignorable
                 (jpw-lj-tag-find-nearest pos-last-start pos-next-end
                                          from-pos to-pos)
                 from-pos to-pos)
              ;; else
              (jpw-lj-tag-find-nearest pos-last-start pos-next-end
                                       from-pos to-pos)
              ) ;;end if
            )
           (info-last-tag (car tmp-nearest-pair))
           (info-next-tag (cadr tmp-nearest-pair))
           (last-tag (caddr info-last-tag))
           (next-tag (caddr info-next-tag)) 
           nested-tag-count
           ) ;;end var defs

      ;; Here are the various cases we're looking for:

      (and
       ;; Edge Cases:    
       ;;   ..pt.. [TAG]
       ;;   ..pt.. [/TAG]
       ;;   [/TAG] ..pt.. 
       ;;   [TAG] ..pt.. 
       ;;   ..pt..
       ;; Returns `nil'
       info-last-tag info-next-tag
       ;; end: edge cases

       ;; Design Note:  The boolean logic of the first case:
       ;;
       ;;   (and (not is-edge-case) (cond ... other-cases ...))
       ;; 
       ;; makes use of shortcut-side-effects.  When faced with the edge case,
       ;; the (and ...) will stop here an eval to `nil', as desired.
       ;;
       ;; The first case is structured this way because it's a prerequisite to
       ;; all of the following ones, whose code may puke on a missing
       ;; last/next tag.

       ;; All subsequent cases.
       (cond
        ;; [/TAG] ..pt.. [TAG]
        ;; [/TAG1] ..pt.. [TAG2]
        ;; => We are in-between tags.  We may be in a higher-level
        ;;    outer tag, but that's not this function's concern.
        ;;    Returns `nil' and the tag positions.
        ((and (cadr info-last-tag) (not (cadr info-next-tag)))
         ;; Result List
         (list nil (list (car info-last-tag) (car info-next-tag)))
         ) ;; end: between tags

        ;; [TAG] ..pt.. [/TAG]
        ;; => We're inside the named tag.  This is a simple case.
        ;; [TAG1] ..pt.. [/TAG2]
        ;; => Malformed tag.  Returns `nil'
        ((and (not (cadr info-last-tag)) (cadr info-next-tag))
         (if (equal last-tag next-tag)
             ;; Result List
             (list last-tag (list (car info-last-tag) (car info-next-tag)))
           ) ;;end if
         ) ;; end: Inside tags.

        ;; [TAG1] ..pt.. [TAG2]
        ;; [TAG1] ..pt.. [TAG1]
        ((not (or (cadr info-last-tag) (cadr info-next-tag)))
         ;; We're between two start tags.  Search forwards for the
         ;; matching end tag.
         ;; The latter case, where a tag is nested inside of itself, is
         ;; allowed, and must be handled by counting repeated occurrences of
         ;; "TAG1".
         (setq nested-tag-count
               (if (equal last-tag (caddr info-next-tag)) 2 1))
         (while (and info-next-tag
                     (> nested-tag-count 0))
           ;; Note:  To prevent `jpw-lj-tag-find-next' from "getting
           ;; stuck" at the tag we found on the last iteration, we need to
           ;; start the search one char forward from `pos-next-end'.
           (setq info-next-tag 
                 (jpw-lj-tag-pos-to-syntactic
                  (jpw-lj-tag-find-next (1+ (car info-next-tag)) to-pos)))
           (if (equal last-tag (caddr info-next-tag))
               (if (cadr info-next-tag) ;; is end tag
                   (setq nested-tag-count (1- nested-tag-count))
                 ;; else:  it's a nested start tag 
                 (setq nested-tag-count (1+ nested-tag-count))
                 ) ;;end if end-or-start tag
             ) ;;end if equal
           ) ;; end while

         ;; Loop stopped.  Do we have a result?
         (if info-next-tag
             ;; Result List
             (list last-tag (list (car info-last-tag) (car info-next-tag)))
           ) ;;end if
         ) ;; end: Between two start tags.

        ;; [/TAG1] ..pt.. [/TAG2]
        ;; [/TAG2] ..pt.. [/TAG2]
        ((and (cadr info-last-tag) (cadr info-next-tag))
         ;; We're between end start tags.  Search backwards for the
         ;; matching start tag.
         ;; The latter case, where a tag is nested inside of itself, is
         ;; allowed, and must be handled by counting repeated occurrences of
         ;; "TAG2".
         (setq nested-tag-count
               (if (equal (caddr info-last-tag) next-tag) 2 1))
         (while (and info-last-tag
                     (> nested-tag-count 0))
           (setq info-last-tag 
                 (jpw-lj-tag-pos-to-syntactic
                  (jpw-lj-tag-find-last (car info-last-tag) from-pos)))
           (if (equal (caddr info-last-tag) next-tag)
               (if (cadr info-last-tag) ;; is end tag
                   (setq nested-tag-count (1+ nested-tag-count))
                 ;; else:  it's a matching start tag 
                 (setq nested-tag-count (1- nested-tag-count))
                 ) ;;end if end-or-start tag
             ) ;;end if equal
           ) ;; end while

         ;; Loop stopped.  Do we have a result?
         (if info-last-tag
             ;; Result List
             (list next-tag (list (car info-last-tag) (car info-next-tag)))
           ) ;;end if
         ) ;; end: Between two end tags.

        ;; Else:
        ;; Error - unknown case.  Return nil.
        (nil)
        ) ;;end cond : tag cases.
       ) ;; end edge-case `and'
      ) ;;end let
    ) ;;end excursion
  )


(defun jpw-lj-tag-nested-analysis ()
  "Search forward and backward from `point' to see what tags we're enclosed
in.  Each pair is the position of the start tag and end tag, respectively.
The list of pairs is built from the inside-out.  The results are stored in the
variable `jpw-lj-tag-analysis-cache', in a depth-descending order.

So, `\(car \(caadr \(jpw-lj-tag-analysis-cache\)\)\)' would return the
position of the outermost start tag.

The result of analysis is nil if `point' is not enclosed in any HTML tags.
{jpw: 03/06}"
  (let* ((buffer-state (list (buffer-modified-p) (buffer-modified-tick)
                             (buffer-size)))
         ;; Point the iterator at the cdr, so we can manipulate the
         ;; cache through the iterator.
         (cache-pos-cdr-iter (cons 'nil jpw-lj-tag-analysis-cache))
         ;; Simple work variable to make the code more readable; contains the
         ;; current taginfo list, the cdr of which holds the tag locations.
         (taginfo-from-iter (cadr cache-pos-cdr-iter))
         ;; Pull out the first pair of positions.
         (cached-startag-pos (car (cadr taginfo-from-iter)))
         (cached-endtag-pos (cadr (cadr taginfo-from-iter)))
         from-pos 
         to-pos
         analysis-cache-working
         current-tagspec
         ) ;;end var defs

    ;; Examine cached state, removing any state no longer consistent with our
    ;; current position.
    (if (equal jpw-lj-t-a-c-l-u-buffer-state buffer-state)
        (progn
          (while (and (not (or (null taginfo-from-iter)
                               (null cached-startag-pos)
                               (null cached-endtag-pos))) ;; Guard conds
                      ;; What we really want to check:
                      (<= cached-startag-pos (point))
                      (<= (point) cached-endtag-pos)
                      )
            (setq 
             ;; Update the search bounds with the old position
             from-pos cached-startag-pos
             to-pos cached-endtag-pos
             ;; Bump the iterator.
             cache-pos-cdr-iter (cdr cache-pos-cdr-iter)
             taginfo-from-iter (cadr cache-pos-cdr-iter)
             ;; Pull out the next pair of positions.
             cached-startag-pos (car (cadr taginfo-from-iter))
             cached-endtag-pos (cadr (cadr taginfo-from-iter))
             )
            ) ;;end while
          (if taginfo-from-iter
              (if (car cache-pos-cdr-iter)
                  ;; Truncate the cache, removing all elements that point is
                  ;; now outside of.
                  (setcdr cache-pos-cdr-iter 'nil)
                ;; else:
                ;; We never executed the while-loop.  ==> Point is outside of
                ;; everything in the cache.  Trash the old cache.
                (setq jpw-lj-tag-analysis-cache '())
                ) ;;end if
            ) ;;end if taginfo-from-iter
          ) ;;end progn
      ;;
      ;; else:
      ;; Force a full reanalysis.
      (setq jpw-lj-tag-analysis-cache '())
      ) ;;end if-same-buffer-state

    ;; We still want to attempt a constrained rebuild, however, as we may now
    ;; be inside of a more deeply nested tag.  Consider this example:
    ;;     <h1>dolor ipse <em>lorem</em> sit</h1>
    ;; If point was at "i" on initial analysis, then moved to the "r" in
    ;; "lorem," we couldn't use the cached analysis, now, could we?
    (save-excursion
      ;; Reset the buffer state and start rebuilding.
      (setq jpw-lj-t-a-c-l-u-buffer-state buffer-state
            current-tagspec (jpw-lj-tag-find-enclosing nil 
                                                       nil 
                                                       t 
                                                       from-pos 
                                                       to-pos)  
            )
      (while (car current-tagspec)
        ;; Prepend each new tag pair.  Since we're searching outward, this
        ;; builds `analysis-cache-working' in depth-descending order.
        (setq analysis-cache-working
              (cons current-tagspec analysis-cache-working)
              )
        ;;  Note the backward/forward offsetting by one character from the
        ;;  positions of the last start/end tags.
        (setq current-tagspec 
              (jpw-lj-tag-find-enclosing (1- (caadr current-tagspec))
                                         (1+ (cadadr current-tagspec))
                                         t
                                         from-pos
                                         to-pos))
        ) ;;end while
      ) ;;end excursion

    ;; Lastly, append the working analysis cache to the remnant of the old
    ;; cache (both of which are in depth-descending order).  This will also
    ;; make the contents of the cache the expression to which this defun
    ;; evals.
    (setq jpw-lj-tag-analysis-cache 
          (nconc jpw-lj-tag-analysis-cache analysis-cache-working))
    ) ;;end let
  )


;;------------------------------------------------------------
;;
;; Mode Interactive Functions
;; 


;; Skeleton Templates


(define-skeleton jpw-html-size
  "Insert HTML font resizing tags, or puts the active region inside such tags.
{jpw: 03/06}"
  nil
  "<!-- size>" _ "</size -->")

(define-skeleton jpw-html-italic
  "Insert HTML [physical] italics tags, or puts the active region inside HTML
italics tags.
{jpw: 03/06}"
  nil
  "<i>" _ "</i>")

(define-skeleton jpw-html-bold
  "Insert HTML [physical] bold tags, or puts the active region inside HTML
bold tags.
{jpw: 03/06}"
  nil
  "<b>" _ "</b>")

(define-skeleton jpw-html-underline
  "Insert HTML [physical] underline tags, or puts the active region inside
HTML underline tags.
{jpw: 03/06}"
  nil
  "<u>" _ "</u>")

(define-skeleton jpw-html-emphasized
  "Insert HTML [logical] emphasized tags, or puts the active region inside
HTML emphasized tags.
{jpw: 03/06}"
  nil
  "<em>" _ "</em>")

(define-skeleton jpw-html-strong
  "Insert HTML [logical] strong tags, or puts the active region inside HTML
strong tags.
{jpw: 03/06}"
  nil
  "<strong>" _ "</strong>")

(define-skeleton jpw-html-code
  "Insert HTML code tags, or puts the active region inside HTML list
tags.
{jpw: 03/06}"
  nil
  "<code>" _ "</code>")


(define-skeleton jpw-lj-user
  "A LiveJounral \"user\" tag."
  "Who? "
  "<lj user=" str ">")


(define-skeleton jpw-lj-cut
  "A LiveJounral \"cut\" tag."
  "Cut label: "
  "<lj-cut text=\"" str "\">" _ "</lj-cut>")


;; Redefinition of some of the html-mode skeleton functions, to ensure that
;; that they use HTML v4.0 / XHTML syntax.


(define-skeleton html-line
  "XHTML line break tag."
  nil
  "<br />" \n)

(define-skeleton html-ordered-list
  "XHTML ordered list tags."
  nil
  "<ol>" \n
  "<li>" _ "</li>" \n
  "</ol>")

(define-skeleton html-unordered-list
  "XHTML unordered list tags."
  nil
  "<ul>" \n
  "<li>" _ "</li>" \n
  "</ul>")

(define-skeleton html-list-item
  "XHTML list item tag."
  nil
  (if (bolp) nil '\n)
  "<li>" _  "</li>")

(define-skeleton html-paragraph
  "XHTML paragraph tag."
  nil
  (if (bolp) nil ?\n)
  \n "<p>" _ "</p>")

(define-skeleton html-horizontal-rule
  "XHTML horizontal rule tag."
  nil
  "<hr />" \n)


;; Other functions.


(defun jpw-lj-insert-emphasized (&optional type)
  "Insert HTML italics tags, or puts the active region inside HTML italics
tags.
Actually, it uses the logical tag \"<em>\", unless called with an arg.
{jpw: 03/06}"
  (interactive "P")
  (if (null type)
      (jpw-html-emphasized)
    (jpw-html-italic)
    )
  )


(defun jpw-lj-insert-strong (&optional type)
  "Insert HTML bold tags, or puts the active region inside HTML bold
tags.
Actually, it uses the logical tag \"<strong>\", unless called with an arg.
{jpw: 03/06}"
  (interactive "P")
  (if (null type)
      (jpw-html-strong)
    (jpw-html-bold)
    )
  )


(defun jpw-lj-insert-list (&optional type)
  "Insert HTML list tags, or puts the active region inside HTML list
tags.
The optional `type' specifies the type of list.  It can be passed directly or
specified using a prefix-arg.  If `type' is an integer (e.g. a prefix-arg),
then the list will be an ordered list.  Otherwise, the list is unordered.
Any other type is an error.
{jpw: 03/06}"
  (interactive "P")
  ;; Validation check.
  (or (null type)
      (char-or-string-p type)
      (signal 'wrong-type-argument 
              (list 'char-or-string-p type)))
  (if (or (null type)
          (stringp type))
      (html-unordered-list)
    (html-ordered-list)
    )
  )


(defsubst jpw-unfill-skip-line (next-nonws-char)
  (char-equal (char-after next-nonws-char) ?\[)
  )


(defun jpw-lj-display-tag-analysis ()
  "Show the results of `jpw-lj-tag-nested-analysis' in the *Messages*
buffer.
{jpw: 03/06}"
  (interactive)
  (jpw-lj-tag-nested-analysis)
  (message "%S" jpw-lj-tag-analysis-cache)
  )


;;------------------------------------------------------------
;;
;; Bindings: Define Local Keymap
;; 


(defvar jpw-lj-mode-map nil)
(if (null jpw-lj-mode-map)
    (progn
      (setq jpw-lj-mode-map (make-sparse-keymap))
      (define-key jpw-lj-mode-map "\M-\"" 'jpw-unfill-paragraph)

      (define-key jpw-lj-mode-map "\C-c\C-s" 'jpw-lj-display-tag-analysis)

      (define-key jpw-lj-mode-map "\M-gi" 'jpw-html-italic)
      (define-key jpw-lj-mode-map "\M-ge" 'jpw-html-emphasized)
      (define-key jpw-lj-mode-map "\C-ci" 'jpw-lj-insert-italic)

      (define-key jpw-lj-mode-map "\M-gb" 'jpw-html-bold)
      (define-key jpw-lj-mode-map "\M-gs" 'jpw-html-strong)
      (define-key jpw-lj-mode-map "\C-cb" 'jpw-lj-insert-bold)

      (define-key jpw-lj-mode-map "\M-gu" 'jpw-html-underline)

      (define-key jpw-lj-mode-map "\M-gs" 'jpw-html-size)

      (define-key jpw-lj-mode-map "\M-po" 'jpw-html-code)

      (define-key jpw-lj-mode-map "\M-p\C-i" 'html-image)


      (define-key jpw-lj-mode-map "\M-pa" 'html-href-anchor)
      (define-key jpw-lj-mode-map "\M-p\C-u" 'html-href-anchor)

      (define-key jpw-lj-mode-map "\M-pl" 'jpw-lj-insert-list)

      (define-key jpw-lj-mode-map "\M-p*" 'html-list-item)
      (define-key jpw-lj-mode-map "\M-p." 'html-list-item)

      (define-key jpw-lj-mode-map "\C-cp" 'html-paragraph)
      (define-key jpw-lj-mode-map "\M-p\C-m" 'html-paragraph)

      (define-key jpw-lj-mode-map "\M-p\C-j" 'html-line)

      (define-key jpw-lj-mode-map "\M-p-" 'html-horizontal-rule)

      ;; LJ-specific commands
      (define-key jpw-lj-mode-map "\M-pu" 'jpw-lj-user)

      (define-key jpw-lj-mode-map "\M-p=" 'jpw-lj-cut)

      )                                 ;end progn
  )                                     ;end if


;;------------------------------------------------------------
;;
;; Font-Lock Support
;; 


;;
;; Support var, consts, & defsubsts (i.e. ones used by the Font-Lock MATCHER
;; defuns and keyword lists).
;;


(defconst jpw-lj-re-foo "")

(defsubst jpw-lj-search-foo (ulim-pt)
  nil
  )


;;
;; Font-lock MATCHER functions.
;;
;; Must be a function instead of a regexp.  Will be called with
;; a single arg:  the limit of the search, and on success:
;;   1. Must return non-nil
;;   2. Must set `match-data' accordingly.
;;   3. It must move `point' past `match-beginning', or font-lock will loop
;;      forever.
;;


(defun jpw-lj-find-foo (upper-limit)
  ;; Leave docstring blank, except for date.  Use doc-comments, instead.
  "{jpw: 03/06}"
  ;;
  ;; Examines region between `point' and `upper-limit', looking for all
  ;; multiply-nested quotes.  Sets the `match-data' to a list of points as
  ;; follows:
  ;; 
  ;;    ( all-matched-begin-p all-matched-end-p
  ;;      tag-begin-p                tag-end-p
  ;;      start-tag-post-arg-begin-p start-tag-post-arg-end-p
  ;;      singly-quoted-text-begin-p   singly-quoted-text-end-p
  ;;      multiply-quoted-text-begin-p multiply-quoted-text-end-p )
  ;;
  ;; The first 4 elements will never be nil.  Any remaining elements could be
  ;; nil.  There should never be isolated nil-elements, however.  They should
  ;; always be pairs of nil's or pairs of positions.
  ;;
  ;; The `start-tag-post-arg-*-p' pair will be non-nil for a starting quote
  ;; tag.  At the moment, it simply indicates the location of the
  ;; starting-tag's closing square bracket.
  ;;
  ;; The last two pairs, `singly-quoted-text-*-p' and
  ;; `multiply-quoted-text-*-p', are all nil when we find an ending quote tag
  ;; that isn't nested in any other quote tags.  Otherwise *one* of two pairs
  ;; will be nil.  The other pair marks the position of the text between quote
  ;; tags.  As the name implies, which pair is non-nil indicates the type of
  ;; quoted text (i.e. nested in another quote or not).
  ;;
  ;; We have two cases we need to deal with:
  ;;
  ;; 1. We're starting afresh, from the bobp.
  ;; 2. We're starting inside of a tag, one already fontified.  The immediate
  ;;    enclosing tag *may* be a quote.  Or, it may not.  We dunno.
  ;;
  ;; The following code is designed for the first case.  It doesn't work quite
  ;; right with the second case.  However, the second case usually only arises
  ;; during editing.  How often do you alter a quote of what someone else
  ;; said?

;;  (let* (tag1-start-pos 
;;         tag1-end-pos
;;         text-end-pos
;;         quote-match-data
;;         ) ;; end var defs
    nil
;;    ) ;; end let
  )


;;
;; Individual `font-lock-keyword' lists.  Each value is of the same form as an
;; element of `font-lock-keywords'; see that variable's documentation for more
;; info.
;;


(defconst jpw-lj-font-lock-strong-face-key-1
  ;; See jpw-lj-font-lock-underline-face-key-1 for info
  (list
   (concat
    "\\("
    "<strong>"
    "[^<]*\\(<[^/]+/[^s][^>]*>[^<]*\\)*"
    "</strong>"
    "\\)"
    ) ;;end concat
   '(1 'jpw-lj-bold-face prepend)
   ) ;;end list
  ) ;;end defconst


(defconst jpw-lj-font-lock-em-face-key-1
  ;; See jpw-lj-font-lock-underline-face-key-1 for info
  (list
   (concat
    "\\("
    "<em>"
    "[^<]*\\(<[^/]+/[^e][^>]*>[^<]*\\)*"
    "</em>"
    "\\)"    
    ) ;;end concat
   '(1 'jpw-lj-italic-face prepend)
   ) ;;end list
  ) ;;end defconst


(defconst jpw-lj-font-lock-bold-face-key-1
  ;; See jpw-lj-font-lock-underline-face-key-1 for info
  (list
   (concat
    "\\("
    "<b>"
    "[^<]*\\(<[^/]+/[^b][^>]*>[^<]*\\)*"
    "</b>"
    "\\)"
    ) ;;end concat
   '(1 'jpw-lj-bold-face prepend)
   ) ;;end list
  ) ;;end defconst


(defconst jpw-lj-font-lock-italics-face-key-1
  ;; See jpw-lj-font-lock-underline-face-key-1 for info
  (list
   (concat
    "\\("
    "<i>"
    "[^<]*\\(<[^/]+/[^i][^>]*>[^<]*\\)*"
    "</i>"
    "\\)"    
    ) ;;end concat
   '(1 'jpw-lj-italic-face prepend)
   ) ;;end list
  ) ;;end defconst


;; Bold, italic, and underline all use a regexp designed to fontify nested
;; tags.  As a side-benefit, the regexp also matches multiline tag content.
(defconst jpw-lj-font-lock-underline-face-key-1
  (list
   (concat
    "\\("
    "<u>"
    "[^<]*\\(<[^/]+/[^u][^>]*>[^<]*\\)*"
    "</u>"
    "\\)"    
    ) ;;end concat
   '(1 'jpw-lj-underline-face prepend)
   ) ;;end list
  ) ;;end defconst


;; Simple regexp for quoted strings, whether single or multiline.
(defconst jpw-lj-font-lock-string-face-key-2
  (list
   (concat
    "\\(\"[^\"]+\"\\)"
    ) ;;end concat
   '(1 'font-lock-string-face prepend)
   ) ;;end list
  ) ;;end defconst


;; Regexp for nested multiline "big tags".  This regexp is more general than
;; the one for underline, bold, or italic.  It matches tags that are more
;; than one character long, and handles tag attributes.
(defconst jpw-lj-font-lock-size-face-key-2
  (list
   (concat
    "\\(<size\\)"
    "=[^>]+"
    "\\(>\\)"
    "\\([^<]*\\(<[^/]+/[^s][^]]*>[^<]*\\)*\\)"
    "\\(</size>\\)"
    ) ;;end concat
   '(1 'jpw-lj-tag-face t)
   '(2 'jpw-lj-tag-face t)
   '(3 'jpw-lj-size-face append)
   '(5 'jpw-lj-tag-face t)
   ) ;;end list
  ) ;;end defconst


;; Regexp similar to `jpw-lj-font-lock-size-face-key-2', but does not expect
;; tag attributes.
(defconst jpw-lj-font-lock-code-face-key-3
  (list
   (concat
    "\\(<code>\\)"
    "\\([^<]*\\(<[^/]+/[^c][^]]*>[^<]*\\)*\\)"
    "\\(</code>\\)"
    ) ;;end concat
   '(1 'jpw-lj-tag-face t)
   '(2 'jpw-lj-code-face t)
   '(4 'jpw-lj-tag-face t)
   ) ;;end list
  ) ;;end defconst


;; Regexp similar to `jpw-lj-font-lock-size-face-key-2', but does not expect
;; tag attributes.
(defconst jpw-lj-font-lock-pre-face-key-3
  (list
   (concat
    "\\(<pre>\\)"
    "\\([^<]*\\(<[^/]+/[^c][^]]*>[^<]*\\)*\\)"
    "\\(</pre>\\)"
    ) ;;end concat
   '(1 'jpw-lj-tag-face t)
   '(2 'jpw-lj-code-face t)
   '(4 'jpw-lj-tag-face t)
   ) ;;end list
  ) ;;end defconst


;;
;; Variables grouping each font-lock keyword list by font-lock level.
;;


(defconst jpw-lj-font-lock-keywords-1
  (list jpw-lj-font-lock-strong-face-key-1
        jpw-lj-font-lock-em-face-key-1
        jpw-lj-font-lock-bold-face-key-1
        jpw-lj-font-lock-italics-face-key-1
        jpw-lj-font-lock-underline-face-key-1
        ) ;;end list
  )

(defconst jpw-lj-font-lock-keywords-2-common
  (append jpw-lj-font-lock-keywords-1
          (list jpw-lj-font-lock-size-face-key-2
                jpw-lj-font-lock-string-face-key-2
                ) ;;end list
          ) ;;end append
  )
(defconst jpw-lj-font-lock-keywords-2
  (append jpw-lj-font-lock-keywords-2-common
          ) ;;end append
  )

(defconst jpw-lj-font-lock-keywords-3
  (append jpw-lj-font-lock-keywords-2-common
          (list jpw-lj-font-lock-code-face-key-3
                jpw-lj-font-lock-pre-face-key-3
                ) ;;end list
          ) ;;end append
  )


;; The actual value of `font-lock-defaults' for jpw-lj-mode
;;

(defconst jpw-lj-font-lock-defaults
  (list
   ;; Per-level variable names.  Each variable named in this list should be of
   ;; the same form as `font-lock-keywords'
   '(jpw-lj-font-lock-keywords-1 jpw-lj-font-lock-keywords-2
                                 jpw-lj-font-lock-keywords-3)
   nil t
   ;; Syntax table.  None used here.
   '()
   'backward-paragraph ;; Always a reasonable defun to use here.
   ) ;;end list
  )


;;------------------------------------------------------------
;;
;; Define the mode proper
;; 



;;;###autoload (jpw-lj-mode)
(define-derived-mode jpw-lj-mode html-mode "jpw-lj"
  "A major mode for editing LiveJournal messages.  Derived from `html-mode'.

Key bindings:
\\{jpw-lj-mode-map}

{jpw: 03/06}"
  ;; Set up font-lock for jpw-lj-mode
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-multiline)
  (make-local-variable 'font-lock-support-mode)
  (make-local-variable 'lazy-lock-minimum-size)
  (make-local-variable 'lazy-lock-defer-on-the-fly)
  (make-local-variable 'lazy-lock-defer-on-scrolling)
  (make-local-variable 'lazy-lock-defer-contextually)
  (make-local-variable 'lazy-lock-stealth-time)
  (make-local-variable 'lazy-lock-stealth-load)
  (make-local-variable 'lazy-lock-stealth-nice)
  (make-local-variable 'lazy-lock-stealth-verbose)
  (setq font-lock-defaults jpw-lj-font-lock-defaults
        font-lock-multiline t
        ;; `jit-lock-mode' doesn't correctly fontify everything we want it to.
        ;; 
        ;; `fast-lock-mode' and `lazy-lock-mode' require you to
        ;; manually-refontify when editing text in some of the more complex
        ;; multiline expressions.  `fast-lock-mode' works by keeping a cache
        ;; of fontifications.  Great for programming modes.  Bad for modes
        ;; operating on temporary buffers.
        ;; 
        ;; So, we'll use `lazy-lock-mode', suitably tuned to behave as
        ;; desired.
        font-lock-support-mode lazy-lock-mode
        lazy-lock-minimum-size nil
        lazy-lock-stealth-verbose t
        lazy-lock-defer-on-the-fly t
        lazy-lock-defer-on-scrolling nil
        lazy-lock-defer-contextually nil
        lazy-lock-stealth-time 3
        lazy-lock-stealth-nice 0.1
        lazy-lock-stealth-load nil)

  ;; FIXME:
  ;;
  ;; Things needed to circumvent behavior inherited by HTML mode.
  ;; 

  ;;~~~;; Has no effect:
  ;;~~~;;(make-local-variable 'sgml-tag-alist)
  ;;~~~;;(make-local-variable 'sgml-face-tag-alist)
  ;;~~~;;(setq sgml-tag-alist nil
  ;;~~~;;      sgml-face-tag-alist nil)
  ;;~~~;; This doesn't work:
  ;;~~~;;(sgml-mode-common jpw-lj-font-lock-keywords-3 html-display-text)

  ;; TODO:
  ;; Eventually, this will be moved into an if-statement controlled by a
  ;; customization flag.  That flag will also need to perform an unfill-buffer
  ;; before saving/killing.  Will need to build in that functionality, as
  ;; well. 
  (progn (setq fill-column 0) (auto-fill-mode -1))
  )

(global-set-key "\M-p\M-j" 'jpw-lj-mode)


;;------------------------------------------------------------
;;
;; Unit Tests
;; 


;; (defun jpw-lj-test-tag-find-last ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-lj-tag-find-last))
;;             (starttag (car tagspec))
;;             (endtag (cadr tagspec)) )
;;       (cond 
;;        (starttag (goto-char starttag))
;;        (endtag (goto-char endtag))
;;        ))))
;; (defun jpw-lj-test-tag-find-next ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-lj-tag-find-next))
;;             (starttag (car tagspec))
;;             (endtag (cadr tagspec)) )
;;       (cond 
;;        (starttag (goto-char starttag))
;;        (endtag (goto-char endtag))
;;        ))))
;; (defun jpw-lj-test-tag-find-nearest ()
;;   (interactive)
;;   (message "%S" (jpw-lj-tag-find-nearest)))
;; (defun jpw-lj-test-get-tag-last ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-lj-tag-find-last))
;;             (tagval (jpw-lj-get-tag-at tagspec))
;;             )
;;       (message tagval))))
;; (defun jpw-lj-test-get-tag-next ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-lj-tag-find-next))
;;             (tagval (jpw-lj-get-tag-at tagspec))
;;             )
;;       (message tagval))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
