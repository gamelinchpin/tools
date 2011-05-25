;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A major mode for editing phpBB messages.
;;
;;  Copyright © 2005-2010 John P. Weiss
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
;; You can enter this mode using `\M-p\M-P'.
;;
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'custom-defuns)
(require 'jit-lock)


;;------------------------------------------------------------
;;
;; User Customizations
;;


(defgroup phpBB nil
  "Customizable aspects of phpBB-mode.
{jpw: 02/05}"
  :group 'hypermedia
  :group 'local)


(defface phpBB-bold-face
  '((t (:inherit bold)))
  "How to color [b]...[/b] tags."
  :group 'phpBB)

(defface phpBB-italic-face
  '((t (:inherit italic)))
  "How to color [i]...[/i] tags."
  :group 'phpBB)

(defface phpBB-underline-face
  '((t (:inherit underline)))
  "How to color [u]...[/u] tags."
  :group 'phpBB)

(defface phpBB-smiley-face
  '((t (:bold t :foreground "orange")))
  "How to color smileys"
  :group 'phpBB)

(defface phpBB-tag-face
  '((t (:inherit font-lock-function-name-face)))
  "The face to use for non-markup phpBB tags."
  :group 'phpBB)

(defface phpBB-size-face
  '((t (:inherit font-lock-type-face)))
  "The face to use for size markup."
  :group 'phpBB)

(defface phpBB-color-face
  '((t (:foreground "magenta" :background "PaleGreen")))
  "The face to use for color markup."
  :group 'phpBB)

(defface phpBB-quote-face
  '((t (:inherit font-lock-comment-face)))
  "The face to use for quoted text."
  :group 'phpBB)

(defface phpBB-multiply-quoted-face
  '((t (:inherit phpBB-quote-face
        :background "LightCyan")))
  "The face to use for text that has been quoted more than once.
(i.e. a quote that contains another quote)."
  :group 'phpBB)

(defface phpBB-code-face
  '((t (:inherit font-lock-constant-face)))
  "The face to use for code markup."
  :group 'phpBB)

(defface phpBB-url-face
  '((t (:inherit font-lock-keyword-face
        :underline t :bold t)))
  "The face to use for URLs."
  :group 'phpBB)


;;------------------------------------------------------------
;;
;; Variables
;;


(defvar jpw-phpBB-tag-analysis-cache '()
  "Contains the most recent result of a call to `jpw-phpBB-tag-analysis'.
{jpw: 09/2005}")


(defvar jpw-phpBB-t-a-c-l-u-buffer-state '(t 0 0)
  "Internal variable used by `jpw-phpBB-tag-analysis'.
{jpw: 09/2005}")


;;------------------------------------------------------------
;;
;; Tag-Type-Specific Constants
;;


(defconst jpw-tag-finder-re "\\[.+\\]")
(defconst jpw-tag-end-re "\\[/")
(defconst jpw-tag-full-startend-re "\\[/?\\([^]=]+\\)\\(=[^]]+\\)?\\]")


;;------------------------------------------------------------
;;
;; Utility Functions
;;


(defsubst jpw-insert-phpBB-tag (tag)
  "Inserts the begin- and end-tags for the phpBB markup tag, `tag', inserting
them on either side of `point'.  If a region is active, it inserts the two
tags on either side of the region.
{jpw: 2/05}"
  (jpw-insert-markup-tags (concat "[" tag "]") (concat "[/" tag "]"))
  )


(defsubst jpw-insert-phpBB-tag-with-arg (tag arg &optional unquoted)
  "Inserts the begin- and end-tags for a phpBB markup tag that takes an
argument.  `tag' is the name of the tag.  `arg' is the value to use for the
phpBB tag argument.  `arg' will be enclosed in quotes unless the optional
`unquoted' is non-nil.
The tags are inserted on either side of `point' or, if a region is active, on
either side of the region.
{jpw: 2/05}"
  (or (stringp arg)
      (signal 'wrong-type-argument (list 'stringp arg)))
  (if unquoted
      (jpw-insert-markup-tags (concat "[" tag "=" arg "]")
                          (concat "[/" tag "]"))
    ;; else
    (jpw-insert-markup-tags (concat "[" tag "=\"" arg "\"]")
                            (concat "[/" tag "]"))
    );;end if
  )


(defsubst jpw-phpBB-tag-find-last (&optional pos boundpos)
  "Searches backward from `pos' (defaults to `point') for the nearest phpBB
tag.  Returns a list of the form:

\(START-TAG END-TAG\)

Either START-TAG or END-TAG will be nil, indicating the type of tag in that
direction.  If both are nil (i. e. no tags were found), returns nil instead of
a list.

The optional `boundpos' is an integer buffer position.  It stops the search at
that position.  If no tag is found before `boundpos', the function returns
nil.  The default is to search backward to the beginning of the buffer.

Alters `point'.  Be sure to call this only from within a `save-excursion'.
{jpw:03/05}"
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
      );; end if
    (if (or last-start-tag last-end-tag)
        (list last-start-tag last-end-tag))
    );;end let
  )


(defsubst jpw-phpBB-tag-find-next (&optional pos boundpos)
  "Searches forward from `pos' (defaults to `point') for the nearest phpBB
tag.  Returns a list of the form:

\(START-TAG END-TAG\)

Either START-TAG or END-TAG will be nil, indicating the type of tag in that
direction.  If both are nil (i. e. no tags were found), returns nil instead of
a list.

The optional `boundpos' is an integer buffer position.  It stops the search at
that position.  If no tag is found before `boundpos', the function returns
nil.  The default is to search forward to the end of the buffer.

Alters `point'.  Be sure to call this only from within a `save-excursion'.
{jpw:03/05}"
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
      );; end if
    (if (or next-start-tag next-end-tag)
        (list next-start-tag next-end-tag))
    );;end let
  )


(defsubst jpw-phpBB-get-tag-at (&optional pos)
  "Extract the name of the phpBB tag at `pos', or `point' if pos is omitted.
If `pos' is a list, uses the first non-nil element.  (Actually, it checks only
 the `car' or `cadr'.)

Alters `point'.  Be sure to call this only from within a `save-excursion'.
{jpw 03/05}"
  (if pos
      (if (listp pos)
          (goto-char (cond ((car pos)) ((cadr pos))))
        );; end if listp
    ;; else:  scalar
    (goto-char pos)
    )
  (if (looking-at jpw-tag-full-startend-re)
      (match-string-no-properties 1))
  )


(defsubst jpw-phpBB-tag-pos-to-syntactic (search-result)
  "Used by several of the defun's below.  Converts `search-result' the list
returned by `jpw-phpBB-tag-find-last' or `jpw-phpBB-tag-find-next' to a list
of the form:

\(POS IS_ENDTAG TAG_NAME\)

This defun *greatly* simplifies all of the subsequent syntactic analysis code.
{jpw:07/05}"
  (if search-result
      (let ((is-end-tag (and (cadr search-result) t))
            (tag-pos (or (car search-result) (cadr search-result)))
            );;end var defs
        (list tag-pos is-end-tag (jpw-phpBB-get-tag-at tag-pos))
        );;end let
    );; end if
  )


(defsubst jpw-phpBB-tag-find-nearest (&optional pos-last-start
                                                pos-next-end
                                                from-pos
                                                to-pos)
  "Used by `jpw-phpBB-tag-analysis'.  Returns a list of the form:

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
{jpw:07/05}"
  (if (not pos-last-start) (setq pos-last-start (point)))
  (if (not pos-next-end) (setq pos-next-end pos-last-start))
  (let* ((last (jpw-phpBB-tag-pos-to-syntactic
                (jpw-phpBB-tag-find-last pos-last-start from-pos)))
         (next (jpw-phpBB-tag-pos-to-syntactic
                (jpw-phpBB-tag-find-next pos-next-end to-pos)))
        ) ;; end var defs

    (if (or last next)
        (list last next))
    );;end let
  )


(defsubst jpw-phpBB-tag-analysis-ignore (tag)
  "Returns `t' when syntactic tag analysis should ignore `tag', `nil'
otherwise.
{jpw: 07/2005}"
  (equal tag "*")
  )


(defsubst jpw-phpBB-tag-skip-ignorable (nearest
                                        &optional from-pos to-pos)
  "Extension of `jpw-phpBB-tag-find-nearest' which skips tags that
`jpw-phpBB-tag-analysis-ignore' returns `t' for.

`nearest' should be the a list of the same form returned by
`jpw-phpBB-tag-find-nearest', which is what this defun evals to, as well.

`from-pos' and `to-pos' are, respectively, the upper and lower bounds of the
search.  This lets you narrow the tag search to the region between these two
positions without having to call `narrow-to-region'.  The default is to search
the entire buffer.

Alters `point'.  Be sure to call this only from within a `save-excursion'.
{jpw: 07/2005}"
  (and
   nearest
   (let* ((info-last-tag (car nearest))
          (info-next-tag (cadr nearest))
          );;end var defs

     ;; Check the preceding tag
     (while (and info-last-tag
                 (jpw-phpBB-tag-analysis-ignore (caddr info-last-tag)))
       (setq info-last-tag
             (jpw-phpBB-tag-pos-to-syntactic
              (jpw-phpBB-tag-find-last (car info-last-tag) from-pos)))
       );; end backward search

     ;; Check the following tag
     (while (and info-next-tag
                 (jpw-phpBB-tag-analysis-ignore (caddr info-next-tag)))
       (setq info-next-tag
             (jpw-phpBB-tag-pos-to-syntactic
              (jpw-phpBB-tag-find-next (1+ (car info-next-tag)) to-pos)))
       );; end forward search

     ;; The new positions, free of non-syntactic tags.
     (and info-last-tag info-next-tag
          (list info-last-tag info-next-tag))
     );; end let*
   );; end and
  )


(defun jpw-phpBB-tag-find-enclosing (&optional pos-last-start
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

Returns nil if `point' is not enclosed in any phpBB tags whatsoever.

Returns a `nil' TAG when the nearest tags aren't a matching \"[TAG]
... [/TAG]\" pair.
{jpw: 07/2005}"
  (if (not pos-last-start) (setq pos-last-start (point)))
  (if (not pos-next-end) (setq pos-next-end pos-last-start))
  (save-excursion
    (let* ((tmp-nearest-pair
            (if skip-unanalyzable-tags
                (jpw-phpBB-tag-skip-ignorable
                 (jpw-phpBB-tag-find-nearest pos-last-start pos-next-end
                                             from-pos to-pos)
                 from-pos to-pos)
              ;; else
              (jpw-phpBB-tag-find-nearest pos-last-start pos-next-end
                                          from-pos to-pos)
              );;end if
            )
           (info-last-tag (car tmp-nearest-pair))
           (info-next-tag (cadr tmp-nearest-pair))
           (last-tag (caddr info-last-tag))
           (next-tag (caddr info-next-tag))
           nested-tag-count
           );;end var defs

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
           ;; Note:  To prevent `jpw-phpBB-tag-find-next' from "getting
           ;; stuck" at the tag we found on the last iteration, we need to
           ;; start the search one char forward from `pos-next-end'.
           (setq info-next-tag
                 (jpw-phpBB-tag-pos-to-syntactic
                  (jpw-phpBB-tag-find-next (1+ (car info-next-tag)) to-pos)))
           (if (equal last-tag (caddr info-next-tag))
               (if (cadr info-next-tag) ;; is end tag
                   (setq nested-tag-count (1- nested-tag-count))
                 ;; else:  it's a nested start tag
                 (setq nested-tag-count (1+ nested-tag-count))
                 );;end if end-or-start tag
             );;end if equal
           );; end while

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
                 (jpw-phpBB-tag-pos-to-syntactic
                  (jpw-phpBB-tag-find-last (car info-last-tag) from-pos)))
           (if (equal (caddr info-last-tag) next-tag)
               (if (cadr info-last-tag) ;; is end tag
                   (setq nested-tag-count (1+ nested-tag-count))
                 ;; else:  it's a matching start tag
                 (setq nested-tag-count (1- nested-tag-count))
                 );;end if end-or-start tag
             );;end if equal
           );; end while

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
       );; end edge-case `and'
      );;end let
    );;end excursion
  )


(defun jpw-phpBB-tag-nested-analysis ()
  "Search forward and backward from `point' to see what tags we're enclosed
in.  Each pair is the position of the start tag and end tag, respectively.
The list of pairs is built from the inside-out.  The results are stored in the
variable `jpw-phpBB-tag-analysis-cache', in a depth-descending order.

So, `\(car \(caadr \(jpw-phpBB-tag-analysis-cache\)\)\)' would return the
position of the outermost start tag.

The result of analysis is nil if `point' is not enclosed in any phpBB tags.
{jpw: 09/2005}"
  (let* ((buffer-state (list (buffer-modified-p) (buffer-modified-tick)
                             (buffer-size)))
         ;; Point the iterator at the cdr, so we can manipulate the
         ;; cache through the iterator.
         (cache-pos-cdr-iter (cons 'nil jpw-phpBB-tag-analysis-cache))
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
         );;end var defs

    ;; Examine cached state, removing any state no longer consistent with our
    ;; current position.
    (if (equal jpw-phpBB-t-a-c-l-u-buffer-state buffer-state)
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
                (setq jpw-phpBB-tag-analysis-cache '())
                );;end if
            );;end if taginfo-from-iter
          ) ;;end progn
      ;;
      ;; else:
      ;; Force a full reanalysis.
      (setq jpw-phpBB-tag-analysis-cache '())
      );;end if-same-buffer-state

    ;; We still want to attempt a constrained rebuild, however, as we may now
    ;; be inside of a more deeply nested tag.  Consider this example:
    ;;     <h1>dolor ipse <em>lorem</em> sit</h1>
    ;; If point was at "i" on initial analysis, then moved to the "r" in
    ;; "lorem," we couldn't use the cached analysis, now, could we?
    (save-excursion
      ;; Reset the buffer state and start rebuilding.
      (setq jpw-phpBB-t-a-c-l-u-buffer-state buffer-state
            current-tagspec (jpw-phpBB-tag-find-enclosing nil
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
              (jpw-phpBB-tag-find-enclosing (1- (caadr current-tagspec))
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
    (setq jpw-phpBB-tag-analysis-cache
          (nconc jpw-phpBB-tag-analysis-cache analysis-cache-working))
    );;end let
  )


(defsubst jpw-phpBB-unfill-skip-line (next-nonws-char)
  (or (char-equal (char-after next-nonws-char) ?\[)
      (char-equal (char-after next-nonws-char) ?:))
  )


;;------------------------------------------------------------
;;
;; Mode Interactive Functions
;;


(defun phpBB-insert-italic ()
  "Insert phpBB italics tags, or puts the active region inside phpBB italics
tags.
{jpw: 2/05}"
  (interactive)
  (jpw-insert-phpBB-tag "i")
  )


(defun phpBB-insert-bold ()
  "Insert phpBB bold tags, or puts the active region inside phpBB bold
tags.
{jpw: 2/05}"
  (interactive)
  (jpw-insert-phpBB-tag "b")
  )


(defun phpBB-insert-underline ()
  "Insert phpBB underline tags, or puts the active region inside phpBB
underline tags.
{jpw: 2/05}"
  (interactive)
  (jpw-insert-phpBB-tag "u")
  )


(defun phpBB-insert-size (sz)
  "Insert phpBB size tags, or puts the active region inside phpBB color
tags.
{jpw: 2/05}"
  (interactive "NSize: ")
  (jpw-insert-phpBB-tag-with-arg "size" (format "%d" sz) t)
  )


(defun phpBB-insert-url (url)
  "Insert a phpBB hyperlink at point or around the active region.  `url' is
the URL to refer to.  If `url' is `nil' or the empty string, assumes that you
will specify the URL as the tag's contents.
{jpw: 2/05}"
  (interactive "sURL: ")
  (if (or (null url) (equal "" url))
      (jpw-insert-phpBB-tag "url")
    ;;else
    (jpw-insert-phpBB-tag-with-arg "url" url t)
    )
  )


(defun phpBB-insert-image ()
  "Insert a phpBB hyperlink to an image at point or around the active region.
The URL of the image goes inside the tags.
{jpw: 2/05}"
  (interactive)
  (jpw-insert-phpBB-tag "img")
  )


(defun phpBB-insert-code ()
  "Insert phpBB code tags, or puts the active region inside phpBB list
tags.
{jpw: 2/05}"
  (interactive)
  (jpw-insert-phpBB-tag "code")
  )


(defun phpBB-insert-color (color)
  "Insert phpBB color tags, or puts the active region inside phpBB color
tags.
{jpw: 2/05}"
  (interactive "sColor or #RRGGBB Code: ")
  (jpw-insert-phpBB-tag-with-arg "color" color t)
  )


(defun phpBB-insert-quote (quoting-who)
  "Insert phpBB quote tags, or puts the active region inside phpBB quote
tags.
{jpw: 2/05}"
  (interactive "sWho are you quoting?  ")
  (jpw-insert-phpBB-tag-with-arg "quote" quoting-who)
  )


(defun phpBB-insert-list-item ()
  "Insert a phpBB list item tag at point.
{jpw: 2/05}"
  (interactive)
  (insert "[*] ")
  )


(defun phpBB-insert-list (&optional type)
  "Insert phpBB list tags, or puts the active region inside phpBB list
tags.
The optional `type' specifies the type of list.  It can be passed directly or
specified using a prefix-arg.  If `type' is a string, that string is used
directly as the type attribute.  If `type' is an integer (e.g. a prefix-arg),
then its sign determines the type of list:
  `type' > 0 : Enumerated list, lowercase letters.
  `type' == 0 : Numbered list, lowercase Roman numerals
Any other type is an error.

When no arg is specified, the opening \"[list]\" tag contains no type, and
will therefore be a bullet-list.
{jpw: 2/05}"
  (interactive "P")
  ;; Validation check.
  (or (null type)
      (char-or-string-p type)
      (signal 'wrong-type-argument
              (list 'char-or-string-p type)))
  (let ((type-str (and (not (null type))
                       (or (and (stringp type) type)
                           (and (> type 0) "1")
                           (and (<= type 0) "a")
                           )
                       ));;end (type-str (and ...
        );; end vardefs.
    (if type-str
        (jpw-insert-phpBB-tag-with-arg "list" type-str t)
      ;;else
      (jpw-insert-phpBB-tag "list")
      );; end if
    );; end let
  )


(defun phpBB-break-quote ()
  "Split the current quote in twain, at point.  Starts the next quote with the
same name.

Evals to `nil' and does nothing if we're not inside of a quote environment.
{jpw: 7/05}"
  (interactive)
  (let (enclosing-tag
        quote-arg
        );;end vardefs.
    (save-excursion
      (setq enclosing-tag (jpw-phpBB-tag-find-enclosing))
      (and enclosing-tag
           (equal "quote" (car enclosing-tag))
           (progn
             (goto-char (+ 6 (caadr enclosing-tag)))
             (setq quote-arg (if (looking-at "\\(=[^]]+\\)")
                                 (match-string-no-properties 1)
                               ""))
             );;end progn
           );;end and
      );; end excursion

    ;; If we're inside a quote-tag, split it.
    (if (and enclosing-tag quote-arg)
        (jpw-insert-markup-tags "[/quote]\n"
                                (concat "\n[quote" quote-arg "]"))
      );;end if
    );; end let
  )


(defun jpw-phpBB-display-tag-analysis ()
  "Show the results of `jpw-phpBB-tag-nested-analysis' in the *Messages*
buffer.
{jpw: 09/2005}"
  (interactive)
  (jpw-phpBB-tag-nested-analysis)
  (message "%S" jpw-phpBB-tag-analysis-cache)
  )


(defun jpw-phpBB-unfill-paragraph (&optional remove-blank-line)
  "Behaves almost identically to `jpw-unfill-paragraph' with one exception:
lines that begin with a phpBB tag are not unfilled.
{jpw: 03/2006}"
  (interactive "P")
  (jpw-unfill-paragraph-engine (not (eq remove-blank-line nil))
                               'jpw-phpBB-unfill-skip-line)
  )


;;------------------------------------------------------------
;;
;; Bindings: Define Local Keymap
;;


(defvar phpBB-mode-map nil)
(if (null phpBB-mode-map)
    (progn
      (setq phpBB-mode-map (make-sparse-keymap))
      (define-key phpBB-mode-map "\M-\"" 'jpw-phpBB-unfill-paragraph)

      (define-key phpBB-mode-map "\C-c\C-s" 'jpw-phpBB-display-tag-analysis)

      (define-key phpBB-mode-map "\M-oi" 'phpBB-insert-italic)
      (define-key phpBB-mode-map "\M-pi" 'phpBB-insert-italic)

      (define-key phpBB-mode-map "\M-ob" 'phpBB-insert-bold)
      (define-key phpBB-mode-map "\M-pb" 'phpBB-insert-bold)

      (define-key phpBB-mode-map "\M-ou" 'phpBB-insert-underline)
      (define-key phpBB-mode-map "\M-pu" 'phpBB-insert-underline)

      (define-key phpBB-mode-map "\M-os" 'phpBB-insert-size)
      (define-key phpBB-mode-map "\M-ps" 'phpBB-insert-size)

      (define-key phpBB-mode-map "\M-oc" 'phpBB-insert-color)
      (define-key phpBB-mode-map "\M-pc" 'phpBB-insert-color)

      (define-key phpBB-mode-map "\M-pa" 'phpBB-insert-url)
      (define-key phpBB-mode-map "\M-p\C-u" 'phpBB-insert-url)

      (define-key phpBB-mode-map "\M-p\C-i" 'phpBB-insert-image)

      (define-key phpBB-mode-map "\M-pl" 'phpBB-insert-list)

      (define-key phpBB-mode-map "\M-p*" 'phpBB-insert-list-item)

      (define-key phpBB-mode-map "\M-po" 'phpBB-insert-code)

      (define-key phpBB-mode-map "\M-pq" 'phpBB-break-quote)

      (define-key phpBB-mode-map "\M-pQ" 'phpBB-insert-quote)
      );end progn
  );end if


;;------------------------------------------------------------
;;
;; Font-Lock Support
;;


;;
;; Support var, consts, & defsubsts (i.e. ones used by the Font-Lock MATCHER
;; defuns and keyword lists).
;;


(defconst phpBB-re-quote-start
  "\\(\\[quote\\)\\(=[^]]+\\)?\\(\\]\\)")

(defconst phpBB-re-quote-end "\\(\\[/quote\\]\\)")

(defconst phpBB-re-mid-quote-markup
  "\\([^[]*\\(\\[[^/]+/[^q][^]]*\\][^[]*\\)*\\)")

(defvar phpBB-nested-quote-level 0)

(defsubst phpBB-search-next-quote-tag (ulim-pt)
  (re-search-forward "\\[/?quote" ulim-pt t)
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


(defun phpBB-find-nested-quotes (upper-limit)
  ;; Leave docstring blank, except for date.  Use doc-comments, instead.
  "{jpw: 10/05}"
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

  (let* (tag1-start-pos tag1-end-pos text-end-pos
         quote-match-data
         );; end var defs

    ;; Is font-lock refontifying in the middle of the text?  If not, reset the
    ;; quote tag nesting counter.
    (if (bobp) (setq phpBB-nested-quote-level 0))
    ;; Note:  We _will_ NOT try to determine our nesting level if we're in the
    ;; middle of the buffer.  Yes, we may be refontifying a specific region.
    ;;
    ;; BUT - we may already be fontifying the text.  We want back-to-back
    ;; calls to this defun by the font-lock engine to work, quickly, and with
    ;; no side-effects.
    ;;
    ;; So, we'll have to trade some accuracy during region refontification
    ;; for correctness of full buffer refontification.

    (and
     (phpBB-search-next-quote-tag upper-limit)
     ;; Only reaches here if we had a match, so no danger of a nil here.
     (setq tag1-start-pos (match-beginning 0))
     ;; Set the `0th-match' start of the first text we match.  We'll set the
     ;; end point of the 0th-match later on; use a placeholder-nil for now.
     (setq quote-match-data (list tag1-start-pos nil))
     ;; Start or end quote tag?
     (goto-char tag1-start-pos)
     (if (looking-at phpBB-re-quote-start)
         ;; N.B.:  Must use `looking-at' to make sure we capture all parts of
         ;; the start-tag.
         (progn
           ;; Start quote tag.
           ;; Set the 1st and 2nd matches to the tag itself, ignoring any tab
           ;; attribute in the middle.  Also bumps the quote nesting level.
           (nconc quote-match-data
                  (list (match-beginning 1) (match-end 1)
                        (match-beginning 3) (match-end 3)
                        )
                  )
           (setq phpBB-nested-quote-level (1+ phpBB-nested-quote-level))
           ;; end quote start-tag case.
           t);; end progn [enforce true result]
       ;;else
       ;; End quote tag.
       ;; No need for (looking-at phpBB-re-quote-end); we know that the
       ;; end-quote tag is 8 chars long.
       ;; Set the 1st match to the tag itself.  The 2nd match is unused for
       ;; an end tag.   Also drops the quote nesting level.
       (nconc quote-match-data
              (list tag1-start-pos (+ tag1-start-pos 8)
                    nil nil)
              )
       (setq phpBB-nested-quote-level (1- phpBB-nested-quote-level))
       ;;end quote end-tag case.
       t) ;;end if [enforce true result]

     ;; Note that we'll incorrectly identify quote start-tags as end-tags if
     ;; there are any errors whatsoever in `phpBB-re-quote-start'.

     ;; Prepwork, pre-search.
     (goto-char (match-end 0))
     (setq tag1-end-pos (point))

     ;; Look for the next quote-tag, but only if we're still inside the quote
     ;; (i.e. last quote tag was a start tag or a nested end tag).
     (if (and (> phpBB-nested-quote-level 0)
              (phpBB-search-next-quote-tag upper-limit))
         (progn
           (setq text-end-pos (match-beginning 0))
           (if (> phpBB-nested-quote-level 1)
               ;; This is multiply-quoted text.
               ;; The 3rd-match is, therefore, unused.  The 4th-match marks
               ;; the text between the quote-tags.
               (nconc quote-match-data
                      (list nil nil tag1-end-pos text-end-pos))
             ;; else
             ;; This is singly-quoted text.
             ;; The 3rd-match marks the text between the quote-tags.  The
             ;; 4th-match is now unused.
             (nconc quote-match-data
                    (list tag1-end-pos text-end-pos nil nil))
             );; end if quote-level
           ;; Remember that placeholder-nil in the "0th-match"?  Now we set
           ;; it, to the end of the matched text.
           (setcar (cdr quote-match-data) text-end-pos)
           (goto-char text-end-pos)
           );;end progn
       ;;else
       ;; No more quote tags.  Reset the quote-level and set the
       ;; placeholder-nil to the end of what we did find.
       (setq phpBB-nested-quote-level 0)
       (setcar (cdr quote-match-data) tag1-end-pos)
       );end if
     ;; Need to do this because `set-match-data' may eval to nil.
     (or (set-match-data quote-match-data) t)
     ;;(message "found: %S" (match-data))
     );; end and
    );; end let
  )


;;
;; Individual `font-lock-keyword' lists.  Each value is of the same form as an
;; element of `font-lock-keywords'; see that variable's documentation for more
;; info.
;;


(defconst phpBB-font-lock-bold-face-key-1
  ;; See phpBB-font-lock-underline-face-key-1 for info
  (list
   (concat
    "\\("
    "\\[b\\]"
    "[^[]*\\(\\[[^/]+/[^b]\\][^[]*\\)*"
    "\\[/b\\]"
    "\\)"
    );;end concat
   '(1 'phpBB-bold-face prepend)
   );;end list
  );;end defconst


(defconst phpBB-font-lock-italics-face-key-1
  ;; See phpBB-font-lock-underline-face-key-1 for info
  (list
   (concat
    "\\("
    "\\[i\\]"
    "[^[]*\\(\\[[^/]+/[^i]\\][^[]*\\)*"
    "\\[/i\\]"
    "\\)"
    );;end concat
   '(1 'phpBB-italic-face prepend)
   );;end list
  );;end defconst


;; Bold, italic, and underline all use a regexp designed to fontify nested
;; tags.  As a side-benefit, the regexp also matches multiline tag content.
(defconst phpBB-font-lock-underline-face-key-1
  (list
   (concat
    "\\("
    "\\[u\\]"
    "[^[]*\\(\\[[^/]+/[^u]\\][^[]*\\)*"
    "\\[/u\\]"
    "\\)"
    );;end concat
   '(1 'phpBB-underline-face prepend)
   );;end list
  );;end defconst


(defconst phpBB-font-lock-smiley-face-key-1
  (list
   (concat
    "\\(;)\\|:"
    "\\(\\w+:\\|"
    "\\S \\s \\)"
    "\\)"
    );;end concat
   '(1 'phpBB-smiley-face t)
   );;end list
  );;end defconst


;; Simple regexp for quoted strings, whether single or multiline.
(defconst phpBB-font-lock-string-face-key-2
  (list
   (concat
    "\\(\"[^\"]+\"\\)"
    );;end concat
   '(1 'font-lock-string-face prepend)
   );;end list
  );;end defconst


;; Regexp for nested multiline "big tags".  This regexp is more general than
;; the one for underline, bold, or italic.  It matches tags that are more
;; than one character long, and handles tag attributes.
(defconst phpBB-font-lock-size-face-key-2
  (list
   (concat
    "\\(\\[size\\)"
    "=[^]]+"
    "\\(\\]\\)"
    "\\([^[]*\\(\\[[^/]+/[^s][^]]*\\][^[]*\\)*\\)"
    "\\(\\[/size\\]\\)"
    );;end concat
   '(1 'phpBB-tag-face t)
   '(2 'phpBB-tag-face t)
   '(3 'phpBB-size-face append)
   '(5 'phpBB-tag-face t)
   );;end list
  );;end defconst


;; Regexp similar to `phpBB-font-lock-size-face-key-2', but does not expect
;; tag attributes.
(defconst phpBB-font-lock-code-face-key-3
  (list
   (concat
    "\\(\\[code\\]\\)"
    "\\([^[]*\\(\\[[^/]+/[^c][^]]*\\][^[]*\\)*\\)"
    "\\(\\[/code\\]\\)"
    );;end concat
   '(1 'phpBB-tag-face t)
   '(2 'phpBB-code-face t)
   '(4 'phpBB-tag-face t)
   );;end list
  );;end defconst


(defconst phpBB-font-lock-list1-face-key-3
  (list
   (concat
    "\\("
    "\\[\\(\\*\\|/list\\)\\]"
    "\\)"
    );;end concat
   '(1 'phpBB-tag-face t)
   );;end list
  );;end defconst


(defconst phpBB-font-lock-list2-face-key-3
  (list
   (concat
    "\\(\\[list\\)"
    "\\(=\\([^]]\\)+\\)?"
    "\\(\\]\\)"
    );;end concat
   '(1 'phpBB-tag-face t)
   '(3 'phpBB-tag-face nil t)
   '(4 'phpBB-tag-face t)
   );;end list
  );;end defconst


;; Regexp matching single-line tag + attributes.
(defconst phpBB-font-lock-url-face-key-3
  (list
   (concat
    "\\(\\[url\\)"
    "\\(=\\([^]]+\\)\\)?"
    "\\(\\]\\)"
    "\\(.+\\)"
    "\\(\\[/url\\]\\)"
    );;end concat
   '(1 'phpBB-tag-face t)
   '(3 'font-lock-string-face nil t)
   '(4 'phpBB-tag-face t)
   '(5 'phpBB-url-face prepend)
   '(6 'phpBB-tag-face t)
   );;end list
  );;end defconst


;; Regexp matching single-line tag w/o attributes.
(defconst phpBB-font-lock-img-face-key-3
  (list
   (concat
    "\\(\\[img\\]\\)"
    "\\(.+\\)"
    "\\(\\[/img\\]\\)"
    );;end concat
   '(1 'phpBB-tag-face t)
   '(2 'phpBB-url-face prepend)
   '(3 'phpBB-tag-face t)
   );;end list
  );;end defconst


;; Regexp similar to `phpBB-font-lock-size-face-key-2', but does not match
;; multiline content.
(defconst phpBB-font-lock-color-face-key-3
  (list
   (concat
    "\\(\\[color\\)"
    "=[^]]+"
    "\\(\\]\\)"
    "\\(.+\\)"
    "\\(\\[/color\\]\\)"
    );;end concat
   '(1 'phpBB-tag-face t)
   '(2 'phpBB-tag-face t)
   '(3 'phpBB-color-face append)
   '(4 'phpBB-tag-face t)
   );;end list
  );;end defconst


(defconst phpBB-font-lock-quote-face-key-2
  (list
   (concat
    phpBB-re-quote-start         ; 3 groupings
    phpBB-re-mid-quote-markup    ; 2 grouping
    phpBB-re-quote-end           ; 1 grouping
    );;end concat
   '(1 'phpBB-tag-face t)
   '(3 'phpBB-tag-face t)
   '(4 'phpBB-quote-face append)
   '(6 'phpBB-tag-face t)
   );;end list
  );;end defconst

(defconst phpBB-font-lock-multi-quote-face-key-3
  (list
   'phpBB-find-nested-quotes
   '(1 'phpBB-tag-face t)
   '(2 'phpBB-tag-face t t)
   '(3 'phpBB-quote-face append t)
   '(4 'phpBB-multiply-quoted-face append t)
   );;end list
  );;end defconst


;;
;; Variables grouping each font-lock keyword list by font-lock level.
;;


(defconst phpBB-font-lock-keywords-1
  (list phpBB-font-lock-bold-face-key-1
        phpBB-font-lock-italics-face-key-1
        phpBB-font-lock-underline-face-key-1
        phpBB-font-lock-smiley-face-key-1
        );;end list
  )

(defconst phpBB-font-lock-keywords-2-common
  (append phpBB-font-lock-keywords-1
          (list phpBB-font-lock-size-face-key-2
                phpBB-font-lock-string-face-key-2
                );;end list
          );;end append
  )
(defconst phpBB-font-lock-keywords-2
  (append phpBB-font-lock-keywords-2-common
          (list phpBB-font-lock-quote-face-key-2)
          );;end append
  )

(defconst phpBB-font-lock-keywords-3
  (append phpBB-font-lock-keywords-2-common
          (list phpBB-font-lock-list1-face-key-3
                phpBB-font-lock-list2-face-key-3
                phpBB-font-lock-multi-quote-face-key-3
                ;; The phpBB-font-lock-multi-quote-face-key-3 successfully
                ;; fontifies plain quotes, too.  So, we don't need it here.
                phpBB-font-lock-code-face-key-3
                phpBB-font-lock-color-face-key-3
                phpBB-font-lock-url-face-key-3
                phpBB-font-lock-img-face-key-3
                );;end list
          );;end append
  )


;; The actual value of `font-lock-defaults' for phpBB-mode
;;

(defconst phpBB-font-lock-defaults
  (list
   ;; Per-level variable names.  Each variable named in this list should be of
   ;; the same form as `font-lock-keywords'
   '(phpBB-font-lock-keywords-1 phpBB-font-lock-keywords-2
     phpBB-font-lock-keywords-3)
   nil t
   ;; Syntax table.  None used here.
   '()
   'backward-paragraph ;; Always a reasonable defun to use here.
   );;end list
  )


;;------------------------------------------------------------
;;
;; Define the mode proper
;;



;;;###autoload (phpBB-mode)
(define-derived-mode phpBB-mode text-mode "phpBB"
  "A major mode for editing phpBB messages.  Derived from `text-mode'.

Key bindings:
\\{phpBB-mode-map}

{jpw: 02/05}"
  ;; Set up font-lock for phpBB-mode
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-multiline)
  (make-local-variable 'font-lock-support-mode)
  (make-local-variable 'jit-lock-contextually)
  (make-local-variable 'jit-lock-stealth-time)
  (make-local-variable 'jit-lock-stealth-load)
  (make-local-variable 'jit-lock-stealth-nice)
  (make-local-variable 'jit-lock-stealth-verbose)
  (setq font-lock-defaults phpBB-font-lock-defaults
        font-lock-multiline t
        ;; `jit-lock-mode' doesn't correctly fontify everything we want it to.
        ;;
        ;; `fast-lock-mode' and `jit-lock-mode' require you to
        ;; manually-refontify when editing text in some of the more complex
        ;; multiline expressions.  `fast-lock-mode' works by keeping a cache
        ;; of fontifications.  Great for programming modes.  Bad for modes
        ;; operating on temporary buffers.
        ;;
        ;; So, we'll use `jit-lock-mode', suitably tuned to behave as
        ;; desired.
        font-lock-support-mode 'jit-lock-mode
        jit-lock-stealth-verbose t
        jit-lock-contextually nil
        jit-lock-stealth-time 3
        jit-lock-stealth-nice 0.1
        jit-lock-stealth-load nil)
  ;; TODO:
  ;; Eventually, this will be moved into an if-statement controlled by a
  ;; customization flag.  That flag will also need to perform an unfill-buffer
  ;; before saving/killing.  Will need to build in that functionality, as
  ;; well.
  (progn (setq fill-column 0) (auto-fill-mode -1))
  )

(global-set-key "\M-p\M-P" 'phpBB-mode)


;; How to define this as a minor mode.
;; I may make this a minor mode in the future if that proves necessary (i.e. I
;; want to use it with a major mode other than `text-mode'.
(if nil
    (easy-mmode-define-minor-mode phpBB-mode
     "A minor mode for editing phpBB messages.
Key bindings:
\\{phpBB-mode-map}

{jpw: 02/05}"
     nil "phpBB" phpBB-mode-map)
  );;end if


;;------------------------------------------------------------
;;
;; Unit Tests
;;


;; (defun jpw-phpBB-test-tag-find-last ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-phpBB-tag-find-last))
;;             (starttag (car tagspec))
;;             (endtag (cadr tagspec)) )
;;       (cond
;;        (starttag (goto-char starttag))
;;        (endtag (goto-char endtag))
;;        ))))
;; (defun jpw-phpBB-test-tag-find-next ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-phpBB-tag-find-next))
;;             (starttag (car tagspec))
;;             (endtag (cadr tagspec)) )
;;       (cond
;;        (starttag (goto-char starttag))
;;        (endtag (goto-char endtag))
;;        ))))
;; (defun jpw-phpBB-test-tag-find-nearest ()
;;   (interactive)
;;   (message "%S" (jpw-phpBB-tag-find-nearest)))
;; (defun jpw-phpBB-test-get-tag-last ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-phpBB-tag-find-last))
;;             (tagval (jpw-phpBB-get-tag-at tagspec))
;;             )
;;       (message tagval))))
;; (defun jpw-phpBB-test-get-tag-next ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-phpBB-tag-find-next))
;;             (tagval (jpw-phpBB-get-tag-at tagspec))
;;             )
;;       (message tagval))))
;; (defun phpBB-test-find-nested-quotes ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (phpBB-find-nested-quotes (point-max))
;;     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
