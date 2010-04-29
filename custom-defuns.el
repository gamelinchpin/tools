;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Functions
;;
;;  Copyright © 1995-2010 John P. Weiss
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
;; My custom functions.  Used as tools and bound to keys, or used as
;; autohooks, or just basic building-block functions.
;;
;;
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile
  (require 'cperl-mode)
;;  (require 'sgml-mode)
  (require 'xml-lite)
  (require 'sql))
(require 'skeleton)
(require 'tempo)


;;----------------------------------------------------------------------
;;
;; Customization/Setup Tools
;;


(defun jpw-win-or-unix (win-val unix-val)
  "Return the appropriate value, depending on which OS we're running in.
{jpw: 09/2006}"
  (if is-winblows
      win-val
    ;; else
    unix-val
    )
  )


(defun jpw-custom-set-variables-nonsaved  (&rest args)
  "Initializes the default value of a customization variable.

Calls `custom-set-variables' on the list of arguments, then converts the
\"saved value\" to the \"default value\".  This prevents localized
customizations from being written to your \".emacs\" file.

Note that this function may require modification whenever `cus-edit.el'
changes.  {jpw: 9/2004}"
  (apply 'custom-set-variables args)
  (while args
    (let ((entry (car args)))
      (if (listp entry)
          (let* ((symbol (nth 0 entry))
                 (value (get symbol 'saved-value))
                 )
            (if value
                (progn
                 (put symbol 'standard-value value)
                 (put symbol 'saved-value nil)
                 )) ;; end if
            );; end let*
        ));; end (let ... (if ...
    (setq args (cdr args))
    );; end while
  )


(defun jpw-custom-set-faces-nonsaved  (&rest args)
  "Initializes the default value of a customizable face.

Calls `custom-set-faces' on the list of arguments, then converts the
\"saved value\" to the \"default value\".  This prevents localized
customizations from being written to your \".emacs\" file.

Note that this function may require modification whenever `cus-edit.el' and
`cus-faces.el' changes.  {jpw: 9/2004}"
  (apply 'custom-set-faces args)
  (while args
    (let ((entry (car args)))
      (if (listp entry)
          (let* ((face (nth 0 entry))
                 (spec (get face 'saved-face))
                 )
            (if spec
                (progn
                 (make-empty-face face)
                 (face-spec-set face spec)
                 (put face 'saved-face nil)
                 )) ;; end if
            );; end let*
        ));; end (let ... (if ...
    (setq args (cdr args))
    );; end while
  )


(if running-xemacs
    ;; Replace several "missing" GNU Emacs functions
    (progn

      (defun set-face-bold-p (face boldp &rest args)
        "A little placeholder for a GNU Emacs function missing from XEmacs.
Note that this fn. only sets a face bold.  It cannot unset it.
{jpw: 9/2004}"
        (if boldp
            (set-face-font face [bold])
          ;; else
          ;;(set-face-font face [])
          )
        )

      (defun line-beginning-position (&optional n)
        (save-excursion
          (beginning-of-line n)
          (point)
          )
        )

      (defun line-end-position (&optional n)
        (save-excursion
          (end-of-line n)
          (point)
          )
        )

      );;end progn

  ;; else
  (progn
    (defun jpw-cust-colorful-modeline ()
      (let ( (modeline-active-modeflags
              (if is-version-twentytwo
                  '((class color) (min-colors 88))
                ;; else
                t
                );; endif
              )
             (modeline-lowcolor-modeflags
              (if is-version-twentytwo
                  '((class color) (min-colors 8))
                ;; else
                nil
                );; endif
              )
             );; end varbindings

        (jpw-custom-set-faces-nonsaved
         (list 'mode-line
               (list (list modeline-active-modeflags
                           '(:background "plum3"
                             :foreground "black"
                             :box (:line-width -1
                                   :style released-button)))
                     (list modeline-lowcolor-modeflags
                           '(:background "magenta"
                             :foreground "black"))
                     )
               nil
               "For when I'm in the mood for a more colorful modeline, use
                this."
               )
         '(mode-line-highlight
           ((t (:foreground "green4"
                :box (:line-width 2 :color "grey40" :style released-button))))
           nil
           "Just using a box with a darker gray is unsatisfying.  Let's
            change the text color to something that will stand out (but not
            water our eyes).  Change the modeline color, and we may need to
            change this."
           )
         (list 'mode-line-inactive
               (list (list modeline-active-modeflags
                           '(:inherit mode-line
                             :background "LemonChiffon3"
                             :foreground "grey20"
                             :box (:line-width -1 :style released-button)
                             :weight light))
                     (list modeline-lowcolor-modeflags
                           '(:inherit mode-line
                             :background "gray"))
                     )
           nil
           "To accompany my more colorful modeline, I'll pick an off-white
            color for the inactive modeline."
           )
         )
        );; end let
      )
    );; end progn:  else

  );; end if running-xemacs


;;----------------------------------------------------------------------
;;
;; Building Blocks
;;


(defsubst jpw-insert-markup-tags (start-tag end-tag)
  "Inserts the two markup tags, `start-tag' and `end-tag', on either side of
`point'.  If a region is active, it inserts the two tags on either side of the
region.
{jpw: 2/2005}"
  (let* ((start-tag-point (or (and mark-active (region-beginning))
                              (point)))
         (end-tag-point (or (and mark-active (region-end))
                            (point)))
         (must-pop-mark mark-active)
         );end bindings
    (if mark-active (push-mark))
    (save-excursion
      ;; Insert end tag first to preserve position of start tag.
      (goto-char end-tag-point)
      (insert end-tag)
      (goto-char start-tag-point)
      (insert-before-markers start-tag)
      );end excursion
    (if must-pop-mark (pop-mark))
    t ;; To keep the message buffer quiet (`pop-mark' is noisy).
    );end let
  )


;; FIXME:  Move this defun into a more appropriate elisp file.
(defun jpw-insert-xml-tag (tag)
  "Inserts the begin- and end-tags for the XML entity named `tag', inserting
them on either side of `point'.  If a region is active, it inserts the two
tags on either side of the region.
{jpw: 2/2005}"
  (jpw-insert-markup-tags (concat "<" tag ">") (concat "</" tag ">"))
  )


;;----------------------------------------------------------------------
;;
;; Editing Tools
;;


(defun ebuffer-files (buffer-A buffer-B &optional startup-hooks job-name)
  "Like `ebuffers', does an `ediff' on the two buffers underlying files.
{jpw: 9/2006}"
  ;; [jpw] Stolen lock, stock, and barrel from "ediff.el"
  (interactive
   (let (bf)
     ;; [jpw] This "if"-block is my own custom tweak.
     (if (not (and (functionp 'ediff-files)
                   (functionp 'ediff-other-buffer)
                   )
              )
         (load "ediff")
       )
     (list (setq bf (read-buffer "Buffer A to compare: "
                                 (ediff-other-buffer "") t))
           (read-buffer "Buffer B to compare: "
                        (progn
                          ;; realign buffers so that two visible bufs will be
                          ;; at the top
                          (save-window-excursion (other-window 1))
                          (ediff-other-buffer bf))
                        t))))
  (ediff-files (buffer-file-name (get-buffer buffer-A))
               (buffer-file-name (get-buffer buffer-B))
               startup-hooks)
  )


(defun kill-ring-save-entire-buffer ()
  "Like kill-ring-save, but grabs the entire buffer."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Saved all (visible) buffer text.")
  )


(defun join-next-line ()
  "Join the current line to the next line.
{jpw: 2/1999}"
  (interactive)
  (end-of-line)
  (let ((oldeolpos (point)))
    (forward-line 1)
    (back-to-indentation)
    (delete-region oldeolpos (point))
    )
  )


(defsubst jpw-unfill-paragraph-engine (remove-blank-lines
                                       jpw-unfill-skip-line)
  "Takes paragraphs separated by blank lines and merges the paragraph into a
single line.  The inter-paragraph blank lines are preserved by default.

Evaluates to `nil' if this is the current paragraph is the last paragraph in
the buffer.  Evaluates to `t' otherwise.

When `remove-blank-lines' is `t', all consecutive inter-paragraph blank lines
{i.e. empty lines} will be removed.  If set to `collapse', the consecutive
inter-paragraph blank lines will be collapsed into a single blank line.  If
set to `first', the first inter-paragraph blank line will be removed.  The
others will be left alone.

`jpw-unfill-skip-line' should be either nil or the name of a function to
call.  The function will take a single arg, the position of the first
non-whitespace character on a line.  If that line should not be unfolded into
the preceding one, the function specified in `jpw-unfill-skip-line' must eval
to `nil'.
{jpw: 03/2006}"
  (save-excursion
    ;; Position at 1st char of the paragraph proper.
    (backward-paragraph)
    (if (not (bobp))
        (forward-char))
    (end-of-line)
    ;; Main Loop
    (while (looking-at "\n\\([ \t]*\\)\\([^ \t\n]\\)")
      (if (and (functionp jpw-unfill-skip-line)
               (funcall jpw-unfill-skip-line (match-beginning 2)))
          (forward-line)
        ;; else:
        ;; Merge the two lines.
        (if (not (or (= (preceding-char) ?\ )
                     (= (preceding-char) ?\t)))
            ;; Insert two spaces after '.' or ':'.
            (if (or (= (preceding-char) ?.)
                    (= (preceding-char) ?:))
                (replace-match "  \\2")
              ;; else:
              ;; Other non-whitespace char.  Only put in one space.
              (replace-match " \\2")
              ) ;; end punct-if
          ;; else:
          ;; Already contains a separating whitespace char.
          (replace-match "\\2")
          ) ;;end space-if
        );; end skip-line-if
      (end-of-line)
      );;end Main Loop
    (cond ((eq remove-blank-lines 'collapse)
           (if (looking-at "\n\n\n+")
               (replace-match "\n\n"))
           )
          ((eq remove-blank-lines 'first)
           (if (looking-at "\n\n")
               (replace-match "\n"))
           )
          (remove-blank-lines
           (if (looking-at "\n\n+")
               (replace-match "\n"))
           )
          );; end cond
    );;end excursion
  (not (eobp))
  )


(defsubst jpw-unfill-buffer-engine (remove-blank-lines jpw-unfill-skip-line)
  "Calls `jpw-unfill-paragraph-engine' on every paragraph in the buffer (or
the narrowed region).

The optional `remove-blank-lines' will be passed to every underlying
`jpw-unfill-paragraph-engine' call.
{jpw: 09/2005}"
  (save-excursion
    (goto-char (point-min))
    (while (jpw-unfill-paragraph-engine remove-blank-lines
                                        jpw-unfill-skip-line)
      (forward-word 1)
      );; end while.
    );; end excursion
  )


(defun jpw-unfill-paragraph (&optional remove-blank-lines)
  "Takes paragraphs separated by blank lines and merges the paragraph into a
single line.  The inter-paragraph blank lines are preserved by default.

Evaluates to `nil' if this is the current paragraph is the last paragraph in
the buffer.  Evaluates to `t' otherwise.

If `remove-blank-lines' is non-nil, the inter-paragraph blank lines will
be removed completely.  {See `jpw-unfill-paragraph-engine' for more possible
values of `remove-blank-lines'.}
{jpw: 03/2006}"
  (interactive "P")
  (jpw-unfill-paragraph-engine remove-blank-lines nil)
  )


(defsubst jpw-unfill-buffer (&optional remove-blank-lines)
  "Calls `jpw-unfill-buffer-engine' on every paragraph in the buffer (or
the narrowed region).

The optional `remove-blank-lines' will be passed to
`jpw-unfill-buffer-engine'.  See `jpw-unfill-paragraph-engine' for further
details regarding this arg.
{jpw: 09/2005}"
  (interactive "P")
  (jpw-unfill-buffer-engine remove-blank-lines nil)
  )


(defun reverse-indent-line ()
  "Remove a level of indentation from the current line.
{jpw: 10/2001}"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((bolp (point)))
      (back-to-indentation)
      (indent-rigidly bolp (point) (- 0 tab-width))
      ); end let
    ); end excursion
  )


(defun unindent-line ()
  "Remove all indentation from the current line.
{jpw: 12/1999}"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((bolp (point)))
      (back-to-indentation)
      (delete-region bolp (point))
      ); end let
    ); end excursion
  )


(defun untabify-buffer ()
  "Untabify the current buffer.
{jpw: 12/1998}"
  (interactive)
  (untabify (point-min) (point-max))
  )


(defun tabify-buffer ()
  "Untabify the current buffer.
{jpw: 09/2008}"
  (interactive)
  (tabify (point-min) (point-max))
  )


(defvar jpw-correct-tabs-on-save t
  "If set non-nil, causes `save-buffer-tab-consistent' and
`kill-buffer-tab-consistent' to convert all leading whitespace to
space-characters or tabs.  Which it converts to depends on the value of
`indent-tabs-mode'.
{jpw; 09/2008}")


(defun save-buffer-tab-consistent ()
  "Saves the current buffer, (un)tabifying it beforehand.

If `indent-tabs-mode' is true, calls `tabify-buffer' before saving.
Otherwise, it calls `untabify-buffer'.
{jpw: 9/1998}"
  (interactive)
  (if jpw-correct-tabs-on-save
      (if indent-tabs-mode
          (tabify-buffer)
        ;; else
        (untabify-buffer)
        )
    )
  (save-buffer)
  )


(defun server-quit-l ()
  "Saves the server-buffer, does a (server-edit), then kills the
buffer.
{jpw: 11/2004}"
  (interactive)
  (save-buffer)
  (if (not (or is-winblows is-cygwin))
      (progn
        (rename-buffer "oldserv")
        (server-edit)
        (switch-to-buffer "oldserv")
        ));; end if...progn
  (kill-this-buffer)
  )


(defun kill-buffer-other-window ()
  "Kill the buffer in the next open window.
{jpw: 12/2004}"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window -1)
  )


(defun kill-next-buffer-and-close-other-windows ()
  "Kill the buffer in the next open window, then close all of the other
windows.
{jpw: 2/2005}"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window -1)
  (delete-other-windows)
  )


(defun bury-buffer-other-window ()
  "Bury the buffer in the next open window.
{jpw: 12/2004}"
  (interactive)
  (other-window 1)
  (bury-buffer)
  (other-window -1)
  )


(defun switch-to-buffer-using-other-window (buffer)
  "Custom version of `switch-to-buffer-other-window' that remains in the
current window instead of switching.
{jpw: 12/2004}"
  (interactive "BOpen buffer in other window: ")
  (switch-to-buffer-other-window buffer)
  (other-window -1)
  )


(defun find-file-using-other-window (buffer)
  "Custom version of `find-file-other-window' that remains in the
current window instead of switching.
{jpw: 12/2004}"
  (interactive "FFind file in other window: ")
  (find-file-other-window buffer)
  (other-window -1)
  )


(defun kill-buffer-then-find-file (nubuffer)
  "Open the specified file in the current active window, first killing
whatever buffer is presently open.
{jpw: 2/2005}"
  (interactive "FKill this buffer, then find file: ")
  (kill-this-buffer)
  (find-file nubuffer)
  )


(defun switch-to-self-other-window ()
  "As the name implies.  Useful when working on a long piece of sourcecode.
{jpw: 12/2004}"
  (interactive)
  (let ((mybufname (current-buffer))
        );;end let-varlist
    (switch-to-buffer-using-other-window mybufname)
    );;end let
  )


(defun save-all-buffers ()
  "{jpw: 12/2004}"
  (interactive)
  (save-some-buffers t)
  )


(defun dos2unix-buffer ()
  "Convert a DOS buffer to raw unix text
{jpw: 5/2000}."
  (interactive)
  (set-buffer-file-coding-system 'raw-text-unix nil))


(defun unix2dos-buffer ()
  "Convert a DOS buffer to raw unix text
{jpw: 11/2000}."
  (interactive)
  (set-buffer-file-coding-system 'raw-text-dos nil))


(defun jpw-insert-doc-unitag (tagname)
  "Insert an HTML tag that is self-closing.
{jpw: 07/2004}"
  (interactive "*sEnter HTML tag: ")
  (insert "<" tagname "/>")
  )


(defun jpw-insert-doc-tag (tagname)
  "Insert a doxygen/javadoc HTML style/font tag pair.
{jpw: 07/2004}"
  (interactive "*sEnter HTML tag: ")
  (jpw-insert-xml-tag tagname)
  )


(defun jpw-insert-doc-tagblock (tagname)
  "Insert a doxygen/javadoc HTML style/font tag pair block, with each tag on
its own comment line.
{jpw: 07/2004}"
  (interactive "*sEnter HTML tag: ")
  (if mark-active
      ;; When the user marks a region, assume that they want to enclose it w/o
      ;; inserting breaks anyplace.
      ;; Note that `jpw-insert-xml-tag' unsets the mark.
      (jpw-insert-xml-tag tagname)
    ;; else
    (do-comment-line-break)
    (jpw-insert-xml-tag tagname)
    (save-excursion
      (end-of-line)
      (do-comment-line-break)
      )
    )
  )


(defun jpw-insert-doc-nested-tagblock (outertagname innertagname)
  "Insert two doxygen/javadoc HTML style/font tags, the second nested inside
the first.  The first is inserted as a block.
{jpw: 01/2009}"
  (interactive)
  (jpw-insert-doc-tagblock outertagname)
  (do-comment-line-break)
  (insert-char ?. 1)
  (save-excursion
    (do-comment-line-break)
    )
  (delete-backward-char 1)
  (jpw-insert-doc-tag innertagname)
  )


(defun jpw-insert-doxygen-cmdblock (cmdname)
  "Insert a doxygen '\\command'...'\\endcommand' pair on separate lines.
{jpw: 07/2004}"
  (interactive "*sEnter doxygen command: ")
  (do-comment-line-break)
  (insert "\\" cmdname)
  (do-comment-line-break)
  (save-excursion
    (do-comment-line-break)
    (insert "\\end" cmdname)
    (do-comment-line-break))
  )


(defun jpw-insert-javadoc-link ()
  "Insert a javadoc '@link' field
{jpw: 07/2004}"
  (interactive)
  (if mark-active
      (save-excursion
        (goto-char (region-beginning))
        (if (search-forward "." (region-end) t)
            (replace-match "#" nil t)
          )
        )
    )
  (jpw-insert-markup-tags "{@link " "}")
  )


(defun jpw-insert-javadoc-member-link ()
  "Insert a javadoc '@link' field for a member
{jpw: 01/2009}"
  (interactive)
  (jpw-insert-markup-tags "{@link #" "}")
  )


(defun jpw-insert-tcldoc-tag (tagname)
  "Insert a TCLDoc '@' tag.
{jpw: 09/2006}"
  (interactive "*sEnter TCLDoc tag: ")
  (do-comment-line-break)
  (insert " @" tagname " ")
  (if (equal tagname "param")
      (save-excursion
        (jpw-insert-doc-unitag "br")
        (do-comment-line-break)
        )
    )
  )


;;----------------------------------------------------------------------
;;
;; Common HTML Editing Commands
;;


(defconst jpw-html-symbol-entity-table
  '(
    ("!=" . "&ne;")
    ("&" . "&amp;")
    ("'" . "&rsquo;")
    ("''" . "&rdquo;")
    ("--" . "&ndash;")
    ("---" . "&mdash;")
    ("->" .  "&rarr;")
    ("-^" . "&uarr;")
    ("-v" . "&darr;")
    ("..." . "&hellip;")
    ("<" . "&lt;")
    ("<-" . "&larr;")
    ("<->" . "&harr;")
    ("<=" . "&le;")
    ("<==" . "&lArr;")
    ("<==>" . "&hArr;")
    ("<=>" . "&hArr;")
    ("===" . "&equiv;")
    ("==>" . "&rArr;")
    ("==^" . "&uArr;")
    ("==v" . "&dArr;")
    (">" . "&gt;")
    (">=" . "&ge;")
    ("\"" . "&quot;")
    ("--+" . "&dagger;")
    ("+-+" . "&Dagger;")
    (",," . "&bdquo;")
    (".," . "&sbquo;")
    ("._" . "&thinsp;")
    ("__" . "&ensp;")
    ("___" . "&emsp;")
    ("`" . "&lsquo;")
    ("``" . "&ldquo;")
    ("'`" . "&rdquo;")
    ;; Canonical versions of the up & down arrows.
    ("|^" . "&uarr;")
    ("|v" . "&darr;")
    ("||^" . "&uArr;")
    ("||v" . "&dArr;")
    ("~" . "&sim;")
    ("~=" . "&cong;")
    ("~~" . "&asymp;")
    (" " . "&nbsp;")
    )
  "A table of abreviations for creating HTML entities.  Note that there are a
few alternatives for the same entity, so that mixing two similar mnemnonics
(e.g. \"->\" and \"|v\" to get \"-v\") still works.

If your mnemnonic doesn't follow a word character or whitespace, you can
\"separate\" it from the preceding character(s) using either a ';' or a ':'.
The separator will be consumed with the mnemnonic when it's converted to the
corresponding HTML entity.
{jpw; 08/2009}")


(defconst jpw-html-symbol-entity-table-maxlen
  (eval-when-compile
    (let* ((mnemonics (mapcar 'car jpw-html-symbol-entity-table))
           (mnemonics-len (mapcar 'length mnemonics))
           );end bindings
      (1+ (1+ (eval (append '(max) mnemonics-len))))
      )
    )
  "Length of the largest of the HTML entity mnemonics in
`jpw-html-symbol-entity-table'.
{jpw; 08/2008}")


(defconst jpw-html-symbol-entity-table-re
  (eval-when-compile
    (let* ((mnemonics (mapcar 'car jpw-html-symbol-entity-table))
           (notSyms "[^-<=>&~.\"`'^v|]")
           );end bindings
      ;; N.B. - Must contain a single group, surrounding the portion
      ;; of the regex matching the shortcut-table keys.
      (concat notSyms
              "\\([;:]?"
              (regexp-opt mnemonics t)
              "\\)"
              notSyms
       )
      );end let*
  )
  "A cached regexp that matches any of the HTML entity mnemonics in
`jpw-html-symbol-entity-table'.
{jpw; 03/2006}")


;; Redefine some of the tempo-template-* commands used by `html-helper-mode'
;; so that they produce valid XML
(tempo-define-template
 "html-line-break"
 '(& "<br/>" > n)
 nil
 "XHTML line break tag.
This function has been redefined so that it produces a complete tag.
{jpw: 08/2008}"
 )


(tempo-define-template
 "html-horizontal-line"
 '(& "<hr/>" > n)
 nil
 "XHTML horizontal rule tag.
This function has been redefined so that it produces a complete tag.
{jpw: 08/2008}"
 )


(define-skeleton jpw-html-href-anchor
  "HTML anchor tag with href attribute.
Like the sgml-mode version, but without the annoying \"http:\" defaulting into
the URL prompt.
{jpw: 03/2006}"
  "URL: "
  "<a href=\"" str "\">" _ "</a>")

(define-skeleton jpw-html-del
  "Insert HTML [logical] \"<del>\" tags, or puts the active region inside HTML
strong tags.
{jpw: 03/2006}"
  nil
  "<del>" _ "</del>")

(define-skeleton jpw-html-super
  "Insert HTML \"<sup>\" tags, or puts the active region inside HTML
strong tags.
{jpw: 11/2009}"
  nil
  "<sup>" _ "</sup>")

(define-skeleton jpw-html-sub
  "Insert HTML \"<sub>\" tags, or puts the active region inside HTML
strong tags.
{jpw: 11/2009}"
  nil
  "<sub>" _ "</sub>")


(define-skeleton jpw-html-size-small
  "Insert HTML font resizing tag \"<small>\".
{jpw: 03/2006}"
  nil
  "<small>" _  "</small>"
  )

(define-skeleton jpw-html-size-big
  "Insert HTML font resizing tag \"<big>\".
{jpw: 03/2006}"
  nil
  "<big>" _  "</big>"
  )

(define-skeleton jpw-html-size-relative
  "Insert XHTML font resizing markup.
{jpw: 03/2006}"
  (completing-read "Size: " jpw-html-size-alist nil nil "small")
  "<span style=\"font-size: " str "\">" _ "</span>")


(defun jpw-html-insert-list (&optional type)
  "Insert HTML list tags, or puts the active region inside HTML list
tags.
The optional `type' specifies the type of list.  It can be passed directly or
specified using a prefix-arg.  If `type' is an integer [e.g. a prefix-arg],
then the list will be an ordered list.  Otherwise, the list is unordered.
Any other type is an error.
{jpw: 03/2006}"
  (interactive "P")
  ;; Validation check.
  (or (null type)
      (char-or-string-p type)
      (signal 'wrong-type-argument
              (list 'char-or-string-p type)))
  ;; Clear the prefix arg so it doesn't screw up the behavior of the
  ;; `skeleton-insert' call.
  (if type (setq prefix-arg nil
                 current-prefix-arg nil))
  (if (or (null type)
          (stringp type))
      (tempo-template-html-unordered-list)
    (tempo-template-html-ordered-list)
    )
  )


(defun jpw-html-entity-abbrev-expand ()
  "Converts the mnemonic at point to an HTML entity.  See the documentation
for the variable, `jpw-html-symbol-entity-table', for the valid
abbreviations/mnemnonics.
{jpw: 08/2008}"
  (interactive)
  (save-excursion

    (let* ((end (1+ (point)))
           (start (- (point) jpw-html-symbol-entity-table-maxlen))
           entity
           );;end bindings
      (goto-char start)
      (if (posix-search-forward jpw-html-symbol-entity-table-re end t)
          (setq entity (cdr (assoc (match-string 2)
                                   jpw-html-symbol-entity-table)))
        )
      (if entity
          (replace-match entity t t nil 1)
        )
      ) ;;end let

    );;end excursion
  )



;;----------------------------------------------------------------------
;;
;; Custom Behavior
;;
;; Activate behaviors not used regularly, but used nonetheless.
;;


(defun jpw-read-info-dir(file)
  "Invoke Info on the named file, which should be a .info file or info
top-level directory file."
  (interactive "fInfo file to open: ")
  (find-file file)
  (Info-on-current-buffer)
  )


(defun set8tab ()
  "Sets tab width to 8
{jpw: 7/2000}."
  (interactive)
  (setq tab-width 8)
  (recenter))


(defun set4tab ()
  "Sets tab width to 4
{jpw: 7/2000}."
  (interactive)
  (setq tab-width 4)
  (recenter))


(defun c-no-comment-stars ()
  (interactive)
  ;; For Emacs 20.* or earlier
;  (setq c-comment-continuation-stars "")
  (setq c-block-comment-prefix "")
)


(defun do-comment-line-break ()
  "Calls the function that the variable `comment-line-break-function'
is set to
{jpw: 3/2002}."
  (interactive)
  (funcall comment-line-break-function))


(defvar jpw-utf-in-use nil
  "Used internally.  Do not modify.  {jpw; 03/2007}")
(defun jpw-load-utf()
  "Force use of Mule UTF encodings. {jpw; 11/2005}"
  (interactive)
  (if (boundp 'mule-version)
      (setq jpw-utf-in-use t)
    )
  (if (not jpw-utf-in-use)
      (progn
        ;; Taken from `loadup.el'
        (load "international/mule")
        (load "international/mule-conf.el") ; Don't get confused if someone
                                            ; compiled this by mistake.
        (load "international/mule-cmds")
        (load "case-table")
        (load "international/utf-8")
        (load "international/utf-16")
        (load "international/characters")
        (load "international/ucs-tables")
        (setq jpw-utf-in-use t)
        )
    )
  )


(defun decode-utf16 ()
  "Decode a buffer in utf-16
{jpw: 11/2005}."
  (interactive)
  (jpw-load-utf)
  (decode-coding-region (point-min) (point-max)
                        'mule-utf-16-le-with-signature))
(defalias 'decode-xml 'decode-utf16)


(defun decode-entire-buffer-utf8 ()
  "Decode a buffer in utf-8
Unfortunately, it doesn't work too well.

{jpw: 03/2007}."
  (interactive)
  (jpw-load-utf)
  (decode-coding-region (point-min) (point-max) 'mule-utf-8))


(defun revert-to-utf8 ()
  "Reread the current buffer, using utf-8 encoding this time.
Unfortunately, it doesn't work too well.

{jpw: 03/2007}."
  (interactive)
  (jpw-load-utf)
  (revert-buffer-with-coding-system 'mule-utf-8)
  ;;(let ((my-file-name (buffer-file-name (current-buffer)))
  ;;      ;; Set this only for the duration of the scope of the (let ...).
  ;;      (coding-system-for-read 'mule-utf-8)
  ;;      (coding-system-for-write 'mule-utf-8)
  ;;      );; end varbindings
  ;;  (find-alternate-file my-file-name)
  ;;  )
  ;;(setq buffer-file-coding-system 'mule-utf-8)
  )


(defun use-utf8 ()
  "Use the utf-8 encoding system in this buffer.  Does so by performing a
`revert-to-utf8'.

{jpw: 05/2008}."
  (interactive)
  (jpw-load-utf)
  (revert-buffer-with-coding-system 'mule-utf-8)
  (setq buffer-file-coding-system 'mule-utf-8)
  )


;;----------------------------------------------------------------------
;;
;; Autohook-specific functions.
;;


(defun jpw-flip-to-mode (regex mode)
  "Flip from `sh-mode' for embedded \"foreign\" scripting languages.

There's a common technique used for scripts whose interpreter cannot be
started via the \"#!\"-technique:  call 'exec' on the interpreter, passing the
script itself.  This only works with scripting languages that also use the
\"#\" character to start comments and which permit comment lines to be
extended using an EOL-\"\\\"-char.  {jpw; 12/2004}"
  (and
   (save-excursion
     (goto-char (point-min))
     (and (re-search-forward "^#.*\\\\[ \t]*$" (point-max) t)
          (or (beginning-of-line) t)
          (re-search-forward
           (concat "\\\\[ \t]*\nexec .*"
                   regex
                   ".* \\(`basename \\)?\"?\\${?0}?\"?`?"
                   " .*\"?\\${?[@*1]}?\"?")
           (point-max) t)
          ))
   (funcall mode)
   )
  )


(defun jpw-flip-to-tcl-mode ()
  (interactive)
  (jpw-flip-to-mode "tclsh" 'tcl-mode)
  )


(defun use-jpw-perl-dabbrev-skip ()
  (make-local-variable 'dabbrev-abbrev-skip-leading-regexp)
  (setq dabbrev-abbrev-skip-leading-regexp "[$@%&]")
  )


(defun use-jpw-sh-dabbrev-skip ()
  (make-local-variable 'dabbrev-abbrev-skip-leading-regexp)
  (setq dabbrev-abbrev-skip-leading-regexp "[$]")
  )


(defun bind-jpw-doc-comment ()
  (interactive)
  ;; Universal doc-comment keybindings
  (local-set-key "\M-oe" (lambda() (interactive)
                                   (jpw-insert-doc-tag "em")))
  (local-set-key "\M-ot" (lambda() (interactive)
                                 (jpw-insert-doc-tag "tt")))
  (local-set-key "\M-ob" (lambda() (interactive)
                                 (jpw-insert-doc-tag "b")))
  (local-set-key "\M-oi" (lambda() (interactive)
                                 (jpw-insert-doc-tag "i")))
  (local-set-key "\M-ou" (lambda() (interactive)
                                 (jpw-insert-doc-tag "u")))
  (local-set-key "\M-o_" (lambda() (interactive)
                                 (jpw-insert-doc-tag "u")))
  (local-set-key "\M-p\C-j" (lambda() (interactive)
                              (do-comment-line-break)
                              (jpw-insert-doc-unitag "br")))
  (local-set-key "\M-p\C-m" (lambda() (interactive)
                              (jpw-insert-doc-tagblock "p")))
  (local-set-key "\M-p*" (lambda() (interactive)
                           (jpw-insert-doc-tag "li")))
  (local-set-key "\M-p." (lambda() (interactive)
                           (jpw-insert-doc-tag "li")))
  (local-set-key "\M-pl" (lambda() (interactive)
                           (if (null current-prefix-arg)
                               (jpw-insert-doc-nested-tagblock "ul" "li")
                             (setq current-prefix-arg nil)
                             (jpw-insert-doc-nested-tagblock "ol" "li"))))
  (local-set-key "\M-p1" (lambda() (interactive)
                           (jpw-insert-doc-nested-tagblock "ol" "li")))
  (local-set-key "\M-p0" (lambda() (interactive)
                           (jpw-insert-doc-nested-tagblock "ol" "li")))
  (local-set-key "\M-p+" (lambda() (interactive)
                           (jpw-insert-doc-nested-tagblock "ul" "li")))
  (local-set-key "\M-p=" (lambda() (interactive)
                           (jpw-insert-doc-nested-tagblock "ul" "li")))
  (local-set-key "\M-p-" (lambda() (interactive)
                           (jpw-insert-doc-unitag "hr")))
  )

(defun bind-jpw-c-mode-doxy ()
  (interactive)
  ;; Create keymap
  ;; Define doc-comment keybindings.
  (bind-jpw-doc-comment)
  (local-set-key "\M-po" (lambda() (interactive)
                           (jpw-insert-doxygen-cmdblock "code")))
  )


(defun bind-jpw-javadoc ()
  (interactive)
  ;; Define doc-comment keybindings.
  (bind-jpw-doc-comment)
  (local-set-key "\M-oc" (lambda() (interactive)
                           (jpw-insert-doc-tag "code")))
  (local-set-key "\M-pc" (lambda() (interactive)
                           (jpw-insert-doc-tagblock "pre")))
  (local-set-key "\M-pr" 'jpw-insert-javadoc-link)
  (local-set-key "\M-pl" 'jpw-insert-javadoc-link)
  (local-set-key "\M-pm" 'jpw-insert-javadoc-member-link)
  (local-set-key "\M-p\M-l" (lambda() (interactive)
                              (if (null current-prefix-arg)
                                  (jpw-insert-doc-nested-tagblock "ul" "li")
                                (setq current-prefix-arg nil)
                                (jpw-insert-doc-nested-tagblock "ol" "li"))
                              ))
  )


(defun use-jpw-style-text ()
  (interactive)
  (setq fill-column 70)
  (font-lock-mode -1)
  ;; Mozex should not use auto-fill mode
  (or
   (string-match "^mozex\." (buffer-name))
   (turn-on-auto-fill)
   )
  )


(defun use-jpw-style-mutt ()
  (interactive)
  (turn-on-auto-fill)
  (font-lock-mode 't)
  (abbrev-mode 1)
  (local-set-key "\C-ca" 'mutt-alias-insert)
  (local-set-key "\C-ci" 'mutt-alias-insert)
  (local-set-key "\C-cl" 'mutt-alias-lookup)
  )


(defvar jpw-outline-toggle-all-visible 'nil)
(defun jpw-outline-toggle-show-hide-all()
  (interactive)
  (if jpw-outline-toggle-all-visible
      (progn
        (setq jpw-outline-toggle-all-visible 'nil)
        (hide-sublevels 1)
        )
    ;; else:
    (setq jpw-outline-toggle-all-visible 't)
    (show-all)
    )
  )


(defun use-jpw-style-org ()
  (interactive)
  ;;(setq fill-column 78)
  (turn-on-auto-fill)
  ;; Initialize, using the state of the customization variable,
  ;;"org-startup-folded"
  (setq jpw-outline-toggle-all-visible
        (or (not org-startup-folded)
            (eq org-startup-folded 'nofold)
            (eq org-startup-folded 'showall)
            )
        )
  (local-set-key [\M-\S-return] 'org-meta-return)
  (local-set-key [\C-return] 'org-meta-return)
  (local-set-key [\M-return] 'join-next-line)
  (local-set-key [\M-down] 'outline-next-visible-heading)
  (local-set-key [\M-up] 'outline-previous-visible-heading)
  (local-set-key [\M-right] 'outline-forward-same-level)
  (local-set-key [\M-left] 'outline-backward-same-level)
  (local-set-key [?\C-c \C-tab] 'jpw-outline-toggle-show-hide-all)
  (local-set-key [\C-\S-down] 'org-shiftmetadown)
  (local-set-key [\C-\S-up] 'org-shiftmetaup)
  (local-set-key [\C-\S-right] 'org-shiftmetaright)
  (local-set-key [\C-\S-left] 'org-shiftmetaleft)
  (local-set-key [\C-tab] 'unindent-line)
  (local-set-key [\S-tab] 'reverse-indent-line)
  (local-set-key [\S-iso-lefttab] 'reverse-indent-line)
  (font-lock-mode t)
  )


(defun use-jpw-style-html-helper ()
  (interactive)
  (setq fill-column 78)
  (turn-on-auto-fill)

  ;; Fix keys that break my bindings.
  (local-unset-key [f4])
  (local-set-key [\M-\S-return] 'tempo-template-html-paragraph)
  (local-set-key [\C-return] 'tempo-template-html-line-break)
  (local-set-key [\M-return] 'join-next-line)

  ;; Special setup for HTML-Helper Mode  on Thunderbird mail buffers.
  (local-set-key "\M-p\M-t"
                 '(lambda ()
                    (interactive)
                    (revert-to-utf8)
                    (save-excursion
                      (goto-char (point-min))
                      (while (search-forward "<br>" nil t)
                        (replace-match "<br/>" nil t)))
                    )
                 )

  ;; Fonts
  (local-set-key "\M-oi" 'tempo-template-html-italic)
  (local-set-key "\M-ob" 'tempo-template-html-bold)
  (local-set-key "\M-ou" 'tempo-template-html-underline)
  (local-set-key "\M-oe" 'tempo-template-html-emphasized)
  (local-set-key "\M-os" 'tempo-template-html-strong)
  (local-set-key "\M-ot" 'tempo-template-html-fixed)
  (local-set-key "\M-of" 'tempo-template-html-fixed)
  (local-set-key "\M-oo" 'tempo-template-html-code)
  (local-set-key "\M-oc" 'tempo-template-html-code)
  (local-set-key "\M-od" 'jpw-html-del)
  (local-set-key "\M-o^" 'jpw-html-super)
  (local-set-key "\M-o_" 'jpw-html-sub)
  (local-set-key "\M-o\C-s" 'jpw-html-size-small)
  (local-set-key "\M-o\C-b" 'jpw-html-size-big)
  (local-set-key "\M-oz" 'jpw-html-size-relative)

  ;; Paragraph styles
  (local-set-key "\M-p\C-i" 'tempo-template-html-image)
  (local-set-key "\M-pc" 'tempo-template-html-preformatted)
  (local-set-key "\M-pa" 'jpw-html-href-anchor)
  (local-set-key "\M-p\C-u" 'jpw-html-href-anchor)
  (local-set-key "\M-pl" 'jpw-html-insert-list)
  (local-set-key "\M-p*" 'tempo-template-html-list-item)
  (local-set-key "\M-p." 'tempo-template-html-list-item)
  (local-set-key "\C-cp" 'tempo-template-html-paragraph)
  (local-set-key "\M-p\C-m" 'tempo-template-html-paragraph)
  (local-set-key "\M-p\C-j" 'tempo-template-html-line-break)
  (local-set-key "\M-p-" 'tempo-template-html-horizontal-line)
  (local-set-key "\M-pq" 'tempo-template-html-blockquote)
  (local-set-key "\M-p1" 'tempo-template-html-header-1)
  (local-set-key "\M-ph1" 'tempo-template-html-header-1)
  (local-set-key "\M-p2" 'tempo-template-html-header-2)
  (local-set-key "\M-ph2" 'tempo-template-html-header-2)
  (local-set-key "\M-p3" 'tempo-template-html-header-3)
  (local-set-key "\M-ph3" 'tempo-template-html-header-3)
  (local-set-key "\M-p4" 'tempo-template-html-header-4)
  (local-set-key "\M-ph4" 'tempo-template-html-header-4)
  (local-set-key "\M-p5" 'tempo-template-html-header-5)
  (local-set-key "\M-ph5" 'tempo-template-html-header-5)
  (local-set-key "\M-p6" 'tempo-template-html-header-6)
  (local-set-key "\M-ph6" 'tempo-template-html-header-6)

  ;; Entity expansion
  (local-set-key "\C-c'" 'jpw-html-entity-abbrev-expand)
  (local-set-key [?\C-c ?\C-'] 'jpw-html-entity-abbrev-expand)

  (font-lock-mode t)
  )


(define-skeleton jpw-wikipedia-insert-header1
  "Level 1 Heading" nil "== " _ "==")
(define-skeleton jpw-wikipedia-insert-header2
  "Level 2 Heading" nil "=== " _ "===")
(define-skeleton jpw-wikipedia-insert-header3
  "Level 3 Heading" nil "==== " _ "====")
(define-skeleton jpw-wikipedia-insert-header4
  "Level 4 Heading" nil "===== " _ "=====")
(define-skeleton jpw-wikipedia-insert-html
  "Protected HTML markup" nil "<html>" _ "</html>")
(define-skeleton jpw-wikipedia-insert-pre
  "Protected HTML markup" nil "<html><pre>" _ "</pre></html>")


(defun use-jpw-style-wikipedia-mode ()
  (interactive)
  (local-set-key "\C-cl" 'longlines-mode)
  (local-set-key "\C-c\C-l" 'longlines-mode)

  ;; Motion Keys
  (local-set-key [\C-right] 'forward-word)
  (local-set-key [\C-left] 'backward-word)
  (local-set-key [\C-up] 'backward-paragraph)
  (local-set-key [\C-down] 'forward-paragraph)

  ;; More disasters
  (local-set-key "\M-u" 'upcase-word)
  (local-set-key "\C-c\"" 'wikipedia-unfill-paragraph-or-region)
  (local-set-key [\M-return] 'join-next-line)
  (local-set-key "\M-pe" 'wikipedia-insert-enumerate)
  (local-set-key [\C-return] 'wikipedia-terminate-paragraph)
  (local-set-key "\M-pi" 'wikipedia-insert-itemize)

  ;; Outline Navigation
  (local-set-key [\M-down] 'wikipedia-next-header)
  (local-set-key [\M-up] 'wikipedia-prev-header)
  (local-set-key [\M-right] 'outline-forward-same-level)
  (local-set-key [\M-left] 'outline-backward-same-level)
  (local-set-key [?\C-c \C-up] 'outline-up-heading)
  (local-set-key [\C-\S-down] 'outline-move-subtree-down)
  (local-set-key [\C-\S-up] 'outline-move-subtree-up)
  (local-set-key [\C-\S-right] 'wikipedia-simple-outline-demote)
  (local-set-key [\C-\S-left] 'wikipedia-simple-outline-promote)
  (local-set-key [\M-\S-down] 'outline-move-subtree-down)
  (local-set-key [\M-\S-up] 'outline-move-subtree-up)
  (local-set-key [\M-\S-right] 'wikipedia-simple-outline-demote)
  (local-set-key [\M-\S-left] 'wikipedia-simple-outline-promote)

  ;; Fonts
  (local-set-key "\M-oi" 'wikipedia-insert-italics)
  (local-set-key "\M-ob" 'wikipedia-insert-bold)
  (local-set-key "\M-ou" 'tempo-template-html-underline)
  (local-set-key "\M-oe" 'wikipedia-insert-italics)
  (local-set-key "\M-os" 'wikipedia-insert-bold)
  (local-set-key "\M-ot" 'tempo-template-html-fixed)
  (local-set-key "\M-of" 'tempo-template-html-fixed)
  (local-set-key "\M-oo" 'tempo-template-html-code)
  (local-set-key "\M-oc" 'tempo-template-html-code)
  (local-set-key "\M-od" 'tempo-template-html-strikethru)

  ;; Paragraph styles
  (local-set-key "\M-pc" 'jpw-wikipedia-insert-pre)
  (local-set-key "\M-pa" 'wikipedia-insert-link)
  (local-set-key "\M-p\C-u" 'wikipedia-insert-link)
  (local-set-key "\M-pl" 'wikipedia-insert-itemize)
  (local-set-key "\M-p\C-m" 'wikipedia-terminate-paragraph)
  (local-set-key "\M-p\C-j" 'wikipedia-terminate-paragraph)
  (local-set-key "\M-p-" 'wikipedia-insert-html-hline)
  (local-set-key "\M-ph" 'wikipedia-insert-header)
  (local-set-key "\M-p\M-h" 'jpw-wikipedia-insert-html)
  (local-set-key "\M-p1" 'jpw-wikipedia-insert-header1)
  (local-set-key "\M-p2" 'jpw-wikipedia-insert-header2)
  (local-set-key "\M-p3" 'jpw-wikipedia-insert-header3)
  (local-set-key "\M-p4" 'jpw-wikipedia-insert-header4)

  ;; Entity expansion
  (local-set-key "\C-c'" 'jpw-html-entity-abbrev-expand)
  (local-set-key [?\C-c ?\C-'] 'jpw-html-entity-abbrev-expand)
  )


(defun use-jpw-style-elisp ()
  (interactive)
  (auto-fill-mode 1)
;;  (show-paren-mode 1)
  (global-set-key "\C-cg" 'goto-char)
  (local-set-key "\C-cd" 'edebug-eval-top-level-form)
  (local-set-key "\C-c\C-d" 'edebug-eval-top-level-form)
  (local-set-key "\C-ce" 'eval-defun)
  )


(defun use-jpw-style-c-common ()
  (interactive)
  (auto-fill-mode 1)
  (setq fill-column 78)
  (setq c-indent-comments-syntactically-p 't
        c-tab-always-indent "partial"
        )

  ;; Moves forward by capitalizations or words.  Very useful for C++ &
  ;; Java programming.
  (if running-xemacs
      (progn
        (local-set-key '("\C-c" left) 'c-beginning-of-defun)
        (local-set-key '("\C-c" right) 'c-end-of-defun)
        (local-set-key '(control shift left) 'c-backward-into-nomenclature)
        (local-set-key '(control shift right) 'c-forward-into-nomenclature)
        (local-set-key '(control shift up) 'c-beginning-of-statement)
        (local-set-key '(control shift down) 'c-end-of-statement)
        )
    ;; else
    (local-set-key [?\C-c left] 'c-beginning-of-defun)
    (local-set-key [?\C-c right] 'c-end-of-defun)
    (local-set-key [(control shift left)] 'c-backward-into-nomenclature)
    (local-set-key [(control shift right)] 'c-forward-into-nomenclature)
    (local-set-key [(control shift up)] 'c-beginning-of-statement)
    (local-set-key [(control shift down)] 'c-end-of-statement)
    )
  ;; This, for some reason, needs to be in the mode-specific autohook defuns.
  ;; Leave it here, too, just for documentation clarity.
  (local-unset-key [f4])
  ;; Force use of correct comment-break-fn.
  (local-set-key "\M-j" 'do-comment-line-break)
  (local-set-key [?\C-c f7] 'compile)
  (local-set-key [?\C-c f8] 'recompile)
  (bind-jpw-c-mode-doxy)
  )


(defun use-jpw-style-cxx ()
  (interactive)
  ;; Use the value of the `jpw-c-style' variable, for that added level of
  ;; indirection. ;)
  (c-set-style jpw-c-style)
  ;; Make sure this is set correctly...
  (local-unset-key [f4])
  )


(defconst jpw-buf-is-cxx-class-regex
  (concat "^\\s *\\(class\\|namespace\\|"
          "p\\(r\\(ivate\\|otected\\\)\\|ublic\\)\\)")
  )
(defconst jpw-buf-is-cxx-lib-regex
  "\\b\\(std::\\w\\|\\(c\\(err\\|out\\)\||string\\)\\b\\)"
  )
(defun use-jpw-style-c ()
  (interactive)
  (if (and (string-match "\\.h$" (buffer-file-name (current-buffer)))
           (save-excursion
             (goto-char (point-min))
             (or
              (save-excursion
                (re-search-forward jpw-buf-is-cxx-class-regex (point-max) t)
                );; end inner excursion
              (save-excursion
                (re-search-forward jpw-buf-is-cxx-lib-regex (point-max) t)
                );; end inner excursion
              )
             );; end buffer-check excursion
           );; end and
      (c++-mode)
    ;; else - this really is a C-source file.
    ;;
    ;; Use the value of the `jpw-c-style' variable, for that added level of
    ;; indirection. ;)
    (c-set-style jpw-c-style)
    ;; Make sure this is set correctly...
    (local-unset-key [f4])
    );; end if
  )


(defun use-jpw-style-java ()
  (interactive)
  (c-set-style "jpw-java")
  (setq fill-column 78)
  (bind-jpw-javadoc)
  (local-unset-key [f4])
  (local-set-key [?\C-c f7] 'compile)
  (local-set-key [?\C-c f8] 'recompile)
  )


(defun use-jpw-style-tex ()
  (interactive)
  (setq tab-width 4)
  (turn-on-auto-fill)
  )


(defun use-jpw-style-sql ()
  (interactive)
  (setq tab-width 2)
  (turn-on-auto-fill)
  ;;(sql-highlight-oracle-keywords)
  (sql-highlight-sybase-keywords)
  )


(defun use-jpw-style-cperl ()
  (interactive)
  (auto-fill-mode 1)
  ;; Use a style equivalent to perl-mode indentation.
  (cperl-set-style "PerlStyle")
  (setq cperl-label-offset -2)
  (use-jpw-perl-dabbrev-skip)
  )


(defun use-jpw-style-octave ()
  (interactive)
  (turn-on-auto-fill)
  (font-lock-mode)
  (local-set-key "\M-j" 'octave-indent-new-comment-line)
  )


(defun use-jpw-style-tcl ()
  (interactive)
  (turn-on-auto-fill)
  (setq fill-column 78)
  (use-jpw-sh-dabbrev-skip)
  (font-lock-mode t)
  (bind-jpw-doc-comment)
  (local-set-key "\M-po" (lambda() (interactive)
                             (jpw-insert-doc-tag "code")))
  (local-set-key "\M-pp" (lambda() (interactive)
                             (jpw-insert-tcldoc-tag "param")))
  (local-set-key "\M-pa" (lambda() (interactive)
                             (jpw-insert-tcldoc-tag "param")))
  (local-set-key "\M-pr" (lambda() (interactive)
                             (jpw-insert-tcldoc-tag "return")))
  (local-set-key "\M-ps" (lambda() (interactive)
                             (jpw-insert-tcldoc-tag "see")))
  (local-set-key "\M-pl" 'jpw-insert-javadoc-link)
  )


(defun jpw-set-sgml-indent (arg)
 "Set indentation size(s) for SGML modes.
{jpw: 11/2005}"
 (interactive "nIndent Size: ")
 (make-local-variable 'tab-width)
 (make-local-variable 'custom-buffer-indent)
 (make-local-variable 'standard-indent)
 (setq tab-width arg
       custom-buffer-indent arg
       standard-indent arg)
 )


(defun use-jpw-style-sgml ()
  (interactive)
  (turn-on-auto-fill)
  (font-lock-mode)
  (jpw-set-sgml-indent 2)
  (recenter))


(defun jpw-set-xml-lite-indent (arg)
  "Set indentation size for XML-Lite mode.
{jpw: 3/2003}"
  (interactive "nIndent Size: ")
  (jpw-set-sgml-indent arg)
  (setq xml-lite-indent-comment-offset arg
        xml-lite-indent-offset arg)
  )


(defun jpw-xml-lite-mode ()
  (interactive)
  (sgml-mode)
  (turn-on-auto-fill)
  (xml-lite-mode)
  (font-lock-mode)
  (jpw-set-xml-lite-indent 4)
  )


(defun jpw-speedbar-settings ()
  (interactive)
  (speedbar-disable-update)
  (speedbar-change-initial-expansion-list "buffers")
  (speedbar-update-contents)
  (speedbar-set-mode-line-format)
  )


(provide 'custom-defuns)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;