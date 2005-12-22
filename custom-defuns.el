;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    
;; Custom Functions
;;
;;  Copyright © 1995-2005 John P. Weiss
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

;;----------------------------------------------------------------------
;;
;; Customization/Setup Tools
;;


(defun jpw-custom-set-variables-nonsaved  (&rest args)
  "Initializes the default value of a customization variable.

Calls `custom-set-variables' on the list of arguments, then converts the
\"saved value\" to the \"default value\".  This prevents localized
customizations from being written to your \".emacs\" file.

Note that this function may require modification whenever `cus-edit.el'
changes.  {jpw: 9/04}"
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
`cus-faces.el' changes.  {jpw: 9/04}"
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


;;----------------------------------------------------------------------
;;
;; Building Blocks
;;


(defsubst jpw-insert-markup-tags (start-tag end-tag)
  "Inserts the two markup tags, `start-tag' and `end-tag', on either side of
`point'.  If a region is active, it inserts the two tags on either side of the
region.
{jpw: 2/05}"
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


;; FIXME:  Move these two defuns into more appropriate elisp files.
(defun jpw-insert-xml-tag (tag)
  "Inserts the begin- and end-tags for the XML entity named `tag', inserting
them on either side of `point'.  If a region is active, it inserts the two
tags on either side of the region.
{jpw: 2/05}"
  (jpw-insert-markup-tags (concat "<" tag ">") (concat "</" tag ">"))
  )


;;----------------------------------------------------------------------
;;
;; Editing Tools
;;


(defun kill-ring-save-entire-buffer ()
  "Like kill-ring-save, but grabs the entire buffer."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Saved all (visible) buffer text.")
  )


(defun join-next-line ()
  "Join the current line to the next line. {jpw: 2/99}"
  (interactive)
  (end-of-line)
  (let ((oldeolpos (point)))
    (forward-line 1)
    (back-to-indentation)
    (delete-region oldeolpos (point))
    )
  )


(defun jpw-unfill-paragraph (&optional remove-blank-line)
  "Takes paragraphs separated by blank lines and merges the paragraph into a
single line.  The inter-paragraph blank lines are preserved by default.

Evaluates to `nil' if this is the current paragraph is the last paragraph in
the buffer.  Evaluates to `t' otherwise.

If `remove-blank-line' is non-nil, the first inter-paragraph blank line will
be removed.  The others will be left alone.
{jpw: 09/2005}"
  (interactive "P")
  (save-excursion
    ;; Position at 1st char of the paragraph proper.
    (backward-paragraph)
    (if (not (bobp))
        (forward-char))
    (end-of-line)
    ;; Main Loop
    (while (looking-at "\n\\([ \t]*\\)\\([^ \t\n]\\)")
      (if (and (functionp 'jpw-unfill-skip-line)
               (jpw-unfill-skip-line (match-beginning 2)))
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
    (if (and remove-blank-line (looking-at "\n\n"))
        (replace-match "\n"))
    );;end excursion
  (not (eobp))
  )


;; N.B.:  When byte-compiling this file, you'll get warning about
;;        `jpw-unfill-skip-line not being defined.  Ignore it; the file still
;;        compiles.
;;        I tried several techniques to remove this warning, but was
;;        unsuccessful.
(defun jpw-unfill-buffer (&optional remove-blank-line)
  "Calls `jpw-unfill-paragraph' on every paragraph in the buffer.

The optional `remove-blank-line' will be passed to every underlying
`jpw-unfill-paragraph' call.
{jpw: 09/2005}"
  (interactive)
  (save-excursion
    (while (jpw-unfill-paragraph remove-blank-line)
      (forward-word 1)
      );; end while.
    );; end excursion
  )


(defun reverse-indent-line ()
  "Remove a level of indentation from the current line. {jpw: 10/01}"
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
  "Remove all indentation from the current line. {jpw: 12/99}"
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
  "Untabify the current buffer. {jpw: 12/98}"
  (interactive)
  (untabify (point-min) (point-max))
  )


(defun save-buffer-untabified ()
  "Saves the current buffer, untabifying it beforehand.  {jpw: 9/98}"
  (interactive)
  (untabify-buffer)
  (save-buffer)
  )


(defun kill-buffer-untabified ()
  "Untabify the current buffer, save it, then kill it.  {jpw: 9/98}"
  (interactive)
  (save-buffer-untabified)
  (kill-this-buffer)
  )


(defun server-quit-l ()
  "Saves the server-buffer, does a (server-edit), then kills the
buffer.  {jpw: 11/04}"
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
  "Kill the buffer in the next open window.  {jpw: 12/04}"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window -1)
  )


(defun kill-next-buffer-and-close-other-windows ()
  "Kill the buffer in the next open window, then close all of the other
windows.  {jpw: 2/05}"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window -1)
  (delete-other-windows)
  )


(defun bury-buffer-other-window ()
  "Bury the buffer in the next open window.  {jpw: 12/04}"
  (interactive)
  (other-window 1)
  (bury-buffer)
  (other-window -1)
  )


(defun switch-to-buffer-using-other-window (buffer)
  "Custom version of `switch-to-buffer-other-window' that remains in the
current window instead of switching.
{jpw: 12/04}"
  (interactive "BOpen buffer in other window: ")
  (switch-to-buffer-other-window buffer)
  (other-window -1)
  )


(defun find-file-using-other-window (buffer)
  "Custom version of `find-file-other-window' that remains in the
current window instead of switching.
{jpw: 12/04}"
  (interactive "FFind file in other window: ")
  (find-file-other-window buffer)
  (other-window -1)
  )


(defun kill-buffer-then-find-file (nubuffer)
  "Open the specified file in the current active window, first killing
whatever buffer is presently open.
{jpw: 2/05}"
  (interactive "FKill this buffer, then find file: ")
  (kill-this-buffer)
  (find-file nubuffer)
  )


(defun switch-to-self-other-window ()
  "As the name implies.  Useful when working on a long piece of sourcecode.
{jpw: 12/04}"
  (interactive)
  (let ( (mybufname (current-buffer))
         );;end let-varlist
    (switch-to-buffer-using-other-window mybufname)
    );;end let
  )


(defun save-all-buffers () 
  "{jpw: 12/04}"
  (interactive)
  (save-some-buffers t)
  )


(defun dos2unix-buffer ()
  "Convert a DOS buffer to raw unix text {jpw: 5/00}."
  (interactive)
  (set-buffer-file-coding-system 'raw-text-unix nil))


(defun unix2dos-buffer ()
  "Convert a DOS buffer to raw unix text {jpw: 11/00}."
  (interactive)
  (set-buffer-file-coding-system 'raw-text-dos nil))


(defun jpw-insert-doxygen-tag (tagname)
  "Insert a doxygen/javadoc HTML style/font tag pair {jpw: 07/04}"
  (interactive "*sEnter HTML tag: ")
  (insert "<" tagname ">")
  (save-excursion (insert "</" tagname ">"))
  )


(defun jpw-insert-doxygen-tagblock (tagname)
  "Insert a doxygen/javadoc HTML style/font tag pair block, with each tag on
its own comment line {jpw: 07/04}"
  (interactive "*sEnter HTML tag: ")
  (do-comment-line-break) 
  (insert "<" tagname ">")
  (do-comment-line-break)
  (save-excursion 
    (do-comment-line-break) 
    (insert "</" tagname ">")
    (do-comment-line-break))
  )


(defun jpw-insert-doxygen-cmdblock (cmdname)
  "Insert a doxygen '\\command'...'\\endcommand' pair on separate lines. {jpw: 07/04}"
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
  "Insert a javadoc '@link' field {jpw: 07/04}"
  (interactive )
  (insert "{@link ")
  (save-excursion (insert "}"))
  )


;;----------------------------------------------------------------------
;;
;; Custom Behavior
;;
;; Activate behaviors not used regularly, but used nonetheless.
;;


(defun set8tab ()
  "Sets tab width to 8 {jpw: 7/00}."
  (interactive)
  (setq tab-width 8)
  (recenter))


(defun set4tab ()
  "Sets tab width to 4 {jpw: 7/00}."
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
is set to {jpw: 3/02}."
  (interactive)
  (funcall comment-line-break-function))


(defun use-utf()
  "Force use of Mule UTF encodings. {jpw; 11/05}"
  (interactive)
  ;; Taken from `loadup.el'
  (load "international/mule")
  (load "international/mule-conf.el")  ; Don't get confused if someone
                                       ; compiled this by mistake.
  (load "international/mule-cmds")
  (load "case-table")
  (load "international/utf-8")
  (load "international/utf-16")
  (load "international/characters")
  (load "international/ucs-tables")
  )


(defun decode-utf16 ()
  "Decode a buffer in utf-16 {jpw: 11/05}."
  (interactive)
  (use-utf)
  (decode-coding-region (point-min) (point-max) 'utf-16-le-dos))
(defalias 'decode-xml 'decode-utf16)


(defun rebind-to (key map-or-cmd required-lib-name)
  "Custom function to do take over for `autoload' when it fails to do its 
job (which happens from time to time) {jpw: 02/05}."
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  ;; Signals an error, so no need to handle specially.
  (require required-lib-name)
  ;; Must check here, since `map-name' may not exist until `required-lib-name'
  ;; is loaded.
  (or (keymapp map-or-cmd)
      (commandp map-or-cmd)
      (signal 'wrong-type-argument (list 'keymapp 'commandp map-or-cmd)))
  (local-unset-key key)
  (global-set-key key map-or-cmd)
  ;; Run that key, as if we've always been bound to it.
  (if (commandp map-or-cmd)
      (funcall map-or-cmd)
    ;;else
    (message "Rebound: %s.  Retype it now to use." key)
    ;;; This doesn't work.  Need to figure out why.
    ;;(progn 
    ;;  (add-to-list 'unread-command-events
    ;;               (car (listify-key-sequence key)))
    ;;  (read-key-sequence nil)
    ;;  )
    );;end if
  ) ;; end defun


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
extended using an EOL-\"\\\"-char.  {jpw; 12/04}"
  (and
   (save-excursion
     (goto-char (point-min))
     (and (re-search-forward "^#.*\\\\[ \t]*$" (point-max) t)
          (or (beginning-of-line) t)
          (re-search-forward
           (concat "\\\\[ \t]*\nexec .*" 
                   regex  
                   ".* `basename \"?\\${?0}?\"?` .*\"?\\${?[@*1]}?\"?")
           (point-max) t)
          ))
   (funcall mode)
   )
  )


(defun jpw-flip-to-tcl-mode ()
  (interactive)
  (jpw-flip-to-mode "tclsh" 'tcl-mode)
  )


(defun bind-jpw-c-mode-doxy ()
  (interactive)
  ;; Create keymap
  ;; Define doc-comment keybindings.
  (local-set-key [?\C-c ?\C-e] (lambda() (interactive)
                                   (jpw-insert-doxygen-tag "em")))
  (local-set-key [?\C-c ?\C-t] (lambda() (interactive)
                                 (jpw-insert-doxygen-tag "tt")))
  (local-set-key [?\C-c ?\C-b] (lambda() (interactive)
                                 (jpw-insert-doxygen-tag "b")))
  (local-set-key [?\C-c ?\C-j] 
                 (lambda () (interactive)
                   (do-comment-line-break) 
                   (insert "<p>") 
                   (do-comment-line-break))
                 )
  (local-set-key [?\C-c ?\C-q] (lambda() (interactive)
                                 (jpw-insert-doxygen-cmdblock "code")))
  )


(defun bind-jpw-javadoc ()
  (interactive)
  ;; Define doc-comment keybindings.
  (bind-jpw-c-mode-doxy)
  (local-set-key [?\C-c ?\C-c] (lambda() (interactive)
                                   (jpw-insert-doxygen-tag "code")))
  (local-set-key [?\C-c ?\C-q] (lambda() (interactive)
                                 (jpw-insert-doxygen-tagblock "pre")))
  (local-set-key [?\C-c ?\C-r] 'jpw-insert-javadoc-link)
  (local-set-key [?\C-c l] 'jpw-insert-javadoc-link)
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


(defun use-jpw-style-html-helper ()
  (interactive)
  (setq fill-column 78)
  (turn-on-auto-fill)
  (local-unset-key [f4])
  (font-lock-mode t)
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
  (setq c-indent-comments-syntactically-p 't
        c-tab-always-indent "partial"
        )

  ;; Moves forward by capitalizations or words.  Very useful for C++ &
  ;; Java programming.
  (local-set-key [?\C-c right] 'c-forward-into-nomenclature)
  (local-set-key [\C-\S-right] 'c-forward-into-nomenclature)
  (local-set-key [?\C-c left] 'c-backward-into-nomenclature)
  (local-set-key [\C-\S-left] 'c-backward-into-nomenclature)
  ;; Force use of correct comment-break-fn.  
  (local-set-key "\M-j" 'do-comment-line-break)
  (bind-jpw-c-mode-doxy)
  )


(defun use-jpw-style-c ()
  (interactive)
  (c-set-style "jpw")
  ;; Make sure this is set correctly...
  (local-unset-key [f4])
  )


(defun use-jpw-style-java ()
  (interactive)
  (c-set-style "jpw-java")
  (bind-jpw-javadoc)
  (local-unset-key [f4])
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
  (sql-highlight-oracle-keywords)
  )


(defun use-jpw-style-cperl ()
  (interactive)
  (auto-fill-mode 1)
  ;; Use a style equivalent to perl-mode indentation.
  (cperl-set-style "PerlStyle")
  (setq cperl-label-offset -2)
)


(defun use-jpw-style-octave ()
  (interactive)
  (turn-on-auto-fill)
  (font-lock-mode)
  (local-set-key "\M-j" 'octave-indent-new-comment-line)
  )


(defun jpw-set-sgml-indent (arg)
 "Set indentation size(s) for SGML modes. {jpw: 11/05}"
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
  "Set indentation size for XML-Lite mode. {jpw: 3/03}"
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


(provide 'custom-defuns)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;