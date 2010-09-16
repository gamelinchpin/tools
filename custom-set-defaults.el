;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Core Emacs Setup File
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
;; Modify the customization defaults, based on the settings that I want to
;; use.
;;
;; Load this file in your ".emacs", after the customization block.
;;
;;
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'custom)
(require 'cus-face)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Defuns
;;
;;   Moved here, in the hope that they'll be present and available when this
;;   file is loaded at startup.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jpw-custom-set-variables-nonsaved  (&rest args)
  "Initializes the default value of a customization variable.

Calls `custom-set-variables' on the list of arguments, then converts the
\"saved value\" to the \"default value\".  This prevents localized
customizations from being written to your \".emacs\" file.

Note that this function may require modification whenever
`custom-declare-variable' changes.  {jpw: 9/2004}"
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
                 (put symbol 'force-value nil)
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

Note that this function may require modification whenever
`custom-declare-face' changes.  {jpw: 9/2004}"
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customization-Menu Variables.
;;
;;   This block only changes what emacs thinks the default values of these
;;   defcustom vars are.  It will NOT change the user's .emacs file when they
;;   next save their customizations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Misc Default Filenames
(jpw-custom-set-variables-nonsaved
 '(abbrev-file-name "~/.emacs.d/.abbrevs" t)
 '(ps-printer-name "~/emacs-out.ps")
 '(quickurl-url-file "~/.emacs.d/.quickurls")
 )


;; Frame Settings
(jpw-custom-set-variables-nonsaved
 '(column-number-mode t)
 '(line-number-mode t)
 '(minibuffer-prompt-properties (quote (read-only
                                        t
                                        point-entered
                                        minibuffer-avoid-prompt
                                        face minibuffer-prompt)))
 '(scroll-bar-mode (quote right))
 )


;; General Behavior
(jpw-custom-set-variables-nonsaved
 '(case-fold-search t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(default-major-mode (quote text-mode))
 '(history-delete-duplicates t)
 '(revert-without-query (quote (".*")))
 '(safe-local-variable-values
   (quote ((coding-system . utf-8-unix)
           (coding-system . utf-8)
           (buffer-file-coding-system . utf-8)
           (buffer-file-coding-system-explicit . mule-utf-8-dos)
           (tab-stop-list 8 16 24 32 40 48 56 64 72))))
 '(save-abbrevs (quote silently))
 )


;; Misc. Modes to Enable by Default
(jpw-custom-set-variables-nonsaved
 '(abbrev-mode t)
 '(partial-completion-mode t)
 )


;; Editing Behavior
(jpw-custom-set-variables-nonsaved
 '(blink-matching-paren t)
 '(fill-column 78)  ;; For all but text mode.
 '(indent-tabs-mode 'nil) ;; Auto-convert Tabs to Spaces.
 '(next-line-add-newlines nil) ;; [Down] doesn't add \n at EOB
 '(require-final-newline nil)
 '(tab-width 4)  ;; Tab every 4 spaces.
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40
                           44 48 52 56 60 64 68 72 76)) )
 )


;; Keep Running File Versions (swiped from Jerry Leichter)
(jpw-custom-set-variables-nonsaved
 '(delete-old-versions nil)
 '(kept-new-versions 100)
 '(kept-old-versions 0)
 '(version-control t)
 )


;; Fontification-Related
(jpw-custom-set-variables-nonsaved
 '(global-font-lock-mode t nil (font-lock))
 '(transient-mark-mode t)
 '(show-paren-mode t nil (paren))
 '(show-paren-delay 0.5 nil (paren))
 '(show-paren-style (quote parenthesis) nil (paren))
 )


;; Settings for Multiple Languages:  use Latin1
(jpw-custom-set-variables-nonsaved
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 )


;; Programming-Related Settings
(jpw-custom-set-variables-nonsaved
 '(align-c++-modes (quote (c++-mode c-mode
                                    java-mode javascript-generic-mode)))
 '(align-open-comment-modes (quote (vhdl-mode
                                    emacs-lisp-mode
                                    lisp-interaction-mode
                                    lisp-mode
                                    scheme-mode c++-mode c-mode
                                    java-mode
                                    perl-mode cperl-mode
                                    python-mode
                                    makefile-mode
                                    javascript-generic-mode)))
 '(c-doc-comment-style (quote ((c-mode . javadoc)
                               (c++-mode . javadoc)
                               (java-mode . javadoc)
                               (pike-mode . autodoc))))
 '(c-echo-syntactic-information-p t)
 '(c-report-syntactic-errors t)
 '(compile-command "LC_CTYPE=ascii make -k ")
 '(cperl-font-lock t)
 '(cperl-highlight-variables-indiscriminately t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain)
                                nil (ediff-wind))
 '(generic-define-mswindows-modes t)
 '(generic-define-unix-modes t)
 '(grep-command
   "grep --binary-files=without-match --exclude=\\*.svn\\* -n -r -P ")
 '(vc-handled-backends (quote (SVN RCS CVS SCCS)))
 '(which-function-mode nil nil (which-func))
 )


;; Time Display Settings (only shown in TTYs)
(jpw-custom-set-variables-nonsaved
 '(display-time-24hr-format t nil (time))
 '(display-time-mail-face (quote mode-line) nil (time))
 '(display-time-mail-file (quote none) nil (time))
 '(display-time-string-forms
   (quote ((format-time-string (or
                                display-time-format
                                (if display-time-24hr-format
                                    "%H:%M" "%-I:%M%p")) now)))
   nil (time)))


;; Org/Outline Mode
(jpw-custom-set-variables-nonsaved
 '(org-archive-tag ":ARCHIVE:" nil (org))
 '(org-blank-before-new-entry (quote ((heading . t) (plain-list-item)))
                              nil (org))
 '(org-comment-string ":COMMENT:" nil (org))
 '(org-cycle-include-plain-lists t nil (org))
 '(org-ellipsis "···
"
                nil (org))
 '(org-export-headline-levels 6 nil (org))
 '(org-insert-heading-respect-content
   t nil (org)) ;; Insert heading after body.
 '(org-level-color-stars-only nil nil (org))
 '(org-publish-timestamp-directory "~/.emacs.d/.org-timestamps/" nil (org))
 '(org-quote-string ":QUOTE:" nil (org))
 '(org-special-ctrl-a/e (quote reversed) nil (org))
 '(org-startup-folded t nil (org)) ;; Just top-level headings.
 '(org-startup-truncated nil nil (org))
 '(org-todo-keywords (quote ("TODO" "STARTED" "DONE")) nil (org))
 '(org-use-extra-keys t nil (org))
 '(outline-regexp "[*§¶­]+" nil (outline))
 )


;; Speedbar
(jpw-custom-set-variables-nonsaved
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|.svn\\)\\'")
 '(speedbar-update-speed 300 t)
 )


;; TRAMP Mode Settings
(jpw-custom-set-variables-nonsaved
 '(password-cache-expiry 86400)
 '(tramp-auto-save-directory "/tmp/")
 '(tramp-default-method "sftp")
 )


;; WOMAN Settings
(jpw-custom-set-variables-nonsaved
 '(woman-cache-filename "~/.emacs.d/.wmncache.el" nil (woman))
 '(woman-cache-level 1 nil (woman))
 '(woman-use-own-frame nil nil (woman))
 )


;; Some GNU-Emacs-specific settings.
(if running-gnu-emacs
    (progn
      ;;
      ;; Customization-Menu Variables.
      (jpw-custom-set-variables-nonsaved
       '(colon-double-space t)
       '(global-font-lock-mode t nil (font-lock))
       '(initial-buffer-choice t)
       '(inhibit-startup-message t)
       '(inhibit-startup-screen t)
       '(initial-scratch-message nil)
       '(kill-ring-max 100)
       '(longlines-show-effect "¶
")
       '(mouse-wheel-mode t nil (mwheel))
       '(mouse-wheel-follow-mouse nil)
       '(recentf-save-file "~/.emacs.d/.recentf")
       )

      (jpw-custom-set-variables-nonsaved
       '(type-break-mode-line-message-mode nil nil (type-break))
       '(type-break-keystroke-threshold (quote (10000)) nil (type-break))
       '(type-break-interval 6000 nil (type-break))
       '(type-break-good-rest-interval 60 nil (type-break))
       '(type-break-query-interval 180 nil (type-break))
       '(type-break-file-name "~/.emacs.d/.type-break" nil (type-break))
       '(type-break-time-warning-intervals (quote (60 30)) nil (type-break))
       )
      );; end progn
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Font-lock colors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Only turn it on automatically if we have windows (X11) or are on a
;; supported terminal type
(if is-fontifiable-term
    (progn
      (require 'font-lock)

      (if running-gnu-emacs
        (progn
          (jpw-custom-set-faces-nonsaved
           ;; General
           '(italic ((t (:foreground "#544080" :slant italic))))
           '(region ((t (:background "LightBlue"))))
           '(highlight ((t (:background "Gray"))))
           '(underline ((t (:underline "purple4"))))
           ;; Faces that inherit from others.
           '(font-lock-doc-face
             ((t (:inherit font-lock-comment-face :background "azure"))))
           '(bold-italic ((t (:inherit (bold italic)))))
           '(woman-bold ((t (:inherit bold :foreground "blue"))))
           '(woman-italic
             ((t (:inherit italic :foreground "Purple4" :underline t))))
           )

          (if (or is-version-twentytwo is-version-twentythree)
              (progn
                (jpw-custom-set-faces-nonsaved
                 '(develock-long-line-1 ((t (:foreground "DeepPink"))))
                 '(develock-long-line-2 ((t
                                          (:inherit develock-long-line-1
                                                    :background "#ffff7f"
                                                    :foreground "DeepPink3")
                                          )))
                 '(develock-whitespace-1 ((t (:background "#ffdfdf"))))
                 '(develock-whitespace-2
                   ((t
                     (:background "#ffdfbf"
                                  :box (:line-width 1 :color "#ffcf9f")))))
                 '(develock-whitespace-3
                   ((t (:background "#ffffbf"
                                    :box (:line-width 1 :color "yellow3")))))
                 '(diff-added ((t (:inherit diff-changed
                                            :foreground "SpringGreen2"
                                            :weight bold))))
                 '(diff-changed ((nil (:foreground "#8000FF" :weight bold))))
                 '(diff-removed ((t (:inherit diff-changed
                                              :foreground "VioletRed4"))))
                 '(escape-glyph ((((class color) (background light))
                                  (:foreground "chartreuse"))))
                 '(font-wikipedia-bold-face ((((class color)
                                               (background light))
                                              (:inherit bold))))
                 '(font-wikipedia-italic-face ((((class color)
                                                 (background light))
                                                (:inherit italic))))
                 '(font-wikipedia-math-face
                   ((((class color) (background light))
                     (:inherit font-lock-function-name-face))))
                 '(font-wikipedia-sedate-face
                   ((((class color) (background light))
                     (:inherit variable-pitch
                               :foreground "SlateGray" :height 1.25))))
                 '(font-wikipedia-string-face
                   ((((class color) (background light))
                     (:inherit font-lock-string-face))))
                 '(font-wikipedia-verbatim-face
                   ((((class color) (background light))
                     (:inherit font-lock-constant-face))))
                 '(minibuffer-prompt ((((class color) (background light))
                                       (:foreground "blue"))))
                 '(mode-line
                   ((t (:background "plum3" :foreground "black"
                                    :box (:line-width -1
                                                      :style released-button)
                                    )))
                   nil
                   "For when I'm in the mood for a more colorful modeline,
                    use this instead of \"grey75\".")
                 '(mode-line-highlight
                   ((t (:foreground "green4"
                                    :box (:line-width 2 :color "grey40"
                                                      :style released-button)
                                    )))
                   nil
                   "Just using a box with a darker gray is unsatisfying.
                    Let's change the text color to something that will stand
                    out (but not water our eyes).  Change the modeline
                    color, and we may need to change this.")
                 '(mode-line-inactive
                   ((t (:inherit mode-line
                                 :background "PaleGoldenrod"
                                 :foreground "grey35"
                                 :box (:line-width -1
                                                   :style released-button)
                                 :weight light)))
                   nil
                   "To accompany my more colorful modeline, I'll pick an
                    off-white color for the inactive modeline instead of
                    \"grey90\"")
                 '(sh-heredoc ((((class color) (background light))
                                (:inherit font-lock-string-face
                                          :background "beige"))))
                 '(show-paren-match ((((class color))
                                      (:background "yellow"))) t)
                 '(show-paren-mismatch ((((class color))
                                         (:background "DarkOrange"))) t)
                 '(trailing-whitespace
                   ((t (:background "#ffdfdf"
                                    :box (:line-width 1 :color "#ff9fbf")))))
                 )
                )
            );; end if GNU Emacs v22
          )
        ) ;; end if XEmacs

      (if (not window-system)
          (progn
            (jpw-custom-set-faces-nonsaved
             '(region ((t (:background "cyan" :foreground "black"))))
             '(secondary-selection
               ((t (:background "blue" :foreground "white"))))
             '(highlight ((t (:background "yellow")))))
            )
        ) ;; end if !window-system
      ) ;;end progn
  );; end if is-fontifiable-term-type


(provide 'custom-set-defaults)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;
