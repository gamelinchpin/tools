;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Core Emacs Setup File
;;
;;  Copyright © 1995-2012 John P. Weiss
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
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'custom)
(require 'cus-face)
(require 'custom-vars)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Defuns
;;
;;   Moved here, in the hope that they'll be present and available when this
;;   file is loaded at startup.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftheme jpw-cust-defaults-theme
  "A theme containing all of the customization variables & faces defaults that
I use.

{jpw: 12/2012}")


(defun jpw-custom-set-variables-nonsaved  (&rest args)
  "Initializes the default value of a customization variable.

Calls `custom-set-variables' on the list of arguments, then
passes them to `custom-theme-set-variables' using the
`jpw-cust-defaults-theme'.

Lastly, it attempts to convert the \"saved value\" to the \"default value\".
This is in hope that localized customizations won't be written to your
\".emacs\" file in older versions of Emacs.

`args' is a list of lists of the form:

   (VARNAME VAL_EXPRESSION [MAKE_DEFAULT_AND_EVAL_NOW
                               [REQUIRED_FEATURE_LIST [COMMENT]]])

REQUIRED_FEATURE_LIST is a list of packages to load before setting VARNAME to
the value of VAL_EXPRESSION.  Note that this function partially-overrides
MAKE_DEFAULT_AND_EVAL_NOW (since it always makes the \"new\" value the
default).
{jpw: 12/2012}"
  (apply 'custom-set-variables args)
  (apply 'custom-theme-set-variables 'jpw-cust-defaults-theme args)

  ;; The attempt to convert the saved value to the default value:
  (while args
    (let ((entry (car args)))
      (if (listp entry)
          (let* ((symbol (nth 0 entry))
                 (nuval (nth 1 entry))
                 (value (or (get symbol 'customized-value)
                            (get symbol 'saved-value)
                            nuval
                            ))
                 )
            ;; Before we try to convert the saved value to the new default,
            ;; let's force-set it, just to be sure it's done.
            ;; [20121218] Well, this next expression works *sometimes*.  But
            ;;            other times, it just fails with a
            ;;            value-type-mismatch error.
            ;;            Using `funcall' or `eval' fails in even worse ways.
            ;;            Using `apply' is no different than the direct call.
            ;;            And `setq' fails right-out.
            ;;(set symbol value)

            (if value
                (progn
                  (put symbol 'standard-value value)
                  (put symbol 'default-value value)
                  (put symbol 'customized-value value)
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

Calls `custom-set-faces' on the list of arguments, then passes
them to `custom-theme-set-faces' using the
`jpw-cust-defaults-theme'.

Lastly, it attempts to convert the \"saved value\" to the \"default value\".
This is in hope that localized customizations won't be written to your
\".emacs\" file in older versions of Emacs.
{jpw: 9/2004}"
  (apply 'custom-set-faces args)
  (apply 'custom-theme-set-faces 'jpw-cust-defaults-theme args)

  ;; The attempt to convert the saved value to the default value:
  (while args
    (let ((entry (car args)))
      (if (listp entry)
          (let* ((face (nth 0 entry))
                 (nuspec (nth 1 entry))
                 (spec (or (get face 'customized-face)
                           (get face 'saved-face)
                           nuspec
                           ))
                 )
            (if spec
                (progn
                  ;; The last arg changes the base-spec (i.e. the default
                  ;; one).
                  (face-spec-set face spec t)
                  (put face 'force-face nil)
                  (put face 'saved-face nil)
                  )) ;; end if
            );; end let*
        ));; end (let ... (if ...
    (setq args (cdr args))
    );; end while
  )


;;
;; "'.emacs'-Only" Customization-Menu Variables:
;;
;;   These are defcustom vars that `jpw-custom-set-variables-nonsaved' fails
;;   to change.  Or at least it seems that way.
;;
;;   You have to leave the following in your '~/.emacs' file's
;;   `custom-set-variables' block:
;;
;;       blink-matching-paren
;;       blink-matching-paren-distance
;;       c-doc-comment-style
;;       c-echo-syntactic-information-p    (optional; see below)
;;       c-report-syntactic-errors         (optional; see below)
;;       paren-message-linefeed-display
;;       password-cache-expiry
;;       recentf-save-file
;;       tramp-auto-save-directory
;;       tramp-default-method
;;       which-function-mode
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customization-Menu Variables.
;;
;;   This block only changes what emacs thinks the default values of these
;;   defcustom vars are.  It will NOT change the user's .emacs file when they
;;   next save their customizations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Filenames and Paths
(jpw-custom-set-variables-nonsaved
 '(desktop-path (quote ("~/.emacs.d" "." "~")))  ;; No longer a variable?
 '(abbrev-file-name "~/.emacs.d/.abbrevs" t)
 '(ps-printer-name "~/emacs-out.ps")  ;; No longer a variable?
 '(quickurl-url-file "~/.emacs.d/.quickurls")  ;; No longer a variable?
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
 ;; obsolete since 23.2
 '(default-major-mode (quote text-mode))
 ;; replacement for `default-major-mode'
 '(major-mode 'text-mode)
 '(history-delete-duplicates t)
 '(revert-without-query (quote (".*")))
 '(enable-local-eval t)
 '(safe-local-eval-forms (quote ((add-hook
                                  (quote write-file-hooks)
                                  (quote time-stamp)
                                  (quote sh-set-shell)
                                  ))))
 '(safe-local-variable-values
   (quote ((buffer-file-coding-system . utf-16)
           (buffer-file-coding-system . utf-8)
           (buffer-file-coding-system-explicit . mule-utf-16-dos)
           (buffer-file-coding-system-explicit . mule-utf-8-dos)
           (coding-system . utf-16)
           (coding-system . utf-16-unix)
           (coding-system . utf-8)
           (coding-system . utf-8-unix)
           (sql-product . Postgres)
           (sql-product . ansi)
           (tab-stop-list 8 16 24 32 40 48 56 64 72))))
 '(save-abbrevs (quote silently))
 )


;; Misc. Settings and Modes to Enable by Default
(jpw-custom-set-variables-nonsaved
 '(archive-zip-use-pkzip nil)  ;; No longer a variable?
 '(browse-url-browser-function (quote browse-url-w3))
 '(before-save-hook (quote (copyright-update
                            jpw-clean-trailing-whitespace)))
 ;; Modes to enable by default:
 '(abbrev-mode t)
 '(partial-completion-mode t)
 )


;; Editing Behavior
(jpw-custom-set-variables-nonsaved
 '(fill-column 78)  ;; For all but text mode.
 '(indent-tabs-mode 'nil) ;; Auto-convert Tabs to Spaces.
 '(next-line-add-newlines nil) ;; [Down] doesn't add \n at EOB
 '(require-final-newline nil)
 '(tab-width 4)  ;; Tab every 4 spaces.
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40
                           44 48 52 56 60 64 68 72 76)) )
 )


;; Session and File Version Control
(jpw-custom-set-variables-nonsaved
 '(session-initialize (quote (de-saveplace session places keys menus))
                      nil
                      (session))
 '(session-name-disable-regexp
   "\\`\\(/tmp\\|~/tmp/\\(itsalltext\\|mozex.wrk\\)\\)")
 '(session-save-file "~/.emacs.d/.session")
 ;; Below:  Keep Running File Versions (swiped from Jerry Leichter)
 '(delete-old-versions nil)
 '(kept-new-versions 100)
 '(kept-old-versions 0)
 '(version-control t)
 )


;; Fontification- and Display-Related
(jpw-custom-set-variables-nonsaved
 '(global-font-lock-mode t nil (font-lock))
 '(transient-mark-mode t)
 '(blink-matching-paren t nil (simple)) ;; FIXME:  Not being set.
 '(blink-matching-paren-distance nil nil (simple)) ;; FIXME:  Not being set?
 ;;'(show-paren-mode t nil (paren))
 ;; `show-paren-mode' disables `blink-matching-paren' mode.  :P
 '(show-paren-mode nil nil (paren))
 '(show-paren-delay 0.5 nil (paren))
 '(show-paren-style (quote parenthesis) nil (paren))
 '(paren-message-linefeed-display "¶") ;; FIXME:  Not being set?
 )


;; Settings for Multiple Languages:  use Latin1
(jpw-custom-set-variables-nonsaved
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 )


;; Programming-Related Settings
(setq-default ess-language "R")
(jpw-custom-set-variables-nonsaved
 ;; No longer a variable?
 '(align-c++-modes (quote (c++-mode c-mode
                                    java-mode javascript-generic-mode)))
 ;; No longer a variable?
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
 ;; FIXME:  Not being set.
 '(c-doc-comment-style (quote ((c-mode . javadoc)
                               (c++-mode . javadoc)
                               (java-mode . javadoc))))
 ;; NOTE[2012-06-19]:  The following two have been disabled.  There's no need
 ;;                    for them unless one's debugging a c-style programming
 ;;                    mode.
 ;;'(c-echo-syntactic-information-p t)   ;; FIXME:  Not being set.
 ;;'(c-report-syntactic-errors t)  ;; FIXME:  Not being set.
 '(compile-command "LC_CTYPE=ascii make -k ")
 '(cperl-font-lock t)
 '(cperl-highlight-variables-indiscriminately t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain)
                                nil (ediff-wind))

 '(ess-default-style (quote OWN))
 '(ess-fancy-comments nil)
 '(ess-language "R")
 '(ess-own-style-list (quote ((ess-indent-level . 2) (ess-continued-statement-offset . 2) (ess-brace-offset . 0) (ess-expression-offset . 4) (ess-else-offset . 0) (ess-brace-imaginary-offset . 0) (ess-continued-brace-offset . 0) (ess-arg-function-offset-new-line . 2) (ess-close-brace-offset . 0))))

 ;; `generic-define-*-modes is now obsolete, replaced by
 ;;`generic-extras-enable-list'
 '(generic-define-mswindows-modes t)
 '(generic-define-unix-modes t)
 '(generic-extras-enable-list (quote (alias-generic-mode
                                      apache-conf-generic-mode
                                      apache-log-generic-mode
                                      bat-generic-mode
                                      etc-fstab-generic-mode
                                      etc-modules-conf-generic-mode
                                      etc-passwd-generic-mode
                                      etc-services-generic-mode
                                      etc-sudoers-generic-mode
                                      hosts-generic-mode
                                      inf-generic-mode
                                      ini-generic-mode
                                      java-manifest-generic-mode
                                      java-properties-generic-mode
                                      javascript-generic-mode
                                      mailagent-rules-generic-mode
                                      mailrc-generic-mode
                                      prototype-generic-mode
                                      rc-generic-mode
                                      reg-generic-mode
                                      resolve-conf-generic-mode
                                      rul-generic-mode
                                      samba-generic-mode
                                      show-tabs-generic-mode
                                      x-resource-generic-mode))
                              nil (generic-x))

 '(grep-command
   "grep --binary-files=without-match --exclude=\\*.svn\\* -n -r -P ")

 '(inferior-ess-same-window nil)

 '(vc-handled-backends (quote (SVN RCS CVS SCCS)))
 ;; NOTE[2012-06-19]:  I don't want to see the current fn. name.  Makes the
 ;;                    mode-bar too cluttered.
 '(which-function-mode nil nil (which-func))  ;; FIXME:  Not being set?
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
 '(org-alphabetical-lists t nil (org)) ;; FIXME:  Not being set?
 '(org-archive-tag ":ARCHIVE:" nil (org))
 '(org-blank-before-new-entry (quote ((heading . t) (plain-list-item)))
                              nil (org))
 '(org-comment-string ":COMMENT:" nil (org))
 '(org-cycle-global-at-bob t)
 '(org-cycle-include-plain-lists t nil (org))
 '(org-ellipsis "···
"
                nil (org))
 '(org-enable-fixed-width-editor nil)
 '(org-export-headline-levels 6 nil (org))  ;; No longer a variable?
 '(org-hide-emphasis-markers t)
 '(org-imenu-depth 4)
 '(org-insert-heading-respect-content
   t nil (org)) ;; Insert heading after body.
 '(org-insert-mode-line-in-empty-file t)
 '(org-level-color-stars-only nil nil (org))
 '(org-list-demote-modify-bullet (quote (("-" . "+")
                                         ("+" . "¤")
                                         ("¤" . "·")
                                         ("·" . "°")
                                         ("°" . "»")
                                         ("»" . "-")
                                         ("*" . "1)")
                                         ("1." . "a.")
                                         ("a." . "1)")
                                         ("1)" . "a)")
                                         ("a)" . "1.")
                                         )))
 '(org-modules (quote (org-mouse)))
 ;; No longer a variable?
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
 ;; According to its documentation, this next var doesn't need to include
 ;; version-control dirs.
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|.svn\\)\\'")
 '(speedbar-update-speed 300 t) ;; obsolete since v23.1
 ;; Global-replacement for `speedbar-update-speed'.  But, because it's global,
 ;; it affects every frame.  Feh!
 ;;[replace at later date]'(dframe-update-speed 30 t) ;; default is 1
 )


;; TRAMP Mode Settings
;; FIXME:  Not being set.
;;Possibly being overwritten by the pkg. itself when loaded?
(jpw-custom-set-variables-nonsaved
 '(password-cache-expiry 86400 t (password-cache))
 '(tramp-auto-save-directory "/tmp/" t (tramp))
 '(tramp-default-method "scp" t (tramp))
 )


;; WOMAN Settings
(jpw-custom-set-variables-nonsaved
 '(woman-cache-filename "~/.emacs.d/.wmncache.el" nil (woman))
 '(woman-cache-level 1 nil (woman))
 '(woman-use-own-frame nil nil (woman))
 )

;; WOMAN:  OS-Specific `woman-manpath'
(cond
 (is-cygwin
  (jpw-custom-set-variables-nonsaved
   '(woman-manpath (quote (("/bin" . "/usr/share/man")
                           ("/usr/bin" . "/usr/share/man")
                           ("/sbin" . "/usr/share/man")
                           ("/usr/sbin" . "/usr/share/man")
                           ("/usr/local/bin" . "/usr/local/man")
                           ("/usr/local/bin" . "/usr/local/share/man")
                           ("/usr/local/sbin" . "/usr/local/man")
                           ("/usr/local/sbin" . "/usr/local/share/man")
                           ("/usr/games" . "/usr/share/man")
                           ("/opt/bin" . "/opt/man")
                           ("/opt/bin" . "/opt/share/man")
                           ("/opt/sbin" . "/opt/man")
                           ("/opt/sbin" . "/opt/share/man")
                           )
                          );;end quote
                   nil (woman));; end woman-manpath
   );;end jpw-custom-set-variables-nonsaved
  ;; Ensure that this case succeeds even if the body doesn't.
  t
  );; end is-cygwin

 (is-winblows
  (jpw-custom-set-variables-nonsaved
   '(woman-manpath (quote ("/usr/share/man"
                           "/usr/local/share/man"
                           "/opt/share/man/"
                           ("/bin" . "/usr/share/man")
                           ("/usr/bin" . "/usr/share/man")
                           ("/sbin" . "/usr/share/man")
                           ("/usr/sbin" . "/usr/share/man")
                           ("/usr/local/bin" . "/usr/local/man")
                           ("/usr/local/bin" . "/usr/local/share/man")
                           ("/usr/local/sbin" . "/usr/local/man")
                           ("/usr/local/sbin" . "/usr/local/share/man")
                           ("/usr/games" . "/usr/share/man")
                           ("/opt/bin" . "/opt/man")
                           ("/opt/sbin" . "/opt/man")
                           )
                          );;end quote
                   nil (woman));; end woman-manpath
   );;end jpw-custom-set-variables-nonsaved
  ;; Ensure that this case succeeds even if the body doesn't.
  t
  );; end is-winblows

 (t ;; default case
  (jpw-custom-set-variables-nonsaved
   '(woman-manpath (quote (("/bin" . "/usr/share/man")
                           ("/usr/bin" . "/usr/share/man")
                           ("/sbin" . "/usr/share/man")
                           ("/usr/sbin" . "/usr/share/man")
                           ("/usr/local/bin" . "/usr/local/man")
                           ("/usr/local/bin" . "/usr/local/share/man")
                           ("/usr/local/sbin" . "/usr/local/man")
                           ("/usr/local/sbin" . "/usr/local/share/man")
                           ("/usr/games" . "/usr/share/man")
                           ("/opt/bin" . "/opt/man")
                           ("/opt/sbin" . "/opt/man")
                           ("/opt/trinity/bin" . "/opt/trinity/share/man")
                           ;; [jpw; 2012] Old paths.  Remove eventually...
                           ("/usr/X11R6/bin" . "/usr/X11R6/man")
                           ("/usr/bin/X11" . "/usr/X11R6/man")
                           ("/opt/kde3/bin" . "/opt/kde3/share/man")
                           )
                          );;end quote
                   nil (woman));; end woman-manpath
   );;end jpw-custom-set-variables-nonsaved
  ;; Ensure that this case succeeds even if the body doesn't.
  t
  );; end default
 );; end cond


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
       '(mouse-wheel-follow-mouse nil nil (mwheel))
       '(recentf-save-file "~/.emacs.d/.recentf") ;; FIXME:  Not being set.
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

           ;; Programming Mode
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

           '(ess-function-call-face
             ((default (:inherit font-lock-function-name-face))
              (nil (:foreground "#1f3fff"))))

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

           '(html-helper-bold-face
             ((t (:foreground nil :weight normal :inherit (bold)))))
           '(html-helper-builtin-face
             ((t (:foreground nil :weight normal
                              :inherit (font-lock-builtin-face)))))
           '(html-helper-italic-face
             ((t (:foreground nil :weight normal :inherit (italic)))))
           '(html-helper-underline-face
             ((t (:foreground nil :weight normal :inherit (underline)))))
           '(html-tag-face
             ((t (:foreground nil :weight normal
                              :inherit (font-lock-function-name-face)))))

           '(org-agenda-done
             ((((class color) (min-colors 16) (background light))
               (:inherit org-done :weight normal))))
           '(org-block ((t (:inherit font-lock-comment-face))))
           '(org-code ((t (:inherit font-lock-constant-face))))
           '(org-document-info
             ((((class color) (background light))
               (:inherit font-lock-doc-face))))
           '(org-document-info-keyword
             ((t (:inherit font-lock-builtin-face))))
           '(org-done ((t (:background "#e0ffe0"
                                       :foreground "midnight blue"
                                       :weight bold))))
           '(org-formula
             ((((class color) (min-colors 88) (background light))
               (:inherit font-lock-type-face))))
           '(org-level-1
             ((t (:inherit outline-1 :overline t :underline t))))
           '(org-level-2
             ((t (:inherit outline-2 :overline t :underline t))))
           '(org-level-3
             ((t (:inherit outline-3 :overline t :underline t))))
           '(org-level-4
             ((t (:inherit outline-4 :overline t :underline t))))
           '(org-level-5
             ((t (:inherit outline-5 :overline t :underline t))))
           '(org-level-6
             ((t (:inherit outline-6 :overline t :underline t))))
           '(org-level-7
             ((t (:inherit outline-7 :overline t :underline t))))
           '(org-level-8
             ((t (:inherit outline-8 :overline t :underline t))))
           '(org-scheduled
             ((((class color) (min-colors 88) (background light))
               (:foreground "SeaGreen2"))))
           '(org-scheduled-today
             ((t (:inherit org-scheduled :weight bold))))
           '(org-special-keyword
             ((((class color) (min-colors 16) (background light))
               (:inherit font-lock-keyword-face))))
           '(org-todo ((t (:background "LemonChiffon"
                                       :foreground "DeepPink2"
                                       :weight bold))))
           '(org-verbatim ((t (:inherit font-lock-constant-face))))

           '(sh-heredoc ((t (:inherit (font-lock-doc-face)))))
           )

          (if (or is-version-twentytwo is-version-twentythree)
              (progn
                (jpw-custom-set-faces-nonsaved
                 '(escape-glyph ((((class color) (background light))
                                  (:foreground "chartreuse"))))

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
                                 :background "LemonChiffon3"
                                 :foreground "grey35"
                                 :box (:line-width -1 :style released-button)
                                 :weight light)))
                   nil
                   "To accompany my more colorful modeline, I'll pick an
                    off-white color for the inactive modeline instead of
                    \"grey90\"")

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

(provide-theme 'jpw-cust-defaults-theme)
(enable-theme 'jpw-cust-defaults-theme)

(provide 'custom-set-defaults)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;
