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
;; All startup tasks, general setting, special file suffix defns., custom
;; general font-lock colors and customizations for simple modes go
;; here.  Loads the other `custom-*' files that it needs.
;;
;; · Keybindings have their own file.  Do not put keybindings here.
;; · Autohooks have their own file.  Do not put autohooks here.
;; · EmacsNT-specific and Win32-specific settings have their own file.  Do
;;   not put EmacsNT-specific or Win32-specific settings here.
;; · Mutt-mode has its own file.
;;   So do several other modes not loaded every startup.
;;
;;
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Inhibit displaying the startup message -EWINK
;; Put this in your own .emacs file
;; (setq inhibit-startup-message t)

(defconst term-lc (or (getenv "TERM")
                      (and (boundp 'term)
                           (setq term (downcase term))
                           )
                      )
  "The value of the \"TERM\" environment variable lowercased.  Used by the
startup files to shorten the elisp.

You shouldn't change the value of this variable.

{jpw 12/31}")

;; Special flag for WinEmacs
;;
(defconst is-winblows (or (eq window-system 'win32)
                          (eq window-system 'w32)
                          )
  "Set to true if this is a version of emacs for M$ WinBlows.  The
default (and preferred) value is nil.

You shouldn't change the value of this variable.

{jpw 10/01}")


;; Special flag for XEmacs.  Doesn't exist in GNU Emacs, so we'll create it
;; and set it to nil
;;
(eval-and-compile
;;  (if (not (or is-winblows
  (if (not (boundp 'running-xemacs))
      (defconst running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
        "Non-nil when the current emacs is XEmacs."
        )
    )
  )


;; Special flag for WinEmacs
;;
(defconst is-cygwin (or (string-match "cygwin"
                                      (downcase
                                       (or (getenv "OSTYPE") "")))

                        (and (not is-winblows)
                         (string-match "windows"
                                       (downcase
                                        (or (getenv "OS") ""))))
                        (and (not window-system) is-winblows)
                        )
  "Set to true if this is a version of emacs build for Cygwin.  The
default (and preferred) value is nil.

You shouldn't change the value of this variable.

{jpw 11/04}")

;; Version 20 and above changes some old stuff.  To handle those changes, we
;; define and set these variables.
;;
(defconst is-version-twenty (not (eq emacs-major-version '19))
  "Set to true if this is emacs 20.*.*.  The default value is nil.

You shouldn't change the value of this variable.

{jpw 9/98}")

(defconst is-version-twentytwo (eq emacs-major-version '22)
  "Set to true if this is emacs 22.*.*.  The default value is nil.

You shouldn't change the value of this variable.

{jpw 9/07}")


;;
;; Load Custom Functions
;;
(require 'custom-defuns)
(if is-winblows
    (load "custom-winemacs" t)
  )

;;
;; Some libraries to load on startup.
;;

;; Stall loading of the keybindings until *after* the terminal or
;; windows setup files have been loaded
(setq term-setup-hook
      (lambda()
        (load "custom-keybindings" t)
        )
      )

(if running-xemacs
    (setq window-setup-hook
          (lambda()
            (load "custom-keybindings-xemacs" t)
            )
          )
  ;;else
  (setq window-setup-hook
        (lambda()
          (load "custom-keybindings" t)
          )
        )
  )

;; Load mode-hooks
(load "custom-autohooks" t)


;; Non-global libraries to load
;(load "pcl-cvs" t)
(load "crypt++" t)


;;-----------------------------------------------------------


;; Set up a server for emacsclient.  Note that Winblows uses "gnuserv"
;; instead of the standard server package.
;;
(if (and (or is-winblows is-cygwin)
         t)
         ;;(not is-version-twentytwo))
    (if (load "gnuserv" t)   ;; May not have gnuserv on cygwin
        (progn
          (setq gnuserv-frame (selected-frame))
          (gnuserv-start)
          )
        )
  ;;else
  (if (not running-xemacs)
      (progn
        (require 'server)
        (server-start)
      )
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Common Behavioral Variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Customization-Menu Variables.
;; This block only changes what emacs thinks the default values of these
;; defcustom vars are.  It will NOT change the user's .emacs file when they
;; next save their customizations.
;;
(jpw-custom-set-variables-nonsaved
 ;; Misc Default Filenames
 '(abbrev-file-name "~/.emacs.d/.abbrevs" t)
 '(ps-printer-name "~/emacs-out.ps")
 '(quickurl-url-file "~/.emacs.d/.quickurls")

 ;; Frame Settings
 '(column-number-mode t)
 '(line-number-mode t)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(scroll-bar-mode (quote right))

 ;; General Behavior
 '(case-fold-search t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(default-major-mode (quote text-mode))
 '(history-delete-duplicates t)
 '(revert-without-query (quote (".*")))
 '(safe-local-variable-values (quote ((coding-system . utf-8-unix) (coding-system . utf-8) (buffer-file-coding-system . utf-8) (buffer-file-coding-system-explicit . mule-utf-8-dos) (tab-stop-list 8 16 24 32 40 48 56 64 72))))
 '(save-abbrevs (quote silently))

 ;; Misc. Modes to Enable by Default
 '(abbrev-mode t)
 '(partial-completion-mode t)

 ;; Editing Behavior
 '(blink-matching-paren t)
 '(fill-column 78)  ;; For all but text mode.
 '(indent-tabs-mode 'nil) ;; Auto-convert Tabs to Spaces.
 '(next-line-add-newlines nil) ;; [Down] doesn't add \n at EOB
 '(require-final-newline nil)
 '(tab-width 4)  ;; Tab every 4 spaces.
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40
                           44 48 52 56 60 64 68 72 76)) )

 ;; Keep Running File Versions (swiped from Jerry Leichter)
 '(delete-old-versions nil)
 '(kept-new-versions 100)
 '(kept-old-versions 0)
 '(version-control t)

 ;; Fontification-Related
 '(global-font-lock-mode t nil (font-lock))
 '(transient-mark-mode 1)
 '(show-paren-delay 0.5)
 '(show-paren-style (quote mixed))

 ;; Settings for Multiple Languages:  use Latin1
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")

 ;; Programming-Related Settings
 '(align-c++-modes (quote (c++-mode c-mode java-mode javascript-generic-mode)))
 '(align-open-comment-modes (quote (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode perl-mode cperl-mode python-mode makefile-mode javascript-generic-mode)))
 '(c-doc-comment-style (quote ((c-mode . javadoc) (c++-mode . javadoc) (java-mode . javadoc) (pike-mode . autodoc))))
 '(c-echo-syntactic-information-p t)
 '(c-report-syntactic-errors t)
 '(compile-command "LC_CTYPE=ascii make -k ")
 '(cperl-font-lock t)
 '(cperl-highlight-variables-indiscriminately t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(generic-define-mswindows-modes t)
 '(generic-define-unix-modes t)
 '(grep-command "grep --binary-files=without-match --exclude=\\*.svn\\* -n -r -P ")
 '(vc-handled-backends (quote (SVN RCS CVS SCCS)))
 '(which-function-mode nil nil (which-func))

 ;; Time Display Settings (only shown in TTYs)
 '(display-time-24hr-format t)
 '(display-time-mail-face (quote mode-line))
 '(display-time-mail-file (quote none))
 '(display-time-string-forms (quote ((format-time-string (or display-time-format (if display-time-24hr-format "%H:%M" "%-I:%M%p")) now))))

 ;; Org/Outline Mode
 '(org-blank-before-new-entry (quote ((heading . t) (plain-list-item))))
 '(org-cycle-include-plain-lists t)
 '(org-ellipsis "···
")
 '(org-level-color-stars-only nil)
 '(org-startup-folded t) ;; Just top-level headings.
 '(org-startup-truncated nil)
 '(org-todo-keywords (quote ("TODO" "STARTED" "DONE")))
 '(outline-regexp "[*§¶­]+")

 ;; Speedbar
 '(speedbar-update-speed 300 t)
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|.svn\\)\\'")

 ;; TRAMP Mode Settings
 '(password-cache-expiry 86400)
 '(tramp-auto-save-directory "/tmp/")
 '(tramp-default-method "ssh")

 ;; WOMAN Settings
 '(woman-cache-filename "~/.emacs.d/.wmncache.el")
 '(woman-cache-level 1)
 '(woman-use-own-frame nil)
 )

(if running-xemacs
    (progn
      (require 'mwheel)

      (setq abbrev-file-name "~/.xemacs/.abbrevs"
            kill-ring-max 100
            minibuffer-max-depth nil
            mwheel-follow-mouse t
            mwheel-scroll-amount (quote (6 . 1))
            quickurl-url-file "~/.xemacs/.quickurls"
            type-break-file-name "~/.xemacs/.type-break"
            woman-cache-filename "~/.xemacs/.wmncache.el"
            zmacs-regions t
            )
      )
  ;; else:
  ;; Some GNU-Emacs-specific settings.
  ;;
  ;; Customization-Menu Variables.
  (jpw-custom-set-variables-nonsaved
   '(colon-double-space t)
   '(global-font-lock-mode t nil (font-lock))
   '(kill-ring-max 100)
   '(longlines-show-effect "¶
")
   '(mouse-wheel-mode t nil (mwheel))
   '(mouse-wheel-follow-mouse nil)
   '(recentf-save-file "~/.emacs.d/.recentf")
   '(type-break-mode-line-message-mode nil)
   '(type-break-keystroke-threshold (quote (10000)))
   '(type-break-interval 6000)
   '(type-break-good-rest-interval 60)
   '(type-break-query-interval 180)
   '(type-break-file-name "~/.emacs.d/.type-break")
   '(type-break-time-warning-intervals (quote (60 30)))
   )

  ;; Shut off the stoopid toolbar in X.
  (tool-bar-mode -1)

  ;; Activate the recent-file menu
  (recentf-mode)

  ;; Inhibit displaying the startup message -EWINK
  (setq inhibit-startup-message t)
  )


;; From Fred Korz.
;;
;; Activate certain commands disabled by default.
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; PostScript® printout var.
;;
(setq ps-font-size 10)
(setq ps-print-color-p 'nil)

;; Puts hilighted parens and warns you when they don't match.
;;
(setq paren-sexp-mode nil)
(setq paren-dingaling-mode t)

;; Misc.
;;
(setq bookmark-save-flag 1)

;; Stop Emacs from pestering us whenever a file doesn't end in a newline (a
;; setting which, for some odd reason, Emacs ignores when set via the standard
;; customization engine).  Custom-set-variables loses this, for some reason.
;; Let's try forcing it after everything starts.
(setq require-final-newline 'nil)
(add-hook 'emacs-startup-hook
          (lambda () (setq require-final-newline 'nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Corrections for Flow Control Troubles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the Suggested Fix for C-s C-q flow control problems:
;; (enable-flow-control)

;; This evidently disables ^S/^Q, enables interrupt via ^C
; (set-input-mode (car (current-input-mode))
;            (nth 1 (current-input-mode))
;            0)

;;
;; Rebinds the C-x C-s sequence in case we have flow control
;; trouble....
;;
(global-set-key "\C-xs" 'save-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Font-lock colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only turn it on automatically if we have windows (X11) or are on a
;; supported terminal type
(if (or window-system
        (string= term-lc "xterm")
        (string= term-lc "linux")
        )
    (progn

      ;; Force the colors to something reasonable
      (if window-system
          (if running-xemacs
              (progn
                (setq font-lock-auto-fontify t
                      font-lock-maximum-size 256000
                      font-lock-mode-enable-list t
                      font-lock-mode-disable-list nil
                      font-lock-use-fonts nil
                      font-lock-use-colors t
                      font-lock-use-default-fonts nil
                      font-lock-use-default-colors nil
                      )
                ) ;; end XEmacs customizations
            )
        ;; else:  GNU Emacs
        (set-background-color "black")
        (set-foreground-color "white")
        (set-cursor-color "blue")
        (set-mouse-color "blue")
        (setq x-cursor-fore-pixel "black")
        (setq font-lock-global-modes t)
        )

      (require 'font-lock)

      ;;(setq font-lock-support-mode 'fast-lock-mode)
      (setq font-lock-maximum-decoration t)
      (if running-xemacs
          (progn
            (remove-hook 'font-lock-mode-hook 'turn-on-fast-lock)
            (remove-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
            )
        ;; else: GNU Emacs
        (global-font-lock-mode 1)
        )

      (if running-xemacs
          (progn
            (set-face-foreground 'default "black")
            (set-face-background 'default "white")
            (set-face-foreground 'text-cursor "white")
            (set-face-background 'text-cursor "blue")
            (set-face-foreground 'font-lock-reference-face
                                 "ForestGreen")
            (set-face-foreground 'font-lock-doc-string-face
                                 "DeepSkyBlue3")
            (set-face-foreground 'font-lock-preprocessor-face
                                 "ForestGreen")
            (set-face-background 'zmacs-region "LightBlue")
            (set-face-background 'highlight "Gray")
            ) ;; end XEmacs
        ;; else - GNU Emacs
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
         '(woman-bold-face ((t (:inherit bold :foreground "blue"))))
         '(woman-italic-face
           ((t (:inherit italic :foreground "Purple4" :underline t))))
         )
        (if is-version-twentytwo
            (progn
              (jpw-custom-set-faces-nonsaved
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
                                        :style released-button))))
                 nil
                 "For when I'm in the mood for a more colorful modeline, use
                  this instead of \"grey75\".")
               '(mode-line-highlight
                 ((t (:foreground "green4"
                                  :box (:line-width 2 :color "grey40"
                                        :style released-button))))
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
               '(show-paren-match-face ((((class color))
                                         (:background "yellow"))) t)
               '(show-paren-mismatch-face ((((class color))
                                            (:background "yellow3"))) t)
               )
              )
          );; end if GNU Emacs v22
        ) ;; end if XEmacs

      ;; In GNU Emacs, `modify-face' just does interactive calls to
      ;; `set-face-*'.  `modify-face' doesn't exist in XEmacs.
      (set-face-foreground 'font-lock-builtin-face "MediumOrchid")
      (set-face-foreground 'font-lock-comment-face "red3")
      (set-face-foreground 'font-lock-constant-face "ForestGreen")
      (set-face-foreground 'font-lock-function-name-face "Blue")
      (set-face-foreground 'font-lock-keyword-face "magenta3")
      (set-face-foreground 'font-lock-string-face "DeepSkyBlue3")
      (set-face-foreground 'font-lock-type-face "purple4")
      (set-face-foreground 'font-lock-variable-name-face "orange3")
      (set-face-bold-p     'font-lock-variable-name-face t)

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
  );; end if-fontifiable-term-type


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode defines.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Generic Modes (GNU Emacs only)
;;
(if (not running-xemacs)
    (progn
      (require 'generic)
      (require 'generic-x)
      )
  )
;; N.B.:  The customization vars `generic-define-unix-modes' and
;; `generic-define-mswindows-modes' are automagically set to platform-specific
;; values by 'generic-x by default.  Override if you want to access all of the
;; modes on any platform.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Bash mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'auto-mode-alist '("\\.bashrc.*" . sh-mode) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Makefile mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'auto-mode-alist '("\\.make\\'" . makefile-mode) t)
;; Note: '(("\\Makefile\\'" . makefile-mode) ("\\.mk\\'" . makefile-mode))
;; are already added by default.

;; The old way of appending to a list:
;;(setq auto-mode-alist
;;      (append
;;       '(("\\Makefile\\'" . makefile-mode)
;;         ("\\.make\\'" . makefile-mode)
;;         ("\\.mk\\'" . makefile-mode))
;;       auto-mode-alist
;;       )
;;      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    C/C++ Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar jpw-c-style "jpw"
  "The style that the c-mode autohook should use by default.  Provided as a
variable rather than hardcoded.

{jpw; 06/06}")


;; Create a style for use with c-mode, c++-mode, and objc-mode
(c-add-style "jpw"
             '(;;(indent-tabs-mode . nil)
               (fill-column . 78)
               (c-hanging-braces-alist
                (substatement-open before after)
                )
               (c-basic-offset . 4)
               (c-offsets-alist
                (arglist-cont . c-lineup-arglist)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-close . c-lineup-arglist)
                (inline-open . 0)
                (block-open . +)
                (block-close . 0)
                (brace-list-open . 0)
                (brace-list-intro . +) ; Default aligns enum contents with '{'
                ;;(brace-list-entry . +)
                ;;(brace-entry-open . 0)
                ;;(brace-list-close . 0)
                (class-open . 0) ; Default indents lone '{' for classes
                (class-close . 0) ; Default indents lone '}' for classes
                (defun-open . 0) ; whitesmith does weird things to defun.
                (defun-block-intro . +)
                (defun-close . 0)
                (namespace-open . 0) ; Default indents lone '{' for classes
                (namespace-close . 0) ; Default indents lone '}' for classes
                (topmost-intro . 0)
                (topmost-intro-cont . 0)
                (case-label . *)
                ;;(statement . c-indent-multi-line-block)
                (statement-block-intro . +)
                (statement-case-intro . *)
                (statement-case-open . 0)
                (substatement . +)
                (substatement-open . 0)
                (substatement-label . +)
                (access-label . -)
                (label . 0)
                (inclass . +)
                (innamespace . 1)
                )
               (c-hanging-comment-starter-p . nil)
               (c-hanging-comment-ender-p . nil)
               (c-comment-only-line-offset . 0)
               (c-comment-continuation-stars . "* ")
               ) ;; end list literal
             ;;'t  ;; Activate it now
             )


(c-add-style "jpw-flat-case"
             '("jpw"
               (c-offsets-alist
                (case-label . 0)
                (statement-case-intro . +)
                )
               ) ;; end list literal
             ;;'t  ;; Activate it now
             )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Java Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Create a style for use with java-mode and jde-mode
(c-add-style "jpw-java"
             '("jpw"
               (c-offsets-alist
                (class-open . 0) ; Default indents lone '{' for classes
                (class-close . 0) ; Default indents lone '}' for classes
                (inclass . +) ; In Java, everything's in a class.
                ;; The default for this guy is a function, which doesn't
                ;; understand Java, apparently...
                (defun-block-intro . +)
                (block-close . 0)
                ;; From the definition of "java" style
;;                (arglist-intro . c-lineup-arglist-intro-after-paren)
;;                (arglist-close . c-lineup-arglist)
;;                (access-label . 0)
;;                (inher-cont . c-lineup-java-inher)
;;                (func-decl-cont . c-lineup-java-throws)                )
                )
             ;;'t  ;; Activate it now
               ) ;; end list literal
             )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    SH Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq sh-shell-file "/bin/sh")
(setq sh-shell 'bash)
(if (not running-xemacs)
    (jpw-custom-set-faces-nonsaved
     '(sh-heredoc ((((class color) (background light))
                         (:inherit font-lock-string-face
                                   :background "beige"))))
     )
  )
(eval-after-load "sh-script"
  '(lambda()
     (jpw-sh-mode-font-lock-enhance)
     )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    CVS Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;see the lib-loading section up top.
(autoload 'send-pr "gnats" "Emacs GNATS Interface" t)
(autoload 'cvs-update "pcl-cvs" "Emacs CVS Interface" t)
(autoload 'cvs-mode-log "pcl-cvs" "Emacs CVS Interface" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Transact/SQL Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'sql-mode "trans-sql" "SQL mode with Transact/SQL keywords" t)
(eval-after-load "sql-mode"
  '(lambda()
     (load "sql-indent")
     (setq sql-mode-font-lock-keywords
           sql-mode-oracle-font-lock-keywords)
;;           sql-mode-trans-sql-font-lock-keywords)
     )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    SGML/XML Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'sgml-mode "sgml-mode" "Major mode to edit SGML files." t)
(autoload 'xml-lite-mode "xml-lite" "Major mode to edit XML files." t)
(add-to-list 'auto-mode-alist '("\\.sgml?$" . sgml-mode) t)
(add-to-list 'auto-mode-alist '("\\.menu?$" . sgml-mode) t)
(add-to-list 'auto-mode-alist '("\\.xml$" . jpw-xml-lite-mode) t)
;;(add-to-list 'auto-mode-alist '("\\.xml$" . xml-mode) t)


;; Commented out
;;
(if nil (progn
  (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
  (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

  ;; Things to set once, for all files.
  ;;
  (eval-after-load "psgml"
    '(lambda()
       (setq sgml-set-face t
             sgml-ignore-undefined-elements t
             sgml-warn-about-undefined-elements nil
             sgml-warn-about-undefined-entities nil
             sgml-ignore-undefined-elements t
             sgml-insert-end-tag-on-new-line t
             )
       (setq sgml-markup-faces '((start-tag . font-lock-function-name-face)
                                 (end-tag . font-lock-function-name-face)
                                 (comment . font-lock-comment-face)
                                 (pi . font-lock-variable-name-face)
                                 (sgml . font-lock-keyword-face)
                                 (doctype . font-lock-type-face)
                                 (entity . font-lock-constant-face)
                                 (shortref . font-lock-constant-face)))
       )
    )
));;end (if nil (progn......


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    HTML Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(jpw-custom-set-faces-nonsaved
 '(html-helper-bold-face ((t (:inherit bold))))
 '(html-helper-builtin-face ((t (:inherit font-lock-builtin-face))))
 '(html-helper-italic-face ((t (:inherit italic))))
 '(html-helper-underline-face ((t (:inherit underline))))
 '(html-tag-face ((t (:inherit font-lock-function-name-face))))
 )

(if (not running-xemacs)
    (jpw-custom-set-variables-nonsaved
     ;; XEmacs barfs on this, for some reason.
     '(html-helper-mode-uses-visual-basic t nil (html-helper-mode))
     )
  )

(autoload 'html-helper-mode "html-helper-mode"
  "Major mode for editing HTML" t)
(autoload 'asp-html-helper-mode "html-helper-mode"
  "Major mode for editing ASP" t)
(autoload 'jsp-html-helper-mode "html-helper-mode"
  "Major mode for editing JSP" t)
(autoload 'php-html-helper-mode "html-helper-mode"
  "Major mode for editing PHP" t)
;; Put this one to the front so that html-helper-mode is used instead of
;; any other modes.
(dolist (mode-entry
         '(("\\.html$" . html-helper-mode)
           ("\\.eml$" . html-helper-mode)
           ;;("\\.php$" . php-html-helper-mode)
           ("\\.jsp$" . jsp-html-helper-mode)
           ("\\.asp$" . asp-html-helper-mode))
         )
  (add-to-list 'auto-mode-alist mode-entry)
  )
(setq html-helper-do-write-file-hooks t)
(setq html-helper-build-new-buffer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Perl & CPerl Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'perl-mode "perl-mode")
(autoload 'cperl-mode "cperl-mode")
;;(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
;;(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(if (not running-xemacs)
    (jpw-custom-set-faces-nonsaved
     '(cperl-array-face ((t (:inherit font-lock-variable-name-face
                                      :underline t))))
     '(cperl-hash-face ((t (:inherit (cperl-array-face italic)))))
     '(cperl-nonoverridable-face
       ((((class color) (background light))
         (:inherit font-lock-constant-face))))
     )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Octave Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'octave-mode "octave-mod")
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode) t)
(add-to-list 'auto-mode-alist '("\\.octave$" . octave-mode) t)
(add-to-list 'auto-mode-alist '("\\.oct$" . octave-mode) t)


;;---------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;
