;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Core Emacs Setup File
;;
;;  Copyright © 1995-2011 John P. Weiss
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
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Load Custom Functions
;;
(require 'custom-vars)
(require 'custom-defuns)

(require 'custom-set-defaults)

(if is-winblows
    (load "custom-winemacs" t)
  )

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
        (with-no-warnings
          (setq gnuserv-frame (selected-frame))
          (gnuserv-start)
          )
        )
  ;;else
  (if running-gnu-emacs
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


(if running-xemacs
    (progn
      (require 'mwheel)

      (with-no-warnings
        (setq abbrev-file-name "~/.xemacs/.abbrevs"
              kill-ring-max 100
              minibuffer-max-depth nil
              mwheel-follow-mouse t
              mwheel-scroll-amount (quote (6 . 1))
              quickurl-url-file "~/.xemacs/.quickurls"
              type-break-file-name "~/.xemacs/.type-break"
              woman-cache-filename "~/.xemacs/.wmncache.el"
              zmacs-regions t
              ))
      )
  ;; else:
  ;; Some GNU-Emacs-specific settings.
  ;;
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
(if is-fontifiable-term
    (progn
      ;; Force the colors to something reasonable
      (if window-system
          (if running-xemacs
              (with-no-warnings
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
      ) ;;end progn
  );; end if is-fontifiable-term-type


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode defines.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Generic Modes (GNU Emacs only)
;;
(if running-gnu-emacs
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

(eval-after-load "sgml-mode"
  '(lambda()
     (require 'custom-html_sgml_xml)))
(eval-after-load "xml"
  '(lambda()
     (require 'custom-html_sgml_xml)))
(eval-after-load "xml-lite"
  '(lambda()
     (require 'custom-html_sgml_xml)))


;; Commented out
;;
(if nil (progn
  (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
  (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

  ;; Things to set once, for all files.
  ;;
  (eval-after-load "psgml"
    '(lambda()
       (require 'custom-html_sgml_xml)
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
(let (mode-entry)
  (dolist (mode-entry
           '(("\\.html$" . html-helper-mode)
             ("\\.eml$" . html-helper-mode)
             ;;("\\.php$" . php-html-helper-mode)
             ("\\.jsp$" . jsp-html-helper-mode)
             ("\\.asp$" . asp-html-helper-mode))
           )
    (add-to-list 'auto-mode-alist mode-entry)
    )
  )

(setq html-helper-do-write-file-hooks t)
(setq html-helper-build-new-buffer t)

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

(eval-after-load "html-helper-mode"
  '(lambda()
     (require 'custom-html_sgml_xml)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Perl & CPerl Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The `is-cperl-installed' is defined in custom-vars.
(if is-cperl-installed
    (progn
      ;; Prefer CPerl mode to the std. Perl Mode
      (defalias 'perl-mode-orig (symbol-function 'perl-mode))
      (defalias 'perl-mode 'cperl-mode)
      (autoload 'cperl-mode "cperl-mode")

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
      );; end progn
  ;; else
  (autoload 'perl-mode "perl-mode")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Octave Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'octave-mode "octave-mod")
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode) t)
(add-to-list 'auto-mode-alist '("\\.octave$" . octave-mode) t)
(add-to-list 'auto-mode-alist '("\\.oct$" . octave-mode) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;
