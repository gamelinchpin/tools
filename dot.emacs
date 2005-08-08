;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  .emacs setup file
;;
;;
;;  last modified 06/2005            (jpw)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Fix Alt-key for certain window managers under X
;;;(load "map-alt-to-meta")

;; Path Customization
;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; [jpw; 11/2004]
;; On local installations, put the site-lisp in ~/.emacs.d/ and run 
;; custom-start manually.
;; ;(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
;; ;(load "custom-start")

;; Libraries to load.
;;
;; ;(load "elib-startup")
;; ;(load "cedet-setup")
;; ;(load "phpBB-macros")
;; ;(load "psvn")
;; ;(load "custom-mutt_start")

;; Read abbrevs.
(read-abbrev-file "~/.emacs-abbrevs")

;; cedet and html-helper-mode do not play well together.
(remove-hook 'html-mode-hook 'semantic-default-html-setup)

;; TCL mode stuff
;; ;(load "tcl-enhancements")


(if (not window-system)
  (display-time-mode)) ;; Maybe also use (display-batter)
(setq require-final-newline 'nil)


;; Local Customizations
;; 
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(abbrev-file-name "~/.emacs-abbrevs")
 '(align-c++-modes (quote (c++-mode c-mode java-mode javascript-generic-mode)))
 '(align-open-comment-modes (quote (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode perl-mode cperl-mode python-mode makefile-mode javascript-generic-mode)))
 '(archive-zip-use-pkzip nil)
 '(browse-url-browser-function (quote browse-url-w3) t)
 '(case-fold-search t)
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(display-time-24hr-format t)
 '(display-time-mail-face (quote mode-line))
 '(display-time-mail-file (quote none))
 '(display-time-string-forms (quote ((format-time-string (or display-time-format (if display-time-24hr-format "%H:%M" "%-I:%M%p")) now))))
 '(global-font-lock-mode t nil (font-lock))
 '(html-helper-mode-uses-visual-basic t nil (html-helper-mode))
 '(line-number-mode t)
 '(mouse-wheel-mode t nil (mwheel))
 '(ps-printer-name "~/emacs-out.ps")
 '(require-final-newline nil)
 '(revert-without-query (quote (".*")))
 '(save-abbrevs (quote silently))
 '(type-break-good-rest-interval 60)
 '(type-break-interval 600)
 '(type-break-mode t nil (type-break))
 '(type-break-mode-line-message-mode nil)
 '(type-break-time-warning-intervals (quote (60 30)))
 '(which-function-mode nil nil (which-func))
 '(woman-cache-filename "~/.emacs.d/.wmncache.el")
 '(woman-cache-level 1)
 '(woman-use-own-frame nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(bold-italic ((t (:inherit (bold italic)))))
 '(cperl-array-face ((t (:inherit font-lock-variable-name-face :underline t))))
 '(cperl-hash-face ((t (:inherit (cperl-array-face italic)))))
 '(cperl-nonoverridable-face ((((class color) (background light)) (:inherit font-lock-constant-face))))
 '(highlight ((t (:background "Gray"))))
 '(html-helper-bold-face ((t (:inherit bold))))
 '(html-helper-builtin-face ((t (:inherit font-lock-builtin-face))))
 '(html-helper-italic-face ((t (:inherit italic))))
 '(html-helper-underline-face ((t (:inherit underline))))
 '(html-tag-face ((t (:inherit font-lock-function-name-face))))
 '(italic ((t (:foreground "#544080" :slant italic))))
 '(region ((t (:background "LightBlue"))))
 '(sh-heredoc-face ((((class color) (background light)) (:inherit font-lock-string-face :background "beige"))))
 '(underline ((t (:underline "purple4"))))
 '(woman-bold-face ((t (:inherit bold :foreground "blue"))))
 '(woman-italic-face ((t (:inherit italic :foreground "Purple4" :underline t)))))


(message "Your .emacs has loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End .emacs
;; 
