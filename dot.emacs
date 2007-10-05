;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  .emacs setup file
;;
;;
;;  last modified 10/2007            (jpw)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Emacs 21.2 Fixes for Linux
(setq require-final-newline 'nil) ;; custom-set-variables loses this.

(message "In ~/.emacs")

;; Initial frame state.  Set before any other package is loaded.
(setq initial-frame-alist '((width . 80) 
                            (vertical-scroll-bars . 'right)
                            (menu-bar-lines . 1) 
                            (tool-bar-lines . 0)))

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
;; ;(load "discord")
;; ;(load "elib-startup")
;; ;(load "cedet-setup")
;; ;(load "phpBB-helper")
;; ;(load "jpw-lj-helper")
;; ;(load "vc-svn-wrapper")  ;; Calls: (load "vc-svn")
;; ;(load "psvn")
;; ;(load "custom-mutt_start")

;; Read abbrevs.
(read-abbrev-file "~/.emacs.d/.abbrevs")

;; cedet and html-helper-mode do not play well together.
(remove-hook 'html-mode-hook 'semantic-default-html-setup)
;; Custom-set-variables loses this, for some reason.  Let's try forcing it
;; after everything starts.
(add-hook 'emacs-startup-hook 
          (lambda () (setq require-final-newline 'nil)))

;; TCL mode stuff
;; ;(load "tcl-enhancements")

;; Work Stuff
;; ;(load "ee-mode")
;; ;(load "p4")
;; ;(load "p4extra")


(if (not window-system)
  (display-time-mode)) ;; Maybe also use (display-batter)
(setq require-final-newline 'nil)


(put 'scroll-left 'disabled nil)

;; Local Customizations
;; 
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(align-c++-modes (quote (c++-mode c-mode java-mode javascript-generic-mode)))
 '(align-open-comment-modes (quote (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode perl-mode cperl-mode python-mode makefile-mode javascript-generic-mode)))
 '(archive-zip-use-pkzip nil)
 '(browse-url-browser-function (quote browse-url-w3))
 '(case-fold-search t)
 '(compile-command "LC_CTYPE=ascii make -k ")
 '(confirm-kill-emacs (quote y-or-n-p))
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(display-time-24hr-format t)
 '(display-time-mail-face (quote mode-line))
 '(display-time-mail-file (quote none))
 '(display-time-string-forms (quote ((format-time-string (or display-time-format (if display-time-24hr-format "%H:%M" "%-I:%M%p")) now))))
 '(grep-command "grep --binary-files=without-match --exclude=\\*.svn\\* -n -r -P ")
 '(history-delete-duplicates t)
 '(jpw-lj-friend-groups (quote ("husband" "from way back" "Regularly Read")))
 '(jpw-lj-unfill-on-save t)
 '(jpw-lj-user-avatars (quote ("Dr._Alchemist" "Fiend::Kitty" "Gir::cheery" "Gir::cheery" "Gir::piggy" "Gir::serious" "Gir::waffles" "Me_1995" "MsBitters::doom" "MsBitters::looming" "MsBitters::pensive" "My_Icon" "fascists")))
 '(jpw-lj-xlate-entities-on-save t)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(outline-plain-bullets-string "·§-+.:,;¶¤»×°")
 '(outline-regexp "[*§¶]+\\|[ 	]+[-+¤°»]+" t)
 '(require-final-newline nil)
 '(scroll-bar-mode (quote right))
 '(show-paren-delay 0.5)
 '(show-paren-style (quote mixed))
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|.svn\\)\\'")
 '(speedbar-update-speed 300 t)
 '(vc-handled-backends (quote (SVN RCS CVS SCCS)))
 '(visual-basic-mode-indent 2 t)
 '(which-function-mode nil nil (which-func)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "SpringGreen2" :weight bold))))
 '(diff-changed ((nil (:foreground "#8000FF" :weight bold))))
 '(diff-removed ((t (:inherit diff-changed :foreground "VioletRed4"))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "plum3" :foreground "black" :box (:line-width -1 :style released-button)))) nil "For when I'm in the mood for a more colorful modeline, use this.")
 '(mode-line-highlight ((t (:foreground "green4" :box (:line-width 2 :color "grey40" :style released-button)))) nil "Just using a box with a darker gray is unsatisfying.  Let's change the text color to something that will stand out (but not water our eyes).  Change the modeline color, and we may need to change this.")
 '(mode-line-inactive ((t (:inherit mode-line :background "LemonChiffon3" :foreground "grey20" :box (:line-width -1 :style released-button) :weight light))) nil "To accompany my more colorful modeline, I'll pick an off-white color fo the inactive modeline.")
 '(show-paren-match-face ((((class color)) (:background "yellow"))) t)
 '(show-paren-mismatch-face ((((class color)) (:background "yellow3"))) t))

;; Old type-break-mode settings
;; '(type-break-mode t nil (type-break))

;; For Speedbar w/o icons, use this as the button face:
;; '(speedbar-button-face ((((class color) (background light)) (:background "yellow4" :foreground "purple4" :box (:line-width 1 :color "yellow3" :style released-button)))))


(message "Your .emacs has loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Local Variables:
;; mode: emacs-lisp
;; End:
;; 
