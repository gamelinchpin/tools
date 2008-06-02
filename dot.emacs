;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  .emacs setup file
;;
;;
;;  last modified 06/2008            (jpw)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Emacs Fixes for Linux
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
(load "discord")
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

;; Work Stuff
;;...


;; If we have the official version of Tramp installed (and not the one bundled
;; with GNU Emacs), uncomment this.
;; ;(load "password.el")


(if (not window-system)
  (display-time-mode)) ;; Maybe also use (display-batter)
(setq require-final-newline 'nil)


;; These are customization variables, but we set them here instead of below,
;; since their values are programmatic, not constant.
;(setq 
; vc-path (list cygwin-binpath (concat cygwin-basepath "/lib/cvs/contrib"))
; exec-path (append (list cygwin-binpath) exec-path)
; )

;; Temp [TpX40 only]:  Map the [\C-menu] key to something.
;; ;(define-key function-key-map [print] [\C-menu])
;; ;(global-set-key [?\C-c f7] 'compile)
;; ;(global-set-key [?\C-c f8] 'recompile)


(put 'scroll-left 'disabled nil)


;; Some Local Face Customizations that seem to get erased whenever I save
;; settings.
;;
(jpw-cust-colorful-modeline)


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
 '(cperl-font-lock t)
 '(cperl-highlight-variables-indiscriminately t)
 '(current-language-environment "Latin-1")
 '(cygwin-mount-cygwin-bin-directory "c:\\cygwin\\bin")
 '(default-input-method "latin-1-prefix")
 '(display-time-24hr-format t)
 '(display-time-mail-face (quote mode-line))
 '(display-time-mail-file (quote none))
 '(display-time-string-forms (quote ((format-time-string (or display-time-format (if display-time-24hr-format "%H:%M" "%-I:%M%p")) now))))
 '(global-font-lock-mode t nil (font-lock))
 '(grep-command "grep --binary-files=without-match --exclude=\\*.svn\\* -n -r -P ")
 '(history-delete-duplicates t)
 '(jpw-lj-friend-groups (quote ("husband" "from way back" "Regularly Read")))
 '(jpw-lj-unfill-on-save t)
 '(jpw-lj-user-avatars (quote ("Dr._Alchemist" "Fiend::Kitty" "Gir::cheery" "Gir::cheery" "Gir::piggy" "Gir::serious" "Gir::waffles" "Me_1995" "MsBitters::doom" "MsBitters::looming" "MsBitters::pensive" "My_Icon" "fascists")))
 '(jpw-lj-xlate-entities-on-save t)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(org-blank-before-new-entry (quote ((heading . t) (plain-list-item))))
 '(org-cycle-include-plain-lists t)
 '(org-ellipsis "···
")
 '(org-level-color-stars-only nil)
 '(org-startup-folded (quote content))
 '(org-startup-truncated nil)
 '(org-todo-keywords (quote ("TODO" "STARTED" "DONE")))
 '(outline-regexp "[*§¶­\^L]+")
 '(partial-completion-mode t)
 '(password-cache-expiry 86400)
 '(ps-printer-name "~/emacs-out.ps")
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(require-final-newline nil)
 '(safe-local-variable-values (quote ((buffer-file-coding-system-explicit . mule-utf-8-dos) (tab-stop-list 8 16 24 32 40 48 56 64 72))))
 '(scroll-bar-mode (quote right))
 '(show-paren-delay 0.5)
 '(show-paren-style (quote mixed))
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|.svn\\)\\'")
 '(speedbar-update-speed 300 t)
 '(tramp-auto-save-directory "/tmp/")
 '(tramp-default-method "ftp")
 '(transient-mark-mode t)
 '(vc-handled-backends (quote (SVN RCS CVS SCCS)))
 '(visual-basic-mode-indent 2 t)
 '(which-function-mode nil nil (which-func))
 '(woman-use-own-frame nil))

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
 '(sh-heredoc ((((class color) (background light)) (:inherit font-lock-string-face :background "beige"))))
 '(show-paren-match-face ((((class color)) (:background "yellow"))) t)
 '(show-paren-mismatch-face ((((class color)) (:background "yellow3"))) t))

;; Old type-break-mode settings
;; '(type-break-mode t nil (type-break))

;; To use PuTTY for Tramp-mode:
;; '(tramp-default-method "plink")

;; My earlier attempt at outline setup.
;; '(outline-plain-bullets-string "·§-+.:,;»×°¸ø¬÷¹²³ªº¶¤")
;; '(outline-regexp "[*§¶]+\\|[ 	]+[-+¤°»]+")
;; '(outline-regexp "[*§¶]+\\|[	  ]+[*§¶­]+")

;; For Speedbar w/o icons, use this as the button face:
;;  '(speedbar-button-face 
;;    ((((class color) (background light)) 
;;      (:background "yellow4" :foreground "purple4" 
;;                   :box (:line-width 1 
;;                                     :color "yellow3" 
;;                                     :style released-button)))))


;; [jpw] Force the recentf menu to update.  Dunno if it'll work.
(setq recentf-update-menu-p t)
;; [jpw] May Need to do this:
;;(recentf-update-menu-hook)
(message "Your .emacs has loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Local Variables:
;; mode: emacs-lisp
;; End:
;; 
