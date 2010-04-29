;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  .emacs setup file
;;
;;
;;  last modified 01/2010            (jpw)
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
;; Needed when using the Ubuntu package, "emacs-extras":
;;(set-background-color "white")
;;(set-foreground-color "black")
;;(set-cursor-color "blue")

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


;; By default, custom-start.el sets the default value of indent-tabs-mode to
;; nil.  Change that here.  You may need to put it in the emacs-startup-hook,
;; above.
;; There are a few other things you may want to bind or change.
;;;(setq-default indent-tabs-mode t)
;;;(setq jpw-correct-tabs-on-save nil)
;;;(add-hook 'before-save-hook 'save-buffer-tab-consistent)


;; Work Stuff

;; Wikipedia Mode Customizations
;; ;(load "longlines")
;; ;(load "wikipedia-mode")
;; ;(global-set-key "\M-pw" 'wikipedia-mode)


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

;; Todo:  Move to custom-keybindings.
(global-set-key [?\C-c f7] 'compile)
(global-set-key [?\C-c f8] 'recompile)


;; N.B. - In the current version of Emacs {1/07}, `type-break-mode' interferes
;; with the latin-15 keymap ("\C-x 8" and "\C-menu").
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
 '(archive-zip-use-pkzip nil)
 '(browse-url-browser-function (quote browse-url-w3))
 '(cygwin-mount-cygwin-bin-directory "c:\\cygwin\\bin")
 '(jpw-lj-friend-groups (quote ("husband" "from way back" "Regularly Read")))
 '(jpw-lj-unfill-on-save t)
 '(jpw-lj-user-avatars (quote ("Babuuuuusha" "Dr._Alchemist" "Fiend::Kitty" "Frannie" "Gay::duo""Gay::rainbow_flag" "Gir::cheery""Gir::cheery" "Gir::piggy" "Gir::serious""Gir::waffles" "Ian""Ian::Bashful""Martin" "Me_1995" "MsBitters::doom""MsBitters::looming" "MsBitters::pensive""My_Icon" "fascists" "linux")))
 '(jpw-lj-xlate-entities-on-save t)
 '(visual-basic-mode-indent 2 t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

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

;; Needed when using the Ubuntu package, "emacs-extras"
;; '(highlight ((t (:background "CornflowerBlue"))))



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
