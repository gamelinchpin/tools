;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  .xemacs setup file
;;
;;
;;  last modified 10/2007            (jpw)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Fix Alt-key for certain window managers under X
;;;(load "map-alt-to-meta")

;; Path Customization
;;
(add-to-list 'load-path (expand-file-name "~/.xemacs/site-lisp"))

;; Do custom startup tasks.
(load "custom-start")

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
(read-abbrev-file "~/.xemacs/.abbrevs")

;; cedet and html-helper-mode do not play well together.
;; ;(remove-hook 'html-mode-hook 'semantic-default-html-setup)


(if (not window-system)
  (display-time-mode)) ;; Maybe also use (display-batter)
(setq require-final-newline 'nil)

;; Temp [TpX40 only]:  Map the [\C-menu] key to something.
(define-key function-key-map [print] [\C-menu])

;; XEmacs-Specific
(if running-xemacs
    (setq initial-frame-plist '(width 80)))


;; Local Customizations
;; 




(message "Your .emacs has loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Local Variables:
;; mode: emacs-lisp
;; End:
;; 
