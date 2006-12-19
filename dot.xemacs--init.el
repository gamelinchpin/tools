;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  .xemacs setup file
;;
;;
;;  last modified 12/2006            (jpw)
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
;; ;(load "elib-startup")
;; ;(load "cedet-setup")
;; ;(load "phpBB-helper")
;; ;(load "jpw-lj-helper")
;; ;(load "vc-svn-wrapper")  ;; Calls: (load "vc-svn")
;; ;(load "psvn")
;; ;(load "custom-mutt_start")

;; Read abbrevs.
(read-abbrev-file "~/.emacs-abbrevs")

;; cedet and html-helper-mode do not play well together.
;; ;(remove-hook 'html-mode-hook 'semantic-default-html-setup)

;; TCL mode stuff
(load "tcl-enhancements")

;; Work Stuff
(load "ee-mode")
;;(fset 'jpw-p4job-cleanup
;;   [?\C-* ?\M-< ?\C-\M-% ?^ ?\C-8 ?  ?\C-m ?\C-q ?\C-i ?\C-m ?!])
(load "p4")
;; ;(load "p4extra")


(if (not window-system)
  (display-time-mode)) ;; Maybe also use (display-batter)
(setq require-final-newline 'nil)


;; Temp [TpX40 only]:  Map the [\C-menu] key to something.
(define-key function-key-map [print] [\C-menu])


;; Local Customizations
;; 




(message "Your .emacs has loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End .emacs
;; 
