;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    
;; Keyboard Bindings File
;;
;;  Copyright © 2006-2007 John P. Weiss
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
;; Binding conventions:  See the XEmacs manual, the help for
;; `define-key', or the sample "init.el" file.
;;
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'custom-defuns)


;;X-Windows specific stuff
(if (and window-system running-xemacs)
    (progn
     ;; Tab Key
     (define-key function-key-map 'iso-lefttab '(shift tab))
     (define-key function-key-map '(control iso-lefttab) '(shift control tab))
     (define-key function-key-map '(meta iso-lefttab) '(shift meta tab))
     (define-key function-key-map
       '(control meta iso-lefttab) '(shift control meta tab))
     ;; Backspace and delete: these "just work" under XEmacs

     ;; No key translation map for Xwin mode.
     ;;(setq key-translation-map nil)

     (global-set-key [find] 'find-file)   ;Find File
     (global-unset-key [(shift find)])
     (global-set-key [f12] 'repeat-complex-command)

     ;; For some strange reason, XEmacs doesn't bind these by default.
     (global-set-key [button4] 'mwheel-scroll)
     (global-set-key [button5] 'mwheel-scroll)
     ;;
     ;; End of window-mode setup
     )
  )

;;
;; Generic Logical defines
;;
;; format: (define-key <map> <key> <newdefn>)
;;


(define-key function-key-map [f1] [help])
(define-key function-key-map [f4] [undo])
(define-key function-key-map [find] [home])
(define-key function-key-map [select] [end])


;;
;;
;; Global defines - set certain keys to do certain functions...
;;
;;

;; Several keys that are messed up in GNU Emacs are ok in XEmacs.  If
;; a binding is missing from this file, that's why.

;Set up Help
(global-set-key "\C-x?" 'help-for-help)
(global-set-key "\C-xh" 'help-command)        ;; overrides mark whole buffer

;; Home and End keys
(global-set-key '(shift home) 'back-to-indentation)

;; Ins and Del
(global-set-key [insert] 'yank)       ;Same as S-insert under X
;Default binding for insert is 'overwrite-mode'

;;Undo
(global-set-key [undo] 'advertised-undo)

;; Motion Keys:  The default XEmacs bindings for these are different,
;; but do much the same. LEave commented out, for now.
;;;(global-set-key '(control up) 'backward-paragraph)
;;;(global-set-key '(control down) 'forward-paragraph)
 
(global-set-key '(control find) 'isearch-repeat-forward)
(global-set-key '(meta find) 'isearch-repeat-backward)


;------------more bindings-------------------

;; Bind the Repeat Search command
;;; The Default: (global-set-key "\M-s" 'isearch-repeat-forward)
(global-set-key "\C-cs" 'isearch-repeat-forward)
;;; The Default: (global-set-key "\M-R" 'isearch-repeat-backward)
(global-set-key "\C-cr" 'isearch-repeat-backward)

;; binds the goto line command to something 
(global-set-key "\C-xg" 'goto-line)

;; binds the copy region command to something
(global-set-key "\C-xw" 'copy-region-as-kill)

;; Function keys and the named keys
(global-set-key '(shift f3) 'bury-buffer)
(global-set-key 'f15 'bury-buffer) ;; \S-f3 on some systems.
(global-set-key '(meta f3) 'bury-buffer-other-window)
(global-set-key [?\C-x f3] 'kill-next-buffer-and-close-other-windows)
(global-set-key 'f6 'other-window)
(global-set-key [?\C-x f6] 'kill-buffer-other-window)
(global-set-key 'f7 'split-window-vertically)
(global-set-key '(shift f7) 'switch-to-self-other-window)
(global-set-key 'f8 'switch-to-buffer)
(global-set-key '(shift f8) 'switch-to-buffer-using-other-window)
(global-set-key 'f20 'switch-to-buffer-using-other-window)
(global-set-key '(meta f8) 'switch-to-buffer-other-window)
(global-set-key 'f9 'save-buffer)
(global-set-key [?\C-x f9] 'save-all-buffers)
(global-set-key 'f10 'find-file)
(global-set-key [?\C-x f10] 'kill-buffer-then-find-file)
(global-set-key '(shift f10) 'find-file-using-other-window)
(global-set-key 'f22 'find-file-using-other-window)
(global-set-key '(meta f10) 'revert-buffer)
(global-set-key '(meta pause) 'toggle-read-only)

; Misc. custom bindings.
(global-set-key "\C-xs" 'save-buffer)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-xK" 'kill-buffer)
(global-set-key "\C-c\C-l" 'font-lock-fontify-buffer)
(global-set-key "\C-x\M-f" 'auto-fill-mode)

;; Binding the Tab-key is tricky; WinBlows steals \M-Tab and
;; \M-\S-Tab.  Furthermore, \S-Tab and Tab are treated as the same key
;; on some systems unless \S-Tab and its combinations are explicitly
;; defined.
(global-set-key '(shift tab) 'reverse-indent-line)
(global-set-key '(control tab) 'unindent-line)

;; Bind some custom functions from custom-defuns.
(global-set-key "\C-cc" 'server-quit-l)
(global-set-key "\C-x\M-u" 'untabify-buffer)
(global-set-key "\M-\C-m" 'join-next-line)
(global-set-key [?\C-x insert] 'kill-ring-save-entire-buffer)

;; I may alter these bindings in the future.
(global-set-key '(control *) 'set8tab) ;; Do Not Change Keysym
(global-set-key '(control $) 'set4tab) ;; Do Not Change Keysym


;; Bindings that autoload functions or keymaps

;; Note that there's no such function in iso-insert.el, so the autoload
;; fails.
;;;(autoload '8859-1-map "iso-insert.el" "" t 'keymap)
;; We'll kludge it, instead.
(defun get-8859-1-map ()
  (interactive)
  (if (not (boundp '8859-1-map))
      (progn
        (require 'iso-insert)
        (define-key global-map '(control menu) 8859-1-map)
        (message "Rebound: C-menu.  Retype it now to use.")
        )
    )
  )
;; XEmacs: Put this into the global map, not the key-translation-map
(define-key global-map '(control menu) 'get-8859-1-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;