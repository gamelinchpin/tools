;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keyboard Bindings File
;;
;;  Copyright © 1995-2013 John P. Weiss
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
;; Binding conventions:
;;
;;    Use the ESC- prefix in place of "M-" modifier when binding
;;    function keys.  This will ensure that the binding is available
;;    to non-win mode.  Emacs will translate all "Meta- + charkey"
;;    combinations automagically.
;;
;;    raw key "Hyper-x" in a vector:  [?\H-x]
;;    prefix + fnkey:                 [?\C-x fnkey]
;;    Meta + prefix, "p" + key, "k":  "\M-pk"
;;    Meta + fnkey:                   [?\e fnkey]
;;    Meta + prefix, "p" + fnkey:     [\M-p fnkey]
;;
;; Note that one only needs the "ESC-is-Meta" paradigm when dealing
;; with "(global-set-key ...)" or "(local-set-key ...)".  When
;; defining function key translation, use the "M-" prefix instead.
;;
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'custom-defuns)


;;;Add any terminal-dependent bindings
;;(setq term (getenv "TERM"))
;;(if term
;;    (setq term (downcase term))
;;  )


;;
;; Emacs Translation Maps (v23.*)
;;
;; There are 3 of them.  Use each one for its intended purpose.
;;
;; `function-key-map':
;;     - Contains "terminal-type-independent" definitions.
;;     - Overridden by the other two.
;;     - Put function key definitions that apply, regardless the terminal
;;       type, in here.
;;
;; `local-function-key-map':
;;     - Contains "terminal-type-specific" definitions.
;;     - Here, "local" refers to the type of terminal device, not the buffer.
;;     - Overrides `function-key-map'.
;;     - Overridden by `key-translation-map'.
;;     - Use this for function key definitions that are specific to a
;;       particular type of terminal.
;;
;; `key-translation-map':
;;     - Contains "terminal-type-independent" definitions.
;;     - Applied last.  Overrides the other two.
;;     - Use it to override keymaps and non-prefix bindings.
;;
(eval-when-compile
  (if (not (and (boundp 'local-function-key-map)
                (keymapp local-function-key-map)))
      (defvaralias local-function-key-map 'function-key-map
        "Alias with the name of a v23 key map variable.")
    )
  (if (not (and (boundp 'input-decode-map)
                (keymapp input-decode-map)))
      (defvaralias input-decode-map 'function-key-map
        "Alias for a missing key map variable.")
    )
  (if (not (and (boundp 'key-translation-map)
                (keymapp key-translation-map)))
      (defvaralias key-translation-map 'function-key-map
        "Alias for a missing key map variable.")
    )
  )


;;X-Windows specific stuff
(if window-system
    (progn
      ;; Tab Key
      (define-key local-function-key-map [\S-iso-lefttab] [\S-tab])
      (define-key local-function-key-map [\C-\S-iso-lefttab] [\C-\S-tab])
      (define-key local-function-key-map [\M-\S-iso-lefttab] [\M-\S-tab])
      (define-key local-function-key-map [\C-\M-\S-iso-lefttab] [\C-\M-\S-tab])

      ;; There are certain mappings in `function-key-map' and
      ;;`local-function-key-map' that are incorrect, as far as I'm
      ;;concerned.  To make sure that "delete" does what every other
      ;;modern editor does, I've created my own function key,
      ;;`[deletekey]' which I map things to.  Then, I bind things to it
      ;;instead of to [delete] (which emacs screws up from UI to UI.)
      (define-key local-function-key-map [backspace] [8])
      (define-key local-function-key-map [delete] [deletekey])
      (define-key local-function-key-map [\M-delete] [\M-deletekey])

      (global-set-key [find] 'find-file)   ;Find File
      (global-unset-key [S-find])
      (global-set-key [equals] "-")
      (global-set-key [f12] 'repeat-complex-command)
      ;;
      ;; End of window-mode setup
      )

  ;;else
  ;;Terminal-specific stuff
  (progn
    ;; Do some additional remappings....
    (if (string= term-lc "xterm")
        ;; FIXME:  These should all be `local-function-key-map' not
        ;;`key-translation-map':
        (progn
          (define-key key-translation-map [f1] [help])
          (define-key key-translation-map [8] [backspace])
          )
      ;;else
      ;; Non-XTerm Remappings
      (progn
        (define-key key-translation-map [f1] [help])
        (define-key key-translation-map [8] [backspace])
        ;;(define-key key-translation-map [127] [deletekey])
        (define-key key-translation-map [f2] [menu])
        (define-key key-translation-map [insertchar] [insert])
        )
      );;end if

    ;; VT and XTerm-specific mappings, for multiple keys
    ;;
    ;; These really should be taken care of by Emacs itself, but
    ;; there are so many stupid variants of the VT bindings,
    ;; including several XTerm ones, that occasionally, a key gets
    ;; forgotten about.
    (if (or (string= term-lc "xterm")
            (string= (substring term-lc 0 2) "vt")
            (string= (substring term-lc 0 5) "linux")
            )
        (progn
          ;; FIXME:  Change to `local-function-key-map', once we have a
          ;; version-blind alias to `local-function-key-map'.  Or change to
          ;;'input-decode-map'
          (define-key input-decode-map "\eOP" [help])
          (define-key input-decode-map "\eOQ" [f2])
          (define-key input-decode-map "\eOR" [f3])
          (define-key input-decode-map "\eOS" [f4])
          (define-key input-decode-map "\e[3~" [deletekey])
          (define-key input-decode-map "\e[2~" [insert])
          (define-key input-decode-map "\e[1~" [home])
          (define-key input-decode-map "\e[H" [home])
          (define-key input-decode-map "\e[F" [end])
          (define-key input-decode-map "\eOH" [home])
          (define-key input-decode-map "\eOF" [end])
          (define-key input-decode-map "\e[p" [pause])
          ;; These next two are for Cygwin's rxvt (may go away in the
          ;; future).
          (define-key input-decode-map "\e[7~" [home])
          (define-key input-decode-map "\e[8~" [end])
          (global-set-key [copy] 'kill-ring-save)
          )
      );;end if

    ;; Console-Only Bindings
    (if (not (string= term-lc "xterm"))
        (progn
          (global-set-key "\e[1;5C" 'forward-word)
          (global-set-key "\e[1;5D" 'backward-word)
          ;; This messes up gpm pasting, so move it someplace else.
          (global-unset-key [mouse-2])
          (global-set-key [S-mouse-2] 'mouse-yank-at-click)
          )
      );;end if

    ;; Key remappings/translations when running inside of some kind of
    ;; terminal.
    (define-key function-key-map [f2] [menu])

    ;; Some emacs/terminals won't remap [find] or [select], so we
    ;; need to force things...
    (global-set-key [find] 'beginning-of-line) ;[home]
    (global-set-key [select] 'end-of-line) ;[end]
    (global-set-key [f4] 'advertised-undo)

    (global-set-key [?\e right] 'forward-word)
    (global-set-key [?\e left] 'backward-word)
    (global-set-key [?\e home] 'beginning-of-buffer)
    (global-set-key [?\e end] 'end-of-buffer)
    (global-set-key [?\e prior] 'scroll-other-window-down)
    (global-set-key [?\e next] 'scroll-other-window)
    (global-set-key [f2] 'tmm-menubar)
    (global-set-key [menu] 'tmm-menubar)

    ;; Bind F11 and F12 if we're not running X
    (global-set-key [f11] 'backward-word)
    (global-set-key [f12] 'forward-word)
    ;; Map \S-f3
    (global-set-key "\e[28~" 'bury-buffer)
    ;;
    ;; End of plain terminal setup
    );;end progn: else-clause
  );;end if window-system


;;
;; Generic Logical defines
;;
;; format: (define-key <map> <keyseqence> <newdefn>)
;;


(define-key key-translation-map [f1] [help])
(define-key key-translation-map [f4] [undo])
(define-key key-translation-map [find] [home])
(define-key key-translation-map [select] [end])
;; Special mapping for C-linefeed
(define-key key-translation-map [\C-linefeed] [?\M-j])
;; Fix the mapping for "M-" + certain function keys
(define-key key-translation-map [?\e deletekey] [\M-deletekey])
(define-key key-translation-map [?\e backspace] [\M-backspace])
(define-key key-translation-map [?\e f6] [\M-f6])
(define-key key-translation-map [?\e f7] [\M-f7])
(define-key key-translation-map [?\e f8] [\M-f8])
(define-key key-translation-map [?\e f9] [\M-f9])
(define-key key-translation-map [?\e f10] [\M-f10])
(define-key key-translation-map [?\e pause] [\M-pause])


;;
;;
;; Global defines - set certain keys to do certain functions...
;;
;;

;; Put the backspace function in the right place
(global-set-key [backspace] 'delete-backward-char)

;Set up Help
(global-set-key [?\C-x help] 'help-for-help)
(global-set-key [?\C-x f1] 'help-for-help)
(global-set-key "\C-x?" 'help-for-help)
(global-set-key [help] 'help-command)
(global-set-key "\C-xh" 'help-command)        ;; overrides mark whole buffer

(global-set-key [help ?b] 'jpw-describe-bindings)
(global-set-key [?\C-x ?h ?b] 'jpw-describe-bindings)


;; Home and End keys
(global-set-key [home] 'beginning-of-line)
(global-set-key [\S-home] 'back-to-indentation)
(global-set-key [end] 'end-of-line)

;; PgUp and PgDn
(global-set-key [prior] 'scroll-down)
(global-set-key [next] 'scroll-up)

;; Ins and Del
(global-set-key [insert] 'yank)       ;Same as S-insert under X
;; Default binding for insert is 'overwrite-mode'
(global-set-key [deletekey] 'delete-char)
(global-set-key [\C-deletekey] 'delete-char)

;; Emacs binds <f2> to the Two-column prefix.  But I keep hitting it, and it
;; Drives Me Crazy.  Fix it.
(global-set-key [\S-f2] '2C-command)
(global-unset-key [f2])
(global-set-key [menu] 'tmm-menubar)

;; Emacs 22 binds <f3> and <f4>.  However, I have preexisting bindings that
;; conflict.  Keep mine and rebind the defaults.
(global-set-key [\C-f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [\C-f4] 'kmacro-end-or-call-macro)
(global-unset-key [f3])
(global-unset-key [f4])

;;Undo
(global-set-key [undo] 'advertised-undo)
(global-set-key [f4] 'advertised-undo)

;;Open
(global-set-key [open] 'find-file)

;; Motion Keys
(global-set-key [\C-right] 'forward-word)
(global-set-key [\C-left] 'backward-word)
(global-set-key [\C-up] 'backward-paragraph)
(global-set-key [\C-down] 'forward-paragraph)

(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

(global-set-key [\M-backspace] 'backward-kill-word)
(global-set-key [\M-deletekey] 'kill-word)

(global-set-key [\C-find] 'isearch-repeat-forward)
(global-set-key [\M-find] 'isearch-repeat-backward)



;------------more bindings-------------------


;; Bind the Repeat Search command
;;; The Default: (global-set-key "\M-s" 'isearch-repeat-forward)
(global-set-key "\C-cs" 'isearch-repeat-forward)
;;; The Default: (global-set-key "\M-R" 'isearch-repeat-backward)
(global-set-key "\C-cr" 'isearch-repeat-backward)

;; NOTE:  Now on "\M-g g" :)
;; binds the goto line command to something
;;;(global-set-key "\C-xg" 'goto-line)

;; binds the copy region command to something
(global-set-key "\C-xw" 'copy-region-as-kill)

;; binds the set-mark command to ctrl-space
(global-set-key [\C-space] 'set-mark-command)

;; Function keys and the named keys
(global-set-key [\S-f3] 'bury-buffer)
(global-set-key [f15] 'bury-buffer) ;; \S-f3 on some systems.
(global-set-key [\M-f3] 'bury-buffer-other-window)
(global-set-key [?\C-x f3] 'kill-next-buffer-and-close-other-windows)
(global-set-key [f6] 'other-window)
(global-set-key [?\C-x f6] 'kill-buffer-other-window)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [\S-f7] 'switch-to-self-other-window)
(global-set-key [f8] 'switch-to-buffer)
(global-set-key [\S-f8] 'switch-to-buffer-using-other-window)
(global-set-key [f20] 'switch-to-buffer-using-other-window)
(global-set-key [\M-f8] 'switch-to-buffer-other-window)
(global-set-key [f9] 'save-buffer)
(global-set-key [?\C-x f9] 'save-all-buffers)
(global-set-key [f10] 'find-file)
(global-set-key [?\C-x f10] 'kill-buffer-then-find-file)
(global-set-key [\S-f10] 'find-file-using-other-window)
(global-set-key [f22] 'find-file-using-other-window)
(global-set-key [\M-f10] 'revert-buffer)
(global-set-key [\M-pause] 'toggle-read-only)

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
(global-set-key [\S-tab] 'reverse-indent-line)
(global-set-key [\C-tab] 'unindent-line)

;; Bind some custom functions from custom-defuns.
(global-set-key "\C-cc" 'server-quit-l)
(global-set-key "\C-x\M-u" 'untabify-buffer)
(global-set-key "\C-x\M-t" 'tabify-buffer)
(global-set-key "\M-\C-m" 'join-next-line)
(global-set-key [\M-\S-return] 'join-next-line)
(global-set-key [?\C-x insert] 'kill-ring-save-entire-buffer)

;; I may alter these bindings in the future.
(global-set-key [?\C-*] 'set8tab) ;; Do Not Change Keysym
(global-set-key [?\C-$] 'set4tab) ;; Do Not Change Keysym

;; CVS-mode bindings
;(global-set-key [\A-v] 'cvs-mode)

;; Other VC-bindings
;;   Need to use the key-vector, since we have a double-key prefix.
(global-set-key [?\C-x ?v ?\C-=] 'ediff-revision)
(global-set-key [?\C-x ?v ?\C-~] 'ediff-revision)

;; Compile-mode bindings
(global-set-key [?\C-c f7] 'compile)
(global-set-key [?\C-c f8] 'recompile)


;; Bindings that autoload functions or keymaps
(require 'iso-transl)
(define-key key-translation-map [\C-menu] 'iso-transl-ctl-x-8-map)


(provide 'custom-keybindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;