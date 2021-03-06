;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mutt mode customizations.
;;
;;  Copyright � 1995-2008 John P. Weiss
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
;; Commands for setting up emacs Mutt modes.
;;
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'custom-defuns)
(require 'custom-set-defaults)


;;;;;;;;;;;;;;;;;;;;
;; Muttrc
;;;;;;;;;;;;;;;;;;;;

;; Note:
;; There are two versions of `muttrc-mode'.  One, "muttrc.el", just uses
;; the `generic-x' package to define the new mode.  The other,
;; "muttrc-mode.el", builds a full-featured mode from scratch.
;; Uncomment the appropriate line below to use the one you want.

;;(load "muttrc" t) ; No autoload for the `generic-x' version.
(autoload 'muttrc-mode "muttrc-mode" "Major mode to edit muttrc files" t)


;;;;;;;;;;;;;;;;;;;;
;; Mutt Mail-Composing
;;;;;;;;;;;;;;;;;;;;

(require 'mutt)
(add-to-list 'auto-mode-alist
             '("[Mm]utt-.*" . mutt-mode) t)
(jpw-custom-set-faces-nonsaved
 '(mutt-header-keyword-face
   ((t (:inherit font-lock-keyword-face :weight bold))))
 '(mutt-header-value-face
   ((((class color) (background light)) (:inherit font-lock-constant-face))))
 '(mutt-multiply-quoted-text-face
   ((t (:inherit font-lock-comment-face :slant italic))))
 '(mutt-quoted-text-face ((t (:inherit font-lock-string-face))))
 )


;;;;;;;;;;;;;;;;;;;;
;; Mutt-alias
;;;;;;;;;;;;;;;;;;;;


(defun use-jpw-mutt-mode ()
  (interactive)
  (local-set-key "\C-c\C-a" 'mutt-alias-insert)
  (local-set-key [?\C-c ?\C-/] 'mutt-alias-lookup)
  )
(add-hook 'mutt-mode-hook 'use-jpw-mutt-mode)


(require 'mutt-alias)
(jpw-custom-set-variables-nonsaved
 '(mutt-alias-file-list (quote ("~/Mail/mutt/aliases"
                                "~/Mail/mutt/aliases.prof"
                                "~/Mail/mutt/aliases.work"
                                "~/Mail/mutt/aliases.sb")))
 )
;; Add all of these files to the auto-mode-alist so that they use muttrc-mode
;; by default.
(dolist (maf mutt-alias-file-list)
    (add-to-list 'auto-mode-alist (cons maf (car '(muttrc-mode))) t))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;