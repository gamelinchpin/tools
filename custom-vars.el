;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Variables and Constants
;;
;;  Copyright © 1995-2010 John P. Weiss
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
;; My custom variables and constants.  Used by all of the other customization
;; files.
;;
;;
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst term-lc (or (getenv "TERM")
                      (and (boundp 'term)
                           (setq term (downcase term))
                           )
                      )
  "The value of the \"TERM\" environment variable lowercased.  Used by the
startup files to shorten the elisp.

You shouldn't change the value of this variable.

{jpw 12/31}")


;; Special flag for WinEmacs
;;
(defconst is-winblows (or (eq window-system 'win32)
                          (eq window-system 'w32)
                          )
  "Set to true if this is a version of emacs for M$ WinBlows.  The
default (and preferred) value is nil.

You shouldn't change the value of this variable.

{jpw 10/01}")


;; Special flag for XEmacs.  Doesn't exist in GNU Emacs, so we'll create it
;; and set it to nil
;;
(eval-when-compile
  (if (not (boundp 'running-xemacs))
      (defconst running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
        "Non-nil when the current emacs is XEmacs."
        )
    )
  )
;; For some reason, eval-and-compile isn't capturing this expression and
;; putting it into the '*.elc' file.  >:(
(if (not (boundp 'running-xemacs))
    (defconst running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
      "Non-nil when the current emacs is XEmacs."
      )
  )


(defconst running-gnu-emacs (not running-xemacs)
  "Non-nil when the current emacs is GNU-Emacs."
  )


;; Special flag for WinEmacs
;;
(defconst is-cygwin (or (string-match "cygwin"
                                      (downcase
                                       (or (getenv "OSTYPE") "")))

                        (and (not is-winblows)
                         (string-match "windows"
                                       (downcase
                                        (or (getenv "OS") ""))))
                        (and (not window-system) is-winblows)
                        )
  "Set to true if this is a version of emacs build for Cygwin.  The
default (and preferred) value is nil.

You shouldn't change the value of this variable.

{jpw 11/04}")


;; Version 20 and above changes some old stuff.  To handle those changes, we
;; define and set these variables.
;;
(defconst is-version-twenty (not (eq emacs-major-version '19))
  "Set to true if this is emacs 20.*.*.  The default value is nil.

You shouldn't change the value of this variable.

{jpw 9/98}")

(defconst is-version-twentytwo (eq emacs-major-version '22)
  "Set to true if this is emacs 22.*.*.  The default value is nil.

You shouldn't change the value of this variable.

{jpw 9/07}")

(defconst is-version-twentythree (eq emacs-major-version '23)
  "Set to true if this is emacs 22.*.*.  The default value is nil.

You shouldn't change the value of this variable.

{jpw 8/10}")


(defconst is-fontifiable-term (or window-system
                                  (string= term-lc "xterm")
                                  (string= term-lc "linux")
                                  )
  "Set to true if emacs is running under a window system or an ANSI terminal.

You shouldn't change the value of this variable.

{jpw 9/10}")



(provide 'custom-vars)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;
