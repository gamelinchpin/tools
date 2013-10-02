;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode Hooks
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
;; Makes use of my mode-specific custom defuns and autohooks.
;;
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'custom-mode-styles)
(eval-when-compile
  (require 'custom-html_sgml_xml)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Establish system-wide mode hooks


;; Library-Load Hooks
(add-hook 'org-load-hook 'jpw-org-init-hook)

;; Misc hooks
(add-hook 'mail-mode-hook '(lambda () (abbrev-mode 1)))
(add-hook 'mail-citation-hook 'sc-cite-original)
(add-hook 'mail-yank-hooks 'sc-cite-original)
(add-hook 'mutt-mode-hook 'use-jpw-style-mutt)
(add-hook 'post-mode-hook 'use-jpw-style-mutt)
(add-hook 'text-mode-hook 'use-jpw-style-text)
(add-hook 'org-mode-hook 'use-jpw-style-org)
(add-hook 'wikipedia-mode-hook 'use-jpw-style-wikipedia-mode)
(add-hook 'outline-minor-mode-hook '(lambda ()
                                      (if (eq major-mode 'wikipedia-mode)
                                          (use-jpw-style-wikipedia-mode))))
(add-hook 'speedbar-after-create-hook 'jpw-speedbar-settings)

;; Makefiles
(add-hook 'makefile-mode-hook 'use-jpw-style-common)


;; C/C++/Obj-C/Java
(add-hook 'c-mode-common-hook 'use-jpw-style-c-common)
(add-hook 'c-mode-hook 'use-jpw-style-c)
(add-hook 'c++-mode-hook 'use-jpw-style-cxx)
(add-hook 'jde-mode-hook 'use-jpw-style-java)
(add-hook 'java-mode-hook 'use-jpw-style-java)


;; Lisp [Emacs and plain]
(add-hook 'emacs-lisp-mode-hook 'use-jpw-style-elisp)
(add-hook 'lisp-mode-hook 'use-jpw-style-common)

;; Scripting languages
(add-hook 'awk-mode-hook 'use-jpw-style-common)
(add-hook 'perl-mode-hook 'use-jpw-style-perl)
(add-hook 'cperl-mode-hook 'use-jpw-style-cperl)
(add-hook 'octave-mode-hook 'use-jpw-style-octave)
(add-hook 'ess-mode-hook 'use-jpw-style-ess)

;; There is no longer an separate "shell-script-mode-hook"; the `sh-mode-hook'
;; serves both the programming mode and the Shell-in-a-Box mode.
(add-hook 'sh-mode-hook 'use-jpw-style-common)
(add-hook 'sh-mode-hook 'use-jpw-sh-dabbrev-skip)


;; WWW and Friends
(add-hook 'sgml-mode-hook 'use-jpw-style-sgml)
(add-hook 'xml-mode-hook 'use-jpw-style-xml-lite)
(add-hook 'html-mode-hook 'use-jpw-style-common)
(add-hook 'html-helper-mode-hook 'use-jpw-style-html-helper)

;; LaTeX and friends.
(add-hook 'latex-mode-hook 'use-jpw-style-common)
(add-hook 'bibtex-mode-hook 'use-jpw-style-common)
(add-hook 'tex-mode-hook 'use-jpw-style-tex)

;; FORTRAN [F77 & F90]
(add-hook 'fortran-mode-hook 'use-jpw-style-common)
(add-hook 'f90-mode-hook 'use-jpw-style-common)

;; Pascal/Modula-2
(add-hook 'pascal-mode-hook 'use-jpw-style-common)
(add-hook 'modula-2-mode-hook 'use-jpw-style-common)

;; Man pages/texinfo
(add-hook 'texinfo-mode-hook 'use-jpw-style-common)
(add-hook 'nroff-mode-hook 'use-jpw-style-common)

;; SQL
(add-hook 'sql-mode-hook 'use-jpw-style-sql)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;