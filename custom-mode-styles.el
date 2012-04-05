;; -*-byte-compile-dynamic: t;-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Functions
;;
;;  Copyright © 1995-2012 John P. Weiss
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
;; My custom functions used to set up major modes the way I like them.  Used
;; as autohooks.
;;
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'custom-defuns)
(eval-when-compile
  (require 'custom-html_sgml_xml)
  (require 'cperl-mode nil t)
  (require 'sgml-mode)
  ;; N.B. - This isn't defining `xml-lite-mode', so we're getting a
  ;;        compilation warning.
  (require 'xml-lite)
  (require 'sql))


;;----------------------------------------------------------------------
;;
;; Autohook-specific functions.
;;


(defun use-jpw-perl-dabbrev-skip ()
  (make-local-variable 'dabbrev-abbrev-skip-leading-regexp)
  (setq dabbrev-abbrev-skip-leading-regexp "[$@%&]")
  )


(defun use-jpw-sh-dabbrev-skip ()
  (make-local-variable 'dabbrev-abbrev-skip-leading-regexp)
  (setq dabbrev-abbrev-skip-leading-regexp "[$]")
  )


(defun bind-jpw-doc-comment ()
  (interactive)
  ;; Fix 'M-q' so that it works with regions.
  (local-set-key "\M-q" 'jpw-c-fill-paragraph)
  ;; Universal doc-comment keybindings
  (local-set-key "\M-oe" (lambda() (interactive)
                                   (jpw-insert-doc-tag "em")))
  (local-set-key "\M-ot" (lambda() (interactive)
                                 (jpw-insert-doc-tag "tt")))
  (local-set-key "\M-ob" (lambda() (interactive)
                                 (jpw-insert-doc-tag "b")))
  (local-set-key "\M-oi" (lambda() (interactive)
                                 (jpw-insert-doc-tag "i")))
  (local-set-key "\M-ou" (lambda() (interactive)
                                 (jpw-insert-doc-tag "u")))
  (local-set-key "\M-o_" (lambda() (interactive)
                                 (jpw-insert-doc-tag "u")))
  (local-set-key "\M-p\C-j" (lambda() (interactive)
                              (do-comment-line-break)
                              (jpw-insert-doc-unitag "br")))
  (local-set-key "\M-p\C-m" (lambda() (interactive)
                              (jpw-insert-doc-tagblock "p")))
  (local-set-key "\M-p*" (lambda() (interactive)
                           (jpw-insert-doc-tag "li")))
  (local-set-key "\M-p." (lambda() (interactive)
                           (jpw-insert-doc-tag "li")))
  (local-set-key "\M-pl" (lambda() (interactive)
                           (if (null current-prefix-arg)
                               (jpw-insert-doc-nested-tagblock "ul" "li")
                             (setq current-prefix-arg nil)
                             (jpw-insert-doc-nested-tagblock "ol" "li"))))
  (local-set-key "\M-p1" (lambda() (interactive)
                           (jpw-insert-doc-nested-tagblock "ol" "li")))
  (local-set-key "\M-p0" (lambda() (interactive)
                           (jpw-insert-doc-nested-tagblock "ol" "li")))
  (local-set-key "\M-p+" (lambda() (interactive)
                           (jpw-insert-doc-nested-tagblock "ul" "li")))
  (local-set-key "\M-p=" (lambda() (interactive)
                           (jpw-insert-doc-nested-tagblock "ul" "li")))
  (local-set-key "\M-p-" (lambda() (interactive)
                           (jpw-insert-doc-unitag "hr")))
  )

(defun bind-jpw-c-mode-doxy ()
  (interactive)
  ;; Create keymap
  ;; Define doc-comment keybindings.
  (bind-jpw-doc-comment)
  (local-set-key "\M-po" (lambda() (interactive)
                           (jpw-insert-doxygen-cmdblock "code")))
  )


(defun bind-jpw-javadoc ()
  (interactive)
  ;; Define doc-comment keybindings.
  (bind-jpw-doc-comment)
  (local-set-key "\M-oc" (lambda() (interactive)
                           (jpw-insert-doc-tag "code")))
  (local-set-key "\M-pc" (lambda() (interactive)
                           (jpw-insert-doc-tagblock "pre")))
  (local-set-key "\M-pr" 'jpw-insert-javadoc-link)
  (local-set-key "\M-pl" 'jpw-insert-javadoc-link)
  (local-set-key "\M-pm" 'jpw-insert-javadoc-member-link)
  (local-set-key "\M-p\'" 'jpw-cleanup-javadoc-block)
  (local-set-key "\M-p\M-l" (lambda() (interactive)
                              (if (null current-prefix-arg)
                                  (jpw-insert-doc-nested-tagblock "ul" "li")
                                (setq current-prefix-arg nil)
                                (jpw-insert-doc-nested-tagblock "ol" "li"))
                              ))
  )


(defun use-jpw-style-text ()
  (interactive)
  (setq fill-column 70)
  (font-lock-mode -1)
  ;; Mozex should not use auto-fill mode
  (or
   (string-match "^mozex\." (buffer-name))
   (turn-on-auto-fill)
   )
  )


(defun use-jpw-style-mutt ()
  (interactive)
  (turn-on-auto-fill)
  (font-lock-mode 't)
  (abbrev-mode 1)
  (local-set-key "\C-ca" 'mutt-alias-insert)
  (local-set-key "\C-ci" 'mutt-alias-insert)
  (local-set-key "\C-cl" 'mutt-alias-lookup)
  )


(defvar jpw-outline-toggle-all-visible 'nil)
(defun jpw-outline-toggle-show-hide-all()
  (interactive)
  (if jpw-outline-toggle-all-visible
      (progn
        (setq jpw-outline-toggle-all-visible 'nil)
        (hide-sublevels 1)
        )
    ;; else:
    (setq jpw-outline-toggle-all-visible 't)
    (show-all)
    )
  )


(defun use-jpw-style-org ()
  (interactive)
  ;;(setq fill-column 78)
  (turn-on-auto-fill)
  ;; Initialize, using the state of the customization variable,
  ;;"org-startup-folded"
  (setq jpw-outline-toggle-all-visible
        (or (not org-startup-folded)
            (eq org-startup-folded 'nofold)
            (eq org-startup-folded 'showall)
            )
        )
  (local-set-key [\M-\S-return] 'org-meta-return)
  (local-set-key [\C-return] 'org-meta-return)
  (local-set-key [\M-return] 'join-next-line)
  (local-set-key [\M-down] 'outline-next-visible-heading)
  (local-set-key [\M-up] 'outline-previous-visible-heading)
  (local-set-key [\M-right] 'outline-forward-same-level)
  (local-set-key [\M-left] 'outline-backward-same-level)
  (local-set-key [?\C-c \C-tab] 'jpw-outline-toggle-show-hide-all)
  (local-set-key [?\C-c tab] 'jpw-outline-toggle-show-hide-all)
  (local-set-key [\C-\S-down] 'org-shiftmetadown)
  (local-set-key [\C-\S-up] 'org-shiftmetaup)
  (local-set-key [\C-\S-right] 'org-shiftmetaright)
  (local-set-key [\C-\S-left] 'org-shiftmetaleft)
  (local-set-key [\C-tab] 'unindent-line)
  (local-set-key [\S-tab] 'reverse-indent-line)
  (local-set-key [\S-iso-lefttab] 'reverse-indent-line)
  (font-lock-mode t)
  )


(defun use-jpw-style-html-helper ()
  (interactive)
  (setq fill-column 78)
  (turn-on-auto-fill)

  ;; Fix keys that break my bindings.
  (local-unset-key [f4])
  (local-set-key [\M-\S-return] 'tempo-template-html-paragraph)
  (local-set-key [\C-return] 'tempo-template-html-line-break)
  (local-set-key [\M-return] 'join-next-line)

  ;; Special setup for HTML-Helper Mode  on Thunderbird mail buffers.
  (local-set-key "\M-p\M-t"
                 '(lambda ()
                    (interactive)
                    (revert-to-utf8)
                    (save-excursion
                      (goto-char (point-min))
                      (while (search-forward "<br>" nil t)
                        (replace-match "<br/>" nil t)))
                    )
                 )

  ;; Fonts
  (local-set-key "\M-oi" 'tempo-template-html-italic)
  (local-set-key "\M-ob" 'tempo-template-html-bold)
  (local-set-key "\M-ou" 'tempo-template-html-underline)
  (local-set-key "\M-oe" 'tempo-template-html-emphasized)
  (local-set-key "\M-os" 'tempo-template-html-strong)
  (local-set-key "\M-ot" 'tempo-template-html-fixed)
  (local-set-key "\M-of" 'tempo-template-html-fixed)
  (local-set-key "\M-oo" 'tempo-template-html-code)
  (local-set-key "\M-oc" 'tempo-template-html-code)
  (local-set-key "\M-od" 'jpw-html-del)
  (local-set-key "\M-o^" 'jpw-html-super)
  (local-set-key "\M-o_" 'jpw-html-sub)
  (local-set-key "\M-o\C-s" 'jpw-html-size-small)
  (local-set-key "\M-o\C-b" 'jpw-html-size-big)
  (local-set-key "\M-oz" 'jpw-html-size-relative)

  ;; Paragraph styles
  (local-set-key "\M-p\C-i" 'tempo-template-html-image)
  (local-set-key "\M-pc" 'tempo-template-html-preformatted)
  (local-set-key "\M-pa" 'jpw-html-href-anchor)
  (local-set-key "\M-p\C-u" 'jpw-html-href-anchor)
  (local-set-key "\M-pl" 'jpw-html-insert-list)
  (local-set-key "\M-p*" 'tempo-template-html-list-item)
  (local-set-key "\M-p." 'tempo-template-html-list-item)
  (local-set-key "\C-cp" 'tempo-template-html-paragraph)
  (local-set-key "\M-p\C-m" 'tempo-template-html-paragraph)
  (local-set-key "\M-p\C-j" 'tempo-template-html-line-break)
  (local-set-key "\M-p-" 'tempo-template-html-horizontal-line)
  (local-set-key "\M-pq" 'tempo-template-html-blockquote)
  (local-set-key "\M-p1" 'tempo-template-html-header-1)
  (local-set-key "\M-ph1" 'tempo-template-html-header-1)
  (local-set-key "\M-p2" 'tempo-template-html-header-2)
  (local-set-key "\M-ph2" 'tempo-template-html-header-2)
  (local-set-key "\M-p3" 'tempo-template-html-header-3)
  (local-set-key "\M-ph3" 'tempo-template-html-header-3)
  (local-set-key "\M-p4" 'tempo-template-html-header-4)
  (local-set-key "\M-ph4" 'tempo-template-html-header-4)
  (local-set-key "\M-p5" 'tempo-template-html-header-5)
  (local-set-key "\M-ph5" 'tempo-template-html-header-5)
  (local-set-key "\M-p6" 'tempo-template-html-header-6)
  (local-set-key "\M-ph6" 'tempo-template-html-header-6)

  ;; Entity expansion
  (local-set-key "\C-c'" 'jpw-html-entity-abbrev-expand)
  (local-set-key [?\C-c ?\C-'] 'jpw-html-entity-abbrev-expand)

  (font-lock-mode t)
  (jpw-html-fix-tempo-templates)
  )


(define-skeleton jpw-wikipedia-insert-header1
  "Level 1 Heading" nil "== " _ "==")
(define-skeleton jpw-wikipedia-insert-header2
  "Level 2 Heading" nil "=== " _ "===")
(define-skeleton jpw-wikipedia-insert-header3
  "Level 3 Heading" nil "==== " _ "====")
(define-skeleton jpw-wikipedia-insert-header4
  "Level 4 Heading" nil "===== " _ "=====")
(define-skeleton jpw-wikipedia-insert-html
  "Protected HTML markup" nil "<html>" _ "</html>")
(define-skeleton jpw-wikipedia-insert-pre
  "Protected HTML markup" nil "<html><pre>" _ "</pre></html>")


(defun use-jpw-style-wikipedia-mode ()
  (interactive)
  (local-set-key "\C-cl" 'longlines-mode)
  (local-set-key "\C-c\C-l" 'longlines-mode)

  ;; Motion Keys
  (local-set-key [\C-right] 'forward-word)
  (local-set-key [\C-left] 'backward-word)
  (local-set-key [\C-up] 'backward-paragraph)
  (local-set-key [\C-down] 'forward-paragraph)

  ;; More disasters
  (local-set-key "\M-u" 'upcase-word)
  (local-set-key "\C-c\"" 'wikipedia-unfill-paragraph-or-region)
  (local-set-key [\M-return] 'join-next-line)
  (local-set-key "\M-pe" 'wikipedia-insert-enumerate)
  (local-set-key [\C-return] 'wikipedia-terminate-paragraph)
  (local-set-key "\M-pi" 'wikipedia-insert-itemize)

  ;; Outline Navigation
  (local-set-key [\M-down] 'wikipedia-next-header)
  (local-set-key [\M-up] 'wikipedia-prev-header)
  (local-set-key [\M-right] 'outline-forward-same-level)
  (local-set-key [\M-left] 'outline-backward-same-level)
  (local-set-key [?\C-c \C-up] 'outline-up-heading)
  (local-set-key [\C-\S-down] 'outline-move-subtree-down)
  (local-set-key [\C-\S-up] 'outline-move-subtree-up)
  (local-set-key [\C-\S-right] 'wikipedia-simple-outline-demote)
  (local-set-key [\C-\S-left] 'wikipedia-simple-outline-promote)
  (local-set-key [\M-\S-down] 'outline-move-subtree-down)
  (local-set-key [\M-\S-up] 'outline-move-subtree-up)
  (local-set-key [\M-\S-right] 'wikipedia-simple-outline-demote)
  (local-set-key [\M-\S-left] 'wikipedia-simple-outline-promote)

  ;; Fonts
  (local-set-key "\M-oi" 'wikipedia-insert-italics)
  (local-set-key "\M-ob" 'wikipedia-insert-bold)
  (local-set-key "\M-ou" 'tempo-template-html-underline)
  (local-set-key "\M-oe" 'wikipedia-insert-italics)
  (local-set-key "\M-os" 'wikipedia-insert-bold)
  (local-set-key "\M-ot" 'tempo-template-html-fixed)
  (local-set-key "\M-of" 'tempo-template-html-fixed)
  (local-set-key "\M-oo" 'tempo-template-html-code)
  (local-set-key "\M-oc" 'tempo-template-html-code)
  (local-set-key "\M-od" 'tempo-template-html-strikethru)

  ;; Paragraph styles
  (local-set-key "\M-pc" 'jpw-wikipedia-insert-pre)
  (local-set-key "\M-pa" 'wikipedia-insert-link)
  (local-set-key "\M-p\C-u" 'wikipedia-insert-link)
  (local-set-key "\M-pl" 'wikipedia-insert-itemize)
  (local-set-key "\M-p\C-m" 'wikipedia-terminate-paragraph)
  (local-set-key "\M-p\C-j" 'wikipedia-terminate-paragraph)
  (local-set-key "\M-p-" 'wikipedia-insert-html-hline)
  (local-set-key "\M-ph" 'wikipedia-insert-header)
  (local-set-key "\M-p\M-h" 'jpw-wikipedia-insert-html)
  (local-set-key "\M-p1" 'jpw-wikipedia-insert-header1)
  (local-set-key "\M-p2" 'jpw-wikipedia-insert-header2)
  (local-set-key "\M-p3" 'jpw-wikipedia-insert-header3)
  (local-set-key "\M-p4" 'jpw-wikipedia-insert-header4)

  ;; Entity expansion
  (local-set-key "\C-c'" 'jpw-html-entity-abbrev-expand)
  (local-set-key [?\C-c ?\C-'] 'jpw-html-entity-abbrev-expand)
  )


(defun use-jpw-style-elisp ()
  (interactive)
  (auto-fill-mode 1)
;;  (show-paren-mode 1)
  (global-set-key "\C-cg" 'goto-char)
  (local-set-key "\C-cd" 'edebug-eval-top-level-form)
  (local-set-key "\C-c\C-d" 'edebug-eval-top-level-form)
  (local-set-key "\C-ce" 'eval-defun)
  )


(defun use-jpw-style-c-common ()
  (interactive)
  (auto-fill-mode 1)
  (setq fill-column 78)
  (setq c-indent-comments-syntactically-p 't
        c-tab-always-indent "partial"
        )

  ;; Moves forward by capitalizations or words.  Very useful for C++ &
  ;; Java programming.
  (if running-xemacs
      (progn
        (local-set-key '("\C-c" left) 'c-beginning-of-defun)
        (local-set-key '("\C-c" right) 'c-end-of-defun)
        (local-set-key '(control shift left) 'c-backward-into-nomenclature)
        (local-set-key '(control shift right) 'c-forward-into-nomenclature)
        (local-set-key '(control shift up) 'c-beginning-of-statement)
        (local-set-key '(control shift down) 'c-end-of-statement)
        )
    ;; else
    (local-set-key [?\C-c left] 'c-beginning-of-defun)
    (local-set-key [?\C-c right] 'c-end-of-defun)
    (local-set-key [(control shift left)] 'c-backward-into-nomenclature)
    (local-set-key [(control shift right)] 'c-forward-into-nomenclature)
    (local-set-key [(control shift up)] 'c-beginning-of-statement)
    (local-set-key [(control shift down)] 'c-end-of-statement)
    )
  ;; This, for some reason, needs to be in the mode-specific autohook defuns.
  ;; Leave it here, too, just for documentation clarity.
  (local-unset-key [f4])
  ;; Force use of correct comment-break-fn.
  (local-set-key [?\C-c f7] 'compile)
  (local-set-key [?\C-c f8] 'recompile)
  (local-set-key "\M-j" 'do-comment-line-break)
  (local-set-key [\C-linefeed] 'do-comment-line-break)
  (local-set-key [\C-return] 'do-comment-line-break)
  (bind-jpw-c-mode-doxy)
  )


(defun use-jpw-style-cxx ()
  (interactive)
  (use-jpw-style-c-common)
  ;; Use the value of the `jpw-c-style' variable, for that added level of
  ;; indirection. ;)
  (c-set-style jpw-c-style)
  ;; Make sure this is set correctly...
  (local-unset-key [f4])
  )


(defconst jpw-buf-is-cxx-class-regex
  (concat "^\\s *\\(class\\|namespace\\|"
          "p\\(r\\(ivate\\|otected\\\)\\|ublic\\)\\)")
  )
(defconst jpw-buf-is-cxx-lib-regex
  "\\b\\(std::\\w\\|\\(c\\(err\\|out\\)\||string\\)\\b\\)"
  )
(defun use-jpw-style-c ()
  (interactive)
  (use-jpw-style-c-common)
  (if (and (string-match "\\.h$" (buffer-file-name (current-buffer)))
           (save-excursion
             (goto-char (point-min))
             (or
              (save-excursion
                (re-search-forward jpw-buf-is-cxx-class-regex (point-max) t)
                );; end inner excursion
              (save-excursion
                (re-search-forward jpw-buf-is-cxx-lib-regex (point-max) t)
                );; end inner excursion
              )
             );; end buffer-check excursion
           );; end and
      (c++-mode)
    ;; else - this really is a C-source file.
    ;;
    ;; Use the value of the `jpw-c-style' variable, for that added level of
    ;; indirection. ;)
    (c-set-style jpw-c-style)
    ;; Make sure this is set correctly...
    (local-unset-key [f4])
    );; end if
  )


(defun use-jpw-style-java ()
  (interactive)
  (use-jpw-style-c-common)
  (c-set-style "jpw-java")
  (setq fill-column 78)
  (bind-jpw-javadoc)
  (local-unset-key [f4])
  (local-set-key [?\C-c f7] 'compile)
  (local-set-key [?\C-c f8] 'recompile)
  )


(defun use-jpw-style-tex ()
  (interactive)
  (setq tab-width 4)
  (turn-on-auto-fill)
  )


(defun use-jpw-style-sql ()
  (interactive)
  (setq tab-width 2)
  (turn-on-auto-fill)
  ;;(sql-highlight-oracle-keywords)
  (sql-highlight-sybase-keywords)
  )


(defun use-jpw-style-perl ()
  (interactive)
  (auto-fill-mode 1)
  ;; Use a style equivalent to perl-mode indentation.
  (setq perl-label-offset -2)
  (use-jpw-perl-dabbrev-skip)
  )


(defun use-jpw-style-cperl ()
  (interactive)
  (auto-fill-mode 1)
  ;; Set the indentation style to my personal one.
  (cperl-set-style "jpw-cperl-style")
  (setq cperl-font-lock t  ;; Always use font-lock-mode
        cperl-highlight-variables-indiscriminately t
        ;; Additional Indentation Customizations
        cperl-break-one-line-blocks-when-indent nil
        cperl-comment-column 52
        ;; Disable a bunch of "electric" stuff.
        cperl-electric-parens nil
        cperl-electric-lbrace-space nil
        cperl-electric-linefeed nil
        )
  (use-jpw-perl-dabbrev-skip)
  )


(defun use-jpw-style-octave ()
  (interactive)
  (turn-on-auto-fill)
  (font-lock-mode)
  (local-set-key "\M-j" 'octave-indent-new-comment-line)
  (local-set-key [\C-linefeed] 'octave-indent-new-comment-line)
  )


(defun jpw-set-sgml-indent (arg)
 "Set indentation size(s) for SGML modes.
{jpw: 11/2005}"
 (interactive "nIndent Size: ")
 (make-local-variable 'tab-width)
 (make-local-variable 'standard-indent)
 (setq tab-width arg
       standard-indent arg)
 )


(defun use-jpw-style-sgml ()
  (interactive)
  (turn-on-auto-fill)
  (font-lock-mode)
  ;; Set other indentation variables to the sgml-mode's offset.  The latter
  ;;should be set using the (x)emacs customization engine.
  (jpw-set-sgml-indent sgml-basic-offset)
  (recenter))


(defun jpw-set-xml-lite-indent (arg)
  "Set indentation size for XML-Lite mode.
{jpw: 3/2003}"
  (interactive "nIndent Size: ")
  (jpw-set-sgml-indent arg)
  (setq xml-lite-indent-comment-offset arg
        xml-lite-indent-offset arg)
  )


(defun jpw-xml-lite-mode ()
  (interactive)
  (sgml-mode)
  (turn-on-auto-fill)
  (xml-lite-mode)
  (font-lock-mode)
  (jpw-set-xml-lite-indent 4)
  )


(defun jpw-speedbar-settings ()
  (interactive)
  (speedbar-disable-update)
  (speedbar-change-initial-expansion-list "buffers")
  (speedbar-update-contents)
  (speedbar-set-mode-line-format)
  )


(provide 'custom-mode-styles)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;