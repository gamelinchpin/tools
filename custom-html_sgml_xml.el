;; -*-byte-compile-dynamic: t;-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Functions
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
;; My custom functions.  Used as tools and bound to keys, or used as
;; autohooks, or just basic building-block functions.
;;
;;
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'skeleton)
(require 'tempo)
(require 'custom-defuns)


;;----------------------------------------------------------------------
;;
;; Common HTML Editing Commands
;;


(defconst jpw-html-symbol-entity-table
  '(
    ("!=" . "&ne;")
    ("&" . "&amp;")
    ("'" . "&rsquo;")
    ("''" . "&rdquo;")
    ("--" . "&ndash;")
    ("---" . "&mdash;")
    ("->" .  "&rarr;")
    ("-^" . "&uarr;")
    ("-v" . "&darr;")
    ("..." . "&hellip;")
    ("<" . "&lt;")
    ("<-" . "&larr;")
    ("<->" . "&harr;")
    ("<=" . "&le;")
    ("<==" . "&lArr;")
    ("<==>" . "&hArr;")
    ("<=>" . "&hArr;")
    ("===" . "&equiv;")
    ("==>" . "&rArr;")
    ("==^" . "&uArr;")
    ("==v" . "&dArr;")
    (">" . "&gt;")
    (">=" . "&ge;")
    ("\"" . "&quot;")
    ("--+" . "&dagger;")
    ("+-+" . "&Dagger;")
    (",," . "&bdquo;")
    (".," . "&sbquo;")
    ("._" . "&thinsp;")
    ("__" . "&ensp;")
    ("___" . "&emsp;")
    ("`" . "&lsquo;")
    ("``" . "&ldquo;")
    ("'`" . "&rdquo;")
    ;; Canonical versions of the up & down arrows.
    ("|^" . "&uarr;")
    ("|v" . "&darr;")
    ("||^" . "&uArr;")
    ("||v" . "&dArr;")
    ("~" . "&sim;")
    ("~=" . "&cong;")
    ("~~" . "&asymp;")
    (" " . "&nbsp;")
    )
  "A table of abreviations for creating HTML entities.  Note that there are a
few alternatives for the same entity, so that mixing two similar mnemnonics
(e.g. \"->\" and \"|v\" to get \"-v\") still works.

If your mnemnonic doesn't follow a word character or whitespace, you can
\"separate\" it from the preceding character(s) using either a ';' or a ':'.
The separator will be consumed with the mnemnonic when it's converted to the
corresponding HTML entity.
{jpw; 08/2009}")


(defconst jpw-html-symbol-entity-table-maxlen
  (eval-when-compile
    (let* ((mnemonics (mapcar 'car jpw-html-symbol-entity-table))
           (mnemonics-len (mapcar 'length mnemonics))
           );end bindings
      (1+ (1+ (eval (append '(max) mnemonics-len))))
      )
    )
  "Length of the largest of the HTML entity mnemonics in
`jpw-html-symbol-entity-table'.
{jpw; 08/2008}")


(defconst jpw-html-symbol-entity-table-re
  (eval-when-compile
    (let* ((mnemonics (mapcar 'car jpw-html-symbol-entity-table))
           (notSyms "[^-<=>&~.\"`'^v|]")
           );end bindings
      ;; N.B. - Must contain a single group, surrounding the portion
      ;; of the regex matching the shortcut-table keys.
      (concat notSyms
              "\\([;:]?"
              (regexp-opt mnemonics t)
              "\\)"
              notSyms
       )
      );end let*
  )
  "A cached regexp that matches any of the HTML entity mnemonics in
`jpw-html-symbol-entity-table'.
{jpw; 03/2006}")


(defun jpw-html-fix-tempo-templates ()
"Redefine some of the tempo-template-* commands used by `html-helper-mode'
so that they produce valid XML.
{jpw: 08/2008}"
  (interactive)

  (tempo-define-template
   "html-line-break"
   '(& "<br/>" > n)
   nil
   "XHTML line break tag.
This function has been redefined so that it produces a complete tag.
{jpw: 08/2008}"
   )


  (tempo-define-template
   "html-horizontal-line"
   '(& "<hr/>" > n)
   nil
   "XHTML horizontal rule tag.
This function has been redefined so that it produces a complete tag.
{jpw: 08/2008}"
   )

  )
(jpw-html-fix-tempo-templates)


(define-skeleton jpw-html-href-anchor
  "HTML anchor tag with href attribute.
Like the sgml-mode version, but without the annoying \"http:\" defaulting into
the URL prompt.
{jpw: 03/2006}"
  "URL: "
  "<a href=\"" str "\">" _ "</a>")

(define-skeleton jpw-html-del
  "Insert HTML [logical] \"<del>\" tags, or puts the active region inside HTML
strong tags.
{jpw: 03/2006}"
  nil
  "<del>" _ "</del>")

(define-skeleton jpw-html-super
  "Insert HTML \"<sup>\" tags, or puts the active region inside HTML
strong tags.
{jpw: 11/2009}"
  nil
  "<sup>" _ "</sup>")

(define-skeleton jpw-html-sub
  "Insert HTML \"<sub>\" tags, or puts the active region inside HTML
strong tags.
{jpw: 11/2009}"
  nil
  "<sub>" _ "</sub>")


(define-skeleton jpw-html-size-small
  "Insert HTML font resizing tag \"<small>\".
{jpw: 03/2006}"
  nil
  "<small>" _  "</small>"
  )

(define-skeleton jpw-html-size-big
  "Insert HTML font resizing tag \"<big>\".
{jpw: 03/2006}"
  nil
  "<big>" _  "</big>"
  )

(define-skeleton jpw-html-size-relative
  "Insert XHTML font resizing markup.
{jpw: 03/2006}"
  (completing-read "Size: " jpw-html-size-alist nil nil "small")
  "<span style=\"font-size: " str "\">" _ "</span>")


(defun jpw-html-insert-list (&optional type)
  "Insert HTML list tags, or puts the active region inside HTML list
tags.
The optional `type' specifies the type of list.  It can be passed directly or
specified using a prefix-arg.  If `type' is an integer [e.g. a prefix-arg],
then the list will be an ordered list.  Otherwise, the list is unordered.
Any other type is an error.
{jpw: 03/2006}"
  (interactive "P")
  ;; Validation check.
  (or (null type)
      (char-or-string-p type)
      (signal 'wrong-type-argument
              (list 'char-or-string-p type)))
  ;; Clear the prefix arg so it doesn't screw up the behavior of the
  ;; `skeleton-insert' call.
  (if type (setq prefix-arg nil
                 current-prefix-arg nil))
  (if (or (null type)
          (stringp type))
      (tempo-template-html-unordered-list)
    (tempo-template-html-ordered-list)
    )
  )


(defun jpw-html-entity-abbrev-expand ()
  "Converts the mnemonic at point to an HTML entity.  See the documentation
for the variable, `jpw-html-symbol-entity-table', for the valid
abbreviations/mnemnonics.
{jpw: 08/2008}"
  (interactive)
  (save-excursion

    (let* ((end (1+ (point)))
           (start (- (point) jpw-html-symbol-entity-table-maxlen))
           entity
           );;end bindings
      (goto-char start)
      (if (posix-search-forward jpw-html-symbol-entity-table-re end t)
          (setq entity (cdr (assoc (match-string 2)
                                   jpw-html-symbol-entity-table)))
        )
      (if entity
          (replace-match entity t t nil 1)
        )
      ) ;;end let

    );;end excursion
  )


(provide 'custom-html_sgml_xml)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;