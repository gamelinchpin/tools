;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Major and Minor modes for editing LiveJournal entries.
;;
;;  Copyright © 2006-2008 John P. Weiss except as documented below
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
;; Certain code in this file was taken, lock, stock, and barrel fro Jamie
;; Zawinski's "jwz-lj.el" file.  That code has the following copyright:
;;
;;   Copyright © 2002, 2003, 2004, 2005 Jamie Zawinski <jwz@jwz.org>.
;;
;;   Permission to use, copy, modify, distribute, and sell this software and
;;   its documentation for any purpose is hereby granted without fee, provided
;;   that the above copyright notice appear in all copies and that both that
;;   copyright notice and this permission notice appear in supporting
;;   documentation.  No representations are made about the suitability of this
;;   software for any purpose.  It is provided "as is" without express or
;;   implied warranty.
;;
;;
;; You can enter the major-mode using `\M-p\M-j'.
;; You enter the minor-mode using `\M-x jpw-lj-minor-mode'.
;;
;;
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'sgml-mode)
(require 'custom-defuns)
(require 'lazy-lock)
(eval-when-compile
  (require 'mule)
  )


;;------------------------------------------------------------
;;
;; Utility Functions ... Required by Customizations
;;


(defun jpw-lj-custom-set-var (symbol val)
  ;; Add the elements of `val' to the alist, `symbol', as alist keys.
  (if (not (boundp symbol))
      (set symbol (list))
      )
  (let ((sym-alist (car
                    (rassq symbol
                           '((jpw-lj-avatar-alist . jpw-lj-user-avatars)
                             (jpw-lj-security-alist . jpw-lj-friend-groups)
                             ))))
        l-add-to-sym)
    (fset 'l-add-to-sym (lambda (arg)
                          (add-to-list symbol arg t)
                          (if sym-alist (add-to-list sym-alist (list arg) t))
                          )
          )
    (mapcar 'l-add-to-sym val)
    )
  )


;;------------------------------------------------------------
;;
;; User Customizations
;;


(defgroup jpw-lj 'nil
  "Customizable aspects of jpw-lj-mode."
  :group 'hypermedia
  :group 'local)


(defcustom jpw-lj-unfill-on-save nil
  "Unfill the buffer [using `jpw-unfill-buffer'] before saving it."
  :type 'boolean
  :group 'jpw-lj)


(defcustom jpw-lj-unfill-removes-blank-line nil
  "When unfilling a paragraph or the buffer [using `jpw-lj-unfill-paragraph'
or `jpw-lj-unfill-buffer'] remove all interparagraph blank lines."
  :type 'boolean
  :group 'jpw-lj)


(defcustom jpw-lj-xlate-entities-on-save nil
  "Translate all Latin-1 chars in the buffer to HTML entites [using
`jwz-lj-entify'] before saving."
  :type 'boolean
  :group 'jpw-lj)


(defcustom jpw-lj-user-avatars nil
  "A list of your avatars.  Each element in the list should be the
\"official\" LiveJournal name of your avatars.

Manual changes to this setting [i.e. outside of `custom-mode'] won't become
active until after you restart Emacs.  If you want to make changes effective
immediately, call the function `jpw-lj-init-customizations'."
  :type '(repeat string)
  :set  'jpw-lj-custom-set-var
  :group 'jpw-lj)


(defcustom jpw-lj-friend-groups nil
  "A list of your LiveJournal \"Friends\" groups.
It will be used by the `jpw-lj-security-hdr' skeleton.

Manual changes to this setting [i.e. outside of `custom-mode'] won't become
active until after you restart Emacs.  If you want to make changes effective
immediately, call the function `jpw-lj-init-customizations'."
  :type '(repeat string)
  :set  'jpw-lj-custom-set-var
  :group 'jpw-lj)


;; N.B.: Must quote the initial value, or emacs complains of an undefined
;; variable.
(defcustom jpw-lj-default-html-size-function 'jpw-html-size-relative
  "Name of the default type of HTML tag to use for adjusting font size.

 The three possible values are `jpw-html-size-relative', `jpw-html-size-small'
and `jpw-html-size-big'.  The former uses a \"<span>\" tag with a font sizing
style.  The other two use the HTML 4.0 tags, \"<small>\" or \"<big>\",
respectively."
  :type '(choice (const jpw-html-size-relative)
                 (const jpw-html-size-small)
                 (const jpw-html-size-big))
  :group 'jpw-lj)


(defface jpw-lj-bold-face
  '((t (:inherit bold)))
  "How to color <strong>...</strong> and <b>...</b> tags."
  :group 'jpw-lj)

(defface jpw-lj-italic-face
  '((t (:inherit italic)))
  "How to color <em>...</em> and <i>...</i> tags."
  :group 'jpw-lj)

(defface jpw-lj-underline-face
  '((t (:inherit underline)))
  "How to color <u>...</u> tags."
  :group 'jpw-lj)

(defface jpw-lj-tag-face
  '((t (:inherit font-lock-function-name-face)))
  "The face to use for non-markup jpw-lj tags."
  :group 'jpw-lj)

(defface jpw-lj-size-face
  '((t (foreground "DarkMagenta")))
  "The face to use for the HTML `FONT' tag."
  :group 'jpw-lj)

(defface jpw-lj-header-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to use for LiveJournal Headers."
  :group 'jpw-lj)

(defface jpw-lj-code-face
  '((t (:inherit font-lock-constant-face)))
  "The face to use for code markup."
  :group 'jpw-lj)

(defface jpw-lj-del-face
  '((t (:strike-through t)))
  "The face to use for the <del> ... </del> HTML markup [a logical style that
replaces the old, deprecated \"<strike>\" tag]."
  :group 'jpw-lj)

(defface jpw-lj-url-face
  '((t (:inherit font-lock-keyword-face
                 :underline t :bold t)))
  "The face to use for URLs."
  :group 'jpw-lj)


;;------------------------------------------------------------
;;
;; Constants and Variables
;;


(defconst jpw-html-size-alist
  '(("xx-small") ("x-small") ("small") ("smaller") ("medium") ("large")
    ("larger") ("x-large") ("xx-large"))
  "Alist of LiveJournal size keywords.
{jpw: 03/2006}")


(defconst jpw-lj-mood-alist
  '(("accomplished") ("aggravated") ("amused") ("angry") ("annoyed")
    ("anxious") ("apathetic") ("artistic") ("awake") ("bitchy") ("blah")
    ("blank") ("bored") ("bouncy") ("busy") ("calm") ("cheerful") ("chipper")
    ("cold") ("complacent") ("confused") ("contemplative") ("content")
    ("cranky") ("crappy") ("crazy") ("creative") ("crushed") ("curious")
    ("cynical") ("depressed") ("determined") ("devious") ("dirty")
    ("disappointed") ("discontent") ("distressed") ("ditzy") ("dorky")
    ("drained") ("drunk") ("ecstatic") ("embarrassed") ("energetic")
    ("enraged") ("enthralled") ("envious") ("exanimate") ("excited")
    ("exhausted") ("flirty") ("frustrated") ("full") ("geeky") ("giddy")
    ("giggly") ("gloomy") ("good") ("grateful") ("groggy") ("grumpy")
    ("guilty") ("happy") ("high") ("hopeful") ("horny") ("hot") ("hungry")
    ("hyper") ("impressed") ("indescribable") ("indifferent") ("infuriated")
    ("intimidated") ("irate") ("irritated") ("jealous") ("jubilant") ("lazy")
    ("lethargic") ("listless") ("lonely") ("loved") ("melancholy") ("mellow")
    ("mischievous") ("moody") ("morose") ("naughty") ("nauseated") ("nerdy")
    ("nervous") ("nostalgic") ("numb") ("okay") ("optimistic") ("peaceful")
    ("pensive") ("pessimistic") ("pissed off") ("pleased") ("predatory")
    ("productive") ("quixotic") ("recumbent") ("refreshed") ("rejected")
    ("rejuvenated") ("relaxed") ("relieved") ("restless") ("rushed") ("sad")
    ("satisfied") ("scared") ("shocked") ("sick") ("silly") ("sleepy")
    ("sore") ("stressed") ("surprised") ("sympathetic") ("thankful")
    ("thirsty") ("thoughtful") ("tired") ("touched") ("uncomfortable")
    ("weird") ("working") ("worried"))
  "Alist of LiveJournal moods.
{jpw: 03/2006}")


(defconst jpw-lj-comments-alist
  '(("N" . "Don't screen comments.")
    ("R" . "Screen anonymous comments only")
    ("F" . "Screen comments made by non-friends")
    ("A" . "Screen *All* comments"))
  "Alist of LiveJournal comment-screening flags, along with text descriptions
of each flag.
{jpw: 03/2006}")


(defvar jpw-lj-security-alist
  '(("public") ("private") ("friends"))
  "Alist of LiveJournal security keywords.
{jpw: 03/2006}")


(defvar jpw-lj-avatar-alist '()
  "Alist of user avatars.

Rebuilt from `jpw-lj-user-avatars' by calling `jpw-lj-init-customizations',
which is called when emacs first loads the \"jpw-lj-helper\" library.
{jpw: 03/2006}")


(defvar jpw-lj-minor-mode-original-font-lock-defaults 'nil)


(defconst jpw-lj-entity-table
  (eval-when-compile
    (list
     (cons "nbsp"     (decode-char 'ucs ?\xa0))
     (cons "OElig"    (decode-char 'ucs ?\x152))
     (cons "oelig"    (decode-char 'ucs ?\x153))
     (cons "Scaron"   (decode-char 'ucs ?\x160))
     (cons "scaron"   (decode-char 'ucs ?\x161))
     (cons "Yuml"     (decode-char 'ucs ?\x178))
     (cons "fnof"     (decode-char 'ucs ?\x192))
     (cons "Alpha"    (decode-char 'ucs ?\x391))
     (cons "Beta"     (decode-char 'ucs ?\x392))
     (cons "Gamma"    (decode-char 'ucs ?\x393))
     (cons "Delta"    (decode-char 'ucs ?\x394))
     (cons "Epsilon"  (decode-char 'ucs ?\x395))
     (cons "Zeta"     (decode-char 'ucs ?\x396))
     (cons "Eta"      (decode-char 'ucs ?\x397))
     (cons "Theta"    (decode-char 'ucs ?\x398))
     (cons "Iota"     (decode-char 'ucs ?\x399))
     (cons "Kappa"    (decode-char 'ucs ?\x39a))
     (cons "Lambda"   (decode-char 'ucs ?\x39b))
     (cons "Mu"       (decode-char 'ucs ?\x39c))
     (cons "Nu"       (decode-char 'ucs ?\x39d))
     (cons "Xi"       (decode-char 'ucs ?\x39e))
     (cons "Omicron"  (decode-char 'ucs ?\x39f))
     (cons "Pi"       (decode-char 'ucs ?\x3a0))
     (cons "Rho"      (decode-char 'ucs ?\x3a1))
     (cons "Sigma"    (decode-char 'ucs ?\x3a3))
     (cons "Tau"      (decode-char 'ucs ?\x3a4))
     (cons "Upsilon"  (decode-char 'ucs ?\x3a5))
     (cons "Phi"      (decode-char 'ucs ?\x3a6))
     (cons "Chi"      (decode-char 'ucs ?\x3a7))
     (cons "Psi"      (decode-char 'ucs ?\x3a8))
     (cons "Omega"    (decode-char 'ucs ?\x3a9))
     (cons "alpha"    (decode-char 'ucs ?\x3b1))
     (cons "beta"     (decode-char 'ucs ?\x3b2))
     (cons "gamma"    (decode-char 'ucs ?\x3b3))
     (cons "delta"    (decode-char 'ucs ?\x3b4))
     (cons "epsilon"  (decode-char 'ucs ?\x3b5))
     (cons "zeta"     (decode-char 'ucs ?\x3b6))
     (cons "eta"      (decode-char 'ucs ?\x3b7))
     (cons "theta"    (decode-char 'ucs ?\x3b8))
     (cons "iota"     (decode-char 'ucs ?\x3b9))
     (cons "kappa"    (decode-char 'ucs ?\x3ba))
     (cons "lambda"   (decode-char 'ucs ?\x3bb))
     (cons "mu"       (decode-char 'ucs ?\x3bc))
     (cons "nu"       (decode-char 'ucs ?\x3bd))
     (cons "xi"       (decode-char 'ucs ?\x3be))
     (cons "omicron"  (decode-char 'ucs ?\x3bf))
     (cons "pi"       (decode-char 'ucs ?\x3c0))
     (cons "rho"      (decode-char 'ucs ?\x3c1))
     (cons "sigmaf"   (decode-char 'ucs ?\x3c2))
     (cons "sigma"    (decode-char 'ucs ?\x3c3))
     (cons "tau"      (decode-char 'ucs ?\x3c4))
     (cons "upsilon"  (decode-char 'ucs ?\x3c5))
     (cons "phi"      (decode-char 'ucs ?\x3c6))
     (cons "chi"      (decode-char 'ucs ?\x3c7))
     (cons "psi"      (decode-char 'ucs ?\x3c8))
     (cons "omega"    (decode-char 'ucs ?\x3c9))
     (cons "thetasym" (decode-char 'ucs ?\x3d1))
     (cons "upsih"    (decode-char 'ucs ?\x3d2))
     (cons "piv"      (decode-char 'ucs ?\x3d6))
     (cons "ensp"     (decode-char 'ucs ?\x2002))
     (cons "emsp"     (decode-char 'ucs ?\x2003))
     (cons "thinsp"   (decode-char 'ucs ?\x2009))
     (cons "zwj"      (decode-char 'ucs ?\x200d))
     (cons "lrm"      (decode-char 'ucs ?\x200e))
     (cons "zwnj"     (decode-char 'ucs ?\x200c))
     (cons "rlm"      (decode-char 'ucs ?\x200f))
     (cons "ndash"    (decode-char 'ucs ?\x2013))
     (cons "mdash"    (decode-char 'ucs ?\x2014))
     (cons "lsquo"    (decode-char 'ucs ?\x2018))
     (cons "rsquo"    (decode-char 'ucs ?\x2019))
     (cons "sbquo"    (decode-char 'ucs ?\x201a))
     (cons "ldquo"    (decode-char 'ucs ?\x201c))
     (cons "rdquo"    (decode-char 'ucs ?\x201d))
     (cons "bdquo"    (decode-char 'ucs ?\x201e))
     (cons "dagger"   (decode-char 'ucs ?\x2020))
     (cons "Dagger"   (decode-char 'ucs ?\x2021))
     (cons "bull"     (decode-char 'ucs ?\x2022))
     (cons "hellip"   (decode-char 'ucs ?\x2026))
     (cons "permil"   (decode-char 'ucs ?\x2030))
     (cons "prime"    (decode-char 'ucs ?\x2032))
     (cons "Prime"    (decode-char 'ucs ?\x2033))
     (cons "oline"    (decode-char 'ucs ?\x203e))
     (cons "frasl"    (decode-char 'ucs ?\x2044))
     (cons "euro"     (decode-char 'ucs ?\x20ac))
     (cons "image"    (decode-char 'ucs ?\x2111))
     (cons "weierp"   (decode-char 'ucs ?\x2118))
     (cons "real"     (decode-char 'ucs ?\x211c))
     (cons "trade"    (decode-char 'ucs ?\x2122))
     (cons "alefsym"  (decode-char 'ucs ?\x2135))
     (cons "larr"     (decode-char 'ucs ?\x2190))
     (cons "uarr"     (decode-char 'ucs ?\x2191))
     (cons "rarr"     (decode-char 'ucs ?\x2192))
     (cons "darr"     (decode-char 'ucs ?\x2193))
     (cons "harr"     (decode-char 'ucs ?\x2194))
     (cons "crarr"    (decode-char 'ucs ?\x21b5))
     (cons "lArr"     (decode-char 'ucs ?\x21d0))
     (cons "uArr"     (decode-char 'ucs ?\x21d1))
     (cons "rArr"     (decode-char 'ucs ?\x21d2))
     (cons "dArr"     (decode-char 'ucs ?\x21d3))
     (cons "hArr"     (decode-char 'ucs ?\x21d4))
     (cons "forall"   (decode-char 'ucs ?\x2200))
     (cons "part"     (decode-char 'ucs ?\x2202))
     (cons "exist"    (decode-char 'ucs ?\x2203))
     (cons "empty"    (decode-char 'ucs ?\x2205))
     (cons "nabla"    (decode-char 'ucs ?\x2207))
     (cons "isin"     (decode-char 'ucs ?\x2208))
     (cons "notin"    (decode-char 'ucs ?\x2209))
     (cons "prod"     (decode-char 'ucs ?\x220f))
     (cons "sum"      (decode-char 'ucs ?\x2211))
     (cons "minus"    (decode-char 'ucs ?\x2212))
     (cons "lowast"   (decode-char 'ucs ?\x2217))
     (cons "radic"    (decode-char 'ucs ?\x221a))
     (cons "prop"     (decode-char 'ucs ?\x221d))
     (cons "infin"    (decode-char 'ucs ?\x221e))
     (cons "ang"      (decode-char 'ucs ?\x2220))
     (cons "and"      (decode-char 'ucs ?\x2227))
     (cons "or"       (decode-char 'ucs ?\x2228))
     (cons "cap"      (decode-char 'ucs ?\x2229))
     (cons "cup"      (decode-char 'ucs ?\x222a))
     (cons "int"      (decode-char 'ucs ?\x222b))
     (cons "there4"   (decode-char 'ucs ?\x2234))
     (cons "sim"      (decode-char 'ucs ?\x223c))
     (cons "cong"     (decode-char 'ucs ?\x2245))
     (cons "asymp"    (decode-char 'ucs ?\x2248))
     (cons "ne"       (decode-char 'ucs ?\x2260))
     (cons "equiv"    (decode-char 'ucs ?\x2261))
     (cons "le"       (decode-char 'ucs ?\x2264))
     (cons "ge"       (decode-char 'ucs ?\x2265))
     (cons "sub"      (decode-char 'ucs ?\x2282))
     (cons "sup"      (decode-char 'ucs ?\x2283))
     (cons "nsub"     (decode-char 'ucs ?\x2284))
     (cons "sube"     (decode-char 'ucs ?\x2286))
     (cons "supe"     (decode-char 'ucs ?\x2287))
     (cons "oplus"    (decode-char 'ucs ?\x2295))
     (cons "otimes"   (decode-char 'ucs ?\x2297))
     (cons "perp"     (decode-char 'ucs ?\x22a5))
     (cons "sdot"     (decode-char 'ucs ?\x22c5))
     (cons "lceil"    (decode-char 'ucs ?\x2308))
     (cons "rceil"    (decode-char 'ucs ?\x2309))
     (cons "lfloor"   (decode-char 'ucs ?\x230a))
     (cons "rfloor"   (decode-char 'ucs ?\x230b))
     (cons "lang"     (decode-char 'ucs ?\x2329))
     (cons "rang"     (decode-char 'ucs ?\x232a))
     (cons "loz"      (decode-char 'ucs ?\x25ca))
     (cons "spades"   (decode-char 'ucs ?\x2660))
     (cons "clubs"    (decode-char 'ucs ?\x2663))
     (cons "hearts"   (decode-char 'ucs ?\x2665))
     (cons "diams"    (decode-char 'ucs ?\x2666))
     ))
  "HTML entities to Unicode characters.
The Unicode characters in this table include the Greek alphabet, math symbols,
and a few typography symbols.")


;; This next variable (C) 2002-2005 by Jamie Zawinski <jwz@jwz.org>
;; Taken lock, stock, and barrel from "jwz-lj.el".
;; This entire variable, including my additions, fall under the copyright
;; notice at the top of this file.
(defconst jwz-lj-entity-table
  ;; [jpw; 03/2006] Append my own additions to jwz's original list
  (append
   ;; [jpw; 03/2006] The original contents of jwz-lj-entity-table
   '(
     ;("quot"   . ?\") ("amp"    . ?\&) ("lt"     . ?\<) ("gt"     . ?\>)
     ;("nbsp"   . ?\ )
     ("iexcl"  . ?\¡) ("cent"   . ?\¢) ("pound"  . ?\£)
     ("curren" . ?\¤) ("yen"    . ?\¥) ("brvbar" . ?\¦) ("sect"   . ?\§)
     ("uml"    . ?\¨) ("copy"   . ?\©) ("ordf"   . ?\ª) ("laquo"  . ?\«)
     ("not"    . ?\¬) ("shy"    . ?\­) ("reg"    . ?\®) ("macr"   . ?\¯)
     ("deg"    . ?\°) ("plusmn" . ?\±) ("sup2"   . ?\²) ("sup3"   . ?\³)
     ("acute"  . ?\´) ("micro"  . ?\µ) ("para"   . ?\¶) ("middot" . ?\·)
     ("cedil"  . ?\¸) ("sup1"   . ?\¹) ("ordm"   . ?\º) ("raquo"  . ?\»)
     ("frac14" . ?\¼) ("frac12" . ?\½) ("frac34" . ?\¾) ("iquest" . ?\¿)
     ("Agrave" . ?\À) ("Aacute" . ?\Á) ("Acirc"  . ?\Â) ("Atilde" . ?\Ã)
     ("Auml"   . ?\Ä) ("Aring"  . ?\Å) ("AElig"  . ?\Æ) ("Ccedil" . ?\Ç)
     ("Egrave" . ?\È) ("Eacute" . ?\É) ("Ecirc"  . ?\Ê) ("Euml"   . ?\Ë)
     ("Igrave" . ?\Ì) ("Iacute" . ?\Í) ("Icirc"  . ?\Î) ("Iuml"   . ?\Ï)
     ("ETH"    . ?\Ð) ("Ntilde" . ?\Ñ) ("Ograve" . ?\Ò) ("Oacute" . ?\Ó)
     ("Ocirc"  . ?\Ô) ("Otilde" . ?\Õ) ("Ouml"   . ?\Ö) ("times"  . ?\×)
     ("Oslash" . ?\Ø) ("Ugrave" . ?\Ù) ("Uacute" . ?\Ú) ("Ucirc"  . ?\Û)
     ("Uuml"   . ?\Ü) ("Yacute" . ?\Ý) ("THORN"  . ?\Þ) ("szlig"  . ?\ß)
     ("agrave" . ?\à) ("aacute" . ?\á) ("acirc"  . ?\â) ("atilde" . ?\ã)
     ("auml"   . ?\ä) ("aring"  . ?\å) ("aelig"  . ?\æ) ("ccedil" . ?\ç)
     ("egrave" . ?\è) ("eacute" . ?\é) ("ecirc"  . ?\ê) ("euml"   . ?\ë)
     ("igrave" . ?\ì) ("iacute" . ?\í) ("icirc"  . ?\î) ("iuml"   . ?\ï)
     ("eth"    . ?\ð) ("ntilde" . ?\ñ) ("ograve" . ?\ò) ("oacute" . ?\ó)
     ("ocirc"  . ?\ô) ("otilde" . ?\õ) ("ouml"   . ?\ö) ("divide" . ?\÷)
     ("oslash" . ?\ø) ("ugrave" . ?\ù) ("uacute" . ?\ú) ("ucirc"  . ?\û)
     ("uuml"   . ?\ü) ("yacute" . ?\ý) ("thorn"  . ?\þ) ("yuml"   . ?\ÿ))
   ;; [jpw; 03/2006] My own additions:
   jpw-lj-entity-table
   )
  "HTML entities to Latin1 characters.

{jpw: 03/2006} Added:  Several forms of Unicode characters.  See
`jpw-lj-entity-table' for details.")


(defconst jpw-lj-entity-shortcut-table
  '(
    (":<=" . "&le;")
    (":<" . "&lt;")
    (":>=" . "&ge;")
    (":>" . "&gt;")
    (":===" . "&equiv;")
    (":~=" . "&cong;")
    (":~" . "&sim;")
    (":~~" . "&asymp;")
    ("<->" . "&darr;")
    ("<-" . "&larr;")
    ("->" . "&rarr;")
    ("<==" . "&lArr;")
    ("==>" . "&rArr;")
    ("---" . "&mdash;")
    ("--" . "&ndash;")
    ("..." . "&hellip;")
    ("\\..." . ".<!---->..")
    ("\\---" . "-<!---->-<!---->-")
    ("\\--" . "-<!---->-")
    )
  "A set of visual shortcuts for certain HTML entities.

NOTE: The order in which these are listed is important for the proper construction
of search regexps.
{jpw: 03/2006}")


(defconst jpw-jwz-lj-entity-table-re
  (eval-when-compile
    (set-buffer-multibyte t)
    (concat "["
            (mapconcat #'(lambda (x)
                             (make-string 1 (cdr x))
                             )
                       jwz-lj-entity-table nil)
            "]")
    )
    "A cached regexp that matches any of the characters in
`jwz-lj-entity-table'.  Replaces code for on-the-fly construction of this
regexp in `jwz-lj-entify'.
{jpw; 03/2006}")


(defconst jpw-lj-entity-shortcut-table-re
  (eval-when-compile
    (concat "\\(?:\\w\\|\\s \\)"
            ;; N.B. - Must contain a single group, surrounding the portion of
            ;; the regex matching the shortcut-table keys.
            (regexp-opt
             (mapcar 'car jpw-lj-entity-shortcut-table)
             t)
            "\\(?:\\w\\|\\s \\)"
            )
    )
  "A cached regexp that matches any of the entity shortcuts in
`jpw-lj-entity-shortcut-table'.
{jpw; 03/2006}")


;;------------------------------------------------------------
;;
;; Utility Functions
;;


;; FIXME:
;; This won't work.  We need to pass this to lj at runtime.
(defsubst jpw-lj-unfill-skip-line (next-nonws-char)
  (and (or (char-equal (char-before next-nonws-char) ?\n)
           (bobp)
           )
       (char-equal (char-after next-nonws-char) ?<)
       )
  )


(defsubst jpw-lj-enhanced-entify (start end)
  (save-excursion
    (goto-char start)
    ;; jpw-lj-entity-shortcut-table-re contains a single matching group.
    ;; Inside of that group is a regexp for all of the shortcut-table keys.
    (while (re-search-forward jpw-lj-entity-shortcut-table-re end t)
      (let* ((entity (cdr
                      (assoc (match-string 1) jpw-lj-entity-shortcut-table)))
             );;end bindings
        ;; Only replace the part of the match that corresponds to the
        ;; shortcut-table key.  Leave any preceeding or following characters
        ;; in the match alone.
        (replace-match entity t t nil 1)
        );;end let
      );;end while
    );;end excursion
  )


(defsubst find-non-ascii-charset-region (beg end)
  "Something to shut up the compilation errors in GNU Emacs
{jpw: 03/2006}"
  (delq 'ascii (find-charset-region beg end))
  )


;; This next defun (C) 2002-2005 by Jamie Zawinski <jwz@jwz.org>
;; Taken lock, stock, and barrel from "jwz-lj.el".
;; This entire defun, including my additions, fall under the copyright
;; notice at the top of this file.
(defun jwz-lj-entify (&optional start end)
  "Convert any non-ASCII characters in the region to HTML entities.
If there is no region, use the whole buffer."
  (interactive)
  (let ((re ;;(concat "["
            ;;        (mapconcat #'(lambda (x) (make-string 1 (cdr x)))
            ;;                   jwz-lj-entity-table nil)
            ;;        "]")
         ;; [jpw; 03/2006] Use my cached version instead of computing it every
         ;; time we call this defun.
         jpw-jwz-lj-entity-table-re)
        (case-fold-search nil))
    (cond ((or start end)
           (or start (setq start (point-min)))
           (or end   (setq end   (point-max))))
          (t
           (setq start (point-min))
           (setq end (point-max)))
          (if (region-active-p)
              (setq start (if (< (point) (mark)) (point) (mark))
                    end   (if (< (point) (mark)) (mark) (point)))))
    (save-excursion
      (goto-char start)
      (setq end (copy-marker end))
      (while (search-forward-regexp re end t)
        (let* ((ch (preceding-char))
               (entity (or (car (rassq ch jwz-lj-entity-table))
                           (error "no entity %c" ch))))
          (delete-char -1)
          (insert-before-markers "&" entity ";"))))

    ;; [jpw; 03/2006] My own special additions
    (jpw-lj-enhanced-entify start end)
    )

  (if (and (boundp 'find-non-ascii-charset-region)
           (fboundp 'find-non-ascii-charset-region)
           (find-non-ascii-charset-region start end))
      (error "non-ascii characters exist in this buffer!"))
  )


;;------------------------------------------------------------
;;
;; Mode Interactive Functions
;;


;; Skeleton Templates


(define-skeleton jpw-html-italic
  "Insert HTML [physical] italics tags, or puts the active region inside HTML
italics tags.
{jpw: 03/2006}"
  nil
  "<i>" _ "</i>")

(define-skeleton jpw-html-bold
  "Insert HTML [physical] bold tags, or puts the active region inside HTML
bold tags.
{jpw: 03/2006}"
  nil
  "<b>" _ "</b>")

(define-skeleton jpw-html-underline
  "Insert HTML [physical] underline tags, or puts the active region inside
HTML underline tags.
{jpw: 03/2006}"
  nil
  "<u>" _ "</u>")

(define-skeleton jpw-html-emphasized
  "Insert HTML [logical] emphasized tags, or puts the active region inside
HTML emphasized tags.
{jpw: 03/2006}"
  nil
  "<em>" _ "</em>")

(define-skeleton jpw-html-strong
  "Insert HTML [logical] strong tags, or puts the active region inside HTML
strong tags.
{jpw: 03/2006}"
  nil
  "<strong>" _ "</strong>")

(define-skeleton jpw-html-del
  "Insert HTML [logical] \"<del>\" tags, or puts the active region inside HTML
strong tags.
{jpw: 03/2006}"
  nil
  "<del>" _ "</del>")

(define-skeleton jpw-html-code
  "Insert HTML code tags, or puts the active region inside HTML code
tags.
{jpw: 03/2006}"
  nil
  "<code>" _ "</code>")


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


(define-skeleton jpw-lj-user
  "A LiveJournal \"user\" tag."
  "Who? "
  "<lj user=" str ">")


(define-skeleton jpw-lj-cut
  "A LiveJournal \"cut\" tag."
  "Cut label: "
  "<lj-cut text=\"" str "\">" _ "</lj-cut>")


(define-skeleton jpw-lj-raw
  "A LiveJournal \"raw\" tag."
  "<lj-raw>" _ "</lj-raw>")


(define-skeleton jpw-lj-poll
  "A LiveJournal \"poll\" tag."
  "<lj-poll>" _ "</lj-poll>")


(define-skeleton jpw-lj-music-hdr
  "The LiveJournal music header."
  "Current Music: "
  (if (bolp) nil ?\n)
  "lj-music: " str ?\n)


(define-skeleton jpw-lj-security-hdr
  "The LiveJournal security header."
  (completing-read "Security or FriendGroup: " jpw-lj-security-alist)
  (if (bolp) nil ?\n)
  "lj-security: " str \n)


(define-skeleton jpw-lj-mood-hdr
  "The LiveJournal mood header."
  (completing-read "Current Mood: " jpw-lj-mood-alist nil t)
  (if (bolp) nil ?\n)
  "lj-mood: " str \n)


(define-skeleton jpw-lj-comments-hdr
  "The LiveJournal comments header."
  (completing-read "Screen These Comments: " jpw-lj-comments-alist nil t)
  (if (bolp) nil ?\n)
  "lj-comments: " str \n)


(define-skeleton jpw-lj-avatar-hdr
  "The LiveJournal avatar header."
  (completing-read "Avatar: " jpw-lj-avatar-alist)
  (if (bolp) nil ?\n)
  "lj-userpic: " str \n)


;; Redefinition of some of the html-mode skeleton functions, to ensure that
;; that they use HTML v4.0 / XHTML syntax.


(define-skeleton html-line
  "XHTML line break tag."
  nil
  "<br/>" \n)

(define-skeleton html-ordered-list
  "XHTML ordered list tags."
  nil
  "<ol>" \n
  "<li>" _ "</li>" \n
  "</ol>")

(define-skeleton html-unordered-list
  "XHTML unordered list tags."
  nil
  "<ul>" \n
  "<li>" _ "</li>" \n
  "</ul>")

(define-skeleton html-list-item
  "XHTML list item tag."
  nil
  (if (bolp) nil '\n)
  "<li>" _  "</li>")

(define-skeleton html-paragraph
  "XHTML paragraph tag."
  nil
  (if (bolp) nil ?\n)
  \n "<p>" _ "</p>")

(define-skeleton html-horizontal-rule
  "XHTML horizontal rule tag."
  nil
  "<hr/>" \n)


;; Other functions.


(defun jpw-lj-insert-emphasized (&optional type)
  "Insert HTML italics tags, or puts the active region inside HTML italics
tags.
Actually, it uses the logical tag \"<em>\", unless called with an arg.
{jpw: 03/2006}"
  (interactive "P")
  ;; Clear the prefix arg so it doesn't screw up the behavior of the
  ;; `skeleton-insert' call.
  (if type (setq prefix-arg nil
                 current-prefix-arg nil))
  (if (null type)
      (jpw-html-emphasized)
    (jpw-html-italic)
    )
  )


(defun jpw-lj-insert-strong (&optional type)
  "Insert HTML bold tags, or puts the active region inside HTML bold
tags.
Actually, it uses the logical tag \"<strong>\", unless called with an arg.
{jpw: 03/2006}"
  (interactive "P")
  ;; Clear the prefix arg so it doesn't screw up the behavior of the
  ;; `skeleton-insert' call.
  (if type (setq prefix-arg nil
                 current-prefix-arg nil))
  (if (null type)
      (jpw-html-strong)
    (jpw-html-bold)
    )
  )


(defun jpw-lj-insert-size ()
  (interactive)
  (funcall jpw-lj-default-html-size-function))


(defun jpw-lj-unfill-paragraph ()
  "Unfills a paragraph using `jpw-unfill-paragraph-engine'.

Any line beginning with a '<' character will no be joined to the preceding
line.  This is to permit you break the line at a markup tag without having to
stick in an entire blank line.
{jpw: 03/2006}"
  (interactive)
  (jpw-unfill-paragraph-engine jpw-lj-unfill-removes-blank-line
                               'jpw-lj-unfill-skip-line)
  (if (looking-at "\\s ")
      (re-search-forward "\\S " nil 't)
    )
  )


(defsubst jpw-lj-unfill-buffer ()
  "Unfills the entire buffer, using `jpw-unfill-buffer-engine'.

As for `jpw-lj-unfill-paragraph', any line beginning with a '<' character will
no be joined to the preceding line.  This is to permit you break the line at a
markup tag without having to stick in an entire blank line.
Furthermore, any block of headers at the top of the buffer are ignored.
`jpw-lj-unfill-buffer' considers any line beginning with word characters
followed by a ':' as a header.
{jpw: 03/2006}"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; Skip over any headers at the top of the buffer.
      (while (re-search-forward "^\\(lj-\\)?\\(\\sw+\\): " nil t))
      (re-search-forward "\\S " nil t)
      (narrow-to-region (point) (point-max))
      (jpw-unfill-buffer-engine jpw-lj-unfill-removes-blank-line
                                'jpw-lj-unfill-skip-line)
      );; end restriction
    );; end excursion
  ;; Return 'nil to make this fn usable with the various `*-write-*-hooks'.
  nil
  )


(defun jpw-lj-init-customizations ()
  "Initialize internal variables from customizations.

Call this function after changing certain customization variables manually
\\=\\[i.e. outside of `custom-mode'\\=\\]
{jpw: 03/2006}"
  (interactive)
  (funcall 'jpw-lj-custom-set-var
           'jpw-lj-security-alist jpw-lj-friend-groups)
  (funcall 'jpw-lj-custom-set-var
           'jpw-lj-avatar-alist jpw-lj-user-avatars)
  )


;;------------------------------------------------------------
;;
;; Bindings: Define Local Keymap
;;


(defvar jpw-lj-mode-map nil)
(if (null jpw-lj-mode-map)
    (progn
      (setq jpw-lj-mode-map (make-sparse-keymap))
      (define-key jpw-lj-mode-map "\M-\"" 'jpw-lj-unfill-paragraph)
      (define-key jpw-lj-mode-map "\C-c\"" 'jpw-lj-unfill-buffer)

      (define-key jpw-lj-mode-map "\M-oi" 'jpw-html-italic)
      (define-key jpw-lj-mode-map "\M-oe" 'jpw-html-emphasized)
      (define-key jpw-lj-mode-map "\C-ci" 'jpw-lj-insert-italic)

      (define-key jpw-lj-mode-map "\M-ob" 'jpw-html-bold)
      (define-key jpw-lj-mode-map "\M-os" 'jpw-html-strong)
      (define-key jpw-lj-mode-map "\C-cb" 'jpw-lj-insert-bold)

      (define-key jpw-lj-mode-map "\M-ou" 'jpw-html-underline)

      (define-key jpw-lj-mode-map "\M-oo" 'jpw-html-code)
      (define-key jpw-lj-mode-map "\M-oc" 'jpw-html-code)
      (define-key jpw-lj-mode-map "\M-ot" 'tempo-template-html-fixed)
      (define-key jpw-lj-mode-map "\M-of" 'tempo-template-html-fixed)

      (define-key jpw-lj-mode-map "\M-od" 'jpw-html-del)

      (define-key jpw-lj-mode-map "\M-o\C-s" 'jpw-html-size-small)
      (define-key jpw-lj-mode-map "\M-o\C-b" 'jpw-html-size-big)
      (define-key jpw-lj-mode-map "\M-o\C-r" 'jpw-html-size-relative)
      (define-key jpw-lj-mode-map "\M-oz" 'jpw-lj-insert-size)
      (define-key jpw-lj-mode-map "\M-o\M-s" 'jpw-lj-insert-size)

      (define-key jpw-lj-mode-map "\M-p\C-i" 'html-image)

      (define-key jpw-lj-mode-map "\M-pa" 'jpw-html-href-anchor)
      (define-key jpw-lj-mode-map "\M-p\C-u" 'jpw-html-href-anchor)

      (define-key jpw-lj-mode-map "\M-pl" 'jpw-html-insert-list)

      (define-key jpw-lj-mode-map "\M-p*" 'html-list-item)
      (define-key jpw-lj-mode-map "\M-p." 'html-list-item)

      (define-key jpw-lj-mode-map "\C-cp" 'html-paragraph)
      (define-key jpw-lj-mode-map "\M-p\C-m" 'html-paragraph)

      (define-key jpw-lj-mode-map "\M-p\C-j" 'html-line)

      (define-key jpw-lj-mode-map "\M-p-" 'html-horizontal-rule)

      (define-key jpw-lj-mode-map "\M-pc" 'tempo-template-html-preformatted)
      (define-key jpw-lj-mode-map "\M-pq" 'tempo-template-html-blockquote)

      ;; Entity expansion
      (define-key jpw-lj-mode-map "\C-c'" 'jpw-html-entity-abbrev-expand)
      (define-key
        jpw-lj-mode-map [?\C-c ?\C-'] 'jpw-html-entity-abbrev-expand)

      ;; LJ-specific commands
      (define-key jpw-lj-mode-map "\M-pu" 'jpw-lj-user)

      (define-key jpw-lj-mode-map "\M-p=" 'jpw-lj-cut)

      (define-key jpw-lj-mode-map "\M-p\C-p" 'jpw-lj-poll)

      (define-key jpw-lj-mode-map "\M-pr" 'jpw-lj-raw)

      (define-key jpw-lj-mode-map "\C-cha" 'jpw-lj-avatar-hdr)
      (define-key jpw-lj-mode-map "\C-cht" 'jpw-lj-music-hdr)
      (define-key jpw-lj-mode-map "\C-chm" 'jpw-lj-mood-hdr)
      (define-key jpw-lj-mode-map "\C-chs" 'jpw-lj-security-hdr)
      (define-key jpw-lj-mode-map "\C-chc" 'jpw-lj-comments-hdr)
      )                                 ;end progn
  )                                     ;end if
(global-set-key "\M-p\M-j" 'jpw-lj-mode)


;;------------------------------------------------------------
;;
;; Font-Lock Support
;;


;;
;; Individual `font-lock-keyword' lists.  Each value is of the same form as an
;; element of `font-lock-keywords'; see that variable's documentation for more
;; info.
;;


(defconst jpw-lj-font-lock-strong-face-key-1
  ;; See jpw-lj-font-lock-underline-face-key-1 for info
  (list
   (concat
    "<strong>"
    "\\("
    "[^<]*\\(<[^/]+/[^s][^>]*>[^<]*\\)*"
    "\\)"
    "</strong>"
    ) ;;end concat
   '(1 'jpw-lj-bold-face append)
   ) ;;end list
  ) ;;end defconst


(defconst jpw-lj-font-lock-em-face-key-1
  ;; See jpw-lj-font-lock-underline-face-key-1 for info
  (list
   (concat
    "<em>"
    "\\("
    "[^<]*\\(<[^/]+/[^e][^>]*>[^<]*\\)*"
    "\\)"
    "</em>"
    ) ;;end concat
   '(1 'jpw-lj-italic-face append)
   ) ;;end list
  ) ;;end defconst


(defconst jpw-lj-font-lock-bold-face-key-1
  ;; See jpw-lj-font-lock-underline-face-key-1 for info
  (list
   (concat
    "<b>"
    "\\("
    "[^<]*\\(<[^/]+/[^b][^>]*>[^<]*\\)*"
    "\\)"
    "</b>"
    ) ;;end concat
   '(1 'jpw-lj-bold-face append)
   ) ;;end list
  ) ;;end defconst


(defconst jpw-lj-font-lock-italics-face-key-1
  ;; See jpw-lj-font-lock-underline-face-key-1 for info
  (list
   (concat
    "<i>"
    "\\("
    "[^<]*\\(<[^/]+/[^i][^>]*>[^<]*\\)*"
    "\\)"
    "</i>"
    ) ;;end concat
   '(1 'jpw-lj-italic-face append)
   ) ;;end list
  ) ;;end defconst


;; Bold, italic, and underline all use a regexp designed to fontify nested
;; tags.  As a side-benefit, the regexp also matches multiline tag content.
(defconst jpw-lj-font-lock-underline-face-key-1
  (list
   (concat
    "<u>"
    "\\("
    "[^<]*\\(<[^/]+/[^u][^>]*>[^<]*\\)*"
    "\\)"
    "</u>"
    ) ;;end concat
   '(1 'jpw-lj-underline-face append)
   ) ;;end list
  ) ;;end defconst


;; Regexp for LiveJournal headers
(defconst jpw-lj-font-lock-header-face-key-2
  (list
   (concat
    "\\(lj-\\sw+:\\)\\s +\\(.*\\)$"
    ) ;;end concat
   '(1 'jpw-lj-header-face append)
   '(2 'font-lock-type-face append)
   ) ;;end list
  ) ;;end defconst


;; Regexp for nested size tags
;; Must prepend to keep the default `html-mode' fontification from overriding
;; this one.
(defconst jpw-lj-font-lock-small-face-key-3
  (list
   (concat
    "<small>"
    "\\([^<]*\\(<[^/]+/[^sb][^>]*>[^<]*\\)*\\)"
    "</small>"
    ) ;;end concat
   '(1 'jpw-lj-size-face prepend)
   ) ;;end list
  ) ;;end defconst


;; Regexp for nested size tags
;; Must prepend to keep the default `html-mode' fontification from overriding
;; this one.
(defconst jpw-lj-font-lock-big-face-key-3
  (list
   (concat
    "<big>"
    "\\([^<]*\\(<[^/]+/[^sb][^>]*>[^<]*\\)*\\)"
    "</big>"
    ) ;;end concat
   '(1 'jpw-lj-size-face prepend)
   ) ;;end list
  ) ;;end defconst


;; Regexp for nested size tags
(defconst jpw-lj-font-lock-size-face-key-3
  (list
   (concat
     "<span style=\"font-size: [^>]+>"
    "\\([^<]*\\(<[^/]+/[^s][^>]*>[^<]*\\)*\\)"
    "</span>"
    ) ;;end concat
   '(1 'jpw-lj-size-face append)
   ) ;;end list
  ) ;;end defconst


;; The Logical style "code" ... which is usually equivalent to <tt> ... </tt>.
(defconst jpw-lj-font-lock-code-face-key-3
  (list
   (concat
    "<code>"
    "\\([^<]*\\(<[^/]+/[^c][^>]*>[^<]*\\)*\\)"
    "</code>"
    ) ;;end concat
   '(1 'jpw-lj-code-face t)
   ) ;;end list
  ) ;;end defconst


(defconst jpw-lj-font-lock-del-face-key-3
  (list
   (concat
    "<del>"
    "\\([^<]*\\(<[^/]+/[^d][^>]*>[^<]*\\)*\\)"
    "</del>"
    ) ;;end concat
   '(1 'jpw-lj-del-face t)
   ) ;;end list
  ) ;;end defconst


;;
;; Variables grouping each font-lock keyword list by font-lock level.
;;


(defconst jpw-lj-font-lock-keywords-1
  (append sgml-font-lock-keywords
          (list jpw-lj-font-lock-strong-face-key-1
                jpw-lj-font-lock-em-face-key-1
                jpw-lj-font-lock-bold-face-key-1
                jpw-lj-font-lock-italics-face-key-1
                jpw-lj-font-lock-underline-face-key-1
                ) ;;end list
          )
  )


(defconst jpw-lj-font-lock-keywords-2
  (append jpw-lj-font-lock-keywords-1
          (list jpw-lj-font-lock-header-face-key-2
                ) ;;end list
          ) ;;end append
  )

(defconst jpw-lj-font-lock-keywords-3
  (append jpw-lj-font-lock-keywords-2
          (list jpw-lj-font-lock-code-face-key-3
                jpw-lj-font-lock-del-face-key-3
                jpw-lj-font-lock-size-face-key-3
                jpw-lj-font-lock-small-face-key-3
                jpw-lj-font-lock-big-face-key-3
                ) ;;end list
          ) ;;end append
  )

(defvar jpw-lj-font-lock-keywords jpw-lj-font-lock-keywords-3
  "The actual set of keywords passed to `font-lock' by `jpw-lj-mode' and
`jpw-lj-minor-mode'.
{jpw: 03/2006")


(defconst jpw-lj-font-lock-defaults-keywords
  '(jpw-lj-font-lock-keywords-1 jpw-lj-font-lock-keywords-2
                                jpw-lj-font-lock-keywords-3)
  "The list of keyword variables used in jpw-lj-font-lock-defaults.
Each element of this list is a variable of the same form as
`font-lock-keywords'.  The separate elements of the list control the
fontification at different levels.
This is a variable, and not a constant, so that it can be made buffer-local
and be reused.
{jpw: 10/2007}")


(defconst jpw-lj-font-lock-defaults
  (list
   jpw-lj-font-lock-defaults-keywords
   nil t nil nil
   '(font-lock-syntactic-keywords . sgml-font-lock-syntactic-keywords)
   ) ;;end list
  )


;;------------------------------------------------------------
;;
;; Define the mode proper
;;


(defsubst jpw-lj-select-font-lock-keywords-by-level (the-font-lock-defaults)
  "Select a font-lock leve as determined by `font-lock-maximum-decoration'.
THE-FONT-LOCK-DEFAULTS should be the value of `font-lock-defaults' or a
similarly structured variable.
{jpw: 03/2006}"
  (let* ((jpw-lj-font-lock-level-list (car the-font-lock-defaults))
         (my-deco1 (if (listp font-lock-maximum-decoration)
                       (or (assq 'jpw-lj-mode font-lock-maximum-decoration)
                           (assq 'jpw-lj-minor-mode
                                 font-lock-maximum-decoration)
                           (assq 't font-lock-maximum-decoration)
                           )
                     ;; else
                     font-lock-maximum-decoration
                     );; end if
                   )
         (my-deco2 (if (listp my-deco1) (cdr my-deco1) my-deco1))
         (my-level (1- (cond
                        ((numberp my-deco2) (max 1 my-deco2))
                        (my-deco2 1000)
                        (t 1)
                        )))
         );; end varbinds
    (if (listp jpw-lj-font-lock-level-list)
        (eval
         (nth (min my-level (1- (length jpw-lj-font-lock-level-list)))
              jpw-lj-font-lock-level-list))
      ;; else
      jpw-lj-font-lock-level-list
      )
    );; end let
  )


(defun jpw-lj-mode-common ()
  ;; Set up font-lock for jpw-lj-mode
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-multiline)
  (make-local-variable 'font-lock-support-mode)
  (make-local-variable 'lazy-lock-minimum-size)
  (make-local-variable 'lazy-lock-defer-on-the-fly)
  (make-local-variable 'lazy-lock-defer-on-scrolling)
  (make-local-variable 'lazy-lock-defer-contextually)
  (make-local-variable 'lazy-lock-stealth-time)
  (make-local-variable 'lazy-lock-stealth-load)
  (make-local-variable 'lazy-lock-stealth-nice)
  (make-local-variable 'lazy-lock-stealth-verbose)
  ;; Changing `font-lock-defaults' won't change the inherited
  ;; font-locking-behavior.
  (setq font-lock-multiline t
        ;; `jit-lock-mode' doesn't correctly fontify everything we want it to.
        ;;
        ;; `fast-lock-mode' and `lazy-lock-mode' require you to
        ;; manually-refontify when editing text in some of the more complex
        ;; multiline expressions.  `fast-lock-mode' works by keeping a cache
        ;; of fontifications.  Great for programming modes.  Bad for modes
        ;; operating on temporary buffers.
        ;;
        ;; So, we'll use `lazy-lock-mode', suitably tuned to behave as
        ;; desired.
        font-lock-support-mode 'lazy-lock-mode
        lazy-lock-minimum-size nil
        lazy-lock-stealth-verbose t
        lazy-lock-defer-on-the-fly t
        lazy-lock-defer-on-scrolling nil
        lazy-lock-defer-contextually nil
        lazy-lock-stealth-time 3
        lazy-lock-stealth-nice 0.1
        lazy-lock-stealth-load nil)

  (if font-lock-mode
      (font-lock-fontify-buffer))

  ;;
  ;; Things needed to circumvent behavior inherited by HTML mode.
  ;;

  ;; Override the skeleton-mode hooks
  (make-local-variable 'skeleton-end-newline)
  (setq skeleton-end-newline nil)

  ;;
  ;; Behavior specific to LJ mode.
  ;;

  (if jpw-lj-unfill-on-save
      (add-hook 'local-write-file-hooks 'jpw-lj-unfill-buffer)
      )
  (if jpw-lj-xlate-entities-on-save
      (add-hook 'local-write-file-hooks 'jwz-lj-entify)
      )
  )



;;;###autoload (jpw-lj-mode)
(define-derived-mode jpw-lj-mode html-mode "jpw-lj"
  "A major mode for editing LiveJournal messages.  Derived from `html-mode'.

Key bindings:
\\{jpw-lj-mode-map}

{jpw: 03/2006}"
  ;; Must call this outside of `jpw-lj-mode-common', due to
  ;; different call-order in the major and minor modes.
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-defaults jpw-lj-font-lock-defaults)
  (jpw-lj-mode-common)
  )



;;;###autoload (jpw-lj-mode)
(define-minor-mode jpw-lj-minor-mode
  "A minor mode for editing LiveJournal messages.

Key bindings:
\\{jpw-lj-mode-map}

{jpw: 03/2006}"
  nil " jpw-LJ" jpw-lj-mode-map

  (if jpw-lj-minor-mode
      ;; We're turning this minor mode ON.
      (progn
        ;; Must call this outside of `jpw-lj-mode-common', due to
        ;; different call-order in the major and minor modes.
        ;; Cache the original font-lock keywords
        (make-local-variable 'font-lock-defaults)
        (make-local-variable 'jpw-lj-font-lock-keywords)

        ;; Cache the original font-lock keywords
        (setq jpw-lj-minor-mode-original-font-lock-defaults
              font-lock-defaults)

        ;; Set `jpw-lj-font-lock-keywords' according to the desired level.
        (setq jpw-lj-font-lock-keywords
              (jpw-lj-select-font-lock-keywords-by-level
               jpw-lj-font-lock-defaults))
        ;; Merge
        (let* ((old-keywords
                (jpw-lj-select-font-lock-keywords-by-level font-lock-defaults))
               ) ;; end bindings
          (if (symbolp old-keywords)
              (setq old-keywords (eval old-keywords))
            )
          (setq jpw-lj-font-lock-keywords
                (append (if (symbolp jpw-lj-font-lock-keywords)
                            (eval jpw-lj-font-lock-keywords)
                          jpw-lj-font-lock-keywords)
                        old-keywords))
          (setcar font-lock-defaults 'jpw-lj-font-lock-keywords)
          );; end let*

        ;; **Now** do the common tasks.
        (jpw-lj-mode-common)
        );; end progn

    ;;else
    ;; We're disabling this minor mode.
    (and jpw-lj-minor-mode-original-font-lock-defaults
         font-lock-mode
         (setq font-lock-defaults
               jpw-lj-minor-mode-original-font-lock-defaults)
         );;end and
    (setq jpw-lj-minor-mode-original-font-lock-defaults nil)
    (if jpw-lj-unfill-on-save
        (remove-hook 'local-write-file-hooks 'jpw-lj-unfill-buffer)
      )
    (if jpw-lj-xlate-entities-on-save
        (remove-hook 'local-write-file-hooks 'jwz-lj-entify)
      )
    );;end if (outer)

  ;; Whether turning the minor mode on or off, refontify the buffer (if
  ;; needed).
  (if font-lock-mode
      (font-lock-fontify-buffer))
  )


;;------------------------------------------------------------
;;
;; Unit Tests
;;


;;...


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
