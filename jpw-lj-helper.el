;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A major mode for editing LiveJournal entries.
;;
;;  Copyright © 2005 John P. Weiss
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
;; You can enter this mode using `\M-p\M-j'.
;;
;;  
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'sgml-mode)
(require 'custom-defuns)
(eval-when-compile
  (require 'lazy-lock)
  )


;;------------------------------------------------------------
;;
;; User Customizations
;; 


(defgroup jpw-lj nil
  "Customizable aspects of jpw-lj-mode."
  :group 'hypermedia
  :group 'local)


(defcustom jpw-lj-default-html-size-function 'jpw-html-size-relative
  "Name of the default type of HTML tag to use for adjusting font size.

 The three possible values are `jpw-html-size-relative', `jpw-html-size-small'
and `jpw-html-size-big'.  The former uses a \"<span\"> tag with a font sizing
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
  "The face to use for the <del> ... </del> HTML markup (a logical style that
replaces the old, deprecated \"<strike>\" tag."
  :group 'jpw-lj)

(defface jpw-lj-url-face 
  '((t (:inherit font-lock-keyword-face
                 :underline t :bold t)))
  "The face to use for URLs."
  :group 'jpw-lj)


;;------------------------------------------------------------
;;
;; Variables
;; 


(defconst jpw-html-size-alist
  '(("xx-small") ("x-small") ("small") ("smaller") ("medium") ("large")
    ("larger") ("x-large") ("xx-large"))
  "Alist of LiveJournal security keywords.
{jpw: 03/06}")


(defconst jpw-lj-security-alist
  '(("public") ("private") ("friends"))
  "Alist of LiveJournal security keywords.
{jpw: 03/06}")


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
{jpw: 03/06}")


(defconst jpw-lj-comments-alist 
  '(("N" . "Don't screen comments.") 
    ("R" . "Screen anonymous comments only") 
    ("F" . "Screen comments made by non-friends")
    ("A" . "Screen *All* comments")) 
 "Alist of LiveJournal comment-screening flags, along with text descriptions
of each flag.
{jpw: 03/06}")


(defconst jpw-lj-avatar-alist ()
  "Alist of user avatars.
{jpw: 03/06}")


;;------------------------------------------------------------
;;
;; Utility Functions
;; 


;...


;;------------------------------------------------------------
;;
;; Mode Interactive Functions
;; 


;; Skeleton Templates


(define-skeleton jpw-html-italic
  "Insert HTML [physical] italics tags, or puts the active region inside HTML
italics tags.
{jpw: 03/06}"
  nil
  "<i>" _ "</i>")

(define-skeleton jpw-html-bold
  "Insert HTML [physical] bold tags, or puts the active region inside HTML
bold tags.
{jpw: 03/06}"
  nil
  "<b>" _ "</b>")

(define-skeleton jpw-html-underline
  "Insert HTML [physical] underline tags, or puts the active region inside
HTML underline tags.
{jpw: 03/06}"
  nil
  "<u>" _ "</u>")

(define-skeleton jpw-html-emphasized
  "Insert HTML [logical] emphasized tags, or puts the active region inside
HTML emphasized tags.
{jpw: 03/06}"
  nil
  "<em>" _ "</em>")

(define-skeleton jpw-html-strong
  "Insert HTML [logical] strong tags, or puts the active region inside HTML
strong tags.
{jpw: 03/06}"
  nil
  "<strong>" _ "</strong>")

(define-skeleton jpw-html-del
  "Insert HTML [logical] \"<del>\" tags, or puts the active region inside HTML
strong tags.
{jpw: 03/06}"
  nil
  "<del>" _ "</del>")

(define-skeleton jpw-html-code
  "Insert HTML code tags, or puts the active region inside HTML code
tags.
{jpw: 03/06}"
  nil
  "<code>" _ "</code>")


(define-skeleton jpw-html-href-anchor
  "HTML anchor tag with href attribute.
Like the sgml-mode version, but without the annoying \"http:\" defaulting into
the URL prompt.
{jpw: 03/06}"
  "URL: "
  "<a href=\"" str "\">" _ "</a>")


(define-skeleton jpw-html-size-smallbig
  "Insert HTML font resizing tags \"<small>\" or \"<big>\", depending on the
sign of the change arg.
{jpw: 03/06}"
  "Relative size change: "
;;  (setq input 0)
  "<" (if (> str 0) "small" "big") ">"
  _ 
  "</" (if (> str 0) "small" "big") ">"
  )


(define-skeleton jpw-html-size-relative
  "Insert HTML font resizing markup
{jpw: 03/06}"
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
  "<br />" \n)

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
  "<hr />" \n)


;; Other functions.


(defun jpw-lj-insert-emphasized (&optional type)
  "Insert HTML italics tags, or puts the active region inside HTML italics
tags.
Actually, it uses the logical tag \"<em>\", unless called with an arg.
{jpw: 03/06}"
  (interactive "P")
  (if (null type)
      (jpw-html-emphasized)
    (jpw-html-italic)
    )
  )


(defun jpw-lj-insert-strong (&optional type)
  "Insert HTML bold tags, or puts the active region inside HTML bold
tags.
Actually, it uses the logical tag \"<strong>\", unless called with an arg.
{jpw: 03/06}"
  (interactive "P")
  (if (null type)
      (jpw-html-strong)
    (jpw-html-bold)
    )
  )


(defun jpw-lj-insert-list (&optional type)
  "Insert HTML list tags, or puts the active region inside HTML list
tags.
The optional `type' specifies the type of list.  It can be passed directly or
specified using a prefix-arg.  If `type' is an integer (e.g. a prefix-arg),
then the list will be an ordered list.  Otherwise, the list is unordered.
Any other type is an error.
{jpw: 03/06}"
  (interactive "P")
  ;; Validation check.
  (or (null type)
      (char-or-string-p type)
      (signal 'wrong-type-argument 
              (list 'char-or-string-p type)))
  (if (or (null type)
          (stringp type))
      (html-unordered-list)
    (html-ordered-list)
    )
  )


(define-skeleton jpw-html-size-small
  "Insert HTML font resizing tag \"<small>\".
{jpw: 03/06}"
  nil
  "<small>" _  "</small>"
  )


(define-skeleton jpw-html-size-big
  "Insert HTML font resizing tag \"<big>\".
{jpw: 03/06}"
  nil
  "<big>" _  "</big>"
  )


(defun jpw-lj-insert-size () 
  (interactive) 
  (funcall jpw-lj-default-html-size-function))


(defsubst jpw-lj-unfill-buffer ()
  (interactive)
  (jpw-unfill-buffer t)
  )


(defsubst jpw-lj-unfill-paragraph ()
  (interactive)
  (jpw-unfill-paragraph t)
  (if (looking-at "\\s ")
      (re-search-forward "\\S " nil 't)
    )
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

      (define-key jpw-lj-mode-map "\M-gi" 'jpw-html-italic)
      (define-key jpw-lj-mode-map "\M-ge" 'jpw-html-emphasized)
      (define-key jpw-lj-mode-map "\C-ci" 'jpw-lj-insert-italic)

      (define-key jpw-lj-mode-map "\M-gb" 'jpw-html-bold)
      (define-key jpw-lj-mode-map "\M-gs" 'jpw-html-strong)
      (define-key jpw-lj-mode-map "\C-cb" 'jpw-lj-insert-bold)

      (define-key jpw-lj-mode-map "\M-gu" 'jpw-html-underline)

      (define-key jpw-lj-mode-map "\M-go" 'jpw-html-code)

      (define-key jpw-lj-mode-map "\M-gd" 'jpw-html-del)

      (define-key jpw-lj-mode-map "\M-g\C-s" 'jpw-html-size-small)
      (define-key jpw-lj-mode-map "\M-g\C-b" 'jpw-html-size-big)
      (define-key jpw-lj-mode-map "\M-g\C-r" 'jpw-html-size-relative)
      (define-key jpw-lj-mode-map "\M-gz" 'jpw-lj-insert-size)
      (define-key jpw-lj-mode-map "\M-g\M-s" 'jpw-lj-insert-size)

      (define-key jpw-lj-mode-map "\M-p\C-i" 'html-image)

      (define-key jpw-lj-mode-map "\M-pa" 'jpw-html-href-anchor)
      (define-key jpw-lj-mode-map "\M-p\C-u" 'jpw-html-href-anchor)

      (define-key jpw-lj-mode-map "\M-pl" 'jpw-lj-insert-list)

      (define-key jpw-lj-mode-map "\M-p*" 'html-list-item)
      (define-key jpw-lj-mode-map "\M-p." 'html-list-item)

      (define-key jpw-lj-mode-map "\C-cp" 'html-paragraph)
      (define-key jpw-lj-mode-map "\M-p\C-m" 'html-paragraph)

      (define-key jpw-lj-mode-map "\M-p\C-j" 'html-line)

      (define-key jpw-lj-mode-map "\M-p-" 'html-horizontal-rule)

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
    "\\(lj-\\sw+:\\) \\(.*\\)$"
    ) ;;end concat
   '(1 'jpw-lj-header-face)
   '(2 'font-lock-type-face)
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
   (list jpw-lj-font-lock-strong-face-key-1
         jpw-lj-font-lock-em-face-key-1
         jpw-lj-font-lock-bold-face-key-1
         jpw-lj-font-lock-italics-face-key-1
         jpw-lj-font-lock-underline-face-key-1
         ) ;;end list
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

(defconst jpw-lj-font-lock-keywords jpw-lj-font-lock-keywords-3)


;; The actual value of `font-lock-defaults' for jpw-lj-mode
;;

(defconst jpw-lj-font-lock-defaults
  (list
   ;; Per-level variable names.  Each variable named in this list should be of
   ;; the same form as `font-lock-keywords'
   '(jpw-lj-font-lock-keywords-1 jpw-lj-font-lock-keywords-2
                                 jpw-lj-font-lock-keywords-3)
   nil t
   ) ;;end list
  )


;;------------------------------------------------------------
;;
;; Define the mode proper
;; 



;;;###autoload (jpw-lj-mode)
(define-derived-mode jpw-lj-mode html-mode "jpw-lj"
  "A major mode for editing LiveJournal messages.  Derived from `html-mode'.

Key bindings:
\\{jpw-lj-mode-map}

{jpw: 03/06}"
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
  (setq font-lock-defaults jpw-lj-font-lock-defaults
        font-lock-multiline t
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
        font-lock-support-mode lazy-lock-mode
        lazy-lock-minimum-size nil
        lazy-lock-stealth-verbose t
        lazy-lock-defer-on-the-fly t
        lazy-lock-defer-on-scrolling nil
        lazy-lock-defer-contextually nil
        lazy-lock-stealth-time 3
        lazy-lock-stealth-nice 0.1
        lazy-lock-stealth-load nil)

  ;; FIXME:
  ;;
  ;; Things needed to circumvent behavior inherited by HTML mode.
  ;; 

  ;; Override the default HTML-mode font-lock behavior 
  (setq font-lock-keywords (append font-lock-keywords 
                                   jpw-lj-font-lock-keywords-3))
  (if font-lock-mode
      (font-lock-fontify-buffer))

  ;; TODO:
  ;; Eventually, this will be moved into an if-statement controlled by a
  ;; customization flag.  That flag will also need to perform an unfill-buffer
  ;; before saving/killing.  Will need to build in that functionality, as
  ;; well. 
  (progn (setq fill-column 0) (auto-fill-mode -1))
  )

(global-set-key "\M-p\M-j" 'jpw-lj-mode)


;;------------------------------------------------------------
;;
;; Unit Tests
;; 


;; (defun jpw-lj-test-tag-find-last ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-lj-tag-find-last))
;;             (starttag (car tagspec))
;;             (endtag (cadr tagspec)) )
;;       (cond 
;;        (starttag (goto-char starttag))
;;        (endtag (goto-char endtag))
;;        ))))
;; (defun jpw-lj-test-tag-find-next ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-lj-tag-find-next))
;;             (starttag (car tagspec))
;;             (endtag (cadr tagspec)) )
;;       (cond 
;;        (starttag (goto-char starttag))
;;        (endtag (goto-char endtag))
;;        ))))
;; (defun jpw-lj-test-tag-find-nearest ()
;;   (interactive)
;;   (message "%S" (jpw-lj-tag-find-nearest)))
;; (defun jpw-lj-test-get-tag-last ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-lj-tag-find-last))
;;             (tagval (jpw-lj-get-tag-at tagspec))
;;             )
;;       (message tagval))))
;; (defun jpw-lj-test-get-tag-next ()
;;   (interactive)
;;   (save-excursion
;;     (let* ( (tagspec (jpw-lj-tag-find-next))
;;             (tagval (jpw-lj-get-tag-at tagspec))
;;             )
;;       (message tagval))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
