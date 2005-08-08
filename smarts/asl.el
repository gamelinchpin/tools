;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  asl.el -- Major mode for editing SMARTS ASL
;;
;;  Copyright © 1999-2000 John P. Weiss
;;  Donated to System Management Arts under the Artistic License
;;
;;  
;;  Uses "cc-mode.el", the standard code for editing C, C++,
;;  Objective-C, and Java, as its basis.
;;
;;  Uses:  variable `c-offsets-alist'
;;  Attempts to use `c-initialize-cc-mode'
;;  
;;  If future releases of `cc-mode.el' change these functions and
;;  vars, someone may need to alter this file accordingly.
;;
;;  last modified 3/31/2000 (jpw)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;; Load this at the beginning of the file, since there are many
;; statements below that depend on the existence of c++-mode
;; variables. 
(require 'cc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ASL Mode support functions and variables


(if (fboundp 'defgroup)
    (progn
      ;; GNU Emacs customization support.
      (defgroup SMARTS nil
        "Customizable aspects of the SMARTS-specific programming modes."
        :group 'emacs
        )


      (defgroup SMARTS-Indentation nil
        "NOTE:  Read carefully!  Indentation in SMARTS-programming modes.

You probably don't want to change the values of variables in this
group using your `.emacs' file (or the emacs `Help->Customize' menu).
More often than not, a special comment-block at the end of a file will
set the variables in this group.  That special comment-block looks
something like this:

/*
 * These variables describe the formatting of this file.  If you don't like the
 * template defaults, feel free to change them here (not in your .emacs file).
 *
 * Local Variables:
 * mode: SMARTS-FOO-MODE
 * comment-column: 32
 * c-basic-offset: 4
 * c-indent-level: 4
 * foo-mode-twiggle-indent: 2
 * foo-mode-widgie-offset: 5
 * foo-mode-default-statement-offset: 4
 * fill-column: 79
 * End:
 */

The advantage of this special comment-block is that the file will have
a consistent style, regardless who is editing it.  You are better off
setting indentation-controlling variables here than in your `.emacs'
file."
        :group 'SMARTS
        )


      ;; Define a hook variable for this mode.  Make it emacs-customizable.
      (defcustom asl-mode-hook nil
        "*Hook called by `asl-mode'."
        :type 'hook
        :group 'SMARTS)


      ;; This is here for Boaz.  Shut off the asl-rule-face for XEmacs
      ;; users, but enable for GNU-Emacs.
      (defcustom asl-use-rule-face t
        "Enable/disable use of the separate asl-rule-face.")


      (defcustom asl-statement-cont-offset 4
        "Indentation for a continuation of an action statement.  Must
evaluate to an integer value.

Example:

You might expect this to be valid:
    (setq asl-statement-cont-offset 3)
but you can also specify the quoted name of a variable:
    (setq asl-statement-cont-offset 'c-basic-offset)
or even a quoted mathematical expression:
    (setq asl-statement-cont-offset '(+ c-basic-offset 2))

<Used in subcases of the `statement-cont' syntax via 
`c-offsets-alist'.>"
        :type 'custom-variable
        :group 'SMARTS-Indentation)


      (defcustom asl-OR-rule-offset 2
        "Indentation for a continuation of a rule preceded by a single `|'.
Must evaluate to an integer value (see `asl-statement-cont-offset').

<Used in subcases of the `statement' syntax via `c-offsets-alist'.>"
        :type 'custom-variable
        :group 'SMARTS-Indentation)

      );; end progn: GNU Emacs
  ;;
  ;; else
  ;;
  (progn
    ;; XEmacs (and older versions of GNU Emacs) does not have
    ;; `defgroup' or `defcustom', so we simply ignore the `defgroup'
    ;; statements and use `defvar' for the user-configurable
    ;; variables.
    ;;

      ;; Define a hook variable for this mode.  Make it emacs-customizable.
      (defvar asl-mode-hook nil
        "*Hook called by `asl-mode'.")


      ;; This is here for Boaz.  Shut off the asl-rule-face for XEmacs
      ;; users.
      (defvar asl-use-rule-face nil
        "Enable/disable use of the separate asl-rule-face.")


      (defvar asl-statement-cont-offset 4
        "Indentation for a continuation of an action statement.  Must
evaluate to an integer value.

Example:

You might expect this to be valid:
    (setq asl-statement-cont-offset 3)
but you can also specify the quoted name of a variable:
    (setq asl-statement-cont-offset 'c-basic-offset)
or even a quoted mathematical expression:
    (setq asl-statement-cont-offset '(+ c-basic-offset 2))

<Used in subcases of the `statement-cont' syntax via 
`c-offsets-alist'.>")


      (defvar asl-OR-rule-offset 2
        "Indentation for a continuation of a rule preceded by a single `|'.
Must evaluate to an integer value (see `asl-statement-cont-offset').

<Used in subcases of the `statement' syntax via `c-offsets-alist'.>")
    );; end progn: XEmacs
  );;end if


;; Regexp that should fail in almost any search in any buffer.  Used
;; to disable certain emacs-cc-mode-engine features.
(defconst c-ASL-bogus-regexp
  "\\n\\t\\r±´¶¸¬¬¸¶´±\\r\\n\\t" )


;; Regexp defining the beginning of an ASL rule.  Note that rule names
;; with lowercase characters are legal, though stylistically
;; discouraged.
;;
;; Note that the regexp ".*" looks for any character, *except* `\n'.  To
;; look for any character, including linebreaks, use [^ ] to look for
;; any character except some seldom-used one.  In this case, I chose
;; Ctrl-Z, due to string arguments with a default value.  It's
;; unlikely one will ever need a default string arg with embedded
;; control character, let alone a Ctrl-Z, which some systems may
;; mistake for an EOF.
;;
;; This is *not* the regexp used for fontifying the beginning of a rule.
(defconst c-ASL-rule-begin-regexp
  (concat
   "^\\([_a-zA-Z][_a-zA-Z0-9]*\\)"    ; Keyword
   "\\("                              ; Optional rule arguments, which
   "([^]*)"                         ; may have default values or be
   "\\)?"                             ; split across multiple lines.
   "\\(\\s +\\(do\\|filter\\)\\)?"    ; Optional 'do' or 'filter'
                                      ; keyword, preceded by at
                                      ; least one whitespace
   "\\s *{"                           ; '{' with optional whitespace
   );end concat
  )


;; Regexp defining the beginning of a do- or filter-block in a rule.
;; Self-explanatory.
(defconst c-ASL-do-or-filter-begin-regexp
  (concat
    "}\\s *"                  ; '}' with optional whitespace
    "\\(do\\|filter\\)"       ; Start of do-block or filter-block
    "\\s *{"                  ; '{' with optional whitespace
   );end concat
  )


;; Language key defining the start of a parsing rule, filter- or do-
;; block.  Currently unused, it remains here for historical purposes.
;; (It was once passed to the emacs-cc-mode-engine variable
;; `c-class-key'.)
(defconst c-ASL-rule-key 
  (concat
   "\\( "c-ASL-rule-begin-regexp          ; Start of a rule
    "\\)\\|\\("
    c-ASL-do-or-filter-begin-regexp      ; Start of do- or filter-block
    "\\)"
   ) ;;end concat
  ) ;;end defconst


;; Language key defining ASL conditional statements.  Passed to the
;; emacs-cc-mode-engine. 
(defconst c-ASL-conditional-key 
  "\\b\\(foreach\\|while\\|if\\|else\\)\\b[^_]" )


;; For old/broker versions of cc-mode
;;
(if (boundp 'c-Java-comment-start-regexp)
    (defvar c-ASL-comment-start-regexp c-Java-comment-start-regexp)
  ;;else
  (defvar c-ASL-comment-start-regexp c-C++-comment-start-regexp))
  

;; Position of the opening brace found by a call to
;; `asl-rule-ending-brace'.  Used by the indentation engine.
(defvar asl-opening-brace-point nil
  "Internal asl-mode variable.  Do not touch.")


;;;###autoload
(defvar asl-mode-syntax-table nil
  "Syntax table used in asl-mode buffers.  It is a modified version of
the c++-mode syntax table.")
(if asl-mode-syntax-table
    ()
  (setq asl-mode-syntax-table c++-mode-syntax-table)
  (modify-syntax-entry ?$ "." asl-mode-syntax-table)
  (modify-syntax-entry ?: "." asl-mode-syntax-table)
  (modify-syntax-entry ?, "." asl-mode-syntax-table)
  (modify-syntax-entry ?' "." asl-mode-syntax-table)
  (modify-syntax-entry ?` "." asl-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" asl-mode-syntax-table)
  )


;;;###autoload
(defvar asl-mode-style-alist 
  '( (c-basic-offset . 4)
     (c-offsets-alist . ((topmost-intro-cont . +)
                         (inclass . +)
                         (label . +)
                         (access-label . +)
                         (case-label . +)
                         (substatement-open . 0)
                         (statement . asl-lineup-statement)
                         (statement-cont . asl-lineup-statement-cont)
                         )
                      )
     )
  "Offsets for asl-mode.  See `c-offsets-alist'.  DO NOT change the
value of the `statement' or `statement-cont' elements; you will break
indentation for asl-mode if you do.")


;; Defines the local-keymap for SMARTS-ASL mode.  This includes
;; undefining certain c/c++-mode keys not used in ASL.
;;
(defvar asl-mode-map nil)
(if (null asl-mode-map)
    (progn
      (setq asl-mode-map
            (let ((tmpmap (make-sparse-keymap)))
              (cond 
               ;; XEmacs 19 & 20
               ((fboundp 'set-keymap-parents)
                (set-keymap-parents tmpmap c++-mode-map))
               ;; Emacs 19
               ((fboundp 'set-keymap-parent)
                (set-keymap-parent tmpmap c++-mode-map))
               ;; Default
               (t (setq tmpmap (copy-keymap c++-mode-map))))
              tmpmap))
      ;; Rebind TAB to the default tabification function.
      (define-key asl-mode-map "\t" 'indent-for-tab-command)
      ;; Unbind several other keys
      (define-key asl-mode-map "," nil)
      (define-key asl-mode-map ";" nil)
      (define-key asl-mode-map ":" nil)
      (define-key asl-mode-map "#" nil)
      (define-key asl-mode-map "\C-c:" 'undefined)
      (define-key asl-mode-map "\M-\C-h" 'undefined)
      (define-key asl-mode-map "\C-c\C-q" 'undefined)
      (define-key asl-mode-map "\M-\C-q" 'undefined)
      ;; Must always be unbound!
      (define-key asl-mode-map "\C-c." 'undefined)
      (define-key asl-mode-map "\C-c\C-b" 'undefined)
      );end progn
  );end if


;; Compute the indentation for a statement.  Returns
;; "asl-OR-rule-offset" if this line begins with a `|' and
;; 0 otherwise.
(defun asl-lineup-statement (langelem)
  "asl-mode internal function."
  (if (asl-OR-rule-statement)
      (eval asl-OR-rule-offset)
    0
    );end if
  )


;; Compute the indentation for a statement-continuation.  Returns
;; "asl-statement-cont-offset" if this is an ASL action statement and
;; 0 otherwise.
(defun asl-lineup-statement-cont (langelem)
  "asl-mode internal function."
  (if (asl-action-statement)
      (eval asl-statement-cont-offset)
    0
    );end if
  )


;; Examines the line of code at point.  If this code is part of an ASL
;; action statement, this function returns the position of the
;; terminating ';'.  Returns `nil' if identification was inconclusive.
(defun asl-action-statement ()
  "asl-mode internal function."
  (save-excursion
    (and ;; stop at first `nil' result
     (re-search-forward "[^{}:;]\\(;\\|:\\|{\\|}\\)" nil t)
     (equal (match-string 1) ";")
     (match-end 1)
     );end and
    );end excursion
  )


;; Examines the code on the present line.  If the code  begins with a
;; '|' that is not part of the string "||", this function returns
;; `t'.  Returns `nil' otherwise.
(defun asl-OR-rule-statement ()
  "asl-mode internal function."
  (and
   (not (asl-action-statement))
   (save-excursion
     (back-to-indentation)
     (looking-at "|\\s *[^|]")
     );end excursion
   );end and
  )


;; Find the '{' brace matching the '}' at point.  Returns the position of
;; the matching brace or `nil' if no match was found.
(defun asl-find-matching-opening-brace ()
  (save-excursion
    ;; bracecount++ <==> found '{'
    ;; bracecount-- <==> found '}'
    ;; We're at a '}', so start with -1.
    (let ((bracecount -1)
          (commentcount 0));end bindings
      ;; Loop until we've found the matching '{' or until we've hit
      ;; bobp.  Use regexp search to verify that we're not in a
      ;; comment or a string.
      (while (and 
              (not (equal bracecount 0))
              (re-search-backward 
               "\\/\\*\\|\\*\\/\\|^[^/\"'`]*\\([{}]\\)[^\"'`]*$"
               nil t) )
        (or ; choose 1st true case
         ;; Comment-block end.  Count it.
         (and (equal (match-string 0) "*/")
              (setq commentcount (1- commentcount))
              )
         ;; Comment-block start.  Count it.
         (and (equal (match-string 0) "/*")
              (setq commentcount (1+ commentcount))
              )
         ;; If we're in a comment, don't check the next two cases; continue
         ;; looping.
         (not (equal commentcount 0))
         (and (equal (match-string 1) "{")
              (setq bracecount (1+ bracecount))
              )
         (and (equal (match-string 1) "}") 
              (setq bracecount (1- bracecount))
              )
         )
        );end while
      (if (equal bracecount 0)
          (match-beginning 1)
        nil
        );end if
      );end let
    );end excursion
  )


;; Returns non-`nil' if the brace at point ends the parsing block of a
;; rule.
(defun asl-rule-ending-brace ()
  (save-excursion
    (let (endpos)
      (and ; stop at first `nil' statement.
       (setq asl-opening-brace-point 
             (asl-find-matching-opening-brace)) ; can be nil
       (setq endpos (1+ asl-opening-brace-point))
       (goto-char endpos) ; no need to search the text between the '{ ... }'
       (re-search-backward c-ASL-rule-begin-regexp nil t)
       ;; Even if we find a rule-begin, must make sure it's the
       ;; correct one.
       (equal endpos (match-end 0))
       );end and
      );end let
    );end excursion
  )


;; Indents a '}' in an asl rule.  Don't call this unless the
;; `c-syntactic-context' is a statement-cont.  It won't work well otherwise.
(defsubst asl-indent-closing-brace ()
  (setq asl-opening-brace-point nil)
  ;; The valid conditions under which to delete the leading
  ;; indentation.
  (if (or (looking-at c-ASL-do-or-filter-begin-regexp)
          (asl-rule-ending-brace)
          );end or
      (progn
        (beginning-of-line)
        (delete-horizontal-space)
        )
    ;;else:
    ;; See if we need to unindent
    (and 
     asl-opening-brace-point
     (let* ((openIndent (save-excursion 
                          (goto-char asl-opening-brace-point)
                          (current-indentation)))
            (offset (-  openIndent (current-indentation)))
            );end bindings
       (if (< offset 0)
           ;; Too much indentation.  Unindent the line.
           (delete-region (save-excursion (beginning-of-line) (point))
                          (point))
         );end if: delete extra
       (if (not (equal 0 offset))
           ;; Need to indent.
           (indent-to openIndent)
         );end if: indent it
       );end let
     );end and
    );end if
  )


;; Special indent function for ASL mode.  Unindents closing parens matching
;; the beginning of a rule.
(defun asl-indent-line ()
  (save-excursion
    (and  ; stop at first `nil' statement.
     ;; Current line must be in a statement-cont.
     (equal (car (car c-syntactic-context)) 'statement-cont)
     (back-to-indentation)
     ;; Must begin with a '}'
     (looking-at "}\\s *")
     (asl-indent-closing-brace)
     );end and
    );end excursion
  )


;; Add the indentation style for ASL-mode.
;;
(c-add-style "SMARTS-ASL" asl-mode-style-alist nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Define the mode itself.


;;;###autoload
(defun asl-mode ()
  "Major mode for editing SMARTS ASL code.
This mode is a mix of the C++ mode and custom stuff.

The hook variable `asl-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also, the hook
`c-mode-common-hook' is run first.

Indentation for this mode works slightly differently from C/C++ mode.
You can customize indentation of a line based on its syntax using
`c-set-offset'.  Do so from a function called by `asl-mode-hook'; any
changes made by `c-mode-common-hook' are overridden.  There are also
two new variables, `asl-statement-cont-offset' and
`asl-OR-rule-statement', that control indentation of certain types of
ASL statements.  Both variables have help-documentation.

DO NOT change the value of the `topmost-intro-cont', `statement' or
`statement-cont' syntaxes; you will break indentation and cause it to
do unpredictable things.

Font-lock support for this mode comes in the traditional three levels.
Level 1 (the default) fontifies type keywords and keywords for special
constants, such as 'TRUE', 'LOG' and 'FAIL'.  Additional keywords in
level 2 include all of the ASL language-specific words, such as 'if',
'self', and 'return'.  Level 3 adds fontification for all ASL built-in
functions.  See the variable `font-lock-maximum-decoration' for more
info about setting the keyword fontification level.

Key bindings:
\\{asl-mode-map}"
  (interactive)
  ;; Run this defun if it exists.  Note that only GNU emacs ver>=20
  ;; have it.  This is no big deal, since in gnu v20*,
  ;; c-initialize-cc-mode only sets up some default indentation
  ;; styles, which we override in here anyhow.  In the future,
  ;; however, this may change, causing incompatibility problems
  ;; between emacsen using this mode.  You have been warned!
  ;;
  ;; Note:  `boundp' only identifies if a *variable* of a given name
  ;; exists.  To do the same for a defun, use `fboundp'
  (if (fboundp 'c-initialize-cc-mode)
      (c-initialize-cc-mode))
  (kill-all-local-variables)
  ;; The following variables must be local, so that they don't end up
  ;; shared by all ASL/C-like programming modes.  This is especially
  ;; important for `c-offsets-alist' and `c-indentation-style'.
  (make-local-variable 'c-conditional-key)
  (make-local-variable 'c-comment-start-regexp)
  (make-local-variable 'c-class-key)
  (make-local-variable 'c-baseclass-key)
  (make-local-variable 'c-access-key)
  (make-local-variable 'c-label-key)
  (make-local-variable 'c-switch-label-key)
  (make-local-variable 'c-recognize-knr-p)
  (make-local-variable 'c-special-indent-hook)
  (make-local-variable 'c-offsets-alist)
  (make-local-variable 'c-indentation-style)
  (make-local-variable 'c-basic-offset)
  (make-local-variable 'asl-statement-cont-offset)
  (make-local-variable 'asl-OR-rule-offset)
  (if (boundp 'c-extra-toplevel-key)
      (make-local-variable 'c-extra-toplevel-key))
  (set-syntax-table asl-mode-syntax-table)
  (setq major-mode 'asl-mode
	mode-name "SMARTS-ASL"
	local-abbrev-table c++-mode-abbrev-table)
  (use-local-map asl-mode-map)
  (c-common-init)
  ;; asl-mode values for c-mode variables:
  (setq comment-start "// "
	comment-end ""
	c-conditional-key c-ASL-conditional-key
	c-comment-start-regexp c-ASL-comment-start-regexp
	c-class-key c-ASL-bogus-regexp           ; deactivate
	c-access-key c-ASL-bogus-regexp          ; deactivate
     c-label-key c-ASL-bogus-regexp           ; deactivate
     c-switch-label-key c-ASL-bogus-regexp    ; deactivate
	c-recognize-knr-p nil
     ;; c-special-indent-hook -- A function called after the
     ;; cc-mode-engine indents a line.
     c-special-indent-hook 'asl-indent-line
	imenu-generic-expression cc-imenu-c++-generic-expression
        imenu-case-fold-search nil)
  ;; Generate the font-lock keywords, if necessary.
  (asl-maybe-provide-keywords)
  (run-hooks 'c-mode-common-hook)
  ;; *After* running the common mode hooks, set the indentation
  ;; style.  This forces the indentation style to be the asl-mode one
  ;; (the only valid one!).
  (c-set-style "SMARTS-ASL")
  (run-hooks 'asl-mode-hook)
  (c-update-modeline)
  )

(provide 'asl-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Font-lock support and setup

(require 'font-lock)


;; Set up font-lock-constant-face for asl-mode for XEmacs
;;
(if (not (boundp 'font-lock-constant-face))
    (if (boundp 'font-lock-reference-face)
        (progn
          (copy-face font-lock-reference-face 'font-lock-constant-face)
          (defvar font-lock-constant-face 'font-lock-reference-face)
          )
      )
  )


;; Make a new face for ASL rules.  May or may not be used...
;;
(if (fboundp 'defface)
    ;; We're in GNU Emacs.
    (defface asl-font-lock-rule-face
      '(( ((class color) (background light)) 
          (:foreground "MediumBlue" :underline t))
        ( ((class color) (background dark)) 
          (:foreground "SkyBlue" :underline t))
        (t (:inverse-video t :underline t))
        )
      "Font Lock mode face used to highlight rule declarations in
SMARTS-ASL mode."
      :group 'SMARTS)
  ;;
  ;; else
  ;;
  (progn
    ;; We're in XEmacs (or an old version of GNU Emacs).  We'll need
    ;; to do some additional work.
    (if (string-match "XEmacs" emacs-version)
        (make-face 'asl-font-lock-rule-face
                   "Font Lock mode face used to highlight rule
declarations in SMARTS-ASL mode.")
      ;;else:  old GNU Emacs version.
      (make-face 'asl-font-lock-rule-face))
    (set-face-foreground 'asl-font-lock-rule-face "MediumBlue")
    (set-face-underline-p 'asl-font-lock-rule-face t)
    ;; Add the rule face's name to the face list (if the face list
    ;; exists).
    (if (boundp 'font-lock-face-list)
        (setq font-lock-face-list 
              (append '(asl-font-lock-rule-face) font-lock-face-list)))
    );; end progn: XEmacs
  );;end if


;; Use the rule-face if the user wishes to.
(if asl-use-rule-face
    (defvar asl-font-lock-rule-face 'asl-font-lock-rule-face
      "Face name used to highlight rule declarations in SMARTS-ASL mode.")
  ;;else
  (defvar asl-font-lock-rule-face 'font-lock-variable-name-face
    "Face name used to highlight rule declarations in SMARTS-ASL mode.")
)


;; Generally:  a key defining default font-lock behavior for
;; SMARTS-ASL mode.
;;
;; Specifically:  Defines how to highlight asl parsing-rule
;; definitions.
;;
;; Do *not* use `c-ASL-rule-begin-regexp'; that would be too slow.
;; Besides, `c-ASL-rule-begin-regexp' doesn't fully work with
;; font-lock mode; it matches multiple lines.  This regexp matches
;; things on a single line, as per font-lock specs.
;;
;; The regexp below looks for the following criteria:
;; 1) Uppercase word (with or without numerals or '_') at the
;;    beginning of a line.  This is what we highlight.
;; 2) This must be followed by either:
;;  a) a '('
;;  b) optional space followed by a '{'
;;  c) at least one space followed by one of the keywords `do' or
;;     `filter'. 
;;
(if (boundp 'font-lock-defaults-alist)
    (defconst asl-font-lock-key
      (list
       (concat
        "^\\([A-Z0-9_]+\\)"
        "\\((\\|\\s *{\\|\\s +\\(filter\\|do\\)\\)"
        );end concat
       1 'asl-font-lock-rule-face);end list
      )
  ;;else
  (progn
  ;; We're in XEmacs, which has to go and be different all of the
  ;; time...
    (defconst asl-font-lock-key
      (list
       (concat
        "^\\([A-Z0-9_]+\\)"
        "\\((\\|\\s *{\\|\\s +\\(filter\\|do\\)\\)"
        );end concat
       1 asl-font-lock-rule-face);end list
      )
    ))


(defconst asl-constant-face-keywords 
  (concat 
   "\\<\\("
   "FA\\(IL\\|LSE\\)\\|"
   "IGNORE\\|"
   "LOG\\|"
   "N\\(EXT\\|OTIFY\\)\\|"
   "STOP\\|"
   "TRUE\\|"
   "do\\|"
   "exact\\|"
   "filter\\|"
   "ignore"
   "\\)\\>" 
   );end concat
  "ASL keywords to color in the `font-lock-constant-face'.  Aside from
being used for the `do' and `filter' keywords, also used for any
keyword following the '?' operator or appearing on the RHS of a '='."
  )


(defconst asl-keyword-face-keywords 
  (concat
   "\\<\\("
   "NO_LOCK\\|"
   "READ_LOCK\\|"
   "WRITE_LOCK\\|"
   "break\\|"
   "c\\(ase\\|ontinue\\)\\|"
   "de\\(fault\\|lim\\)\\|"
   "else\\|"
   "foreach\\|"
   "global\\|"
   "i\\(f\\|nput\\)\\|"
   "local\\|"
   "re\\(pository\\|turn\\)\\|"
   "self\\|"
   "this\\|"
   "while"
   "\\)\\>"
   );end concat
  "ASL keywords to color in the `font-lock-keyword-face'.  Most of the
ASL language's keywords will go here."
  )


(defconst asl-type-face-keywords 
  (concat
   "\\<\\("
   "boolean\\|"
   "char\\|"
   "eol\\|"
   "f\\(loat\\|s\\)\\|"
   "hex\\|"
   "integer\\|"
   "list\\|"
   "numeric\\|"
   "string\\|"
   "table\\|"
   "word"
   "\\)\\>"
   );end concat
  "ASL keywords to color in the `font-lock-type-face'.  ASL keywords that
appear in parsing rules, or that control type-conversion, go here."
  )


(defconst asl-function-name-face-keywords 
  (concat
   "\\<\\("
   "any\\|"
   "c\\(lear\\|reate\\)\\|"
   "de\\(fined\\|lete\\)\\|"
   "g\\(et\\(C\\(auses\\|losure\\)\\|E\\(vent\\(Description\\|"
   "Type\\)\\|xplains\\)\\|Instances\\|ServerName\\)\\|lob\\)\\|"
   "hexToString\\|"
   "is\\(Null\\)?\\|"
   "not\\(any\\|ify\\)?\\|"
   "object\\|"
   "p\\(eek\\|rint\\)\\|"
   "quit\\|"
   "rep\\|"
   "s\\(ize[oO]f\\|leep\\|t\\(art\\|op\\)\\|ubstring\\)\\|"
   "t\\(ab\\|hread\\|ime\\|o\\(Lower\\|Upper\\)\\|ransaction\\)\\|"
   "undef"
   "\\)\\>"
   );end concat
  "ASL keywords to color in the `font-lock-function-name-face'.  The
names of all ASL builtin functions should appear here."
  )


(defconst asl-font-lock-keywords-1 
  (append
   (list asl-font-lock-key)
   (list 
     (cons asl-constant-face-keywords 'font-lock-constant-face)
     (cons asl-type-face-keywords 'font-lock-type-face)
     );end list
   );end append
  )


(defconst asl-font-lock-keywords-2
  (append
   asl-font-lock-keywords-1
   (list
     (cons asl-keyword-face-keywords 'font-lock-keyword-face)
     );end list
   );end append
  )


(defconst asl-font-lock-keywords-3
  (append
   asl-font-lock-keywords-2
   (list
     (cons asl-function-name-face-keywords 'font-lock-function-name-face)
     );end list
   );end append
  )


;; The defaults for font-lock in asl-mode
;;
(defconst asl-font-lock-defaults
   (list
    '(asl-font-lock-keywords-1 asl-font-lock-keywords-1
      asl-font-lock-keywords-2 asl-font-lock-keywords-3)
    nil nil
    '((?_ . "w") (?~ . "w"))
    'beginning-of-defun
    '(font-lock-comment-start-regexp . "/[*/]")   
    '(font-lock-mark-block-function . mark-defun)
    );end list
  )


;; Set up font-lock for asl-mode, taking differeces between XEmacs and
;; GNU Emacs into account.  Only need to do this once.
;;
(if (boundp 'font-lock-defaults-alist)
    (if (assq 'asl-mode font-lock-defaults-alist)
        ;; If there's already an entry for this mode, just change it.
        (setcdr (assq 'asl-mode font-lock-defaults-alist)
                asl-font-lock-defaults)
      ;;else
      ;; Add it if there's no entry.
      (setq font-lock-defaults-alist
            (append font-lock-defaults-alist
                    (list (cons 'asl-mode asl-font-lock-defaults))
                    );end append
            );end setq
      );end if
  ;;else
  (put 'asl-mode 'font-lock-defaults asl-font-lock-defaults)
  );end if


;; This is needed by XEmacs' version of font-lock, which does not
;; define `font-lock-defaults-alist'
;;
(defun asl-maybe-provide-keywords ()
  (if (not (boundp 'font-lock-defaults-alist))
      (progn
        (make-local-variable 'font-lock-defaults)
        ;; Set the defaults:  highlight rule-names in rule-definitions.
        (setq font-lock-defaults
              (get 'asl-mode 'font-lock-defaults))
        );end progn
    );end if
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;
