;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  emacs support for MODEL language
;;
;;  Copyright © 1999-2000 John P. Weiss
;;  Property of John P. Weiss.  Not to be modified or distributed
;;  without consent of the author.
;;
;;  
;;  Uses "cc-mode.el", the standard code for editing C, C++,
;;  Objective-C, Java, and even CORBA-IDL.
;;
;;  MODEL is a complex language.  Be forewarned:  this file is large.
;;
;;  Uses `c-beginning-of-statement'
;;       `c-end-of-statement'
;;       `c-indent-comand'
;;  Also attempts to use `c-initialize-cc-mode'
;;  
;;  If future releases of `cc-mode.el' change these functions and
;;  vars, someone may need to alter this file accordingly.
;;  
;;  last modified 2/7/2000 (jpw)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

;; Load this at the beginning of the file, since there are many
;; statements below that depend on the existence of c++-mode
;; variables. 
(require 'cc-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODEL Mode User-Customizable Variables
;;


;; For XEmacs and earlier versions of GNU Emacs
;;
(if (not (fboundp 'defgroup))
    (defmacro defgroup (vn &rest xtras)
      "No-op."
      t))
(if (not (fboundp 'defcustom))
    (defmacro defcustom (vn vv dstr &rest xtras)
      "Alias for defvar."
      (eval (list 'defvar vn vv dstr))))


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
 * foo-mode-wodgie-offset: 5
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


(defcustom model-mode-hook nil
  "*Hook called by `model-mode'."
  :type 'hook
  :group 'SMARTS)


(defcustom model-statement-cont-offset 'c-basic-offset
  "Default indentation for lines that continue a statement.  Used when
`model-docstring-offset', `model-assign-cont-offset', or
`model-definition-offset' are not appropriate.  Must evaluate to an
integer value.

It is best not to change this variable; set `c-basic-offset' instead."
        :group 'SMARTS-Indentation)


(defcustom model-docstring-offset 2
  "Indentation for MODEL documentation strings.  Must evaluate to an
integer value.

Example:

You might expect this to be valid:
    (setq model-docstring-offset 3)
but you can also specify the quoted name of a variable:
    (setq model-docstring-offset 'c-basic-offset)
or even a quoted mathematical expression:
    (setq model-docstring-offset '(/ c-basic-offset 2))

"
  :type 'custom-variable
  :group 'SMARTS-Indentation)


(defcustom model-assign-cont-offset 0
  "Indentation for extra lines in the assignment part of a MODEL
statement.  Any line following the first `=', `=>', `->' or `<=' will
be indented by this amount.  Must evaluate to an integer value (see
`model-docstring-offset')."
  :type 'custom-variable
  :group 'SMARTS-Indentation)


(defcustom model-definition-offset 4
  "Indentation for lines in the definition of a MODEL operation.  Must
evaluate to an integer value (see `model-docstring-offset')."
  :type 'custom-variable
  :group 'SMARTS-Indentation)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODEL Mode Internal Variables


;; Regexp that should fail in almost any search in any buffer.  Used
;; to disable certain emacs-cc-mode-engine features.
(defconst c-MODEL-bogus-regexp
  "µ\\t\\r±\\nà½¨¢¢¨½à\\n±\\r\\tµ" )


;; Regexp to match the start of a member entity of a MODEL class.
;; "Member entity" is anything appearing in a MODEL class, except
;; operation definitions.  Another regexp covers those.
;;
;; Used by the syntax-analysis engine and for font-lock mode.
(defconst c-MODEL-member-keyword-regexp
  (concat
   "a\\(ggregate\\|ttribute\\)\\|"
   "constraint\\|"
   "e\\(vent\\|xport\\)\\|"
   "fragment\\|"
   "instrument\\|"
   "problem\\|"
   "symptom\\|"
   "re\\(fine\\|lationship\\(set\\)?\\)\\|"
   "table"
   );end concat
  )


;; Regexp to match an assignment.
;;
;; Used by the syntax-analysis engine. 
(defconst c-MODEL-assignment-regexp
  "\\s *\\(=>\\|<=\\|->\\|=\\)\\s +")


;; Regexp to match an operation definition
;;
;; Used by the syntax-analysis engine. 
(defconst c-MODEL-definition-regexp "\\s \\(definition\\)\\s *:\\s *")


;; Regexp to match the opening of a MODEL class.
;;
;; Used by the syntax-analysis engine and for constructing `c-MODEL-class-key'.
(defconst c-MODEL-class-start-regexp
  "\\(traced\\s +\\)?\\(abstract\\s +\\)?interface\\s +")


;; Things other than struct, enum, and MODEL classes that can be
;; defined at the toplevel.
;;
;; Used to modify the emacs-cc-mode-engine's behavior.
(defconst c-MODEL-extra-toplevel-key
  (concat
   "\\("
   "extern\\|typedef\\|enum"
   "\\)[^_]"
   );end concat
  )


;; Key defining any class-like toplevel entity that can appear in a
;; MODEL file.
;;
;; Used to modify the emacs-cc-mode-engine's behavior and for
;; font-lock mode.
(defconst c-MODEL-class-key 
  (concat
   "\\(\\<"
   c-MODEL-class-start-regexp
   "\\)\\|\\(\\bstruct\\b\\)"
   );end concat
  )


;; Key describing the part of a MODEL class-definition where one
;; specifies the name of the base-class.
;;
;; Used to modify the emacs-cc-mode-engine's behavior.
(defconst c-MODEL-baseclass-key
  (concat
   "\\s *:\\s *" c-symbol-key
   );end concat
  )
   

;; The access-key ordinarily contains keywords like "public" or
;; "private".  MODEL doesn't have anything like that.  Only the
;; `implementation' block comes close.
;;
;; Used to modify the emacs-cc-mode-engine's behavior.
(defconst c-MODEL-access-key "\\s *implementation\\s *:\\s *")


;; For old/broker versions of cc-mode
;;
(if (boundp 'c-Java-comment-start-regexp)
    (defvar c-MODEL-comment-start-regexp c-Java-comment-start-regexp)
  ;;else
  (defvar c-MODEL-comment-start-regexp c-C++-comment-start-regexp))


;; The only thing remotely similar to a conditional in a MODEL file:
;; a guard expression.
;;
;; Used to modify the emacs-cc-mode-engine's behavior.
(defconst c-MODEL-conditional-key 
  (concat
   "\\b\\(if\\)\\b"
   "[^;]+"
   "\\(\\bcheck\\b[^;]+\\)?"
   ";"
   );end concat
  )


;; Search key for finding a member method declaration.  Note:  each
;; occurrence of `c-symbol-key' contains a `\\(\\)' pair.
;;
;; Used by the syntax-analysis engine. 
(defconst c-MODEL-method-def-key
   (concat
    "\\("
    "set(\\s *" c-symbol-key "\\s *)" ; Return type (can be a set)
    "\\|"
    "\\b" c-symbol-key "\\)"
    "\\(\\s \\|[\n\r]\\)+"            ; Whitespace or return chars
    c-symbol-key "[(]"                ; The operation's name and open-paren
    "\\("
    "in\\(out\\)?\\s \\|out\\s \\|"   ; The first arg or...
    "\\s *)"                          ; ...a close-paren.
    "\\)"
    );end concat
   )


;; Search key for finding a member declaration (excluding member operations).
;;
;; Used by the syntax-analysis engine.
(defconst c-MODEL-member-key
  (concat
   "\\b\\("                                ; Start of a word
   c-MODEL-member-keyword-regexp           ; One of the special
                                           ; member-entity keywords
   "\\)\\b"                                ; end of word
   );end concat
  )


;; Used to pass the result of a syntax eval between defun that aren't
;; directly called by one another.
(defvar model-syntactic-context nil
  "Variable containing the MODEL-specific sytactic analysis list
during an indentation.  See `model-show-syntactic-information' for
more info.

Do not modify this variable.")


;; Used by the syntax engine.
(defvar model-paren-cache nil
  "Variable containing the psotions of all '(' and ')' in the
assignment block of a MODEL-statement.  Set during indentation.

Do not modify this variable.")


;; Used by the indentation-engine to indent non-model paragraphs.
(defvar old-c-indent-function nil
  "The contents of `indent-line-function' as set by the c-mode
engine.

Do not modify this variable.")


;; Used by the fill-engine to fill non-string-line paragraphs.
(defvar old-c-fill-paragraph-function nil
  "The contents of `fill-paragraph-function' as set by the c-mode
engine.  

Do not modify this variable.")


;; These next three are used by the function `model-progress-dial'.
(defvar model-progress-message "Waiting... "
  "The message displayed by `model-progress-dial'.  Set this variable
to your message before calling `model-progress-dial', then reset it
afterwards.")
(defconst model-fill-string-line-regexp
  "^\\(\\s *\\)\"\\(.*[^ \t]\\)\\(\\s *\\)\"\\s *\\(;\\)?\\s *$")
(defconst model-fill-string-line-stripped-regexp
  "^\\(\\s *\\)\\(.*[^\\\\]\\(\\\\?[^;\n\r]\\)\\)\\(;\\)?$")


;; Used to modify the emacs-cc-mode-engine's behavior.
;; Note: using "\t" in place of an actual Ctrl-I won't work here, for
;; some reason.
(defconst model-fill-regexp
  (concat
  "[ 	]*"
  "\\([\"][ 	]*\\)?"
  "\\|\\([-|#;>*]+ *\\|(?[0-9]+[.)] *\\)*"
  );end concat
  "Modified version of `adaptive-fill-regexp' for use with wrapped
quoted strings.")


;;;###autoload
(defvar model-mode-syntax-table nil
  "Syntax table used in model-mode buffers.")
(if model-mode-syntax-table
    ()
  (setq model-mode-syntax-table c++-mode-syntax-table)
  (modify-syntax-entry ?$ "." model-mode-syntax-table)
  (modify-syntax-entry ?, "." model-mode-syntax-table)
  (modify-syntax-entry ?' "." model-mode-syntax-table)
  (modify-syntax-entry ?` "." model-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" model-mode-syntax-table)
  )


;;;###autoload
(defvar model-mode-style-alist 
  '( (c-basic-offset . 4)
     (c-offsets-alist . ((label . 0)
                         (access-label . 0)
                         (inclass . +)
                         (statement-cont . model-compute-statement-cont)
                         (topmost-intro-cont . model-compute-statement-cont)
                         (substatement-open . 0)
                         (member-init-cont . 0)
                         )
                      )
     )
  "Offsets for model-mode.  See `c-offsets-alist'.

Do not touch `statement-cont' or `topmost-intro-cont'; you will break
MODEL-mode indentation.  See `model-statement-cont-offset' instead.")


;; Defines the local-keymap for SMARTS-MODEL mode.  This includes
;; undefining certain c/c++-mode keys not used in MODEL.
;;
(defvar model-mode-map nil)
(if (null model-mode-map)
    (progn
      (setq model-mode-map
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
      (define-key model-mode-map "\t" 'indent-for-tab-command)
      ;; Add two MODEL-mode keys.
      (define-key model-mode-map "\C-c\C-y" 'model-show-syntactic-information)
      (define-key model-mode-map "\M-\"" 'model-string-fill-region)
      ;; Unbind several other keys
      (define-key model-mode-map "," nil)
      (define-key model-mode-map ";" nil)
      (define-key model-mode-map ":" nil)
      (define-key model-mode-map "#" nil)
      (define-key model-mode-map "\C-c:" 'undefined)
      (define-key model-mode-map "\M-\C-h" 'undefined)
      (define-key model-mode-map "\C-c\C-q" 'undefined)
      (define-key model-mode-map "\M-\C-q" 'undefined)
      ;; Must always be unbound!
      (define-key model-mode-map "\C-c." 'undefined)
      (define-key model-mode-map "\C-c\C-b" 'undefined)
      );end progn
  );end if


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODEL Mode Utility Functions
;;
;; General Purpose
;; 


;; Returns non-nil if thisPoint is inside of a C-style comment block.
;;
(defun model-point-inside-comment-block (thisPoint)
  (save-excursion
    (goto-char thisPoint)
    (and (goto-char thisPoint)
         (re-search-backward "/[*]\\|[*]/" nil t)
         (looking-at "/[*]")
         (goto-char thisPoint)
         (re-search-forward "[*]/" nil t)
         );end and
    );end excursion
  )


;; Fix for old GNU Emacsen
;;
(if (not (fboundp 'line-beginning-position))
    (defun line-beginning-position ()
      "Hack for using MODEL mode in old versions Emacs."
      (save-excursion
        (beginning-of-line)
        (point)
        )))
(if (not (fboundp 'line-end-position))
    (defun line-end-position ()
      "Hack for using MODEL mode in old versions Emacs."
      (save-excursion
        (end-of-line)
        (point)
        )))


;; Return t if thisPoint is inside of a string.
;;
(defun model-point-inside-string (thisPoint)
  (save-excursion
    (and ; stop at first `nil' statement
     (goto-char thisPoint)
     (skip-chars-backward "^\"" (line-beginning-position))
     (equal (preceding-char) ?\")
     (goto-char thisPoint)
     (skip-chars-forward "^\"" (line-end-position))
     (equal (following-char) ?\")
     );end and
    );end excursion
  )


;; Return t if thisPoint is inside of an inline string or C/C++ comment.
;;
(defun model-point-in-inline-string-or-comment (thisPoint)
  (save-excursion
    ;; Look for "/*", "*/", or a single '"', excluding "\""
    (and (goto-char thisPoint)
         (re-search-backward "\/\/\\|/[*]\\|[*]/\\|[^\\]\"" 
                             (1- (line-beginning-position)) t)
          (cond ;; Comment or string?
           (;; Comment starter
            (or (looking-at "\/\/")
                (looking-at "/[*]"))
            t
            );end case: comment
           (;; String
            ;; (Note: this case must come last due to the cursor
            ;; motion in the test condition.)
            (progn (forward-char) (equal (following-char) ?\"))
            ;; Find the matching closing quote (since we could be
            ;; in-between two strings).
            (goto-char thisPoint)
            (if (re-search-forward "[^\\]\"" (line-end-position) t)
                t)
            );end case:string
           );end cond
          );end and
    );end excursion
  )


(defun model-progress-dial ()
  "Displays a rotating progress dial out in front of a message.  The
message is contained in the variable `model-progress-message'."
  (if (not (boundp 'model-progress-dial-list))
      (defvar model-progress-dial-list (list "-" "\\" "|" "/"))
    )
  (message "%s %s" model-progress-message (car model-progress-dial-list))
  (setq model-progress-dial-list 
        (nconc (cdr model-progress-dial-list) 
               (list (car model-progress-dial-list))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODEL Mode Utility Functions
;;
;; Indentation-Specific
;; 


(defun model-compute-statement-cont (syntax)
  (if (not (equal 'other (nth 2 model-syntactic-context)))
      (eval model-statement-cont-offset)
    ;; else
    0
    )
  )


(defun model-skip-chars-forward-nocomment (skipstr endpos targlist
                                                    &optional nostring)
  "`skipstr' and `endpos' are identical to the two arguments to
`skip-chars-forward'.  `skipstr' should match the characters '/' and
'*'.  Otherwise, this function will fail.  If `nostring' is true,
`skipstr' should also match the characters \" \' and \`."
  (save-restriction
    (narrow-to-region (point) endpos)
    (let ((notFound 't)
          (comment-count 0)
          oldpos
          );end bindings
      (while (and notFound
                  (not (eobp)))
        (skip-chars-forward skipstr)
        (or ;3 Cases
         ;; Case 1:  '/' - could be a comment.
         (and (equal (following-char) ?/)
              (looking-at "\\/\\*\\|\\/\\/")
              (progn 
                (if (equal (match-string 0) "/*")
                    (setq comment-count (1+ comment-count))
                  );Increment counter for C-style comments only.
                (end-of-line)
                t);Always succeed
              );end and:  comment start case
         
         ;; Case 2: comment end
         (if (looking-at "\\*\\/")
             (progn (setq comment-count (1- comment-count))
                    (goto-char (match-end 0))
                    t);Always succeed
           );end if:  comment end case
         
         ;; Case 3: must not be inside of a comment
         (and 
          (equal comment-count 0)
          (or ;; non-comment subcases
           (and nostring ;; skip string subcase
                (member (following-char) '(?\" ?' ?`))
                ;;We hit a string start.  Find matching quote.
                (if (equal (following-char) ?')
                    (progn
                      ;; Handle apostrophes correctly.  Look for
                      ;; matching ', but if we hit the end of the
                      ;; statement and not a match, ignore the '.
                      (forward-char)
                      (setq oldpos (point))
                      (skip-chars-forward "^'")
                      (if (equal (following-char) ?') 
                          (forward-char) ;;else
                        (goto-char oldpos))
                      t); Always succeed
                  ;;else
                  (progn (forward-char)
                         (skip-chars-forward "^\"`")
                         t);Always succeed
                  );end if
                );end and: skip-string subcase
           ;; subcase: found char
           (if (not (null (memq (following-char) targlist)))
               (progn (setq notFound nil)
                      t)
             );end if: subcase found char
           );end or: non-comment subcases
          );end and: non-comment case
         );end or: cases
        ;; Move past the matched character.
        (if (and notFound (not (eobp)))
            (forward-char))
        );end while
      );end let
    );end restriction
  )


;; Like re-search-forward, but excludes matches inside of comments or
;; strings.
(defsubst model-nocomment-re-search-forward (regexp endpt)
  (let ((oldpoint (point))
        )
    (prog1 (and 
            (re-search-forward regexp endpt t)
            (not (model-point-in-inline-string-or-comment (point)))
            );end and
      ;; Return to the original position if the search fails.
      (goto-char oldpoint)
      );end prog1
    );end let
  )


;; Docstrp is where the first string-only line starts.  The syntactic
;; analysis searches for the first docstring, however.  This function
;; determines where the point really should be.  Note that we only
;; need to call this for the non-operation docstrings.
(defsubst model-choose-docstrp (docstrp)
  (save-excursion
    ;; Choose bolp if the 1st docstring is on its own line.
    ;; Otherwise, chooose eolp
    (beginning-of-line)
    (if (not (looking-at "\\s *\""))
        (end-of-line))
    (point)
    )
  )


;; Like model-choose-docstrp, but for assignment or definition strings.
(defsubst model-choose-assignp (assignp)
  (save-excursion
    ;; Choose bolp if the 1st docstring is on its own line.
    ;; Otherwise, chooose eolp
    (beginning-of-line)
    (if (looking-at "\\s *[<>-=d]")
        (point)
      assignp
      )
    )
  )


;; If endp isn't at the current-eolp, scan from startp to endp for all
;; '(' and ')'.  Place these in the `model-paren-cache', which should
;; be 'nil' when this is called.
;;
(defsubst model-scan-for-parens (startp endp current-eolp)
  (setq model-paren-cache '())
  ;; The current line cannot be the statement's end.
  (if (> endp current-eolp)
      (let ((bobuf (point-max))
            paropen);end bindings
        (save-excursion
          (save-restriction
            (narrow-to-region startp endp)
            (goto-char startp)
            (skip-chars-forward "^()")
            (while (not (eobp))
              (if (equal (following-char) ?\()
                  (setq paropen (cons (point) paropen))
                ;;else: It's a close-paren.
                ;; Is is dangling (did we find no '(')?  Then use point-min.
                (if (null paropen)
                    (setq model-paren-cache 
                          (cons (list bobuf (point)) 
                                              model-paren-cache))
                  ;;else:  not dangling.  Construct match - pull first
                  ;;'(' off queue.
                  (setq model-paren-cache (cons (list (car paropen)
                                                      (point)) 
                                                model-paren-cache)
                        paropen (cdr paropen))
                  );end if: dangling
                );end if: equal ()
              (forward-char)
              (skip-chars-forward "^()")
              );end while
            );end restriction
          );end excursion
        ;; For dangling open parens, use point-max as match.
        (while (not (null paropen))
          (setq model-paren-cache 
                (cons (list (car paropen) (point-max)) model-paren-cache)
                paropen (car (cdr paropen)))
          );end while
        );end let
    );end if
  )


;; Scan `model-paren-cache' to see if `thisPoint' is between two
;; parens.  Return `t' if it is, `nil' if not.
(defsubst model-in-parens (thisPoint)
  (let* ((offset 0)
        (len (length model-paren-cache))
        (pair (nth offset model-paren-cache))
        result);end bindings
    (while (and (null result)
                (< offset len))
      (setq result (and (< (car pair) thisPoint)
                        (<  thisPoint (car (cdr pair)))
                        );end and
            offset (1+ offset)
            pair (nth offset model-paren-cache)
            );end setq
      );end while
    result
    );end let
  )


(defun model-syntactic-analysis ()
  "Performs syntactic analysis on model statements, placing the
results in `model-syntactic-analysis'.  See
`model-show-syntactic-information' for details of what this function
returns."
  (save-excursion
    ;; In case point is already at the end-of-statement, move to the
    ;; beginning of line.  This way, the call to (c-end-of-statement)
    ;; won't skip to the end of the *next* statement.
    ;; Notice, too, that we don't save-excursion.  This way, when we
    ;; call (c-beginning-of-statement) later on, we are sure to find
    ;; the beginning of the current statement.
    (let (; Begin local bindings
          (origpos (progn (if (bolp) (back-to-indentation))
                          (point)))
          (eosp (progn (beginning-of-line)
                       (c-end-of-statement nil nil t) 
                       (point)))
          ;; Default these to nil
          bosp stype soffset docstrp assignp assigncol assigneolp
          def-endp
          ); End bindings
      (c-beginning-of-statement nil nil t)
      (setq bosp (point)
            soffset (current-indentation)
            model-paren-cache nil)
      (cond ;; The cases
       ;; We evaluate the cases in the following order:
       ;; 1st:  All cases checked using (looking-at ...) 
       ;; 2nd:  All cases checked using (re-search-forward ...)
       ;;
       ;; The next ordering criteria is complexity.  Keywords with
       ;; additional characters (like '(' or '-', etc), come first,
       ;; then regexps with multiple, plain-keywords.  Last come
       ;; regexps with single keywords.
       ;;
       ;; The exception to this ordering of cases is mutual
       ;; exclusivity.  For example, no member can be named
       ;; 'interface', no operation can have an argument called
       ;; 'readonly', and so on.  If the regexp for a non-operation
       ;; member incorrectly matches an operation, that code won't
       ;; compile.  Therefore, the syntactic analysis *should* be wrong!

       (;; Case 1: Goofs.
        (or (> origpos eosp) (< origpos bosp))
        ;;The original position should never be outside of the
        ;;statement bounds.  If it is, assume we're outside of a MODEL
        ;;class, and change the statement bounds to point to the
        ;;current line's bounds.
        (goto-char origpos)
        (beginning-of-line)
        (setq stype 'other
              bosp (point)
              eosp (progn (end-of-line) (point))
              )
        );end case: goofs

       (;; Case 2: class
        (looking-at c-MODEL-class-start-regexp)
        ;; This the start of a class.
        ;; Class syntax:  no assignment positions.
        (setq stype 'class)
        ;; So, find the first docstring, or the start of the class. 
        (skip-chars-forward "^\"{" eosp)
        ;; If we find a docstring, stash its location
        (if (equal (following-char) ?\")
            (setq docstrp (model-choose-docstrp (point)))
          )
        );end case: class

       (;; Case 3: implementation statement
        (looking-at c-MODEL-access-key)
        ;; This is the 'implementation' tag.
        (setq stype 'implementation)
        (end-of-line)
        ;; If the statement ends on the next line, the cc-engine has
        ;; misanalyzed this statement and merged the
        ;; 'implementation:' keyword with the following statement.
        ;; We need to figure out where bosp and eosp *really* belong.
        (if (>= eosp (point))
            ;; Check where `point' was originally located.
            (if (> origpos (point))
                (progn ;; On next line.  Fix bosp.
                  (setq bosp (1+ (point)))
                  ;; Use the default syntax type
                  (setq stype 'other)
                  )
              ;;else
              (progn ;; On `implementation:' line.  Fix eosp.
                (setq eosp (point))
                )
              );end if: check origpos
          );end if:  statement-end error
        );end case: implementation keyword

       (;; Case 4: model-member
        (model-nocomment-re-search-forward c-MODEL-member-key eosp)
        ;; This statement is a MODEL member.
        (setq stype 'member)
        ;; Find the docstring or assignment clause. 
        (model-skip-chars-forward-nocomment "^/*\"<=->" eosp
                                          '(?\" ?< ?> ?- ?=))
        ;; Docstring comes first.  If we find one, stash its
        ;; location, then scan for assign.
        (if (equal (following-char) ?\")
            (progn (setq docstrp (model-choose-docstrp (point)))
                   (model-skip-chars-forward-nocomment 
                    "^/*\"\'\`<=->" eosp '(?< ?> ?- ?=) t)
                   );end progn
          );end if
        ;; Did we find an assignment token?  (The assign regexp
        ;; includes a preceding space, so step back one before checking).
        (backward-char) 
        (if (looking-at c-MODEL-assignment-regexp)
            (progn
              ;; Stash both location and column just after assignment token
              (goto-char (match-end 0))
              (setq assignp (model-choose-assignp (point))
                    assigncol (current-column))
              (end-of-line)
              (setq assigneolp (point))
              (model-scan-for-parens assignp eosp assigneolp)
              )
          );end if: has an assignment
        );end case: model-member

       (;; Case 5: operation
        (model-nocomment-re-search-forward c-MODEL-method-def-key eosp)
        ;; This is the start of a MODEL operation.
        (setq stype 'operation)
        ;; The defaults:  if there's no closing ')' on the arglist,
        ;; then there can be no docstring *and* no definition block.
        ;; In that case, set docstrp to eosp.
        (setq docstrp eosp)
        (goto-char (match-end 0))
        ;;Is this a method with no-args?  If so, we're done.  If
        ;;not, look for closing ')'
        (if (equal (preceding-char) ?\))
            (setq docstrp (point))
          ;;else
          (progn (model-skip-chars-forward-nocomment "^/*\)\"\'" eosp
                                                   '(?\)) t)
                 (if (equal (following-char) ?\))
                     (setq docstrp (1+ (point))))
                 )
          );end if
        ;;Search for the start of a definition block, but only if
        ;;there's something in the statement following the docstring
        ;;(i.e. we found the close of the arglist).
        (if (and (not (equal docstrp eosp))
                 (re-search-forward c-MODEL-definition-regexp eosp t))
            (progn 
              (setq def-endp (match-end 0))
              (goto-char (match-beginning 1))
              ;; Note:  model-choose-assignp destroys search results.
              (setq assignp (model-choose-assignp (point))
                    assigncol (current-column))
              ;;If text follows the `definition:' keyword, align to
              ;;the ':'.
              (goto-char def-endp)
              (if (not (eolp))
                  (setq assigncol (- (current-column)
                                     (eval model-definition-offset)))
                )
              (end-of-line)
              (setq assigneolp (point))
              (model-scan-for-parens assignp eosp assigneolp)
              );end progn
          );end if: has a definition
        );end case: operation

       (;; Default case:
        (setq stype 'other)
        )
       );end cases
      (setq model-syntactic-context (list bosp eosp stype soffset
                                          docstrp
                                          assignp assigncol assigneolp))
      );end let
    );end excursion
  )


(defun model-compute-indent-column (model-syntax)
  "Using a syntax from `model-syntactic-analysis', compute the column to
which to indent.  (Note: Does not use `save-excursion'.)"
  (let ((bosp (nth 0 model-syntax))
        (eosp (nth 1 model-syntax))
        (statement-syntax (nth 2 model-syntax))
        (statement-col (nth 3 model-syntax))
        (docstrp (nth 4 model-syntax))
        (boap (nth 5 model-syntax))
        (boa-column (nth 6 model-syntax))
        (eoap (nth 7 model-syntax))
        new-icol
        );end bindings
    (cond ; Syntax cases

     (;Case: Catch weird errors.
      ;;Point should never be outside of the statement bounds.
      (or (< (point) bosp) (> (point) eosp))
      nil
      )      

     (;Case: model member
      (equal statement-syntax 'member)
      ;; Body
      (if (and (not (null docstrp))
               (<= docstrp (point))
               (or (null boap) ;;If no assign block, it's a docstring.
                   (< (point) boap)))
          (and ; In the docstrp area 
           (back-to-indentation)
           (equal (following-char) ?\")
           (setq new-icol (+ statement-col (eval model-docstring-offset)))
           );end docstrp
        ;;else: check for assign block
        (and 
         (not (null boap))
         (<= boap (point))
         ;; In assign block.  See if we're on the 1st line, or on
         ;; a later one.
         (if (< eoap (point))
             ;; On later line.  Make sure it's not in parens
             (if (not (model-in-parens (save-excursion 
                                         (beginning-of-line)
                                         (point))
                                       ))
                 (setq new-icol (+ boa-column 
                                   (eval model-assign-cont-offset)))
               );;endif
           ;;else: on 1st line
           (setq new-icol (+ statement-col c-basic-offset))
           )
         );end and
        );end if
      );end case: model member

     (;Case: model method
      (equal statement-syntax 'operation)
      ;; Body
      (if (<= docstrp (point))
          (if (or (null boap) ;;If no assign block, it's a docstring.
                  (< (point) boap))
              (and ; In the docstrp area 
               (back-to-indentation)
               (equal (following-char) ?\")
               (setq new-icol (+ statement-col (eval model-docstring-offset)))
               );end docstrp
            ;;else: check for assign block
            (and 
             (not (null boap))
             (<= boap (point))
             ;; In assign block.  See if we're on the 1st line, or on
             ;; a later one.
             (if (< eoap (point))
                 ;; On later line.  Make sure it's not in parens
                 (if (not (model-in-parens (save-excursion 
                                             (beginning-of-line)
                                             (point))
                                           ))
                     (setq new-icol (+ boa-column 
                                       (eval model-definition-offset)))
                   );;endif
               ;;else: on 1st line
               (setq new-icol (+ statement-col c-basic-offset))
               )
             );end and
            );end if
        ;;else:  Do nothing; we're somewhere in the arglist
        t
        );end if
      );end case: model method

     (;Case: model class
      (equal statement-syntax 'class)
      ;; Body
      (if (or (null docstrp) ;; Outside the docstring, if any?
              (< (point) docstrp))
          (setq new-icol 0)
        ;;else
        (and ;check for docstring
          (back-to-indentation)
          (equal (following-char) ?\")
          (setq new-icol (+ statement-col (eval model-docstring-offset)))
          );end else
        );end if
      );end case: model class

     (;Case: `implementation' keyword
      (equal statement-syntax 'implementation)
      ;; Body: indent to bolp
      (setq new-icol 0)
      );end case: `implementation' keyword

     );end cases
    ;;
    ;; Last thing: output the new indentation column
    new-icol
    );end let
  )


(defun model-indent-line ()
  "The `indent-line' function for MODEL model.  Performs a syntactic
analysis, separate from the cc-mode one, then indents parts of MODEL
statements accordingly.  If the syntactic analysis turns up nothing, call
`old-c-indent-function' to indent the line."
  (let ((pos-from-end (- (point-max) (point)))
        return-to-pos indent-offset target-column
        );;End bindings
    (save-excursion
      ;; Perform syntactic analysis
      (model-syntactic-analysis)
      ;; Compute the indentation column, but only if we're in a MODEL
      ;; statement and not a comment.
      (if (not (or (equal 'other (nth 2 model-syntactic-context))
                   (model-point-inside-comment-block (point))
                   ))
          (setq target-column 
                (model-compute-indent-column model-syntactic-context))
        )
      );end excursion
    ;; Is target-column null?  If so, indent using c-mode.  Otherwise,
    ;; indent to target-column.
    (if (null target-column)
        (funcall old-c-indent-function)
      ;;else : indent
      (progn
        (setq indent-offset (- target-column (current-indentation)))
        (if (zerop indent-offset)
            ;; Don't indent, set return-to-pos to the current point,
            ;; and move to the indentation
            (progn (setq return-to-pos (point))
                   (back-to-indentation))
          ;;else: do indent
          (progn
            (back-to-indentation)
            ;;Delete old indentation if it's too long
            (if (< indent-offset 0) 
                (delete-region (save-excursion 
                                 (beginning-of-line) (point))
                               (point))
              )
            (indent-to target-column)
            (setq return-to-pos (- (point-max) pos-from-end))
            );end progn: indent
          );end if: zerop
        ;; If the original point is inside the indentation, go
        ;; back-to-indentation, just like in cc-mode.  (Note that
        ;; we're still at the indentation column.)
        (if (> return-to-pos (point))
            (goto-char return-to-pos))
        );end progn: indent model statement
      );end if
    );end let
  )


(defun model-show-syntactic-information ()
  "Interactive function that displays the results of a MODEL syntactic
analysis.

The output generated in the minibuffer is as follow:

syntax: (BOSP EOSP STYPE SCOL DSTRP BOAP ACOL EOAP)  indent: COLUMN

COLUMN is the column that the current line should be indented to.  A
value of `nil' indicates that the cc-engine will handle indentation of
this line.  Each element of the syntax list has the following meaning:

BOSP  - The point at the beginning of the statement, or the beginning
        of the current line if no model statement was found.

EOSP  - The point at the end of the statement, or the end
        of the current line if no model statement was found.

STYPE - A symbolp describing the statement.  Possible values are:

        'class = Start of a MODEL class.

        'member = Any member of a model class excluding operations,
                  member structs, and member enums.

        'operation = Any member operation in a model class.

        'implementation = The start of the implementation-block

        'other = Any member enum, struct, or any statement in the
                 implementation-block.  Note, however, that methods
                 containing no arglist will be identified as
                 `operation'.  This is a know bug and has no
                 deliterious effects.

SCOL  - The indentation column of the statement's first line, or `nil'
        for STYPE=='other.

DSTRP - The point at the beginning of the documentation string, or
        `nil' if there is none (exception: see below).  If the first
        documentation string is on its own line, this will be the
        point at the beginning of that line.

        When STYPE=='operation, this value is always the point after
        the ')' ending the arglist, whether or not there's any
        documentation string.

BOAP  - The point at the beginning of \"the assignment block\".  This
        is the first non-whitespace character after the first \"=>\",
        \"->\", \"<=\" or \"=\" token, or \"definition:\" keyword.

ACOL  - The value of `(current-column)' at BOAP.  Frequently used for
        indentation in the assignment-block.

EOAP  - The point at the end of the line where BOAP is located.
"
  (interactive)
  (model-syntactic-analysis)
  (save-excursion 
    (message 
     "syntax: %s  indent: %s" 
     model-syntactic-context
     (model-compute-indent-column model-syntactic-context))
    );end excursion
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODEL Mode Utility Functions
;;
;; Fill-Specific
;; 


(defun model-fill-string-block (startpos endpos &optional noisy)
  "Fill the string in the region between the points STARTPOS and
ENDPOS.  Each line in the region must contain only a single string for
this function to work properly.  Note, however, that a ';' character,
surrounded by any amount of whitespace, can follow any string.

This function strips all trailing whitespace from the end of the
string, as well as from the end of the line.  (This includes any
whitespace surrounding a terminating ';' character.)  After the fill,
each string will end with a single ' ' preceeding the \" (Note that it
only adds the space if the string does not end with \"\\n\".)

The optional argument NOISY turns on a progress dial when 't'."
  (save-restriction
    (narrow-to-region startpos endpos)
    (goto-char (point-min))
    (let* ((line1-indent (current-indentation))
           ;;Locally redefine fill-column to make space for the two "
           ;;the terminating space inside the string, and the indentation.
           (fill-column (- fill-column (+ 3 (current-indentation))))
           )
      ;; Strip all lines of their indentation, their ", and any
      ;; terminating whitespace.
      (while (re-search-forward model-fill-string-line-regexp nil t)
        (if noisy (model-progress-dial))
        (replace-match "\\2\\4")
        )
      (if noisy (model-progress-dial))
      ;; Refill everything in the narrowed-region
      (fill-region (point-min) (point-max) 'nil 't)
      (goto-char (point-min))
      (beginning-of-line)
      ;; Reformat the lines into strings again.  Adds a leading ", an
      ;; ending ", and, if the string doesn't end in the text "\n", a
      ;; space before the ending ".  Also indents the line.
      (while (re-search-forward model-fill-string-line-stripped-regexp nil t)
        (if noisy (model-progress-dial))
        (if (equal (match-string 3) "\\n")
            (replace-match "\"\\1\\2\"\\4")
          (replace-match "\"\\1\\2 \"\\4")
          )
        (if noisy (model-progress-dial))
        (indent-line-to line1-indent)
        )
      );end let
    (if noisy (model-progress-dial))
    );end restriction
  ;;Refontify everything, then move to the end of the region.
  (if font-lock-mode
      (font-lock-fontify-region startpos endpos))
  (goto-char endpos)
  )


(defun model-fill-string-paragraph (&optional noisy)
  "Fill the current \"string paragraph\".  See
`model-string-fill-region' for the definition of a string paragraph.

Returns t if there was a string paragraph to fill, nil otherwise."
  (interactive "*")
  (if (interactive-p) 
      (setq noisy t)
    )
  (if noisy
      (setq model-progress-message "Filling paragraph...")
    )
  (let ((string-line-regexp "^\\s *\".+\"[ \t;]*$")
        (string-parend-line-regexp "^.*\\\\n\".*$")
        (ends-with-newline nil) ;Does the string end with "\n"?
        (no-newline-found t) ;Loop-control variable.
        (bosp -1) ;Where the string-paragraph starts
        (eosp -1) ;Where the string-paragraph starts
        );end bindings
    (beginning-of-line)
    ;; Do something only if we're on a string-only line to begin with.
    (if (looking-at string-line-regexp)
        (progn
          ;;First, check if this line ends a string-paragraph.  If so,
          ;;set the appropriate variables, then move to the previous line.
          (if (looking-at string-parend-line-regexp)
              (progn (end-of-line)
                     (setq eosp (point)
                           ends-with-newline t)
                     (beginning-of-line)
                     (forward-line -1))
            );end if
          ;; Look for the beginning of the string paragraph.  Loops
          ;; until we hit the start of the buffer, until we find a
          ;; string ending in "\n", or we hit a non-string-line.
          (save-excursion (while (and (not (bobp))
                                      no-newline-found
                                      (looking-at string-line-regexp))
                            (if noisy (model-progress-dial))
                            (if (looking-at string-parend-line-regexp)
                                (setq no-newline-found 'nil)
                              (forward-line -1)
                              )
                            )
                          ;; Move back to the line that stopped the loop.
                          (forward-line)
                          (setq no-newline-found 't ; reset the flag
                                bosp (point))
                          );end excursion
          ;; Look for the end of the string paragraph, if we're not on
          ;; it already.  Loops until we hit the end of the buffer,
          ;; until we find a string ending in "\n", or we hit a
          ;; non-string-line.
          (and (not ends-with-newline) ;; don't look if already found
               (save-excursion (beginning-of-line)
                               (while (and (not (eobp))
                                           no-newline-found
                                           (looking-at string-line-regexp))
                                 (if noisy (model-progress-dial))
                                 (if (looking-at string-parend-line-regexp)
                                     (setq no-newline-found 'nil
                                           ends-with-newline 't)
                                   (forward-line)
                                   )
                                 )
                               ;; Move back to the line that stopped
                               ;; the loop, unless a "\n" ended the
                               ;; loop, in which case, we're still on
                               ;; the correct line.
                               (if no-newline-found
                                   (forward-line -1)
                                 )
                               (end-of-line)
                               (setq eosp (point))
                               );end excursion
               );end and
          ;;For lines containing only "\n" (plus whitespace), put eosp
          ;;at the line-begin.  Otherwise, we'll end up merging that
          ;;line with the previous one.
          (if ends-with-newline
              (save-excursion
                (goto-char eosp)
                (beginning-of-line)
                (if (looking-at "^\\s *\"\\s *\\\\n\"")
                    (setq eosp (point))
                  )
                )
            )
         ;; Perform the fill.
         (model-fill-string-block bosp eosp noisy)
         ;; Point to the next paragraph
         (forward-line)
         (beginning-of-line)
         (if (interactive-p) (message "Done."))
         t
         );end progn
      nil ;;If we weren't in a string-line to start with, return `nil'.
      );end if
    );end let
  )


(defun model-fill-paragraph (&optional arg)
  "The `fill-paragraph' function for MODEL model.  Places a call to
`model-fill-string-paragraph' first.  If that did nothing, it then calls
`old-c-fill-paragraph-function' to fill the current paragraph.  If
that also did nothing, returns `nil'.  Otherwise, it returns `t'."
  (interactive "*P")
  (or (and (model-fill-string-paragraph t)
           (message "Done.")
           )
      (funcall old-c-fill-paragraph-function arg)
      );end or
  )


(defun model-string-fill-region (startpos endpos)
  "Fill all of the \"string paragraphs\" in the marked region.  Makes
repeated calls to `model-fill-string-paragraph'.

A string paragraph is a sequence of lines each containing a single,
non-empty string.  The string paragraph starts and ends at either a
non-string line or at a string ending with \"\\n\".  Any string in the
middle of the string paragraph will end with a single ' '.

String-fill preserves any whitespace within the string.  It will,
however, delete any trailing whitespace.  See `model-fill-string-block'
for more information.

If a string contains only whitespace and the text \"\\n\", it receives
special handling.  Such a string is left on its own line, its leading
whitespace preserved, and no trailing ' ' is added to the end of the
line."
  (interactive "r")
  (save-restriction
    (narrow-to-region startpos endpos)
    (goto-char (point-min))
    (let ((noisy (interactive-p))
          );end bindings
      (if noisy (setq model-progress-message "Filling region..."))
      ;; Loop over all string-paragraphs in the marked region, until
      ;; we find a non-string-paragraph.
      (while (and (not (eobp))
                  (model-fill-string-paragraph noisy)))
      );end let
    );end restriction
  (if (interactive-p) (message "Done."))
  )


(defun model-do-auto-fill-string ()
  "Auto-fills line, giving special handling to a string-only line.
Unlike its cousins, `model-string-fill-region' and
`model-fill-string-paragraph', this one does not give any special
treatment to the characters \"\\n\".  Returns nil if no feasible place
to break the line could be found, and t otherwise."
  ;; If we're in a string-line without an ending ", add one.
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\s *\"[^\"]*$")
        (progn
          (end-of-line)
          (insert-char ?\" 1 t)
          )
        );end if
    );end excursion
  ;; Only do something if we're in a string-line, and if (point) is
  ;; past the fill column. Otherwise, we'll end up creating dozens of
  ;; tiny string-line fragments.
  (if (and (< fill-column (current-column))
           (model-point-inside-string (point)) )  
      ;; Locally redefine the fill-column to make room for the two "
      ;; and the terminating space.
      (let ( (fill-column (- fill-column 3))
             );end bindings
        ;; Make sure this line's been indented.
        (funcall indent-line-function)
        (save-excursion
          ;; Scan the string from end-to-beginning, looking for the
          ;; first whitespace character inside of the fill-column.
          (while (> (current-column) fill-column)
            (if (equal 0 (skip-chars-backward "^ \t"
                                              (line-beginning-position))
                       )
                (backward-char)
              );end if
            );end while
          (if (or (bolp)
                  (equal (following-char) ?\") )
              nil ; at start of string.  nowhere to break.
            ;;else 
            ;;     break the string, and indent the next line.
            (progn (insert-string "\"\n\"")
                   (funcall indent-line-function)
                   t ;; We broke the line; return true
                   )
            );end if
          );end excursion
        );end let
    ;;else
    ;;     We're not in a string-line.  Use the default auto-fill function.
    (do-auto-fill)
    );end if
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODEL Mode Function
;; 


;; Add the indentation style for MODEL-mode.
;;
(c-add-style "SMARTS-MODEL" model-mode-style-alist nil)



;;;###autoload
(defun model-mode ()
  "Major mode for editing SMARTS MODEL code.
This mode is a mix of the C++ mode, the CORBA-IDL mode, and custom stuff.

The hook variable `model-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also, the hook
`c-mode-common-hook' is run first.

The MODEL languages uses a sequence of single-line strings for class
and member documentation.  Since the strings do not span multiple
lines, the standard C++ mode auto-fill behavior is highly inconvenient
for entering MODEL documentation strings.  This major mode remedies
that, adding terminating '\"' and whitespace to each line as it is
wrapped.  See `model-do-auto-fill-string', `model-string-fill-region', and
especially `model-fill-string-paragraph' for an explanation of how
MODEL-mode auto-fill and paragraph-fill work.  (Also see the
documentation for the M-\" key, which is very handy...).

Indentation for this mode works slightly differently from C/C++ mode.
You can customize indentation of a line based on its syntax using
`c-set-offset'.  Do so from a function called by `model-mode-hook'; any
changes made by `c-mode-common-hook' are overridden.  There are also
four new variables:
    `model-statement-cont-offset'
    `model-docstring-offset'
    `model-assign-cont-offset'
    `model-definition-offset'
These variables, all of which have their own documentation, control
the indentation of various parts of MODEL statements.

DO NOT change the value of the `topmost-intro-cont' or
`statement-cont' syntaxes; you will break indentation and cause it to
do unpredictable things. 

Font-lock support for this mode comes in the traditional three levels.
Level-1 fontifies all preprocessor expressions, class definitions,
standard variable types, basic constants (like TRUE and FALSE) member
operations and member-declarative keywords.  Level-2 fontification
(the default) is an enhancement of level-1.  \"typedef\" and \"type\"
declarations receive fontification.  All type-specifiers and argument
names in operation declarations receive fontification.  Similarly, the
names of variables and type-specifications in attributes,
relationships, events, etc., all are fontified appropriately.  Level-3
adds all remaining MODEL keywords and types to the fontification scheme.
See the variable `font-lock-maximum-decoration' for more info about
setting the keyword fontification level. 

Key bindings:
\\{model-mode-map}"
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
  ;; Make several variables local, so we don't end up sharing them
  ;; with every other MODEL or C/C++ buffer.  This is especially
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
  (make-local-variable 'c-indentation-style)
  (make-local-variable 'c-offsets-alist)
  (make-local-variable 'c-basic-offset)
  (make-local-variable 'model-statement-cont-offset)
  (make-local-variable 'model-docstring-offset)
  (make-local-variable 'model-definition-offset)
  (make-local-variable 'model-assign-cont-offset)
  (make-local-variable 'old-c-fill-paragraph-function)
  (make-local-variable 'old-c-indent-function)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'fill-paragraph-function)
  (make-local-variable 'fill-column)
  (make-local-variable 'auto-fill-function)
  (make-local-variable 'normal-auto-fill-function)
  (if (boundp 'c-extra-toplevel-key)
      (make-local-variable 'c-extra-toplevel-key))
  (set-syntax-table model-mode-syntax-table)
  (setq major-mode 'model-mode
	mode-name "SMARTS-MODEL"
	local-abbrev-table c++-mode-abbrev-table)
  (use-local-map model-mode-map)
  (c-common-init)
  ;; After performing common c-mode initialization, customize some of
  ;; the emacs-cc-mode-engine control variables.
  (setq comment-start "// "
	comment-end ""
	c-conditional-key c-MODEL-conditional-key
	c-comment-start-regexp c-MODEL-comment-start-regexp
	c-class-key c-MODEL-class-key
     c-baseclass-key c-MODEL-baseclass-key
	c-access-key c-MODEL-access-key
     c-label-key c-MODEL-definition-regexp
     c-switch-label-key c-MODEL-bogus-regexp    ; deactivate
	c-recognize-knr-p nil
     c-extra-toplevel-key c-MODEL-extra-toplevel-key
     c-special-indent-hook nil
     ;; Set the storage variables for the fill-par and indent-line
     ;; functions.
     old-c-fill-paragraph-function fill-paragraph-function
     old-c-indent-function indent-line-function
     ;;
     indent-line-function 'model-indent-line
     indent-region-function nil
     fill-paragraph-function 'model-fill-paragraph
     normal-auto-fill-function 'model-do-auto-fill-string
     auto-fill-function 'model-do-auto-fill-string
	imenu-generic-expression cc-imenu-c++-generic-expression
        imenu-case-fold-search nil)
  (model-maybe-provide-keywords)
  (run-hooks 'c-mode-common-hook)
  ;; After running the common mode hooks, set the indentation style.
  ;; This forces use of the MODEL-mode indentation style, which is the
  ;; only valid one.
  (c-set-style "SMARTS-MODEL")
  (run-hooks 'model-mode-hook)
  (c-update-modeline))

(provide 'model-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODEL Mode Utility Functions
;;
;; Font-Lock Support and Setup

(require 'font-lock)


;; Set up font-lock-preprocessor-face for model-mode for recent GNU
;; versions of Emacs
;;
(if (not (boundp 'font-lock-preprocessor-face))
    (if (boundp 'font-lock-builtin-face)
        (defvar font-lock-preprocessor-face 'font-lock-builtin-face)
      ;;else
      ;; use keyword face
      (defvar font-lock-preprocessor-face 'font-lock-keyword-face)
      )
  )


;; Set up font-lock-constant-face for XEmacs and older GNU emacs
;;
(if (not (boundp 'font-lock-constant-face))
    (if (boundp 'font-lock-reference-face)
        (progn
          (defvar font-lock-constant-face 'font-lock-reference-face)
          (if (string-match "XEmacs" emacs-version)
              (copy-face font-lock-reference-face
                         'font-lock-constant-face) )
          )))


;;
;; Keys common to all levels.
;;


(defconst model-font-lock-cpp-key
  (list
   ;; Fontify filenames in #include <...> preprocessor directives as strings.
   '("^#[ \t]*\\(include\\)[ \t]+\\(<[^>\"\n]+>\\)?" 
     (1 font-lock-preprocessor-face)
     (2 font-lock-string-face nil t))
   ;; Fontify #pragma include <...> preprocessor directives correctly.
   '("^#[ \t]*\\(pragma\\)[ \t]+\\(include_[a-z]+\\)[ \t]+\\(<[^>\"\n]+>\\)?"
     (1 font-lock-preprocessor-face)
     (2 font-lock-preprocessor-face)
     (3 font-lock-string-face nil t))
   ;; Fontify other #pragma <...> preprocessor directives.
   '("^#[ \t]*\\(pragma\\)[ \t]*\\([-a-zA-Z0-9_ \t]+\\)*"
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))
   ;; Fontify function macro names.
   '("^#[ \t]*\\(define\\)[ \t]+\\(\\(\\sw+\\)(\\)" 
     (1 font-lock-preprocessor-face)
     (3 font-lock-function-name-face))
   ;; Fontify symbol names in #if ... defined preprocessor directives.
   '("^#[ \t]*if\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-preprocessor-face) (2 font-lock-variable-name-face nil t)))
   ;; Fontify symbol names in #elif ... defined preprocessor directives.
   '("^#[ \t]*elif\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-preprocessor-face) (2 font-lock-variable-name-face nil t)))
   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
   (list
    (concat
     "^#[ \t]*\\(define\\|e\\(l\\(if\\|se\\)\\|ndif\\)\\|"
     "if\\(\\(n\\)?def\\)?\\)\\>[ \t]*\\(\\sw+\\)?")
     '(1 font-lock-preprocessor-face) 
     '(6 font-lock-variable-name-face nil t))
   );end list
  ) ;; end defconst


(defconst model-font-lock-classlike-key
  (list
   ;; Fontify struct and enum declarations.
   '("\\<\\(struct\\|enum\\)\\s +\\(\\sw*\\)\\s *{"
     (1 font-lock-type-face) (2 font-lock-variable-name-face))
   ;; Fontify MODEL classes (interfaces).
   ;; `c-MODEL-class-start-regexp' contains 2 \\(\\) groups.
   (list
    (concat "\\<\\(" c-MODEL-class-start-regexp "\\)\\(\\sw*"
            "\\)\\(\\s *[:]\\s *\\(\\sw*\\)\\)?")
    '(1 font-lock-type-face)
    '(4 font-lock-function-name-face)
    '(6 font-lock-variable-name-face nil t))
   );end list
  ) ;; end defconst


;;
;; Keywords and full keys for level-1 fontification.
;;


(defconst model-constant-face-keywords-1
  (list
   (concat
    "\\<\\("
    "FALSE\\|"
    "TRUE\\|"
    "i\\(mplementation\\|n\\(out\\)?\\)\\|" ;; implementation, in, inout
    "out"
    "\\)\\>"
    ) ;; end concat
   '(1 font-lock-constant-face)
   );end list
  ) ;; end defconst


(defconst model-keyword-face-keywords-1
  (list
   (concat "\\<\\(" c-MODEL-member-keyword-regexp "\\|return\\)\\>")
   '(1 font-lock-keyword-face)
   );end list
  )


(defconst model-type-face-keywords-1 
  (list
   (concat
    ;;"bool" "boolean" "char" "complex" "const" "double" "error_code"
    ;;"float" "int" "long" "set" "short" "signed" "static" "string"
    ;;"unsigned" "void"
    "\\<\\("
    "bool\\(lean\\)?\\|c\\(har\\|o\\(mplex\\|nst\\)\\)\\|"
    "double\\|error_code\\|float\\|int\\|long\\|"
    "s\\(et\\|hort\\|igned\\|t\\(atic\\|ring\\)\\)\\|"
    "unsigned\\|void"
    "\\)\\>"
    ) ;; end concat
   '(1 font-lock-type-face)
   );end list
  ) ;; end defconst


(defconst model-function-face-keywords-1
  ;; Note that the type can be of the form "set(<typeSpec>)".
  '("\\<\\(\\sw*\\)?)?\\s +\\(\\sw*\\)(\\(in\\(out\\)?\\s +\\|out\\s +\\|)\\)"
    (2 font-lock-function-name-face))
  )


(defconst model-font-lock-keywords-1 
  (append
   ;; Always use the cpp and class-def expressions
   model-font-lock-cpp-key
   model-font-lock-classlike-key
   ;; Now add all of the level-1 expressions
   (list
    model-constant-face-keywords-1
    model-keyword-face-keywords-1
    model-type-face-keywords-1
    model-function-face-keywords-1
    );end list
   );end append
  )


;;
;; Keywords and full keys for level-2 fontification.
;;


(defconst model-constant-face-keywords-2
  (list
   (concat
    "\\<\\("
    "FALSE\\|"
    "TRUE\\|"
    "implementation"
    "\\)\\>"
    ) ;; end concat
   '(1 font-lock-constant-face)
   );end list
  ) ;; end defconst


(defconst model-font-lock-arg-key-2
  (list
   '("\\<\\(in\\(out\\)?\\|out\\)\\>\\s *\\(\\sw*\\)\\s *\\(\\sw*\\)"
    (1 font-lock-constant-face)
    (3 font-lock-type-face nil t)
    (4 font-lock-variable-name-face nil t)
    ));end list
  )


(defconst model-type-face-key-2
  (list
   ;;Plain keywords
   ;;
   model-type-face-keywords-1
   ;; Add complex syntax for 'type(def)'
   ;; 
   '("\\<\\(type\\(def\\)?\\)\\>\\s *\\([\"][^\"]*[\"]\\)?\\s *\\(\\sw*\\)?"
     (1 font-lock-keyword-face) 
     (4 font-lock-variable-name-face nil t))
   );end list
  )


(defconst model-function-face-keywords-2
  ;; Note that the type can be of the form "set(<typeSpec>)".  For
  ;; level-2 fontification, we fontify this typespec, too.
  '("\\<\\(\\sw*\\)?)?\\s +\\(\\sw*\\)(\\(in\\(out\\)?\\s +\\|out\\s +\\|)\\)"
    (1 font-lock-type-face) (2 font-lock-function-name-face))
  )


(defconst model-keyword-face-key-2-complex-expr
  (list
   ;; Keywords followed by an id.
   (list
    (concat
     "\\<\\(aggregate\\|constraint\\|event\\|problem\\|symptom\\)\\>"
     "\\s +\\(\\sw*\\)")
    '(1 font-lock-keyword-face)
    '(2 font-lock-variable-name-face))
   ;; Fragments get special handling
   (list
    (concat "\\<\\(fragment\\)\\s +\\(\\(\\sw*\\)[^(]\\)?")
    '(1 font-lock-keyword-face)
    '(2 font-lock-variable-name-face nil t))
   ;; Handle refine's next.  Only fontify the `refine' keyword and the
   ;; variable name; skip over any other keywords that can appear
   ;; in-between.
   (list
    (concat "\\<\\(refine\\)\\>\\(\\s +"
            "\\(a\\(ttribute\\|ggregate\\)\\|computed\\|event\\|"
            "instrumented\\|pro\\(blem\\|pagate\\)\\|"
            "s\\(tored\\|ymptom\\)\\|uncomputable\\)"
            "\\)*\\s *\\(\\<\\sw*\\)?")
    '(1 font-lock-keyword-face)
    '(7 font-lock-variable-name-face nil t))
   ;; These next three need special handling, since a linebreak could
   ;; occur at any point after the keyword.
   (list
    (concat
     "\\<\\(attribute\\)\\>\\s *\\(\\(unsigned\\s +\\)?\\sw*\\)?\\s *"
     "\\([[].*[]]\\)?\\s *\\("
     "\\(a\\(nd\\|vg\\)\\|m\\(ax\\|in\\)\\|or\\|prod\\|sum\\)\\s +\\)?"
     "\\(\\sw*\\)?")
    '(1 font-lock-keyword-face)
    '(2 font-lock-type-face nil t)
    '(9 font-lock-variable-name-face nil t)
    )
   (list
    (concat
     "\\<\\(relationship\\(set\\)?\\)\\>\\s *"
     "\\(\\sw*\\)?[,]?\\s *\\(\\sw*\\)?")
     '(1 font-lock-keyword-face)
     '(3 font-lock-variable-name-face nil t)
     '(4 font-lock-type-face nil t))
   ;; Propagate tables follow the same regexp.
   '("\\<\\(table\\)\\>\\s *\\(\\sw*\\)?\\s *\\(\\(unsigned\\)?\\sw*\\)?"
     (1 font-lock-keyword-face) (2 font-lock-variable-name-face nil t)
     (3 font-lock-type-face nil t))
   );end list
  )


(defconst model-keyword-face-key-2
  (append
   (list
    ;; Plain keywords
    '("\\<\\(definition\\|export\\|instrument\\|propagate\\|return\\)\\>" 
      (1 font-lock-keyword-face))
    );end list
   ;; Add on the more complex expressions involving keyword-face
   model-keyword-face-key-2-complex-expr
   );end append
  )


(defconst model-font-lock-keywords-2 
  (append
   ;; Always use the cpp and class-def expressions
   model-font-lock-cpp-key
   model-font-lock-classlike-key
   ;; Now add all of the level-2 expressions
   (list model-constant-face-keywords-2)
   model-type-face-key-2
   model-keyword-face-key-2
   (list model-function-face-keywords-2)
   model-font-lock-arg-key-2
   );end append
  )


;;
;; Keywords and full keys for level-3 fontification.
;;


(defconst model-constant-face-keywords-3
  (list
   (concat
    "\\<\\("
    "FALSE\\|"
    "TRUE\\|"
    "_time\\|"
    "implementation\\|"
    "self\\|"
    "timestamp"
    "\\)\\>"
    ) ;; end concat
   '(1 font-lock-constant-face)
   );end list
  ) ;; end defconst


(defconst model-function-face-key-3
  (list 
   model-function-face-keywords-2
   ;; Add C++-style function declarations and names.
   '("^\\s *\\sw+\\s +\\(\\sw+\\)\\(::\\(\\sw+\\)\\)?[ \t]*("
     (1 (if (match-beginning 2)
            font-lock-type-face
          font-lock-function-name-face))
     (3 (if (match-beginning 2) 
            font-lock-function-name-face) nil t))
   '("^\\(\\sw+\\)\\(::\\(\\sw+\\)\\)?[ \t]*("
     (1 (if (match-beginning 2)
            font-lock-type-face
          font-lock-function-name-face))
     (3 (if (match-beginning 2) 
            font-lock-function-name-face) nil t))
   );end list
  )


(defconst model-type-face-key-3
  (append
   (list
    ;;Plain keywords
    ;;
    ;;"SNMP" "ascending" "bool" "boolean" "char" "complex" "computed"
    ;;"const" "const\\s +value" "descending" "double" "error_code" "float"
    ;;"hard" "idempotent" "imported" "instrumented" "instrumented_op" "int"
    ;;"internal" "long" "readonly" "required" "set" "short" "signed" "soft"
    ;;"static" "stored" "string" "timestamped" "uncomputable" "unique"
    ;;"unsigned" "unstringable" "void"
    ;;
    ;; "mr_\\sw+" "sm_\\sw+"
    ;; "MR_\\sw" "SM_\\sw"
    (list
     (concat
      "\\<\\("
      "MR_\\sw\\|S\\(M_\\sw\\|NMP\\)\\|"
      "ascending\\|bool\\(ean\\)?\\|"
      "c\\(har\\|o\\(mplex\\|mputed\\|nst\\(\\s +value\\)?\\)\\)\\|"
      "d\\(escending\\|ouble\\)\\|e\\(rror_code\\|xternal\\)\\|"
      "f\\(loat\\|unction\\)\\|hard\\|"
      "i\\(dempotent\\|mported\\|"
      "n\\(strumented\\(_op\\)?\\|t\\(ernal\\)?\\)\\)\\|"
      "long\\|mr_\\sw+\\|re\\(adonly\\|quired\\)\\|"
      "s\\(et\\|hort\\|igned\\|m_\\sw+\\|oft\\|t\\(atic\\|ored\\|ring\\)\\)\\|"
      "timestamped\\|un\\(computable\\|ique\\|s\\(igned\\|tringable\\)\\)\\|"
      "void" 
      "\\)\\>"
      ) ;; end concat
     '(1 font-lock-type-face))
    );end list
   ;; Get complex syntax for 'type(def)' from level-2
   ;; 
   (cdr model-type-face-key-2)
   );end append
  ) ;; end defconst


(defconst model-keyword-face-key-3
  (append
   (list
    ;; Add more words to the list of plain keywords.
    ;;
    ;;"and" "apriori" "avg" "check" "count" "definition" "delta"
    ;;"events_only" "explains" "export" "foreach" "function" "if"
    ;;"instrument" "key" "loss" "max" "min" "obj" "old" "or"
    ;;"polling_frequency" "prod" "propagate" "proxy" "rate" "return"
    ;;"row" "spurious" "sum" "with\\s +attributes"
    (list
     (concat
      "\\<\\("
      "a\\(nd\\|priori\\|vg\\)\\|c\\(heck\\|ount\\)\\|"
      "de\\(finition\\|lta\\)\\|e\\(vents_only\\|x\\(plains\\|port\\)\\)\\|"
      "f\\(oreach\\|unction\\)\\|i\\(f\\|nstrument\\)\\|key\\|loss\\|"
      "m\\(ax\\|in\\)\\|o\\(bj\\|ld\\|r\\)\\|"
      "p\\(olling_frequency\\|ro\\(d\\|pagate\\|xy\\)\\)\\|"
      "r\\(ate\\|eturn\\|ow\\)\\|s\\(purious\\|um\\)\\|"
      "with\\s +attributes"
      "\\)\\>")
      '(1 font-lock-keyword-face))
    );end list
   ;; Add on the more complex expressions involving keyword-face
   model-keyword-face-key-2-complex-expr
   );end append
  )


(defconst model-font-lock-keywords-3
  (append
   ;; Always use the cpp and class-def expressions
   model-font-lock-cpp-key
   model-font-lock-classlike-key
   ;; Add any level-2 expressions that are used here
   model-font-lock-arg-key-2
   ;; Now add all of the level-3 expressions
   (list model-constant-face-keywords-3)
   model-type-face-key-3
   model-keyword-face-key-3
   model-function-face-key-3
   );end append
  )


;; The defaults for font-lock in model-mode
;;
(defconst model-font-lock-defaults
   (list
    '(model-font-lock-keywords-2 model-font-lock-keywords-1
      model-font-lock-keywords-2 model-font-lock-keywords-3)
    nil nil
    '((?_ . "w") (?~ . "w"))
    'beginning-of-defun
    '(font-lock-comment-start-regexp . "/[*/]")   
    '(font-lock-mark-block-function . mark-defun)
    );end list
  )


;; Set up font-lock for model-mode, taking differeces between XEmacs and
;; GNU Emacs into account.  Only need to do this once.
;;
(if (boundp 'font-lock-defaults-alist)
    (if (assq 'model-mode font-lock-defaults-alist)
        ;; If there's already an entry for this mode, just change it.
        (setcdr (assq 'model-mode font-lock-defaults-alist)
                model-font-lock-defaults)
      ;;else
      ;; Add it if there's no entry.
      (setq font-lock-defaults-alist
            (append font-lock-defaults-alist
                    (list (cons 'model-mode model-font-lock-defaults))
                    );end append
            );end setq
      );end if
  ;;else
  (put 'model-mode 'font-lock-defaults model-font-lock-defaults)
  );end if


;; This is needed by XEmacs' version of font-lock, which does not
;; define `font-lock-defaults-alist'
;;
(defun model-maybe-provide-keywords ()
  (if (not (boundp 'font-lock-defaults-alist))
      (progn
        (make-local-variable 'font-lock-defaults)
        ;; Set the defaults:  highlight rule-names in rule-definitions.
        (setq font-lock-defaults
              (get 'model-mode 'font-lock-defaults))
        );end progn
    );end if
  )


;;
;; Constants for the older version of MODEL font-lock
;;


; (defconst model-keyword-face-keywords 
;   (concat
;    "\\<\\("
;    "SNMP\\|"
;    "_time\\|"
;    "a\\(bstract\\|nd\\|priori\\|scending\\|vg\\)\\|"
;    "c\\(heck\\|o\\(mputed\\|nstraint\\|unt\\)\\)\\|"
;    "de\\(finition\\|lta\\|scending\\)\\|"
;    "e\\(vents_only\\|x\\(plains\\|port\\)\\)\\|"
;    "f\\(oreach\\|ragment\\)\\|"
;    "hard\\|"
;    "i\\(f\\|mported\\|n\\(strument\\(ed\\(_op\\)?\\)?\\|ternal\\)\\)\\|"
;    "loss\\|"
;    "m\\(ax\\|min\\)\\|"
;    "o\\(ld\\|r\\)\\|"
;    "p\\(olling_frequency\\|ro\\(d\\|pagate\\|oxy\\)\\)\\|"
;    "r\\(ate\\|e\\(adonly\\|fine\\|quired\\)\\|ow\\)\\|"
;    "s\\(oft\\|purious\\|tored\\|um\\)\\|"
;    "t\\(imestamp\\(ed\\)?\\|raced\\)\\|"
;    "un\\(computable\\|ique\\)\\|"
;    "w\\(ith\\s +attributes\\)\\|"
;    "\\)\\>"
;    ) ;; end concat
;   ) ;; end defconst


; (defconst model-type-face-keywords 
;   (concat
;    "\\<\\("
;    "a\\(ggregate\\|ttribute\\)\\|"
;    "boolean\\|"
;    "c\\(onst\\s +value\\)\\|"
;    "e\\(rror_code\\|vent\\|xternal\\)\\|"
;    "function\\|"
;    "i\\(dempotent\\|nterface\\)\\|"
;    "key\\|"
;    "problem\\|"
;    "relationship\\(set\\)?\\|"
;    "s\\(et\\|tring\\|ymptom\\)\\|"
;    "t\\(able\\|ype\\)\\|"
;    "unstringable\\|"
;    "\\)\\>"
;    ) ;; end concat
;   ) ;; end defconst


; (defconst model-font-lock-keywords-1-old 
;   (append
;    (list 
;     (cons model-constant-face-keywords-1 'font-lock-constant-face)
;     );end list
;    c++-font-lock-keywords-1
;    );end append
;   )


; (defconst model-font-lock-keywords-2-old
;   (append
;    (list 
;     (car model-font-lock-keywords-1-old)
;     (cons model-type-face-keywords 'font-lock-type-face)
;     );end list
;    c++-font-lock-keywords-2
;    );end append
;   )


; (defconst model-font-lock-keywords-3-old
;   (append
;    (list 
;     (car model-font-lock-keywords-2-old)
;     (car (cdr model-font-lock-keywords-2-old))
;     (cons model-keyword-face-keywords 'font-lock-keyword-face)
;     );end list
;    c++-font-lock-keywords-3
;    );end append
;   )




;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;