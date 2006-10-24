;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ExpressionEvaluator Mode
;;
;;  Copyright © 2006 John P. Weiss
;;
;;  Donated to royalblue Corporation under the Artistic License.
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
;; A programming mode for royalblue's "ExpressionEvaluator" language.
;;
;;  
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'font-lock)
(require 'jpw-indent-tools)
(eval-when-compile
  (require 'lazy-lock)
  (require 'jit-lock))


;;------------------------------------------------------------
;;
;; User Customizations
;; 


(defgroup ee-mode nil
  "Customizable aspects of ee-generic-mode.
{jpw: 10/05}"
  :group 'languages
  :group 'local)

;;:type:
  ;; One of: integer, number, string, regexp, character, hook,
  ;;         symbol, function, variable, boolean


(defcustom ee-mode-hook nil
  "*Hook called by `ee-mode'.
{jpw: 10/05}"
  :type 'hook
  :group 'ee-mode)


(defcustom ee-mode-always-indent-line nil
  "If non-nil, then `ee-mode-indent-line' will always indent the entire line,
no matter where `point' may be located.

Otherwise, `ee-mode-indent-line' will indent relatively if `point' is outside
of the indentation column.  \(Note that being at the indenation column counts
as \"inside\".\)
{jpw: 10/06}"
  :type '(boolean variable)
  :group 'ee-mode)


(defcustom ee-mode-block-indent 4
  "Indentation level for any form of block.
{jpw: 10/05}"
  :type '(integer variable)
  :group 'ee-mode)


(defcustom ee-mode-statement-continuation-indent 4
  "Indentation level for the additional lines in statements spanning multiple
lines.
{jpw: 10/05}"
  :type '(integer variable)
  :group 'ee-mode)


(defcustom ee-mode-comma-extra-indent 0
  "Indentation level for statements inside of one of the \"language-element\"
functions.

Usually, the only thing at this level is a line containing only the
\"syntactic comma\" that separates clauses in the \"language-element\"
functions, hence the name.
{jpw: 10/05}"
  :type '(integer variable)
  :group 'ee-mode)


(defface ee-mode-syntactic-punctuation-face 
  '((t (:inherit font-lock-builtin-face)))
  "Fontification for punctuation that has syntactic meaning in Expression
Evaluator, such as \";\" and \",\".
{jpw: 10/05}"
  :group 'ee-mode)


(defface ee-mode-map-function-face 
  '((t (:inherit font-lock-function-name-face)))
  "The face to use for \"XRef\" map functions.
{jpw: 10/05}"
  :group 'ee-mode)


(defface ee-mode-language-function-face 
  '((t (:inherit font-lock-builtin-face)))
  "The face to use for \"functions\" that are really Expression Evaluator
language elements.
{jpw: 10/05}"
  :group 'ee-mode)


(defface ee-mode-globals-face 
  '((t (:inherit font-lock-constant-face)))
  "The face to use for certain global Expression Evaluator variables and
expressions.
{jpw: 10/05}"
  :group 'ee-mode)


;;------------------------------------------------------------
;;
;; Language Elements
;;


(defconst ee-mode-type-re
  "\\(double\\|int\\|string\\)"
  "Expression Evaluator data types.  Found mainly in typecast expressions.
{jpw: 10/05}")


(defconst ee-mode-typecast-re
  (concat "{" ee-mode-type-re "}")
  "Expression Evaluator typecast expression.
{jpw: 10/05}")


(defconst ee-mode-language-fn-list
  '("AddFn"
    "AndExpr"
    "ExactExpr"
    "For"
    "If"
    "OrExpr"
    "StartsWithExpr"
    "WildcardExpr")
  "\"Functions\" that are really Expression Evaluator language elements.
{jpw: 10/05}")
(defconst ee-mode-language-fn-re
  (eval-when-compile
    (concat "\\<" (regexp-opt ee-mode-language-fn-list t))
    ))


(defconst ee-mode-keyword-list
  '("Abs"
    "ByteAt"
    "CheckAccessRight"
    "Chr\044"
    "DateAdd\044"
    "DateDiff"
    "DatePart"
    "DatePart\044"
    "DateTimeAdd\044"
    "DateTimeDiff"
    "Date\044"
    "DbGet"
    "DbGetFirst"
    "DbGetFirstInGroup"
    "DbGetGT"
    "DbGetGTE"
    "DbGetLT"
    "DbGetLTE"
    "DbGetLast"
    "DbGetLastInGroup"
    "DbGetNextInGroup"
    "DbGetPrevInGroup"
    "DbOpen"
    "DctDateTimeAdd"
    "DctDateTimeDiff"
    "DctDateTimeFromString"
    "DctDateTimeNow"
    "DctDateTimeToString"
    "DctDateTimeToZone"
    "DctFromShortString"
    "DctToShortId"
    "Display"
    "ErrGetField"
    "ErrGetLogging"
    "ErrGetName"
    "ErrGetSeverity"
    "ErrGetText"
    "ErrInitialize"
    "ExprToken"
    "ExprTokenCount"
    "FracPrice"
    "GetEnv\044"
    "GetUserName"
    "HostName"
    "In\044"
    "IsDctConstantValue"
    "IsNull"
    "Keyword"
    "LTrim\044"
    "Left\044"
    "Len\044"
    "LogDataOperator\044"
    "LogOperator\044"
    "LogTrace\044"
    "MapAdd"
    "MapDelete"
    "MapGet"
    "Match"
    "Mid\044"
    "OAServiceInfo"
    "QPrice"
    "RTrim\044"
    "RecCopyRecord"
    "RecCreate"
    "RecDelete"
    "RecDump"
    "RecGetDoubleField"
    "RecGetField"
    "RecGetIntField"
    "RecGetRecordNumber"
    "RecSetField"
    "RecSetRecordNumber"
    "Replace\044"
    "Right\044"
    "Round"
    "Sds_Dump"
    "Sds_FieldList"
    "Sds_GetDouble"
    "Sds_GetInt"
    "Sds_GetRepCount"
    "Sds_GetRepField"
    "Sds_GetString"
    "Sds_Remove"
    "Sds_RemoveField"
    "Sds_Set"
    "Sds_SetRepDoubleField"
    "Sds_SetRepIntField"
    "Sds_SetRepStringField"
    "ShellCmd"
    "Sprintf"
    "TimeAdd\044"
    "TimeDiff"
    "Time\044"
    "ToGMTDate\044"
    "ToGMTTime\044"
    "ToLocalDate\044"
    "ToLocalTime\044"
    "ToLower\044"
    "ToUpper\044"
    "TokenCount"
    "Token\044"
    "Translate\044"
    "exp"
    "log"
    "log10")
  "Expression Evaluator keywords.
{jpw: 10/05}"
  )
(defconst ee-mode-keyword-re
  (eval-when-compile
    (concat "\\<" (regexp-opt ee-mode-keyword-list t))
    ))


(defconst ee-mode-globals-list
  '("rds" "sds" "txn" "updateIdx" "updateOp")
  "Expression Evaluator global vars & expressions.
{jpw: 10/05}")
(defconst ee-mode-globals-re
  (eval-when-compile
    (concat "\\<" (regexp-opt ee-mode-globals-list t) "\\>")
    ))


(defconst ee-mode-generic-blockdefn-list
  '("default" "name")
  "Expression Evaluator keywords that always begin a block definition.
{jpw: 10/05}")

(defconst ee-mode-expression-defn-list
  '("dll" "expression" "preExpression" "xrefTables")
  "Expression Evaluator keywords that begin certain expression blocks.
{jpw: 10/05}")

(defconst ee-mode-error-defn-list
  '("messages" "error" "language" "warning")
  "Expression Evaluator keywords that begin a block defining errors, warnings,
or other form of messages.
{jpw: 10/05}")

(defconst ee-mode-linkmgr-defn-list
  '("address" 
    "applyDefaultToLocal"
    "capabilities"
    "compressLevel"
    "linkMgr"
    "onDatabaseAvailable"
    "onDatabaseRecoverable"
    "service"
    "stateLink")
  "Expression Evaluator keywords that start link-manager-related blocks.
{jpw: 10/05}")

(defconst ee-mode-db-defn-list
  '("alias"
    "backwardJoinKey"
    "backwardJoinWhere"
    "constant"
    "field"
    "joinIf"
    "joinKey"
    "joinType"
    "record"
    "table"
    "updateQueue"
    "value"
    "where")
  "Expression Evaluator keywords that start database-related blocks.
{jpw: 10/05}")

(defconst ee-mode-blockelement-re
  (eval-when-compile
    (concat "\\<" 
            (regexp-opt 
             (sort
              ;; The order of the following is important, since:
              ;; (1) `append' doesn't copy its last arg
              ;; (2) `sort' modifies the list passed to it.
              (append ee-mode-generic-blockdefn-list
                      ee-mode-expression-defn-list
                      ee-mode-error-defn-list
                      ee-mode-linkmgr-defn-list
                      (copy-list ee-mode-db-defn-list))
              'string-lessp)
             )
            )
    ) ;; end eval-when-compile
  ) ;;end defconst


(defconst ee-mode-comment-re
  ;; Not to be confused with `ee-mode-font-lock-comment-re', which is defined
  ;; below.
  ;; Any modificaitons to this const should be reflected in
  ;; `ee-mode-font-lock-comment-re' too.
  "!\\(==\\|[^=]\\|$\\)")


(defconst ee-mode-fill-prefix-re
  ;; Controls the behavior of auto-fill.
  "[ 	]*\\(![ 	]*\\([-:#*|+.][ 	]*\\)?\\)")


(defconst ee-mode-paragraph-start-re
  ;; How a paragraph starts in ee-mode
  "[ 	]*\\([!\({,}]\\|\)[,;]\\)?[ 	]*$\\|^\f")


(defconst ee-mode-var-re
  "\\<\\(\\w+\\)\\(##?[sid]\\)\\>")


;;
;; Syntax/indentation-only expressions.
;; 
;; All of these regexps are designed for use in `looking-at', after a call to
;; `back-to-indentation'.
;;


(defconst ee-mode-expr-opening-quote-re "\"\\([^,\;\"]?[^\"]+\\)$")

(defconst ee-mode-expr-closing-quote-re "\"[,\;]\\s *$")


;;------------------------------------------------------------
;;
;; Other Variables
;; 


(defvar ee-mode-syntax-table
  (let ((nutable (copy-syntax-table)))
    (modify-syntax-entry ?_  "_" nutable)
    (modify-syntax-entry ?(  "(" nutable)
    (modify-syntax-entry ?)  ")" nutable)
    (modify-syntax-entry ?{  "(" nutable)
    (modify-syntax-entry ?}  ")" nutable)
    (modify-syntax-entry ?[  "(" nutable)
    (modify-syntax-entry ?]  ")" nutable)
    (modify-syntax-entry ?'  "$" nutable)
    (modify-syntax-entry ?\" "$" nutable)
    (modify-syntax-entry ?\\ "\\" nutable)
    nutable);;end let
  "Syntax table for use in ee-mode buffers.
Makes '_' into a symbol character, defines all parens with the correct syntax,
and turns \' and \" into paired delimiters, instead of string quote
delimiters.")


;;------------------------------------------------------------
;;
;; Utility Functions
;; 


;;
;; Inlines
;;


(defsubst ee-mode-at-or-inside-comment ()
  ;; Wraps `comment-beginning' in an excursion, and also checks if we're at
  ;; the start of a comment or inside the margin before a comment-only line.
  (save-excursion 
    (or (comment-beginning)
        (progn (back-to-indentation)
               (looking-at ee-mode-comment-re))
        )
    )
  )


(defsubst ee-mode-syntax-in-xref-table (parent-block-type
                                        parent-block-posn
                                        parent-statement-name)
  ;; Returns non-`nil' if point is inside of an XRef table.
  (and (eq parent-block-type ?{)
       (or (string-match ee-mode-blockelement-re parent-statement-name)
           (save-excursion 
             (goto-char (1+ parent-block-posn))
             (and (re-search-forward "[{}]" nil t)
                  (backward-char))
             ;; Skip the closing }, and search backward for any
             ;; chars that indicate an internal statement.
             (re-search-backward "[][(){},;!&|=]" parent-block-posn t)
             ;; If the only thing we find is the { opening this
             ;; block, then we must be inside of a table.
             (= (point) parent-block-posn)
             );; end excursion
           ) ;; end or
       ) ;; end and
  )


(defsubst ee-mode-syntax-follows-comment-line ()
  ;; Returns non-`nil' if the previous line begins with a "!"
  (save-excursion
    ;; FIXME:  We should really cache the syntactic information of the
    ;; previous line.
    (jpw-next-nonblank-line -1)
    (ee-mode-at-or-inside-comment)
    );; end excursion
  )


;;(defsubst 
;;  )


;;
;; Regular Functions
;;


(defun ee-comment-indent (&optional fixed-body-indent)
  (if (jpw-indent-comment fixed-body-indent) 
      0)
  )


(defun ee-mode-syntactic-analysis ()
;; FIXME:  This should have a wrapper ... or something ... to prevent it from
;; being called when we're in the whitespace in-between two comments.
  "Return a list containing syntactic information about the code at `point'.
the list elements and structure are as follows:
    (CURRENT-STATEMENT-TYPE 
          (PARENT-BLOCK-TYPE
           PARENT-BLOCK-INDENT
           PARENT-BLOCK-PAREN-COLUMN
           (PARENT-BLOCK-LINE-BEGIN PARENT-BLOCK-LINE-END)
           PARENT-BLOCK-POSN)
          (PARENT-BLOCK-STATEMENT-NAME
           PARENT-BLOCK-STATEMENT-INDENT
           PARENT-BLOCK-STATEMENT-IS-QUOTED
           (PARENT-BLOCK-STATEMENT-LINE-BEGIN PARENT-BLOCK-STATEMENT-LINE-END)
           PARENT-BLOCK-STATEMENT-POSN)
          )
Most of these should be self-explanatory.

The /PARENT-BLOCK-.*/ list describes the opening \( or \{.  PARENT-BLOCK-TYPE,
is, actually, one of those two characters, whichever starts the block
containing `point'.  It can also be `nil', which means that the line is at the
root-level.  Note that PARENT-BLOCK-INDENT is the indentation column of the
*line* where the opening \( or \{ sits.  This may or may not be the same as
the column where the opening \( or \{ is located.  There may be other text
preceding the \(/\{.

The /PARENT-BLOCK-STATEMENT-.*/ describes the statement attached to that
opening \(/\{.  PARENT-BLOCK-STATEMENT-NAME is the word preceding the \(/\{,
whether on the same line or a previous line.  {If there are blank lines
between the paren/brace, they'll be skipped.  Bear that in mind.}  It may also
have the specfial value, \"unknown-conditional\", which means that `point' is
inside of a conditional subexpression.  PARENT-BLOCK-STATEMENT-IS-QUOTED will
be non-nil if the character before PARENT-BLOCK-STATEMENT-NAME is a '\"'.

Lastly \(and firstly\) is CURRENT-STATEMENT-TYPE,.  This is a symbol
indicating the syntax of the line containing `point'.  It can have one
of the following values:
    root
    open-paren
    block-close-paren
    dangling-close-paren
    open-brace
    close-brace
    conditional
    comma-line
    statement
    function-arg
    statement-continuation

This function wraps its innards in a `save-excursion', for your convenience.

{jpw: 10/06}"
  (let ((parent-block-posn 0)
        (parent-block-line '(0 0))
        (parent-block-indent 0)
        (parent-block-paren-column 0)
        (parent-statement-posn (line-beginning-position))
        (parent-statement-line (list (line-beginning-position) 
                                     (line-end-position)))
        (parent-statement-indent 0)
        parent-block-type
        parent-statement-name
        parent-statement-is-quoted
        current-statement-type
        at-root
        is-typecast
        );; end var defs

    (save-excursion
      ;; If we're already on top of an opening syntactic paren, stay put.
      (unless (looking-at "\\s(")
        ;; This will move us up to the nearest enclosing syntactic paren.
        ;; Since it signals an error if we're outside of any list, we need to
        ;; wrap the call in a condition-case
        (condition-case nil 
            (backward-up-list)
          (error (setq at-root t)))
        )

      (unless at-root
        ;; Get info about the paren.
        (setq parent-block-posn (point)
              parent-block-line (list (line-beginning-position) 
                                      (line-end-position))
              parent-block-type (char-after)
              parent-block-paren-column (current-column)
              parent-block-indent (current-indentation))
        ;; Now examine the parent statement.
        (forward-word -1)
        (setq parent-statement-posn (point)
              parent-statement-line (list (line-beginning-position) 
                                          (line-end-position))
              parent-statement-indent (current-indentation))
        (if (re-search-forward "\\w+" parent-block-posn t)
            (setq parent-statement-name (match-string-no-properties 0))
          )
        (goto-char (1- parent-statement-posn))
        (setq parent-statement-is-quoted (looking-at "[\"']"))

        ;; Check if this is some form of conditional, and not an ee-statement:
        ;; - Are we in a block that opens with a paren, not a brace?
        ;; - Is the parent statement one of the ee language "function"
        ;;   keywords? 
        ;; - If not, is it one of the other ee functions?
        ;; - If not, do we have any boolean operators inside of the block?
        (if (and parent-block-type
                 (equal parent-block-type ?\()
                 parent-statement-name
                 (not (string-match ee-mode-language-fn-re
                                    parent-statement-name))
                 (not (string-match ee-mode-keyword-re
                                    parent-statement-name))
                 )
            (progn
              (goto-char (1+ parent-block-posn))
              (condition-case nil
                  (down-list)
                  ;; If down-list fails, just go to the next nonblank line.
                  ;; (This only works because we customize paragraph-start &
                  ;; paragraph-separate for this mode.)
                  (error (forward-paragraph))
                )
              (if (re-search-backward "\\(&&\\|||\\|==\\|!=\\)" 
                                      parent-block-posn t)
                  (setq parent-statement-name "unknown-conditional")
                )
              ) ;; end progn
          ) ;;end if
        );; end unless at-root
      );; end excursion

    ;; Now let's have a look at the current statement.
    (save-excursion
      (back-to-indentation)
      (setq current-statement-type
            (cond

             ;; Some trivial cases:

             ((equal "unknown-conditional" parent-statement-name)
              'conditional)
             ((looking-at ee-mode-comment-re)
              'comment)
             ((looking-at "(\\s *$")
              ;; Only a paren and whitespace on this line
              'open-paren
              )
             ((looking-at "{\\s *$")
              'open-brace
              )
             ((looking-at "}\\s *$")
              ;; Only a brace and whitespace on this line
              'close-brace
              )
             ((looking-at ",\\s *$")
              ;; Only a comma and whitespace on this line
              'comma-line
              )
             ;; If the line starts with any form of quote, we're looking at
             ;; the first statement of a new expression. 
             ((looking-at "[\"']")
              'new-statement)

             ;; More complex cases:

             ;; A close-paren, believe it or not, is a tad more complex...
             ((looking-at ")\\s *\"?\\s *")
              (goto-char (match-end 0))
              (if (or (looking-at "[;,\)}]\\s *")
                      (progn (end-of-line)
                             (forward-char)
                             (back-to-indentation)
                             (looking-at "[;,\)}]")))
                  'block-close-paren
                ;; else:
                'dangling-close-paren
                );; end if
              )
             ;; Are we in the conditional block of an If-statement?
             ((save-excursion
                (and (equal parent-statement-name "If")
                     (not (re-search-backward "," parent-block-posn t))))
              'conditional)

             ;; The default case.
             (t
              (save-excursion
                (re-search-backward "\\S " parent-block-posn t)
                ;; We need to handle ?{ separately, since it's used as part of
                ;; the typecast operator.
                (if (or (looking-at "[][()},;!]")
                        (and (eq (char-after) ?{)
                             (not (setq is-typecast 
                                        (looking-at ee-mode-typecast-re)))
                             )
                        )
                    ;; If the preceeding nonblank char was some form of paren,
                    ;; a statement separator, or the comment start char, it's
                    ;; a statement.
                    'statement
                  ;; else
                  'statement-continuation)
                )              
              );; end default case

             );;end cond
            );;end setq
      );; end excursion

    ;; Statement Type Post-Processing
    ;; 
    ;; Some of the prior analysis guesses incorrectly.  We handle those
    ;; special cases below
    (cond 
     ;; Function args which are on a separate line get identified as
     ;; statements (due to the preceding terminating "," char.
     ((and (eq current-statement-type 'statement)
           (eq parent-block-type ?\()
           (/= parent-block-indent parent-block-paren-column))
      (setq current-statement-type 'function-arg)
      );; end function-arg case

     ;; statment-continuation
     ;; - The items in an XRef table (except the very first pair) look like
     ;;   continued statements, but are really statements.
     ;; - Lines following a comment-only line will sometimes be misidentified
     ;;   as continuation statements.
     ((and (eq current-statement-type 'statement-continuation)
           (not is-typecast)
           (or (ee-mode-syntax-in-xref-table parent-block-type
                                             parent-block-posn
                                             parent-statement-name)
               (ee-mode-syntax-follows-comment-line))
           ) ;; end and
      (setq current-statement-type 'statement)
      ) ;; end statement-continuation expcetions
     );;end cond

    (list current-statement-type 
          (list parent-block-type
                parent-block-indent
                parent-block-paren-column
                parent-block-line
                parent-block-posn)
          (list parent-statement-name
                parent-statement-indent
                parent-statement-is-quoted
                parent-statement-line
                parent-statement-posn)
          )
    );; end let
  )


(defun ee-syntax-to-indentation (syntax)
  "Convert the syntactic information returned by `ee-mode-syntactic-analysis'
to an indentation amount.
{jpw: 10/06}"
  (let* ((type (car syntax))
         (tmp-paren-info (nth 1 syntax))
         (tmp-parent-info (nth 2 syntax))
         (which-paren (car tmp-paren-info))
         (paren-line-indentation (nth 1 tmp-paren-info))
         (block-indentation (nth 2 tmp-paren-info))
         (block-line (nth 3 tmp-paren-info))
         (block-point (nth 4 tmp-paren-info))
         (parent (car tmp-parent-info))
         (parent-indentation (nth 1 tmp-parent-info))
         (parent-follows-quote (nth 2 tmp-parent-info))
         (parent-line (nth 3 tmp-parent-info))
         (block-on-next-line-after-parent (= (1- (car block-line)) 
                                             (cadr parent-line)))
         (parent-and-block-on-same-line (and 
                                         (< (car parent-line) block-point)
                                         (< block-point (cadr parent-line))))
         );; end var defs

    (cond 
     ;; At the root level
     ((or (eq type 'root)
          (null parent))
      0
      )

     ;; Open paren/brace
     ((or (eq type 'open-paren)
          (eq type 'open-brace))
      (if parent-follows-quote
          (1+ parent-indentation)
        ;; else
        parent-indentation)
      )

     ;; a lone comma
     ((eq type 'comma-line)
      (+ block-indentation ee-mode-comma-extra-indent)
      )

     ;; close paren/brace
     ((or (eq type 'block-close-paren)
          (eq type 'close-brace))
      block-indentation
      )

     ;; A dangling close paren
     ((eq type 'dangling-close-paren)
      paren-line-indentation
      )

     ;; Inside of a set of function args
     ((eq type 'function-arg)
      ;; In this case, the "block-indentation" marks the location of the
      ;; function's opening-paren.
      (1+ block-indentation)
      )

     ;; Conditional
     ((eq type 'conditional)
      (back-to-indentation)
      (if (or (eq (char-after) ?\()
              block-on-next-line-after-parent)
          ;; If this is the start of a sub-conditional, or if we're outside of
          ;; all sub-conditionals, indent as normal.
          (+ block-indentation ee-mode-block-indent)
        ;; else:
        ;; Assume we're inside of a sub-conditional expression and indent
        ;; relative to the parent.
        (jpw-last-line-column-of "\\w")
        )
      )

     ;; A new statement requires a bit more work, depending on context.
     ((eq type 'new-statement)
      (if (and (eq which-paren ?\()
               (save-excursion
                 (back-to-indentation)
                 (not (looking-at "'[^'\n]*'")))
               )
          ;; Inside of a paren-block AND it's not a complete string.
          ;; Indent the character after the quote relative to the block.
          (1- (+ block-indentation ee-mode-block-indent))
        ;; else:
        ;; The Default: indent relative to the block
        (+ block-indentation ee-mode-block-indent)
        );; end if
      )

     ;; A statement indents relative to its parent block.
     ((eq type 'statement)
      (save-excursion
         (+ block-indentation ee-mode-block-indent)
        );; end excursion
      )

     ;; A statement-continuation should be the indentation of the previous
     ;; line plus the offset.  However, we don't really know if the previous
     ;; line is starts the statement, or is yet another statement-continuation
     ;; line.  So, we need to check it.
     ;; 
     ;; To complicate matters further, when a statement begins with a "\"",
     ;; its continuation lines should be indented 1 +  the normal offset.
     ((eq type 'statement-continuation)
      ;; FIXME:  We really should cache the syntactic analysis of the
      ;; previous line instead of doing this every time through.
      (+ (jpw-last-line-indentation) 
         (save-excursion
           (forward-line -1)
           (back-to-indentation)
           (if (eq 'statement-continuation 
                   (car (ee-mode-syntactic-analysis)))
               0
             ;; else
             ;; Do we start with a \" ?
             (if (looking-at "[\"']")
                 (1+ ee-mode-statement-continuation-indent)
               ;; else
               ;; The Default Behavior:
               ee-mode-statement-continuation-indent)
             );; check-prev-line-syntax if
           );; end excursion
         );; end +
      )

     ;; Default:  indent relative to the previous line.
     (t 
      nil)

     );;end cond
    );;end let
  )


(defun ee-syntactic-indent-line ()
  "Perform syntactic analysis on the line at `point', convert to an
indentaiton column, then indent.
{jpw: 10/06}"
  (back-to-indentation)
  (let* ((my-syntax (ee-mode-syntactic-analysis))
         (new-indent (or (and my-syntax
                              (ee-syntax-to-indentation my-syntax))
                         (jpw-last-line-indentation)
                         ))
         );; end var defs

    (if (and new-indent
             (or ee-mode-always-indent-line
                 ;; If we're not forcing line-indentation, then only indent
                 ;; the line if we're inside of the left margin.
                 (<= (current-column) (current-indentation))))
        (progn 
          (back-to-indentation)
          (indent-to new-indent))
      ;; else:
      (indent-relative)
      )
    );;end let
  )


;;------------------------------------------------------------
;;
;; Mode Interactive Functions
;; 


(defun ee-electric-char (arg)
  "Inserts the character it's bound to.  Then, if this is the first
non-whitespace character on the line, indents the entire line correctly.
{jpw: 10/06}"
  (interactive "p")
  (self-insert-command arg)
  (save-excursion
    (let* ((bolp (progn (beginning-of-line)
                        (point)))
                 (is-lone-char (looking-at "\\s *.\\s *$"))
                 (indentp (progn (back-to-indentation)
                                 (point)))
                 ) ;;end vars
      (if is-lone-char
          (progn (delete-region bolp indentp)
                 (ee-mode-indent-line))
        );; end if
      );; end let
    );; end excursion
  )


(defun ee-electric-bang (arg)
  "Inserts the \"!\" character.  If this is/will be the first
non-whitespace character on the line, indents to the current level first, then
inserts the \"!\".
{jpw: 10/06}"
  (interactive "p")
  (let* ((bolp (progn (beginning-of-line)
                      (point)))
         (empty-line (looking-at "\\s *$"))
         (indentp (progn (back-to-indentation)
                         (point)))
         ) ;;end vars
    (if empty-line
        (progn (delete-region bolp indentp)
               (ee-mode-indent-line))
      );; end if
    (self-insert-command arg)
    );; end let
  )


(defun ee-mode-show-syntactic-analysis ()
  "Display the results of a call to `ee-mode-syntactic-analysis' in the
minibuffer.
{jpw: 10/06}"
  (interactive)
  (message "%S" (ee-mode-syntactic-analysis))
  )

(defun ee-mode-indent-line ()
  "Indent a line according to ExpressionEvaluator syntax.
{jpw: 10/06}"
  (interactive)
  (if (ee-mode-at-or-inside-comment)
      (let ((last (jpw-last-line-indentation))
             (current (current-indentation))
             );; end var defs
        (if (<= last current)
            ;; Comment is already indented.  Only indent relative if the
            ;; current comment is already indented mroe deeply
            (if (/= last current)
                (indent-relative))
          ;; else:
          ;; Comment isn't correctly indented yet.
          (ee-comment-indent)
          )
        );; end let

    ;; else:  not in a comment
    (if (or ee-mode-always-indent-line
            (<= (current-column) (current-indentation)))
        ;; If we're inside (or on) the indentation margin, perform syntactic
        ;; indentation of the entire line.
        ;; If the user has disabled line-internal relative indenting, also
        ;; perform syntactic line indentation.
        (ee-syntactic-indent-line)
      ;; else:
      (indent-relative)
      )
    )
  )


;;------------------------------------------------------------
;;
;; Bindings: Define Local Keymap
;; 


(defvar ee-mode-map nil)
(if (null ee-mode-map)
    (progn
      (setq ee-mode-map (make-sparse-keymap))
      (define-key ee-mode-map "\C-i" 'indent-according-to-mode)
      (define-key ee-mode-map "\C-c\C-s" 'ee-mode-show-syntactic-analysis)
      (define-key ee-mode-map "{" 'ee-electric-char)
      (define-key ee-mode-map "}" 'ee-electric-char)
      (define-key ee-mode-map "(" 'ee-electric-char)
      (define-key ee-mode-map ")" 'ee-electric-char)
      (define-key ee-mode-map "," 'ee-electric-char)
      (define-key ee-mode-map "!" 'ee-electric-bang)
      ;; More bindings go here:
      );end progn
  );end if


;;------------------------------------------------------------
;;
;; Font-Lock Support
;; 


;;
;; Support var, consts, & defsubsts (i.e. ones used by the Font-Lock MATCHER
;; defuns and keyword lists).
;;


(defconst ee-mode-font-lock-comment-re
  "\\(!$\\|!\\(==+.*\\|[^=].*\\)$\\)")


(defconst ee-mode-font-lock-keywords
  (eval-when-compile
    (list (concat "\\<" ee-mode-keyword-re "\\>")
          '(1 'font-lock-keyword-face nil)
          ))
  )


;;
;; Font-lock MATCHER functions.
;;
;; Must be a function instead of a regexp.  Will be called with
;; a single arg:  the limit of the search, and on success:
;;   1. Must return non-nil
;;   2. Must set `match-data' accordingly.
;;   3. It must move `point' past `match-beginning', or font-lock will loop
;;      forever.
;;


(defun ee-mode-find-comment (upper-limit)
  ;; Leave docstring blank, except for date.  Use doc-comments, instead.
  "{jpw: 10/05}"
  ;;
  ;; Examines region between `point' and `upper-limit', looking for all
  ;; comments.  Sets the `match-data' to a list of points as
  ;; follows:
  ;; 
  ;;    ( all-matched-begin-p  all-matched-end-p
  ;;      comment-begin-p      comment-end-p )
  ;;
  ;; Unfortunately, EE uses the '!' char for comment start, which makes it
  ;; difficult to tell it apart from the boolean negation operator.  This
  ;; function performs the extra analysis needed to discern the two.
  (let* ((search-start-p (point))
         ;; Perform the search at var-defn time.
         (found-match
          (re-search-forward ee-mode-font-lock-comment-re upper-limit t))
         (comment-start-p (match-beginning 0))
         (comment-end-p (match-end 0))
         first-word-end-p
         );; end var defs

    ;; First, check for simple overzealous cases, where the negated flag
    ;; follows a boolean operator or starts an expression.
    (and
     found-match
     (goto-char (1+ comment-start-p))
     (re-search-backward "\\(?:&&\\|||\\)\\s *!" 
                         (min (line-beginning-position) search-start-p)
                         t)
     ;; If we match, skip it.
     (setq found-match nil)
     ) ;; end and
    (and
     ;; Find where the first "word" after the '!' char ends.
     ;; This search is needed by subsequent checks.
     found-match
     (goto-char comment-start-p)
     (re-search-forward "!\\s *\\w\\S *" 
                        (min (line-end-position) upper-limit) t)
     (setq first-word-end-p (match-end 0))
     ;; Check if the negated flag preceeds any boolean operators or a
     ;; close-paren.
     (goto-char first-word-end-p) ;; For emacs elisp-mode (
     (or
      ;; Boolean operator follows 1st word, with optional whitespace between.
      (looking-at "\\s *\\(?:&&\\|||\\)")
      ;; Whitespace, then comma or paren, follows 1st word
      (looking-at "\\s +[,)]")
      )
     ;; If we match, skip it.
     (setq found-match nil)
     ) ;; end and

    ;; Prepare the results.
    (if found-match
        (progn (set-match-data (list comment-start-p comment-end-p 
                                     comment-start-p comment-end-p))
               (goto-char comment-end-p)
               )
      );; end if
    );; end let
  )


;;
;; Individual `font-lock-keyword' lists.  Each value is of the same form as an
;; element of `font-lock-keywords'; see that variable's documentation for more
;; info.
;;


(defconst ee-mode-font-lock-keywords-1
  (list
   (list
    "\\(XRef\\w+\\$\\)" 
    '(1 'ee-mode-map-function-face prepend))
   (list
    ee-mode-language-fn-re
    '(1 'ee-mode-language-function-face prepend))
   (list
    (concat "\\(" ee-mode-typecast-re "\\)")
    '(1 'font-lock-type-face prepend))
   (list
    "\\(\\.\\([bfi][0-9]+\\|[cs]\\)\\)" 
    '(1 'font-lock-type-face prepend))
   (list
    "#include"
    '(0 'font-lock-constant-face prepend))
   (list
    ee-mode-globals-re
    '(1 'ee-mode-globals-face prepend))
   (list
    ee-mode-var-re
    '(1 'font-lock-variable-name-face prepend)
    '(2 'font-lock-type-face prepend))
   ;; This should be optional.
   (list
    "\\([;,]\\)\\s *$"
    '(1 'ee-mode-syntactic-punctuation-face prepend))
   (list
    "^\\s *\\(,\\)\\s *$"
    '(1 'ee-mode-syntactic-punctuation-face prepend))
   ;; Does definition ordering matter for FontLock?  If so, these must come
   ;; last.
   (list 'ee-mode-find-comment '(1 'font-lock-comment-face t))
   (list
    "[^!']*\\('[^\\\\']*\\(\\\\.[^\\\\']*\\)*'\\)"
    '(1 'font-lock-string-face append))
   );; end outer list.
  "Mode-specific value of `font-lock-keywords'.
{jpw: 10/05}")


(defconst ee-mode-font-lock-defaults
  (list
   ;; TODO:  Change so that there are different levels of fontificaiton.
   '(ee-mode-keyword-list ee-mode-font-lock-keywords-1)
   nil nil
   ;; Syntax table must not be used here.  Otherwise, we end up fontifying the
   ;; != op.
   '()
   ;; ExpressionEvaluator is a block-based language.  So, using
   ;; `backward-up-list' will refontify less while still moving backward an
   ;; entire expression.
   'backward-paragraph
   );; end list
  )


;;------------------------------------------------------------
;;
;; Define the mode proper
;; 



;; Set up the auto-mode-alist with some defaults.  Do this just once, at load
;; time.
(dolist (re (list "\\.cfg\\'" "\\.syscfg\\'" "\\.templt\\'"))
  (add-to-list 'auto-mode-alist (cons re 'ee-mode)))



;;;###autoload (ee-mode)
(define-derived-mode ee-mode text-mode "ee"
  "Major Mode for Expression Evaluator files.

Activating \"ee-mode\" runs the usual hook: `ee-mode-hook'.

Key bindings:
\\{ee-mode-map}

{jpw: 10/05}"

  ;; Define indentation function
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ee-mode-indent-line)

  ;; Define comment syntax.
  ;; All of these should be defined, or the built-in comment functions may not
  ;; work.
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-end-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-use-syntax)
  (setq comment-start "! "
        comment-start-skip " *!+ *"
        comment-end ""
        comment-end-skip "\n"
        comment-use-syntax nil
        comment-indent-function 'ee-comment-indent)
  ;; `comment-indent-function' should be set to something that returns the
  ;; indentation of the previous line.  Well, probably...

  ;; Configure paragraph start/separate vars
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start ee-mode-paragraph-start-re
        paragraph-separate ee-mode-paragraph-start-re)

  ;; Configure auto-fill.
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode t
        adaptive-fill-regexp ee-mode-fill-prefix-re)

  ;; Set up font-lock for ee-mode
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-multiline)
  (make-local-variable 'font-lock-support-mode)
  (make-local-variable 'jit-lock-defer-contextually)
  (make-local-variable 'jit-lock-stealth-nice)
  (make-local-variable 'jit-lock-stealth-time)
  (make-local-variable 'lazy-lock-defer-contextually)
  (make-local-variable 'lazy-lock-minimum-size)
  (setq font-lock-defaults ee-mode-font-lock-defaults
        font-lock-multiline t
        ;; `jit-lock-mode' doesn't correctly fontify everything we want it to.
        ;; 
        ;; `fast-lock-mode' and `lazy-lock-mode' require you to
        ;; manually-refontify when editing text in some of the more complex
        ;; multiline expressions.  `fast-lock-mode' works by keeping a cache
        ;; of fontifications.
        jit-lock-stealth-time  3
        jit-lock-stealth-nice  0.1
        ;; Remove contextual deferment from all of the modes.  Permits
        ;; in-place fontification & preserves font of subsequent text, which
        ;; is useful for multiline syntactic contexts.
        jit-lock-defer-contextually  nil
        lazy-lock-defer-contextually  nil)

  (run-hooks 'ee-mode-hook)
  )


(provide 'ee-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;