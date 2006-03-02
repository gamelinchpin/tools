;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ExpressionEvaluator Mode
;;
;;  Copyright © 2005 John P. Weiss
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
(require 'newcomment)
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


(defcustom ee-mode-language-fn-indent 2
  "Indentation level for statements inside of one of the \"language-element\"
functions.
This also controls the indentation level of expressions following a
\"syntactic comma\", i.e. one inside of a language-element function, sitting
on its own line.
{jpw: 10/05}"
  :type '(integer variable)
  :group 'ee-mode)


(defcustom ee-mode-comma-extra-indent 0
  "Indentation level for statements inside of one of the \"language-element\"
functions.
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
    "joinIf"
    "joinKey"
    "joinType"
    "table"
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


(defconst ee-mode-var-re
  "\\<\\(\\w+\\)\\(##?[sid]\\)\\>")


;;
;; Syntax/indentation-only expressions.
;; 
;; All of these regexps are designed for use in `looking-at', after a call to
;; `back-to-indentation'.
;;


(defconst ee-mode-expr-opening-quote-re "\"\\([^,\;\"]?[^\"]+\\)*$")

(defconst ee-mode-expr-closing-quote-re "\"[,\;]\\s *$")


;;------------------------------------------------------------
;;
;; Other Variables
;; 


(defvar ee-mode-syntax-table
  (let ((nutable (copy-syntax-table)))
    (modify-syntax-entry ?_  "w" nutable)
    (modify-syntax-entry ?_  "w" nutable)
    (modify-syntax-entry ?'  "$" nutable)
    (modify-syntax-entry ?\" "$" nutable)
    (modify-syntax-entry ?\\ "\\" nutable)
    nutable);;end let
  "Syntax table for use in ee-mode buffers.
Makes '_' into a word character and turns \' and \" into paired delimiters,
instead of string quote delimiters.")


;;------------------------------------------------------------
;;
;; Utility Functions
;; 


;;
;; Inlines
;;


(defsubst ee-inside-comment ()
  ;; Wraps `comment-beginning' in an excursion.
  (save-excursion (comment-beginning)))


;;(defsubst 
;;  )


;;------------------------------------------------------------
;;
;; Mode Interactive Functions
;; 


(defun ee-mode-indent ()
  (interactive)
  nil
  ;; if ina-comment and comment begin properly indented
  ;;   (indent-relative)
  ;;  else indent comment correctly
  ;; Else - not in a comment
  )


;;------------------------------------------------------------
;;
;; Bindings: Define Local Keymap
;; 


(defvar ee-mode-map nil)
(if (null ee-mode-map)
    (progn
      (setq ee-mode-map (make-sparse-keymap))
      (define-key ee-mode-map [Tab] 'indent-according-to-mode)
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
    "\\({\\(double\\|int\\|string\\)}\\)" 
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
;;   (list ee-mode-font-lock-comment-re '(1 'font-lock-comment-face t))
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
  (kill-all-local-variables)
  (set-syntax-table ee-mode-syntax-table)

  ;; Define comment syntax.
  ;; All of these should be defined, or the built-in comment functions may not
  ;; work.
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-end-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-use-syntax)
  (setq comment-start "! "
        comment-start-skip "!+ *"
        comment-end ""
        comment-end-skip "\n"
        comment-use-syntax nil)
  ;; `comment-indent-function' should be set to something that returns the
  ;; indentation of the previous line.  Well, probably...

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

;;  (make-local-variable 'indent-line-function)
;;  (setq indent-line-function 'ee-mode-indent)

  ;; Set the mode line
  ;; FIXME:  We shouldn't need to do this.  Not required for phpBB-mode.  So,
  ;; why here?
  (make-local-variable 'major-mode)
  (make-local-variable 'mode-name)
  (setq major-mode 'ee-mode
        mode-name "ee")
  (run-hooks 'ee-mode-hook)
  )


(provide 'ee-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;