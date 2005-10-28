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
;; At present, it's just a variant of generic-mode.
;;
;;  
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'generic)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup ee-mode nil
  "Customizable aspects of ee-generic-mode.
{jpw: 10/05}"
  :group 'languages
  :group 'local)

;;:type:
  ;; One of: integer, number, string, regexp, character, hook,
  ;;         symbol, function, variable, boolean


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Language Elements
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (concat "\\("
            (regexp-opt ee-mode-language-fn-list)
            "\\)")))


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
    (concat "\\("
            (regexp-opt ee-mode-keyword-list)
            "\\)")))


(defconst ee-mode-globals-list
  '("rds" "txn" "updateIdx" "updateOp")
  "Expression Evaluator global vars & expressions.
{jpw: 10/05}")
(defconst ee-mode-globals-re
  (eval-when-compile
    (concat "\\("
            (regexp-opt ee-mode-globals-list)
            "\\)")))


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
    (concat "\\("
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
            "\\)")
    );; end eval-when-compile
  );;end defconst


(defconst ee-mode-comment-re
  "\\(!\\(==+.*\\|[^=].*\\|\\)$\\)")


(defconst ee-mode-var-re
  "\\(\\w+\\)\\(##?[sid]\\)")


;;
;; Syntax/indentation-only expressions.
;; 
;; All of these regexps are designed for use in `looking-at', after a call to
;; `back-to-indentation'.
;;


(defconst ee-mode-expr-opening-quote-re "\"\\([^,\;\"]?[^\"]+\\)*$")

(defconst ee-mode-expr-closing-quote-re "\"[,\;]\\s *$")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode Definition
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-generic-mode 'ee-generic-mode
  ;; Adding "! ... \n" to the syntax table as comment-start/comment-end screws
  ;; up font-lock w/ the != operator.  So, we need a workaround; see below.
  (list "!" "\n")
  ee-mode-keyword-list
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
   ;;
   ;; Optional font-locking.  Right now, controlled by commenting out. :P
   ;;
   (list
    "\\([;,]\\)\\s *$"
    '(1 'ee-mode-syntactic-punctuation-face prepend))
   (list
    "^\\s *\\(,\\)\\s *$"
    '(1 'ee-mode-syntactic-punctuation-face prepend))
   ;;
   ;; Does definition ordering matter for FontLock?  If so, these must come
   ;; last.
   ;;
   (list ee-mode-comment-re '(1 'font-lock-comment-face t))
   (list
    "[^!']*\\('[^\\\\']*\\(\\\\.[^\\\\']*\\)*'\\)"
    '(1 'font-lock-string-face append))
   )
  (list "\\.cfg\\'" "\\.templt\\'")
  (list 'generic-ee-mode-setup-function)
  "Generic Mode for ExpressionEvaluator files.")


(defun generic-ee-mode-setup-function ()
  (make-local-variable 'ee-mode-syntax-table)
  (setq ee-mode-syntax-table (copy-syntax-table))
  (modify-syntax-entry ?_  "w" ee-mode-syntax-table)
  (modify-syntax-entry ?_  "w" ee-mode-syntax-table)
  (modify-syntax-entry ?'  "$" ee-mode-syntax-table)
  (modify-syntax-entry ?\" "$" ee-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" ee-mode-syntax-table)

  (make-local-variable 'font-lock-multiline)
  (make-local-variable 'font-lock-support-mode)
  (make-local-variable 'lazy-lock-minimum-size)
  (make-local-variable 'jit-lock-stealth-time)
  (make-local-variable 'jit-lock-stealth-nice)
  (make-local-variable 'jit-lock-defer-contextually)
  (make-local-variable 'lazy-lock-defer-contextually)
  (setq font-lock-multiline t
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

  ;; The `define-generic-mode' defun sets up the syntax table and sets
  ;; `comment-start' and `comment-end'.  We only need to add the following:
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "!+ *")
  (setq comment-start-skip "![ \t]*")

  ;; Adding "! ... \n" to the syntax table as comment-start/comment-end screws
  ;; up font-lock w/ the != operator.  As a workaround, disable syntactic
  ;; keywords.
  (make-local-variable 'font-lock-syntactic-keywords)
  (setq font-lock-syntactic-keywords 'nil)

  (set-syntax-table ee-mode-syntax-table)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Special Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Inlines
;;


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Some library functions that may be of use for indentation.

;;  Works, but is a tad top-heavy.
;;  (how-many regex start-pt end-pt)
;; thing-at-point
;; indent-to
;; current-indentation
;; back-to-indentation ;; intra-line
;; (backward-to-indentation arg) ;; find indentation ARG lines back
;; forward-to-indentation 
;; indent-relative ;; Good fallback fn.
;; Moving by balanced parens:
;; forward-sexp
;; backward-sexp
;; backward-list
;; forward-list
;; backward-up-list
;; down-list

;; Causes the sexp/list fns to skip comments.
;; (setq parse-sexp-ignore-comments t)

;;(defsubst 
;;  )


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;