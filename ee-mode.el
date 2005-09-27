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
;; RCS $Id: elisp.el 1401 2005-08-08 23:09:12Z candide $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'generic)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Language Elements
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar ee-mode-logic-re
  (concat
   "\\("
   "AddFn\\|"
   "AndExpr\\|"
   "ExactExpr\\|"
   "For\\|"
   "If\\|"
   "OrExpr\\|"
   "StartsWithExpr\\|"
   "WildcardExpr"
   "\\)"
   )
  )


(defvar ee-mode-keyword-list
  (list
   "Abs"
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
   "log10"
   )
  )



(defvar ee-mode-generic-blockdefn-re_frag
  "name\\|default")
(defvar ee-mode-expression-defn-re_frag
  "dll\\|expression\\|preExpression\\|xrefTables")
(defvar ee-mode-error-defn-re_frag
  "messages\\|error\\|warning\\|language")
(defvar ee-mode-linkmgr-defn-re_frag
  (concat
   "linkMgr\\|stateLink\\|onDatabase\\(Available\\|Recoverable\\)\\|"
   "compressLevel\\|applyDefaultToLocal\\|service\\|capabilities\\|"
   "address"
   ))
(defvar ee-mode-db-defn-re_frag
  (concat
   "table\\|alias\\|where\\|"
   "\\(join\\(If\\|Key\\|Type\\)?\\)\\|"
   "\\(backwardJoin\\(Key\\|Where\\)?\\)"
   ))

(defvar ee-mode-blockelement-re
  (concat
   "\\("
   ee-mode-generic-blockdefn-re_frag
   "\\|"
   ee-mode-expression-defn-re_frag
   "\\|"
   ee-mode-error-defn-re_frag
   "\\|"
   ee-mode-linkmgr-defn-re_frag
   "\\|"
   ee-mode-db-defn-re_frag
   "\\)"
   )
  )


(defvar ee-mode-comment-re
  "\\(^\\s *!.*$\\)"
  )


(defvar ee-mode-var-re
  "\\(\\w+\\)\\(##?[sid]\\)" 
  )


(defvar ee-mode-globals-re
  "\\(rds\\|txn\\|update\\(Idx\\|Op\\)\\)"
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode Definition
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-generic-mode 'ee-generic-mode
  nil  ;; Can't handle comment here.  Ends up matching the != op.
  ee-mode-keyword-list
  (list
   (list
    "\\(XRef\\w+\\$\\)" 
    '(1 font-lock-function-name-face prepend))
   (list
    ee-mode-logic-re
    '(1 font-lock-builtin-face prepend))
   (list
    "\\({\\(double\\|int\\|string\\)}\\)" 
    '(1 font-lock-type-face prepend))
   (list
    "\\(\\.\\([bfi][0-9]+\\|[cs]\\)\\)" 
    '(1 font-lock-type-face prepend))
   (list
    "#include"
    '(0 font-lock-constant-face prepend))
   (list
    ee-mode-globals-re
    '(1 font-lock-constant-face prepend))
   (list
    ee-mode-var-re
    '(1 font-lock-variable-name-face prepend)
    '(2 font-lock-type-face prepend))
   ;;
   ;; Optional font-locking.  Right now, controlled by commenting out. :P
   ;;
   ;;;(list "^\\s *\\([,(){}]\\)" '(1 font-lock-constant-face prepend))
   ;;;(list "\\(\"\\)\\s *$" '(1 font-lock-warning-face t))
   (list "\\([;,]\\)\\s *$" '(1 font-lock-builtin-face prepend))
   (list "^\\s *\\(,\\)\\s *$" '(1 font-lock-builtin-face prepend))
   ;;
   ;; Does definition ordering matter for FontLock?  If so, these must come
   ;; last.
   ;;
   (list  ee-mode-comment-re '(1 font-lock-comment-face t))
   (list
    "[^!']*\\('[^\\\\']*\\(\\\\.[^\\\\']*\\)*'\\)"
    '(1 font-lock-string-face append))
   )
  (list "\\.cfg\\'" "\\.templt\\'")
  (list 'generic-ee-mode-setup-function)
  "Generic Mode for ExpressionEvaluator files.")


(defun generic-ee-mode-setup-function ()
;;  (make-local-variable 'ee-mode-syntax-table)
;;  (setq ee-mode-syntax-table (make-syntax-table))
;;  (modify-syntax-entry ?_  "w" ee-mode-syntax-table)
;;  (set-syntax-table ee-mode-syntax-table)
  (modify-syntax-entry ?_  "w")
  (modify-syntax-entry ?'  "$")
  (modify-syntax-entry ?\" "$")
  (make-local-variable 'font-lock-multiline)
  (setq font-lock-multiline t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;