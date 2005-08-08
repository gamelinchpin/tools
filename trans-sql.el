;;; trans-sql.el --- specialized sql.el for Transact/SQL.  Mainly
;;;                  defines new keywords.

;; Copyright (C) 2001  John Weiss under the Artistic License

;; Author: John Weiss <jpweiss@idsi.net>
;; Maintainer: John Weiss <jpweiss@idsi.net>
;; Version: 1.0
;; Keywords: comm languages processes



;;; Code:

(require 'sql)
(require 'sql-indent)
;; Need the following to allow GNU Emacs 19 to compile the file.
(require 'regexp-opt)


;; Keywords:
;;
;; ANSI SQL-99
;;

(defvar ansi-sql99-statements
  '("alter" "call" "case" "cast" "close" "commit" "connect"
    "connection" "create" "cursor" "database" "declare" "delete"
    "disconnect" "drop" "fetch" "function" "grant" "index" "insert"
    "join" "like" "open" "operators" "procedure" "return" "revoke"
    "role" "rollback" "savepoint" "schema" "select" "set" "start"
    "table" "transaction" "trigger" "truncate" "update" "view")
  "ANSI SQL99 keywords: statements")

(defvar ansi-sql99-functions
  '("avg" "bit_length" "char_length" "concat" "concatenate" "convert"
    "count" "current_date" "current_time" "current_timestamp"
    "current_user" "extract" "len" "length" "lower" "max" "min"
    "octet_length" "position" "session_user" "substr" "substring"
    "sum" "system_user" "translate" "trim" "upper")
  "ANSI SQL99 keywords: functions")

(defvar ansi-sql99-types
  '("binary" "bit" "blob" "boolean" "char" "character" "clob" "date"
    "datetime" "dec" "decimal" "double" "float" "int" "integer"
    "nchar" "nclob" "numeric" "precision" "real" "smallint" "time"
    "timestamp" "varying" "zone")
  "ANSI SQL99 keywords: types")

(defvar ansi-sql99-const
  '("null")
  "ANSI SQL99 keywords: constants")


(defvar ansi-sql99-misc-keywords
  '("absolute" "action" "add" "admin" "after" "aggregate" "alias"
    "all" "allocate" "and" "any" "are" "array" "as" "asc" "assertion"
    "at" "authorization" "before" "begin" "both" "breadth" "break"
    "by" "cascade" "catalog" "check" "class" "collate" "collation"
    "column" "completion" "condition" "constraint" "constraints"
    "constructor" "contains" "continue" "corresponding" "cross" "cube"
    "current" "current_path" "current_role" "cycle" "data" "datalink"
    "day" "deallocate" "default" "deferrable" "depth" "deref" "desc"
    "descriptor" "diagnostics" "dictionary" "do" "domain" "end"
    "equals" "escape" "except" "exception" "exec" "execute" "exit"
    "expand" "expanding" "false" "first" "for" "foreign" "free" "from"
    "general" "get" "global" "goto" "group" "grouping" "handler"
    "hash" "having" "hour" "identity" "if" "ignore" "immediate" "in"
    "indicator" "initialize" "initially" "inner" "inout" "input"
    "intersect" "interval" "into" "is" "isolation" "iterate" "key"
    "language" "large" "last" "lateral" "leading" "leave" "left"
    "less" "level" "limit" "local" "localtime" "localtimestamp"
    "locator" "loop" "match" "meets" "minute" "modifies" "modify"
    "module" "month" "names" "national" "natural" "new" "next" "no"
    "none" "normalize" "not" "object" "of" "off" "old" "on"
    "only" "operation" "option" "or" "order" "ordinality" "out"
    "outer" "output" "pad" "parameter" "parameters" "partial" "path"
    "period" "postfix" "precedes" "prefix" "preorder" "prepare"
    "preserve" "primary" "prior" "privileges" "public" "read" "reads"
    "recursive" "redo" "ref" "references" "referencing" "relative"
    "repeat" "resignal" "restrict" "result" "returns" "right" "rollup"
    "routine" "row" "rows" "scroll" "search" "second" "section"
    "sequence" "session" "sets" "signal" "size" "specific"
    "specifictype" "sql" "sqlexception" "sqlstate" "sqlwarning"
    "state" "static" "structure" "succeeds" "temporary" "terminate"
    "than" "then" "timezone_hour" "timezone_minute" "to" "trailing"
    "translation" "treat" "true" "under" "undo" "union" "unique"
    "unknown" "until" "usage" "user" "using" "value" "values" "values"
    "variable" "when" "whenever" "where" "while" "with" "write" "year") 
  "ANSI SQL99 keywords: miscellaneous (including reserved words")

;;
;; Transact/SQL
;;

(defvar trans-sql-statements nil
  "Additional keywords for Transact/SQL statements")
(if trans-sql-statements
    ()
  (eval-when-compile
    (setq trans-sql-statements
          (sort
           (append ansi-sql99-statements '("dump" "print" "raiserror"))
           'string<))
    )
  )

(defvar trans-sql-functions nil
  "Additional keywords for Transact/SQL functions")
(if trans-sql-functions
    ()
  (eval-when-compile
    (setq trans-sql-functions
          (sort (append ansi-sql99-functions '("compute"))
                'string<))
    )
  )

(defvar trans-sql-types nil
  "Additional keywords for Transact/SQL types")
(if trans-sql-types 
    ()
  (eval-when-compile
    (setq trans-sql-types
          (sort
           (append ansi-sql99-types '("nvarchar" "tinyint" "varchar"))
           'string<))
    )
  )

(defvar trans-sql-misc-keywords nil
  "Additional general keywords for Transact/SQL")
(if trans-sql-misc-keywords
    ()
  (eval-when-compile
    (setq trans-sql-misc-keywords
          (sort
           (append ansi-sql99-misc-keywords
                   '("between" "browse" "bulk" "checkpoint"
                     "clustered" "confirm" "dbcc" "dummy" "errlvl"
                     "exists" "proc" "tran" "use"))
           'string<))
    )
  )

;;
;; The regexps for the modes.
;;

(defconst _sql-mode-local-var-regexp "\\([#@][a-zA-Z0-9_]+\\)\\>")
(defconst _sql-mode-global-var-regexp nil)
(if _sql-mode-global-var-regexp
    ()
  (eval-when-compile
    (setq _sql-mode-global-var-regexp
          (concat "\\(\\(##\\|@@\\)[a-zA-Z0-9_]+\\|" 
                  (car ansi-sql99-const) "\\)\\>"))
    )
  )

(defvar sql-mode-ansi-sql99-font-lock-keywords nil)
(if sql-mode-ansi-sql99-font-lock-keywords
    ()
  (setq sql-mode-ansi-sql99-font-lock-keywords
        (eval-when-compile
          (list
           (cons
            (concat "\\b"
                    (regexp-opt ansi-sql99-statements t) "\\b")
            'font-lock-function-name-face)
           (cons
            (concat "\\b"
                    (regexp-opt ansi-sql99-types t) "\\b")
            'font-lock-type-face)
           (cons _sql-mode-global-var-regexp 'font-lock-constant-face)
           (cons _sql-mode-local-var-regexp 'font-lock-variable-name-face)
           (cons
            (concat "\\b"
                    (regexp-opt ansi-sql99-functions t) "\\b")
            'font-lock-builtin-face)
           (cons
            (concat "\\b"
                    (regexp-opt ansi-sql99-misc-keywords t) "\\b")
            'font-lock-keyword-face)
           );; end list
          )
        )
  );; end if

(defvar sql-mode-trans-sql-font-lock-keywords nil)
(if sql-mode-ansi-sql99-font-lock-keywords
    ()
  (setq sql-mode-ansi-sql99-font-lock-keywords
        (eval-when-compile
          (list
           (cons
            (concat "\\b"
                    (regexp-opt trans-sql-statements t) "\\b")
            'font-lock-function-name-face)
           (cons
            (concat "\\b"
                    (regexp-opt trans-sql-types t) "\\b")
            'font-lock-type-face)
           (cons _sql-mode-global-var-regexp 'font-lock-constant-face)
           (cons _sql-mode-local-var-regexp 'font-lock-variable-name-face)
           (cons 
            (concat "\\b"
                    (regexp-opt trans-sql-functions t) "\\b")
            'font-lock-builtin-face)
           (cons
            (concat "\\b"
                    (regexp-opt trans-sql-misc-keywords t) "\\b")
            'font-lock-keyword-face)
           );; end list
          )
        )
  );; end if

;;
;; Functions that set the font to something.
;;

(defun sql-highlight-ansi-sql99-keywords ()
  "Highlight ANSI SQL99 keywords.
Basically, this just sets `font-lock-keywords' appropriately."
  (interactive)
  (setq font-lock-keywords sql-mode-ansi-sql99-font-lock-keywords)
  (font-lock-fontify-buffer))

(defun sql-highlight-trans-sql-keywords ()
  "Highlight Transact/SQL keywords.
Basically, this just sets `font-lock-keywords' appropriately."
  (interactive)
  (setq font-lock-keywords sql-mode-trans-sql-font-lock-keywords)
  (font-lock-fontify-buffer))


(provide 'trans-sql)

;;; sql.el ends here


