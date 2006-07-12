;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some enhancements for the default TCL-mode that comes with Emacs
;;
;;  Copyright © 2005-2006 John P. Weiss
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
;; Based on a post on the EmacsWiki.
;;
;;  
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'tcl)
(require 'jpw-indent-tools)


;;------------------------------------------------------------
;;
;; User Customizations
;; 


(defface tcl-builtin-face
  '((t (:inherit font-lock-builtin-face :italic t
                 :foreground "#6000FF")))
  "The face to use for builtin TCL functions, such as 'array', 'set', and so
on.
{jpw: 6/05}"
  :group 'tcl)


(defface tcl-builtin-arg1-face
  '((t (:inherit font-lock-type-face
                 :foreground "#00A050")))
  "The face to use for the 'keyword' first arg required by certain TCL
builtins (e.g. 'array', 'string').
{jpw: 6/05}"
  :group 'tcl)


(defface tcl-keyword-arg1-face
  '((t (:inherit font-lock-constant-face)))
  "The face to use for the 'keyword' first arg required by the special TCL
keywords, such as 'namespace'.  Inherits from `tcl-builtin-arg1-face' by
default, but is provided for finer-grained control.
{jpw: 6/05}"
  :group 'tcl)


(defcustom jpw-tcl-allow-comment-unindent t
  "When set to non-nil, allows the function `jpw-tcl-comment-indent-command'
to \"indent backward\".  By default, `jpw-tcl-comment-indent-command' ignores
comments with internal indentation deeper than the previous line.
{jpw: 7/06}"
  :group 'tcl)


;;------------------------------------------------------------
;;
;; Enhancement Vars
;; 


(defconst tcl-core-builtins
  '("expr" "exec" "source" "rename")
  "Some rather crucial builtins.  One could argue that they should be
keywords.  One could, however, never use one in a Tcl script, whereas it's
hard to write a Tcl script without a single 'set' in it.
{jpw: 6/05}")


(defconst tcl-list-builtins
  '("list" "lappend" "lindex" "linsert" "llength"
    "lrange" "lreplace" "lsearch" "lset" "lsort")
  "Tcl builtins for list manipulation.
{jpw: 6/05}")


(defconst tcl-text-builtins
  '("append" "concat" "join" "format" "scan" "split" "subst" "regexp"
    "regsub")
  "Tcl builtins for text manipulation.
{jpw: 6/05}")


(defconst tcl-file-builtins
  '("glob" "cd" "pwd")
  "A smattering of file- and os-related Tcl builtins.  See also
`tcl-io-builtins'.
{jpw: 6/05}")


(defconst tcl-io-builtins
  '("fcopy" "close" "eof" "fblocked" "fconfigure" "fileevent" "flush"
    "gets" "seek" "tell" "open" "puts" "read" "pid")
  "Tcl builtins for I/O.
{jpw: 6/05}")


(defconst tclLib-all-builtins
  '("auto_execok" "auto_import" "auto_load" "auto_mkindex"
    "auto_mkindex_old" "auto_qualify" "auto_reset"
    "pkg_mkIndex"
    "parray"
    "tcl_endOfWord" "tcl_findLibrary" "tcl_startOfNextWord"
    "tcl_startOfPreviousWord" "tcl_wordBreakAfter" "tcl_wordBreakBefore"
    )
  "The proc's from the Tcl library of coding tools, which is included as part
of the language but aren't really keywords or builtins.
{jpw: 6/05}")


(defconst tclLib-text-builtins
  '("parray"
    "tcl_endOfWord" "tcl_startOfNextWord"
    "tcl_startOfPreviousWord" "tcl_wordBreakAfter" "tcl_wordBreakBefore"
    )
  "A subset of `tclLib-all-builtins' used for text manipulation.  Also
includes 'parray', which is more of a coding swiss-army-knife than anything
else.
{jpw: 6/05}")


(defconst tcl-misc-builtins
  '("bgerror" "dde" "msgcat"
    "resource" 
    "socket"
    "tcltest"
    "load" "time"
    "unknown" "update" "vwait")
  "Miscellaneous Tcl proc's and builtins.
{jpw: 6/05}")


;; I want these two consts to be hardcoded with whatever value is in the
;; latest version of the `tcl' package.
(if (boundp 'orig-tcl-proc-list)
    ()
  (defconst orig-tcl-proc-list tcl-proc-list)
  (defconst orig-tcl-typeword-list tcl-typeword-list)
  (defconst orig-tcl-keyword-list tcl-keyword-list)
  ) ;; end if

;; Regexps used in font-lock

(defconst tcl-proc-list-re
  (eval-when-compile
    (regexp-opt orig-tcl-proc-list)
    )
  "The original value of `tcl-proc-list', converted to a regexp.
{jpw: 6/05}")

(defconst tcl-typeword-list-re
  (eval-when-compile
    (regexp-opt orig-tcl-typeword-list)
    )
  "The original value of `tcl-typeword-list', converted to a regexp.
{jpw: 6/05}")

(defconst tcl-proc-list-enhanced
  (eval-when-compile
    (sort (copy-list orig-tcl-proc-list) 'string-lessp)
    )
  "The original value of `tcl-proc-list', sorted. 
{jpw: 6/05}")

(defconst tcl-typeword-list-enhanced
  (eval-when-compile
    (sort (copy-list orig-tcl-typeword-list) 'string-lessp)
    )
  "The original value of `tcl-typeword-list', sorted.
{jpw: 6/05}")

(defconst tcl-keyword-list-enhanced 
  (eval-when-compile
    (sort 
     ;; The order of the following is important, since:
     ;; (1) `append' doesn't copy the last arg
     ;; (2) `sort' modifies the list passed to it.
     (append orig-tcl-keyword-list '("set" "incr" "unset" "catch"))
     'string-lessp)
    )
  "The original value of `tcl-keyword-list', appended with a few more
keywords and converted to a regexp.  The additional keywords are:
  - catch
  - set
  - incr
  - unset
One could argue that these 4 are really builtins.  However, Tcl is a scripting
language, which means everything is, ultimately, a builtin.  I felt that these
4 are so fundamental to the language that they needed to be part of the
keyword list, not the builtin list.
{jpw: 6/05}")

(defconst tcl-keyword-list-re
  (eval-when-compile
    (regexp-opt tcl-keyword-list-enhanced)
    )
  "The value of `tcl-keyword-list-enhanced', converted to a regexp.
{jpw: 6/05}")

;; Below are the regexps for the new constants:

;; Not really needed; will handle separately.
(defconst tcl-keywords-with-arg-re
  (eval-when-compile
    (regexp-opt '("namespace" "package"))
    )
  "Special keywords; they take a required arg, which is a known-constant.
{jpw: 6/05}")

(defconst tcl-const-list-re 
  (eval-when-compile
    (regexp-opt '("argc" "argv" "argv0" "errorCode" "env" "stderr" "stdin"
                  "stdout" "errorInfo" "tcl_library"
                  "tcl_patchLevel" "tcl_pkgPath"  "tcl_platform"
                  "tcl_precision" "tcl_traceCompile" "tcl_traceExec"
                  "tcl_version" 
                  "auto_execs" "auto_index" "auto_noexec" "auto_noload"
                  "auto_path"))
    )
  "Builtin Tcl constants/variables.
{jpw: 6/05}")

(defconst tcl-builtins-with-arg-re 
  (eval-when-compile
    (regexp-opt '("array" "info" "interp" "string" "trace" "binary"
                  "file" "clock"
                  ))
    )
  "Special builtins that require another keyword as their first arg.  Not
all of the basic Tcl builtins are included.  The following are intentionally omitted:
  - after
  - encoding
  - history
  - http
  - memory
  - registry
  - resource
They may be added at a later date.
{jpw: 6/05}")

(defconst tcl-builtins-re-1
  (eval-when-compile
    (regexp-opt (append tcl-core-builtins
                        tcl-file-builtins))
    )
  "Minimal fontification of builtins.  Only `tcl-core-builtins' and
`tcl-file-builtins' are included.
{jpw: 6/05}")

(defconst tcl-builtins-re-2
  (eval-when-compile
    (regexp-opt (append tcl-core-builtins
                        tcl-text-builtins
                        tcl-list-builtins
                        tcl-file-builtins))
    )
  "Medium fontification of builtins.  Adds `tcl-text-builtins' and
`tcl-list-builtins' to the set of builtins in `tcl-builtins-re-1'.
{jpw: 6/05}")

(defconst tcl-builtins-re-3
  (eval-when-compile
    (regexp-opt (append tcl-core-builtins
                        tcl-text-builtins
                        tcl-list-builtins
                        tcl-io-builtins
                        tclLib-text-builtins
                        tcl-file-builtins))
    )
  "Even more colorful fontification of builtins.  Adds `tcl-io-builtins' and
`tclLib-text-builtins' to the set of builtins in `tcl-builtins-re-2'.
{jpw: 6/05}")

(defconst tcl-builtins-re-4
  (eval-when-compile
    (regexp-opt (append tcl-core-builtins
                        tcl-text-builtins
                        tcl-list-builtins
                        tcl-io-builtins
                        tclLib-all-builtins
                        tcl-file-builtins))
    )
  "An alternative to `tcl-builtins-re-3' that uses `tclLib-all-builtins'
instead of just `tclLib-text-builtins'.
{jpw: 6/05}")

(defconst tcl-builtins-re-5
  (eval-when-compile
    (regexp-opt (append tcl-core-builtins
                        tcl-text-builtins
                        tcl-list-builtins
                        tcl-io-builtins
                        tclLib-all-builtins
                        tcl-misc-builtins
                        tcl-file-builtins))
    )
  "Fontifies all of the builtins (i.e. everything in `tcl-builtins-re-4'
plus `tcl-misc-builtins'.
{jpw: 6/05}")


;;------------------------------------------------------------
;;
;; Enhancement Functions
;; 


(defun jpw-back-to-matching (delim inv-delim)
  "Move backward until DELIM is found, ignoring any intermediate
\"DELIM ... INV-DELIM\" in the buffer.
{jpw: 7/06}"
  )


(defun jpw-indent-to-matching-brace ()
  "Do not call this function directly.
{jpw: 7/06}"
  (back-to-indentation)
  (let ((old-indent-pos (point))
        (matching-brace-col (if (jpw-back-to-matching '?{ '?})
                                (current-column)))
        );; end bindings
    (goto-char old-indent-pos)
    (if matching-brace-col
        (indent-to matching-brace-col)
      )
    );;end let
  )


(defun jpw-tcl-indent-command (&optional arg)
  "Enhanced indentation for TCL code.
{jpw: 7/06}"
  (interactive "p")
  (case (save-excursion (back-to-indentation)
                        (following-char))
    ;; Comment Case 
    ('?#
     (jpw-comment-indent)
     ) ;; end Comment Case
    ('?}
     (message "Is block end")
     (if (looking-at "} +{")
         (jpw-indent-to-matching-brace)
       ;;else
       (tcl-indent-command arg)
       )
     ) ;; end block-close case
    ;; Default
    (t
     (tcl-indent-command arg)
     ) ;; end default case
    ) ;; end case
  )

;; FIXME: The following TCL code is valid style:
;; 
;;     for { set i 79 } \
;;         { $i < [string length $outStr] } \
;;         { set i [expr $i_0 + 80] } {
;;
;;         set nextspc [string wordstart $outStr [expr $i - 1]]
;;
;; The official TCL mode screws this code up, however:  It double-indents the
;; body of the for-loop.
;; 
;; This should also be permissible:
;; 
;;     for { 
;;        set i 79 
;;     } { 
;;        $i < [string length $outStr]
;;     } { 
;;         set i [expr $i_0 + 80] 
;;     } {
;;
;;         set nextspc [string wordstart $outStr [expr $i - 1]]
;; :
;; :
;; :
;; ...with tunable indentation level inside of the loop-control-expressions.
;;
;; FIXME: Actually, now that I look at it, all line-continuation from inside
;; of "syntactic" {...} strings is fubar.  It all gets overindented.
;;
;; Look at the TCL mode code & fix it.


(defun jpw-smart-indent-relative (&optional arg)
  "Context-sensitive version of indent-relative.
{jpw: 7/06}"
  (interactive "p")
  (back-to-indentation)
  (unless (jpw-comment-indent)
    (indent-relative))
  )


(defun tcl-indent-command-toggler (&optional arg)
  "Perform \"normal\" indenting on the first stroke of the TAB key.
Back-to-back TAB keystrokes, however, indent relative to the current
position.
{jpw: 7/06}"
  (interactive "p")
  (let* ( (last-100-keys (recent-keys))
          (i (1- (length last-100-keys)))
          (last-key (aref last-100-keys i))
          next-to-last-key
         );; end bindings
    (setq i (1- i))
    (setq next-to-last-key (aref last-100-keys i))
    (if (and (equal (vector last-key) [tab])
             (equal last-key next-to-last-key))
        (jpw-smart-indent-relative)
      ;; else:  Not a back-to-back [tab] keypress.
      ;; Indent normally.
      (jpw-tcl-indent-command arg)
      );; end if
    );;end let
  )


(defun tcl-enhance-indentation () 
  ;; Fix the binding of TAB to the normal, std. value, but only for the
  ;; current TCL-mode buffer.
  (local-set-key [tab] 'indent-for-tab-command)
  (set (make-local-variable 'indent-line-function) 
       'tcl-indent-command-toggler)
  (unless comment-end-skip
    (set (make-local-variable 'comment-end-skip) 
         "[ 	]*\\(\\s>\\|\n\\)"))
  )
(remove-hook 'tcl-mode-hook 'tcl-enhance-indentation)
(add-hook 'tcl-mode-hook 'tcl-enhance-indentation)




;;------------------------------------------------------------
;;
;; Font-Lock Support
;; 


(defvar tcl-font-lock-keywords-enhanced nil
  "Enhancements for the standard `tcl-font-lock-keywords' variable.
{jpw: 06/05}")
;; Separate out the defvar from setting the value to make debugging easier.
(setq tcl-font-lock-keywords-enhanced
  (eval-when-compile
    (list
     ;;
     ;; Variables
     '("[$]\\(\\(::\\)?\\sw+\\)" (1 font-lock-variable-name-face))

     ;;
     ;; Internal Variables
     (cons (concat "\\<\\(" tcl-const-list-re "\\)\\>")
           'font-lock-constant-face)

     ;;
     ;; Builtins
     (list (concat "\\<\\("
                   ;; TODO:
                   ;; [jpw] Right now, this just reflects my own tastes.
                   ;; Really, it should be customizable.
                   tcl-builtins-re-3
                   "\\)\\>")
           '(1 'tcl-builtin-face))

     ;;
     ;; Keywords with an extra arg.  May use `tcl-keywords-with-arg-re'
     ;; someday, but right now, this is actually simpler
     (list (concat "\\<\\(namespace\\|package\\)\\>[ \t]+\\(\\w+\\)?")
           '(1 'font-lock-keyword-face)
           '(2 'tcl-keyword-arg1-face t t))

     ;;
     ;; Builtins with an extra arg.
     (list (concat "\\<\\(" 
                   tcl-builtins-with-arg-re
                   "\\)\\>[ \t]+\\(\\sw+\\)?")
           '(1 'tcl-builtin-face)
           ;; Note:  Index will shift depending on how many groups are in
           ;; `tcl-builtins-with-arg-re'.  This is a problem.
           '(3 'tcl-builtin-arg1-face t t))
     ) ;; end list
    ) ;; end eval-when-compile
  ) 
;; end setq

(defun tcl-enhance-font-lock ()
    (set (make-local-variable 'font-lock-defaults)
         (list
          (append tcl-font-lock-keywords tcl-font-lock-keywords-enhanced)
          nil nil '((?_ . "w") (?: . "w"))))
  )

;; Right now, I'm trying to preserve the std. tcl-mode behavior, rather than
;; completely override the `tcl-font-lock-keywords' var.
;; 
;; Unfortunately, we can't use a regexp with the existing tcl-mode code
;; because it expects the keyword to appear in a certain part of the regexp.
;; Which fails.
;;
;; So, we'll resort to a sorted list, instead.  Uncomment the other 3 lines
;; if/when we ever get the default tcl code working with regexps
(setq tcl-proc-list tcl-proc-list-enhanced
      tcl-keyword-list tcl-keyword-list-enhanced
      tcl-typeword-list tcl-typeword-list-enhanced
 ;; tcl-proc-list (list tcl-proc-list-re)
 ;; tcl-keyword-list (list tcl-keyword-list-re)
 ;; tcl-typeword-list (list tcl-typeword-list-re)
 )
(tcl-set-proc-regexp)
(tcl-set-font-lock-keywords)
(add-hook 'tcl-mode-hook 'tcl-enhance-font-lock)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;