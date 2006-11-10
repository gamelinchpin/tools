;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some enhancements for the default TCL-mode that comes with Emacs
;;
;;  Copyright Å© 2005-2006 John P. Weiss
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


;;:type:
  ;; One of: integer, number, string, regexp, character, hook,
  ;;         symbol, function, variable, boolean


(defcustom jpw-tcl-allow-comment-unindent t
  "When set to non-nil, allows the function `jpw-tcl-comment-indent-command'
to \"indent backward\".  By default, `jpw-tcl-comment-indent-command' ignores
comments with internal indentation deeper than the previous line.
{jpw: 7/06}"
  :type '(boolean variable)
  :group 'tcl)


(defcustom jpw-tcl-align-for-loop-braces nil
  "Used to control indentation of a TCL for-loop written in a certain style.

When writing complex TCL for-loops, some people like to put the body of each
clause on its own line, placing the separating \"} {\" on a line by itself.
Setting `jpw-tcl-align-for-loop-braces' to non-nil will align those close-open
braces like so:

    for { 
            ...
        } {
            ...
        } {
            ...
        } {
        :
        :
    }

Otherwise, indentation of the  \"} {\" lines is controlled by
`jpw-tcl-for-loop-bracepair-offset'.
{jpw: 7/06}"
  :type '(boolean variable)
  :group 'tcl)


(defcustom jpw-tcl-for-loop-bracepair-offset 0
  "Used to control indentation of a TCL for-loop written in a certain style.

This is the other way of controlling  complex TCL for-loops writtne in the
style described by `jpw-tcl-align-for-loop-braces'.  When
`jpw-tcl-align-for-loop-braces' is nil, the enhanced indenation engine will
align each \"} {\" line with the \"for\" statement\(1\), plus an additional
offset specified by this variable.  The default is 0, which means align with
the \"for\" statement\(1\).

If, for example, you set this to 2, your for-loop would look like this:

    for { 
          ...
      } {
          ...
      } {
          ...
      } {
        :
        :
    }

Otherwise, indentation of the \"} {\" lines is controlled by
`jpw-tcl-for-loop-bracepair-offset'.

\(1\) Well, this is *almost* true.  It really aligns the \"} {\" lines with the
*line* containing the \"for\" command.  Usually, though, the  \"for\" command
is the first thing on the line.
{jpw: 7/06}"
  :type '(integer variable)
  :group 'tcl)


(defcustom jpw-tcl-align-body-to-for-statement t
  "In complex for-loops, indent the first line of the body relative to the
statement's first line, and not the body's opening brace.

This setting only affects for-loops written in the following style:

    for { ... } \
        { ... } \
        { ... } {
        :
        :
    }

By default, `tcl-mode' aligns the first statement in the body to its opening
brace, i.e. it uses the indentation of the line containing the brace, plus
`tcl-indent-level'.  As a result, every other statement in the body is
indented an \"extra\" `tcl-indent-level' relative to the original \"for\"
statement.  

That includes the closing \"}\", which end up aligned with the opening braces
instead of the \"for\" command.

Some people consider this ugly {I'm one}.  When this variable is non-nil, the
enhanced indenting engine will align the first statement of the for-loop's
body relative to the \"for\" command.

Set it to nil to restore the default `tcl-mode' behavior.
{jpw: 7/06}"
  :type '(boolean variable)
  :group 'tcl)


(defcustom jpw-tcl-align-to-arg t
  "When indending, how to align continued statements enclosed in a \[ ... \]
pair.  When non-nil, aligns to the first word after the command in the
\[ ... \].  Otherwise, let `tcl-mode' do the indendation.
{jpw: 7/06}"
  :type '(boolean variable)
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


(defsubst jpw-tcl-statement-data ()
  ;; Return a 2-element list describing the statement at `point'.
  (list (cond ((looking-at "catch") 'catch)
              ((looking-at "elseif") 'elseif)
              ((looking-at "for ") 'for)
              ((looking-at "foreach") 'foreach)
              ((looking-at "if") 'if)
              ((looking-at "while") 'while)
              (t 'other-tcl-statement))
        (current-indentation))
  )


(defsubst jpw-tcl-analyze-brace-pair ()
  ;; Return the indentation-information of the parent of a brace-pair, or nil
  ;; if we're not currently looking at a "} {" pair.
  ;; The "indentation-information" is a 3-element list containing (in order):
  ;; 1. A symbol indicating parent-statement.
  ;; 2. The indentation of the parent-statement.
  ;; 3. The column of the matching open-brace of this line
  ;; 4. The indentation of the line containing the open-brace matching the
  ;;    close-brace on this line.
  ;; 5. The location of the ?{ on the line of the parent-statement
  (let (matching-open-brace-line-indent
        matching-open-brace-indent
        last-matching-open-brace-column
        parent-statement-info
        );;end vars
    (if (looking-at "} +{")
        (progn
          (setq matching-open-brace-line-indent (current-column)
                matching-open-brace-indent 
                (+ (current-column) (- (match-end 0) (match-beginning 0))))
          (while (looking-at "} +{")
            (if (jpw-back-to-matching '?{ '?})
                (setq last-matching-open-brace-column (current-column))
              ;; else
              ;; Move off-char in case `jpw-back-to-matching' didn't move
              ;; point.   Otherwise, we risk an infinite loop. 
              (forward-char -1)
              )
            (back-to-indentation)
            )
          (setq parent-statement-info (jpw-tcl-statement-data))
          (append parent-statement-info ;; 2 elts.
                  (list matching-open-brace-indent
                        matching-open-brace-line-indent
                        last-matching-open-brace-column))
          );; end progn
      ;;else
      nil
      );; end outermost-if
    );; end let
  )


(defsubst jpw-tcl-compute-brace-pair-indent ()
  ;; Computes the indentation to use for a "} {" pair.  Evals to nil if
  ;; `point' isn't currently looking at /} +{/.
  (let* ((indent-info (jpw-tcl-analyze-brace-pair))
         (parent-type (car indent-info))
         ) ;; end vars
    (if parent-type
        (cond
         ;; for-loops get special handling.
         ((eq parent-type 'for)
          (if jpw-tcl-align-for-loop-braces
              (nth 4 indent-info)
            ;; else:
            ;; Offset it from the "for" statement.
            (+ jpw-tcl-for-loop-bracepair-offset (nth 1 indent-info))
            )
          );; end case: for
         ;; Indent other known statements (if, foreach, while, and so on) so
         ;; that the close-brace aligns with the first open-brace after the
         ;; statmenet.
         ((not (eq parent-type 'other-tcl-statement))
          (nth 4 indent-info)
          );; end case: other known statement.
         ;; Default:  let normal TCL mode handle it.
         (t
          nil)
         ) ;; end cond
      ) ;; end if
    ) ;; end let
  )


(defsubst jpw-tcl-compute-1starg-indent ()
  ;; Find the indendation column of the 1st arg of a command.  Assumes that
  ;; `point' is on the command, or on a \[ character.
  ;; If there's only one word on the line after `point' (and before any '\\'
  ;; and the end of the line), return 'nil'.
  (and (re-search-forward "\\s *" (line-end-position) t)
       (re-search-forward "\\S *" (line-end-position) t)
       (re-search-forward "\\s *" (line-end-position) t)
       (not (looking-at "\\\\\\s *$"))
       (current-column)
       )
  )


(defsubst jpw-tcl-analyze-extended-line ()
  ;; If the present line is part of a continued line, evaluates to a 5-element
  ;; list containing: 
  ;; 1. The type of parent statement;
  ;; 2. The indentation of the 1st line in the statement;
  ;; 3. The `point' where the 1st line in the statement begins;
  ;; 4. The type of opening paren on the 1st line of the statement (or nil if
  ;;    none);
  ;; 5. The column of (4), or nil.
  ;; If the present line isn't an extended line, evaluates to nil.
  ;;
  ;; This function does not change `point'.
  (save-excursion
    (if (jpw-backward-extended-line)
        (let ((parent-point (progn (back-to-indentation) 
                                   (point)))
              (parent (jpw-tcl-statement-data))
              );;end vars
          (if (re-search-forward "[[{\(]" (line-end-position) t)
              (progn (forward-char -1)
                     (append parent
                             (list parent-point
                                   (char-after) (current-column))))
            ;; else
            (append parent (list parent-point
                                 nil nil))
            )
          );; end let
      ;; else 
      nil
      );; end if backward-extended-line
    );; end excursion
  )


(defsubst jpw-tcl-compute-extended-close-brace-indent ()
  ;; Compute the indent for a line that starts with ?} and is part of an
  ;; extended TCL statement.
  ;; There are 3 cases, described below.
  (let* ((extended-line-info (jpw-tcl-analyze-extended-line))
         (statement-type (car extended-line-info))
         );;end vars
    (cond
     ;; Case (a):
     ;; The for-statement is the exception, of course.  If the
     ;; previous line begins with a "} {" pair, align to the 1st
     ;; "{" in the statement.  Otherwise, let TCL-mode handle
     ;; things.
     ((eq statement-type 'for)
      (forward-line -1)
      (back-to-indentation)
      (if (looking-at "}\\s +{")
          (nth 4 extended-line-info)
        nil)
      )
     ;; Case (b):
     ;; If we're looking at some non-conditional statement, then
     ;; find the matching curly and align to it. 
     ((eq statement-type 'other-tcl-statement)
      (and (jpw-back-to-matching '?{ '?})
           (current-column))
      )
     ;; Case (c):
     ;; An if, while, catch, or other control statement should
     ;; align the close to the statement.  Which is the default
     ;; behavior - i.e. eval to nil.
     ;;
     ;; This also handles the case of a nil `extended-line-info' - i.e. this
     ;; isn't part of an extended TCL statement.  We want the default TCL-mode
     ;; behavior in this case, too.
     (t nil)
     ) ;; end cond
    );;end let
  )


(defsubst jpw-tcl-compute-bracket-indent ()
  ;; Find the matching "[".  Returns the column of the beginning of the second
  ;; word after the "[".
  ;; If the user disables this behavior, do nothing and eval to nil.
  (if (and jpw-tcl-align-to-arg
           (jpw-back-to-matching '?\[ '?\]))
      (jpw-tcl-compute-1starg-indent)
    ;; else:  Let TCL-mode handle it.
    nil
    )
  )


(defsubst jpw-tcl-compute-extended-line-indent (extended-line-info)
  ;; Computes the indentation for an extended TCL statement.
  ;; 
  ;; EXTENDED-LINE-INFO must be non-nil; the behavior of this function is
  ;; undefined otherwise.
  (let ((old-pos (point))
        (statement-type (car extended-line-info))
        (statement-open-paren (nth 3 extended-line-info))
        (statement-bolp (- (nth 2 extended-line-info)
                           (cadr extended-line-info)))
        nearest-open-grp
        nearest-open-grp-bolp
        open-grp-column
        last-line-indent
        last-line-bolp
        tmp-1st-arg-column
        ) ;; end vars
          
    (if (jpw-back-to-matching "\([{" "\)]}" statement-bolp t)
        (setq nearest-open-grp (char-after)
              open-grp-column (current-column)
              nearest-open-grp-bolp (line-beginning-position)
              )
      ;; else
      ;; This is a plain-ol-extended statement (or at least, this line is).
      ;; We'll just hijack the control variables for this special case.
      (goto-char old-pos)
      (setq nearest-open-grp '?\\
            nearest-open-grp-bolp (line-beginning-position))
      (forward-line -1)
      (setq last-line-bolp (line-beginning-position)
            last-line-indent (current-indentation))
      )

    ;; Some cases and their controlling conditionals.

    (if nearest-open-grp
        ;; Indent by type of paren:
        (cond
         ;; A ?[ always has the same indent, regardless of context.
         ((eq nearest-open-grp '?\[)
          (goto-char old-pos)
          (jpw-tcl-compute-bracket-indent)
          )

         ;; A ?\\ means "not in any parens".  If this also happens to be a
         ;; for-loop, do nothing.  Let `tcl-mode' handle it.
         ((and (eq nearest-open-grp '?\\)
               (eq statement-type 'for))
          nil
          )

         ;; A ?\\ means "not in any parens".  This case indents almost like an
          ;; in-?[ statement. 
         ((eq nearest-open-grp '?\\)
          (if (= statement-bolp last-line-bolp)
              (progn
                (goto-char statement-bolp)
                (back-to-indentation)
                (setq tmp-1st-arg-column (jpw-tcl-compute-1starg-indent))
                (if tmp-1st-arg-column
                    (if (and (eq (char-after) ?\")
                             (not (progn
                                    (forward-char)
                                    (re-search-forward "[^\\]\"" 
                                                       (line-end-position)
                                                       t))
                                  );;end not
                             );; end and
                        (1+ tmp-1st-arg-column)
                      ;; else
                      tmp-1st-arg-column
                      )
                  )
                );; end progn
            ;; else
            ;; Indent relative to the previous line in this extended command.
            last-line-indent
            );; end if
          )

         ;; else
         ;; We must be inside of a (...) or {...} pair.
         (t
          ;; Move off of the brace/paren, so we don't match it.
          (forward-char)
          (if (re-search-forward "\\S " (line-end-position) t)
              ;; Align to the 1st non-ws char after the paren.
              (1- (current-column))
            ;; else
            ;; Fallback:  Use the previous line's indent.
            (goto-char old-pos)
            (jpw-last-line-indentation)
            );; end indent-handler-if
          )

         );;end cond
      );; end if nearest-open-grp
    ) ;; end let
  )


(defun jpw-tcl-compute-enhanced-indent ()
  "Compute a more syntactically-correct indent than the old TCL mode indent
function does.
{jpw: 11/06}"
  (save-excursion
    (back-to-indentation)
    (let ((starting-char (char-after))
          tmp-extended-line-info
          );; end vars
      (cond

       ;; Case #1:  Are we looking at a ")"?
       ((eq starting-char '?\))
        ;; Find its matching "(" and align to it, always.
        (if (jpw-back-to-matching '?\( '?\))
            (current-column)
          ;; else:  Let TCL-mode handle it.
          nil
          )
        )

       ;; Case #2:  Are we looking at a "]"?
       ((eq starting-char '?\])
        (jpw-tcl-compute-bracket-indent)
        )

       ;; Case #3 & #4: Closing curly-brace.
       ((eq starting-char '?})
        (if (looking-at "}\\s +{")
            ;; Case #3:  We're starting a line off with a closing curly-brace
            ;; followed by an open curly-brace.
            (jpw-tcl-compute-brace-pair-indent)
          ;; else:
          ;; 
          ;; We're looking at a "}" followed by something else.
          ;; Case #4:  It's part of an extended line.
          ;; {Note that `jpw-tcl-compute-brace-pair-indent' evals to nil if
          ;; Case #4 isn't in effect.}
          (jpw-tcl-compute-extended-close-brace-indent)
          ) ;; end outer-if
        )

       ;; Case #5:  Are we looking at an extended line?
       ((setq tmp-extended-line-info (jpw-tcl-analyze-extended-line))
        (jpw-tcl-compute-extended-line-indent tmp-extended-line-info)
        )

       ;; Case #6:  Are we the first statement in the body of a for-loop that
       ;; starts itself with extended lines?  Also, has the user set
       ;; `jpw-tcl-align-body-to-for-statement' non-nil?
       ;;
       ;; If so, align to the parent "for" statement plus `tcl-indent-level'.
       ((and jpw-tcl-align-body-to-for-statement
             (jpw-last-line-column-of "{")
             (progn (jpw-next-nonblank-line -1) t)
             (setq tmp-extended-line-info (jpw-tcl-analyze-extended-line))
             (eq (car tmp-extended-line-info) 'for)
             )
        (+ (cadr tmp-extended-line-info) tcl-indent-level)
        )

      ) ;; end cond
    ) ;; end let
  ) ;; end excursion
)


(defun jpw-tcl-indent-new-comment-line (&optional soft)
  "Wrapper around `comment-indent-new-line' that corrects the indendation
after breaking the line.
{jpw: 10/06}"
  (interactive)
  (comment-indent-new-line soft)
  (jpw-tcl-indent-comment)
)


(defsubst jpw-tcl-indent-comment (&optional arg)
  ;; Mode-specific version of `jpw-indent-comment'
  (jpw-indent-comment)
  (tcl-indent-command arg)
  )


(defun jpw-tcl-indent-command (&optional arg)
  "Enhanced indentation for TCL code.
{jpw: 7/06}"
  (interactive "p")
  (let (initial-char
        indentp
        enhanced-indent
        );; end vars

    (save-excursion 
      (back-to-indentation)
      (setq indentp (point))
      (setq initial-char (following-char))
      ) ;;end excursion

    (if (eq initial-char '?#)
        (jpw-tcl-indent-comment)
      ;; else
      (setq enhanced-indent (jpw-tcl-compute-enhanced-indent))
      (if enhanced-indent
          (progn
            ;; Determine whether or not we should indent backward.
            (if (and (or tcl-tab-always-indent
                         (<= (current-column) (current-indentation)))
                     (< (current-indentation) enhanced-indent))
                (save-excursion
                  (delete-region (progn (beginning-of-line)
                                        (point))
                                 indentp)
                  ) ;;end excursion
              )
            (save-excursion (back-to-indentation)
                            (indent-to enhanced-indent)
                            );; end excursion
            (if (<= (current-column) (current-indentation))
                (back-to-indentation))
            )
        ;; else
        (tcl-indent-command arg)
        ) ;; end if enhanced-indent
      ) ;;end outer-if
    ) ;; end let
  )


(defun jpw-smart-indent-relative (&optional arg)
  "Context-sensitive version of indent-relative.
{jpw: 7/06}"
  (interactive "p")
  (back-to-indentation)
  (if (jpw-comment-internal-indentation)
      (jpw-tcl-indent-comment)
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


(defun jpw-tcl-electric-hash (&optional count)
  "A wrapper around `tcl-electric-hash' that uses the enhanced indentation of
`jpw-tcl-indent-command'.
{jpw: 11/06}"
  (interactive "p")
  (tcl-electric-hash count)
  (save-excursion
    (back-to-indentation)
    (jpw-tcl-indent-command nil))
  )


(defun jpw-tcl-electric-brace (arg)
  "A wrapper around `tcl-electric-brace' that uses the enhanced indentation of
`jpw-tcl-indent-command'.
{jpw: 11/06}"
  (interactive "p")
  (tcl-electric-brace arg)
  (save-excursion
    (back-to-indentation)
    (jpw-tcl-indent-command nil))
  )


(defun jpw-tcl-electric-char (arg)
  "A wrapper around `tcl-electric-char' that uses the enhanced indentation of
`jpw-tcl-indent-command'.
{jpw: 11/06}"
  (interactive "p")
  ;; Rather than call this, mess up the indentation of the existing line, then
  ;; correct it (which all looks rather noisy on screen), we'll just steal the
  ;; implementation of `tcl-electric-char'
  (self-insert-command arg)
  (if (and tcl-auto-newline (= last-command-char ?\;))
      (progn (newline)
             (tcl-indent-line))
    )
  ;;(tcl-electric-char arg)
  (save-excursion
    (back-to-indentation)
    (jpw-tcl-indent-command nil))
  )


(defun tcl-enhance-indentation () 
  ;; Fix the binding of TAB to the normal, std. value, but only for the
  ;; current TCL-mode buffer.
  (local-set-key [tab] 'indent-for-tab-command)
  (local-set-key "\M-j" 'jpw-tcl-indent-new-comment-line)
  (local-set-key "#" 'jpw-tcl-electric-hash)
  (local-set-key "}" 'jpw-tcl-electric-brace)
  (local-set-key "{" 'jpw-tcl-electric-char)
  (local-set-key "[" 'jpw-tcl-electric-char)
  (local-set-key "]" 'jpw-tcl-electric-char)
  (local-set-key ";" 'jpw-tcl-electric-char)
  (set (make-local-variable 'indent-line-function) 
       'tcl-indent-command-toggler)
  (unless comment-end-skip
    (set (make-local-variable 'comment-end-skip) 
         "[ 	]*\\(\\s>\\|\n\\)"))

  ;; Configure paragraph start/separate vars
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start  "[ 	]*\\(#[ 	]*\\)?$\\|^\f"
        paragraph-separate paragraph-start)
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