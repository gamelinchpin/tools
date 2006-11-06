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
;; of "syntactic" {...} strings is fubar.  It all gets overindented.  Even
;; conditional expressions.
;;
;; Look at the TCL mode code & fix it.


(defun jpw-tcl-indent-to-matching-brace (&optional offset)
  "Do not call this function directly.
Evals to nil if it didn't indent.
{jpw: 7/06}"
  (back-to-indentation)
  (let (matching-brace-col
        matching-brace-indent
        new-indent
        in-a-conditional
        );; end bindings
    (if (not offset)
        (setq offset 0))

    (save-excursion
      (if (jpw-back-to-matching '?{ '?})
          (progn 
            (setq matching-brace-col (current-column))
            (backward-word 1)
            (setq in-a-conditional 
                  (looking-at "\\(elseif\\|if\\|while\\)"))
            )
        );;end if
      (jpw-backward-extended-line)
      (back-to-indentation)
      (setq matching-brace-indent (current-column))
      );;end excursion

    (if matching-brace-col
        (if in-a-conditional
            (tcl-indent-command)
          ;; else
          (setq new-indent (+ matching-brace-indent offset))
          (if (< new-indent (current-indentation))
              (save-excursion 
                (let ((indentp (progn (back-to-indentation)
                                      (point)))
                      (bolp (progn (beginning-of-line)
                                   (point)))
                      );; end vars
                  (delete-region bolp indentp)
                  );; end let
                );; end excursion
            )
          (indent-to new-indent)
          )
      ;; else
      nil
      )
    );;end let
  )


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
  ;; 1. The column of the matching open-brace of this line
  ;; 2. The indentation of the line containing the open-brace matching the
  ;;    close-brace on this line.
  ;; 3. A symbol indicating parent-statement.
  ;; 4. The indentation of the parent-statement.
  ;; 5. The location of the ?{ on the line of the parent-statement
  (let ((fwd-bound (point))
        matching-open-brace-line-indent
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
            (if (jpw-back-to-matching '?{ '?} nil fwd-bound)
                (setq last-matching-open-brace-column (current-column))
              ;; else
              ;; Move off-char in case `jpw-back-to-matching' didn't move
              ;; point.   Otherwise, we risk an infinite loop. 
              (forward-char -1)
              )
            (back-to-indentation)
            )
          (setq parent-statement-info (jpw-tcl-statement-data))
          (append matching-open-brace-indent
                  matching-open-brace-line-indent
                  parent-statement-info ;; 2 elts.
                  last-matching-open-brace-column)
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
         (parent-type (nth 2 indent-info))
         ) ;; end vars
    (if parent-type
        (cond
         ;; for-loops get special handling.
         ((eq parent-type 'for)
          (if jpw-tcl-align-for-loop-braces
              (nth 4 indent-info)
            ;; else:
            ;; Offset it from the "for" statement.
            (+ jpw-tcl-for-loop-bracepair-offset (nth 3 indent-info))
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
       )      ;;sdfqretfwe asdrewq
  )


(defsubst jpw-tcl-analyze-extended-line ()
  ;; If the present line is part of a continued line, evaluates to a 5-element
  ;; list containing: 
  ;; 1. The type of parent statement;
  ;; 2. The indentation of the 1st line in the statement;
  ;; 3. The point where the 1st line in the statement starts;
  ;; 4. The type of opening paren on the 1st line of the statement (or nil if
  ;;    none);
  ;; 5. The column of (4), or nil.
  ;; If the present line isn't an extended line, evaluates to nil.
  ;;
  ;; This function does not change `point'.
  (save-excursion
    (if (jpw-backward-extended-line)
        (let ((parent (progn (back-to-indentation)
                             (jpw-tcl-statement-data))
              );;end vars
          (if (re-search-forward "[[{\(]" (line-end-position) t)
              (append parent
                      (current-indentation) 
                      (line-beginning-position)
                      (list (char-after) (current-column)))
            ;; else
            (append parent 
                    (current-indentation) 
                    (line-beginning-position)
                    nil nil)
            )
          );; end let
      ;; else 
      nil
      )
    )
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
          (nth 4 tmp-extended-line-info)
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
        (statement-bolp (nth 2 extended-line-info))
        nearest-open-grp
        nearest-open-grp-bolp
        open-grp-column
        statement-starts-group
        ) ;; end vars

    (setq nearest-open-grp 
          (re-search-backward "[[\({]" 
                              (nth 2 extended-line-info)
                              t))
    (if nearest-open-grp
        (setq open-grp-column (current-column)
              nearest-open-grp-bolp (line-beginning-position)
              ;; - Does the nearest paren/bracket/brace equal the one in the
              ;;   parent? 
              ;; - Is the start of the line the same as the start of the
              ;;   parent line?
              statement-starts-group (and (eq nearest-open-grp 
                                              statement-open-paren)
                                          (= nearest-open-grp-bolp 
                                             statement-bolp))
              )
      )

    ;; Some cases and their controlling conditionals.

    ;; No enclosing paren/brace/bracket?  Nothing special to do.  Let the
    ;; normal TCL mode handle things.
    ;; Only continue if we're inside of a grouping (i.e. a
    ;; paren/brace/bracket).
    (if nearest-open-grp
        ;; - Is this a for/foreach loop?
        ;; - Does the nearest paren/bracket/brace equal the one starting the
        ;;   loop? 
        ;; - Is the nearest paren/bracket/brace on the line starting the loop?
        (if (and statement-starts-group
                 (or (eq statement-type 'for)
                     (eq statement-type 'foreach)
                     ))
            ;; for/foreach loops require special handling.  Of course.
            do-for-or-foreach-case

          ;; else
          ;; Indent by type of paren:
          (if (eq nearest-open-grp '?\[)
              ;; - A ?[ always has the same indent, regardless of context.
              (jpw-tcl-compute-bracket-indent)

            ;; else
            ;; We must be inside of a (...) or {...} pair.
            (if (re-search-forward "\\S " (line-end-position) t)
                ;; Align to the 1st non-ws char after the paren.
                (if (eq nearest-open-grp '?{)
                    ;; For a curly, add an optional offset to the paren posn.
                    (+ (current-column) jpw-tcl-some-offset)
                  ;; else:
                  ;; eq ?\(
                  (current-column)
                  );; end if search-for-1st-non-ws-char

              ;; else
              ;; Fallback:  Use the previous line's indent.
              (goto-char old-pos)
              (jpw-last-line-indentation)
              );; end indent-handler-if
            );;end if '?[

          );;end if in-a-for-loop

      );; end if nearest-open-grp
    ) ;; end let
  )


(defsubst jpw-tcl-compute-group-internal-indent()
      (let* ((nearest-open-grp (re-search-backward "[[\({]" 
                                                   (nth 2 extended-line-info)
                                                   t))
             (statement-type (car extended-line-info))
             (statement-open-paren (nth 3 tmp-extended-line-info))
             );; end vars
        );; end let
  )


(defun jpw-tcl-compute-enhanced-indent ()
  "Compute a more syntactically-correct indent than the old TCL mode indent
function does.
{jpw: 11/06}"
  (save-excursion
    (back-to-indentation)
    (let ((starting-char (char-after))
          (eolp (line-end-position))
          tmp-extended-line-info
          );; end vars
      (cond

       ;; Case #1:  Are we looking at a ")"?
       ((eq starting-char '?\))
        ;; Find its matching "(" and align to it, always.
        (if (jpw-back-to-matching '?\( '?\))
            (current-indentation)
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

       ;; Case #6:  Are we inside of the start, test, or next expressions of a
       ;; multi-line for-loop?
       ;;
       ;; If so, align to the previous line's "{", plus a user-defined offset.
       (nil
        )

       ;; Case #7:  Are we the first statement in the body of a for-loop (or
       ;; foreach-loop) that starts itself with extended lines?
       ;;
       ;; If so, align to the parent "for" (or "foreach") statement plus
       ;; tcl-indent-level plus an optional offset.
       ;;
       ;; Alternative:  have a control flag (e.g.
       ;; "correct-stoopid-tcl-for-loop-indenting") and, if it's set, check for
       ;; this case, and indent as described above but without any optional
       ;; offset.
       (nil
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


(defun tcl-enhance-indentation () 
  ;; Fix the binding of TAB to the normal, std. value, but only for the
  ;; current TCL-mode buffer.
  (local-set-key [tab] 'indent-for-tab-command)
  (local-set-key "\M-j" 'jpw-tcl-indent-new-comment-line)
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