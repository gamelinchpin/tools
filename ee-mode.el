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

(define-generic-mode 'ee-generic-mode
  nil  ;; Can't handle comment here.  Ends up matching the != op.
  (list "Left\\$" "Mid\\$" "Right\\$" "In\\$" "If" "For")
  (list
   (list  "\\(^\\s *!.*$\\)" '(1 font-lock-comment-face t))
   (list
    "\\('[^\\\\']*\\(\\\\.[^\\\\']*\\)*'\\)"
    '(1 font-lock-string-face append))
   (list
    "\\(XRef\\w+\\$\\)" 
    '(1 font-lock-function-name-face prepend))
   (list
    "\\(AddFn\\)" 
    '(1 font-lock-builtin-face prepend))
   (list
    "\\({\\(double\\|int\\|string\\)}\\)" 
    '(1 font-lock-type-face prepend))
   (list
    "\\(\\.\\([bfi][0-9]+\\|[cs]\\)\\)" 
    '(1 font-lock-type-face prepend))
   (list
    "\\(rds\\|txn\\)" 
    '(1 font-lock-constant-face prepend))
   (list
    "\\(\\w+\\)\\(##?[sid]\\)" 
    '(1 font-lock-variable-name-face prepend)
    '(2 font-lock-type-face prepend))
   ;;
   ;; Optional font-locking.  Right now, controlled by commenting out. :P
   ;;
   ;;;(list "^\\s *\\([,(){}]\\)" '(1 font-lock-constant-face prepend))
   ;;;(list "\\(\"\\)\\s *$" '(1 font-lock-warning-face t))
   (list "\\([;,]\\)\\s *$" '(1 font-lock-builtin-face prepend))
   (list "^\\s *\\(,\\)\\s *$" '(1 font-lock-builtin-face prepend))
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
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;