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
  (list ?! )
  (list "Left$" "Mid$" "Right$" "In$" "If" "For" "\"")
  (list
   (list
    "[^\\]\\('\\([^\\']*\\(\\'\\)*\\)*'\\)"  
     '(1 font-lock-string-face))
   (list
    "\\(XRef[^$]+\\)" 
    '(1 font-lock-builtin-face))
   (list
    "\\(AddFn\\)" 
    '(1 font-lock-function-name-face))
   (list
    "\\({\\(double\\|int\\|string\\)}\\)" 
    '(1 font-lock-type-face))
   (list
    "\\(\\.\\([bfi]\\d+\\|[cs]\\)\\)" 
    '(1 font-lock-type-face))
   (list
    "\\(rds\\|txn\\)" 
    '(1 font-lock-constant-face))
   (list
    "\\([A-Za-z0-9_]+\\)\\(##?[sid]\\)" 
    '(1 font-lock-variable-name-face)
    '(2 font-lock-type-face))
   )
  (list "\\.cfg\\'" "\\.templt\\'")
  nil
  "Generic Mode for ExpressionEvaluator files.")





;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;