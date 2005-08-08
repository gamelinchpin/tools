;;; ==== SMARTS-wide extensions ====

;;; Establish indentation styles for C and C++ code

;; SMARTS indentation style indents a function as follows:
;; function_type
;; function_name
;;     (arg_1_type arg_1,
;;      arg_2_type arg_2, ...
;;
;; Unfortunately the function_name and the beginning of the argument list
;; are considered "topmost-intro-cont".
;;
;; c-smarts-topmost-intro-cont determines which case and returns the relevant
;; indentation amount -- c-basic-offset for an argument list beginning, and
;; 0 for all other cases.
(defun c-smarts-indent-topmost-intro-cont (langelem)
  (save-excursion
    ;; Skip whitespace
    (c-forward-syntactic-ws)

    ;; Is the next element the paren of an arglist or anything else?
    (if	(looking-at "(")
	c-basic-offset 0)
    ))

;; Install SMARTS-C indentation style if not already installed
;; Set SMARTS-C indentation style if no other style is requested.
(add-hook
 'c-mode-hook
 '(lambda ()
    (or (assoc "SMARTS-C" c-style-alist)
	(progn
	  (c-add-style
	   ;; Style name
	   "SMARTS-C"
	   ;; Customizations of SMARTS-C style
	   '((c-basic-offset . 4)
	     (c-file-offsets
	      .
	      (
	       (topmost-intro-cont . c-smarts-indent-topmost-intro-cont)
	       (substatement-open . 0)
	       )
	      ))
	   ;; Don't set this style for the current buffer
	   nil
	   )
	  (message "Added & set SMARTS C style")
	  ))
    ;; Only override style if not explicitly set
    (or
     c-file-style;; file-variables-hook set by user
     (progn
       ;; Style left unset -- add and set SMARTS style
       (c-set-style "SMARTS-C")
       (message "Set C indentation style to SMARTS-C"))
     ))
 )

;; Install SMARTS-C++ indentation style if not already installed
;; Set SMARTS-C++ indentation style if no other style is requested.
(add-hook
 'c++-mode-hook
 '(lambda ()
    (or (assoc "SMARTS-C++" c-style-alist)
	(progn
	  (c-add-style
	   ;; Style name
	   "SMARTS-C++"
	   ;; Customizations of SMARTS-C++ style
	   '((c-basic-offset . 4)
	     (c-file-offsets
	      .
	      (
	       (topmost-intro-cont . c-smarts-indent-topmost-intro-cont)
	       (substatement-open . 0)))
	     )
	   ;; Don't set this style for the current buffer
	   nil
	   )
	  (message "Added SMARTS-C++ indentation style")
	  ))
    ;; Only override style if not explicitly set in file local variables
    (or
     c-file-style;; file-variables-hook set by user
     (progn
       ;; Style left unset -- add and set SMARTS style
       (c-set-style "SMARTS-C++")
       (message
	"Defaulting indentation style to SMARTS-C++"))
     )
    )
 )

;;; Mark the completion of running this file in the *Messages* buffer
(message "Ran site-start for SMARTS -- 28 Aug 97 v 1")
