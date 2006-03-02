;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Setup for Font-lock colors
;  Various variables and functions
;  used to make handling Font-Lock mode a 
;  little bit easier....
;
;  Only needed for pre v20.* of Emacs.
;
;  last modified 9/98            {jpw}
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'font-lock)


(defun _major-mode-font-variable-name ()
  "Returns the string used by `set-user-font-lock-fonts' that
corresponds to the current major mode."
  (setq modestr (substring 
			  (downcase (symbol-name (symbol-value 'major-mode)))
			  0 -5)
	   )
  (cond ( (or (string= "lisp" modestr)
		    (string= "lisp" (substring modestr -4))
		    )
		(concat "lisp")
		)
	   
	   ( (string= "c" (substring modestr 0 1))
		(concat "c")
		)

	   ( (string= "tex" (substring modestr -3))
		(concat "tex")
		)

	   ( (or (string= "fortran" modestr)
		    (string= "idl" modestr)
		    (string= "html" modestr)
		    )
		(concat modestr)
		)

	   (t (concat "t"))
	   )
  )




;----------------------------------------------------------
; Variable list section


;
; user default fonts
;
;
(defvar user-comment-font '("red3")
  "*Default font for comments.
See the documentation for the variable `user-string-font'.")
(defvar user-keyword-font '("magenta3")
  "*Default font for keywords.
See the documentation for the variable `user-string-font'.")
(defvar user-function-font '("Blue")
  "*Default font for functions.
See the documentation for the variable `user-string-font'.")
(defvar user-variable-font '("orange3" nil t)
  "*Default font for variables.
See the documentation for the variable `user-string-font'.")
(defvar user-type-font '("Navy")
  "*Default font for data types.
See the documentation for the variable `user-string-font'.")
(defvar user-reference-font '("green4")
  "*Default font for references.
See the documentation for the variable `user-string-font'.")
(defvar user-string-font '("DeepSkyBlue3" nil nil t nil)
  "*Default font for strings.

The method for setting this variable and its cousins is:

`(setq mmm-fff-font '(\"FgColor\" \"BgColor\" BOLD ITALIC UNDERLINE))'

where `mmm-fff-font' is the name of the variable to set,

e.g. `user-reference-font' or `c-string-font'.  

The strings \"FgColor\" and \"BgColor\" must be valid color names for
the foreground and background colors, respectively.  (Check out
`Display Faces' or `Display Colors' in the `Text Properties' submenu
of the `Edit' menu).  If you don't want to alter the foreground color,
put 'nil' in place of \"FgColor\".

To turn on boldface, italics, or underline, put a 't' in place of
`BOLD' `ITALIC' or `UNDERLINE'.  Put 'nil' in place of any options you
don't want to set.

For example, the default value of this variable is 
`(\"DeepSkyBlue3\" nil nil t nil)' which sets the foreground 
color to \"DeepSkyBlue3\", the background color to the default, turns on
italics and leaves underlining and boldface off.


The default vaules of each of the fonts is:
user-string-font:           (\"DeepSkyBlue3\" nil nil t nil)
user-keyword-font:          (\"magenta3\")
user-function-font:         (\"Blue\")
user-variable-font:         (\"orange3\" nil t)
user-type-font:             (\"Navy\")
user-reference-font:        (\"green4\")
user-comment-font:          (\"red3\")


If you set *any* of these to `nil', the default font-lock-mode value
is used.

See the documentation for the function `change-fonts-font-lock-mode'
for more info.


{jpw 11/95}")


;
; lisp-*-font
;
(defvar lisp-comment-font nil
  "*Default font for lisp comments.
See the documentation for the variable `user-string-font'.")
(defvar lisp-keyword-font nil
  "*Default font for lisp keywords.
See the documentation for the variable `user-string-font'.")
(defvar lisp-function-font nil
  "*Default font for lisp functions.
See the documentation for the variable `user-string-font'.")
(defvar lisp-variable-font nil
  "*Default font for lisp variables.
See the documentation for the variable `user-string-font'.")
(defvar lisp-type-font nil
  "*Default font for lisp data types.
See the documentation for the variable `user-string-font'.")
(defvar lisp-reference-font nil
  "*Default font for lisp references.
See the documentation for the variable `user-string-font'.")
(defvar lisp-string-font nil
  "*Default font for lisp strings.
See the documentation for the variable `user-string-font'.")


;
; c-*-font
;
(defvar c-comment-font nil
  "*Default font for c comments.
See the documentation for the variable `user-string-font'.")
(defvar c-keyword-font nil
  "*Default font for c keywords.
See the documentation for the variable `user-string-font'.")
(defvar c-function-font nil
  "*Default font for c functions.
See the documentation for the variable `user-string-font'.")
(defvar c-variable-font nil
  "*Default font for c variables.
See the documentation for the variable `user-string-font'.")
(defvar c-type-font nil
  "*Default font for c data types.
See the documentation for the variable `user-string-font'.")
(defvar c-reference-font nil
  "*Default font for c references.
See the documentation for the variable `user-string-font'.")
(defvar c-string-font nil
  "*Default font for c strings.
See the documentation for the variable `user-string-font'.")


;
; tex-*-font
;
(defvar tex-comment-font nil
  "*Default font for tex comments.
See the documentation for the variable `user-string-font'.")
(defvar tex-keyword-font nil
  "*Default font for tex keywords.
See the documentation for the variable `user-string-font'.")
(defvar tex-function-font nil
  "*Default font for tex functions.
See the documentation for the variable `user-string-font'.")
(defvar tex-variable-font nil
  "*Default font for tex variables.
See the documentation for the variable `user-string-font'.")
(defvar tex-type-font nil
  "*Default font for tex data types.
See the documentation for the variable `user-string-font'.")
(defvar tex-reference-font nil
  "*Default font for tex references.
See the documentation for the variable `user-string-font'.")
(defvar tex-string-font nil
  "*Default font for tex strings.
See the documentation for the variable `user-string-font'.")


;
; fortran-*-font
;
(defvar fortran-comment-font nil
  "*Default font for fortran comments.
See the documentation for the variable `user-string-font'.")
(defvar fortran-keyword-font nil
  "*Default font for fortran keywords.
See the documentation for the variable `user-string-font'.")
(defvar fortran-function-font nil
  "*Default font for fortran functions.
See the documentation for the variable `user-string-font'.")
(defvar fortran-variable-font nil
  "*Default font for fortran variables.
See the documentation for the variable `user-string-font'.")
(defvar fortran-type-font nil
  "*Default font for fortran data types.
See the documentation for the variable `user-string-font'.")
(defvar fortran-reference-font nil
  "*Default font for fortran references.
See the documentation for the variable `user-string-font'.")
(defvar fortran-string-font nil
  "*Default font for fortran strings.
See the documentation for the variable `user-string-font'.")


;
; idl-*-font
;
(defvar idl-comment-font nil
  "*Default font for idl comments.
See the documentation for the variable `user-string-font'.")
(defvar idl-keyword-font nil
  "*Default font for idl keywords.
See the documentation for the variable `user-string-font'.")
(defvar idl-function-font nil
  "*Default font for idl functions.
See the documentation for the variable `user-string-font'.")
(defvar idl-variable-font nil
  "*Default font for idl variables.
See the documentation for the variable `user-string-font'.")
(defvar idl-type-font nil
  "*Default font for idl data types.
See the documentation for the variable `user-string-font'.")
(defvar idl-reference-font nil
  "*Default font for idl reference routines.
See the documentation for the variable `user-string-font'.")
(defvar idl-string-font nil
  "*Default font for idl strings.
See the documentation for the variable `user-string-font'.")


;
; html-*-font
;
(defvar html-comment-font nil
  "*Default font for html comments.
See the documentation for the variable `user-string-font'.")
(defvar html-keyword-font nil
  "*Default font for html keywords.
See the documentation for the variable `user-string-font'.")
(defvar html-function-font nil
  "*Default font for html functions.
See the documentation for the variable `user-string-font'.")
(defvar html-variable-font nil
  "*Default font for html variables.
See the documentation for the variable `user-string-font'.")
(defvar html-type-font nil
  "*Default font for html data types.
See the documentation for the variable `user-string-font'.")
(defvar html-reference-font nil
  "*Default font for html references.
See the documentation for the variable `user-string-font'.")
(defvar html-string-font nil
  "*Default font for html strings.
See the documentation for the variable `user-string-font'.")











;-----------------------------------------------------------
; font-setting functions
;
;
(defun _font-lock-cut-attribute-from-list (fontattribute)
  "Used by the `_setfl-xxxxxx-font' functions.  Of no use to the user."
  (progn
    (setq nn 0)
    (while (and (< nn 10) 
			 (not 
			  (prog1
				 (setq foundone 
					  (member fontattribute 
							(nth nn font-lock-face-attributes)
							)
					  )
			    (setq nn (1+ nn))
			    )
			  )
			 )
	 )
    (if foundone
	   (setq font-lock-face-attributes
		    (delete foundone font-lock-face-attributes)
		    )
	 )
    )
  )


;
;
(defun _setfl-string-font (fontlist)
  "See `user-string-font' for input syntax."
  (if (listp fontlist)      ;only do something if this is a list
      (progn
	   (_font-lock-cut-attribute-from-list 'font-lock-string-face)
	   (setq font-lock-face-attributes
		    (append
			(list (append '(font-lock-string-face) fontlist))
			font-lock-face-attributes)
		    )
	   )
    )
  )


;
;
(defun _setfl-comment-font (fontlist)
  "See `user-string-font' for input syntax."
  (if (listp fontlist)      ;only do something if this is a list
      (progn
	(_font-lock-cut-attribute-from-list 'font-lock-comment-face)
	(setq font-lock-face-attributes
	      (append
	       (list (append '(font-lock-comment-face) fontlist))
	       font-lock-face-attributes)
	      )
	)
    )
  )


;
;
(defun _setfl-keyword-font (fontlist)
  "See `user-string-font' for input syntax."
  (if (listp fontlist)      ;only do something if this is a list
      (progn
	(_font-lock-cut-attribute-from-list 'font-lock-keyword-face)
	(setq font-lock-face-attributes
	      (append
	       (list (append '(font-lock-keyword-face) fontlist))
	       font-lock-face-attributes)
	      )
	)
    )
  )


;
;
(defun _setfl-function-font (fontlist)
  "See `user-string-font' for input syntax."
  (if (listp fontlist)      ;only do something if this is a list
      (progn
	(_font-lock-cut-attribute-from-list 'font-lock-function-name-face)
	(setq font-lock-face-attributes
	      (append
	       (list (append '(font-lock-function-name-face) fontlist))
	       font-lock-face-attributes)
	      )
	)
    )
  )


;
;
(defun _setfl-variable-font (fontlist)
  "See `user-string-font' for input syntax."
  (if (listp fontlist)      ;only do something if this is a list
      (progn
	(_font-lock-cut-attribute-from-list 'font-lock-variable-name-face)
	(setq font-lock-face-attributes
	      (append
	       (list (append '(font-lock-variable-name-face) fontlist))
	       font-lock-face-attributes)
	      )
	)
    )
  )


;
;
(defun _setfl-type-font (fontlist)
  "See `user-string-font' for input syntax."
  (if (listp fontlist)      ;only do something if this is a list
      (progn
	(_font-lock-cut-attribute-from-list 'font-lock-type-face)
	(setq font-lock-face-attributes
	      (append
	       (list (append '(font-lock-type-face) fontlist))
	       font-lock-face-attributes)
	      )
	)
    )
  )


;
;
(defun _setfl-reference-font (fontlist)
  "See `user-string-font' for input syntax."
  (if (listp fontlist)      ;only do something if this is a list
      (progn
	(_font-lock-cut-attribute-from-list 'font-lock-reference-face)
	(setq font-lock-face-attributes
	      (append
	       (list (append '(font-lock-reference-face) fontlist))
	       font-lock-face-attributes)
	      )
	)
    )
  )









;----------------------------------------------------------------------
;
; This function calls the appropriate `_setfl-xxxxxxx-font' function
; and sets it to the value of the variables for the requested mode
; 
(defun set-user-font-lock-fonts (typestr)
  "Sets the appropriate section of the variable
`font-lock-face-attributes' to the user-specified font for the mode
given in TYPESTR.

For more info, see the documentation for the variables:
`user-string-font' and `font-lock-face-attributes'
and the functions: `change-fonts-font-lock-mode'.



The possible values of TYPESTR are: \"lisp\" \"c\" \"fortran\" \"tex\"
\"idl\" and \"html\".  Each possible value has its own set of
variables for fonts and faces.

There are sets of variables of the form: `MODE-CATEGORY-font' where
MODE is the word: `lisp', `c', `fortran', `tex', `idl', `html', or
`user' and CATEGORY is the word: `comment', `keyword', `function',
`variable', `type', `reference', or `string'.  

If the value of `MODE-CATEGORY-font' is 'nil', then the variable
`user-CATEGORY-font' is used instead.  If that variable is also 'nil',
then the font isn't changed.

You set one of these variables to specify the font to use in the mode
MODE to fontify the words in the CATEGORY list.
`set-user-font-lock-fonts' then changes the appropriate font-lock mode
varibles, setting things up for the mode you specified in TYPESTR.  

If TYPESTR isn't one of the possible values listed above, the set of
variables `user-*-font' is used by default.  


If you set one of the `*-*-font' variables and want to make the change
take effect at once, use the function `change-fonts-font-lock-mode'.


This function, like its documentation, is a mess.  Don't bother using
it, unless it's for a mode-hook.  Use the function
`change-fonts-font-lock-mode' instead; its documentation even contains
examples!


{jpw 11/95}"

  (cond ( (string= "lisp" typestr)
		(progn
		  (if lisp-comment-font
			 (_setfl-comment-font lisp-comment-font)
		    ;else
		    (_setfl-comment-font user-comment-font)
		    )

		  (if lisp-keyword-font
			 (_setfl-keyword-font lisp-keyword-font)
		    ;else
		    (_setfl-keyword-font user-keyword-font)
		    )

		  (if lisp-function-font
			 (_setfl-function-font lisp-function-font)
		    ;else
		    (_setfl-function-font user-function-font)
		    )

		  (if lisp-variable-font
			 (_setfl-variable-font lisp-variable-font)
		    ;else
		    (_setfl-variable-font user-variable-font)
		    )

		  (if lisp-type-font
			 (_setfl-type-font lisp-type-font)
		    ;else
		    (_setfl-type-font user-type-font)
		    )

		  (if lisp-reference-font
			 (_setfl-reference-font lisp-reference-font)
		    ;else
		    (_setfl-reference-font user-reference-font)
		    )

		  (if lisp-string-font
			 (_setfl-string-font lisp-string-font)
		    ;else
		    (_setfl-string-font user-string-font)
		    )
		  )
		)
	   ;
	   ; c and c++
	   ; 
	   ( (string= "c" typestr)
		(progn
		  (if c-comment-font
			 (_setfl-comment-font c-comment-font)
		    ;else
		    (_setfl-comment-font user-comment-font)
		    )

		  (if c-keyword-font
			 (_setfl-keyword-font c-keyword-font)
		    ;else
		    (_setfl-keyword-font user-keyword-font)
		    )

		  (if c-function-font
			 (_setfl-function-font c-function-font)
		    ;else
		    (_setfl-function-font user-function-font)
		    )

		  (if c-variable-font
			 (_setfl-variable-font c-variable-font)
		    ;else
		    (_setfl-variable-font user-variable-font)
		    )

		  (if c-type-font
			 (_setfl-type-font c-type-font)
		    ;else
		    (_setfl-type-font user-type-font)
		    )

		  (if c-reference-font
			 (_setfl-reference-font c-reference-font)
		    ;else
		    (_setfl-reference-font user-reference-font)
		    )

		  (if c-string-font
			 (_setfl-string-font c-string-font)
		    ;else
		    (_setfl-string-font user-string-font)
		    )
		  )
		)
	   ;
	   ; fortran
	   ;
	   ( (string= "fortran" typestr)
		(progn
		  (if fortran-comment-font
			 (_setfl-comment-font fortran-comment-font)
		    ;else
		    (_setfl-comment-font user-comment-font)
		    )

		  (if fortran-keyword-font
			 (_setfl-keyword-font fortran-keyword-font)
		    ;else
		    (_setfl-keyword-font user-keyword-font)
		    )

		  (if fortran-function-font
			 (_setfl-function-font fortran-function-font)
		    ;else
		    (_setfl-function-font user-function-font)
		    )

		  (if fortran-variable-font
			 (_setfl-variable-font fortran-variable-font)
		    ;else
		    (_setfl-variable-font user-variable-font)
		    )

		  (if fortran-type-font
			 (_setfl-type-font fortran-type-font)
		    ;else
		    (_setfl-type-font user-type-font)
		    )

		  (if fortran-reference-font
			 (_setfl-reference-font fortran-reference-font)
		    ;else
		    (_setfl-reference-font user-reference-font)
		    )

		  (if fortran-string-font
			 (_setfl-string-font fortran-string-font)
		    ;else
		    (_setfl-string-font user-string-font)
		    )
		  )
		)
	   ;
	   ; tex and variants
	   ;
	   ( (string= "tex" typestr)
		(progn
		  (if tex-comment-font
			 (_setfl-comment-font tex-comment-font)
		    ;else
		    (_setfl-comment-font user-comment-font)
		    )

		  (if tex-keyword-font
			 (_setfl-keyword-font tex-keyword-font)
		    ;else
		    (_setfl-keyword-font user-keyword-font)
		    )

		  (if tex-function-font
			 (_setfl-function-font tex-function-font)
		    ;else
		    (_setfl-function-font user-function-font)
		    )

		  (if tex-variable-font
			 (_setfl-variable-font tex-variable-font)
		    ;else
		    (_setfl-variable-font user-variable-font)
		    )

		  (if tex-type-font
			 (_setfl-type-font tex-type-font)
		    ;else
		    (_setfl-type-font user-type-font)
		    )

		  (if tex-reference-font
			 (_setfl-reference-font tex-reference-font)
		    ;else
		    (_setfl-reference-font user-reference-font)
		    )

		  (if tex-string-font
			 (_setfl-string-font tex-string-font)
		    ;else
		    (_setfl-string-font user-string-font)
		    )
		  )
		)
	   ;
	   ; idl
	   ;
	   ( (string= "idl" typestr)
		(progn
		  (if idl-comment-font
			 (_setfl-comment-font idl-comment-font)
		    ;else
		    (_setfl-comment-font user-comment-font)
		    )

		  (if idl-keyword-font
			 (_setfl-keyword-font idl-keyword-font)
		    ;else
		    (_setfl-keyword-font user-keyword-font)
		    )

		  (if idl-function-font
			 (_setfl-function-font idl-function-font)
		    ;else
		    (_setfl-function-font user-function-font)
		    )

		  (if idl-variable-font
			 (_setfl-variable-font idl-variable-font)
		    ;else
		    (_setfl-variable-font user-variable-font)
		    )

		  (if idl-type-font
			 (_setfl-type-font idl-type-font)
		    ;else
		    (_setfl-type-font user-type-font)
		    )

		  (if idl-reference-font
			 (_setfl-reference-font idl-reference-font)
		    ;else
		    (_setfl-reference-font user-reference-font)
		    )

		  (if idl-string-font
			 (_setfl-string-font idl-string-font)
		    ;else
		    (_setfl-string-font user-string-font)
		    )
		  )
		)
	   ;
	   ; html
	   ;
	   ( (string= "html" typestr)
		(progn
		  (if html-comment-font
			 (_setfl-comment-font html-comment-font)
		    ;else
		    (_setfl-comment-font user-comment-font)
		    )

		  (if html-keyword-font
			 (_setfl-keyword-font html-keyword-font)
		    ;else
		    (_setfl-keyword-font user-keyword-font)
		    )

		  (if html-function-font
			 (_setfl-function-font html-function-font)
		    ;else
		    (_setfl-function-font user-function-font)
		    )

		  (if html-variable-font
			 (_setfl-variable-font html-variable-font)
		    ;else
		    (_setfl-variable-font user-variable-font)
		    )

		  (if html-type-font
			 (_setfl-type-font html-type-font)
		    ;else
		    (_setfl-type-font user-type-font)
		    )

		  (if html-reference-font
			 (_setfl-reference-font html-reference-font)
		    ;else
		    (_setfl-reference-font user-reference-font)
		    )

		  (if html-string-font
			 (_setfl-string-font html-string-font)
		    ;else
		    (_setfl-string-font user-string-font)
		    )
		  )
		)
	   ;
	   ; default
	   ;
	   ( t
		(progn
		  (_setfl-comment-font user-comment-font)
		  (_setfl-keyword-font user-keyword-font)
		  (_setfl-function-font user-function-font)
		  (_setfl-variable-font user-variable-font)
		  (_setfl-type-font user-type-font)
		  (_setfl-reference-font user-reference-font)
		  (_setfl-string-font user-string-font)
		  )
		)
	   ;; End of Function `set-user-font-lock-fonts'
  )
)



;
;
(defun change-fonts-font-lock-mode (&optional typestr)
  "*Changes the fonts for the mode specified in TYPESTR.  

This function turns on font-lock mode (if it's not on already), and
makes any changes to the `*-*-font' variables take effect.

To learn how to set the `*-*-font' variables, see the documentation
for the variable `user-string-font'.

Here's a table of possible values of TYPESTR, and the set of variables
they correspond to:


TYPESTR:                Variable set:
------------------------------------------
\"lisp\"                  lisp-*-font
\"c\"                     c-*-font
\"fortran\"               fortran-*-font
\"tex\"                   tex-*-font
\"idl\"                   idl-*-font
\"html\"                  html-*-font

anything else:          user-*-font
------------------------------------------


{jpw 11/95}"

  (interactive)
  (progn
    (if typestr
	   (set-user-font-lock-fonts typestr)
	 ;else
	 (set-user-font-lock-fonts '_major-mode-font-variable-name)
	 )
    (font-lock-make-faces t)
    )
  )

(global-set-key [\C-kp-5] 'change-fonts-font-lock-mode)
(global-set-key [?\A-l] 'change-fonts-font-lock-mode)



(provide 'font-lock-addons)
;;;; END OF FILE ;;;;
