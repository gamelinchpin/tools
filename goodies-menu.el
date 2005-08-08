;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      Goodies Menu
;
;         Adds some additional menu items as well as 
;         defining a new menu called "Goodies".
;
;
;      jpw 10/98
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;
; Add some things to the 'Files' menu
;
(define-key-after menu-bar-files-menu [delete-window]
  '("Delete Window" . delete-window) 'one-window)
(define-key-after menu-bar-files-menu [equal-window]
  '("Balance Window Heights" . balance-windows) 'split-window)


; Now we start to construct our Goodies Menu
;
(defvar menu-bar-goodies-menu 
  (make-sparse-keymap "Various Goodies")
  "Various Goodies! [jpw 11/95]")

(modify-frame-parameters (selected-frame)
					'((menu-bar-lines . 1))
					)

; This little set of commands inserts the "Goodies"
; menu after "Tools" in the menu bar.
;
; What it does:
;
; - Searches the 'global-map for the 'menu-bar submap.
; - When it finds it, binds that submap to a variable.
; - After the loop, adds the binding for the goodies menu.
;
(setq nn 0)
(while (and (< nn 1000)
		  (not
		   (prog1
			  (progn
			    (setq tmplist (nth nn global-map))
			    (if (listp tmplist)
				   (if (equal (car tmplist) 'menu-bar)
					  (setq menu-bar-map 
						   (nth nn global-map))
					'nil ;else
					)
				 'nil ;else
				 )
			    )
			(setq nn (1+ nn))
			)
		   )
		  )
  )
(makunbound 'nn)
(makunbound 'tmplist)

(setq menu-bar-map (member 'keymap menu-bar-map))
(define-key-after menu-bar-map [goodies]
  (cons "Goodies" menu-bar-goodies-menu)
  'tools)

;-------------------------------------------------------------------
; NOW we can start defining menu items in the normal way!!!!!!
;
; Define in *reverse* order of appearance.
;
;

(define-key global-map [menu-bar goodies expand-word]
  '("Auto-Complete Word/Expression" . dabbrev-expand)
  )

;(define-key global-map [menu-bar goodies cvs-log]
;  '("CVS Mode" . cvs-mode)
;  )

(define-key global-map [menu-bar goodies separator-abbrevsub]
  '("--" . (nil)
    )
  )

;
; Abbrev-mode stuff
;

; Define a submenu
;
(defvar menu-bar-abbrevs-menu 
  (make-sparse-keymap "Abbreviations")
  "Abbreviation Mode Submenu [jpw 10/98]")

(define-key global-map [menu-bar goodies abbrevs]
  (cons "Abbreviations" menu-bar-abbrevs-menu)
  )

(define-key menu-bar-abbrevs-menu [ab-write]
  '("Save All Abbreviations" . (lambda ()
						   (interactive)
						   (write-abbrev-file abbrev-file-name))
    )
  )

(define-key menu-bar-abbrevs-menu [ab-separator-save]
  '("--" . (nil)
    )
  )

(define-key global-map [menu-bar goodies abbrevs ab-edit]
  '("Edit All Abbreviations" . edit-abbrevs)
  )

(define-key global-map [menu-bar goodies abbrevs ab-list]
  '("List All Abbreviations" . list-abbrevs)
  )

(define-key global-map [menu-bar goodies abbrevs ab-expand]
  '("Expand Abbreviation Behind Cursor" . expand-abbrev)
  )

(define-key global-map [menu-bar goodies abbrevs ab-define-local]
  '("Create a Current-Mode Abbreviation" . define-mode-abbrev)
  )

(define-key global-map [menu-bar goodies abbrevs ab-define-global]
  '("Create a Global Abbreviation" . define-global-abbrev)
  )

(define-key global-map [menu-bar goodies abbrevs ab-separator-onoff]
  '("--" . (nil)
    )
  )

(define-key global-map [menu-bar goodies abbrevs ab-off]
  '("Turn Off Abbreviation-Mode" . (lambda ()
							  (interactive)
							  (abbrev-mode -1))
    )
  )

(define-key global-map [menu-bar goodies abbrevs ab-on]
  '("Turn On Abbreviation-Mode" . (lambda ()
							 (interactive)
							 (abbrev-mode 1))
    )
  )

;
; Font-lock toggle second from top
;

(define-key global-map [menu-bar goodies separator-font-lock]
  '("--" . (nil)
    )
  )

(if is-version-twenty
    () ; do nothing
    (define-key global-map [menu-bar goodies font-lock-reset]
	 '("Make Font Changes Active" . change-fonts-font-lock-mode)
	 )
  )

(define-key global-map [menu-bar goodies font-lock-off]
  '("Turn Off Fancy Fonts" . (lambda ()
						 (interactive)
						 (font-lock-mode -1))
    )
  )

(define-key global-map [menu-bar goodies font-lock-on]
  '("Turn On Fancy Fonts" . (lambda ()
						(interactive)
						(font-lock-mode 1))
    )
  )

;
; Auto-fill toggle at top
;

(define-key global-map [menu-bar goodies separator-auto-fill]
  '("--" . (nil)
    )
  )

(define-key global-map [menu-bar goodies auto-fill-off]
  '("Turn Off Line-Wrapping" . (lambda ()
						   (interactive)
						   (auto-fill-mode -1))
    )
  )

(define-key global-map [menu-bar goodies auto-fill-on]
  '("Turn On Line-Wrapping" . (lambda ()
						  (interactive)
						  (auto-fill-mode 1))
    )
  )
