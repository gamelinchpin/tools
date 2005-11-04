;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; M$ WinBlows-specific setup.
;;
;;  Copyright © 1995-2005 John P. Weiss
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
;; Includes settings for EmacsNT, Cygwin, and VSS-integration amongst other
;; things).
;;
;;
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile
  (require 'source-safe))
(message "loading WinBlows-specific crap.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customizable Config Variables.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Customization-Menu Variables.  These are set in a block, and will
;; show up in the user's .emacs file when they next save their
;; customizations. 
;;
(custom-set-variables
;;; '(printer-name "//SOME_WIN_DOMAIN/SOME_PRINTER" t)
 '(ps-printer-name "~/emacs-out.ps"))

;; Use Unix-style line endings.
;(setq-default buffer-file-coding-system 'undecided-unix)

;; CygWin
;;
(defvar cygwin-basepath "D:/cygwin")
;;(defvar cygwin-basepath "D:/local/cygwin") ;Keep changin' my mind...
(defvar cygwin-tmppath (or (getenv "TEMP") 
                           (getenv "TMP")
                           ;; Customize below, or set one of the envvars above.
                           "D:/TEMP"))
(defvar cygwin-home (or (getenv "HOME") 
                        ;; Customize below, or set the envvar above.
                        "D:/TEMP"))

;; Visual Source Safe
;;

;; Installation Path for VSS
;; Leave commented out if you aren't using VSS
;; 
;;;(defvar vss-install-path "C:\\Program Files\\Microsoft Visual Studio")

;; Variables defining the path to various VSS DB's.
;;;(defvar vss-db "p:\\RepositoryPath\\VSS")

;; VSS Login Info
;; 
;;;(setq ss-username "JWeiss")
;;;(setq ss-password 'nil)
;;;(setq ss-tmp-dir cygwin-tmppath)

;;(setq ss-trace 'nil)

;; The list of VSS projects.
;;;(setq jpw-vss-projects 
;;;      (list
;;;       '("^C:/home/jweiss/SomeProjectDir.working"
;;;         "$/TheVSSRepository/" vss-db)
;;;       '((concat "^" cygwin-home "SomeProjectDir.working")
;;;         "$/TheVSSRepository/" vss-db)
;;;       ))



;;**********************************************************************
;;**********************************************************************
;;
;;                  No User Servicable Parts Below
;;
;;**********************************************************************
;;**********************************************************************




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Cygwin Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar cygwin-binpath (concat cygwin-basepath "/bin"))
(setenv "INFOPATH"
        (concat cygwin-basepath "/usr/info" 
                ";" cygwin-basepath "/usr/share/info")
        )


;; Ediff setup
;;
(setq ediff-diff3-program (concat cygwin-binpath "/diff3")
      ediff-diff-program (concat cygwin-binpath "/diff")
      diff-command (concat cygwin-binpath "/diff") )


;; Frame Geometry & Font
;;
(setq initial-frame-alist
      '((top . 0) (left . 0) 
        (width . 80) (height . 46)
        (user-position t))
      )
(setq res (downcase (or (getenv "SCREENRES") "none")))
(cond 
 ((equal res "800x600")
  (setcdr (assoc 'height initial-frame-alist) '37)
  (set-default-font 
   "-*-Lucida Console-normal-r-*-*-12-*-*-*-c-*-*")
  )
 ((equal res "1024x768")
  (setcdr (assoc 'height initial-frame-alist) '45)
  (set-default-font 
;;   "-*-Lucida Sans Typewriter-normal-r-*-*-13-*-*-*-*-*-*")
   "-*-Lucida Console-normal-r-*-*-15-*-*-*-c-*-*")
  )
 ((equal res "1152x864")
  ;; For NT, the height is '53.  For XP, it's down to '51
  ;;;(setcdr (assoc 'height initial-frame-alist) '51)
  ;;;(set-default-font 
  ;;; "-*-Lucida Console-normal-r-*-*-15-*-*-*-c-*-*")
  ;; For larger fonts, one could do this:
  (setcdr (assoc 'height initial-frame-alist) '43)
  (set-default-font 
   "-*-Lucida Console-normal-r-*-*-17-*-*-*-c-*-*")
  )
  ;;else
 (t
  (setcdr (assoc 'height initial-frame-alist) '24)
  (set-default-font 
   "-*-Lucida Console-normal-r-*-*-13-*-*-*-c-*-*")
  )
 );;end cond


;(set-fontset-font name charset-symbol fontname)
;;; To query font info, use these interactive commands:
;; 
;;(describe-font) ;; gives details (including fullname) of current font
;;(describe-fontset) ;; gives details (including fullname) of current fontset
;;(list-fontsets)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode Setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CygWin
;;
(require 'custom-defuns)  ;; defines: jpw-custom-set-variables-nonsaved
(jpw-custom-set-variables-nonsaved
 (list 'cygwin-mount-cygwin-bin-directory cygwin-binpath)
 '(cygwin-mount-build-mount-table-asynch t)
 )
;;; NOTE:
;;; I have a "cygwin32-mount" file on Linux, but a "cygwin-mount" file on my
;;; winboxen.  To resolve the inconsistency (it looks like the /.*32-.*/
;;; version is the older one), I'll do some compile-time fancy-footwork.
(eval-when-compile
  (or (require 'cygwin-mount nil t)
      (and (require 'cygwin32-mount nil t)
           (defalias 'cygwin-mount-activate 'cygwin32-mount-activate))))
(cygwin-mount-activate)

;; I've forgotten what this does.
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)


;; [jpw; 8/04]  Taken from Markus Hoenika's setup-cygwin.el
;;
;;; Follow Cygwin symlinks.
;;; Handles old-style (text file) symlinks and new-style (.lnk file) symlinks.
;;; (Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
;;; loaded as such.)
(defun follow-cygwin-symlink ()
  (save-excursion
    (goto-char 0)
    (if (looking-at
         "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
        (progn
          (if (re-search-forward
               "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_:\\.\\\\\\$%@(){}~!#^'` ]+\\)" 
               nil t)
              (find-alternate-file (match-string 1))
            (progn (message "Cannot decode file into Cygwin symlink.")
                   (ding)
                   nil)))
      (if (looking-at "!<symlink>")
          (progn
            (re-search-forward "!<symlink>\\(.*\\)\0")
            (find-alternate-file (match-string 1))))
      )))
(add-hook 'find-file-hooks 'follow-cygwin-symlink)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Regular Symlinks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; [jpw; 11/04]  Modified from the code for following Cygwin symlinks.
;;
(defun follow-win-symlink ()
  (save-excursion
    (goto-char 0)
    (if (looking-at
         "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\223")
        (progn
          (re-search-forward
           "\x000\\([A-Z]:\\)\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'` ]+\\)")
          (find-alternate-file (concat (match-string 1) (match-string 2))))
      )))
(add-hook 'find-file-hooks 'follow-win-symlink)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  VB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist 
       '("\\.\\(frm\\|bas\\|cls\\)$" . visual-basic-mode)
       t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  EmacsNT Keyboard Quirks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-key function-key-map [apps] [menu])
(define-key function-key-map [\S-apps] [\S-menu])
(define-key function-key-map [\C-apps] [\C-menu])
(define-key function-key-map [\M-apps] [\M-menu])
(define-key function-key-map [?\e apps] [\M-menu])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Font Setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (and (boundp 'w32-fixed-font-alist)
         (listp w32-fixed-font-alist))
    (add-to-list 
     'w32-fixed-font-alist
     '(
       ("Monotype.com"
        ("8" "-*-Monotype.com-normal-r-*-*-11-*-*-*-c-iso8859-1")
        ("9" "-*-Monotype.com-normal-r-*-*-12-*-*-*-c-iso8859-1")
        ("10" "-*-Monotype.com-normal-r-*-*-13-*-*-*-c-iso8859-1")
        ("11" "-*-Monotype.com-normal-r-*-*-15-*-*-*-c-iso8859-1"))
       ("Tahoma"
        ("8" "-*-Tahoma-normal-r-*-*-11-*-96-96-p-*-viscii1.1-*")
        ("9" "-*-Tahoma-normal-r-*-*-12-*-96-96-p-*-viscii1.1-*")
        ("10" "-*-Tahoma-normal-r-*-*-13-*-96-96-p-*-viscii1.1-*")
        ("11" "-*-Tahoma-normal-r-*-*-15-*-96-96-p-*-viscii1.1-*")
        ("8 bold" "-*-Tahoma-bold-r-*-*-11-*-96-96-p-*-viscii1.1-*")
        ("9 bold" "-*-Tahoma-bold-r-*-*-12-*-96-96-p-*-viscii1.1-*")
        ("10 bold" "-*-Tahoma-bold-r-*-*-13-*-96-96-p-*-viscii1.1-*")
        ("11 bold" "-*-Tahoma-bold-r-*-*-15-*-96-96-p-*-viscii1.1-*"))
       ("Lucida Sans Unicode"
        ("8" "-*-Lucida Sans Unicode-normal-r-*-*-11-*-96-96-p-*-*-*")
        ("9" "-*-Lucida Sans Unicode-normal-r-*-*-12-*-96-96-p-*-*-*")
        ("10" "-*-Lucida Sans Unicode-normal-r-*-*-13-*-96-96-p-*-*-*")
        ("11" "-*-Lucida Sans Unicode-normal-r-*-*-15-*-96-96-p-*-*-*"))
       ("Lucida Console"
        ("8" "-*-Lucida Console-normal-r-*-*-11-*-96-96-p-*-*-*")
        ("9" "-*-Lucida Console-normal-r-*-*-12-*-96-96-p-*-*-*")
        ("10" "-*-Lucida Console-normal-r-*-*-13-*-96-96-p-*-*-*")
        ("11" "-*-Lucida Console-normal-r-*-*-15-*-96-96-p-*-*-*")
        ("12" "-*-Lucida Console-normal-r-*-*-16-*-96-96-p-*-*-*"))
       )
     )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Source Safe support 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Only do this setup if VSS is on this machine (and the user sets the var
;; describing where it's installed.
(if (boundp 'vss-install-path)
    (progn
      ;; Path to the VSS commandline binary.
      (setq ss-program 
            (concat vss-install-path "\\Common\\VSS\\win32\\SS.EXE"))

      (require 'source-safe)

      ;; Tool function for adding projects, their working dirs, and their
      ;; VSS-DB's
      (if (not (boundp 'ss-project-dirs)) (setq ss-project-dirs '()))
      (if (not (boundp 'ss-database-alist)) (setq ss-database-alist '()))

      (defun jpw-ss-add-project (proj-wrk-dir proj-name proj-db-path)
        "Inform Source-Safe mode that the project `proj-name', in the VSS
database `proj-db-path', lives in the local directory `proj-wrk-dir'.

When called interactively, do *not* add extra '\' characters.  They
will be added automatically by the minibuffer when the strings are read.

Prepends the entries to the appropriate alists. 

{jpw 10/01}"
        (interactive
         "DProject Working Directory: \nsVSS Project: \nsVSS DB Path: ")
        (setq ss-project-dirs (cons 
                               (cons proj-wrk-dir proj-name) 
                               ss-project-dirs))
        (setq ss-database-alist (cons 
                                 (cons proj-wrk-dir proj-db-path) 
                                 ss-database-alist))
        )


      ;; Add any predefined projects here, now that we've defined the custom
      ;; function to do it.
      (if (and (boundp 'jpw-vss-projects)
               (listp jpw-vss-projects))
          (mapcar '(lambda (proj_l) (apply 'jpw-ss-add-project proj_l))
                  jpw-vss-projects)
        ) ;; end if ... progn


      ;; Not needed as long as we force the load of VSS
      (autoload 'ss-diff "source-safe"
        "Compare the current buffer to the version of the file under SourceSafe.
   If NON-INTERACTIVE, put the results in a buffer and switch to that buffer;
   otherwise run ediff to view the differences." t)
      (autoload 'ss-get "source-safe"
        "Get the latest version of the file currently being visited." t)
      (autoload 'ss-checkout "source-safe"
        "Check out the currently visited file so you can edit it." t)
      (autoload 'ss-uncheckout "source-safe" 
        "Un-checkout the curently visited file." t)
      (autoload 'ss-update "source-safe"
        "Check in the currently visited file." t)
      (autoload 'ss-branch "source-safe"
        "Branch off a private, writable copy of the current file for you to work on."
        t)
      (autoload 'ss-unbranch "source-safe"
        "Delete a private branch of the current file.  This is not undoable."
        t)
      (autoload 'ss-merge "source-safe"
        "Check out the current file and merge in the changes that you have made." 
        t)
      (autoload 'ss-history "source-safe"
        "Show the checkin history of the currently visited file." t)
      (autoload 'ss-submit-bug-report "source-safe"
        "Submit a bug report, with pertinent information." t)
      (autoload 'ss-help "source-safe" "Describe the SourceSafe mode." t)
      );; end progn ;; VSS Setup
  );; end if


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Tell us everything worked hunky-dory
;;
(message "Done with WinBlows-specific crap.")



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;
