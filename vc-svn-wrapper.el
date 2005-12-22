;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Wrapper around vc-svn to make it work with emacs v21.*
;;
;;  Copyright © 2005 John P. Weiss
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
;; Various missing things needed by vc-svn.el, but not present in emacs
;; v21.*.  They've been copied whole from the "vc.el" in the emacs CVS.
;;
;; Rather than try to load "vc-svn.el" directly, do the following, instead:
;; 
;;   (load "vc-svn-wrapper")
;;
;; As long as "vc-svn" is somewhere in your `load-path', this file will pull
;; it in.
;;
;;  
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; We'll require vc-svn at the end of this file, so that you don't have to.

;;
;; `vc-mode' variables missing from emacs < v22
;;


(defcustom vc-stay-local t
  "*Non-nil means use local operations when possible for remote repositories.
This avoids slow queries over the network and instead uses heuristics
and past information to determine the current status of a file.

The value can also be a regular expression or list of regular
expressions to match against the host name of a repository; then VC
only stays local for hosts that match it.  Alternatively, the value
can be a list of regular expressions where the first element is the
symbol `except'; then VC always stays local except for hosts matched
by these regular expressions."
  :type '(choice (const :tag "Always stay local" t)
	  (const :tag "Don't stay local" nil)
	  (list :format "\nExamine hostname and %v" :tag "Examine hostname ..."
		(set :format "%v" :inline t 
                     (const :format "%t" :tag "don't" except))
		(regexp :format " stay local,\n%t: %v" :tag "if it matches")
		(repeat :format "%v%i\n" :inline t (regexp :tag "or"))))
  :version "22.1"
  :group 'vc)


(defvar vc-disable-async-diff nil
  "VC sets this to t locally to disable some async diff operations.
Backends that offer asynchronous diffs should respect this variable
in their implementation of vc-BACKEND-diff.")


(defvar vc-annotate-parent-rev nil)


;;
;; `vc-mode' functions missing from emacs < v22
;;


(defun vc-stay-local-p (file)
  "Return non-nil if VC should stay local when handling FILE.
This uses the `repository-hostname' backend operation."
  (let* ((backend (vc-backend file))
	 (sym (vc-make-backend-sym backend 'stay-local))
	 (stay-local (if (boundp sym) (symbol-value sym) t)))
    (if (eq stay-local t) (setq stay-local vc-stay-local))
    (if (symbolp stay-local) stay-local
      (let ((dirname (if (file-directory-p file)
			 (directory-file-name file)
		       (file-name-directory file))))
	(eq 'yes
	    (or (vc-file-getprop dirname 'vc-stay-local-p)
		(vc-file-setprop
		 dirname 'vc-stay-local-p
		 (let ((hostname (vc-call-backend
				  backend 'repository-hostname dirname)))
		   (if (not hostname)
		       'no
		     (let ((default t))
		       (if (eq (car-safe stay-local) 'except)
			   (setq default nil stay-local (cdr stay-local)))
		       (when (consp stay-local)
			 (setq stay-local
			       (mapconcat 'identity stay-local "\\|")))
		       (if (if (string-match stay-local hostname)
			       default (not default))
			   'yes 'no)))))))))))


(defun vc-switches (backend op)
  (let ((switches
	 (or (if backend
		 (let ((sym (vc-make-backend-sym
			     backend (intern (concat (symbol-name op)
						     "-switches")))))
		   (if (boundp sym) (symbol-value sym))))
	     (let ((sym (intern (format "vc-%s-switches" (symbol-name op)))))
	       (if (boundp sym) (symbol-value sym)))
	     (cond
	      ((eq op 'diff) diff-switches)))))
    (if (stringp switches) (list switches)
      ;; If not a list, return nil.
      ;; This is so we can set vc-diff-switches to t to override
      ;; any switches in diff-switches.
      (if (listp switches) switches))))


;; Old def for compatibility with Emacs-21.[123].
(defmacro vc-diff-switches-list (backend) `(vc-switches ',backend 'diff))
(make-obsolete 'vc-diff-switches-list 'vc-switches "22.1")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Now load vc-svn
;; We'll also make sure that SVN is added to the vc-engine.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "vc"
  (quote (progn (add-to-list 'vc-handled-backends 'SVN)
                (setq vc-dired-listing-switches "-Al")
                (add-to-list 'vc-directory-exclusion-list ".svn")
                )))

(require 'vc)
(provide 'vc-svn-wrapper)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;
