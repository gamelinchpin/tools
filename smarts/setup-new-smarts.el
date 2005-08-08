;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup for the emacs ASL and MODEL programming modes.
;;
;;  Copyright © 1999-2000 John P. Weiss
;;  Donated to System Management Arts under the Artistic License
;;
;;  Since many MODEL and ASL files contain file-local variables that
;;  try to set the mode to "C++", we have a problem.  The code in this
;;  file will transparently fix this.
;;
;;  You must run the function `configure-new-smarts-modes' *after* the
;;  SMARTS system-wide `site-start.el' file loads, but *before* you
;;  set any of your own C++ mode-hooks.
;;  
;;  last modified 5/26/2000 (jpw)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar buffer-file-suffix nil
  "Cache of the filename suffix extracted by the last call to
`get-buffer-file-suffix'.  Invoke `get-buffer-file-suffix' to ensure
that this variable is updated.")


;; Get the file's suffix
;;
(defun get-buffer-file-suffix ()
  "Extract the dot-suffix from the name of the file in the current buffer."
  (interactive)
  (let ((ftype-pos (string-match "\\.[^.]*$" 
                                 buffer-file-name))
        );; end bindings
    ;; Increment the position in the string by 1 to remove
    ;; the dot.
    (setq buffer-file-suffix
          (and ftype-pos 
               (substring buffer-file-name (1+ ftype-pos))))
    ); end let
  )


;; Changes to the correct mode, and aborts further processing of C++-mode.
;;
(defun c++-correct-mode ()
  ;; Get the file's dot-suffix to determine its actual type.      
  (get-buffer-file-suffix)
  ;; Are we in a call from C-++ mode?
  (if (equal major-mode 'c++-mode)
      ;; Yes.  Use the file-suffix to determine the correct mode.
      (let* ((mode-suf-alist '(("asl" . asl-mode)
                               ("mdl" . model-mode)))
             (new-mode 
              (cdr (assoc buffer-file-suffix mode-suf-alist)))
             );;end bindings
        (if new-mode
            ;; Change the mode to the desired one.
            (funcall new-mode)
          );;end if
        );;end let
    );end if: equal c++-mode
  )


;; Fix the indentation.  This must be added as a C++-mode-hook, so
;; that it evaluates after the call to `model-mode' or `asl-mode'
;; completes and returns to `c++-mode'.
;;
(defun c++-fix-indentation ()
  ;; Get the file's dot-suffix to determine its actual type.      
  (get-buffer-file-suffix)
  (let* ((mode-isty-alist '(("asl" . "SMARTS-ASL")
                            ("mdl" . "SMARTS-MODEL")))
         (new-isty (cdr (assoc buffer-file-suffix mode-isty-alist)))
         );;end bindings
    (if new-isty
        (c-set-style new-isty))
    )
  )


(defun configure-new-smarts-modes ()
  "Run this function to configure the ASL and MODEL programming
modes.  This will setup various hooks and \"override\" the file-local
variable that forces C++-mode."
  (interactive)
  ;; Library Autoload
  (autoload 'asl-mode "asl" "SMARTS ASL Programming Mode." t)
  (autoload 'model-mode "model" "SMARTS MODEL Programming Mode." t)
  ;; Filename Mode Hook
  (setq auto-mode-alist (append '(("\\.mdl" . model-mode)
                                  ("\\.asl" . asl-mode))
                                auto-mode-alist))
  ;; Add the file-local modevar overrides.
  (add-hook 'c-mode-common-hook 'c++-correct-mode)
  (add-hook 'c++-mode-hook 'c++-fix-indentation t) ;append this hook
  )  


(defun new-smarts-modes-postload-fix ()
  "It seems that `configure-new-smarts-modes' only partially works.
If you need to edit the ASL or MODEL file, auto-fill mode and
indentation will not work quite right (probably due to some leftover
C++-mode settings).  The only way to fix it is to forcibly reactivate
the correct mode.  This function will do that for you."
  (interactive)
  (let* ((mode-suf-alist '(("asl" . asl-mode)
                           ("mdl" . model-mode)))
         (new-mode 
          (cdr (assoc buffer-file-suffix mode-suf-alist)))
         );;end bindings
    (if new-mode
        ;; Change the mode to the desired one.
        (funcall new-mode)
      );;end if
    );;end let
  )


(global-set-key "\C-c\C-m" 'new-smarts-modes-postload-fix)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;