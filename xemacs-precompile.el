;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prepare to compile on XEmacs
;;
;;  Copyright © 2006 John P. Weiss
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
;; Prepares XEmacs to byte-compile some of my custom files.  XEmacs,
;; unfortunately, does funky things with `eval-when-compile', making this
;; script necessary.
;;
;; You do not need to byte-compile this file, nor should you load it except
;; manually.
;;
;; NEVER `require' this file from any other package!!!!  The results could be
;; disasterous.
;;
;;  
;; RCS $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'jpw-indent-tools)
(require 'ee-mode)
(require 'tcl-enhancements)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;