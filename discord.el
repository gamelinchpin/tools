;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Hail Eris!
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
;;  Package for printing out the a date in the discordian calendar.
;;
;;  `discordian-date' does the conversion.
;;  `discordian-date-today' converts today's date.
;;
;;  
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile
  (require 'cl))
(require 'calendar)

;;;###autoload
(defun hail-eris ()
  "Print today's date in the discordian calendar."
  (interactive)
  (discordian-date-today)
)

;;;###autoload
(defun discordian-date-today ()
  "Print today's date in the discordian calendar."
  (interactive)
  (let* ((today (nthcdr 3 (decode-time (current-time))))
         (d (car today))
         )
    (setcar today (cadr today))
    (setcar (cdr today) d)
    (discordian-date today)
    )
  )

;;;###autoload
(defun discordian-date (date)
  "Print DATE from the discordian calendar."
  (interactive (list (calendar-read-date)))
  (let* ((erisstr (discordian-date-string date))
         (erisl (string-width erisstr))
         )
    (if (< erisl (window-width))
        (message erisstr)
      ;; else
      (progn
        (if (string-match "[0-9a-zA-Z]\\. *" erisstr)
            (aset erisstr (1- (match-end 0)) 10) ;; 10=='\n'
          )
        (with-output-to-temp-buffer "Hail-Eris!" (princ erisstr))
        (shrink-window-if-larger-than-buffer 
         (get-buffer-window "Hail-Eris!"))
        )
      )
    )
  )

(defun* discordian-date-string (&optional (date (calendar-current-date)))
  "Convert DATE to discordian format."
  (let* ((days      ["Sweetmorn" "Boomtime" "Pungenday" "Prickle-Prickle" "Setting Orange"])
         (months    ["Chaos" "Discord" "Confusion" "Bureaucracy" "Aftermath"])
         (holidays  
          (list (cons 5 ["Mungday" "Mojoday" "Syaday" "Zaraday" "Maladay"])
                (cons 50 ["Chaoflux" "Discoflux" "Confuflux" "Bureflux"
                          "Afflux"])
                ))
         (endings   ["th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th"])
         (day-count [0 31 59 90 120 151 181 212 243 273 304 334])
         (year      (+ (extract-calendar-year date) 1166))
         (month     (1- (extract-calendar-month date)))
         (day       (1- (extract-calendar-day date)))
         (gregory   (+ (aref day-count month) day))
         (erisd     (1+ (mod gregory 73)))
         (erismo    (floor (/ gregory 73)))
         (tibs      (and (= month 1) (= day 28)))
         ;; do we need to subtract a day from years with St. Tibs Day?
         (haileris  (assoc (if (and (> erisd 59)
                                    (= (mod year 4) 2)
                                    (or (/= (mod year 100) 66)
                                        (= (mod year 400) 366))
                                    )
                               (1- erisd)
                             erisd) holidays))
         );; end local declarations
    (format "Today is %s in the YOLD %d%s"
            (if tibs
                "St. Tib's Day"
              ;; else
              (format "%s, the %d%s day of %s"
                      (aref days (mod gregory 5))
                      erisd
                      (aref endings (mod erisd 10))
                      (aref months erismo)
                      )
              )
            year
            (if haileris
                (format ".  Celebrate %s!" (aref (cdr haileris) erismo))
              ;;else
              "."
              )
            );; end format
    ) ;;end-let
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End
;;