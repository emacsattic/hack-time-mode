;;; hack-time.el --- Hack the time in Emacs   -*- lexical-binding: t -*-


;; This file is supposed to be generated.  The real source is an Org
;; file.  Keep this in mind when editing.

;; [[id:bdf129d9-29f3-477c-9fab-a7879bdb7e5a][inner-program]]
;; [[id:e83c08f0-f37a-44c3-b9e9-bf6bb7a58402][prologue]]


;; Copyright 2017 Marco Wahl
;;
;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: 2017
;; Version: see version control
;; Keywords: time, convenience
;; URL: https://marcowahl.github.io/ https://github.com/marcowahl/little-helpers
;;
;; This file is not part of Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; prologue ends here
;; [[id:e62ab536-0322-4583-9994-0150a330445c][freeze-current-time-core]]


(let (hack-time-day)

  (defun hack-time--freeze-advicer (x)
    "Can be advicer for ‘current-time’."
    (append (date-to-time (concat hack-time-day " 11:55")) (list 0 0)))

  (defun hack-time--current-time-back-to-normal ()
    "Remove all time hacks."
    (if (advice-member-p #'hack-time--freeze-advicer #'current-time)
        (advice-remove #'current-time #'hack-time--freeze-advicer)))

  (defun hack-time--current-time-do-freeze (yyyy-mm-dd-??:??-string)
    "Change ‘current-time’ to return the chosen date until reset.

Advice ‘current-time’ to return time YYYY-MM-DD-??:??-STRING.

If no hours and minutes given then use 11:55.

Note: This change does not affect every functionality that
depends on time in Emacs.  E.g. ‘format-time-string’ is not
affected."
    (hack-time--current-time-back-to-normal)
    (setf hack-time-day (concat yyyy-mm-dd-??:??-string " 11:55"))
    (advice-add #'current-time :filter-return #'hack-time--freeze-advicer)))
;; freeze-current-time-core ends here
;; [[id:5febcc2d-8798-4b1b-98ae-eb0f478db53d][commands]]


;; commands
(defun hack-time-current-time-back-to-normal ()
  "Remove all time hacks."
  (interactive)
  (hack-time--current-time-back-to-normal))

(defun hack-time-current-time-to-calendar-date (yyyy-mm-dd-?\?:\?\?-string)
  "Hack time to the date of the calendar.

Hour and minute are set to 11:55am.

Note: This change does not affect every functionality that
depends on time in Emacs.  E.g. ‘format-time-string’ is not
affected."
  (interactive (let ((from-calendar (org-get-date-from-calendar)))
                 (list (format "%02d-%02d-%02d"
                               (caddr from-calendar)
                               (car from-calendar)
                               (cadr from-calendar)))))
  (hack-time--current-time-do-freeze yyyy-mm-dd-?\?:\?\?-string))
;; commands ends here
;; inner-program ends here


(provide 'hack-time)


;;; hack-time.el ends here
