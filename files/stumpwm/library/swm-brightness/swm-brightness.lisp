;;; Copyright (C) 2025  Erik P Almaraz <erikalmaraz@fastmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;;

(defpackage #:swm-brightness
  (:use #:cl 
        :stumpwm)
  (:local-nicknames (#:re #:ppcre))
  (:export #:*command*
           #:*step*))
(in-package #:swm-brightness)


(defparameter *command* "brightnessctl"
  "Base command for interacting with brightness.")

(defparameter *step* 5
  "Value for brightness steps.")

(defun format-output (value)
  "Format output string VALUE to remove superflous content."
  (let* ((regexp "\\((.*?)\\)") ;; captures (#%), etc
         (filter "[^()]+") ;; Filters captured string to remove parentheses.
         (match (re:scan-to-strings regexp value))
         (content (re:scan-to-strings filter match)))
    content))

;; TODO: Refactor (similar to bluetooth module...
(defcommand increase-brightness () ()
  (format nil "Brightness: ~A" 
          (format-output
           (run-shell-command (format nil
                                      "~A set +~A%"
                                      *command*
                                      *step*) t))))

(defcommand decrease-brightness () ()
  (format nil "Brightness: ~A"
          (format-output
           (run-shell-command (format nil
                                      "~A set ~A%-"
                                      *command*
                                      *step*) t))))
