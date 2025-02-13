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

;;; References:
;;; 1.
;;;

(defpackage #:swm-brightness
  (:use #:cl 
        #:stumpwm)
  (:local-nicknames (#:re #:ppcre))
  (:export #:increase-brightness
           #:decrease-brightness
           #:*brightness-command*
           #:*brightness-step*))
(in-package #:swm-brightness)


(defparameter *brightness-command* "brightnessctl"
  "Base command for interacting with brightness.")

(defparameter *brightness-step* 5
  "String value for brightness steps.")

(defun format-output (value)
  "Format output string VALUE to remove superflous content."
  (let* ((regexp "\\((.*?)\\)") ;; captures (#%), etc
         (filter "[^()]+") ;; Filters captured string to remove parentheses.
         (match (re:scan-to-strings regexp value))
         (content (re:scan-to-strings filter match)))
    content))

;; TODO: Refactor (similar to bluetooth module...
(defcommand increase-brightness () ()
  (format nil "^B^6Brightness: ~A^b" 
          (format-output
           (run-shell-command (format nil
                                      "~A set +~A%"
                                      *brightness-command*
                                      *brightness-step*) t))))

(defcommand decrease-brightness () ()
  (format nil "^B^6Brightness: ~A^b"
          (format-output
           (run-shell-command (format nil
                                      "~A set ~A%-"
                                      *brightness-command*
                                      *brightness-step*) t))))
