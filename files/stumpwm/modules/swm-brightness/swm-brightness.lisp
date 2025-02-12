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
  (:use #:cl #:stumpwm)
  (:local-nicknames (#:re #:ppcre))
  (:export #:set-brightness-command))
(in-package #:swm-brightness)


(defvar *brightness-command* ""
  "Base command for interacting with bluetooth.")

(defun set-brightness-command (&optional (brightness-command *brightness-command))
  "Set brightness command, *BRIGHTNESS-COMMAND*, defaults to brightnessctl."
  (setf *brightness-command* brightness-command))

