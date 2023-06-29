;;; aio-plz.el --- Async HTTP library                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: comm
;; Package-Requires: ((emacs "26.3") (aio "1.0") (plz "0.6"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides an `aio' wrapper for `plz'.

;;; Code:

;;;; Requirements

(require 'plz)

(require 'aio)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(defun aio-plz (method url &rest rest)
  "FIXME: Docstring."
  (declare (indent defun))
  (let* ((promise (aio-promise))
         (then (lambda (result)
                 (aio-resolve promise (lambda () result))))
         (rest (plist-put rest :then then)))
    (prog1 promise
      (condition-case err
          (apply #'plz method url rest)
        (error (aio-resolve promise (lambda ()
                                      (signal (car err) (cdr err)))))))))

;;;; Footer

(provide 'aio-plz)

;;; aio-plz.el ends here
