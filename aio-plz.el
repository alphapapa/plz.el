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
  (cl-assert (not (plist-member rest :then)) nil "Argument THEN is not allowed for `aio-plz'")
  (cl-assert (not (plist-member rest :else)) nil "Argument ELSE is not allowed for `aio-plz'")
  (let* ((promise (aio-promise))
         (rest (plist-put rest :then
                          (lambda (result)
                            (aio-resolve promise (lambda ()
                                                   result)))))
         (rest (plist-put rest :else
                          (lambda (plz-error)
                            (aio-resolve promise (lambda ()
                                                   ;; FIXME: When removing `plz-curl-error' and `plz-http-error',
                                                   ;; also remove this string from the error data.
                                                   (signal 'plz-error (list "error" plz-error))))))))
    (prog1 promise
      (condition-case err
          (apply #'plz method url rest)
        (error (aio-resolve promise (lambda ()
                                      (signal (car err) (cdr err)))))))))

(defun aio-plz-run (queue)
  (cl-assert (not (plz-queue-finally queue)) nil "Queue already has a FINALLY function: %S" queue)
  (let ((promise (aio-promise)))
    (setf (plz-queue-finally queue)
          (lambda ()
            (aio-resolve promise (lambda () t))))
    (prog1 promise
      (condition-case err
          (plz-run queue)
        (error (aio-resolve promise (lambda ()
                                      (signal (car err) (cdr err)))))))))

;;;; Footer

(provide 'aio-plz)

;;; aio-plz.el ends here
