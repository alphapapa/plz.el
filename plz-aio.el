;;; plz-aio.el --- Async HTTP library                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: comm
;; Package-Requires: ((emacs "26.3") (aio "1.0"))

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

;;

;;; Code:

;;;; Requirements

(require 'aio)

(require 'plz)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(aio-defun aio-plz-get (url &key headers as then else
                            (connect-timeout plz-connect-timeout)
                            (decode t decode-s))
  "FIXME: Docstring."
  (declare (indent defun))
  (plz--curl 'get url
             :headers headers
             :connect-timeout connect-timeout
             :decode (if (and decode-s (not decode)) nil decode)
             :as as :then then :else else))

;;;; Footer

(provide 'plz-aio)

;;; plz-aio.el ends here
