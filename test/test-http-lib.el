;;; test-http-lib.el --- Tests for http-lib          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords:

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

(require 'ert)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

;;;; Tests

(ert-deftest http-lib-get-sync nil
  (http-lib-response-p (http-lib-get "https://httpbin.org/get" :sync t)))

;;;; Footer

(provide 'test-http-lib)

;;; test-http-lib.el ends here
