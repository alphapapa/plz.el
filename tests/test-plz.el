;;; test-plz.el --- Tests for plz          -*- lexical-binding: t; -*-

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
(require 'json)

(require 'plz)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(defun plz-test-get-response (response)
  "Return non-nil if RESPONSE seems to be a correct GET response."
  (and (plz-response-p response)
       (numberp (plz-response-version response))
       (eq 200 (plz-response-status response))
       (equal "application/json" (alist-get "Content-Type" (plz-response-headers response) nil nil #'equal))
       (let* ((json (json-read-from-string (plz-response-body response)))
              (headers (alist-get 'headers json))
              (user-agent (alist-get 'User-Agent headers nil nil #'equal)))
         (string-match "curl" user-agent))))

;;;; Tests

;;;;; Async

(ert-deftest plz-get-string nil
  (should (let* ((test-string)
                 (process (plz-get "https://httpbin.org/get"
                            :as 'string
                            :then (lambda (string)
                                    (setf test-string string)))))
            (cl-loop for i upto 100 ;; 10 seconds
                     while (equal 'run (process-status process))
                     do (sleep-for 0.1))
            (string-match "curl" test-string))))

(ert-deftest plz-get-buffer nil
  ;; The sentinel kills the buffer, so we get the buffer as a string.
  (should (let* ((test-buffer-string)
                 (process (plz-get "https://httpbin.org/get"
                            :as 'buffer
                            :then (lambda (buffer)
                                    (with-current-buffer buffer
                                      (setf test-buffer-string (buffer-string)))))))
            (cl-loop for i upto 100 ;; 10 seconds
                     while (equal 'run (process-status process))
                     do (sleep-for 0.1))
            (string-match "curl" test-buffer-string))))

(ert-deftest plz-get-response nil
  (should (let* ((test-response)
                 (process (plz-get "https://httpbin.org/get"
                            :as 'response
                            :then (lambda (response)
                                    (setf test-response response)))))
            (cl-loop for i upto 100 ;; 10 seconds
                     while (equal 'run (process-status process))
                     do (sleep-for 0.1))
            (plz-test-get-response test-response))))

(ert-deftest plz-get-json nil
  (should (let* ((test-json)
                 (process (plz-get "https://httpbin.org/get"
                            :as #'json-read
                            :then (lambda (json)
                                    (setf test-json json)))))
            (cl-loop for i upto 100 ;; 10 seconds
                     while (equal 'run (process-status process))
                     do (sleep-for 0.1))
            (let* ((headers (alist-get 'headers test-json))
                   (user-agent (alist-get 'User-Agent headers nil nil #'equal)))
              (string-match "curl" user-agent)))))

;;;;; Sync

(ert-deftest plz-get-sync-string nil
  (should (string-match "curl" (plz-get-sync "https://httpbin.org/get"
                                 :as 'string))))

(ert-deftest plz-get-sync-response nil
  (should (plz-test-get-response (plz-get-sync "https://httpbin.org/get"
                                   :as 'response))))

(ert-deftest plz-get-sync-json nil
  (should (let* ((test-json (plz-get-sync "https://httpbin.org/get"
                              :as #'json-read))
                 (headers (alist-get 'headers test-json))
                 (user-agent (alist-get 'User-Agent headers nil nil #'equal)))
            (string-match "curl" user-agent))))

(ert-deftest plz-get-sync-buffer nil
  ;; `buffer' is not a valid type for `plz-get-sync'.
  (should-error (plz-get-sync "https://httpbin.org/get"
                  :as 'buffer)))

;;;; Footer

(provide 'test-plz)

;;; test-plz.el ends here
