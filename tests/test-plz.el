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
(require 'let-alist)

(require 'plz)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Macros

(cl-defmacro plz-test-wait (process &optional (seconds 0.1) (times 100))
  "Wait for SECONDS seconds TIMES times for PROCESS to finish."
  `(cl-loop for i upto ,times ;; 10 seconds
            while (equal 'run (process-status ,process))
            do (sleep-for ,seconds)))

;;;; Functions

(defmacro plz-test-get-response (response)
  "Test parts of RESPONSE with `should'."
  `(and (should (plz-response-p ,response))
        (should (numberp (plz-response-version ,response)))
        (should (eq 200 (plz-response-status ,response)))
        (should (equal "application/json" (alist-get 'content-type (plz-response-headers ,response))))
        (let* ((json (json-read-from-string (plz-response-body ,response)))
               (headers (alist-get 'headers json))
               (user-agent (alist-get 'User-Agent headers nil nil #'equal)))
          (should (string-match "curl" user-agent)))))

;;;; Tests

;;;;; Async

(ert-deftest plz-get-string nil
  (let* ((test-string)
         (process (plz 'get "https://httpbin.org/get"
                    :as 'string
                    :then (lambda (string)
                            (setf test-string string)))))
    (plz-test-wait process)
    (should (string-match "curl" test-string))))

(ert-deftest plz-get-buffer nil
  ;; The sentinel kills the buffer, so we get the buffer as a string.
  (let* ((test-buffer-string)
         (process (plz 'get "https://httpbin.org/get"
                    :as 'buffer
                    :then (lambda (buffer)
                            (with-current-buffer buffer
                              (setf test-buffer-string (buffer-string)))))))
    (plz-test-wait process)
    (should (string-match "curl" test-buffer-string))))

(ert-deftest plz-get-response nil
  (let* ((test-response)
         (process (plz 'get "https://httpbin.org/get"
                    :as 'response
                    :then (lambda (response)
                            (setf test-response response)))))
    (plz-test-wait process)
    (plz-test-get-response test-response)))

(ert-deftest plz-get-json nil
  (let* ((test-json)
         (process (plz 'get "https://httpbin.org/get"
                    :as #'json-read
                    :then (lambda (json)
                            (setf test-json json)))))
    (plz-test-wait process)
    (let-alist test-json
      (should (string-match "curl" .headers.User-Agent)))))

(ert-deftest plz-post-json-string nil
  (let* ((json-string (json-encode (list (cons "key" "value"))))
         (response-json)
         (process (plz 'post "https://httpbin.org/post"
                    :headers '(("Content-Type" . "application/json"))
                    :body json-string
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json)))))
    (plz-test-wait process)
    (let-alist response-json
      (should (string-match "curl" .headers.User-Agent))
      (should (string= "value" (alist-get 'key (json-read-from-string .data)))))))

(ert-deftest plz-post-jpeg-string nil
  (let* ((jpeg-to-upload (plz-get-sync "https://httpbin.org/image/jpeg"
                           :as 'binary))
         (response-jpeg)
         (process (plz 'post "https://httpbin.org/post"
                    :headers '(("Content-Type" . "image/jpeg"))
                    :body jpeg-to-upload :body-type 'binary
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-jpeg
                                  (base64-decode-string
                                   (string-remove-prefix "data:application/octet-stream;base64,"
                                                         (alist-get 'data json))))))))
    (should (equal 'jpeg (image-type-from-data jpeg-to-upload)))
    (plz-test-wait process)
    (should (equal 'jpeg (image-type-from-data response-jpeg)))
    (should (equal (length jpeg-to-upload) (length response-jpeg)))
    (should (equal jpeg-to-upload response-jpeg))))

;; TODO: POST JSON buffer.

(ert-deftest plz-put-json-string nil
  (let* ((json-string (json-encode (list (cons "key" "value"))))
         (response-json)
         (process (plz 'put "https://httpbin.org/put"
                    :headers '(("Content-Type" . "application/json"))
                    :body json-string
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json)))))
    (plz-test-wait process)
    (let-alist response-json
      (should (string-match "curl" .headers.User-Agent))
      (should (string= "value" (alist-get 'key (json-read-from-string .data)))))))

;; TODO: Put JSON buffer.

;;;;; Sync

(ert-deftest plz-get-string-sync nil
  (should (string-match "curl" (plz-get-sync "https://httpbin.org/get"
                                 :as 'string)))
  (should (string-match "curl" (plz-get-sync "https://httpbin.org/get"))))

(ert-deftest plz-get-response-sync nil
  (plz-test-get-response (plz-get-sync "https://httpbin.org/get"
                           :as 'response)))

(ert-deftest plz-get-json-sync nil
  (let-alist (plz-get-sync "https://httpbin.org/get"
               :as #'json-read)
    (should (string-match "curl" .headers.User-Agent))))

(ert-deftest plz-get-buffer-sync nil
  ;; `buffer' is not a valid type for `plz-get-sync'.
  (should-error (plz-get-sync "https://httpbin.org/get"
                  :as 'buffer)))

;;;;; Errors

(ert-deftest plz-get-curl-error nil
  ;; Async.
  (let* ((err)
         (process (plz 'get "https://httpbinnnnnn.org/get/status/404"
                    :as 'string
                    :else (lambda (e)
                            (setf err e)))))
    (plz-test-wait process)
    (should (and (plz-error-p err)
                 (equal '(6 . "Couldn't resolve host. The given remote host was not resolved.")
                        (plz-error-curl-error err))))))

;; FIXME: This test works interactively but not in batch mode: it
;; stalls the Emacs process indefinitely, using either sleep-for or
;; sit-for.

;; (ert-deftest plz-get-killed-error nil
;;   ;; Async.
;;   (let* ((err)
;;          (process (plz 'get "https://httpbinnnnnn.org/get/status/404"
;;                     :as 'string
;;                     :else (lambda (e)
;;                             (setf err e)))))
;;     (sit-for 0.01)
;;     (delete-process process)
;;     (should (not (process-live-p process)))
;;     (should (plz-error-p err))
;;     (should (equal "curl process killed"
;;                    (plz-error-message err)))))

(ert-deftest plz-get-curl-error-sync nil
  ;; Sync.
  (let ((err (should-error (plz-get-sync "https://httpbinnnnnn.org/get/status/404"
                             :as 'string)
                           :type 'plz-curl-error)))
    (should (eq 'plz-curl-error (car err)))
    (should (plz-error-p (cdr err)))
    (should (equal '(6 . "Couldn't resolve host. The given remote host was not resolved.")
                   (plz-error-curl-error (cdr err))))))

(ert-deftest plz-get-404-error nil
  ;; Async.
  (let* ((err)
         (process (plz 'get "https://httpbin.org/get/status/404"
                    :as 'string
                    :else (lambda (e)
                            (setf err e)))))
    (plz-test-wait process)
    (should (and (plz-error-p err)
                 (plz-response-p (plz-error-response err))
                 (eq 404 (plz-response-status (plz-error-response err))))))

  ;; Sync.
  (let ((err (should-error (plz-get-sync "https://httpbin.org/get/status/404"
                             :as 'string)
                           :type 'plz-http-error)))
    (should (and (eq 'plz-http-error (car err))
                 (plz-error-p (cdr err))
                 (plz-response-p (plz-error-response (cdr err)))
                 (eq 404 (plz-response-status (plz-error-response (cdr err))))))))

(ert-deftest plz-get-timeout-error nil
  ;; Async.
  (let* ((start-time (current-time))
         (end-time)
         (plz-error)
         (process (plz 'get "https://httpbin.org/delay/5"
                    :as 'response :timeout 1
                    :else (lambda (e)
                            (setf end-time (current-time)
                                  plz-error e)))))
    (plz-test-wait process)
    (should (eq 28 (car (plz-error-curl-error plz-error))))
    (should (equal "Operation timeout." (cdr (plz-error-curl-error plz-error))))
    (should (< (time-to-seconds (time-subtract end-time start-time)) 1.1)))

  ;; Sync.
  (let ((start-time (current-time))
        (err (cdr
              (should-error (plz-get-sync "https://httpbin.org/delay/5"
                              :as 'string :timeout 1)
                            :type 'plz-curl-error)))
        (end-time (current-time)))
    (should (eq 28 (car (plz-error-curl-error err))))
    (should (equal "Operation timeout." (cdr (plz-error-curl-error err))))
    (should (< (time-to-seconds (time-subtract end-time start-time)) 1.1))))

;;;;; Finally

(ert-deftest plz-get-finally nil
  (let* ((finally-null t)
         (process (plz 'get "https://httpbin.org/get"
                    :as 'string
                    :then #'ignore
                    :finally (lambda ()
                               (setf finally-null nil)))))
    (plz-test-wait process)
    (should-not finally-null)))

;;;;; Binary

(ert-deftest plz-get-jpeg ()
  (let* ((test-jpeg)
         (process (plz 'get "https://httpbin.org/image/jpeg"
                    :as 'binary
                    :then (lambda (string)
                            (setf test-jpeg string)))))
    (plz-test-wait process)
    (should (equal 'jpeg (image-type-from-data test-jpeg)))))

(ert-deftest plz-get-jpeg-sync ()
  (let ((jpeg (plz-get-sync "https://httpbin.org/image/jpeg"
                :as 'binary)))
    (should (equal 'jpeg (image-type-from-data jpeg)))))

;;;; Footer

(provide 'test-plz)

;;; test-plz.el ends here
