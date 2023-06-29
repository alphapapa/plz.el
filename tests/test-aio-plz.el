;;; test-aio-plz.el --- Tests for aio-plz          -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>

;; This file is part of GNU Emacs.

;;; License:

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

;; This file provides tests for `aio-plz'.

;;; Code:

;;;; Requirements

(require 'ert)
(require 'json)
(require 'let-alist)
(require 'map)

(require 'aio-plz)

(require 'test-plz)

;;;; Variables

;;;; Customization

;;;; Commands

;;;; Macros

(defmacro aio-with-test (timeout &rest body)
  ;; Copied from aio-test.el.
  "Run body asynchronously but block synchronously until it completes.

If TIMEOUT seconds passes without completion, signal an
aio-timeout to cause the test to fail."
  (declare (indent 1))
  `(let* ((promises (list (aio-with-async ,@body)
                          (aio-timeout ,timeout)))
          (select (aio-make-select promises)))
     (aio-wait-for
      (aio-with-async
        (aio-await (aio-await (aio-select select)))))))

(cl-defmacro aio-plz-deftest (name (&key (timeout 1)) &body docstring-keys-and-body)
  "Like `ert-deftest', but defines tests for both HTTP/1.1 and HTTP/2."
  (declare (debug (&define [&name "test@" symbolp]
			   sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  `(progn
     ,@(cl-loop for http-version in '("1.1" "2")
                collect (let ((name (intern (format "%s-http%s" name http-version))))
                          `(ert-deftest ,name ()
                             (let ((plz-curl-default-args
                                    ',(append plz-curl-default-args (list (format "--http%s" http-version)))))
                               (aio-with-test ,timeout
                                 ,@docstring-keys-and-body)))))))

;;;; Functions

;;;; Tests

;;;;; Async

(aio-plz-deftest aio-plz-get-string ()
  (let ((result (aio-await (aio-plz 'get (plz-test-url "/get")
                             :as 'string))))
    (should-not (string-match "cxrl" result))
    (should (string-match "curl" result))))

(aio-plz-deftest aio-plz-get-buffer ()
  (let ((result-buffer (aio-await (aio-plz 'get (plz-test-url "/get")
                                    :as 'buffer))))
    (should (string-match "curl" (with-current-buffer result-buffer
                                   (buffer-string))))))

(aio-plz-deftest aio-plz-get-response ()
  (let ((result-response (aio-await (aio-plz 'get (plz-test-url "/get")
                                      :as 'response))))
    (plz-test-get-response result-response)))

(aio-plz-deftest aio-plz-get-json ()
  (let ((result-json (aio-await (aio-plz 'get (plz-test-url "/get")
                                  :as #'json-read))))
    (let-alist result-json
      (should (string-match "curl" .headers.User-Agent)))))

(aio-plz-deftest aio-plz-post-json-string ()
  (let* ((json-string (json-encode (list (cons "key" "value"))))
         (result-json (aio-await
                       (aio-plz 'post (plz-test-url "/post")
                         :headers '(("Content-Type" . "application/json"))
                         :body json-string
                         :as #'json-read))))
    (let-alist result-json
      (should (string-match "curl" .headers.User-Agent))
      (should (string= "value" (alist-get 'key (json-read-from-string .data)))))))

(aio-plz-deftest aio-plz-post-jpeg-string ()
  (let* ((jpeg-to-upload (aio-await
                          (aio-plz 'get (plz-test-url "/image/jpeg")
                            :as 'binary)))
         (_ (unless jpeg-to-upload
              (error "jpeg-to-upload is nil")))
         (response-json (aio-await
                         (aio-plz 'post (plz-test-url "/post")
                           :headers '(("Content-Type" . "image/jpeg"))
                           :body jpeg-to-upload :body-type 'binary
                           :as #'json-read)))
         (response-jpeg (base64-decode-string
                         (string-remove-prefix "data:application/octet-stream;base64,"
                                               (alist-get 'data response-json)))))
    (should (equal 'jpeg (image-type-from-data jpeg-to-upload)))
    (should response-json)
    (should (equal 'jpeg (image-type-from-data response-jpeg)))
    (should (equal (length jpeg-to-upload) (length response-jpeg)))
    (should (equal jpeg-to-upload response-jpeg))))

;; TODO: POST JSON buffer.

(aio-plz-deftest aio-plz-put-json-string ()
  (let* ((json-string (json-encode (list (cons "key" "value"))))
         (response-json (aio-await
                         (aio-plz 'put (plz-test-url "/put")
                           :headers '(("Content-Type" . "application/json"))
                           :body json-string
                           :as #'json-read))))
    (let-alist response-json
      (should (string-match "curl" .headers.User-Agent))
      (should (string= "value" (alist-get 'key (json-read-from-string .data)))))))

;; TODO: Put JSON buffer.

;;;;; Sync

(aio-plz-deftest aio-plz-get-string-sync ()
  (let-alist (json-read-from-string (aio-await
                                     (aio-plz 'get (plz-test-url "/get")
                                       :as 'string)))
    (should (equal (plz-test-url "/get") .url))))

(aio-plz-deftest aio-plz-get-response-sync ()
  (plz-test-get-response (aio-await
                          (aio-plz 'get (plz-test-url "/get")
                            :as 'response))))

(aio-plz-deftest aio-plz-get-json-sync ()
  (let-alist (aio-await (aio-plz 'get (plz-test-url "/get")
                          :as #'json-read))
    (should (string-match "curl" .headers.User-Agent))))

(aio-plz-deftest aio-plz-get-buffer-sync ()
  (let ((buffer (aio-await (aio-plz 'get (plz-test-url "/get")
                             :as 'buffer))))
    (unwind-protect
        (progn
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should-not (looking-at-p plz-http-response-status-line-regexp))
            (should (string-match "curl" (buffer-string)))))
      (kill-buffer buffer))))

;;;;; Headers

;; These tests were added when plz--curl was changed to send headers
;; with "--config" rather than on the command line.

(aio-plz-deftest aio-plz-get-with-headers ()
  (let* ((response-json (aio-await
                         (aio-plz 'get (plz-test-url "/get")
                           :headers '(("X-Plz-Test-Header" . "plz-test-header-value"))
                           :as #'json-read))))
    (let-alist response-json
      (should (equal "plz-test-header-value" .headers.X-Plz-Test-Header)))))

(aio-plz-deftest aio-plz-post-with-headers ()
  (let* ((alist (list (cons "key" "value")))
         (response-json (aio-await
                         (aio-plz 'post (plz-test-url "/post")
                           :headers '(("Content-Type" . "application/json")
                                      ("X-Plz-Test-Header" . "plz-test-header-value"))
                           :body (json-encode alist)
                           :as #'json-read))))
    (let-alist response-json
      (should (equal "plz-test-header-value" .headers.X-Plz-Test-Header))
      (should (equal "value" (alist-get 'key (json-read-from-string .data)))))))

(aio-plz-deftest aio-plz-get-json-with-headers-sync ()
  (let-alist (aio-await
              (aio-plz 'get (plz-test-url "/get")
                :headers '(("X-Plz-Test-Header" . "plz-test-header-value"))
                :as #'json-read))
    (should (string-match "curl" .headers.User-Agent))
    (should (equal "plz-test-header-value" .headers.X-Plz-Test-Header))))

;;;;; HEAD requests

;; NOTE: httpbin.org doesn't appear to support a "/head" endpoint,
;; so we'll use "/get".

(aio-plz-deftest aio-plz-head-without-headers ()
  ;; I'm not sure how useful it may be to make a HEAD request without
  ;; caring about the headers, but perhaps it could be useful as a
  ;; lightweight way to test a server's presence, so we should
  ;; probably support it.  This merely tests that no error is
  ;; signaled, which should mean that the HEAD request succeeded.
  (should (aio-await
           (aio-plz 'head (plz-test-url "/get")))))

(aio-plz-deftest aio-plz-head-as-response ()
  (let ((response (aio-await
                   (aio-plz 'head (plz-test-url "/get")
                     :as 'response))))
    (should (equal "application/json"
                   (alist-get 'content-type
                              (plz-response-headers response))))))

;;;;; POST requests

(aio-plz-deftest aio-plz-post-empty-body ()
  (should (equal ""
                 (alist-get 'data
                            (json-read-from-string
                             (aio-await
                              (aio-plz 'post (plz-test-url "/post")))))))
  (should (equal "application/json"
                 (alist-get 'content-type
                            (plz-response-headers
                             (aio-await
                              (aio-plz 'post (plz-test-url "/post") :as 'response)))))))

;;;;; Status codes

(aio-plz-deftest aio-plz-201-succeeds ()
  ;; This merely tests that a 201 response does not signal an error.
  (should (aio-await
           (aio-plz 'get (plz-test-url "/status/201")))))

;; TODO: Test the error signals.

(aio-plz-deftest aio-plz-400-errors ()
  (should-error (aio-await
                 (aio-plz 'get (plz-test-url "/status/400")))))

(aio-plz-deftest aio-plz-500-errors ()
  (should-error (aio-await
                 (aio-plz 'get (plz-test-url "/status/500")))))

;;;;; Redirects

(aio-plz-deftest aio-plz-301-redirects ()
  (plz-test-get-response
   (aio-await
    (aio-plz 'get (plz-test-url "/redirect-to?url=URI-PREFIX%2Fget&status_code=301")
      :as 'response))))

;;;;; Errors

;; TODO: Sync requests with ":as 'response" should return response for errors rather than signaling.

;; FIXME: This test works interactively but not in batch mode: it
;; stalls the Emacs process indefinitely, using either sleep-for or
;; sit-for.

;; (plz-deftest plz-get-killed-error nil
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

(aio-plz-deftest aio-plz-get-curl-error ()
  (pcase-let ((`(,_signal . (,_message ,data))
	       (should-error (aio-await
                              (aio-plz 'get "https://httpbinnnnnn.org/get/status/404"
                                :as 'string))
                             :type 'plz-error)))
    (should (plz-error-p data))
    (should (equal '(6 . "Couldn't resolve host. The given remote host was not resolved.")
                   (plz-error-curl-error data)))))

(aio-plz-deftest aio-plz-get-404-error ()
  (pcase-let ((`(,_signal . (,_message ,data))
	       (should-error (aio-await
                              (aio-plz 'get (plz-test-url "/get/status/404")
			        :as 'string))
                             :type 'plz-error)))
    (should (plz-error-p data))
    (should (plz-response-p (plz-error-response data)))
    (should (eq 404 (plz-response-status (plz-error-response data))))))

(aio-plz-deftest aio-plz-get-timeout-error (:timeout 2)
  (pcase-let ((`(,_signal . (,_message ,(cl-struct plz-error (curl-error `(,code . ,message)))))
	       (should-error (aio-await
                              (aio-plz 'get (plz-test-url "/delay/5")
				:as 'string :timeout 1))
			     :type 'plz-error)))
    (should (eq 28 code))
    (should (equal "Operation timeout." message))))

;;;;; Finally

(aio-plz-deftest aio-plz-get-finally ()
  ;; NOTE: Not sure if this is sensible for AIO.
  (let* ((finally-null t)
         (result (aio-await
                  (aio-plz 'get (plz-test-url "/get")
                    :as 'string))))
    (should finally-null)
    (setf finally-null nil)
    (should-not finally-null)))

;;;;; Binary

(aio-plz-deftest aio-plz-get-jpeg ()
  (let ((result-string (aio-await
                        (aio-plz 'get (plz-test-url "/image/jpeg")
                          :as 'binary))))
    (should (equal 'jpeg (image-type-from-data result-string)))))

;;;;; Downloading to files

(aio-plz-deftest aio-plz-get-temp-file ()
  (let ((filename (aio-await
                   (aio-plz 'get (plz-test-url "/image/jpeg")
                     :as 'file))))
    (unwind-protect
        (let ((jpeg-data (with-temp-buffer
                           (insert-file-contents filename)
                           (buffer-string))))
          (should (equal 'jpeg (image-type-from-data jpeg-data))))
      ;; It's a temp file, so it should always be deleted.
      (delete-file filename))))

(aio-plz-deftest aio-plz-get-named-file ()
  (let ((filename (make-temp-file "plz-")))
    ;; HACK: Delete the temp file and reuse its name, because
    ;; `make-temp-name' is less convenient to use.
    (delete-file filename)
    (unwind-protect
        (progn
          (aio-await
           (aio-plz 'get (plz-test-url "/image/jpeg")
             :as `(file ,filename)))
          (let ((jpeg-data (with-temp-buffer
                             (insert-file-contents filename)
                             (buffer-string))))
            (should (equal 'jpeg (image-type-from-data jpeg-data)))))
      ;; It's a temp file, so it should always be deleted.
      (when (file-exists-p filename)
        (delete-file filename)))))

(aio-plz-deftest aio-plz-upload-file-by-name ()
  (let ((filename (make-temp-file "plz-"))
        response-json)
    (unwind-protect
        (progn
          (with-temp-file filename
            (insert "deadbeef"))
          (setf response-json
                (aio-await
                 (aio-plz 'put (plz-test-url "/put")
                   :body `(file ,filename)
                   :as #'json-read)))
          (should (equal "deadbeef" (alist-get 'data response-json)))
          (should-not (alist-get 'files response-json)))
      (delete-file filename))))

;;;;; Queue

;; TODO: Test that limit is enforced (though it seems to work fine).

(aio-plz-deftest aio-plz-queue-with-finally (:timeout 5)
  "Ensure that a queue with a FINALLY function calls it correctly.
That is, that the function is called after the queue is emptied,
and only called once."
  (let* ((finally-called-at nil)
         (finally-called-times 0)
         (queue (make-plz-queue :limit 2))
         (urls (list (plz-test-url "/delay/2")))
         completed-urls queue-started-at)
    (dolist (url urls)
      (plz-queue queue
        'get url :then (lambda (_)
                         (push url completed-urls))))
    (setf queue-started-at (current-time))
    (should (aio-await
             (aio-plz-run queue)))
    (should (seq-set-equal-p urls completed-urls))
    (should (zerop (plz-length queue)))
    (should (>= (float-time (time-subtract (current-time) queue-started-at))
                2))))

;; TODO: Add test for canceling queue.

;;;; Footer

(provide 'test-aio-plz)

;;; test-aio-plz.el ends here
