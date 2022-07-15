;;; test-plz.el --- Tests for plz          -*- lexical-binding: t; -*-

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

;; NOTE: NOTE: NOTE: NOTE: Yes, four NOTEs, because this is important:
;; As of this moment, all of the tests pass when run with makem.sh.
;; And when running them in an interactive Emacs with ERT, one test at
;; a time, individual tests pass, or almost always do (depending on
;; whether the httpbin.org server is overloaded).  But when running
;; multiple tests in ERT at one time,
;; i.e. (ert-run-tests-interactively "plz-"), multiple, if not most,
;; tests fail, but not the same ones every time.

;; I have now spent hours trying to figure out why, inserting many
;; debug statements in many functions, and come up with nothing.  I
;; tried changing the way `accept-process-output' is called, like
;; using timeouts or JUST-THIS-ONE, but it made no difference.  I
;; tried calling it extra times, nope.  I tried calling the sentinel
;; extra times when it seemed that it hadn't run the THEN function,
;; nope.  Nothing seems to make a difference.

;; I even checked out an earlier commit, before the commit that
;; rewrote/merged the synchronous request code into the `plz'
;; function, thinking that surely I broke something--but, nope, they
;; apparently failed the same way back then: passing with makem.sh,
;; passing individually, but failing when run in close succession by
;; ERT.

;; After inserting enough debug statements, I noticed that the process
;; sentinel sometimes seemed to run for the last time after the ERT
;; test had returned, which suggests that ERT might be doing something
;; weird, or somehow its instrumentation interferes with the
;; process-handling code.  But if that's not the cause, then I'm out
;; of ideas.

;; So then I tried rewriting the synchronous request code to use
;; `call-process-region', instead of calling `accept-process-output'
;; in a loop to block on the curl process (which is how the Elisp
;; manual says to do it), but that still made no difference: even the
;; async requests fail in the same way with ERT.  So that doesn't
;; appear to be the problem, either.

;; So is there some kind of fundamental flaw in the `plz' design?
;; Maybe.  Is there a simple, logical oversight in its code that only
;; manifests under certain conditions?  Maybe.  Is ERT doing something
;; weird that's interfering with process-related code?  Maybe.  Is
;; Emacs's own process-handling code still broken in some mysterious
;; way?  Maybe.

;; But despite all of that, when using `plz' "in anger", in `ement',
;; it seems to work reliably for me.  I did get one report from one
;; user that sounded like the same kind of problem I'm seeing with ERT
;; here, but then he tried `ement-connect' again, and it worked.  And
;; I'm sitting here watching `ement' constantly using `plz' to talk to
;; the matrix.org server, and I haven't had a single error or failure,
;; even after hours of being connected.  It *seems* to *actually*
;; work.

;; So, if you're reading this, and you're wondering whether you should
;; use `plz': Well, please do, and please let me know if you have any
;; problems; I do need to know whether it's working for other users.
;; And if you think you might know what's going wrong when running the
;; tests in ERT, please let me know, because I'm out of ideas: as far
;; as I can tell, when it comes to process-handling in Emacs, "there
;; be dragons."

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

(cl-defun plz-test-wait (process &optional (seconds 0.1) (times 100))
  "Wait for SECONDS seconds TIMES times for PROCESS to finish."
  (when process
    ;; Sometimes it seems that the process is killed, the THEN
    ;; function called by its sentinel, and its buffer killed, all
    ;; before this function gets called with the process argument;
    ;; when that happens, tests that use this can fail.  Testing
    ;; whether PROCESS is non-nil seems to fix it, but it's possible
    ;; that something funny is going on...
    (cl-loop for i upto times ;; 10 seconds
             while (equal 'run (process-status process))
             do (sleep-for seconds))))

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
  (let* ((jpeg-to-upload (plz 'get "https://httpbin.org/image/jpeg"
                           :as 'binary :then 'sync))
         (_ (unless jpeg-to-upload
              (error "jpeg-to-upload is nil")))
         (response-json)
         (response-jpeg)
         (process (plz 'post "https://httpbin.org/post"
                    :headers '(("Content-Type" . "image/jpeg"))
                    :body jpeg-to-upload :body-type 'binary
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json
                                  response-jpeg
                                  (base64-decode-string
                                   (string-remove-prefix "data:application/octet-stream;base64,"
                                                         (alist-get 'data json))))))))
    (should (equal 'jpeg (image-type-from-data jpeg-to-upload)))
    (plz-test-wait process)
    (should response-json)
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
  (let-alist (json-read-from-string (plz 'get "https://httpbin.org/get"
                                      :as 'string :then 'sync))
    (should (equal "https://httpbin.org/get" .url))))

(ert-deftest plz-get-response-sync nil
  (plz-test-get-response (plz 'get "https://httpbin.org/get"
                           :as 'response :then 'sync)))

(ert-deftest plz-get-json-sync nil
  (let-alist (plz 'get "https://httpbin.org/get"
               :as #'json-read :then 'sync)
    (should (string-match "curl" .headers.User-Agent))))

(ert-deftest plz-get-buffer-sync nil
  (let ((buffer (plz 'get "https://httpbin.org/get"
                  :as 'buffer :then 'sync)))
    (unwind-protect
        (should (buffer-live-p buffer))
      (kill-buffer buffer))))

;;;;; Headers

;; These tests were added when plz--curl was changed to send headers
;; with "--config" rather than on the command line.

(ert-deftest plz-get-with-headers ()
  (let* ((response-json)
         (process (plz 'get "https://httpbin.org/get"
                    :headers '(("X-Plz-Test-Header" . "plz-test-header-value"))
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json)))))
    (plz-test-wait process)
    (let-alist response-json
      (should (equal "plz-test-header-value" .headers.X-Plz-Test-Header)))))

(ert-deftest plz-post-with-headers ()
  (let* ((alist (list (cons "key" "value")))
         (response-json)
         (process (plz 'post "https://httpbin.org/post"
                    :headers '(("Content-Type" . "application/json")
                               ("X-Plz-Test-Header" . "plz-test-header-value"))
                    :body (json-encode alist)
                    :as #'json-read
                    :then (lambda (json)
                            (setf response-json json)))))
    (plz-test-wait process)
    (let-alist response-json
      (should (equal "plz-test-header-value" .headers.X-Plz-Test-Header))
      (should (equal "value" (alist-get 'key (json-read-from-string .data)))))))

(ert-deftest plz-get-json-with-headers-sync ()
  (let-alist (plz 'get "https://httpbin.org/get"
               :headers '(("X-Plz-Test-Header" . "plz-test-header-value"))
               :as #'json-read :then 'sync)
    (should (string-match "curl" .headers.User-Agent))
    (should (equal "plz-test-header-value" .headers.X-Plz-Test-Header))))

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
  (let ((err (should-error (plz 'get "https://httpbinnnnnn.org/get/status/404"
                             :as 'string :then 'sync)
                           :type 'plz-curl-error)))
    (should (eq 'plz-curl-error (car err)))
    (should (plz-error-p (cdr err)))
    (should (equal '(6 . "Couldn't resolve host. The given remote host was not resolved.")
                   (plz-error-curl-error (cdr err))))))

(ert-deftest plz-get-404-error nil
  ;; FIXME: Wrap each test expression in `should' rather than using `should-and'.

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
  (let ((err (should-error (plz 'get "https://httpbin.org/get/status/404"
                             :as 'string :then 'sync)
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
              (should-error (plz 'get "https://httpbin.org/delay/5"
                              :as 'string :then 'sync :timeout 1)
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
  (let ((jpeg (plz 'get "https://httpbin.org/image/jpeg"
                :as 'binary :then 'sync)))
    (should (equal 'jpeg (image-type-from-data jpeg)))))

;;;;; Downloading to files

(ert-deftest plz-get-temp-file ()
  (let ((filename (plz 'get "https://httpbin.org/image/jpeg"
                    :as 'file :then 'sync)))
    (unwind-protect
        (let ((jpeg-data (with-temp-buffer
                           (insert-file-contents filename)
                           (buffer-string))))
          (should (equal 'jpeg (image-type-from-data jpeg-data))))
      ;; It's a temp file, so it should always be deleted.
      (delete-file filename))))

(ert-deftest plz-get-named-file ()
  (let ((filename (make-temp-file "plz-")))
    ;; HACK: Delete the temp file and reuse its name, because
    ;; `make-temp-name' is less convenient to use.
    (delete-file filename)
    (unwind-protect
        (progn
          (plz 'get "https://httpbin.org/image/jpeg"
            :as `(file ,filename) :then 'sync)
          (let ((jpeg-data (with-temp-buffer
                             (insert-file-contents filename)
                             (buffer-string))))
            (should (equal 'jpeg (image-type-from-data jpeg-data)))))
      ;; It's a temp file, so it should always be deleted.
      (when (file-exists-p filename)
        (delete-file filename)))))

;;;;; Queue

;; TODO: Test that limit is enforced (though it seems to work fine).

(ert-deftest plz-queue ()
  (let ((queue (make-plz-queue :limit 2))
        (urls '("https://httpbin.org/get?foo=0"
                "https://httpbin.org/get?foo=1"))
        completed-urls)
    (dolist (url urls)
      (plz-queue queue
        'get url :then (lambda (_)
                         (push url completed-urls))))
    (plz-run queue)
    (cl-loop with waits = 0
             while (and (plz-queue-active queue) (< waits 20))
             do (progn
                  (sleep-for 0.1)
                  (cl-incf waits)))
    (and (seq-set-equal-p urls completed-urls)
         (zerop (plz-length queue)))))

;;;; Footer

(provide 'test-plz)

;;; test-plz.el ends here
