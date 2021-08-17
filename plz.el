;;; plz.el --- HTTP library                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/plz.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.3"))
;; Keywords: comm, network, http

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
;;
;; An HTTP library that uses curl as a backend.  Inspired by, and some
;; code copied from, Christopher Wellons's library, elfeed-curl.el.
;;
;; Why this package?
;;
;; 1.  `url' works well for many things, but it has some issues (and have
;;     you seen its code?).
;; 2.  `request' works well for many things, but it has some issues (and
;;     have you seen its code?).
;; 3.  Chris Wellons doesn't have time to factor his excellent
;;     elfeed-curl.el library out of Elfeed.  This will have to do.
;;
;; Why is it called `plz'?
;;
;; 1.  There's already a package called `http'.
;; 2.  There's already a package called `request'.
;; 3.  Naming things is hard.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'rx)
(require 'subr-x)

;;;; Errors

;; FIXME: `condition-case' can't catch these...?
(define-error 'plz-curl-error "Curl error")
(define-error 'plz-http-error "HTTP error")

;;;; Structs

(cl-defstruct plz-response
  version status headers body)

(cl-defstruct plz-error
  curl-error response message)

;;;; Constants

(defconst plz-http-response-status-line-regexp
  (rx bol "HTTP/" (group (1+ (or digit "."))) (1+ blank)
      (group (1+ digit)))
  "Regular expression matching HTTP response status line.")

(defconst plz-curl-errors
  ;; Copied from elfeed-curl.el.
  '((1 . "Unsupported protocol.")
    (2 . "Failed to initialize.")
    (3 . "URL malformed. The syntax was not correct.")
    (4 . "A feature or option that was needed to perform the desired request was not enabled or was explicitly disabled at build-time.")
    (5 . "Couldn't resolve proxy. The given proxy host could not be resolved.")
    (6 . "Couldn't resolve host. The given remote host was not resolved.")
    (7 . "Failed to connect to host.")
    (8 . "FTP weird server reply. The server sent data curl couldn't parse.")
    (9 . "FTP access denied.")
    (11 . "FTP weird PASS reply.")
    (13 . "FTP weird PASV reply.")
    (14 . "FTP weird 227 format.")
    (15 . "FTP can't get host.")
    (17 . "FTP couldn't set binary.")
    (18 . "Partial file. Only a part of the file was transferred.")
    (19 . "FTP couldn't download/access the given file, the RETR (or similar) command failed.")
    (21 . "FTP quote error. A quote command returned error from the server.")
    (22 . "HTTP page not retrieved.")
    (23 . "Write error.")
    (25 . "FTP couldn't STOR file.")
    (26 . "Read error. Various reading problems.")
    (27 . "Out of memory. A memory allocation request failed.")
    (28 . "Operation timeout.")
    (30 . "FTP PORT failed.")
    (31 . "FTP couldn't use REST.")
    (33 . "HTTP range error. The range \"command\" didn't work.")
    (34 . "HTTP post error. Internal post-request generation error.")
    (35 . "SSL connect error. The SSL handshaking failed.")
    (36 . "FTP bad download resume.")
    (37 . "FILE couldn't read file.")
    (38 . "LDAP bind operation failed.")
    (39 . "LDAP search failed.")
    (41 . "Function not found. A required LDAP function was not found.")
    (42 . "Aborted by callback.")
    (43 . "Internal error. A function was called with a bad parameter.")
    (45 . "Interface error. A specified outgoing interface could not be used.")
    (47 . "Too many redirects.")
    (48 . "Unknown option specified to libcurl.")
    (49 . "Malformed telnet option.")
    (51 . "The peer's SSL certificate or SSH MD5 fingerprint was not OK.")
    (52 . "The server didn't reply anything, which here is considered an error.")
    (53 . "SSL crypto engine not found.")
    (54 . "Cannot set SSL crypto engine as default.")
    (55 . "Failed sending network data.")
    (56 . "Failure in receiving network data.")
    (58 . "Problem with the local certificate.")
    (59 . "Couldn't use specified SSL cipher.")
    (60 . "Peer certificate cannot be authenticated with known CA certificates.")
    (61 . "Unrecognized transfer encoding.")
    (62 . "Invalid LDAP URL.")
    (63 . "Maximum file size exceeded.")
    (64 . "Requested FTP SSL level failed.")
    (65 . "Sending the data requires a rewind that failed.")
    (66 . "Failed to initialise SSL Engine.")
    (67 . "The user name, password, or similar was not accepted and curl failed to log in.")
    (68 . "File not found on TFTP server.")
    (69 . "Permission problem on TFTP server.")
    (70 . "Out of disk space on TFTP server.")
    (71 . "Illegal TFTP operation.")
    (72 . "Unknown TFTP transfer ID.")
    (73 . "File already exists (TFTP).")
    (74 . "No such user (TFTP).")
    (75 . "Character conversion failed.")
    (76 . "Character conversion functions required.")
    (77 . "Problem with reading the SSL CA cert (path? access rights?).")
    (78 . "The resource referenced in the URL does not exist.")
    (79 . "An unspecified error occurred during the SSH session.")
    (80 . "Failed to shut down the SSL connection.")
    (82 . "Could not load CRL file, missing or wrong format (added in 7.19.0).")
    (83 . "Issuer check failed (added in 7.19.0).")
    (84 . "The FTP PRET command failed")
    (85 . "RTSP: mismatch of CSeq numbers")
    (86 . "RTSP: mismatch of Session Identifiers")
    (87 . "unable to parse FTP file list")
    (88 . "FTP chunk callback reported error")
    (89 . "No connection available, the session will be queued")
    (90 . "SSL public key does not matched pinned public key"))
  "Alist mapping curl error code integers to helpful error messages.")

;;;; Variables

(defvar-local plz-else nil
  "Callback function for errored completion of request.
Called in current curl process buffer.")

(defvar-local plz-then nil
  "Callback function for successful completion of request.
Called in current curl process buffer.")

(defvar-local plz-finally nil
  "Function called unconditionally after completion of request.
Called after the then/else function, without arguments, outside
the curl process buffer.")

(defvar-local plz-result nil
  "Used when `plz' is called synchronously.")

(defvar-local plz-sync nil
  "Used when `plz' is called synchronously.")

;;;; Customization

(defgroup plz nil
  "Options for `plz'."
  :group 'network
  :link '(url-link "https://github.com/alphapapa/plz.el"))

(defcustom plz-curl-program "curl"
  "Name of curl program to call."
  :type 'string)

(defcustom plz-curl-default-args
  '("--silent"
    "--compressed"
    "--location"
    "--dump-header" "-")
  "Default arguments to curl.
Note that these arguments are passed on the command line, which
may be visible to other users on the local system."
  :type '(repeat string))

(defcustom plz-connect-timeout 5
  "Default connection timeout in seconds.
This limits how long the connection phase may last (the
\"--connect-timeout\" argument to curl)."
  :type 'number)

(defcustom plz-timeout 60
  "Default request timeout in seconds.
This limits how long an entire request may take, including the
connection phase and waiting to receive the response (the
\"--max-time\" argument to curl)."
  :type 'number)

;;;; Functions

;;;;; Public

(cl-defun plz (method url &key headers body else finally noquery
                      (as 'string) (then 'sync)
                      (body-type 'text) (decode t decode-s)
                      (connect-timeout plz-connect-timeout) (timeout plz-timeout))
  "Request METHOD from URL with curl.
Return the curl process object or, for a synchronous request, the
selected result.

HEADERS may be an alist of extra headers to send with the
request.

BODY-TYPE may be `text' to send BODY as text, or `binary' to send
it as binary.

AS selects the kind of result to pass to the callback function
THEN, or the kind of result to return for synchronous requests.
It may be:

- `buffer' to pass the response buffer.

- `binary' to pass the response body as an undecoded string.

- `string' to pass the response body as a decoded string.

- `response' to pass a `plz-response' struct.

- `file' to pass a temporary filename to which the response body
  has been saved without decoding.

- `(file FILENAME)' to pass FILENAME after having saved the
  response body to it without decoding.  FILENAME must be a
  non-existent file; if it exists, it will not be overwritten,
  and an error will be signaled.

- A function, which is called in the response buffer with it
  narrowed to the response body (suitable for, e.g. `json-read').

If DECODE is non-nil, the response body is decoded automatically.
For binary content, it should be nil.  When AS is `binary',
DECODE is automatically set to nil.

THEN is a callback function, whose sole argument is selected
above with AS.  Or THEN may be `sync' to make a synchronous
request, in which case the result is returned directly.

ELSE is an optional callback function called when the request
fails with one argument, a `plz-error' struct.  If ELSE is nil,
an error is signaled when the request fails, either
`plz-curl-error' or `plz-http-error' as appropriate, with a
`plz-error' struct as the error data.  For synchronous requests,
this argument is ignored.

FINALLY is an optional function called without argument after
THEN or ELSE, as appropriate.  For synchronous requests, this
argument is ignored.

CONNECT-TIMEOUT and TIMEOUT are a number of seconds that limit
how long it takes to connect to a host and to receive a response
from a host, respectively.

NOQUERY is passed to `make-process', which see."
  ;; Inspired by and copied from `elfeed-curl-retrieve'.
  (declare (indent defun))
  (setf decode (if (and decode-s (not decode))
                   nil decode))
  ;; NOTE: By default, for PUT requests and POST requests >1KB, curl sends an
  ;; "Expect:" header, which causes servers to send a "100 Continue" response, which
  ;; we don't want to have to deal with, so we disable it by setting the header to
  ;; the empty string.  See <https://gms.tf/when-curl-sends-100-continue.html>.
  ;; TODO: Handle "100 Continue" responses and remove this workaround.
  (push (cons "Expect" "") headers)
  (let* ((data-arg (pcase-exhaustive body-type
                     ('binary "--data-binary")
                     ('text "--data")))
         (curl-command-line-args (append plz-curl-default-args
                                         (list "--config" "-")))
         (curl-config-header-args (cl-loop for (key . value) in headers
                                           collect (cons "--header" (format "%s: %s" key value))))
         (curl-config-args (append curl-config-header-args
                                   (list (cons "--url" url))
                                   (when connect-timeout
                                     (list (cons "--connect-timeout"
                                                 (number-to-string connect-timeout))))
                                   (when timeout
                                     (list (cons "--max-time" (number-to-string timeout))))
                                   (pcase method
                                     ((or 'put 'post)
                                      (cl-assert body)
                                      (list (cons "--request" (upcase (symbol-name method)))
                                            ;; It appears that this must be the last argument
                                            ;; in order to pass data on the rest of STDIN.
                                            (cons data-arg "@-"))))))
         (curl-config (cl-loop for (key . value) in curl-config-args
                               concat (format "%s \"%s\"\n" key value)))
         (decode (pcase as
                   ('binary nil)
                   (_ decode)))
         sync-p)
    (when (eq 'sync then)
      (setf sync-p t
            then (lambda (result)
                   (setf plz-result result))))
    (with-current-buffer (generate-new-buffer " *plz-request-curl*")
      ;; Avoid making process in a nonexistent directory (in case the current
      ;; default-directory has since been removed).  It's unclear what the best
      ;; directory is, but this seems to make sense, and it should still exist.
      (let ((default-directory temporary-file-directory)
            (process (make-process :name "plz-request-curl"
                                   :buffer (current-buffer)
                                   :coding 'binary
                                   :command (append (list plz-curl-program) curl-command-line-args)
                                   :connection-type 'pipe
                                   :sentinel #'plz--sentinel
                                   :stderr (current-buffer)
                                   :noquery noquery))
            ;; The THEN function is called in the response buffer.
            (then (pcase-exhaustive as
                    ((or 'binary 'string)
                     (lambda ()
                       (let ((coding-system (or (plz--coding-system) 'utf-8)))
                         (pcase as
                           ('binary (set-buffer-multibyte nil)))
                         (plz--narrow-to-body)
                         (when decode
                           (decode-coding-region (point) (point-max) coding-system))
                         (funcall then (buffer-string)))))
                    ('buffer (lambda ()
                               (funcall then (current-buffer))))
                    ('response (lambda ()
                                 (funcall then (plz--response :decode-p decode))))
                    ('file (lambda ()
                             (set-buffer-multibyte nil)
                             (plz--narrow-to-body)
                             (let ((filename (make-temp-file "plz-")))
                               (condition-case err
                                   (write-region (point-min) (point-max) filename)
                                 ;; In case of an error writing to the file, delete the temp file
                                 ;; and signal the error.  Ignore any errors encountered while
                                 ;; deleting the file, which would obscure the original error.
                                 (error (ignore-errors
                                          (delete-file filename))
                                        (signal (car err) (cdr err))))
                               (funcall then filename))))
                    (`(file ,(and (pred stringp) filename))
                     (lambda ()
                       (set-buffer-multibyte nil)
                       (plz--narrow-to-body)
                       (condition-case err
                           (write-region (point-min) (point-max) filename nil nil nil 'excl)
                         ;; Since we are creating the file, it seems sensible to delete it in case of an
                         ;; error while writing to it (e.g. a disk-full error).  And we ignore any errors
                         ;; encountered while deleting the file, which would obscure the original error.
                         (error (ignore-errors
                                  (when (file-exists-p filename)
                                    (delete-file filename)))
                                (signal (car err) (cdr err))))
                       (funcall then filename)))
                    ((pred functionp) (lambda ()
                                        (let ((coding-system (or (plz--coding-system) 'utf-8)))
                                          (plz--narrow-to-body)
                                          (when decode
                                            (decode-coding-region (point) (point-max) coding-system))
                                          (funcall then (funcall as))))))))
        (setf plz-then then
              plz-else else
              plz-finally finally
              plz-sync sync-p)
        ;; Send --config arguments.
        (process-send-string process curl-config)
        (when body
          (cl-typecase body
            (string (process-send-string process body))
            (buffer (with-current-buffer body
                      (process-send-region process (point-min) (point-max))))))
        (process-send-eof process)
        (if sync-p
            (progn
              (while
                  ;; According to the Elisp manual, blocking on a process's
                  ;; output is really this simple.  And it seems to work.
                  (accept-process-output process))
              (prog1 plz-result
                (unless (eq as 'buffer)
                  (kill-buffer))))
          process)))))

;;;;; Private

(defun plz--sentinel (process-or-buffer status)
  "Process buffer of curl output in PROCESS-OR-BUFFER.
If PROCESS-OR-BUFFER if a process, uses its buffer; if a buffer,
uses it.  STATUS should be the process's event string (see info
node `(elisp) Sentinels').  Kills the buffer before returning."
  ;; Inspired by and some code copied from `elfeed-curl--sentinel'.
  (let* ((buffer (cl-etypecase process-or-buffer
                   (process (process-buffer process-or-buffer))
                   (buffer process-or-buffer)))
         (finally (buffer-local-value 'plz-finally buffer))
         sync)
    (unwind-protect
        (with-current-buffer buffer
          (setf sync plz-sync)
          (pcase-exhaustive status
            ((or 0 "finished\n")
             ;; Curl exited normally: check HTTP status code.
             (pcase (plz--http-status)
               (200 (funcall plz-then))
               (_ (let ((err (make-plz-error :response (plz--response))))
                    (pcase-exhaustive plz-else
                      (`nil (signal 'plz-http-error err))
                      ((pred functionp) (funcall plz-else err)))))))

            ((or (and (pred numberp) code)
                 (rx "exited abnormally with code " (let code (group (1+ digit)))))
             ;; Curl error.
             (let* ((curl-exit-code (cl-typecase code
                                      (string (string-to-number code))
                                      (number code)))
                    (curl-error-message (alist-get curl-exit-code plz-curl-errors))
                    (err (make-plz-error :curl-error (cons curl-exit-code curl-error-message))))
               (pcase-exhaustive plz-else
                 ;; FIXME: Returning a plz-error struct which has a curl-error slot, wrapped in a plz-curl-error, is confusing.
                 (`nil (signal 'plz-curl-error err))
                 ((pred functionp) (funcall plz-else err)))))

            ("killed\n"
             ;; Curl process killed.
             (let ((err (make-plz-error :message "curl process killed")))
               (pcase-exhaustive plz-else
                 (`nil (signal 'plz-curl-error err))
                 ((pred functionp) (funcall plz-else err)))))))
      (when finally
        (funcall finally))
      (unless sync
        (kill-buffer buffer)))))

;;;;;; HTTP Responses

;; Functions for parsing HTTP responses.

(cl-defun plz--response (&key (decode-p t))
  "Return response struct for HTTP response in current buffer.
When DECODE-P is non-nil, decode the response body automatically
according to the apparent coding system."
  (save-excursion
    (goto-char (point-min))
    ;; Parse HTTP version and status code.
    (looking-at plz-http-response-status-line-regexp)
    (let* ((http-version (string-to-number (match-string 1)))
           (status-code (string-to-number (match-string 2)))
           (headers (plz--headers))
           (coding-system (or (plz--coding-system headers) 'utf-8)))
      (plz--narrow-to-body)
      (when decode-p
        (decode-coding-region (point) (point-max) coding-system))
      (make-plz-response
       :version http-version
       :status status-code
       :headers headers
       :body (buffer-string)))))

(defun plz--coding-system (&optional headers)
  "Return coding system for HTTP response in current buffer.
HEADERS may optionally be an alist of parsed HTTP headers to
refer to rather than the current buffer's unparsed headers."
  (let* ((headers (or headers (plz--headers)))
         (content-type (alist-get "Content-Type" headers nil nil #'string=)))
    (when content-type
      (coding-system-from-name content-type))))

(defun plz--http-status ()
  "Return HTTP status code for HTTP response in current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at plz-http-response-status-line-regexp)
      (string-to-number (match-string 2)))))

(defun plz--headers ()
  "Return headers alist for HTTP response in current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((limit (save-excursion
                   (re-search-forward "^\r\n" nil)
                   (point))))
      (cl-loop while (re-search-forward (rx bol (group (1+ (not (in ":")))) ":" (1+ blank)
                                            (group (1+ (not (in "\r\n")))))
                                        limit t)
               ;; NOTE: Some HTTP servers send all-lowercase header keys, which means an alist
               ;; lookup with `equal' or `string=' fails when the case differs.  We don't want
               ;; users to have to worry about this, so for consistency, we downcase the
               ;; header name.  And while we're at it, we might as well intern it so we can
               ;; use `alist-get' without having to add "nil nil #'equal" every time.
               collect (cons (intern (downcase (match-string 1))) (match-string 2))))))

(defun plz--narrow-to-body ()
  "Narrow to body of HTTP response in current buffer."
  (goto-char (point-min))
  (re-search-forward "^\r\n" nil)
  (narrow-to-region (point) (point-max)))

;;;; Footer

(provide 'plz)

;;; plz.el ends here
