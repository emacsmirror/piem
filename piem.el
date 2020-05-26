;;; piem.el --- Emacs tools for working with public-index  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>
;; Keywords: vc, tools
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.3"))

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

(require 'message)
(require 'subr-x)


;;;; Options

(defgroup piem ()
  "Emacs tools for working with public-inbox archives."
  :link '(info-link "(piem)Top")
  :group 'tools)

;; TODO: These intentionally follow public-inbox's configuration
;; names.  Eventually reading values from there as well should be
;; supported.
;;
;; TODO: Decide how to deal with inboxes that map to more than one
;; coderepos.  This is important to support for people that want to
;; use a catchall inbox for small projects which they don't think
;; (yet) need a dedicated address.
(defcustom piem-inboxes nil
  "List of public-inbox-archived projects.

Elements have the form (NAME . INFO), where INFO is a property
list that supports the following properties:

  :address
  :listid
      The email address and List-ID for the inbox.
  :coderepo
      Local path of the code repository associated with the inbox.
  :url
      A URL hosting HTTPS archives.

Here's an example for the public-inbox project itself:

    (\"public-inbox\"
     :coderepo \"~/src/public-inbox/\"
     :address \"meta@public-inbox.org\"
     :listid \"meta.public-inbox.org\"
     :url \"https://public-inbox.org/meta/\")"
  :type '(alist :key-type string
                :value-type
                (plist :value-type string)))

(defcustom piem-get-inbox-functions nil
  "Functions tried to get the inbox of the current buffer.
Each function should accept no arguments and return the name of
the inbox associated with the current buffer or nil."
  :type 'hook)

(defcustom piem-get-mid-functions nil
  "Functions tried to get the message ID of the current buffer.
Each function should accept no arguments and return the
message ID associated with the current buffer or nil."
  :type 'hook)

(defcustom piem-mid-to-thread-functions nil
  "Functions tried to create an mbox from a message ID.
Each function should accept one argument, the message ID.  If the
function knows how to create an mbox for the message ID, it
should return a function that takes no arguments and inserts the
mbox's contents in the current buffer."
  :type 'hook)

(defvar piem-link-re
  (rx "/" (group (one-or-more (not (any "/" "\n"))))
      "/" (group (one-or-more (not (any "/" "\n"))))
      "/" (group (zero-or-one
                  (or "raw"
                      "t.mbox.gz"
                      (and (or "t" "T") "/#"
                           (one-or-more (not (any "/" "\n")))))))
      string-end)
  "Regular expression matching public-inbox HTTP link.
The first group is the inbox, the second is the message ID, and
the rest is any trailing endpoint.")

(defun piem-inbox-by-header-match ()
  "Return inbox based on matching message headers.
This should be called from a buffer containing a message and is
intended to be used by libraries implementing a function for
`piem-get-mid-functions'."
  (pcase-let ((`(,listid ,to ,cc)
               (save-restriction
                 (message-narrow-to-headers)
                 (list (message-fetch-field "list-id")
                       (message-fetch-field "to")
                       (message-fetch-field "cc")))))
    (catch 'hit
      (dolist (inbox piem-inboxes)
        (let* ((info (cdr inbox))
               (p-listid (plist-get info :listid)))
          (when (and listid
                     p-listid
                     (string-match-p (concat "<" (regexp-quote p-listid) ">")
                                     listid))
            (throw 'hit (car inbox)))
          (when-let ((addr (plist-get info :address))
                     (to (mapconcat #'identity (list to cc)
                                    " ")))
            (when (string-match-p (regexp-quote addr) to)
              (throw 'hit (car inbox)))))))))


;;;; Subprocess handling

(defvar piem-process-mode-font-lock-keywords
  `((,(rx line-start
          ";;; " (or "process" "directory") ":" (one-or-more not-newline)
          line-end)
      (0 font-lock-comment-face t))
    (,(rx line-start
          "Process " (one-or-more not-newline) " finished"
          line-end)
     (0 font-lock-comment-face))))

(define-derived-mode piem-process-mode nil "piem-process"
  "Major mode for displaying processes created by piem."
  :group 'piem
  (setq buffer-read-only t)
  (setq-local window-point-insertion-type t)
  (setq-local font-lock-defaults
              '(piem-process-mode-font-lock-keywords)))

(define-error 'piem-process-error "piem process error")

(defvar-local piem--buffer-process nil)

(defun piem--process-go (buffer dir program program-args fn)
  (setq dir (or dir default-directory))
  (setq buffer (get-buffer-create buffer))
  (with-current-buffer buffer
    (when (and piem--buffer-process
               (process-live-p piem--buffer-process))
      (user-error "Buffer %s already has an active process: %s"
                  (current-buffer) piem--buffer-process))
    (unless (derived-mode-p 'piem-process-mode)
      (piem-process-mode))
    (setq default-directory (file-name-as-directory dir))
    (display-buffer buffer)
    (let ((inhibit-read-only t))
      (insert (format "
%s
;;; process: %S
;;; directory:  %s
"
                      (char-to-string 12) ; form feed
                      (cons program program-args)
                      default-directory))
      (funcall fn))))

(defun piem-process-start (buffer dir program &rest program-args)
  (setq program-args (remq nil program-args))
  (piem--process-go
   buffer dir program program-args
   (lambda ()
     (setq piem--buffer-process
          (apply #'start-process
                 (file-name-nondirectory program)
                 (current-buffer)
                 program program-args)))))

(defun piem-process-call (buffer dir program &rest program-args)
  (setq program-args (remq nil program-args))
  (piem--process-go
   buffer dir program program-args
   (lambda ()
     (unless (= 0 (apply #'call-process program nil t nil program-args))
       (signal 'piem-error
               (list (format "%s call in %s failed"
                             program default-directory)))))))


;;;; Extractors

(defun piem-inbox ()
  "Return the current buffer's inbox."
  (run-hook-with-args-until-success 'piem-get-inbox-functions))

(defun piem-inbox-coderepo ()
  "Return the code repository of current buffer's inbox."
  (when-let ((p (piem-inbox))
             (repo (plist-get (cdr (assoc p piem-inboxes)) :coderepo)))
    (expand-file-name repo)))

(defun piem-mid ()
  "Return the current buffer's message ID."
  (run-hook-with-args-until-success 'piem-get-mid-functions))

(defun piem-please ()
  "How I wish I could intersect my emails, feeds, and repos")

;;; piem.el ends here
(provide 'piem)
