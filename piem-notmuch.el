;;; piem-notmuch.el --- Notmuch integration for piem  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>
;; Keywords: vc, tools
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

;; This library provides a minor mode, `piem-notmuch-mode', that
;; modifies `piem' variables to teach functions like `piem-inbox' and
;; `piem-am-ready-mbox' how to extract information from Notmuch
;; buffers.

;;; Code:

(require 'mm-decode)
(require 'notmuch)
(require 'piem)
(require 'subr-x)

(defgroup piem-notmuch nil
  "Notmuch integration for piem."
  :group 'piem)

(defmacro piem-notmuch--with-current-message (&rest body)
  (declare (indent 0) (debug (body)))
  (let ((rv (make-symbol "rv")))
    `(let (,rv)
       (with-current-notmuch-show-message
        (setq ,rv ,(macroexp-progn body)))
       ,rv)))

(defun piem-notmuch-get-inbox ()
  "Return inbox name from a `notmuch-show-mode' buffer."
  (when (derived-mode-p 'notmuch-show-mode)
    (piem-notmuch--with-current-message
      (piem-inbox-by-header-match))))

(defun piem-notmuch-get-mid ()
  "Return the message ID of a `notmuch-show-mode' buffer."
  (notmuch-show-get-message-id 'bare))

(defun piem-notmuch-known-mid-p (mid)
  "Return non-nil if MID is known to Notmuch.
The message ID should not include Notmuch's \"id:\" prefix or
have surrounding brackets."
  (let ((query (concat "id:" mid)))
    (equal query
           (string-trim-right
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process notmuch-command
                              nil '(t nil) nil
                              "search" "--output=messages" query)))))))

(defun piem-notmuch-mid-to-thread (mid)
  "Return a function that inserts an mbox for MID's thread."
  (when (piem-notmuch-known-mid-p mid)
    (lambda ()
      (call-process notmuch-command
                    nil '(t nil) nil
                    "show" "--format=mbox" "--entire-thread=true"
                    (concat "id:" mid)))))

(defun piem-notmuch-am-ready-mbox ()
  "Return a function that inserts an am-ready mbox.
If the buffer has any MIME parts that look like a patch, use
those parts' contents (in order) as the mbox.  Otherwise, use the
message itself if it looks like a patch."
  (when (derived-mode-p 'notmuch-show-mode)
    (let* ((handle (piem-notmuch--with-current-message
                     (mm-dissect-buffer)))
           (n-attachments (notmuch-count-attachments handle))
           patches)
      (if (= n-attachments 0)
          (when (string-match-p piem-patch-subject-re
                                (notmuch-show-get-subject))
            (let ((id (notmuch-show-get-message-id)))
              (lambda ()
                (call-process notmuch-command nil t nil
                              "show" "--format=mbox" id))))
        (notmuch-foreach-mime-part
         (lambda (p)
           (when-let ((patch (piem-am-extract-attached-patch p)))
             (push patch patches)))
         handle)
        (when patches
          (setq patches (nreverse patches))
          (cons (lambda ()
                  (dolist (patch patches)
                    (insert patch)))
                "mbox"))))))

;;;###autoload
(define-minor-mode piem-notmuch-mode
  "Toggle Notmuch support for piem.
With a prefix argument ARG, enable piem-notmuch mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  :init-value nil
  (if piem-notmuch-mode
      (progn
        (add-hook 'piem-am-ready-mbox-functions #'piem-notmuch-am-ready-mbox)
        (add-hook 'piem-get-inbox-functions #'piem-notmuch-get-inbox)
        (add-hook 'piem-get-mid-functions #'piem-notmuch-get-mid)
        (add-hook 'piem-mid-to-thread-functions #'piem-notmuch-mid-to-thread))
    (remove-hook 'piem-am-ready-mbox-functions #'piem-notmuch-am-ready-mbox)
    (remove-hook 'piem-get-inbox-functions #'piem-notmuch-get-inbox)
    (remove-hook 'piem-get-mid-functions #'piem-notmuch-get-mid)
    (remove-hook 'piem-mid-to-thread-functions #'piem-notmuch-mid-to-thread)))

;;; piem-notmuch.el ends here
(provide 'piem-notmuch)
