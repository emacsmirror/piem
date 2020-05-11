;;; piem-notmuch.el --- Notmuch integration for piem  -*- lexical-binding: t; -*-

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

(require 'notmuch)
(require 'piem)
(require 'subr-x)

(defgroup piem-notmuch nil
  "Notmuch integration for piem."
  :link '(info-link "(piem)Notmuch integration")
  :group 'piem)

(defcustom piem-notmuch-executable "notmuch"
  "Which notmuch executable to use."
  :type 'string)

(defmacro piem-notmuch--with-current-message (&rest body)
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

(defun piem-notmuch-mid-to-thread (mid)
  "Return a function that inserts an mbox for MID's thread."
  (let ((query (concat "id:" mid)))
    (when (equal query
                 (string-trim-right
                  (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process piem-notmuch-executable
                                    nil '(t nil) nil
                                    "search" "--output=messages" query)))))
      (lambda ()
        (call-process piem-notmuch-executable
                      nil '(t nil) nil
                      "show" "--format=mbox" "--entire-thread=true"
                      query)))))

(define-minor-mode piem-notmuch-mode
  "Toggle Notmuch support for piem.
With a prefix argument ARG, enable piem-notmuch mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  :init-value nil
  (if piem-notmuch-mode
      (progn
        (add-hook 'piem-get-inbox-functions #'piem-notmuch-get-inbox)
        (add-hook 'piem-get-mid-functions #'piem-notmuch-get-mid)
        (add-hook 'piem-mid-to-thread-functions #'piem-notmuch-mid-to-thread))
    (remove-hook 'piem-get-inbox-functions #'piem-notmuch-get-inbox)
    (remove-hook 'piem-get-mid-functions #'piem-notmuch-get-mid)
    (remove-hook 'piem-mid-to-thread-functions #'piem-notmuch-mid-to-thread)))

;;; piem-notmuch.el ends here
(provide 'piem-notmuch)
