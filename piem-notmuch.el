;;; piem-notmuch.el --- Notmuch integration for piem  -*- lexical-binding: t; -*-

;; Copyright all piem contributors <piem@inbox.kyleam.com>
;; Copyright (C) 2019 Sean Whitton <spwhitton@spwhitton.name>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; Keywords: mail, tools, vc
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.2") (notmuch))

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

(defcustom piem-notmuch-extract-patch-executable "notmuch-extract-patch"
  "Which notmuch-extract-patch executable to use."
  :package-version '(piem . "0.4.0")
  :type 'string)

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

;;;###autoload
(defun piem-notmuch-known-mid-p (mid)
  "Return non-nil if MID is known to Notmuch.
The message ID should not include Notmuch's \"id:\" prefix or
have surrounding brackets."
  (let ((query (concat "id:" mid)))
    (equal query
           (string-trim-right
            (with-output-to-string
              (call-process notmuch-command
                            nil (list standard-output nil) nil
                            "search" "--output=messages" query))))))

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
those parts' contents as the mbox, ordering the patches based on
the number at the start of the file name.  If none of the file
names start with a number, retain the original order of the
attachments.

If no MIME parts look like a patch, use the message itself if it
looks like a patch."
  (when (derived-mode-p 'notmuch-show-mode)
    (let* ((handle (piem-notmuch--with-current-message
                     (mm-dissect-buffer)))
           (n-attachments (notmuch-count-attachments handle))
           patches)
      (if (= n-attachments 0)
          (let ((id (notmuch-show-get-message-id)))
            (lambda ()
              (call-process notmuch-command nil t nil
                            "show" "--format=mbox" id)))
        (notmuch-foreach-mime-part
         (lambda (p)
           (when-let* ((patch (piem-am-extract-attached-patch p)))
             (push patch patches)))
         handle)
        (when patches
          (setq patches (sort (nreverse patches)
                              (lambda (x y) (< (car x) (car y)))))
          (cons (lambda ()
                  (dolist (patch patches)
                    (insert (cdr patch))))
                "mbox"))))))

(defun piem-notmuch-extract-patch-am-ready-mbox ()
  "Return a function that inserts an am-ready mbox.
Use the message itself if it looks like a patch using
notmuch-extract-patch to get the latest patch series from the
notmuch thread."
  (when (and (derived-mode-p 'notmuch-show-mode)
             (= (notmuch-count-attachments
                 (piem-notmuch--with-current-message
                   (mm-dissect-buffer)))
                0))
    (let* ((thread-id
            (or notmuch-show-thread-id
                (error "bug: notmuch-show-thread-id unexpectedly nil")))
           (tid
            ;; Copied from mailscripts.el
            ;;
            ;; If `notmuch-show' was called with a notmuch query rather
            ;; than a thread ID, as `org-notmuch-follow-link' in
            ;; org-notmuch.el does, then `notmuch-show-thread-id' might
            ;; be an arbitrary notmuch query instead of a thread ID.  We
            ;; need to wrap such a query in thread:{} before passing it
            ;; to notmuch-extract-patch(1), or we might not get a whole
            ;; thread extracted (e.g. if the query is just id:foo)
            (if (string= (substring thread-id 0 7) "thread:")
                thread-id
              (concat "thread:{" thread-id "}"))))
      (lambda ()
        (call-process piem-notmuch-extract-patch-executable nil t nil
                      tid)))))

(defun piem-notmuch-show-get-public-inbox-link (mid)
  "Given the message-id MID, return the public-inbox url.
This will lookup the url in the inboxes returned by
`piem-merged-inboxes'."
  (piem-mid-url mid
                (or (piem-notmuch-get-inbox)
                    (user-error "No inbox associated with current buffer"))))

;;;###autoload
(define-minor-mode piem-notmuch-mode
  "Toggle Notmuch support for piem.

With a prefix argument ARG, enable piem-notmuch mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This will add a new entry to
`notmuch-show-stash-mlarchive-link-alist' which will determine
the archive url by searching the inboxes returned by
`piem-merged-inboxes'.  You can also set
`notmuch-show-stash-mlarchive-link-default' to \"piem\" to make
this the default behavior when calling
`notmuch-show-stash-mlarchive-link'."
  :global t
  :init-value nil
  (if piem-notmuch-mode
      (progn
        (add-hook 'piem-am-ready-mbox-functions #'piem-notmuch-am-ready-mbox)
        (add-hook 'piem-get-inbox-functions #'piem-notmuch-get-inbox)
        (add-hook 'piem-get-mid-functions #'piem-notmuch-get-mid)
        (add-hook 'piem-mid-to-thread-functions #'piem-notmuch-mid-to-thread)
        (add-to-list 'notmuch-show-stash-mlarchive-link-alist
                     '("piem" . piem-notmuch-show-get-public-inbox-link)))
    (remove-hook 'piem-am-ready-mbox-functions #'piem-notmuch-am-ready-mbox)
    (remove-hook 'piem-get-inbox-functions #'piem-notmuch-get-inbox)
    (remove-hook 'piem-get-mid-functions #'piem-notmuch-get-mid)
    (remove-hook 'piem-mid-to-thread-functions #'piem-notmuch-mid-to-thread)
    (setq notmuch-show-stash-mlarchive-link-alist
          (delete '("piem" . piem-notmuch-show-get-public-inbox-link)
                  notmuch-show-stash-mlarchive-link-alist))))

(provide 'piem-notmuch)
;;; piem-notmuch.el ends here
