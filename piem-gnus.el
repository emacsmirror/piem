;;; piem-gnus.el --- Gnus integration for piem  -*- lexical-binding: t; -*-

;; Copyright all piem contributors <piem@inbox.kyleam.com>

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

;; This library provides a minor mode, `piem-gnus-mode', that modifies
;; `piem' variables to teach functions like `piem-inbox' and
;; `piem-am-ready-mbox' how to extract information from Gnus buffers.

;;; Code:

(require 'gnus)
(require 'gnus-art)
(require 'gnus-sum)
(require 'mail-parse)
(require 'message)
(require 'piem)

(defgroup piem-gnus nil
  "Gnus integration for piem."
  :group 'piem)

(defun piem-gnus-get-inbox ()
  "Return inbox name from a Gnus article"
  (when (derived-mode-p 'gnus-article-mode 'gnus-summary-mode)
    (with-current-buffer gnus-original-article-buffer
      (piem-inbox-by-header-match))))

(defun piem-gnus-get-mid ()
  "Return the message ID of a Gnus article."
  (when (derived-mode-p 'gnus-article-mode 'gnus-summary-mode)
    (with-current-buffer gnus-original-article-buffer
      (when-let ((mid (message-field-value "message-id")))
        (if (string-match (rx string-start (zero-or-more space) "<"
                              (group (one-or-more (not (any ">"))))
                              ">" (zero-or-more space) string-end)
                          mid)
            (match-string 1 mid)
          mid)))))

(defun piem-gnus-mid-to-thread (mid)
  (when (and (derived-mode-p 'gnus-summary-mode)
             (string-equal (substring
                            (mail-header-id (gnus-summary-article-header))
                            1 -1)       ; Remove "<" and ">"
                           mid))
    (save-excursion
      ;; Cursor has to be at the root of the thread
      (gnus-summary-refer-parent-article most-positive-fixnum)
      (let ((articles (gnus-summary-articles-in-thread))
            messages
            ;; Just show raw message
            (gnus-have-all-headers t)
            gnus-article-prepare-hook
            gnus-article-decode-hook
            gnus-display-mime-function
            gnus-break-pages)
        (mapc (lambda (article)
                (gnus-summary-display-article article)
                (push (format
                       "From mboxrd@z Thu Jan  1 00:00:00 1970\n%s\n"
                       (replace-regexp-in-string ; From-munge
                        "^>*From "
                        ">\\&"
                        (with-current-buffer gnus-article-buffer
                          (buffer-substring-no-properties
                           (point-min)
                           (point-max)))))
                      messages))
              articles)
        (lambda ()
          (insert (apply #'concat (nreverse messages))))))))

(defun piem-gnus-am-ready-mbox ()
  "Return a function that inserts an am-ready mbox.

If the buffer has any MIME parts that look like a patch, use
those parts' contents as the mbox, ordering the patches based on
the number at the start of the file name.  If none of the file
names start with a number, retain the original order of the
attachments.

If no MIME parts look like a patch, use the message itself if it
looks like a patch."
  (when (derived-mode-p 'gnus-article-mode 'gnus-summary-mode)
    (cond
     (gnus-article-mime-handles
      (when-let ((patches (delq nil (mapcar #'piem-am-extract-attached-patch
                                            gnus-article-mime-handles))))
        (setq patches (sort patches (lambda (x y) (< (car x) (car y)))))
        (cons (lambda ()
                (dolist (patch patches)
                  (insert (cdr patch))))
              "mbox")))
     (gnus-article-buffer
      (when-let ((patch (with-current-buffer gnus-article-buffer
                          (save-restriction
                            (widen)
                            (and (string-match-p
                                  piem-patch-subject-re
                                  (mail-decode-encoded-word-string
                                   (message-field-value "subject")))
                                 (buffer-substring-no-properties
                                  (point-min) (point-max)))))))
        (cons (lambda () (insert patch))
              "mbox"))))))

;;;###autoload
(define-minor-mode piem-gnus-mode
  "Toggle Gnus support for piem.
With a prefix argument ARG, enable piem-gnus mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  :init-value nil
  (if piem-gnus-mode
      (progn
        (add-hook 'piem-am-ready-mbox-functions #'piem-gnus-am-ready-mbox)
        (add-hook 'piem-get-inbox-functions #'piem-gnus-get-inbox)
        (add-hook 'piem-get-mid-functions #'piem-gnus-get-mid)
        (add-hook 'piem-mid-to-thread-functions #'piem-gnus-mid-to-thread))
    (remove-hook 'piem-am-ready-mbox-functions #'piem-gnus-am-ready-mbox)
    (remove-hook 'piem-get-inbox-functions #'piem-gnus-get-inbox)
    (remove-hook 'piem-get-mid-functions #'piem-gnus-get-mid)
    (remove-hook 'piem-mid-to-thread-functions #'piem-gnus-mid-to-thread)))

;;; piem-gnus.el ends here
(provide 'piem-gnus)
