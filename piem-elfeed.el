;;; piem-elfeed.el --- Elfeed integration for piem  -*- lexical-binding: t; -*-

;; Copyright all piem contributors <piem@inbox.kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; Keywords: tools, vc
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.2") (elfeed))

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

;; This library provides a minor mode, `piem-elfeed-mode', that
;; modifies `piem' variables to teach functions like `piem-inbox' how
;; to extract information from Elfeed buffers.

;;; Code:

(require 'elfeed)
(require 'elfeed-show)
(require 'piem)

(defgroup piem-elfeed nil
  "Elfeed integration for piem."
  :group 'piem)

(defun piem-elfeed-get-inbox ()
  "Return inbox name from an `elfeed-show-mode' buffer."
  (when (derived-mode-p 'elfeed-show-mode)
    (when-let* ((link (elfeed-entry-link elfeed-show-entry)))
      (piem-inbox-by-url-match link))))

(defun piem-elfeed-get-mid ()
  "Return the message ID of an `elfeed-show-mode' buffer."
  (when-let* ((inbox (piem-elfeed-get-inbox))
              (inbox-url (piem-inbox-url inbox))
              (link (elfeed-entry-link elfeed-show-entry)))
    (and (string-match (piem-message-link-re inbox-url) link)
         (url-unhex-string (match-string 1 link)))))

;;;###autoload
(define-minor-mode piem-elfeed-mode
  "Toggle Elfeed support for piem.
With a prefix argument ARG, enable piem-elfeed mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  :init-value nil
  (if piem-elfeed-mode
      (progn
        (add-hook 'piem-get-inbox-functions #'piem-elfeed-get-inbox)
        (add-hook 'piem-get-mid-functions #'piem-elfeed-get-mid))
    (remove-hook 'piem-get-inbox-functions #'piem-elfeed-get-inbox)
    (remove-hook 'piem-get-mid-functions #'piem-elfeed-get-mid)))

(provide 'piem-elfeed)
;;; piem-elfeed.el ends here
