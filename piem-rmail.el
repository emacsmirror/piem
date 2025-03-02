;;; piem-rmail.el --- Rmail integration for piem  -*- lexical-binding: t; -*-

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

;; This library provides a minor mode, `piem-rmail-mode', that
;; modifies `piem' variables to teach functions like `piem-inbox' how
;; to extract information from Rmail mode buffers.

;;; Code:

(require 'piem)
(require 'rmail)

(defgroup piem-rmail nil
  "Rmail integration for piem."
  :group 'piem)

(defun piem-rmail--call-with-message (fn)
  (when (derived-mode-p 'rmail-mode 'rmail-summary-mode)
    (rmail-apply-in-message
     rmail-current-message
     (lambda ()
       (search-forward "\n\n" nil 'move)
       (narrow-to-region (point-min) (point))
       (funcall fn)))))

(defun piem-rmail-get-inbox ()
  "Return inbox name from an Rmail buffer."
  (piem-rmail--call-with-message #'piem-inbox-by-header-match))

(defun piem-rmail-get-mid ()
  "Return the message ID of an Rmail buffer."
  (when-let* ((mid (piem-rmail--call-with-message
                    (lambda () (mail-fetch-field "message-id")))))
    (replace-regexp-in-string "\\`<\\(.*\\)>\\'" "\\1" mid)))

;;;###autoload
(define-minor-mode piem-rmail-mode
  "Toggle Rmail support for piem.
With a prefix argument ARG, enable piem-rmail mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  :init-value nil
  (if piem-rmail-mode
      (progn
        (add-hook 'piem-get-inbox-functions #'piem-rmail-get-inbox)
        (add-hook 'piem-get-mid-functions #'piem-rmail-get-mid))
    (remove-hook 'piem-get-inbox-functions #'piem-rmail-get-inbox)
    (remove-hook 'piem-get-mid-functions #'piem-rmail-get-mid)))

(provide 'piem-rmail)
;;; piem-rmail.el ends here
