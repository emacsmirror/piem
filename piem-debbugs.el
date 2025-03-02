;;; piem-debbugs.el --- Debbugs integration for piem  -*- lexical-binding: t; -*-

;; Copyright all piem contributors <piem@inbox.kyleam.com>

;; Author: Jelle Licht <jlicht@fsfe.org>
;; Keywords: mail, news, tools, vc
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.2") (debbugs "0.29"))

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

;; This library provides a minor mode, `piem-debbugs-mode', that modifies
;; `piem' variables to teach functions like `piem-inbox' how to
;; extract information from Debbugs buffers.

;;; Code:

(require 'debbugs-gnu)
(require 'piem)

(defgroup piem-debbugs nil
  "Debbugs integration for piem."
  :group 'piem)

(defun piem-debbugs-get-inbox ()
  "Return inbox name from a Debbugs buffer."
  (when (and (derived-mode-p 'debbugs-gnu-mode)
             (boundp 'debbugs-gnu-local-query))
    (when-let* ((gnu-package (alist-get 'package debbugs-gnu-local-query)))
      (piem-inbox-by-gnu-package-match gnu-package))))

(defun piem-debbugs-get-mid ()
  "Return the message ID of a Debbugs buffer."
  (when (derived-mode-p 'debbugs-gnu-mode)
    (let ((msgid (alist-get 'msgid (debbugs-gnu-current-status))))
      (when (stringp msgid)
        (string-trim msgid "<" ">")))))

;;;###autoload
(define-minor-mode piem-debbugs-mode
  "Toggle Debbugs support for piem.
With a prefix argument ARG, enable piem-debbugs mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  :init-value nil
  (if piem-debbugs-mode
      (progn
        (add-hook 'piem-get-inbox-functions #'piem-debbugs-get-inbox)
        (add-hook 'piem-get-mid-functions #'piem-debbugs-get-mid))
    (remove-hook 'piem-get-inbox-functions #'piem-debbugs-get-inbox)
    (remove-hook 'piem-get-mid-functions #'piem-debbugs-get-mid)))

(provide 'piem-debbugs)
;;; piem-debbugs.el ends here
