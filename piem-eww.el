;;; piem-eww.el --- EWW integration for piem  -*- lexical-binding: t; -*-

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

(require 'eww)
(require 'piem)

(defgroup piem-eww nil
  "EWW integration for piem."
  :link '(info-link "(piem)EWW integration")
  :group 'piem)

(defun piem-eww-get-inbox ()
  "Return inbox name from an EWW buffer."
  (when (derived-mode-p 'eww-mode)
    (when-let ((link (plist-get eww-data :url)))
      (and (string-match piem-link-re link)
           (match-string 1 link)))))

(defun piem-eww-get-mid ()
  "Return the message ID of an EWW buffer."
  (when (derived-mode-p 'eww-mode)
    (when-let ((link (plist-get eww-data :url)))
      (and (string-match piem-link-re link)
           (match-string 2 link)))))

;;;###autoload
(define-minor-mode piem-eww-mode
  "Toggle EWW support for piem.
With a prefix argument ARG, enable piem-eww mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  :init-value nil
  (if piem-eww-mode
      (progn
        (add-hook 'piem-get-inbox-functions #'piem-eww-get-inbox)
        (add-hook 'piem-get-mid-functions #'piem-eww-get-mid))
    (remove-hook 'piem-get-inbox-functions #'piem-eww-get-inbox)
    (remove-hook 'piem-get-mid-functions #'piem-eww-get-mid)))

;;; piem-eww.el ends here
(provide 'piem-eww)
