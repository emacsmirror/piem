;;; piem-elfeed.el --- Elfeed integration for piem  -*- lexical-binding: t; -*-

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

(require 'elfeed)
(require 'elfeed-show)
(require 'piem)

(defgroup piem-elfeed nil
  "Elfeed integration for piem."
  :link '(info-link "(piem)Elfeed integration")
  :group 'piem)

(defvar piem-elfeed--link-re
  (rx "/" (group (one-or-more (not (any "/" "\n"))))
      "/" (group (one-or-more (not (any "/" "\n"))))
      "/" string-end))

(defun piem-elfeed-get-inbox ()
  "Return inbox name from an `elfeed-show-mode' buffer."
  (when (derived-mode-p 'elfeed-show-mode)
    (when-let ((link (elfeed-entry-link elfeed-show-entry)))
      (and (string-match piem-elfeed--link-re link)
           (match-string 1 link)))))

(defun piem-elfeed-get-mid ()
  "Return the message ID of an `elfeed-show-mode' buffer."
  (when (derived-mode-p 'elfeed-show-mode)
    (when-let ((link (elfeed-entry-link elfeed-show-entry)))
      (and (string-match piem-elfeed--link-re link)
           (match-string 2 link)))))

;; No function is defined for `piem-mid-to-thread-functions'.  All
;; callers should fall back to getting the thread from a public-inbox
;; instance.

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

;;; piem-elfeed.el ends here
(provide 'piem-elfeed)
