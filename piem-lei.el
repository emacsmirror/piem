;;; piem-lei.el --- lei integration for piem  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Kyle Meyer <kyle@kyleam.com>

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

;;; Code:

(require 'piem)

(defgroup piem-lei nil
  "lei integration for piem."
  :group 'piem)


;;;; Message display

(defun piem-lei-show (mid &optional display)
  "Show message for MID.
When called non-interactively, return the buffer but do not display it
unless DISPLAY is non-nil."
  (interactive
   (list (read-string "Message ID: " nil nil (piem-mid))
         'display))
  (with-current-buffer (get-buffer-create "*lei-show*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (call-process "lei" nil '(t nil) nil
                    "q" "--format=text" (concat "m:" mid))
      (goto-char (point-min))
      (when (looking-at-p "# blob:")
        (delete-region (line-beginning-position)
                       (1+ (line-end-position))))
      (piem-lei-show-mode))
    (if display
        (pop-to-buffer (current-buffer))
      (current-buffer))))

(define-derived-mode piem-lei-show-mode special-mode "lei-show"
  "Major mode for displaying message via lei."
  :group 'piem-lei
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t))

;;; piem-lei.el ends here
(provide 'piem-lei)
