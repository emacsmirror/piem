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

(require 'json)
(require 'message)
(require 'piem)

(defgroup piem-lei nil
  "lei integration for piem."
  :group 'piem)


;;;; Message display

(defface piem-lei-show-header-name
  '((t :inherit message-header-name))
  "Face for header names in `piem-lei-show-mode' buffers.")

(defface piem-lei-show-header-from
  ;; Given it's focused on sending, message.el unsurprisingly doesn't
  ;; define a -from.
  '((t :inherit message-header-to))
  "Face for From headers in `piem-lei-show-mode' buffers.")

(defface piem-lei-show-header-to
  '((t :inherit message-header-to))
  "Face for To headers in `piem-lei-show-mode' buffers.")

(defface piem-lei-show-header-cc
  '((t :inherit message-header-cc))
  "Face for Cc headers in `piem-lei-show-mode' buffers.")

(defface piem-lei-show-header-other
  '((t :inherit message-header-other))
  "Face for all other headers in `piem-lei-show-mode' buffers.")

(defface piem-lei-show-header-subject
  '((t :inherit message-header-subject))
  "Face for Subject headers in `piem-lei-show-mode' buffers.")

(defface piem-lei-show-cited-text-1
  '((t :inherit message-cited-text-1))
  "Face for 1st-level cited text in `piem-lei-show-mode' buffers.")

(defface piem-lei-show-cited-text-2
  '((t :inherit message-cited-text-2))
  "Face for 2nd-level cited text in `piem-lei-show-mode' buffers.")

(defface piem-lei-show-cited-text-3
  '((t :inherit message-cited-text-3))
  "Face for 3rd-level cited text in `piem-lei-show-mode' buffers.")

(defface piem-lei-show-cited-text-4
  '((t :inherit message-cited-text-4))
  "Face for 4th-level cited text in `piem-lei-show-mode' buffers.")

(defun piem-lei-show--fontify-headers ()
  (save-excursion
    (let (last-value-face)
      (while (looking-at
              (rx line-start
                  (group (one-or-more (not (or ":" "\n"))) ":")
                  (group (one-or-more not-newline))))
        (put-text-property
         (match-beginning 1) (match-end 1)
         'font-lock-face 'piem-lei-show-header-name)
        (put-text-property
         (match-beginning 2) (match-end 2)
         'font-lock-face
         (setq last-value-face
               (pcase (downcase (match-string 1))
                 ("cc:" 'piem-lei-show-header-cc)
                 ("from:" 'piem-lei-show-header-from)
                 ("subject:" 'piem-lei-show-header-subject)
                 ("to:" 'piem-lei-show-header-to)
                 (_ 'piem-lei-show-header-other))))
        (forward-line)
        ;; Handle values that continue onto next line.
        (while (eq (char-after) ?\t)
          (save-excursion
            (skip-chars-forward "\t")
            (put-text-property (point) (line-end-position)
                               'font-lock-face last-value-face))
          (forward-line))))))

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
      (piem-lei-show-mode)
      (piem-lei-show--fontify-headers))
    (if display
        (pop-to-buffer (current-buffer))
      (current-buffer))))

(defvar piem-lei-show-mode-font-lock-keywords
  '(("^> \\(.*\\)" 0 'piem-lei-show-cited-text-1)
    ("^>> \\(.*\\)" 0 'piem-lei-show-cited-text-2)
    ("^>>> \\(.*\\)" 0 'piem-lei-show-cited-text-3)
    ("^>>>> \\(.*\\)" 0 'piem-lei-show-cited-text-4))
  "Font lock keywords for `piem-lei-show-mode'.")

(define-derived-mode piem-lei-show-mode special-mode "lei-show"
  "Major mode for displaying message via lei."
  :group 'piem-lei
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq font-lock-defaults (list piem-lei-show-mode-font-lock-keywords t))
  (setq-local line-move-visual t))


;;;; Searching

(defun piem-lei-query--read-json-item ()
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        ;; Using symbols for lei-q's output should be fine, though
        ;; it's a little odd for the "t:" field.
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (json-read)))

(defvar piem-lei-query--date-re
  (rx string-start
      (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
      "T" (group (= 2 digit) ":" (= 2 digit)) ":" (= 2 digit) "Z"
      string-end))

(defun piem-lei-query--format-date (data)
  (let ((date (cdr (assq 'dt data))))
    (if (string-match piem-lei-query--date-re date)
        (concat (match-string 1 date) " " (match-string 2 date))
      (error "Date did not match expected format: %S" date))))

;;;###autoload
(defun piem-lei-query (query)
  "Call `lei q' with QUERY.
QUERY is split according to `split-string-and-unquote'."
  (interactive
   (list (split-string-and-unquote
          (read-string "Query: " "d:20.days.ago.. " 'piem-lei-query-history))))
  (with-current-buffer (get-buffer-create "*lei-query*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (apply #'call-process "lei" nil '(t nil) nil
             "q" "--format=ldjson" query)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((data (piem-lei-query--read-json-item)))
          (delete-region (line-beginning-position) (point))
          (insert
           (format "%s %3s %-20.20s %s"
                   (piem-lei-query--format-date data)
                   (if-let ((pct (cdr (assq 'pct data))))
                       (concat (number-to-string (cdr (assq 'pct data)))
                               "%")
                     "")
                   (let ((from (car (cdr (assq 'f data)))))
                     (or (car from) (cadr from)))
                   (cdr (assq 's data))))
          (add-text-properties (line-beginning-position) (line-end-position)
                               (list 'piem-lei-query-result data)))
        (forward-line))
      (insert "End of lei-q results"))
    (goto-char (point-min))
    (piem-lei-query-mode)
    (pop-to-buffer-same-window (current-buffer))))

(defun piem-lei-query-get-mid (&optional pos)
  "Return message ID for position POS in a `piem-lei-query-mode' buffer.
When POS is nil, use the position at the start of the current
line."
  (cdr (assq 'm (get-text-property (or pos (line-beginning-position))
                                   'piem-lei-query-result))))

(defun piem-lei-query-show ()
  "Display message for current `piem-lei-query-mode' line."
  (interactive)
  (display-buffer
   (piem-lei-show
    (or (piem-lei-query-get-mid)
        (user-error "No Message ID associated with current line")))
   '(display-buffer-below-selected
     (inhibit-same-window . t)
     (window-height . 0.8))))

(define-derived-mode piem-lei-query-mode special-mode "lei-query"
  "Major mode for displaying overview of `lei q' results."
  :group 'piem-lei
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t))

;;; piem-lei.el ends here
(provide 'piem-lei)
