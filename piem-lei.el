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

(require 'cl-lib)
(require 'iso8601)
(require 'json)
(require 'message)
(require 'piem)
(require 'seq)

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

(defface piem-lei-query-date
  '((t :inherit font-lock-variable-name-face))
  "Face for date in `piem-lei-query-mode' buffers.")

(defface piem-lei-query-pct
  '((t :inherit shadow))
  "Face for \"search relevance\" in `piem-lei-query-mode' buffers.")

(defface piem-lei-query-from
  '((t :inherit font-lock-doc-face))
  "Face for sender name in `piem-lei-query-mode' buffers.")

(defface piem-lei-query-subject
  '((t :inherit default))
  "Face for subject in `piem-lei-query-mode' buffers.")

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
    (propertize
     (if (string-match piem-lei-query--date-re date)
         (concat (match-string 1 date) " "
                 (match-string 2 date))
       (error "Date did not match expected format: %S" date))
     'font-lock-face 'piem-lei-query-date)))

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
                       (propertize
                        (concat (number-to-string (cdr (assq 'pct data)))
                                "%")
                        'font-lock-face 'piem-lei-query-pct)
                     "")
                   (propertize (let ((from (car (cdr (assq 'f data)))))
                                 (or (car from) (cadr from)))
                               'font-lock-face 'piem-lei-query-from)
                   (propertize (cdr (assq 's data))
                               'font-lock-face 'piem-lei-query-subject)))
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

(defun piem-lei-query--get-visible-message-window ()
  (seq-some
   (lambda (w)
     (with-current-buffer (window-buffer w)
       (and (derived-mode-p 'piem-lei-show-mode)
            w)))
   (window-list (selected-frame))))

(defun piem-lei-query-next-line (n)
  "Move to the Nth next query result.
If a `piem-lei-show-mode' buffer is visible in the frame, update
it to display the message."
  (interactive "p")
  (unless (= n 0)
    (pcase-let ((ntimes (abs n))
                (`(,move-fn ,pos-fn)
                 (if (> n 0)
                     (list #'next-single-property-change
                           #'line-end-position)
                   (list #'previous-single-property-change
                         #'line-beginning-position)))
                (target nil))
      (while (and (> ntimes 0)
                  (setq target (funcall move-fn
                                        (funcall pos-fn)
                                        'piem-lei-query-result)))
        (cl-decf ntimes))
      (if (not target)
          (ding)
        (goto-char target)
        (goto-char (line-beginning-position))
        (when (piem-lei-query--get-visible-message-window)
          (piem-lei-query-show))))))

(defun piem-lei-query-previous-line (n)
  "Move to the Nth previous query result.
If a `piem-lei-show-mode' buffer is visible in the frame, update
it to display the message."
  (interactive "p")
  (piem-lei-query-next-line (- n)))

(define-derived-mode piem-lei-query-mode special-mode "lei-query"
  "Major mode for displaying overview of `lei q' results."
  :group 'piem-lei
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t))


;;;;; Threading

(defface piem-lei-query-thread-marker
  '((t :inherit default))
  "Face for thread marker in `piem-lei-query-mode' buffers.")

(defface piem-lei-query-thread-ghost
  '((t :inherit font-lock-comment-face))
  "Face for ghost message IDs in `piem-lei-query-mode' buffers.")

;; The approach here tries to loosely follow what is in public-inbox's
;; SearchThread.pm, which in turn is a modified version of the
;; algorithm described at <https://www.jwz.org/doc/threading.html>.

(cl-defstruct piem-lei-msg mid parent children time ghost)

(defun piem-lei-query--add-child (parent child)
  (let ((mid-parent (piem-lei-msg-mid parent))
        (mid-child (piem-lei-msg-mid child)))
    (when (equal mid-parent mid-child)
      (error "Parent and child have same message ID: %s"
             mid-parent))
    (when-let ((parent-old (piem-lei-msg-parent child)))
      (setf (piem-lei-msg-children parent-old)
            (delq child (piem-lei-msg-children parent-old))))
    (push child (piem-lei-msg-children parent))
    (setf (piem-lei-msg-parent child) parent)))

(defun piem-lei-query--has-descendant (msg1 msg2)
  "Is MSG2 a descendant of MSG1?"
  (let ((msg1-mid (piem-lei-msg-mid msg1))
        seen)
    (catch 'stop
      (while msg2
        (let ((msg2-mid (piem-lei-msg-mid msg2)))
          (when (or (equal msg1-mid msg2-mid)
                    (member msg2 seen))
            (throw 'stop t))
          (push msg2-mid seen))
        (setq msg2 (piem-lei-msg-parent msg2)))
      nil)))

(defun piem-lei-query--thread (records)
  "Thread messages in RECORDS.

RECORDS is a list of alists with information from `lei q'.  This
information is used to construct, link, and order `piem-lei-msg'
objects.

Return a list with a `piem-lei-msg' object for each root."
  (let ((thread (make-hash-table :test #'equal)))
    (dolist (record records)
      (let ((mid (cdr (assq 'm record))))
        (puthash mid
                 (make-piem-lei-msg
                  :mid mid :time (cdr (assq 'time record)))
                 thread)))
    (dolist (record (sort (copy-sequence records)
                          (lambda (a b)
                            (time-less-p (cdr (assq 'time a))
                                         (cdr (assq 'time b))))))
      (let ((msg-prev nil)
            (msg-cur (gethash (cdr (assq 'm record)) thread)))
        (dolist (ref (cdr (assq 'refs record)))
          (let ((msg (or (gethash ref thread)
                         (puthash ref
                                  (make-piem-lei-msg :mid ref :ghost t)
                                  thread))))
            (when (and msg-prev
                       (not (piem-lei-msg-parent msg))
                       (not (piem-lei-query--has-descendant msg msg-prev)))
              (piem-lei-query--add-child msg-prev msg))
            (setq msg-prev msg)))
        (when (and msg-prev
                   (not (piem-lei-query--has-descendant msg-cur msg-prev)))
          (piem-lei-query--add-child msg-prev msg-cur))))
    (let (roots)
      (maphash
       (lambda (_ v)
         (setf (piem-lei-msg-children v)
               (sort (piem-lei-msg-children v)
                     (lambda (a b)
                       (time-less-p (piem-lei-msg-time a)
                                    (piem-lei-msg-time b)))))
         (unless (piem-lei-msg-parent v)
           (push v roots)))
       thread)
      (nreverse roots))))

(defvar piem-lei-query--subject-split-re
  (rx string-start
      ;; Prefix.
      (group (zero-or-more space)
             (or (and (one-or-more (and "bug#" (one-or-more digit) ":"))
                      (one-or-more space)
                      (zero-or-more
                       ;; This pattern...
                       "[" (one-or-more (not (any "]" "\n"))) "]"
                       (one-or-more space)))
                 (one-or-more
                  ;; ... is repeated here.  Extract it to an rx-let
                  ;; binding once minimum Emacs version is at least
                  ;; 27.
                  "[" (one-or-more (not (any "]" "\n"))) "]"
                  (one-or-more space))))
      ;; Main subject.  A match consists of at least two islands of
      ;; non-space characters because there's not much point in
      ;; eliding one word.
      (group (one-or-more (not space))
             (one-or-more space)
             (not space)
             (one-or-more anychar))))

(defun piem-lei-query--split-subject (s)
  (if (string-match piem-lei-query--subject-split-re s)
      (cons (match-string 1 s) (match-string 2 s))
    (cons nil s)))

(defun piem-lei-query--elide-subject (s1 s2)
  (pcase-let ((`(,head2 . ,tail2) (piem-lei-query--split-subject s2)))
    (if (and s1 head2
             (let ((tail1 (cdr (piem-lei-query--split-subject s1))))
               (equal tail1 tail2)))
        (concat head2 (if (char-displayable-p ?…) "…" "..."))
      s2)))

(defun piem-lei-query--format-thread-marker (level)
  (if (= level 0)
      ""
    (concat (make-string (* 2 (1- level)) ?\s)
            (propertize "` " 'font-lock-face 'piem-lei-query-thread-marker))))

(defun piem-lei-query--slurp (args)
  (with-temp-buffer
    (apply #'call-process "lei" nil '(t nil) nil
           "q" "--format=ldjson" args)
    (goto-char (point-min))
    (let (items)
      (while (not (eobp))
        (let ((item (piem-lei-query--read-json-item)))
          (push (cons 'time (encode-time
                             (iso8601-parse (cdr (assq 'dt item)))))
                item)
          (push (cons (cdr (assq 'm item)) item) items))
        (forward-line))
      (nreverse items))))

(defun piem-lei-query-thread (mid)
  "Show thread containing message MID."
  (interactive
   (list (or (piem-lei-query-get-mid)
             (read-string "Message ID: " nil nil (piem-mid)))))
  (let* ((records (piem-lei-query--slurp
                   (list "--threads" (concat "m:" mid))))
         (msgs (piem-lei-query--thread records))
         depths pt-final subject-prev)
    (with-current-buffer (get-buffer-create "*lei-thread*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (while msgs
          (let* ((msg (pop msgs))
                 (mid-msg (piem-lei-msg-mid msg))
                 (children (piem-lei-msg-children msg))
                 (depth (1+ (or (cdr (assoc (piem-lei-msg-parent msg) depths))
                                -1))))
            (when children
              (setq msgs (append children msgs)))
            (push (cons msg depth) depths)
            (if (not (piem-lei-msg-ghost msg))
                (let* ((data (cdr (assoc mid-msg records)))
                       (subject (let ((case-fold-search t))
                                  (replace-regexp-in-string
                                   (rx string-start
                                       (one-or-more "re:" (one-or-more space)))
                                   ""
                                   (string-trim (cdr (assq 's data)))))))
                  (insert
                   (piem-lei-query--format-date data) " "
                   (piem-lei-query--format-thread-marker depth)
                   (let ((from (car (cdr (assq 'f data)))))
                     (propertize (or (car from) (cadr from))
                                 'font-lock-face 'piem-lei-query-from))
                   (if (equal subject subject-prev)
                       ""
                     (concat "  "
                             (propertize (piem-lei-query--elide-subject
                                          subject-prev subject)
                                         'font-lock-face
                                         'piem-lei-query-subject))))
                  (add-text-properties (line-beginning-position)
                                       (line-end-position)
                                       (list 'piem-lei-query-result data))
                  (setq subject-prev subject))
              (insert (make-string 17 ?\s) ; Date alignment.
                      (piem-lei-query--format-thread-marker depth)
                      (propertize (concat " <" mid-msg ">")
                                  'font-lock-face
                                  'piem-lei-query-thread-ghost))
              (setq subject-prev nil))
            (when (equal mid-msg mid)
              (setq pt-final (line-beginning-position)))
            (insert ?\n)))
        (insert "End of lei-q results"))
      (goto-char (or pt-final (point-min)))
      (piem-lei-query-mode)
      (pop-to-buffer-same-window (current-buffer)))))

;;; piem-lei.el ends here
(provide 'piem-lei)
