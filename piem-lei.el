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
(require 'subr-x)

(defgroup piem-lei nil
  "lei integration for piem."
  :group 'piem)

(defcustom piem-lei-lei-executable "lei"
  "Which lei executable to use."
  :type 'string)

(defcustom piem-lei-query-initial-input "d:20.days.ago.. "
  "Initial input when reading `lei q' queries."
  :package-version '(piem . "0.4.0")
  :type '(choice (const :tag "None" nil)
                 (string :tag "Query")))


;;;; Helpers

(defun piem-lei-insert-output (args &optional buffer)
  "Call lei with ARGS and insert standard output in BUFFER.
If BUFFER is nil, the current buffer is used."
  (unless (= 0 (apply #'call-process piem-lei-lei-executable nil
                      (list (or buffer t) nil) nil args))
    ;; TODO: Add debugging option for capturing stderr.
    (error "Calling %s with %S failed" piem-lei-lei-executable args)))


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

(defvar-local piem-lei-buffer-mid nil
  "Message ID shown in current buffer.")

(defvar-local piem-lei-buffer-args nil
  "Non-query arguments that lei was called with.")

(defvar-local piem-lei-buffer-query nil
  "Query arguments that `lei q' was called with.")

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

(defun piem-lei-show (mid &optional args display)
  "Show message for MID.

ARGS is passed to the underlying `lei q' call.

When called non-interactively, return the buffer but do not display it
unless DISPLAY is non-nil."
  (interactive
   (list (read-string "Message ID: " nil nil (piem-mid))
         'display))
  (with-current-buffer (get-buffer-create "*lei-show*")
    (let ((inhibit-read-only t)
          (query (list (concat "mid:" mid))))
      (erase-buffer)
      (piem-lei-insert-output
       (append (list "q" "--format=text") args query))
      (goto-char (point-min))
      (when (looking-at-p "# blob:")
        (delete-region (line-beginning-position)
                       (1+ (line-end-position))))
      (piem-lei-show-mode)
      (setq piem-lei-buffer-args args)
      (setq piem-lei-buffer-mid mid)
      (setq  piem-lei-buffer-query query)
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

(defvar piem-lei-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'piem-lei-q)
    (define-key map "t" #'piem-lei-mid-thread)
    map)
  "Keymap for `piem-lei-show-mode'.")

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

(defvar piem-lei-query--buffer-name "*lei-query*")

;;;###autoload
(defun piem-lei-query (query &optional args)
  "Call `lei q' with QUERY and ARGS.
QUERY is split according to `split-string-and-unquote'."
  (interactive
   (list (split-string-and-unquote
          (read-string "Query: "
                       piem-lei-query-initial-input
                       'piem-lei-query-history))
         (transient-args 'piem-lei-q)))
  (with-current-buffer (get-buffer-create piem-lei-query--buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (piem-lei-insert-output
       (append (list "q" "--format=ldjson") args query))
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
    (setq piem-lei-buffer-args args)
    (setq piem-lei-buffer-query query)
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
        (user-error "No message ID associated with current line"))
    piem-lei-buffer-args)
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

(defun piem-lei-query-show-or-scroll-up (arg)
  "Show or scroll up message for current query line.
If there is a visible `piem-lei-show-mode' buffer for the current
line's message, scroll its text upward, passing ARG to
`scroll-up-command'.  Otherwise show the message with
`piem-lei-query-show'."
  (interactive "^P")
  (if-let ((mid (piem-lei-query-get-mid)))
      (let ((w (piem-lei-query--get-visible-message-window)))
        (if (and w
                 (equal (with-current-buffer (window-buffer w)
                          piem-lei-buffer-mid)
                        mid))
            (with-selected-window w
              (scroll-up-command arg))
          (piem-lei-query-show)))
    (ding)))

(defun piem-lei-query-show-or-scroll-down (arg)
  "Show or scroll down message for current query line.
If there is a visible `piem-lei-show-mode' buffer for the current
line's message, scroll its text downward, passing ARG to
`scroll-down-command'.  Otherwise show the message with
`piem-lei-query-show'."
  (interactive "^P")
  (piem-lei-query-show-or-scroll-up
   (cond ((eq arg '-) nil)
         (arg (- arg))
         (t '-))))

(defvar piem-lei-query-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'piem-lei-query-show)
    (define-key map (kbd "DEL") #'piem-lei-query-show-or-scroll-down)
    (define-key map (kbd "SPC") #'piem-lei-query-show-or-scroll-up)
    (define-key map "n" #'piem-lei-query-next-line)
    (define-key map "p" #'piem-lei-query-previous-line)
    (define-key map "s" #'piem-lei-q)
    (define-key map "t" #'piem-lei-mid-thread)
    map)
  "Keymap for `piem-lei-query-mode'.")

(define-derived-mode piem-lei-query-mode special-mode "lei-query"
  "Major mode for displaying overview of `lei q' results."
  :group 'piem-lei
  (buffer-disable-undo)
  (hl-line-mode)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t))


;;;;; lei-q transient

(defun piem-lei-externals ()
  "Return configured externals."
  (seq-remove
   (lambda (e) (string-prefix-p "boost=" e))
   (split-string
    (with-output-to-string
      (piem-lei-insert-output
       (list "ls-external" "-z") standard-output))
    "\0" t)))

(defun piem-lei-inboxdir-urls ()
  "Return hash table mapping each inboxdir to its URL.
These values correspond to local inboxes that are configured via
public-inbox's configuration."
  (let ((case-fold-search t)
        (pi-cfg (piem--git-config-list (piem-public-inbox-config-file)))
        inboxdir-urls)
    (maphash
     (lambda (key val)
       (when (string-match
              (rx string-start "publicinbox."
                  (group (one-or-more not-newline)) "."
                  (or "inboxdir" "mainrepo")
                  string-end)
              key)
         (push (cons (car val)
                     (when-let ((url (car (gethash
                                           (format "publicinbox.%s.url"
                                                   (match-string 1 key))
                                           pi-cfg))))
                       (piem--ensure-trailing-slash url)))
               inboxdir-urls)))
     pi-cfg)
    inboxdir-urls))

(defun piem-lei-external-sources (&optional include-registered)
  "Return a list of known external sources.
Unless INCLUDE-REGISTERED is non-nil, the result does not include
sources that have already been registered with lei as an
external (via `lei add-external')."
  (let ((inboxdir-urls (piem-lei-inboxdir-urls)))
    (nconc
     (let ((inboxdirs (mapcar #'car inboxdir-urls)))
       (if include-registered
           inboxdirs
         (cl-set-difference inboxdirs (piem-lei-externals) :test #'equal)))
     (cl-set-difference
      (delq nil
            (mapcar (lambda (x)
                      (when-let ((url (plist-get (cdr x) :url)))
                        ;; lei-add-external normalizes URLs to
                        ;; have a trailing slash.
                        (piem--ensure-trailing-slash url)))
                    (piem-merged-inboxes)))
      (delq nil (mapcar #'cdr inboxdir-urls))
      :test #'equal))))

(defun piem-lei-read-external-source (prompt &optional default history)
  (completing-read prompt (piem-lei-external-sources)
                   nil nil nil history default))

(defun piem-lei-read-external-source-all (prompt &optional default history)
  (completing-read prompt (piem-lei-external-sources t)
                   nil nil nil history default))

(defun piem-lei-q-read-sort-key (&rest _ignore)
  (pcase (read-char-choice "re[c]eived re[l]evance [d]ocid "
                           (list ?c ?l ?d))
    (?c "received")
    (?l "relevance")
    (?d "docid")))

;; TODO: Support reading multiple values.
(transient-define-argument piem-lei-q:--include ()
  :description "Include external in search"
  :class 'transient-option
  :shortarg "-I"
  :argument "--include="
  :reader #'piem-lei-read-external-source)


;; TODO: Support reading multiple values.
(transient-define-argument piem-lei-q:--only ()
  :description "Search only this location"
  :class 'transient-option
  :shortarg "-O"
  :argument "--only="
  :reader #'piem-lei-read-external-source-all)

(transient-define-argument piem-lei-q:--sort ()
  :description "Sort key for results"
  :class 'transient-option
  :shortarg "-s"
  :argument "--sort="
  :reader #'piem-lei-q-read-sort-key)

(transient-define-argument piem-lei-q:--limit ()
  :description "Limit number of matches (default: 10000)"
  :class 'transient-option
  :shortarg "-n"
  :argument "--limit="
  :reader #'transient-read-number-N+)

(transient-define-argument piem-lei-q:--offset ()
  :description "Shift start of results (default: 0)"
  :class 'transient-option
  :shortarg "-N"
  :argument "--offset="
  :reader #'transient-read-number-N0)

;;;###autoload (autoload 'piem-lei-q "piem-lei" nil t)
(transient-define-prefix piem-lei-q ()
  "Search for messages with `lei q'."
  :man-page "lei-q"
  :incompatible '(("--remote" "--no-remote")
                  ("--no-externals" "--no-local"))
  ["Arguments"
   (piem-lei-q:--include)
   (piem-lei-q:--only)
   ("-g" "Match locations literally" "--globoff")
   ("xe" "Exclude results from externals" "--no-externals")
   ("xl" "Exclude results from local sources" "--no-local")
   ("xr" "Exclude results from remote sources" "--no-remote")
   ("+r" "Include results from remote sources" "--remote")
   (piem-lei-q:--sort)
   ("-r" "Reverse search results" "--reverse")
   (piem-lei-q:--limit)
   (piem-lei-q:--offset)]
  ["Actions"
   ("s" "Search" piem-lei-query)
   ("t" "Search, threaded output" piem-lei-query-threads)])


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
    (piem-lei-insert-output
     (append (list "q" "--format=ldjson") args))
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

(defvar piem-lei-query-threads--buffer-name piem-lei-query--buffer-name)

(defun piem-lei-query-threads (query &optional args pt-mid)
  "Show threads containing matches for QUERY.
ARGS is passed to the underlying `lei q' call.  If PT-MID is
non-nil and matches the message ID of a result, move point to
that line."
  (interactive
   (list (split-string-and-unquote
          (read-string "Query: "
                       piem-lei-query-initial-input
                       'piem-lei-query-history))
         (transient-args 'piem-lei-q)))
  (let* ((records (piem-lei-query--slurp
                   (append args (list "--threads") query)))
         (msgs (piem-lei-query--thread records))
         depths pt-final subject-prev)
    (with-current-buffer
        (get-buffer-create piem-lei-query-threads--buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (while msgs
          (let* ((msg (pop msgs))
                 (mid-msg (piem-lei-msg-mid msg))
                 (children (piem-lei-msg-children msg))
                 (depth (1+ (or (cdr (assoc (piem-lei-msg-parent msg) depths))
                                -1))))
            (when (and (equal depth 0)
                       (not (bobp)))
              ;; Add newline between threads to make different threads
              ;; easier to distinguish.
              (insert ?\n))
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
              (insert (propertize
                       (concat "0000-00-00 00:00 "
                               (piem-lei-query--format-thread-marker depth)
                               " <" mid-msg ">")
                       'font-lock-face
                       'piem-lei-query-thread-ghost))
              (setq subject-prev nil))
            (when (equal mid-msg pt-mid)
              (setq pt-final (line-beginning-position)))
            (insert ?\n)))
        (insert "End of lei-q results"))
      (goto-char (or pt-final (point-min)))
      (piem-lei-query-mode)
      (setq piem-lei-buffer-args args)
      (setq  piem-lei-buffer-query query)
      (pop-to-buffer-same-window (current-buffer)))))

(defun piem-lei-mid-thread (mid &optional args)
  "Show thread containing message MID.
ARGS is passed to the underlying `lei q' call."
  (interactive
   (if-let ((mid (piem-lei-get-mid)))
       (list mid piem-lei-buffer-args)
     (list (read-string "Message ID: " nil nil (piem-mid)) nil)))
  (let ((piem-lei-query-threads--buffer-name "*lei-thread*"))
    (piem-lei-query-threads (list (concat "mid:" mid)) args mid)))


;;;; piem integration

(defun piem-lei-get-mid ()
  "Return the message ID of a lei buffer."
  (cond ((derived-mode-p 'piem-lei-show-mode)
         piem-lei-buffer-mid)
        ((derived-mode-p 'piem-lei-query-mode)
         (piem-lei-query-get-mid))))

(defun piem-lei-get-inbox ()
  "Return inbox name from a lei buffer."
  (when-let ((mid (piem-lei-get-mid)))
    (with-temp-buffer
      (piem-lei-insert-output
       (list "q" "--format=mboxrd" (concat "mid:" mid)))
      (goto-char (point-min))
      (piem-inbox-by-header-match))))

(defun piem-lei-known-mid-p (mid)
  "Return non-nil if MID is known to lei.
The message ID should not include have surrounding brackets."
  (not (string-empty-p
        (with-output-to-string
          (piem-lei-insert-output
           (list "q" "--format=ldjson" (concat "mid:" mid))
           standard-output)))))

(defun piem-lei-mid-to-thread (mid)
  "Return a function that inserts an mbox for MID's thread."
  (when (piem-lei-known-mid-p mid)
    (lambda ()
      (piem-lei-insert-output
       (list "q" "--format=mboxrd" "--threads" (concat "mid:" mid))))))

;;;###autoload
(define-minor-mode piem-lei-mode
  "Toggle lei support for piem.
With a prefix argument ARG, enable piem-lei mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  :init-value nil
  (if piem-lei-mode
      (progn
        (add-hook 'piem-get-inbox-functions #'piem-lei-get-inbox)
        (add-hook 'piem-get-mid-functions #'piem-lei-get-mid)
        (add-hook 'piem-mid-to-thread-functions #'piem-lei-mid-to-thread))
    (remove-hook 'piem-get-inbox-functions #'piem-lei-get-inbox)
    (remove-hook 'piem-get-mid-functions #'piem-lei-get-mid)
    (remove-hook 'piem-mid-to-thread-functions #'piem-lei-mid-to-thread)))

;;; piem-lei.el ends here
(provide 'piem-lei)
