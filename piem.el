;;; piem.el --- Emacs tools for working with public-index  -*- lexical-binding: t; -*-

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

(require 'message)
(provide 'rfc2047)
(require 'subr-x)


;;;; Options

(defgroup piem ()
  "Emacs tools for working with public-inbox archives."
  :link '(info-link "(piem)Top")
  :group 'tools)

;; TODO: These intentionally follow public-inbox's configuration
;; names.  Eventually reading values from there as well should be
;; supported.
;;
;; TODO: Decide how to deal with inboxes that map to more than one
;; coderepos.  This is important to support for people that want to
;; use a catchall inbox for small projects which they don't think
;; (yet) need a dedicated address.
(defcustom piem-inboxes nil
  "List of public-inbox-archived projects.

Elements have the form (NAME . INFO), where INFO is a property
list that supports the following properties:

  :address
  :listid
      The email address and List-ID for the inbox.
  :coderepo
      Local path of the code repository associated with the inbox.
  :url
      A URL hosting HTTPS archives.

Here's an example for the public-inbox project itself:

    (\"public-inbox\"
     :coderepo \"~/src/public-inbox/\"
     :address \"meta@public-inbox.org\"
     :listid \"meta.public-inbox.org\"
     :url \"https://public-inbox.org/meta/\")"
  :type '(alist :key-type string
                :value-type
                (plist :value-type string)))

(defcustom piem-get-inbox-functions nil
  "Functions tried to get the inbox of the current buffer.
Each function should accept no arguments and return the name of
the inbox associated with the current buffer or nil."
  :type 'hook)

(defcustom piem-get-mid-functions nil
  "Functions tried to get the message ID of the current buffer.
Each function should accept no arguments and return the
message ID associated with the current buffer or nil."
  :type 'hook)

(defcustom piem-mid-to-thread-functions nil
  "Functions tried to create an mbox from a message ID.
Each function should accept one argument, the message ID.  If the
function knows how to create an mbox for the message ID, it
should return a function that takes no arguments and inserts the
mbox's contents in the current buffer."
  :type 'hook)

(defvar piem-link-re
  (rx "/" (group (one-or-more (not (any "/" "\n"))))
      "/" (group (one-or-more (not (any "/" "\n"))))
      "/" (group (zero-or-one
                  (or "raw"
                      "t.mbox.gz"
                      (and (or "t" "T") "/#"
                           (one-or-more (not (any "/" "\n")))))))
      string-end)
  "Regular expression matching public-inbox HTTP link.
The first group is the inbox, the second is the message ID, and
the rest is any trailing endpoint.")

(defun piem-inbox-by-header-match ()
  "Return inbox based on matching message headers.
This should be called from a buffer containing a message and is
intended to be used by libraries implementing a function for
`piem-get-mid-functions'."
  (pcase-let ((`(,listid ,to ,cc)
               (save-restriction
                 (message-narrow-to-headers)
                 (list (message-fetch-field "list-id")
                       (message-fetch-field "to")
                       (message-fetch-field "cc")))))
    (catch 'hit
      (dolist (inbox piem-inboxes)
        (let* ((info (cdr inbox))
               (p-listid (plist-get info :listid)))
          (when (and listid
                     p-listid
                     (string-match-p (concat "<" (regexp-quote p-listid) ">")
                                     listid))
            (throw 'hit (car inbox)))
          (when-let ((addr (plist-get info :address))
                     (to (mapconcat #'identity (list to cc)
                                    " ")))
            (when (string-match-p (regexp-quote addr) to)
              (throw 'hit (car inbox)))))))))

(defcustom piem-git-executable
  (or (and (boundp 'magit-git-executable) magit-git-executable)
      "git")
  "Which git executable to use."
  :type 'string)

(defcustom piem-use-magit (featurep 'magit)
  "Whether to use Magit where possible."
  :type 'boolean)

(defcustom piem-default-branch-function
  #'piem-name-branch-who-what-v
  "Function that generates the default branch on completion.

The function is called with one argument, a plist that contains
the following information about the patch series:

  :date
  :from
  :subject
      The date, sender, and subject of the cover letter, if any, or of
      the first patch otherwise.
  :base-commit
      The reported base commit of the patch, if any."
  :type 'function)


;;;; Subprocess handling

(defvar piem-process-mode-font-lock-keywords
  `((,(rx line-start
          ";;; " (or "process" "directory") ":" (one-or-more not-newline)
          line-end)
      (0 font-lock-comment-face t))
    (,(rx line-start
          "Process " (one-or-more not-newline) " finished"
          line-end)
     (0 font-lock-comment-face))))

(defconst piem-process-output-buffer "*piem-process-output*")

(define-derived-mode piem-process-mode nil "piem-process"
  "Major mode for displaying processes created by piem."
  :group 'piem
  (setq buffer-read-only t)
  (setq-local window-point-insertion-type t)
  (setq-local font-lock-defaults
              '(piem-process-mode-font-lock-keywords)))

(define-error 'piem-process-error "piem process error")

(defvar-local piem--buffer-process nil)

(defun piem--process-go (dir program program-args fn)
  (setq dir (or dir default-directory))
  (let ((buffer (get-buffer-create piem-process-output-buffer)))
    (with-current-buffer buffer
      (when (and piem--buffer-process
                 (process-live-p piem--buffer-process))
        (user-error "Buffer %s already has an active process: %s"
                    (current-buffer) piem--buffer-process))
      (unless (derived-mode-p 'piem-process-mode)
        (piem-process-mode))
      (setq default-directory (file-name-as-directory dir))
      (display-buffer buffer)
      (let ((inhibit-read-only t))
        (insert (format "\n%s\n;;; process: %S\n;;; directory:  %s\n"
                        (char-to-string 12) ; form feed
                        (cons program program-args)
                        default-directory))
        (funcall fn)))))

(defun piem-process-start (dir program &rest program-args)
  (setq program-args (remq nil program-args))
  (piem--process-go
   dir program program-args
   (lambda ()
     (setq piem--buffer-process
          (apply #'start-process
                 (file-name-nondirectory program)
                 (current-buffer)
                 program program-args)))))

(defun piem-process-call (dir program &rest program-args)
  (setq program-args (remq nil program-args))
  (piem--process-go
   dir program program-args
   (lambda ()
     (unless (= 0 (apply #'call-process program nil t nil program-args))
       (signal 'piem-process-error
               (list (format "%s call in %s failed"
                             program default-directory)))))))


;;;; Extractors

(defun piem-inbox ()
  "Return the current buffer's inbox."
  (run-hook-with-args-until-success 'piem-get-inbox-functions))

(defun piem-inbox-coderepo ()
  "Return the code repository of current buffer's inbox."
  (when-let ((p (piem-inbox))
             (repo (plist-get (cdr (assoc p piem-inboxes)) :coderepo)))
    (expand-file-name repo)))

(defun piem-inbox-coderepo-maybe-read ()
  "Like `piem-inbox-coderepo', but fall back to reading the repo."
  (or (piem-inbox-coderepo)
      (and (fboundp 'projectile-relevant-known-projects)
           (completing-read
            "Project: "
            (projectile-relevant-known-projects)))
      (and piem-use-magit
           (fboundp 'magit-read-repository)
           (magit-read-repository))
      (read-directory-name "Git repository: ")))

(defun piem-mid ()
  "Return the current buffer's message ID."
  (run-hook-with-args-until-success 'piem-get-mid-functions))


;;;; Patch handling

(defun piem-extract-mbox-info (&optional buffer)
  "Collect information from message in BUFFER.
If BUFFER is nil, the current buffer is used.  Any message after
the first will be ignored."
  (with-current-buffer (or buffer (current-buffer))
    (let ((info (save-excursion
                  (save-restriction
                    (message-narrow-to-head)
                    (list :date (message-fetch-field "date")
                          :from (rfc2047-decode-string
                                 (message-fetch-field "from"))
                          :subject (message-fetch-field "subject"))))))
      (when (re-search-forward (rx line-start "base-commit: "
                                   (group (>= 40 hex-digit))
                                   line-end)
                               nil t)
        (plist-put info :base-commit (match-string 1)))
      info)))

(defun piem--shorten-subject (subject)
  (let ((words
         (mapcar #'downcase
                 (split-string
                  (thread-last subject
                    (replace-regexp-in-string
                     (rx (any "'\""))
                     "")
                    (replace-regexp-in-string
                     (rx string-start (zero-or-more space) "["
                         (zero-or-more (not (any "]" "\n")))
                         "PATCH"
                         (zero-or-more (not (any "]" "\n")))
                         "]" (one-or-more space))
                     ""))
                  "\\W+" t)))
        (ignore-these (list "a" "an" "the"))
        (num-words 0)
        (num-chars 0)
        kept)
    (catch 'stop
      (dolist (word words)
        (when (not (member word ignore-these))
          (cl-incf num-words)
          (cl-incf num-chars (length word))
          (push word kept)
          (when (or (> num-words 5)
                    (> num-chars 20))
            (throw 'stop nil)))))
    (mapconcat #'identity (reverse kept) "-")))

(defun piem-name-branch-who-what-v (info)
  "Construct a branch name like \"km/b4-short-subj__v3\".

In the above example, \"k\" and \"m\" are the first letters of
the first and second \"words\" in the \"From:\" field.
\"b4-short-subj\" is a stripped down, truncated variant of
\"Subject:\".  And \"v3\" is the version of the patch series, as
indicated in the subject.

INFO is a plist with properties documented
in `piem-default-branch-function'."
  (when-let ((sender (car (mail-extract-address-components
                           (plist-get info :from))))
             (subject (plist-get info :subject)))
    (let* ((subnames (split-string sender))
           (initials (mapconcat
                      (lambda (subname)
                        (and subname
                             (downcase (substring subname nil 1))))
                      (list (car subnames) (cadr subnames))
                      ""))
           (version (and (string-match
                          (rx "[" (zero-or-more (not (any "[" "\n")))
                              "PATCH "
                              (zero-or-one (group "v" (one-or-more digit))))
                          subject)
                         (match-string 1 subject))))
      (concat initials "/"
              (piem--shorten-subject subject)
              (and version (concat "__" version))))))

(defun piem-am (mbox &optional info coderepo)
  (let* ((default-directory (or coderepo default-directory)))
    ;; TODO: Optionally do more through Magit.
    (let ((new-branch (read-string
                       "New branch (empty for detached): "
                       (funcall piem-default-branch-function info)))
          (base (completing-read
                 "Base commit: "
                 (let ((cands (and piem-use-magit
                                   (fboundp 'magit-list-local-branch-names)
                                   (magit-list-local-branch-names)))
                       (base (plist-get info :base-commit)))
                   (if base (cons base cands) cands)))))
      (apply #'piem-process-call nil piem-git-executable "checkout"
             (append (if (string-empty-p new-branch)
                         (list "--detach")
                       (list "-b" new-branch))
                     (and (not (string-blank-p base))
                          (list base)))))
    (piem-process-call nil piem-git-executable "am" "--scissors" mbox)
    (if (and piem-use-magit
             (fboundp 'magit-status-setup-buffer))
        (magit-status-setup-buffer)
      (dired "."))))



(defun piem-please ()
  "How I wish I could intersect my emails, feeds, and repos")

;;; piem.el ends here
(provide 'piem)
