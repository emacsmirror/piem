;;; piem.el --- Emacs tools for working with public-inbox  -*- lexical-binding: t; -*-

;; Copyright all piem contributors <piem@inbox.kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; Keywords: mail, news, tools, vc
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.2"))
;; Homepage: https://git.kyleam.com/piem/about/

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

;; This library provides functionality for working with public-inbox
;; archives [1] within Emacs (e.g., applying a patch series to the
;; associated Git repository or downloading an mbox for the thread and
;; injecting it into a local Maildir directory).  The user option
;; `piem-inboxes' defines a set of public-inbox archives.  For most
;; the functions here to do something useful, the current buffer needs
;; to be linked to an inbox in that list, which requires enabling the
;; minor mode provided by at least one integration library (such as
;; `piem-gnus', `piem-eww', or `piem-notmuch').
;;
;; [1] https://public-inbox.org/README

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'mail-extr)
(require 'mail-parse)
(require 'message)
(require 'mm-decode)
(require 'piem-maildir)
(require 'subr-x)
(require 'transient)
(require 'url)
(require 'url-handlers)
(require 'url-http)


;;;; Options

(defgroup piem ()
  "Emacs tools for working with public-inbox archives."
  :link '(info-link "(piem)Top")
  :group 'tools)

(defcustom piem-inboxes nil
  "List of public-inbox-archived projects.

Elements have the form (NAME . INFO), where INFO is a property
list that supports the following properties:

  :address
  :listid
      The email address and List-ID for the inbox.
  :coderepo
      Local path to the code repository associated with the inbox.
      This value should point to the working tree.  Note that this
      differs from public-inbox-config's coderepo, which is a link to
      another section that must point to the repository's git
      directory.

      To map an inbox to multiple repositories, you can set this
      to a list of strings.  When a function (e.g., `piem-am')
      needs a single coderepo, you will be prompted to select a
      repository, with the first value in this list offered as
      the default.
  :url
      A URL hosting HTTPS archives.
  :maildir
      A Maildir directory to inject messages into.
  :gnu-package
      A GNU Bug Tracker label to match with for the inbox.

Here's an example for the public-inbox project itself:

    (\"public-inbox\"
     :coderepo \"~/src/public-inbox/\"
     :address \"meta@public-inbox.org\"
     :listid \"meta.public-inbox.org\"
     :url \"https://public-inbox.org/meta/\"
     :maildir \"~/.local/share/mail/.lists.mail.public-inbox\")

Inboxes may also be fully or partially defined via public-inbox
configuration when `piem-get-inboxes-from-config' is non-nil.
Prefer the function `piem-merged-inboxes' over inspecting
`piem-inboxes' directly so that values from the public-inbox
configuration are considered."
  ;; Note: When adding a property to the list above, update
  ;; `piem--merge-config-inboxes' so that the value can also be set in
  ;; ~/.public-inbox/config.
  :type '(alist :key-type string
                :value-type plist)
  :set (lambda (var val)
         (set var val)
         (when (fboundp 'piem-clear-merged-inboxes)
           (piem-clear-merged-inboxes))))

(defcustom piem-get-inboxes-from-config nil
  "Whether to construct inboxes from public-inbox's configuration.

If any inboxes are mirrored locally, some of the information in
the option `piem-inboxes' may already be present in
~/.public-inbox/config.  When this option is non-nil, the
function `piem-merged-inboxes' combines these two sources, with
values from `piem-inboxes' taking precedence.  For details, see
Info node `(piem) Registering inboxes'.

If you change the option `piem-inboxes' outside of the
`customize' interface or change public-inbox's configuration, the
merged representation can be updated by calling
`piem-clear-merged-inboxes'."
  :package-version '(piem . "0.3.0")
  :type 'boolean)

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
mbox's contents (in mboxrd format) in the current buffer."
  :type 'hook)

(defcustom piem-am-ready-mbox-functions nil
  "Functions tried to get an mbox to be fed to `git am'.
The functions are called with no arguments.  If a function knows
how to create an mbox, it should return a function that takes no
arguments and inserts the mbox's contents in the current buffer.
The return value can also be (FUNCTION . FORMAT), where FORMAT is
either \"mbox\" or \"mboxrd\" and maps to the --patch-format
value passed to `git am'.  If unspecified, \"mboxrd\" is used."
  :type 'hook)

(defcustom piem-add-message-id-header nil
  "Whether to add Message-ID header to non-mail patches.
If this value is non-nil and a patch returned by a function in
`piem-am-ready-mbox-functions' looks like a patch that was
attached rather than sent inline, add a Message-ID header with
the return value of `piem-mid'."
  :type 'boolean)

(defcustom piem-git-executable
  (or (and (boundp 'magit-git-executable) magit-git-executable)
      "git")
  "Which git executable to use."
  :type 'string)

(defcustom piem-use-magit t
  "Whether to use Magit for some user-facing operations."
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

(defcustom piem-am-create-worktree nil
  "Whether to create a dedicated worktree for applying patches."
  :type 'boolean)

(defcustom piem-am-read-worktree-function
  #'piem-am-read-worktree-sibling-named-by-branch
  "Function that reads a to-be-created worktree from the user.
This function is called with two arguments, the directory of the
code repository that the worktree will be created from and the
name of the branch that will be created.  The branch may be nil
if the caller requested a detached HEAD."
  :type 'function)

(defcustom piem-maildir-directory nil
  "Default directory to inject public-inbox threads into.

If non-nil, this must be an existing Maildir directory.  This can be
overriden on a per-list basis by using the \":maildir\" keyword in
`piem-inboxes'."
  :type 'string)

(defcustom piem-mail-injection-skipif-predicate nil
  "Predicate to decide whether to skip injecting a message.

The function is called with the message ID (no surrounding
brackets) within a buffer that is narrowed to the message.  The
function does not need to worry about saving point.  A non-nil
return value signals that `piem-inject-thread-into-maildir'
should skip the message.

Notmuch users can use `piem-notmuch-known-mid-p' as the predicate
to skip messages that are already in the Notmuch database."
  :type 'function)

(defcustom piem-after-mail-injection-functions nil
  "Functions called after writing messages to `piem-maildir-directory'.
Functions should accept one argument, the message ID given to
`piem-inject-thread-into-maildir'."
  :type 'hook)

(defcustom piem-browse-url-browser-function nil
  "Overriding value for `browse-url-browser-function'.

public-inbox's HTTP interface is well suited for browsers like
w3m and EWW, allowing you to stay in Emacs rather than launch an
external browser.  However, assuming you have `browse-url'
configured to usually go through an external browser, sending
public-inbox URLs through, say, EWW would require you to
configure `browse-url-browser-function' with a regular expression
for each inbox URL that you want to be handled by
`eww-browse-url'.

Instead, you can simply set this option to `eww-browse-url' (or
anything else `browse-url-browser-function' accepts), and piem
will use it when calling `browse-url'.

When this option is nil, piem calls `browse-url' without
overriding the value of `browse-url-browser-function'."
  :type (append
         '(choice
           (const :tag "Don't override `browse-url-browser-function'" nil))
         (cdr (get 'browse-url-browser-function 'custom-type))))


;;;; Subprocess handling

(defvar piem-process-mode-font-lock-keywords
  `((,(rx line-start
          ";;; " (or "process" "directory" "time")
          ":" (one-or-more not-newline)
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
      (goto-char (point-max))
      (display-buffer buffer)
      (let ((inhibit-read-only t))
        (insert (format "
%s
;;; process: %S
;;; directory:  %s
;;; time: %s
"
                        (char-to-string 12) ; form feed
                        (cons program program-args)
                        default-directory
                        (format-time-string "%FT%T%z")))
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

(defun piem-process-call-with-buffer-input
    (dir buffer program &rest program-args)
  (setq program-args (remq nil program-args))
  (piem--process-go
   dir program program-args
   (lambda ()
     (let ((outdir default-directory)
           (outbuf (current-buffer)))
       (with-current-buffer buffer
         (let ((default-directory outdir))
           (unless (= 0 (apply #'call-process-region nil nil
                               program nil outbuf nil program-args))
             (signal 'piem-process-error
                     (list (format "%s call in %s failed"
                                   program default-directory))))))))))


;;;; Integration with public-inbox configuration

(defun piem--git-config-list (&optional file)
  "Return a hash table that maps git-config keys to values.

The value for each key is a list in the order that would be
reported by `git config --get-all $key'.  The first value has the
highest precedence (i.e. what would be reported by `git config
--get $key').

If FILE is non-nil, report configuration values from that file.
Otherwise report values from all standard Git configuration
files."
  (with-temp-buffer
    (unless (= 0 (apply #'call-process piem-git-executable nil '(t nil) nil
                        "config" "--list" "-z"
                        (append (and file (list "--file" file)))))
      (error "git-config call failed"))
    (goto-char (point-min))
    (let ((cfg (make-hash-table :test #'equal)))
      (while (not (eobp))
        (let* ((key-end (line-end-position))
               (key (buffer-substring (point) key-end))
               (value (progn (skip-chars-forward "^\0")
                             (buffer-substring (1+ key-end) (point)))))
          (puthash key (cons value (gethash key cfg)) cfg)
          (forward-char 1)))
      cfg)))

(defvar piem--inboxes 'unset)

(defun piem-public-inbox-config-file ()
  "Return public-inbox's configuration file."
  (or (getenv "PI_CONFIG")
      (expand-file-name "~/.public-inbox/config")))

(defun piem--merge-config-inboxes ()
  (let ((cfg-file (piem-public-inbox-config-file)))
    (if (not (file-readable-p cfg-file))
        (setq piem--inboxes piem-inboxes)
      (let ((case-fold-search t)
            (pi-cfg (piem--git-config-list cfg-file))
            cfg-inboxes)
        (maphash
         (lambda (key val)
           (when (string-match
                  (rx string-start "publicinbox."
                      (group (one-or-more not-newline)) "."
                      (group
                       (or "address" "coderepo"
                           "listid" "maildir"
                           "url" "gnu-package"))
                      string-end)
                  key)
             (let* ((inbox-name (match-string 1 key))
                    (inbox-item (assoc inbox-name cfg-inboxes))
                    (prop-name (intern (concat ":" (match-string 2 key))))
                    (prop-pair (list prop-name
                                     (if (eq prop-name :coderepo)
                                         val
                                       (car val)))))
               (when-let* ((coderepos
                            (and (eq prop-name :coderepo)
                                 (mapcar
                                  (lambda (v)
                                    (car (gethash (format "coderepo.%s.dir" v)
                                                  pi-cfg)))
                                  val))))
                 (setq prop-pair
                       (list :coderepo
                             (mapcar
                              (lambda (r)
                                (replace-regexp-in-string "/\\.git/?\\'" "" r))
                              coderepos))))
               (if inbox-item
                   (setcdr inbox-item (nconc prop-pair (cdr inbox-item)))
                 (push (cons inbox-name prop-pair) cfg-inboxes)))))
         pi-cfg)
        (let (merged)
          (dolist (name (delete-dups
                         (mapcar #'car (append cfg-inboxes piem-inboxes))))
            (push (append (list name)
                          (alist-get name piem-inboxes nil nil #'equal)
                          (alist-get name cfg-inboxes nil nil #'equal))
                  merged))
          (setq piem--inboxes merged))))))

(defun piem-merged-inboxes ()
  "Return list of inboxes.

This list has the same form as described in the option
`piem-inboxes'.

If `piem-get-inboxes-from-config' is non-nil, the return value is
constructed by merging inboxes defined in public-inbox's
configuration with the inboxes from `piem-inboxes', with the
values in the latter taking precedence.  If
`piem-get-inboxes-from-config' is nil, this value matches
`piem-inboxes'."
  (if (not piem-get-inboxes-from-config)
      piem-inboxes
    (when (eq piem--inboxes 'unset)
      (piem--merge-config-inboxes))
    piem--inboxes))

(defun piem-clear-merged-inboxes ()
  "Clear inboxes cached by `piem-merged-inboxes'.
When `piem-get-inboxes-from-config' is set to a non-nil value,
`piem-merged-inboxes' constructs a set of inboxes by merging
`piem-inboxes' and public-inbox's configuration and then caches
the result.  If you change `piem-inboxes' outside of the
customize interface or change public-inbox's configuration, call
this command to clear the cached value."
  (interactive)
  (setq piem--inboxes 'unset))


;;;; Extractors

(defun piem--ensure-trailing-slash (s)
  (if (string-suffix-p "/" s) s (concat s "/")))

(defun piem-message-link-re (url &optional mid)
  "Return a regular expression matching a public-inbox url.
URL should be the top-level url for the inbox.  If MID is
non-nil, make the match specific for that message."
  (rx-to-string
   `(and ,(piem--ensure-trailing-slash url)
         (group ,(or (and mid (piem-escape-mid mid))
                     '(one-or-more (not (any "/" "\n")))))
         "/" (group (zero-or-one
                     (or "raw"
                         "t.mbox.gz"
                         (and (or "t" "T") "/#"
                              (one-or-more (not (any "/" "\n")))))))
         string-end)))

(defun piem--message-fetch-decoded-fields (headers)
  (save-excursion
    (save-restriction
      (message-narrow-to-head)
      (mapcar (lambda (header)
                (when-let* ((val (message-fetch-field header)))
                  (mail-decode-encoded-word-string val)))
              headers))))

(defun piem-inbox-by-gnu-package-match (gnu-package)
  "Return inbox based on matching :gnu-package properties.
GNU-PACKAGE should be a string.  This function is intended to be
used by libraries implementing a function for
`piem-get-inbox-function'."
  (when gnu-package
    (catch 'hit
      (dolist (inbox (piem-merged-inboxes))
        (let* ((info (cdr inbox))
               (p-package (plist-get info :gnu-package)))
          (when (and gnu-package
                     p-package
                     (string-equal (downcase p-package)
                                   (downcase gnu-package)))
            (throw 'hit (car inbox))))))))

(defun piem-inbox-by-header-match ()
  "Return inbox based on matching message headers.
This should be called from a buffer containing a message and is
intended to be used by libraries implementing a function for
`piem-get-inbox-functions'."
  (pcase-let ((`(,listid ,to ,cc ,gnu-package)
               (piem--message-fetch-decoded-fields
                '("list-id" "to" "cc" "x-gnu-pr-package"))))
    (or (catch 'hit
          (dolist (inbox (piem-merged-inboxes))
            (let* ((info (cdr inbox))
                   (p-listid (plist-get info :listid)))
              (when (and listid
                         p-listid
                         (string-match-p
                          (concat "<" (regexp-quote p-listid) ">")
                          listid))
                (throw 'hit (car inbox)))
              (when-let* ((addr (plist-get info :address))
                          (to (mapconcat #'identity (list to cc)
                                         " ")))
                (when (string-match-p (regexp-quote addr) to)
                  (throw 'hit (car inbox)))))))
        (piem-inbox-by-gnu-package-match gnu-package))))

(defun piem-inbox ()
  "Return the current buffer's inbox."
  (run-hook-with-args-until-success 'piem-get-inbox-functions))

(defun piem-inbox-get (key &optional inbox)
  "Return value of KEY associated with INBOX.
The key-value pair may be defined in `piem-inboxes' or
public-inbox's configuration.  If INBOX is nil, use the inbox
returned by `piem-inbox'."
  (when-let* ((p (or inbox (piem-inbox))))
    (plist-get (cdr (assoc p (piem-merged-inboxes))) key)))

(defun piem-inbox-url (&optional inbox)
  "Return URL associated with INBOX.
If INBOX is nil, use the inbox returned by `piem-inbox'."
  (when-let* ((inbox (or inbox (piem-inbox)))
              (url (piem-inbox-get :url inbox)))
    (piem--ensure-trailing-slash url)))

(defun piem-inbox-coderepo (&optional inbox)
  "Return the code repository of current buffer's inbox."
  (when-let* ((inbox (or inbox (piem-inbox)))
              (repos (piem-inbox-get :coderepo inbox)))
    (when (stringp repos)
      (setq repos (list repos)))
    (let ((repo (if (= (length repos) 1)
                    (car repos)
                  (completing-read "Code repository: " repos
                                   nil t nil nil (car repos)))))
      (unless (string-blank-p repo)
        (file-name-as-directory (expand-file-name repo))))))

(defun piem-inbox-maildir-directory (&optional inbox)
  "Return the maildir for INBOX.

If INBOX is nil, use the inbox returned by `piem-inbox'.  If the
INBOX doesn't have a maildir configured (via `piem-inboxes' or
public-inbox's configuration), return the value of
`piem-maildir-directory'."
  (or (piem-inbox-get :maildir inbox)
      piem-maildir-directory))

(defun piem-inbox-by-url-match (url)
  "Return inbox based on matching URL against `:url'."
  (setq url (piem--ensure-trailing-slash url))
  (catch 'hit
    (dolist (inbox (piem-merged-inboxes))
      (when-let* ((info (cdr inbox))
                  (p-url (plist-get info :url)))
        (setq p-url (piem--ensure-trailing-slash p-url))
        (when (string-match-p (regexp-quote p-url) url)
          (throw 'hit (car inbox)))))))

(defun piem-inbox-coderepo-maybe-read ()
  "Like `piem-inbox-coderepo', but fall back to reading the repo."
  (let ((repo
         (or (piem-inbox-coderepo)
             (and (bound-and-true-p projectile-known-projects)
                  (expand-file-name
                   (completing-read
                    "Project: "
                    projectile-known-projects nil t nil nil
                    (when-let* ((current (and (fboundp 'projectile-project-root)
                                              (projectile-project-root))))
                      (abbreviate-file-name current)))))
             (and (bound-and-true-p project-list-file)
                  (file-exists-p project-list-file)
                  (fboundp 'project-prompt-project-dir)
                  (expand-file-name
                   (project-prompt-project-dir)))
             (and piem-use-magit
                  (fboundp 'magit-read-repository)
                  (magit-read-repository))
             (read-directory-name "Git repository: "))))
    (if (equal repo "")
        (user-error "No code repository specified")
      (file-name-as-directory repo))))

(defun piem-mid ()
  "Return the current buffer's message ID."
  (run-hook-with-args-until-success 'piem-get-mid-functions))

(defun piem--insert-message-id-header (mid)
  ;; Be strict about case because this is coming from
  ;; git-format-patch.
  (let ((case-fold-search nil))
    (while (re-search-forward
            (rx line-start
                "From " (>= 40 hex-digit) " Mon Sep 17 00:00:00 2001"
                line-end)
            nil t)
      (catch 'has-message-id
        (let ((header-count 0))
          (while (and (= (forward-line) 0)
                      (not (looking-at-p
                            (rx line-start (zero-or-one space) line-end))))
            (cond
             ((looking-at-p
               ;; git-format-patch switched to "Message-ID" spelling
               ;; in v2.41.
               (rx line-start
                   "Message-" (or "Id" "ID")
                   ": <" (one-or-more not-newline) ">"
                   line-end))
              (throw 'has-message-id nil))
             ((looking-at-p
               (rx line-start (or "From" "Date" "Subject") ": "
                   not-newline))
              (cl-incf header-count))))
          (when (= header-count 3)
            ;; Found all the expected headers before hitting a
            ;; blank line.  Assume we're in a header.
            (insert (format "Message-ID: <%s>\n" mid))))))))

(defun piem-am-ready-mbox (&optional buffer-name)
  "Generate a buffer containing an am-ready mbox.
The return value is (BUFFER . FORMAT), where FORMAT is either
\"mbox\" or \"mboxrd\".  Callers are responsible for killing the
buffer.

By default the buffer name is hidden, but when BUFFER-NAME is
non-nil, use that name instead."
  (when-let* ((res (run-hook-with-args-until-success
                    'piem-am-ready-mbox-functions)))
    (pcase-let ((buffer (generate-new-buffer
                         (or buffer-name " *piem am-ready mbox*")))
                (`(,fn . ,format)
                 (if (member (cdr-safe res) '("mbox" "mboxrd"))
                     res
                   (cons res "mboxrd")))
                (mid (and piem-add-message-id-header (piem-mid)))
                (has-content nil))
      (with-current-buffer buffer
        (funcall fn)
        (setq has-content (< 1 (point-max)))
        (when (and has-content mid)
          (goto-char (point-min))
          (piem--insert-message-id-header mid)))
      (if has-content
          (cons buffer format)
        (kill-buffer buffer)
        nil))))


;;;; Link handling

(defconst piem--unreserved-chars
  (append url-unreserved-chars
          ;; These extra characters follow what's used by
          ;; public-inbox's mid_escape().
          (list ?! ?$ ?& ?' ?\( ?\) ?* ?+ ?, ?= ?: ?\; ?@)))

(defun piem-escape-mid (mid)
  "Escape MID for use in path part of a public-inbox URL."
  (url-hexify-string mid piem--unreserved-chars))

(defun piem-mid-url (mid &optional inbox)
  "Return a public-inbox URL for MID.
The URL for INBOX may be defined in `piem-inboxes' or
public-inbox's configuration.  If INBOX is nil, use the inbox
returned by `piem-inbox'."
  (concat
   (or (piem-inbox-url inbox)
       (user-error "Couldn't find URL for %s"
                   (or inbox "current buffer")))
   (piem-escape-mid mid)))

;;;###autoload
(defun piem-copy-mid-url (&optional browse)
  "Copy public-inbox URL for the current buffer's message.
With prefix argument BROWSE, call `browse-url' on the URL
afterwards.  If `piem-browse-url-browser-function' is non-nil, it
is used as the value of `browse-url-browser-function'."
  (interactive "P")
  (let ((url (piem-mid-url
              (or (piem-mid)
                  (user-error "No message ID found for the current buffer"))
              (piem-inbox))))
    (prog1
        (kill-new (message "%s" url))
      (when browse
        (let ((browse-url-browser-function
               (or piem-browse-url-browser-function
                   browse-url-browser-function)))
          (browse-url url))))))


;;;; Download helpers

(defvar piem--has-gunzip)
(defun piem-gunzip-buffer ()
  (unless (if (boundp 'piem--has-gunzip)
              piem--has-gunzip
            (setq piem--has-gunzip (executable-find "gunzip")))
    (user-error "gunzip executable not found"))
  (goto-char (point-min))
  (unless (= 0 (call-process-region nil nil "gunzip" nil t))
    (error "Decompressing buffer failed"))
  (delete-region (point) (point-max))
  (goto-char (point-min)))

(defmacro piem-with-url-contents (url &rest body)
  "Insert URL contents literally into temporary buffer and evaluate BODY."
  (declare (indent 1) (debug t))
  (let ((u (gensym "url")))
    `(with-temp-buffer
       (set-buffer-multibyte nil)
       ;; This mostly copies `url-insert-file-contents', but it embeds
       ;; `url-http--insert-file-helper' and uses `url-insert' rather
       ;; than `url-insert-buffer-contents' to insert the contents
       ;; literally.
       (let* ((,u ,url)
              (buffer (url-retrieve-synchronously ,u)))
         (unless buffer (signal 'file-error (list ,u "No Data")))
         ;; This error handling follows what's in
         ;; `url-http--insert-file-helper'.
         (with-current-buffer buffer
           (when (bound-and-true-p url-http-response-status)
             (unless (or (and (>= url-http-response-status 200)
                              (< url-http-response-status 300))
                         (= url-http-response-status 304))
               (let ((desc (nth 2 (assq url-http-response-status url-http-codes))))
                 (kill-buffer buffer)
                 (signal 'file-error (list ,u desc))))))
         (url-insert buffer))
       (goto-char (point-min))
       ,@body)))


;;;; Maildir injection

(defun piem--write-mbox-to-maildir (maildir-directory)
  (let ((n-added 0)
        (n-skipped 0))
    (while (and (not (eobp))
                (re-search-forward "^From mboxrd@z" nil t))
      (let* ((beg (line-beginning-position 2))
             (end (or (and (re-search-forward "^From mboxrd@z" nil t)
                           (progn (goto-char (line-beginning-position 0))
                                  (point-marker)))
                      (point-max-marker)))
             (basename (piem-maildir-make-uniq-maildir-id))
             (tmpfile (concat maildir-directory "/tmp/" basename)))
        (goto-char beg)
        (if (and (functionp piem-mail-injection-skipif-predicate)
                 (save-excursion
                   (save-restriction
                     (narrow-to-region beg end)
                     (funcall piem-mail-injection-skipif-predicate
                              (replace-regexp-in-string
                               "\\`<\\(.*\\)>\\'" "\\1"
                               (or (save-restriction
                                     (save-excursion
                                       (message-narrow-to-head-1)
                                       (message-fetch-field "message-id" t)))
                                   (error "Message lacks Message-ID header")))))))
            (cl-incf n-skipped)
          (let ((case-fold-search nil))
            (while (re-search-forward
                    (rx line-start ">" (group (zero-or-more ">") "From "))
                    end t)
              (replace-match "\\1" t)))
          (write-region beg end tmpfile nil nil nil 'excl)
          (piem-maildir-move-tmp-to-new maildir-directory
                                        basename)
          (delete-file tmpfile)
          (cl-incf n-added))
        (goto-char end)))
    (cons n-added n-skipped)))

;;;###autoload
(defun piem-inject-thread-into-maildir (mid &optional inbox message-only)
  "Inject thread containing MID into `piem-inbox-maildir-directory'.

Download the message from the :url value configured for INBOX in
`piem-inboxes'.  INBOX is passed to
`piem-inbox-maildir-directory' to determine where to inject the
message.  If INBOX is nil, the inbox returned by `piem-inbox' is
used.

If prefix argument MESSAGE-ONLY is non-nil, inject just the
message for MID, not the entire thread."
  (interactive
   (list (or (piem-mid)
             (user-error "No message ID found for the current buffer"))
         nil
         current-prefix-arg))
  (let* ((inbox (or inbox (piem-inbox)))
         (maildir-directory (piem-inbox-maildir-directory inbox)))
    (cond
     ((not maildir-directory)
      (user-error "No directory returned by `piem-inbox-maildir-directory'"))
     ((not (piem-maildir-dir-is-maildir-p maildir-directory))
      (user-error
       "Does not look like a Maildir directory: %s" maildir-directory)))
    (let ((url (concat (piem-mid-url mid inbox)
                       (if message-only "/raw" "/t.mbox.gz"))))
      (piem-with-url-contents url
        (unless message-only
          (piem-gunzip-buffer))
        (pcase-let ((`(,added-count . ,skipped-count)
                     (piem--write-mbox-to-maildir maildir-directory)))
          (message "Added %d message%s%s for %s to %s"
                   added-count
                   (if (= added-count 1) "" "s")
                   (if (> skipped-count 0)
                       (format " (skipping %d)" skipped-count)
                     "")
                   mid
                   (abbreviate-file-name maildir-directory))))
      (run-hook-with-args 'piem-after-mail-injection-functions mid))))


;;;; Patch handling

(defun piem-extract-mbox-info (&optional buffer)
  "Collect information from message in BUFFER.
If BUFFER is nil, the current buffer is used.  Any message after
the first will be ignored."
  (with-current-buffer (or buffer (current-buffer))
    (let ((info (cl-mapcan #'list '(:date :from :subject)
                           (piem--message-fetch-decoded-fields
                            '("date" "from" "subject")))))
      (when (re-search-forward (rx line-start "base-commit: "
                                   (group (>= 40 hex-digit))
                                   line-end)
                               nil t)
        (plist-put info :base-commit (match-string 1)))
      info)))

(defvar piem-patch-subject-re
  (rx (zero-or-more space) "["
      (zero-or-more (not (any "]" "\n")))
      "PATCH"
      (zero-or-more (not (any "]" "\n")))
      "]" (one-or-more space)))

(defun piem--shorten-subject (subject)
  (let ((words
         (mapcar #'downcase
                 (split-string
                  (thread-last subject
                    (replace-regexp-in-string
                     (rx (any "'\""))
                     "")
                    (replace-regexp-in-string
                     piem-patch-subject-re ""))
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
  (when-let* ((from (plist-get info :from))
              (sender (let ((mail-extr-ignore-single-names nil)
                            (mail-extr-ignore-realname-equals-mailbox-name nil))
                        (car (mail-extract-address-components from))))
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

(defvar piem-am-args (list "--scissors" "--3way"))

(defun piem-am-read-worktree-sibling-named-by-branch (coderepo branch)
  "Read a worktree to create for applying patches.
This function is intended to be used as a value of
`piem-am-read-worktree-function'.  The worktree directory is
completed from the parent directory of CODEREPO.  If BRANCH is
non-nil, it is used to construct the default completion value."
  (let ((fname (directory-file-name coderepo)))
    (read-directory-name
     "Create worktree: "
     (file-name-directory fname) nil nil
     (and branch
          (concat (file-name-nondirectory fname) "-"
                  (replace-regexp-in-string "/" "-" branch))))))

;;;###autoload
(defun piem-am (mbox &optional format info coderepo toggle-worktree)
  "Feed an am-ready mbox to `git am'.

MBOX is a buffer whose contents are an am-ready mbox (obtained
via `piem-am-ready-mbox' when called interactively).  FORMAT
controls the value passed as the --patch-format option of `git
am'.  \"mbox\" and \"mboxrd\" are valid values, and \"mboxrd\" is
the default.

INFO is a plist that with information to help choose a default
branch name or starting point (see `piem-default-branch-function'
for a list of possible properties).

CODEREPO, if given, indicates the code repository to operate
within.  If not specified, the default directory is used.

When prefix argument TOGGLE-WORKTREE is non-nil, invert the
meaning of `piem-am-create-worktree'.  With the default value,
this triggers the creation of a new worktree."
  (interactive
   (pcase-let ((`(,mbox . ,format)
                (or (piem-am-ready-mbox)
                    (user-error
                     "Could not find am-ready mbox for current buffer"))))
     ;; We're responsible for cleaning up the buffer created by
     ;; `piem-am-ready-mbox'; sneak in an indication to let the
     ;; downstream code know.
     (list (cons :interactive mbox)
           format
           (piem-extract-mbox-info mbox)
           (piem-inbox-coderepo-maybe-read)
           current-prefix-arg)))
  (setq format (or format "mboxrd"))
  (let* ((default-directory (or coderepo default-directory))
         (am-directory default-directory)
         (use-worktree (xor piem-am-create-worktree toggle-worktree))
         (interactivep (eq (car-safe mbox) :interactive)))
    (when interactivep
      (setq mbox (cdr mbox)))
    (let ((new-branch
           (let ((b (read-string "New branch (empty for detached): "
                                 (funcall piem-default-branch-function info))))
             (and (not (string-empty-p b)) b)))
          (base (completing-read
                 "Base commit: "
                 (let ((cands (and piem-use-magit
                                   (fboundp 'magit-list-local-branch-names)
                                   (magit-list-local-branch-names)))
                       (base (plist-get info :base-commit)))
                   (if base (cons base cands) cands)))))
      (when use-worktree
        (setq am-directory
              (expand-file-name
               (funcall piem-am-read-worktree-function
                        default-directory new-branch)))
        (when (file-exists-p am-directory)
          (user-error "Worktree directory already exists")))
      (apply #'piem-process-call nil piem-git-executable
             (append (if use-worktree
                         (list "worktree" "add")
                       (list "checkout"))
                     (if new-branch (list "-b" new-branch) (list "--detach"))
                     (and use-worktree (list am-directory))
                     (and (not (string-blank-p base))
                          (list base)))))
    (let ((args (cons (concat "--patch-format=" format)
                      piem-am-args)))
      (if (bufferp mbox)
          (unwind-protect
              (apply #'piem-process-call-with-buffer-input
                     am-directory mbox piem-git-executable "am" args)
            (when interactivep
              (kill-buffer mbox)))
        (apply #'piem-process-call am-directory piem-git-executable "am"
               (append args (list mbox)))))
    (if (and piem-use-magit
             (fboundp 'magit-status-setup-buffer))
        (magit-status-setup-buffer am-directory)
      (dired am-directory))))

(defvar-local piem-edit-patch--coderepo nil)
(defvar-local piem-edit-patch--format nil)

(defun piem-edit ()
  "Edit an am-ready mbox before feeding it to `git am'."
  (interactive)
  (pcase-let ((`(,mbox . ,format)
               (or (piem-am-ready-mbox "*piem-edit-patch*")
                   (user-error
                    "Could not find am-ready mbox for current buffer")))
              (coderepo (piem-inbox-coderepo)))
    (pop-to-buffer mbox)
    (piem-edit-patch-mode)
    (setq piem-edit-patch--coderepo coderepo)
    (setq piem-edit-patch--format format)))

(defun piem-edit-patch-am ()
  "Apply the patch that is currently edited."
  (interactive)
  (let ((buf (current-buffer)))
    (piem-am buf
             piem-edit-patch--format
             (piem-extract-mbox-info (current-buffer))
             piem-edit-patch--coderepo)
    (kill-buffer buf)))

(defvar piem-edit-patch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'piem-edit-patch-am)
    map)
  "Keymap for editing patches with piem.")

(define-derived-mode piem-edit-patch-mode text-mode "piem-edit-patch"
  "Major mode for editing patches with piem."
  :group 'piem
  (buffer-enable-undo))


;;;; Dispatch

;;;###autoload (autoload 'piem-dispatch "piem" nil t)
(transient-define-prefix piem-dispatch ()
  "Invoke a piem command."
  [[("a" "apply patch" piem-am)
    ("b" "call b4-am" piem-b4-am)]
   [("i" "inject thread into maildir" piem-inject-thread-into-maildir)
    ("l" "copy public-inbox link" piem-copy-mid-url)]
   [("s" "search with lei" piem-lei-q)]])



(defun piem-please ()
  "How I wish I could intersect my emails, feeds, and repos")

(provide 'piem)
;;; piem.el ends here
