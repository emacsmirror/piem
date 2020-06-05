;;; piem-b4.el --- Emacs interface to the b4 tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>
;; Keywords: vc, tools
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.3") (transient "0.2.0"))

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

(require 'cl-lib)
(require 'mail-extr)
(require 'message)
(require 'piem)
(require 'subr-x)
(require 'transient)


;;;; Options

(defgroup piem-b4 nil
  "Control the b4 tool from Emacs."
  :link '(info-link "(piem)b4 integration")
  :group 'piem)

(defcustom piem-b4-b4-executable "b4"
  "Which b4 executable to use."
  :type 'string)



(defcustom piem-b4-default-branch-function
  #'piem-b4-name-branch-who-what-v
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
  :type 'boolean)


;;;; Internals

(defconst piem-b4-output-buffer "*piem-b4-output*")

(defun piem-b4--series-info (cover patches)
  "Collect information for a patch series.
COVER is an mbox with the cover letter, and PATCHES is an
am-ready mbox.  If the series does not have a cover letter (e.g.,
a one-patch series), COVER may be nil."
  (with-temp-buffer
    (insert-file-contents (or cover patches))
    (let ((info (save-restriction
                  (message-narrow-to-headers)
                  (list :date (message-fetch-field "date")
                        :from (message-fetch-field "from")
                        :subject (message-fetch-field "subject")))))
      (when (re-search-forward (rx line-start "base-commit: "
                                   (group (>= 40 hex-digit))
                                   line-end)
                               nil t)
        (plist-put info :base-commit (match-string 1)))
      info)))

(defun piem-b4--shorten-subject (subject)
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

(defun piem-b4-name-branch-who-what-v (info)
  "Construct a branch name like \"km/b4-short-subj__v3\".

In the above example, \"k\" and \"m\" are the first letters of
the first and second \"words\" in the \"From:\" field.
\"b4-short-subj\" is a stripped down, truncated variant of
\"Subject:\".  And \"v3\" is the version of the patch series, as
indicated in the subject.

INFO is a plist with properties documented
in `piem-b4-default-branch-function'."
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
              (piem-b4--shorten-subject subject)
              (and version (concat "__" version))))))

;; In many cases, we don't really need b4 to download the mbox for us,
;; as we already have our own mbox to URL mapping.  Perhaps we should
;; default to using that, but it should still be an option to use b4
;; so that we honor its customization/URL resolution.
(defun piem-b4--get-am-files (mid coderepo args)
  (let* ((outdir (file-name-as-directory
                  (make-temp-file "piem-b4-" t)))
         (root (concat outdir "m"))
         (mbox-thread (concat root "-piem"))
         (custom-p nil))
    (when-let ((fn (run-hook-with-args-until-success
                    'piem-mid-to-thread-functions mid)))
      (with-temp-file mbox-thread
        (funcall fn)
        (unless (= (point-max) 1)
          (setq custom-p t))))
    ;; Move to the coderepo so that we pick up any b4 configuration
    ;; from there.
    (apply #'piem-process-call piem-b4-output-buffer coderepo
           piem-b4-b4-executable "am"
           (and custom-p
                (concat "--use-local-mbox=" mbox-thread))
           (concat "--outdir=" outdir)
           (concat "--mbox-name=m")
           (append args (list mid)))
    (let ((mbox-cover (concat root ".cover"))
          (mbox-am (concat root ".mbx")))
      (list (and (file-exists-p mbox-cover)
                 mbox-cover)
            (if (file-exists-p mbox-am)
                mbox-am
              (error "Expected mbox file does not exist: %s" mbox-am))))))


;;;; Commands

;;;###autoload
(defun piem-b4-am-ready-from-mbox (mbox &optional args)
  (interactive (list (read-file-name "mbox: ")
                     (transient-args 'piem-b4-am)))
  (apply #'piem-process-start piem-b4-output-buffer nil
         piem-b4-b4-executable "am"
         (cons (concat "--use-local-mbox=" mbox) args)))

;;;###autoload
(defun piem-b4-am-ready-from-mid (mid &optional args)
  (interactive (list (read-string "Message ID: " nil nil (piem-mid))
                     (transient-args 'piem-b4-am)))
  (apply #'piem-process-start piem-b4-output-buffer nil
         piem-b4-b4-executable "am" (append args (list mid))))

;;;###autoload
(defun piem-b4-am-from-mid (mid &optional args)
  (interactive (list (or (piem-mid)
                         (read-string "Message ID: "))
                     (transient-args 'piem-b4-am)))
  (when-let ((badopt (cl-some
                      (lambda (arg)
                        (and (string-match
                              (rx string-start
                                  (group (or "--outdir" "--mbox-name")) "=")
                              arg)
                             (match-string 1 arg)))
                      args)))
    (user-error "%s is incompatible with this command" badopt))
  (pcase-let* ((coderepo (or (piem-inbox-coderepo)
                             (and (fboundp 'projectile-relevant-known-projects)
                                  (completing-read
                                   "Project: "
                                   (projectile-relevant-known-projects)))
                             (and piem-use-magit
                                  (fboundp 'magit-read-repository)
                                  (magit-read-repository))
                             (read-directory-name "Git repository: ")))
               (`(,cover ,mbox-file)
                (piem-b4--get-am-files mid coderepo args))
               (info (piem-b4--series-info cover mbox-file))
               (default-directory coderepo))
    ;; TODO: Optionally do more through Magit.
    (let ((new-branch (read-string
                       "New branch (empty for detached): "
                       (funcall piem-b4-default-branch-function info)))
          (base (completing-read
                 "Base commit: "
                 (let ((cands (and piem-use-magit
                                   (fboundp 'magit-list-local-branch-names)
                                   (magit-list-local-branch-names)))
                       (base (plist-get info :base-commit)))
                   (if base (cons base cands) cands)))))
      (apply #'piem-process-call piem-b4-output-buffer nil
             piem-git-executable "checkout"
             (append (if (string-empty-p new-branch)
                         (list "--detach")
                       (list "-b" new-branch))
                     (list base))))
    (piem-process-call piem-b4-output-buffer nil
                       piem-git-executable "am" "--scissors"
                       mbox-file)
    (if (and piem-use-magit
             (fboundp 'magit-status-setup-buffer))
        (magit-status-setup-buffer)
      (dired "."))))

(define-infix-argument piem-b4-am:--outdir ()
  :description "Output directory"
  :class 'transient-option
  :shortarg "-o"
  :argument "--outdir="
  :reader #'transient-read-existing-directory)

(define-infix-argument piem-b4-am:--mbox-name ()
  :description "Base file name for mbox"
  :class 'transient-option
  :shortarg "-n"
  :argument "--mbox-name="
  :reader #'read-string)

(define-infix-argument piem-b4-am:--use-version ()
  :description "Desired version of patch series"
  :class 'transient-option
  :shortarg "-v"
  :argument "--use-version="
  :reader #'transient-read-number-N+)

(define-infix-argument piem-b4-am:--cherry-pick ()
  :description "Select a subset of patches by number"
  :class 'transient-option
  :shortarg "-P"
  :argument "--cherry-pick="
  :reader #'read-string)

;;;###autoload (autoload 'piem-b4-am "piem-b4" nil t)
(define-transient-command piem-b4-am ()
  "Filter mbox to patches and feed to git-am"
  ["General options"
   ("-c" "Check newer versions" "--check-newer-revisions")
   ("-C" "Don't use local cache" "--no-cache")
   ("-s" "Add my signed-off-by" "--add-my-sob")
   ("-S" "Apply trailers without checking email addresses" "--sloppy-trailers")
   ("-t" "Apply cover letter trailers" "--apply-cover-trailers")
   ("-T" "Do not add trailers" "--no-add-trailers")
   (piem-b4-am:--use-version)
   (piem-b4-am:--cherry-pick)]
  ["Options for creating am-ready mboxes"
   (piem-b4-am:--outdir)
   (piem-b4-am:--mbox-name)]
  ["Actions"
   [("a" "Message ID -> mbox -> git-am" piem-b4-am-from-mid)]
   [("b" "Local mbox -> am-ready mbox" piem-b4-am-ready-from-mbox)
    ("i" "Message ID -> am-ready mbox" piem-b4-am-ready-from-mid)]])

;;; piem-b4.el ends here
(provide 'piem-b4)
