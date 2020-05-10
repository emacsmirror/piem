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
(require 'piem)
(require 'subr-x)
(require 'transient)


;;; Options

(defgroup piem-b4 nil
  "Control the b4 tool from Emacs."
  :link '(info-link "(piem)b4 integration")
  :group 'piem)

(defcustom piem-b4-b4-executable "b4"
  "Which b4 executable to use."
  :type 'string)

(defcustom piem-b4-git-executable
  (or (and (boundp 'magit-git-executable) magit-git-executable)
      "git")
  "Which git executable to use."
  :type 'string)

(defcustom piem-b4-use-magit (featurep 'magit)
  "Whether to use Magit where possible."
  :type 'boolean)


;;; Internals

(define-error 'piem-b4-error "piem-b4 error")

(defconst piem-b4-output-buffer "*piem-b4-output*")

;; TODO: Use an asynchronous process.
(defun piem-b4--call (program infile &rest args)
  (let ((temp-buffer-show-function (lambda (_))))
    (with-output-to-temp-buffer piem-b4-output-buffer
      (unless (= 0 (apply #'call-process program
                          infile standard-output nil
                          (remq nil args)))
        (display-buffer piem-b4-output-buffer)
        (signal 'piem-b4-error
                (list (format "%s call in %s failed"
                              program default-directory)))))))

(defun piem-b4--call-b4 (infile &rest args)
  (apply #'piem-b4--call piem-b4-b4-executable infile args))

(defun piem-b4--call-git (infile &rest args)
  (apply #'piem-b4--call piem-b4-git-executable infile args))

;; In many cases, we don't really need b4 to download the mbox for us,
;; as we already have our own mbox to URL mapping.  Perhaps we should
;; default to using that, but it should still be an option to use b4
;; so that we honor its customization/URL resolution.
(defun piem-b4--get-mbox-file (mid coderepo args)
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
    (let ((default-directory coderepo))
      (apply #'piem-b4--call-b4 nil "am"
             (and custom-p
                  (concat "--use-local-mbox=" mbox-thread))
             (concat "--outdir=" outdir)
             (concat "--mbox-name=m")
             (append args (list mid))))
    (let ((mbox-am (concat root ".mbx")))
      (if (file-exists-p mbox-am)
          mbox-am
        (error "Expected mbox file does not exist: %s" mbox-am)))))


;;; Commands

;;;###autoload
(defun piem-b4-am-ready-from-mbox (mbox &optional args)
  (interactive (list (read-file-name "mbox: ")
                     (transient-args 'piem-b4-am)))
  (apply #'piem-b4--call-b4 nil "am"
         (cons (concat "--use-local-mbox=" mbox) args))
  (display-buffer piem-b4-output-buffer))

;;;###autoload
(defun piem-b4-am-ready-from-mid (mid &optional args)
  (interactive (list (read-string "Message ID: " nil nil (piem-mid))
                     (transient-args 'piem-b4-am)))
  (apply #'piem-b4--call-b4 nil "am" (append args (list mid)))
  (display-buffer piem-b4-output-buffer))

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
  (let* ((coderepo (or (piem-inbox-coderepo)
                       (and (fboundp 'projectile-relevant-known-projects)
                            (completing-read
                             "Project: "
                             (projectile-relevant-known-projects)))
                       (and piem-b4-use-magit
                            (fboundp 'magit-read-repository)
                            (magit-read-repository))
                       (read-directory-name "Git repository: ")))
         (mbox-file (piem-b4--get-mbox-file mid coderepo args))
         (default-directory coderepo))
    ;; TODO: From the mbox file (1) search for base commit and (2)
    ;; gather information to suggest default branch name.

    ;; TODO: Add branch call.  Without base, will need to ask branch
    ;; name and starting point.  Detached head could be signaled with
    ;; empty string.

    ;; TODO: Optionally do more through Magit.
    (piem-b4--call-git mbox-file "am" "--scissors")
    (if (and piem-b4-use-magit
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

;;;###autoload (autoload 'piem-b4-am "b4" nil t)
(define-transient-command piem-b4-am ()
  "Filter mbox to patches and feed to git-am"
  ["General options"
   ("-c" "Check newer versions" "--check-newer-revisions")
   ("-C" "Don't use local cache" "--no-cache")
   ("-s" "Add my signed-off-by" "--add-my-sob")
   ("-S" "Apply trailers without checking email addresses" "--sloppy-trailers")
   ("-t" "Apply cover letter trailers" "--apply-cover-trailers")
   ("-T" "Do not add trailers" "--no-add-trailers")
   (piem-b4-am:--use-version)]
  ["Options for creating am-ready mboxes"
   (piem-b4-am:--outdir)
   (piem-b4-am:--mbox-name)]
  ["Actions"
   [("a" "Message ID -> mbox -> git-am" piem-b4-am-from-mid)]
   [("b" "Local mbox -> am-ready mbox" piem-b4-am-ready-from-mbox)
    ("i" "Message ID -> am-ready mbox" piem-b4-am-ready-from-mid)]])

;;; piem-b4.el ends here
(provide 'piem-b4)
