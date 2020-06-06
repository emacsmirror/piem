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
(require 'transient)


;;;; Options

(defgroup piem-b4 nil
  "Control the b4 tool from Emacs."
  :link '(info-link "(piem)b4 integration")
  :group 'piem)

(defcustom piem-b4-b4-executable "b4"
  "Which b4 executable to use."
  :type 'string)


;;;; Internals

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
    (apply #'piem-process-call coderepo piem-b4-b4-executable "am"
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
  (apply #'piem-process-start nil piem-b4-b4-executable "am"
         (cons (concat "--use-local-mbox=" mbox) args)))

;;;###autoload
(defun piem-b4-am-ready-from-mid (mid &optional args)
  (interactive (list (read-string "Message ID: " nil nil (piem-mid))
                     (transient-args 'piem-b4-am)))
  (apply #'piem-process-start nil piem-b4-b4-executable "am"
         (append args (list mid))))

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
  (pcase-let* ((coderepo (piem-inbox-coderepo-maybe-read))
               (`(,cover ,mbox-file)
                (piem-b4--get-am-files mid coderepo args))
               (default-directory coderepo))
    (piem-am mbox-file
             (with-temp-buffer
               (insert-file-contents (or cover mbox-file))
               (piem-extract-mbox-info))
             coderepo)))

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
