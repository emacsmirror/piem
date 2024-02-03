;;; piem-b4.el --- Emacs interface to the b4 tool  -*- lexical-binding: t; -*-

;; Copyright all piem contributors <piem@inbox.kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; Keywords: vc, tools
;; Package-Requires: ((emacs "26.3") (transient "0.3.0"))

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

;; This library provides a Transient [1] interface to b4 [2].  It is
;; documented in the piem manual.
;;
;; [1] https://magit.vc/manual/transient/
;; [2] https://git.kernel.org/pub/scm/utils/b4/b4.git

;;; Code:

(require 'cl-lib)
(require 'mail-extr)
(require 'message)
(require 'piem)
(require 'transient)


;;;; Options

(defgroup piem-b4 nil
  "Control the b4 tool from Emacs."
  :link '(info-link "(piem)Using b4 to apply patches")
  :group 'piem)

(defcustom piem-b4-b4-executable "b4"
  "Which b4 executable to use."
  :type 'string)


;;;; Internals

(defvar piem-b4-keep-temp-directory nil
  "Don't clean up the directory created by `piem-b4-am-from-mid'.
This is intended to be used for debugging purposes.")

(defun piem-b4--get-am-files (mid coderepo args)
  (let* ((outdir (file-name-as-directory
                  (make-temp-file "piem-b4-" t)))
         (root (concat outdir "m"))
         (mbox-thread (concat root "-piem"))
         (local-mbox-p nil)
         (clean-fn (and (not piem-b4-keep-temp-directory)
                        (lambda () (delete-directory outdir t)))))
    (when-let ((fn (run-hook-with-args-until-success
                    'piem-mid-to-thread-functions mid)))
      (with-temp-file mbox-thread
        (funcall fn)
        (unless (= (point-max) 1)
          (setq local-mbox-p t))))
    ;; `piem-mid-to-thread-functions' didn't generate an mbox.  Next
    ;; try to download it from an inbox's URL.  Finally, fall back to
    ;; b4's configuration.
    (unless local-mbox-p
      (when-let ((url (and (equal mid (piem-mid))
                           (piem-inbox-url))))
        (ignore-errors
          (piem-with-url-contents
              (concat url (piem-escape-mid mid) "/t.mbox.gz")
            (piem-gunzip-buffer)
            (write-region nil nil mbox-thread))
          (setq local-mbox-p t))))
    ;; Move to the coderepo so that we pick up any b4 configuration
    ;; from there.
    (condition-case err
        (apply #'piem-process-call coderepo piem-b4-b4-executable "am"
               (and local-mbox-p
                    (concat "--use-local-mbox=" mbox-thread))
               (concat "--outdir=" outdir)
               (concat "--mbox-name=m")
               (append args (list mid)))
      (piem-process-error
       (when clean-fn
         (funcall clean-fn))
       (signal (car err) (cdr err))))
    (let ((mbox-cover (concat root ".cover"))
          (mbox-am (concat root ".mbx")))
      (list (and (file-exists-p mbox-cover)
                 mbox-cover)
            (if (file-exists-p mbox-am)
                mbox-am
              (when clean-fn
                (funcall clean-fn))
              (error "Expected mbox file does not exist: %s" mbox-am))
            clean-fn))))


;;;; Commands

;;;###autoload
(defun piem-b4-am-ready-from-mbox (mbox &optional args)
  "Extract an am-ready mbox from a thread of messages.
MBOX is the file name of an mbox that contains a patch series.
ARGS is a list of arguments to pass to `b4 am'."
  (interactive (list (read-file-name "mbox: ")
                     (transient-args 'piem-b4-am)))
  (apply #'piem-process-start nil piem-b4-b4-executable "am"
         (cons (concat "--use-local-mbox=" mbox) args)))

;;;###autoload
(defun piem-b4-am-ready-from-mid (mid &optional args)
  "Download the thread for MID and and extract an am-ready mbox.
MID is a message ID to pass directly to `b4 am', along with the
list of arguments specified via ARGS."
  (interactive (list (read-string "Message ID: " nil nil (piem-mid))
                     (transient-args 'piem-b4-am)))
  (apply #'piem-process-start nil piem-b4-b4-executable "am"
         (append args (list mid))))

;;;###autoload
(defun piem-b4-am-from-mid (mid &optional args toggle-worktree)
  "Get the thread for MID, extract an am-ready mbox, and apply it.

Try to generate a thread for MID with
`piem-mid-to-thread-functions'.  If that fails, try to download
the thread from an inbox URL associated with the current buffer,
provided that the current buffer's message ID matches MID.  And
if that doesn't work, let `b4 am' download the thread according
to its own configuration.

After calling `b4 am' with ARGS to prepare an am-ready mbox, feed
the result to `git am'.

When prefix argument TOGGLE-WORKTREE is non-nil, invert the
meaning of `piem-am-create-worktree'.  With the default value,
this triggers the creation of a new worktree."
  (interactive (list (or (piem-mid)
                         (read-string "Message ID: "))
                     (transient-args 'piem-b4-am)
                     current-prefix-arg))
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
               (`(,cover ,mbox-file ,clean-fn)
                (piem-b4--get-am-files mid coderepo args))
               (default-directory coderepo))
    (unwind-protect
        (piem-am mbox-file
                 nil
                 (with-temp-buffer
                   (insert-file-contents (or cover mbox-file))
                   (piem-extract-mbox-info))
                 coderepo
                 toggle-worktree)
      (when clean-fn
        (funcall clean-fn)))))

(transient-define-argument piem-b4-am:--outdir ()
  :description "Output directory"
  :class 'transient-option
  :shortarg "-o"
  :argument "--outdir="
  :reader #'transient-read-existing-directory)

(transient-define-argument piem-b4-am:--mbox-name ()
  :description "Base file name for mbox"
  :class 'transient-option
  :shortarg "-n"
  :argument "--mbox-name="
  :reader #'read-string)

(transient-define-argument piem-b4-am:--use-version ()
  :description "Desired version of patch series"
  :class 'transient-option
  :shortarg "-v"
  :argument "--use-version="
  :reader #'transient-read-number-N+)

(transient-define-argument piem-b4-am:--cherry-pick ()
  :description "Select a subset of patches by number"
  :class 'transient-option
  :shortarg "-P"
  :argument "--cherry-pick="
  :reader #'read-string)

(transient-define-argument piem-b4-am:--use-project ()
  :description "Use a specific lore project instead of guessing"
  :class 'transient-option
  :shortarg "-p"
  :argument "--use-project="
  :level 7
  :reader #'read-string)

(transient-define-argument piem-b4-am:--guess-branch ()
  :description "Restrict base guess to this branch"
  :class 'transient-option
  :shortarg "-b"
  :argument "--guess-branch="
  ;; TODO: Optionally support `magit-read-branch'.
  :reader #'read-string)

(transient-define-argument piem-b4-am:--guess-lookback ()
  :description "How many days to go back when guessing base"
  :class 'transient-option
  :shortarg "-G"
  :argument "--guess-lookback="
  :reader #'transient-read-number-N+)

;;;###autoload (autoload 'piem-b4-am "piem-b4" nil t)
(transient-define-prefix piem-b4-am ()
  "Filter mbox to patches and feed to git-am"
  :man-page "b4"
  ["General options"
   ("-c" "Check newer versions" "--check-newer-revisions")
   ("-C" "Don't use local cache" "--no-cache")
   ;; Hide by default because it hard codes the URL.
   (7 "-l" "Add a lore.kernel.org/r/ to patches" "--add-link")
   ("-L" "Do not reroll partial series" "--no-partial-reroll")
   (piem-b4-am:--use-version)
   (piem-b4-am:--cherry-pick)]
  ["Trailer options"
   ("+c" "Copy To: and Cc: addresses as Cc: trailers" "--cc-trailers")
   ("-s" "Add my signed-off-by" "--add-my-sob")
   ("-S" "Apply trailers without checking email addresses" "--sloppy-trailers")
   ("-t" "Apply cover letter trailers" "--apply-cover-trailers")
   ("-T" "Do not add trailers" "--no-add-trailers")]
  ["Options for creating am-ready mboxes"
   ("-3" "Prepare for 3-way merge" "--prep-3way")
   ("-g" "Try to guess base" "--guess-base")
   (piem-b4-am:--guess-branch)
   (piem-b4-am:--guess-lookback)
   (piem-b4-am:--use-project)
   (piem-b4-am:--outdir)
   (piem-b4-am:--mbox-name)
   ("-M" "Save as maildir" "--save-as-maildir")
   (7 "-Q" "Save as quilt-ready folder" "--quilt-ready")
   ;; Hide because this is unlikely to be useful outside of
   ;; command-line piping to `git am'.
   (5 "-V" "Do not save cover letter" "--no-cover")]
  ["Actions"
   [("a" "Message ID -> mbox -> git-am" piem-b4-am-from-mid)]
   [("b" "Local mbox -> am-ready mbox" piem-b4-am-ready-from-mbox)
    ("i" "Message ID -> am-ready mbox" piem-b4-am-ready-from-mid)]])

;;; piem-b4.el ends here
(provide 'piem-b4)
