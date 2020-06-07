;;; piem-maildir.el --- Maildir helpers for piem     -*- lexical-binding: t; -*-

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file extracts parts of Notmuch's emacs/notmuch-maildir-fcc.el
;; (ed40579ad3882e6f9bbe9b1ba5e707ab289ca203), simply replacing
;; "notmuch-maildir-fcc-" with "piem-maildir-".  No other
;; modifications have been made.  The copyright notice above matches
;; what is in the original file.

;;; Code:

(defvar piem-maildir-count 0)

(defun piem-maildir-host-fixer (hostname)
  (replace-regexp-in-string "/\\|:"
                            (lambda (s)
                              (cond ((string-equal s "/") "\\057")
                                    ((string-equal s ":") "\\072")
                                    (t s)))
                            hostname
                            t
                            t))

(defun piem-maildir-make-uniq-maildir-id ()
   (let* ((ftime (float-time))
          (microseconds (mod (* 1000000 ftime) 1000000))
          (hostname (piem-maildir-host-fixer (system-name))))
     (setq piem-maildir-count (+ piem-maildir-count 1))
     (format "%d.%d_%d_%d.%s"
             ftime
             (emacs-pid)
             microseconds
             piem-maildir-count
             hostname)))

(defun piem-maildir-dir-is-maildir-p (dir)
  (and (file-exists-p (concat dir "/cur/"))
       (file-exists-p (concat dir "/new/"))
       (file-exists-p (concat dir "/tmp/"))))

(defun piem-maildir-move-tmp-to-new (destdir msg-id)
  (add-name-to-file
   (concat destdir "/tmp/" msg-id)
   (concat destdir "/new/" msg-id ":2,")))

(provide 'piem-maildir)
;;; piem-maildir.el ends here
