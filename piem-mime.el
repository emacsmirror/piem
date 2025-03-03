;;; piem-mime.el --- MIME helpers for piem           -*- lexical-binding: t; -*-

;; Copyright all piem contributors <piem@inbox.kyleam.com>
;; Copyright (C) 2009 Keith Amidon <keith@nicira.com>

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
(require 'mm-decode)
(require 'mm-view)

;; `piem-mime-foreach-part' and `piem-mime-count-attachments' are
;; copied from `notmuch-foreach-mime-part' and
;; `notmuch-count-attachments' (Notmuch commit
;; d0469c5b4c6ed9188b96b12363fced45291813fd), with only name
;; adjustments.

(defun piem-mime-foreach-part (fn mm-handle)
  (cond ((stringp (car mm-handle))
         (dolist (part (cdr mm-handle))
           (piem-mime-foreach-part fn part)))
        ((bufferp (car mm-handle))
         (funcall fn mm-handle))
        (t (dolist (part mm-handle)
             (piem-mime-foreach-part fn part)))))

(defun piem-mime-count-attachments (mm-handle)
  (let ((count 0))
    (piem-mime-foreach-part
     (lambda (p)
       (let ((disposition (mm-handle-disposition p)))
         (and (listp disposition)
              (or (equal (car disposition) "attachment")
                  (and (equal (car disposition) "inline")
                       (assq 'filename disposition)))
              (cl-incf count))))
     mm-handle)
    count))

(defun piem-mime--extract-attached-patch (handle)
  "Get the content for HANDLE if it looks like a patch.
The return value is of the form (N . CONTENT), where N is the
number at the start of the file name."
  (when (listp handle)
    (let ((type (mm-handle-media-type handle))
          (filename (mm-handle-filename handle)))
      (and (or (member type '("text/x-diff" "text/x-patch"))
               (and filename
                    (equal type "text/plain")
                    (string-suffix-p ".patch" filename t)))
           (with-temp-buffer
             (mm-display-inline handle)
             (cons
              (string-to-number filename)
              (buffer-substring-no-properties (point-min) (point-max))))))))

(defun piem-mime-am-ready-mbox ()
  "Return a function that inserts an am-ready mbox.

If the buffer has any MIME parts that look like a patch, use
those parts' contents as the mbox, ordering the patches based on
the number at the start of the file name.

This function is intended to be called underneath by top-level
`piem-am-ready-mbox-functions' functions."
  (let* ((handle (mm-dissect-buffer))
         (n-attachments (piem-mime-count-attachments handle))
         patches)
    (when (/= n-attachments 0)
      (piem-mime-foreach-part
       (lambda (p)
         (when-let* ((patch (piem-mime--extract-attached-patch p)))
           (push patch patches)))
       handle)
      (when patches
        (setq patches (sort (nreverse patches)
                            (lambda (x y) (< (car x) (car y)))))
        (cons (lambda ()
                (dolist (patch patches)
                  (insert (cdr patch))))
              "mbox")))))

(provide 'piem-mime)
;;; piem-mime.el ends here
