;;; piem-mime.el --- MIME helpers for piem           -*- lexical-binding: t; -*-

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

(provide 'piem-mime)
;;; piem-mime.el ends here
