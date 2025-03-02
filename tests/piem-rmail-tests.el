;;; piem-rmail-tests.el --- tests for piem-rmail  -*- lexical-binding: t; -*-

;; Copyright all piem contributors <piem@inbox.kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>

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
(require 'ert)
(require 'piem-rmail)

(defvar piem-rmail-tests-mbox-text "\
From mboxrd@z Thu Jan  1 00:00:00 1970
From: A <a@example.com>
To: i@inbox.example.com
Subject: test
Date: Sun, 23 May 2021 02:26:01 -0400
Message-ID: <123@example.com>

test body

From mboxrd@z Thu Jan  1 00:00:00 1970
From: b <b@example.com>
To: A <a@example.com>
Cc: i@inbox.example.com
Subject: Re: test
Date: Sun, 23 May 2021 02:26:51 -0400
Message-ID: <456@example.com>
In-Reply-To: <123@example.com>
References: <123@example.com>

> test body

no thanks
")

(defun piem-rmail-tests-rmail-mode ()
  (cl-letf (((symbol-function #'message) (lambda (&rest _))))
    (rmail-mode)))

(ert-deftest piem-rmail-get-inbox ()
  (should
   (equal "foo"
          (with-temp-buffer
            (insert piem-rmail-tests-mbox-text)
            (piem-rmail-tests-rmail-mode)
            (let ((piem-get-inboxes-from-config nil)
                  (piem-inboxes '(("foo" :address "i@inbox.example.com"))))
              (piem-rmail-get-inbox))))))

(ert-deftest piem-rmail-get-mid ()
  (should
   (equal (list "123@example.com" "456@example.com")
          (with-temp-buffer
            (insert piem-rmail-tests-mbox-text)
            (piem-rmail-tests-rmail-mode)
            (rmail-first-message)
            (let ((piem-get-inboxes-from-config nil)
                  (piem-inboxes '(("foo" :address "i@inbox.example.com"))))
              (list (piem-rmail-get-mid)
                    (progn
                      (rmail-next-message 1)
                      (piem-rmail-get-mid))))))))

(provide 'piem-rmail-tests)
;;; piem-rmail-tests.el ends here
