;;; piem-tests.el --- tests for piem                 -*- lexical-binding: t; -*-

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
(require 'piem)
(require 'piem-lei-tests)
(require 'piem-rmail-tests)

(defmacro piem-tests-with-pi-config (content &rest body)
  "Point public-inbox's configuration to CONTENT and evaluate BODY."
  (declare (indent 1) (debug t))
  (let ((temp-file (cl-gensym "temp-file"))
        (pi-config-orig (cl-gensym "pi-config-org")))
    `(let ((,temp-file (make-temp-file "piem-tests-" nil nil ,content))
           (,pi-config-orig (getenv "PI_CONFIG")))
       (unwind-protect
           (progn (setenv "PI_CONFIG" ,temp-file)
                  ,@body)
         (setenv "PI_CONFIG" ,pi-config-orig)
         (delete-file ,temp-file)))))

(defvar piem-tests-sample-pi-config "
[publicinbox \"foo\"]
        address = foo@example.com
        url = https://example.com/foo
        inboxdir = /inboxes/foo
        coderepo = foo.git

[coderepo \"foo.git\"]
          dir = /code/foo/.git
")

(defvar piem-tests-sample-pi-config-multiple-coderepos "
[publicinbox \"foo\"]
        address = foo@example.com
        url = https://example.com/foo
        inboxdir = /inboxes/foo
        coderepo = foo.git
        coderepo = bar.git

[coderepo \"foo.git\"]
          dir = /code/foo/.git

[coderepo \"bar.git\"]
          dir = /code/bar/.git
")

(ert-deftest piem-merged-inboxes:from-config-disabled ()
  (let ((piem-get-inboxes-from-config nil)
        (piem-inboxes nil))
    (should-not (piem-merged-inboxes))
    (should-not (piem-tests-with-pi-config ""
                  (piem-merged-inboxes)))
    (should-not (piem-tests-with-pi-config piem-tests-sample-pi-config
                  (piem-merged-inboxes))))
  (let ((piem-get-inboxes-from-config nil)
        (piem-inboxes '(("inbox" :url "inbox-url"))))
    (should (equal (piem-inbox-get :url "inbox") "inbox-url"))))

(ert-deftest piem-merged-inboxes:from-config ()
  (piem-clear-merged-inboxes)
  (let ((piem-get-inboxes-from-config t)
        (piem-inboxes nil))
    (piem-tests-with-pi-config piem-tests-sample-pi-config
      (should (equal (piem-inbox-get :address "foo")
                     "foo@example.com"))
      (should (equal (piem-inbox-get :url "foo")
                     "https://example.com/foo"))
      (should (equal (piem-inbox-coderepo "foo")
                     "/code/foo/")))
    (piem-tests-with-pi-config ""
      (should (equal (piem-inbox-get :address "foo")
                     "foo@example.com"))
      (piem-clear-merged-inboxes)
      (should-not (piem-inbox-get :address "foo")))))

(ert-deftest piem-merged-inboxes:from-config-multiple-coderepos ()
  (piem-clear-merged-inboxes)
  (let ((piem-get-inboxes-from-config t)
        (piem-inboxes nil))
    (piem-tests-with-pi-config piem-tests-sample-pi-config-multiple-coderepos
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "/code/foo")))
        (should (equal (piem-inbox-coderepo "foo")
                       "/code/foo/")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "/code/bar")))
        (should (equal (piem-inbox-coderepo "foo")
                       "/code/bar/"))))))

(ert-deftest piem-merged-inboxes:override-config ()
  (piem-clear-merged-inboxes)
  (let ((piem-get-inboxes-from-config t)
        (piem-inboxes '(("foo" :coderepo "/tmp/override/"))))
    (piem-tests-with-pi-config piem-tests-sample-pi-config
      (should (equal (piem-inbox-coderepo "foo")
                     "/tmp/override/"))
      (should (equal (piem-inbox-get :address "foo")
                     "foo@example.com")))))

(ert-deftest piem-message-link-re ()
  (should-not (string-match-p
               (piem-message-link-re "https://example.com/inbox")
               "https://example.com/inbox"))
  (should (string-match-p
           (piem-message-link-re "https://example.com/inbox")
           "https://example.com/inbox/msg@id/"))
  (should-not (string-match-p
               (piem-message-link-re "https://example.com/inbox" "msg@id1")
               "https://example.com/inbox/msg@id/"))
  (should (string-match-p
           (piem-message-link-re "https://example.com/inbox/")
           "https://example.com/inbox/msg@id/T/#t"))
  (should (string-match-p
           (piem-message-link-re "https://example.com/inbox/")
           "https://example.com/inbox/msg@id/T/#r53974723fd9"))
  (should (string-match-p
           (piem-message-link-re "https://example.com/inbox/")
           "https://example.com/inbox/msg@id/T/#r53974723fd9"))
  (should (string-match-p
           (piem-message-link-re "https://example.com/inbox/")
           "https://example.com/inbox/msg@id/t/#ma0d6f35c094")))

(ert-deftest piem-escape-mid ()
  (should (equal (piem-escape-mid "msg@id") "msg@id"))
  (should (equal (piem-escape-mid "m/g@id") "m%2Fg@id")))

(ert-deftest piem-mid-url ()
  (let ((piem-get-inboxes-from-config nil)
        (piem-inboxes '(("inbox-a" :url "https://example.com/a/")
                        ("inbox-b" :url "https://example.com/b/"))))
    (should (equal (piem-mid-url "msg@id" "inbox-a")
                   "https://example.com/a/msg@id"))
    (should (equal (piem-mid-url "m/sg@id" "inbox-b")
                   "https://example.com/b/m%2Fsg@id"))
    (should-error (piem-mid-url "msg@id")
                  :type 'user-error))
  (let ((piem-get-inboxes-from-config nil)
        (piem-inboxes '(("inbox-a"))))
    (should-error (piem-mid-url "msg@id" "inbox-a")
                  :type 'user-error)))

(ert-deftest piem-name-branch-who-what-v ()
  (should (equal (piem-name-branch-who-what-v
                  (list :from "Foo Bar <f@example.com>"
                        :subject "[PATCH] Do a thing"))
                 "fb/do-thing"))
  (should (equal (piem-name-branch-who-what-v
                  (list :from "Foo <f@example.com>"
                        :subject "[PATCH] Do a thing"))
                 "f/do-thing"))
  (should (equal (piem-name-branch-who-what-v
                  (list :from "Foo Bar <f@example.com>"
                        :subject "[PATCH v3] Do a thing"))
                 "fb/do-thing__v3"))
  (should-not (piem-name-branch-who-what-v
               (list :from "Foo Bar <xyz.com>"))))

(ert-deftest piem--insert-message-id-header ()
  (should-not
   (with-temp-buffer
     (piem--insert-message-id-header "msg@id")))
  (should-not
   (string-match-p
    "Message-Id: <msg@id>"
    (with-temp-buffer
      (insert "\
From 0d732713af1f3fb48b37430e2cd0a3033cea14f3 Mon Sep 17 00:00:00 2001
From: Foo Bar <f@example.com>
Message-ID: <existing@id>
Date: Fri, 22 Jan 2021 22:35:58 -0500
Subject: [PATCH] a

---
 a | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 a")
      (goto-char (point-min))
      (piem--insert-message-id-header "msg@id")
      (buffer-string))))
  (should
   (string-match-p
    (concat
     (rx "Subject: [PATCH 1/2] a\nMessage-Id: <msg@id>\n"
         (one-or-more anychar)
         "Subject: [PATCH 2/2] b\nMessage-Id: <msg@id>\n"))
    (with-temp-buffer
      (insert "\
From 0d732713af1f3fb48b37430e2cd0a3033cea14f3 Mon Sep 17 00:00:00 2001
From: Foo Bar <f@example.com>
Date: Fri, 22 Jan 2021 22:35:58 -0500
Subject: [PATCH 1/2] a

---
 a | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 a

From 7b2433ead6d8d10bff3325cb3719a316ddb52392 Mon Sep 17 00:00:00 2001
From: Foo Bar <f@example.com>
Date: Fri, 22 Jan 2021 22:36:09 -0500
Subject: [PATCH 2/2] b

---
 b | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 b")
      (goto-char (point-min))
      (piem--insert-message-id-header "msg@id")
      (buffer-string)))))

(provide 'piem-tests)
;;; piem-tests.el ends here
