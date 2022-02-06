;;; piem-lei-tests.el --- tests for piem-lei  -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'piem-lei)

(ert-deftest piem-lei-query--add-child ()
  (should-error
   (piem-lei-query--add-child
    (make-piem-lei-msg :mid "m1")
    (make-piem-lei-msg :mid "m1")))
  (let ((m1 (make-piem-lei-msg :mid "m1"))
        (m2 (make-piem-lei-msg :mid "m2")))
    (piem-lei-query--add-child m1 m2)
    (should (equal (piem-lei-msg-parent m2) m1))
    (should (equal (piem-lei-msg-children m1) (list m2))))
  (let ((m1 (make-piem-lei-msg :mid "m1"))
        (m2 (make-piem-lei-msg :mid "m2"))
        (m3 (make-piem-lei-msg :mid "m3"))
        (m4 (make-piem-lei-msg :mid "m4")))
    (piem-lei-query--add-child m1 m2)
    (piem-lei-query--add-child m1 m4)
    (piem-lei-query--add-child m3 m2)
    (should (equal (piem-lei-msg-parent m2) m3))
    (should (equal (piem-lei-msg-children m1) (list m4)))
    (should (equal (piem-lei-msg-children m3) (list m2)))))

(ert-deftest piem-lei-query--has-descendant ()
  (let ((m1 (make-piem-lei-msg :mid "m1"))
        (m2 (make-piem-lei-msg :mid "m2")))
    (should-not
     (piem-lei-query--has-descendant m1 m2))
    (should-not
     (piem-lei-query--has-descendant m2 m1)))
  (let ((m1 (make-piem-lei-msg :mid "m1")))
    (should (piem-lei-query--has-descendant m1 m1)))
  (let ((m1 (make-piem-lei-msg :mid "m1"))
        (m2 (make-piem-lei-msg :mid "m2")))
    (piem-lei-query--add-child m1 m2)
    (should (piem-lei-query--has-descendant m1 m2))
    (should-not
     (piem-lei-query--has-descendant m2 m1)))
  (let ((m1 (make-piem-lei-msg :mid "m1"))
        (m2 (make-piem-lei-msg :mid "m2"))
        (m3 (make-piem-lei-msg :mid "m3")))
    (piem-lei-query--add-child m1 m2)
    (piem-lei-query--add-child m2 m3)
    (should (piem-lei-query--has-descendant m1 m2))
    (should (piem-lei-query--has-descendant m1 m3))
    (should (piem-lei-query--has-descendant m2 m3))
    (should-not (piem-lei-query--has-descendant m2 m1))
    (should-not (piem-lei-query--has-descendant m3 m2))
    (should-not (piem-lei-query--has-descendant m3 m1))))

(ert-deftest piem-lei-query--elide-subject:keep-original ()
  (should (equal "ghi jlk"
                 (piem-lei-query--elide-subject
                  nil
                  "ghi jlk")))
  (should (equal "ghi jlk"
                 (piem-lei-query--elide-subject
                  "abc def"
                  "ghi jlk")))
  (should (equal "abc def"
                 (piem-lei-query--elide-subject
                  "[PATCH] abc def"
                  "abc def")))
  (should (equal "abc def"
                 (piem-lei-query--elide-subject
                  "[bug#00000] [PATCH] abc def"
                  "abc def")))
  (should (equal "abc def"
                 (piem-lei-query--elide-subject
                  "[PATCH] abc def"
                  "abc def")))
  (should (equal "[bug#00000] [PATCH v2] abc"
                 (piem-lei-query--elide-subject
                  "[bug#00000] [PATCH] abc"
                  "[bug#00000] [PATCH v2] abc")))
  (should (equal "[bug#00000] [PATCH v2] ghi jlk mno"
                 (piem-lei-query--elide-subject
                  "[bug#00000] [PATCH] abc def"
                  "[bug#00000] [PATCH v2] ghi jlk mno"))))

(defvar piem-lei-tests-elide-string (if (char-displayable-p ?…) "…" "..."))

(ert-deftest piem-lei-query--elide-subject:elide ()
  (should (equal (concat "[PATCH v2] " piem-lei-tests-elide-string)
                 (piem-lei-query--elide-subject
                  "[PATCH] abc def"
                  "[PATCH v2] abc def")))
  (should (equal (concat "[bug#00000] [PATCH v2] " piem-lei-tests-elide-string)
                 (piem-lei-query--elide-subject
                  "[bug#00000] [PATCH] abc def"
                  "[bug#00000] [PATCH v2] abc def")))
  (should (equal (concat "bug#00000: [PATCH v2] " piem-lei-tests-elide-string)
                 (piem-lei-query--elide-subject
                  "[bug#00000] [PATCH] abc def"
                  "bug#00000: [PATCH v2] abc def"))))

(provide 'piem-lei-tests)
;;; piem-lei-tests.el ends here
