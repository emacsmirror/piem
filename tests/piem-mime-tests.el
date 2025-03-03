;;; piem-mime-tests.el --- tests for piem-mime  -*- lexical-binding: t; -*-

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
(require 'mm-decode)
(require 'piem-mime)

(defun piem-mime-tests-dissect (text)
  (with-temp-buffer
    (insert text)
    (mm-dissect-buffer)))

(defvar piem-mime-tests-case-inline "\
From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Message-ID: <mid@example.com>
MIME-Version: 1.0
Content-Transfer-Encoding: 8bit
Subject: [PATCH] inline patch

---
 foo | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo

diff --git a/foo b/foo
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/foo
@@ -0,0 +1 @@
+foo

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1")

(defvar piem-mime-tests-case-attached-one "\
From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Message-ID: <mid@example.com>
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"=-=-=\"
Subject: [PATCH] attached patch

--=-=-=
Content-Type: text/plain; charset=utf-8
Content-Transfer-Encoding: quoted-printable

see attached

--=-=-=
Content-Type: text/x-patch
Content-Disposition: attachment;
 filename=0001-foo.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH] foo

msg
---
 foo | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo

diff --git a/foo b/foo
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/foo
@@ -0,0 +1 @@
+foo

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=--
")

(defvar piem-mime-tests-case-attached-multi "\
From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Message-ID: <mid@example.com>
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary=\"=-=-=\"
Subject: [PATCH] attached patches

--=-=-=
Content-Type: text/plain

multiple attached

--=-=-=
Content-Type: text/x-diff
Content-Disposition: attachment;
 filename=0001-foo.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 1/3] foo

foo msg
---
 foo | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo

diff --git a/foo b/foo
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/foo
@@ -0,0 +1 @@
+foo

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=
Content-Type: text/x-diff
Content-Disposition: attachment;
 filename=0002-bar.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 2/3] bar

bar msg
---
 bar | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 bar

diff --git a/bar b/bar
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/bar
@@ -0,0 +1 @@
+bar

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=
Content-Type: text/x-diff
Content-Disposition: attachment;
 filename=0003-baz.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 3/3] baz

baz msg
---
 baz | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 baz

diff --git a/baz b/baz
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/baz
@@ -0,0 +1 @@
+baz

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=--
")

(defvar piem-mime-tests-case-attached-multi-nonum "\
From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Message-ID: <mid@example.com>
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary=\"=-=-=\"
Subject: [PATCH] attached patches

--=-=-=
Content-Type: text/plain

multiple attached

--=-=-=
Content-Type: text/x-diff
Content-Disposition: attachment;
 filename=foo.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 1/3] foo

foo msg
---
 foo | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo

diff --git a/foo b/foo
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/foo
@@ -0,0 +1 @@
+foo

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=
Content-Type: text/x-diff
Content-Disposition: attachment;
 filename=bar.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 2/3] bar

bar msg
---
 bar | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 bar

diff --git a/bar b/bar
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/bar
@@ -0,0 +1 @@
+bar

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=
Content-Type: text/x-diff
Content-Disposition: attachment;
 filename=baz.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 3/3] baz

baz msg
---
 baz | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 baz

diff --git a/baz b/baz
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/baz
@@ -0,0 +1 @@
+baz

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=--
")

(defvar piem-mime-tests-case-attached-multi-unordered "\
From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Message-ID: <mid@example.com>
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary=\"=-=-=\"
Subject: [PATCH] attached patches

--=-=-=
Content-Type: text/plain

multiple attached

--=-=-=
Content-Type: text/x-diff
Content-Disposition: attachment;
 filename=0002-bar.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 2/3] bar

bar msg
---
 bar | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 bar

diff --git a/bar b/bar
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/bar
@@ -0,0 +1 @@
+bar

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=
Content-Type: text/x-diff
Content-Disposition: attachment;
 filename=0001-foo.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 1/3] foo

foo msg
---
 foo | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo

diff --git a/foo b/foo
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/foo
@@ -0,0 +1 @@
+foo

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=
Content-Type: text/x-diff
Content-Disposition: attachment;
 filename=0003-baz.patch

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 3/3] baz

baz msg
---
 baz | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 baz

diff --git a/baz b/baz
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/baz
@@ -0,0 +1 @@
+baz

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=--
")

(defvar piem-mime-tests-case-attached-multi-mbox "\
From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 1/3] foo

foo msg
---
 foo | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo

diff --git a/foo b/foo
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/foo
@@ -0,0 +1 @@
+foo

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 2/3] bar

bar msg
---
 bar | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 bar

diff --git a/bar b/bar
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/bar
@@ -0,0 +1 @@
+bar

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH 3/3] baz

baz msg
---
 baz | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 baz

diff --git a/baz b/baz
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/baz
@@ -0,0 +1 @@
+baz

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

")

(defvar piem-mime-tests-case-attached-no-filename "\
From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Message-ID: <mid@example.com>
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"=-=-=\"
Subject: [PATCH] attached patch

--=-=-=
Content-Type: text/plain; charset=utf-8
Content-Transfer-Encoding: quoted-printable

see attached

--=-=-=
Content-Type: text/x-patch
Content-Disposition: attachment

From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH] foo

msg
---
 foo | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo

diff --git a/foo b/foo
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/foo
@@ -0,0 +1 @@
+foo

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

--=-=-=--
")

(defvar piem-mime-tests-case-attached-no-filename-mbox "\
From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH] foo

msg
---
 foo | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo

diff --git a/foo b/foo
new file mode 100644
index 0000000..257cc56
--- /dev/null
+++ b/foo
@@ -0,0 +1 @@
+foo

base-commit: 8d216326295b197bbb222672b773062e119d1a76
--
2.48.1

")

(defvar piem-mime-tests-case-attached-signed "\
From a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Message-ID: <mid@example.com>
MIME-Version: 1.0
Content-Type: multipart/signed; boundary=\"==-=-=\"
Subject: [PATCH] attached patch

--==-=-=
Content-Type: multipart/mixed; boundary=\"=-=-=\"

--=-=-=
Content-Type: text/plain

see attached

--=-=-=
Content-Type: text/x-diff
Content-Disposition: inline;
 filename=0001-foo.patch
Content-Transfer-Encoding: quoted-printable

From=a9b78777b8bcbc8c9ac41efe0a31a39f554ce3f8 Mon Sep 17 00:00:00 2001
From: Barb <example.com>
Date: Sun, 2 Mar 2025 10:54:36 -0500
Subject: [PATCH] attached patch

=2D--
 foo | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo

diff --git a/foo b/foo
new file mode 100644
index 0000000..257cc56
=2D-- /dev/null
+++ b/foo
@@ -0,0 +1 @@
+foo

base-commit: 8d216326295b197bbb222672b773062e119d1a76
=2D-=20
2.48.1

--=-=-=--

--==-=-=
Content-Type: application/pgp-signature; name=\"signature.asc\"

-----BEGIN PGP SIGNATURE-----

somesig
-----END PGP SIGNATURE-----
--==-=-=--
")

(ert-deftest piem-mime-count-attachments ()
  (should
   (= (piem-mime-count-attachments (piem-mime-tests-dissect ""))
      0))
  (should
   (= (piem-mime-count-attachments (piem-mime-tests-dissect "foo"))
      0))
  (should
   (= (piem-mime-count-attachments
       (piem-mime-tests-dissect piem-mime-tests-case-inline))
      0))
  (should
   (= (piem-mime-count-attachments
       (piem-mime-tests-dissect piem-mime-tests-case-attached-one))
      1))
  (should
   (= (piem-mime-count-attachments
       (piem-mime-tests-dissect piem-mime-tests-case-attached-multi))
      3))
  (should
   (= (piem-mime-count-attachments
       (piem-mime-tests-dissect piem-mime-tests-case-attached-no-filename))
      1))
  (should
   (= (piem-mime-count-attachments
       (piem-mime-tests-dissect piem-mime-tests-case-attached-signed))
      1)))

(ert-deftest piem-mime-am-ready-mbox ()
  (should-not
   (with-temp-buffer
     (insert piem-mime-tests-case-inline)
     (piem-mime-am-ready-mbox)))
  (let ((res (with-temp-buffer
               (insert piem-mime-tests-case-attached-multi)
               (piem-mime-am-ready-mbox))))
    (should (equal (cdr res) "mbox"))
    (should
     (equal (with-temp-buffer
              (funcall (car res))
              (buffer-string))
            piem-mime-tests-case-attached-multi-mbox)))
  (should
   (equal (with-temp-buffer
            (funcall
             (car
              (with-temp-buffer
                (insert piem-mime-tests-case-attached-multi-nonum)
                (piem-mime-am-ready-mbox))))
            (buffer-string))
          piem-mime-tests-case-attached-multi-mbox))
  (should
   (equal (with-temp-buffer
            (funcall
             (car
              (with-temp-buffer
                (insert piem-mime-tests-case-attached-multi-unordered)
                (piem-mime-am-ready-mbox))))
            (buffer-string))
          piem-mime-tests-case-attached-multi-mbox))
  (should
   (equal (with-temp-buffer
            (funcall
             (car
              (with-temp-buffer
                (insert piem-mime-tests-case-attached-no-filename)
                (piem-mime-am-ready-mbox))))
            (buffer-string))
          piem-mime-tests-case-attached-no-filename-mbox)))

(provide 'piem-mime-tests)
;;; piem-mime-tests.el ends here
