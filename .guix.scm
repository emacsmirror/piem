;;; Guix package definition for piem development environment

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

;;; Commentary:

;; This file evaluates to a Guix package for piem that specifies
;; inputs needed for development.  Here's one way you can use this to
;; build and enter a development environment:
;;
;;     guix environment --load=.guix.scm

(use-modules (guix build utils)
             (guix build-system emacs)
             (guix build-system python)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages python-web)
             (gnu packages mail)
             (gnu packages texinfo)
             (gnu packages version-control)
             (ice-9 popen)
             (ice-9 rdelim))


;;; Package definitions that should eventually be polished and
;;; submitted upstream.

(define-public piem
  (package
    (name "piem")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kyleam.com/piem.git")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "07jagfxxnj383k48x8wdps160zggpxqknb65qr9f4k4i7hd8nykf"))
       (modules '((guix build utils)))
       ;; TODO: Decide how to deal with these optional libraries.
       (snippet
        '(begin (delete-file "piem-notmuch.el")
                (delete-file "piem-elfeed.el")
                #t))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("b4" ,b4)
       ("emacs" ,emacs)
       ("emacs-transient" ,emacs-transient)))
    (license license:gpl3)
    (home-page "https://git.kyleam.com/piem")
    (synopsis "")
    (description "")))


;;; Development definition for piem

(define %source-dir
  (dirname (current-filename)))

;; Copied from guile-daemon.
(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-port port)
      (string-trim-right output #\newline))))

(let ((commit (git-output "rev-parse" "--short" "HEAD")))
  (package
    (inherit piem)
    (name "piem-dev")
    (version (string-append "000-" commit))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (propagated-inputs
     `(("emacs" ,emacs)
       ("emacs-elfeed" ,emacs-elfeed)
       ("emacs-magit" ,emacs-magit)
       ("git" ,git)
       ("notmuch" ,notmuch)
       ("texinfo" ,texinfo)
       ,@(package-propagated-inputs piem)))))
