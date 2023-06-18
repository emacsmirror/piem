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
             (guix gexp)
             (guix git-download)
             (guix packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages texinfo)
             (ice-9 popen)
             (ice-9 rdelim))


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
    (inherit emacs-piem)
    (name "piem-dev")
    (version (string-append "000-" commit))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (propagated-inputs
     `(("emacs" ,emacs)
       ("emacs-magit" ,emacs-magit)
       ("texinfo" ,texinfo)
       ,@(package-propagated-inputs emacs-piem)))))
