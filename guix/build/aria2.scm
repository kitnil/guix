;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build aria2)
  #:use-module (guix build utils)
  #:use-module (ice-9 format)
  #:export (aria2-fetch))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix aria2-download).  It allows a
;;; aria2 repository to be cloned and checked out at a specific changeset
;;; identifier.
;;;
;;; Code:

(define* (aria2-fetch url directory
                   #:key (aria2-command "aria2"))
  "Fetch CHANGESET from URL into DIRECTORY.  CHANGESET must be a valid
aria2 changeset identifier.  Return #t on success, #f otherwise."

  (mkdir-p directory)

  (with-directory-excursion directory
    (invoke aria2-command
            "--seed-time=0" "--enable-dht=false" "--check-certificate=false" url))

  ;; The contents of '.aria2' vary as a function of the current
  ;; status of the aria2 repo.  Since we want a fixed
  ;; output, this directory needs to be taken out.
  ;; Since the '.aria2' file is also in sub-modules, we have to
  ;; search for it in all sub-directories.
  (for-each delete-file-recursively
            (find-files directory "^\\.aria2$" #:directories? #t))

  #t)

;;; aria2.scm ends here
