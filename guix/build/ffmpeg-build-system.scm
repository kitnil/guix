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

(define-module (guix build ffmpeg-build-system)
  #:use-module (guix build utils)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:export (%standard-phases
            ffmpeg-build))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for video.
;;
;; Code:

(define* (build #:key source inputs #:allow-other-keys)
  (invoke "ffmpeg"
          "-i" source
          "-i" (assoc-ref inputs "audio")
          "-c:v" "copy"
          "-c:a" "copy"
          (strip-store-file-name source))
  #t)

(define %package-path "/share/video/")

(define* (install #:key source inputs outputs #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (package-dir (string-append out %package-path))
         (file (strip-store-file-name source)))
    (mkdir-p package-dir)
    (copy-file file (string-append package-dir file)))
  #t)

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'unpack)
    (delete 'bootstrap)
    (delete 'configure)
    (replace 'build build)
    (delete 'check)
    (replace 'install install)))

(define* (ffmpeg-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given ffmpeg package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; ffmpeg-build-system.scm ends here
