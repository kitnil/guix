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

(define-module (guix youtube-dl-download)
  #:autoload   (guix build-system gnu) (standard-packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:export (youtube-dl-reference
            youtube-dl-reference?
            youtube-dl-reference-url
            youtube-dl-reference-format

            youtube-dl-fetch))

(define-record-type* <youtube-dl-reference>
  youtube-dl-reference make-youtube-dl-reference
  youtube-dl-reference?
  (url    youtube-dl-reference-url)
  (format youtube-dl-reference-format))

(define (youtube-dl-package)
  "Return the default youtube-dl package."
  (let ((distro (resolve-interface '(gnu packages video))))
    (module-ref distro 'youtube-dl)))

(define* (youtube-dl-fetch uri hash-algo hash
                   #:optional name
                   #:key (system (%current-system)) (guile (default-guile))
                   (youtube-dl (youtube-dl-package)))
  "Return a fixed-output derivation that fetches URL (a string), which is
expected to have hash HASH of type HASH-ALGO (a symbol).  By default, the file
name is the base name of URL with a dropped query; optionally, NAME can
specify a different file name."
  (define file-name
    (let ((name (basename (youtube-dl-reference-url uri))))
      (if (string-prefix? "watch?v=" name)
          (string-drop name (string-length "watch?v="))
          name)))

  (define build
    (with-imported-modules (source-module-closure '((guix build youtube-dl)))
      #~(begin
          (use-modules (guix build youtube-dl))
          (youtube-dl-fetch '#$(youtube-dl-reference-url uri)
                            #$(youtube-dl-reference-format uri)
                            #$output
                            #:youtube-dl-command (string-append #+youtube-dl "/bin/youtube-dl")))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name file-name) build
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #f
                      #:guile-for-build guile
                      #:local-build? #t
                      #:substitutable? #f)))

;;; youtube-dl-download.scm ends here
