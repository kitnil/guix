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

(define-module (guix aria2-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:autoload   (guix build-system gnu) (standard-packages)
  #:use-module (ice-9 match)
  #:export (aria2-reference
            aria2-reference?
            aria2-reference-url
            aria2-reference-changeset
            aria2-reference-recursive?

            aria2-fetch))

(define (aria2-package)
  "Return the default aria2 package."
  (let ((distro (resolve-interface '(gnu packages bittorrent))))
    (module-ref distro 'aria2)))

(define* (aria2-fetch uri hash-algo hash
                   #:optional name
                   #:key (system (%current-system)) (guile (default-guile))
                   (aria2 (aria2-package)))
  (define zlib
    (module-ref (resolve-interface '(gnu packages compression)) 'zlib))

  (define config.scm
    (scheme-file "config.scm"
                 #~(begin
                     (define-module (guix config)
                       #:export (%libz))

                     (define %libz
                       #+(file-append zlib "/lib/libz")))))

  (define modules
    (cons `((guix config) => ,config.scm)
          (delete '(guix config)
                  (source-module-closure '((guix build aria2)
                                           (guix build download-nar))))))

  (define build
    (with-imported-modules modules
      #~(begin
          (use-modules (guix build aria2)
                       (guix build download-nar))

          (or (aria2-fetch '#$uri
                           #$output
                           #:aria2-command (string-append #+aria2 "/bin/aria2c"))
              (download-nar #$output)))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "aria2-checkout") build
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:local-build? #t
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

;;; aria2-download.scm ends here
