;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages enchant)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages check)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (srfi srfi-1))

(define-public enchant
  (package
    (name "enchant")
    (version "2.2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/AbiWord/enchant/releases"
                                  "/download/v" version "/enchant-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "029smcna98hllgkm2gy94qa7qphxs4xaa8cdbg5kaaw16mhrf8hv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static"
                           ;; Tests require a relocatable build.
                           "--enable-relocatable")))
    (inputs
     `(("aspell" ,aspell)))   ;; Currently, the only supported backend in Guix
    (propagated-inputs        ;; is aspell. (This information might be old)
     ;; Required by enchant.pc.
     `(("glib" ,glib)))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("unittest-cpp" ,unittest-cpp)))
    (synopsis "Multi-backend spell-checking library wrapper")
    (description
      "On the surface, Enchant appears to be a generic spell checking library.
Looking closer, you'll see the Enchant is more-or-less a fancy wrapper around
the dlopen() system call.

Enchant steps in to provide uniformity and conformity on top of these libraries,
and implement certain features that may be lacking in any individual provider
library.  Everything should \"just work\" for any and every definition of \"just
working\".")
    (home-page "https://abiword.github.io/enchant/")
    (license lgpl2.1+)))

;; Some packages are not ready for the 2.x API yet, so we keep this version
;; around.  The library and executables of Enchant 2 have been designed not to
;; conflict with 1.x, so it's OK if both end up in the same profile.
(define-public enchant-1.6
  (package
    (inherit enchant)
    (version "1.6.0")
    (arguments '(#:configure-flags '("--disable-static")))
    (native-inputs (alist-delete "unittest-cpp"
                                 (package-native-inputs enchant)))
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.abisource.com/downloads/enchant/"
                                  version "/enchant-" version ".tar.gz"))
              (sha256
               (base32
                "0zq9yw1xzk8k9s6x83n1f9srzcwdavzazn3haln4nhp9wxxrxb1g"))))))
