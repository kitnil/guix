;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Brant Gardner <brantcgardner@brantware.com>
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

(define-module (gnu packages distributed)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public boinc-client
  (package
    (name "boinc-client")
    (version "7.16.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/boinc/boinc.git")
                    (commit (string-append "client_release/"
                                           "7.16/"
                                           version))))
              (sha256
               (base32
                "0w2qimcwyjhapk3z7zyq7jkls23hsnmm35iw7m4s4if04fp70dx0"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--disable-server")))
    (inputs `(("openssl" ,openssl)
              ("curl" ,curl)
              ("wxwidgets" ,wxwidgets)
              ("gtk+" ,gtk+)
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("libnotify" ,libnotify)
              ("sqlite" ,sqlite)
              ("python" ,python)
              ("python-pyserial" ,python-pyserial)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (synopsis "BOINC lets you help cutting-edge science research using your computer")
    (description "BOINC is a platform for high-throughput computing on a large
scale (thousands or millions of computers).  It can be used for volunteer
computing (using consumer devices) or grid computing (using organizational
resources).  It supports virtualized, parallel, and GPU-based applications.

BOINC is distributed under the LGPL open source license.  It can be used for
commercial purposes, and applications need not be open source.")
    (home-page "https://boinc.berkeley.edu/")
    (license license:gpl3+)))

(define-public boinc-server
  (package (inherit boinc-client)
    (name "boinc-server")
    (arguments '(#:configure-flags '("--disable-client" "--disable-manager")
                 #:parallel-build? #f
                 #:tests? #f)) ; FIXME: Looks like bad test syntax in the
                               ; source package, 2 tests fail.  Disable for
                               ; now.
    (inputs `(("openssl" ,openssl)
              ("curl" ,curl)
              ("mariadb" ,mariadb)
              ("zlib" ,zlib)))
    (propagated-inputs `(("python" ,python-wrapper)
                         ("perl" ,perl)))))
