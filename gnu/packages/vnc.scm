;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu packages vnc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public tigervnc
  (let ((commit "6f2301d08e64a965ad36b401ec8dc2b24bc47075"))
    (package
      (name "tigervnc")
      (version (git-version "1.9.0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/TigerVNC/tigervnc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0b47fg3741qs3zdpl2zr0s6jz46dypp2j6gqrappbzm3ywnnmm1x"))))
      (build-system cmake-build-system)
      (inputs
       `(("bigreqsproto" ,bigreqsproto)
         ("compositeproto" ,compositeproto)
         ("damageproto" ,damageproto)
         ("fixesproto" ,fixesproto)
         ("fltk" ,fltk)
         ("fontsproto" ,fontsproto)
         ("font-util" ,font-util)
         ("gettext" ,gnu-gettext)
         ("glproto" ,glproto)
         ("glu" ,glu)
         ("gnutls" ,gnutls)
         ("libice" ,libice)
         ("libjpeg_turbo" ,libjpeg-turbo)
         ("libpciaccess" ,libpciaccess)
         ("libsm" ,libsm)
         ("libx11" ,libx11)
         ("libxext" ,libxext)
         ("libxfont2" ,libxfont2)
         ("libxft" ,libxft)
         ("libxi" ,libxi)
         ("libxkbfile" ,libxkbfile)
         ("libxtst" ,libxtst)
         ("linux-pam" ,linux-pam)
         ("nettle" ,nettle)
         ("pixman" ,pixman)
         ("presentproto" ,presentproto)
         ("randrproto" ,randrproto)
         ("renderproto" ,renderproto)
         ("resourceproto" ,resourceproto)
         ("scrnsaverproto" ,scrnsaverproto)
         ("util-macros" ,util-macros)
         ("videoproto" ,videoproto)
         ("xcmiscproto" ,xcmiscproto)
         ("xineramaproto" ,xineramaproto)
         ("zlib" ,zlib)))
      (arguments
       `(#:tests? #f))
      (home-page "http://www.tigervnc.org/")
      (synopsis "Fork of tightVNC, made in cooperation with VirtualGL")
      (description "This package provides a fork of tightVNC, made in cooperation with VirtualGL")
      (license license:gpl2+))))
