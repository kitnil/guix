;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages logging)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages version-control))

(define-public log4cpp
  (package
    (name "log4cpp")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/log4cpp/log4cpp-"
                                  (version-major+minor version) ".x%20%28new%29"
                                  "/log4cpp-" (version-major+minor version)
                                  "/log4cpp-" version ".tar.gz"))
              (sha256
               (base32
                "07gmr3jyaf2239n9sp6h7hwdz1pv7b7aka8n06gmr2fnlmaymfrc"))))
    (build-system gnu-build-system)
    (synopsis "Log library for C++")
    (description
     "Log4cpp is library of C++ classes for flexible logging to files, syslog,
IDSA and other destinations.  It is modeled after the Log4j Java library,
staying as close to their API as is reasonable.")
    (home-page "http://log4cpp.sourceforge.net/")
    (license license:lgpl2.1+)))

(define-public glog
  (package
    (name "glog")
    (version "0.3.5")
    (home-page "https://github.com/google/glog")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1q6ihk2asbx95a56kmyqwysq1x3grrw9jwqllafaidf0l84f903m"))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (search-patches "glog-gcc-5-demangling.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)                             ;for tests
       ("autoconf" ,autoconf-wrapper)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'add-automake-files
                    (lambda _
                      ;; The 'test-driver' file is a dangling symlink to
                      ;; /usr/share/automake; replace it.  We can't just run
                      ;; 'automake -ac' because it complains about version
                      ;; mismatch, so run the whole thing.
                      (delete-file "test-driver")
                      (delete-file "configure")   ;it's read-only
                      (invoke "autoreconf" "-vfi")))
                  (add-before 'check 'disable-signal-tests
                    (lambda _
                      ;; See e.g. https://github.com/google/glog/issues/219
                      ;; and https://github.com/google/glog/issues/256
                      (substitute* "Makefile"
                        (("\tsignalhandler_unittest_sh") "\t$(EMPTY)"))
                      #t)))))
    (synopsis "C++ logging library")
    (description
     "Google glog is a library that implements application-level logging.
This library provides logging APIs based on C++-style streams and various
helper macros.  You can log a message by simply streaming things to log at a
particular severity level.  It allows logging to be controlled from the
command line.")
    (license license:bsd-3)))

(define-public tailon
  (package
    (name "tailon")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "0wl2wm6p3pc0vkk33s7rzgcfvs9cwxfmlz997pdfhlw72r00l7s5"))))
    (build-system python-build-system)
    (inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-sockjs-tornado" ,python-sockjs-tornado)
       ("python-tornado-http-auth" ,python-tornado-http-auth)
       ("python-tornado" ,python-tornado)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-commands.py
                     (lambda args
                       (substitute* "tailon/commands.py"
                         (("self\\.first_in_path\\('grep'\\)")
                          (string-append"'" (which "grep") "'"))
                         (("self\\.first_in_path\\('gawk', 'awk'\\)")
                          (string-append"'" (which "gawk") "'"))
                         (("self\\.first_in_path\\('gsed', 'sed'\\)")
                          (string-append"'" (which "sed") "'"))
                         (("self\\.first_in_path\\('gtail', 'tail'\\)")
                          (string-append"'" (which "tail") "'")))
                       #t)))))
    (home-page "https://tailon.readthedocs.io/")
    (synopsis
     "Webapp for looking at and searching through log files")
    (description
     "Tailon provides a web interface around the tail, grep, awk and sed
commands, displaying the results via a web interface.")
    (license license:bsd-3)))

(define-public multitail
  (package
    (name "multitail")
    (version "6.4.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://vanheusden.com/multitail/multitail-"
                          version ".tgz"))
      (sha256
       (base32
        "1zd1r89xkxngl1pdrvsc877838nwkfqkbcgfqm3vglwalxc587dg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             "PREFIX="
             (string-append "DESTDIR="
                            (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-curses-lib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "mt.h"
                 (("ncursesw\\/panel.h") "panel.h")
                 (("ncursesw\\/ncurses.h") "ncurses.h")))
             #t))
         (delete 'configure))
       #:tests? #f)) ; no test suite (make check just runs cppcheck)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://vanheusden.com/multitail/")
    (synopsis "Monitor multiple logfiles")
    (description
     "MultiTail allows you to monitor logfiles and command output in multiple
windows in a terminal, colorize, filter and merge.")
    (license license:gpl2+)))

(define-public collectd
  (package
    (name "collectd")
    (version "5.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://collectd.org/files/"
                                  name ".tar.bz2"))
              
              (sha256
               (base32
                "1j8mxgfq8039js2bscphd6cnriy35hk4jrxfjz5k6mghpdvg8vxh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://collectd.org")
    (synopsis "Daemon which collects system performance statistics periodically")
    (description "Daemon which collects system performance statistics periodically")
    (license license:expat)))

(define-public go-github-com-collectd
  (let ((commit "c2e6ad57f9a2ad14d791164bd4217392483f4603")
        (revision "0"))
      (package
        (name "go-github-com-collectd")
        (version (git-version "0.3.0" revision commit))
        (source (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/collectd/go-collectd")
                        (commit commit)))
                  (file-name (string-append name "-" version "-checkout"))
                  (sha256
                   (base32
                    "0rr9rnc777jk27a7yxhdb7vgkj493158a8k6q44x51s30dkp78x3"))))
        (build-system go-build-system)
        (arguments
         `(#:import-path "github.com/collectd/go-collectd"
           ;; We don't need to install the source code for end-user applications.
           #:install-source? #f
           ;; #:phases
           ;; (modify-phases %standard-phases
           ;;   (replace 'build
           ;;     (lambda* (#:key import-path #:allow-other-keys)
           ;;       (with-directory-excursion (string-append "src/" import-path)
           ;;         (invoke "go" "install"
           ;;                 "-v"  ; print the name of packages as they are compiled
           ;;                 "-x"  ; print each command as it is invoked
           ;;                 "-ldflags=-s -w" ; strip the symbol table and debug
           ;;                 "./cmd/telegraf")))))
           ))
        (home-page "https://collectd.org")
        (synopsis "Utilities for using collectd together with Golang")
        (description synopsis)
        (license license:expat))))

(define-public telegraf
  (let ((commit "578db7ef5156628efac1ffe98ca2b2a9cbfd89b2")
        (revision "0"))
    (package
      (name "telegraf")
      (version (git-version "1.7.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url
                       "https://github.com/influxdata/telegraf")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07n2198hps2dnf2z8aj7xvb5fz2iaqlnpnz6li3bz90q9g104arm"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/influxdata/telegraf"
         ;; We don't need to install the source code for end-user applications.
         #:install-source? #f
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key import-path #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 (invoke "go" "install"
                         "-v"  ; print the name of packages as they are compiled
                         "-x"  ; print each command as it is invoked
                         "-ldflags=-s -w" ; strip the symbol table and debug
                         "./cmd/telegraf")))))))
      (home-page "https://www.influxdata.com/time-series-platform/telegraf/")
      (synopsis "The plugin-driven server agent for collecting & reporting metrics")
      (description "The plugin-driven server agent for collecting & reporting metrics.")
      (license license:expat))))
