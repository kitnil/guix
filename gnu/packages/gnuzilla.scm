;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
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

(define-module (gnu packages gnuzilla)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libreoffice)  ;for hunspell
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite))

(define-public mozjs
  (package
    (name "mozjs")
    (version "17.0.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/js/"
                   name version ".tar.gz"))
             (sha256
              (base32
               "1fig2wf4f10v43mqx67y68z6h77sy900d1w0pz9qarrqx57rc7ij"))
             (patches (search-patches "mozjs17-aarch64-support.patch"))
             (modules '((guix build utils)))
             (snippet
              ;; Fix incompatibility with Perl 5.22+.
              '(begin
                 (substitute* '("js/src/config/milestone.pl")
                   (("defined\\(@TEMPLATE_FILE)") "@TEMPLATE_FILE"))
                 #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (propagated-inputs
     `(("nspr" ,nspr))) ; in the Requires.private field of mozjs-17.0.pc
    (inputs
     `(("zlib" ,zlib)))
    (arguments
     `(;; XXX: parallel build fails, lacking:
       ;;   mkdir -p "system_wrapper_js/"
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-timedout-test
           ;; This test times out on slower hardware.
           (lambda _
             (delete-file "js/src/jit-test/tests/basic/bug698584.js")
             #t))
         (add-before 'configure 'chdir
           (lambda _
             (chdir "js/src")
             #t))
         (replace 'configure
           ;; configure fails if it is followed by SHELL and CONFIG_SHELL
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))
               (invoke "./configure" (string-append "--prefix=" out)
                       ,@(if (string=? "aarch64-linux"
                                       (%current-system))
                             '("--host=aarch64-unknown-linux-gnu")
                             '()))))))))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey")
    (synopsis "Mozilla javascript engine")
    (description "SpiderMonkey is Mozilla's JavaScript engine written
in C/C++.")
    (license license:mpl2.0))) ; and others for some files

(define-public mozjs-24
  (package (inherit mozjs)
    (name "mozjs")
    (version "24.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ftp.mozilla.org/pub/mozilla.org/js/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1n1phk8r3l8icqrrap4czplnylawa0ddc2cc4cgdz46x3lrkybz6"))
              (modules '((guix build utils)))
              (patches (search-patches "mozjs24-aarch64-support.patch"))
              (snippet
               ;; Fix incompatibility with Perl 5.22+.
               '(begin
                  (substitute* '("js/src/config/milestone.pl")
                    (("defined\\(@TEMPLATE_FILE)") "@TEMPLATE_FILE"))
                  #t))))
    (arguments
      (substitute-keyword-arguments (package-arguments mozjs)
        ((#:phases phases)
         `(modify-phases ,phases
            (replace 'configure
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  ;; configure fails if it is followed by SHELL and CONFIG_SHELL
                  (setenv "SHELL" (which "sh"))
                  (setenv "CONFIG_SHELL" (which "sh"))
                  (invoke "./configure"
                          (string-append "--prefix=" out)
                          "--with-system-nspr"
                          "--enable-system-ffi"
                          "--enable-threadsafe"
                          ,@(if (string=? "aarch64-linux"
                                          (%current-system))
                                '("--host=aarch64-unknown-linux-gnu")
                                '())))))))))
    (inputs
     `(("libffi" ,libffi)
       ("zlib" ,zlib)))))

(define-public mozjs-38
  (package
    (inherit mozjs)
    (name "mozjs")
    (version "38.2.1.rc0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://anduin.linuxfromscratch.org/BLFS/mozjs/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0p4bmbpgkfsj54xschcny0a118jdrdgg0q29rwxigg3lh5slr681"))
              (patches
               (search-patches
                ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1269317 for
                ;; GCC 6 compatibility.

                "mozjs38-version-detection.patch" ; for 0ad
                "mozjs38-tracelogger.patch"

                ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1339931.
                "mozjs38-pkg-config-version.patch"
                "mozjs38-shell-version.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Fix incompatibility with sed 4.4.
                  (substitute* "js/src/configure"
                    (("\\^\\[:space:\\]") "^[[:space:]]"))

                  ;; The headers are symlinks to files that are in /tmp, so they
                  ;; end up broken.  Copy them instead.
                  (substitute*
                      "python/mozbuild/mozbuild/backend/recursivemake.py"
                    (("\\['dist_include'\\].add_symlink")
                     "['dist_include'].add_copy"))

                  ;; Remove bundled libraries.
                  (for-each delete-file-recursively
                            '("intl"
                              "js/src/ctypes/libffi"
                              "js/src/ctypes/libffi-patches"
                              "modules/zlib"))
                  #t))))
    (arguments
     `(;; XXX: parallel build fails, lacking:
       ;;   mkdir -p "system_wrapper_js/"
       #:parallel-build? #f
       ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1008470.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (chdir "js/src")
               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       "--enable-ctypes"
                       "--enable-gcgenerational"
                       "--enable-optimize"
                       "--enable-pie"
                       "--enable-readline"
                       "--enable-shared-js"
                       "--enable-system-ffi"
                       "--enable-threadsafe"
                       "--enable-xterm-updates"
                       "--with-system-icu"
                       "--with-system-nspr"
                       "--with-system-zlib"

                       ;; Intl API requires bundled ICU.
                       "--without-intl-api")))))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (inputs
     `(("libffi" ,libffi)
       ("readline" ,readline)
       ("icu4c" ,icu4c)
       ("zlib" ,zlib)))))

(define-public mozjs-52
  ;; No releases yet at <https://archive.mozilla.org/pub/spidermonkey/releases/>.
  ;; While we could take a snapshot of the complete mozilla-esr52 repository at
  ;; <https://treeherder.mozilla.org/#/jobs?repo=mozilla-esr52&filter-searchStr=sm-tc>,
  ;; we take the Debian version instead, because it is easier to work with.
  (let ((commit "6507e63cc416fd7a3269e390efe712f8b56f374a")
        (revision "1"))
    (package (inherit mozjs-38)
      (version (git-version "52.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://salsa.debian.org/gnome-team/mozjs52.git")
                      (commit commit)))
                (file-name (git-file-name "mozjs" version))
                (sha256
                 (base32
                  "1ny0s53r8wn4byys87h784xrq1xg767akmfm6gqrbvrz57mlm3q2"))))
      (arguments
       `(#:tests? #f ; depends on repository metadata
         #:configure-flags
         '("--enable-ctypes"
           "--enable-optimize"
           "--enable-pie"
           "--enable-readline"
           "--enable-shared-js"
           "--enable-system-ffi"
           "--with-system-icu"
           "--with-system-nspr"
           "--with-system-zlib"

           ;; Intl API requires bundled ICU.
           "--without-intl-api"

           ;; Without this gnome-shell will crash at runtime.
           "--disable-jemalloc")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-and-chdir
             (lambda* (#:key inputs #:allow-other-keys)
               ;; This patch prevents a segfault when executing JS_Init().
               ;; The build does not fail without this patch, but the
               ;; configure phase of the gjs package would fail.
               ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1176787
               (make-file-writable "js/src/old-configure.in")
               (make-file-writable "js/src/old-configure")
               (make-file-writable "mozglue/build/moz.build")
               (invoke "patch" "-p1" "--force"
                       "--input" "debian/patches/disable-mozglue.patch")
               (invoke "touch" "js/src/configure")
               (chdir "js/src")
               #t))
           (replace 'configure
             (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
               ;; The configure script does not accept environment variables
               ;; as arguments.
               (let ((out (assoc-ref outputs "out")))
                 (setenv "SHELL" (which "sh"))
                 (setenv "CONFIG_SHELL" (which "sh"))
                 (setenv "AUTOCONF" (string-append (assoc-ref inputs "autoconf")
                                                   "/bin/autoconf"))
                 (apply invoke "./configure"
                        (cons (string-append "--prefix=" out)
                              configure-flags))))))))
      (native-inputs
       `(("autoconf" ,autoconf-2.13)
         ("automake" ,automake)
         ,@(package-native-inputs mozjs-38))))))

(define-public mozjs-60
  ;; No releases yet at <https://archive.mozilla.org/pub/spidermonkey/releases/>.
  ;; While we could take a snapshot of the complete mozilla-esr60 repository at
  ;; <https://treeherder.mozilla.org/#/jobs?repo=mozilla-esr60&filter-searchStr=sm-tc>,
  ;; we take the Debian version instead, because it is easier to work with.
  (package
    (inherit mozjs-38)
    (version "60.2.3-2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/gnome-team/mozjs60.git")
                    (commit (string-append "debian/" version))))
              (file-name (git-file-name "mozjs" version))
              (sha256
               (base32
                "091w050rwzrdcbgyi934k2viyccmlqxrp13sm2mql71mabb5dai6"))))
    (arguments
     `(#:tests? #f ; FIXME: all tests pass, but then the check phase fails anyway.
       #:test-target "check-jstests"
       #:configure-flags
       '("--enable-ctypes"
         "--enable-optimize"
         "--enable-pie"
         "--enable-readline"
         "--enable-shared-js"
         "--enable-system-ffi"
         "--with-system-nspr"
         "--with-system-zlib"
         "--with-system-icu"
         "--with-intl-api"
         ;; This is important because without it gjs will segfault during the
         ;; configure phase.  With jemalloc only the standalone mozjs console
         ;; will work.
         "--disable-jemalloc")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
             ;; The configure script does not accept environment variables as
             ;; arguments.  It also must be run from a different directory,
             ;; but not the root directory either.
             (let ((out (assoc-ref outputs "out")))
               (mkdir "run-configure-from-here")
               (chdir "run-configure-from-here")
               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))
               (setenv "AUTOCONF" (string-append (assoc-ref inputs "autoconf")
                                                 "/bin/autoconf"))
               (apply invoke "../js/src/configure"
                      (cons (string-append "--prefix=" out)
                            configure-flags))
               #t)))
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             ;; This test assumes that /bin exists and contains certain
             ;; executables.
             (delete-file "js/src/tests/shell/os.js")
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("which" ,which)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))))

(define (mozilla-patch file-name changeset hash)
  "Return an origin for CHANGESET from the mozilla-esr60 repository."
  (origin
    (method url-fetch)
    (uri (string-append "https://hg.mozilla.org/releases/mozilla-esr60/raw-rev/"
                        changeset))
    (sha256 (base32 hash))
    (file-name file-name)))

(define* (computed-origin-method gexp-promise hash-algo hash
                                 #:optional (name "source")
                                 #:key (system (%current-system))
                                 (guile (default-guile)))
  "Return a derivation that executes the G-expression that results
from forcing GEXP-PROMISE."
  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "computed-origin")
                      (force gexp-promise)
                      #:graft? #f       ;nothing to graft
                      #:system system
                      #:guile-for-build guile)))

(define %icecat-version "60.9.0-guix1")

;; 'icecat-source' is a "computed" origin that generates an IceCat tarball
;; from the corresponding upstream Firefox ESR tarball, using the 'makeicecat'
;; script from the upstream IceCat project.
(define icecat-source
  (let* ((base-version (first (string-split %icecat-version #\-)))

         (major-version (first  (string-split base-version #\.)))
         (minor-version (second (string-split base-version #\.)))
         (sub-version   (third  (string-split base-version #\.)))

         (upstream-firefox-version (string-append base-version "esr"))
         (upstream-firefox-source
          (origin
            (method url-fetch)
            (uri (string-append
                  "https://ftp.mozilla.org/pub/firefox/releases/"
                  upstream-firefox-version "/source/"
                  "firefox-" upstream-firefox-version ".source.tar.xz"))
            (sha256
             (base32
              "0gy5x2rnnbkqmjd9sq93s3q5na9nkba68xwpizild7k6qn63qicz"))))

         (upstream-icecat-base-version "60.7.0") ; maybe older than base-version
         (upstream-icecat-gnu-version "1")
         (upstream-icecat-version (string-append upstream-icecat-base-version
                                                 "-gnu"
                                                 upstream-icecat-gnu-version))
         (upstream-icecat-source
          (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/gnuzilla/" upstream-icecat-base-version
                  "/icecat-" upstream-icecat-version ".tar.bz2"))
            (sha256
             (base32
              "09xqdfd8rwbn2n6m7n059qf1psbrj5v5kfzm7gg5xng22ddxawv8"))))

         (gnuzilla-commit (string-append "v" upstream-icecat-base-version))
         (gnuzilla-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://git.savannah.gnu.org/gnuzilla.git")
                  (commit gnuzilla-commit)))
            (file-name (git-file-name "gnuzilla" upstream-icecat-base-version))
            (sha256
             (base32
              "1vqhb0py28hnwcynbaad304ziciz1kn5bv1qg2q4f7g13js3b1hf"))))

         (makeicecat-patch
          (local-file (search-patch "icecat-makeicecat.patch"))))

    (origin
      (method computed-origin-method)
      (file-name (string-append "icecat-" %icecat-version ".tar.xz"))
      (sha256 #f)
      (uri
       (delay
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils))
              (let ((firefox-dir
                     (string-append "firefox-" #$base-version))
                    (icecat-dir
                     (string-append "icecat-" #$%icecat-version))
                    (old-icecat-dir
                     (string-append "icecat-" #$upstream-icecat-base-version)))

                (mkdir "/tmp/bin")
                (set-path-environment-variable
                 "PATH" '("bin")
                 (list "/tmp"
                       #+(canonical-package bash)
                       #+(canonical-package coreutils)
                       #+(canonical-package findutils)
                       #+(canonical-package patch)
                       #+(canonical-package xz)
                       #+(canonical-package sed)
                       #+(canonical-package grep)
                       #+(canonical-package bzip2)
                       #+(canonical-package gzip)
                       #+(canonical-package tar)
                       #+rename))

                (symlink #+(file-append rename "/bin/rename")
                         "/tmp/bin/prename")

                ;; We copy the gnuzilla source directory because it is
                ;; read-only in 'gnuzilla-source', and the makeicecat script
                ;; uses "cp -a" to copy parts of it and assumes that the
                ;; copies will be writable.
                (copy-recursively #+gnuzilla-source "/tmp/gnuzilla"
                                  #:log (%make-void-port "w"))

                (with-directory-excursion "/tmp/gnuzilla"
                  (make-file-writable "makeicecat")
                  (invoke "patch" "--force" "--no-backup-if-mismatch"
                          "-p1" "--input" #+makeicecat-patch)
                  (patch-shebang "makeicecat")
                  (substitute* "makeicecat"
                    (("^FFMAJOR=(.*)" all ffmajor)
                     (unless (string=? #$major-version
                                       (string-trim-both ffmajor))
                       ;; The makeicecat script cannot be expected to work
                       ;; properly on a different version of Firefox, even if
                       ;; no errors occur during execution.
                       (error "makeicecat major version mismatch"))
                     (string-append "FFMAJOR=" #$major-version "\n"))
                    (("^FFMINOR=.*")
                     (string-append "FFMINOR=" #$minor-version "\n"))
                    (("^FFSUB=.*")
                     (string-append "FFSUB=" #$sub-version "\n"))
                    (("^GNUVERSION=.*")
                     (string-append "GNUVERSION="
                                    #$upstream-icecat-gnu-version "\n"))
                    (("^DATA=.*")
                     "DATA=/tmp/gnuzilla/data\n")
                    (("^find extensions/gnu/ ")
                     "find extensions/gnu/ | sort ")
                    (("/bin/sed")
                     #+(file-append (canonical-package sed) "/bin/sed"))))

                (format #t "Unpacking upstream firefox tarball...~%")
                (force-output)
                (invoke "tar" "xf" #+upstream-firefox-source)
                (rename-file firefox-dir icecat-dir)

                (with-directory-excursion icecat-dir
                  (mkdir "l10n")
                  (format #t "Running makeicecat script...~%")
                  (force-output)
                  (invoke "bash" "/tmp/gnuzilla/makeicecat")
                  (delete-file-recursively "l10n"))

                (format #t (string-append "Unpacking l10n/* from"
                                          " upstream IceCat tarball...~%"))
                (force-output)
                (unless (string=? icecat-dir old-icecat-dir)
                  (symlink icecat-dir old-icecat-dir))
                (invoke "tar" "xf" #+upstream-icecat-source
                        (string-append old-icecat-dir "/l10n"))

                (format #t "Packing new IceCat tarball...~%")
                (force-output)
                (invoke "tar" "cfa" #$output
                        ;; Avoid non-determinism in the archive.  We set the
                        ;; mtime of files in the archive to early 1980 because
                        ;; the build process fails if the mtime of source
                        ;; files is pre-1980, due to the creation of zip
                        ;; archives.
                        "--mtime=@315619200" ; 1980-01-02 UTC
                        "--owner=root:0"
                        "--group=root:0"
                        "--sort=name"
                        icecat-dir)

                #t))))))))

(define-public icecat
  (package
    (name "icecat")
    (version %icecat-version)
    (source icecat-source)
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("graphite2" ,graphite2)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ("harfbuzz" ,harfbuzz)
       ("hunspell" ,hunspell)
       ("libcanberra" ,libcanberra)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ;; ("libtheora" ,libtheora) ; wants theora-1.2, not yet released
       ("libvorbis" ,libvorbis)
       ("libxft" ,libxft)
       ("libevent" ,libevent)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxcomposite" ,libxcomposite)
       ("libxt" ,libxt)
       ("libffi" ,libffi)
       ("ffmpeg" ,ffmpeg)
       ("libvpx" ,libvpx-1.7)
       ("icu4c" ,icu4c)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)
       ("mit-krb5" ,mit-krb5)
       ;; See <https://bugs.gnu.org/32833>
       ;;   and related comments in the 'remove-bundled-libraries' phase.
       ;; UNBUNDLE-ME! ("nspr" ,nspr)
       ;; UNBUNDLE-ME! ("nss" ,nss)
       ("sqlite" ,sqlite)
       ("startup-notification" ,startup-notification)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     ;; The following patches are specific to the Guix packaging of IceCat,
     ;; and therefore we prefer to leave them out of 'source', which should be
     ;; a tarball suitable for compilation on any system that IceCat supports.
     ;; (Bug fixes and security fixes, however, should go in 'source').
     `(("icecat-avoid-bundled-libraries.patch"
        ,(search-patch "icecat-avoid-bundled-libraries.patch"))
       ("icecat-use-system-graphite2+harfbuzz.patch"
        ,(search-patch "icecat-use-system-graphite2+harfbuzz.patch"))
       ("icecat-use-system-media-libs.patch"
        ,(search-patch "icecat-use-system-media-libs.patch"))

       ("patch" ,(canonical-package patch))

       ;; Icecat 60 checks for rust>=1.24
       ("rust" ,rust-1.24)
       ("cargo" ,rust-1.24 "cargo")
       ("llvm" ,llvm-3.9.1)
       ("clang" ,clang-3.9.1)
       ("perl" ,perl)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("yasm" ,yasm)
       ("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf-2.13)
       ("which" ,which)))
    (arguments
     `(#:tests? #f          ; no check target
       #:out-of-source? #t  ; must be built outside of the source directory

       ;; XXX: There are RUNPATH issues such as
       ;; $prefix/lib/icecat-31.6.0/plugin-container NEEDing libmozalloc.so,
       ;; which is not in its RUNPATH, but they appear to be harmless in
       ;; practice somehow.  See <http://hydra.gnu.org/build/378133>.
       #:validate-runpath? #f

       #:configure-flags `("--enable-default-toolkit=cairo-gtk3"

                           "--with-distribution-id=org.gnu"

                           "--enable-startup-notification"
                           "--enable-pulseaudio"

                           "--disable-tests"
                           "--disable-updater"
                           "--disable-crashreporter"
                           "--disable-maintenance-service"
                           "--disable-eme"
                           "--disable-gconf"

                           ;; Building with debugging symbols takes ~5GiB, so
                           ;; disable it.
                           "--disable-debug"
                           "--disable-debug-symbols"

                           ;; Clang is needed to build Stylo, Mozilla's new
                           ;; CSS engine.  We must specify the clang paths
                           ;; manually, because otherwise the Mozilla build
                           ;; system looks in the directories returned by
                           ;; llvm-config --bindir and llvm-config --libdir,
                           ;; which return paths in the llvm package where
                           ;; clang is not found.
                           ,(string-append "--with-clang-path="
                                           (assoc-ref %build-inputs "clang")
                                           "/bin/clang")
                           ,(string-append "--with-libclang-path="
                                           (assoc-ref %build-inputs "clang")
                                           "/lib")

                           ;; Hack to work around missing
                           ;; "unofficial" branding in icecat.
                           "--enable-official-branding"

                           ;; Avoid bundled libraries.
                           "--with-system-zlib"
                           "--with-system-bz2"
                           "--with-system-jpeg"        ; must be libjpeg-turbo
                           "--with-system-libevent"
                           "--with-system-ogg"
                           "--with-system-vorbis"
                           ;; "--with-system-theora" ; wants theora-1.2, not yet released
                           "--with-system-libvpx"
                           "--with-system-icu"
                           
                           ;; See <https://bugs.gnu.org/32833>
                           ;;   and related comments in the
                           ;;   'remove-bundled-libraries' phase below.
                           ;; UNBUNDLE-ME! "--with-system-nspr"
                           ;; UNBUNDLE-ME! "--with-system-nss"
                           
                           "--with-system-harfbuzz"
                           "--with-system-graphite2"
                           "--enable-system-pixman"
                           "--enable-system-ffi"
                           "--enable-system-hunspell"
                           "--enable-system-sqlite"

                           ;; Fails with "--with-system-png won't work because
                           ;; the system's libpng doesn't have APNG support".
                           ;; According to
                           ;; http://sourceforge.net/projects/libpng-apng/ ,
                           ;; "the Animated Portable Network Graphics (APNG)
                           ;; is an unofficial extension of the Portable
                           ;; Network Graphics (PNG) format";
                           ;; we probably do not wish to support it.
                           ;; "--with-system-png"
                           )

       #:imported-modules ,%cargo-utils-modules ;for `generate-checksums'

       #:modules ((ice-9 ftw)
                  (ice-9 rdelim)
                  (ice-9 match)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-guix-specific-patches
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((patch (string-append (assoc-ref (or native-inputs inputs)
                                                    "patch")
                                         "/bin/patch")))
               (for-each (match-lambda
                           ((label . file)
                            (when (and (string-prefix? "icecat-" label)
                                       (string-suffix? ".patch" label))
                              (format #t "applying '~a'...~%" file)
                              (invoke patch "--force" "--no-backup-if-mismatch"
                                      "-p1" "--input" file))))
                         (or native-inputs inputs)))
             #t))
         (add-after 'apply-guix-specific-patches 'remove-bundled-libraries
           (lambda _
             ;; Remove bundled libraries that we don't use, since they may
             ;; contain unpatched security flaws, they waste disk space and
             ;; memory, and may cause confusion.
             (for-each (lambda (file)
                         (format #t "deleting '~a'...~%" file)
                         (delete-file-recursively file))
                       '(;; FIXME: Removing the bundled icu breaks configure.
                         ;;   * The bundled icu headers are used in some places.
                         ;;   * The version number is taken from the bundled copy.
                         ;;"intl/icu"
                         ;;
                         ;; FIXME: A script from the bundled nspr is used.
                         ;;"nsprpub"
                         ;;
                         ;; FIXME: With the update to IceCat 60, using system NSS
                         ;;        broke certificate validation.  See
                         ;;        <https://bugs.gnu.org/32833>.  For now, we use
                         ;;        the bundled NSPR and NSS.  TODO: Investigate,
                         ;;        and try to unbundle these libraries again.
                         ;; UNBUNDLE-ME! "security/nss"
                         ;;
                         ;; TODO: Use more system media libraries.  See:
                         ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=517422>
                         ;;   * libtheora: esr60 wants v1.2, not yet released.
                         ;;   * soundtouch: avoiding the bundled library would
                         ;;     result in some loss of functionality.  There's
                         ;;     also an issue with exception handling
                         ;;     configuration.  It seems that this is needed in
                         ;;     some moz.build:
                         ;;       DEFINES['ST_NO_EXCEPTION_HANDLING'] = 1
                         ;;   * libopus
                         ;;   * speex
                         ;;
                         "modules/freetype2"
                         "modules/zlib"
                         "modules/libbz2"
                         "ipc/chromium/src/third_party/libevent"
                         "media/libjpeg"
                         "media/libvpx"
                         "media/libogg"
                         "media/libvorbis"
                         ;; "media/libtheora" ; wants theora-1.2, not yet released
                         "media/libtremor"
                         "gfx/harfbuzz"
                         "gfx/graphite2"
                         "js/src/ctypes/libffi"
                         "db/sqlite3"))
             #t))
         (add-after 'remove-bundled-libraries 'link-libxul-with-libraries
           (lambda _
             ;; libxul.so dynamically opens libraries, so here we explicitly
             ;; link them into libxul.so instead.
             ;;
             ;; TODO: It might be preferable to patch in absolute file names in
             ;; calls to dlopen or PR_LoadLibrary, but that didn't seem to
             ;; work.  More investigation is needed.
             (substitute* "toolkit/library/moz.build"
               (("^# This library needs to be last" all)
                (string-append "OS_LIBS += [
    'GL', 'gnome-2', 'canberra', 'Xss', 'cups', 'gssapi_krb5',
    'avcodec', 'avutil', 'pulse' ]\n\n"
                               all)))
             #t))
         (replace 'bootstrap
           (lambda _
             (invoke "sh" "-c" "autoconf old-configure.in > old-configure")
             ;; 'configure' must be newer than 'old-configure.in', or else the
             ;; build system will raise an alarm and abort.
             (invoke "touch" "configure")))
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda _
             (use-modules (guix build cargo-utils))
             (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
               (substitute* '("Cargo.lock" "servo/Cargo.lock")
                 (("(\"checksum .* = )\".*\"" all name)
                  (string-append name "\"" null-hash "\"")))
               (for-each
                (lambda (filename)
                  (delete-file filename)
                  (let ((dir (dirname filename)))
                    (display (string-append
                              "patch-cargo-checksums: generate-checksums for "
                              dir "\n"))
                    (generate-checksums dir)))
                (find-files "third_party/rust" ".cargo-checksum.json")))
             #t))
         (add-before 'configure 'augment-CPLUS_INCLUDE_PATH
           (lambda* (#:key build inputs #:allow-other-keys)
             ;; Here, we add additional entries to CPLUS_INCLUDE_PATH, to work
             ;; around a problem that otherwise occurs when attempting to
             ;; build Stylo, which requires Rust and Clang.  Without these
             ;; additional entries, errors occur during the build indicating
             ;; that the <cstddef> and "c++config.h" headers cannot be found.
             ;; Note that the 'build' keyword argument contains the GNU
             ;; triplet, e.g. "x86_64-unknown-linux-gnu".
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-append gcc "/include/c++" ":"
                                      gcc "/include/c++/" build ":"
                                      (getenv "CPLUS_INCLUDE_PATH"))))
             #t))
         (replace 'configure
           ;; configure does not work followed by both "SHELL=..." and
           ;; "CONFIG_SHELL=..."; set environment variables instead
           (lambda* (#:key outputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (which "bash"))
                    (abs-srcdir (getcwd))
                    (srcdir (string-append "../" (basename abs-srcdir)))
                    (flags `(,(string-append "--prefix=" out)
                             ,(string-append "--with-l10n-base="
                                             abs-srcdir "/l10n")
                             ,@configure-flags)))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)
               (setenv "AUTOCONF" (which "autoconf")) ; must be autoconf-2.13
               (setenv "CC" "gcc")  ; apparently needed when Stylo is enabled
               (mkdir "../build")
               (chdir "../build")
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               (apply invoke bash
                      (string-append srcdir "/configure")
                      flags))))
         (add-before 'configure 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Install the '.desktop' file.
             (let* ((desktop-file "taskcluster/docker/icecat-snap/icecat.desktop")
                    (out          (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (substitute* desktop-file
                 (("^Exec=icecat")     (string-append "Exec=" out "/bin/icecat"))
                 (("IceCat")           "GNU IceCat")
                 (("Icon=.*")          "Icon=icecat\n")
                 (("NewWindow")        "new-window")
                 (("NewPrivateWindow") "new-private-window"))
               (install-file desktop-file applications)
               #t)))
         (add-after 'install-desktop-entry 'install-icons
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "browser/branding/official"
                 (for-each
                  (lambda (file)
                    (let* ((size (string-filter char-numeric? file))
                           (icons (string-append out "/share/icons/hicolor/"
                                                 size "x" size "/apps")))
                      (mkdir-p icons)
                      (copy-file file (string-append icons "/icecat.png"))))
                  '("default16.png" "default22.png" "default24.png"
                    "default32.png" "default48.png" "content/icon64.png"
                    "mozicon128.png" "default256.png"))
                 #t))))
         ;; This fixes the file chooser crash that happens with GTK 3.
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share")))
               (wrap-program (car (find-files lib "^icecat$"))
                 `("XDG_DATA_DIRS" ":" prefix (,gtk-share)))
               #t))))))
    (home-page "https://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))

(define-public conkeror
  ;; The Conkeror web browser relied on XULRunner, which IceCat > 50 no longer
  ;; provides.  See <http://conkeror.org> for the original web page.
  (deprecated-package "conkeror" icecat))
