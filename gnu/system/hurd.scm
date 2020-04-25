;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu system hurd)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages less)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services hurd)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:export (cross-hurd-image
            hurd-grub-configuration-file
            hurd-grub-minimal-bootloader
            %base-packages/hurd
            %base-services/hurd
            %hurd-default-operating-system))

;;; Commentary:
;;;
;;; This module provides tools to (cross-)build GNU/Hurd virtual machine
;;; images.
;;;
;;; Code:

;; XXX: Surely this belongs in (guix profiles), but perhaps we need high-level
;; <profile> objects so one can specify hooks, etc.?
(define-gexp-compiler (compile-manifest (manifest
                                         (@@ (guix profiles) <manifest>))
                                        system target)
  "Lower MANIFEST as a profile."
  (profile-derivation manifest
                      #:system system
                      #:target target))

;; XXX: We will replace this by addding (gnu services shepherd).
(define shepherd-configuration-file
  (@@ (gnu services shepherd) shepherd-configuration-file))

(define %base-packages/hurd
  (list hurd bash coreutils file findutils grep sed
        guile-3.0 guile-colorized guile-readline guix
        inetutils less net-base openssh shepherd which))

(define %base-services/hurd
  (list (service hurd-console-service-type
                 (hurd-console-configuration (hurd hurd)))
        (service hurd-ttys-service-type
                 (hurd-ttys-configuration (hurd hurd)))
        (service hurd-loopback-service-type)
        (syslog-service)
        (service guix-service-type
                 (guix-configuration
                  (guix guix)
                  (extra-options '("--disable-chroot"
                                   "--disable-deduplication"
                                   "--max-jobs=1"))))))

(define* (hurd-grub-configuration-file config entries
                                       #:key
                                       (system "i586-pc-gnu")
                                       (old-entries '()))
  (let ((hurd (if (equal? (%current-system) "i586-pc-gnu")
                  hurd
                  (with-parameters ((%current-target-system system))
                    hurd)))
        (mach (with-parameters ((%current-system "i686-linux"))
                gnumach))
        (libc (if (equal? system "i586-pc-gnu")
                  glibc
                  (cross-libc system))))
    (computed-file "grub.cfg"
                   #~(call-with-output-file #$output
                       (lambda (port)
                         (format port "
set timeout=2
search.file ~a/boot/gnumach

menuentry \"GNU\" {
  multiboot ~a/boot/gnumach root=device:hd0s1
  module ~a/hurd/ext2fs.static ext2fs \\
    --multiboot-command-line='${kernel-command-line}' \\
    --host-priv-port='${host-port}' \\
    --device-master-port='${device-port}' \\
    --exec-server-task='${exec-task}' -T typed '${root}' \\
    '$(task-create)' '$(task-resume)'
  module ~a/lib/ld.so.1 exec ~a/hurd/exec '$(exec-task=task-create)'
}\n"
                                 #+mach #+mach #+hurd
                                 #+libc #+hurd))))))

(define hurd-grub-minimal-bootloader
  (bootloader
   (inherit grub-minimal-bootloader)
   (configuration-file-generator hurd-grub-configuration-file)))

(define %hurd-default-operating-system
  (operating-system
    (kernel hurd)
    ;; (kernel-loadable-modules '())
    (kernel-arguments '())
    (bootloader (bootloader-configuration
                 (bootloader hurd-grub-minimal-bootloader)
                 (target "/dev/vda")))
    (label (lambda _ "label"))
    (initrd (lambda _ '()))
    (initrd-modules (lambda _ '()))
    (firmware '())
    (host-name "guixygnu")
    ;; (hosts-file #F)
    ;; (mapped-devices '())
    (file-systems '())
    ;; (swap-devices '())
    (users '())
    ;(groups '())
    (skeletons '())
    ;; (issue %default-issue)
    (packages %base-packages/hurd)
    (timezone "GNUrope")
    ;; (locale "en_US.utf8")
    (locale-definitions '())
    ;; (locale-libcs '())
    (name-service-switch #f)
    (essential-services (hurd-default-essential-services this-operating-system))
    (services (cons (service openssh-service-type
                             (openssh-configuration
                              (use-pam? #f)
                              (openssh openssh)
                              (port-number 2222)
                              (permit-root-login #t)
                              (allow-empty-passwords? #t)
                              (password-authentication? #t)))
                    %base-services/hurd))
    (pam-services '())
    (setuid-programs '())
    (sudoers-file #f)))

(define (input->packages input)
  "Return the list of packages in INPUT."
  (match input
    ((label (and (? package?) package) . output)
     (list package))
    (_ '())))

(define %hurd-os-development
  (operating-system
    (inherit %hurd-default-operating-system)
    (packages
     (append
      (list git-minimal)
      (append-map input->packages
                  (fold alist-delete (package-direct-inputs guix)
                        '("graphviz" "po4a")))
      (list gawk diffutils gnu-make m4 tar xz)
      %base-packages/hurd))))

(define operating-system-accounts
  (@@ (gnu system) operating-system-accounts))

(define operating-system-etc-directory
  (@@ (gnu system) operating-system-etc-directory))

(define (hurd-shepherd-services os)
  (append-map hurd-service->shepherd-service (operating-system-services os)))

(define* (cross-hurd-image #:key (hurd hurd) (gnumach gnumach) (os %hurd-default-operating-system))
  "Return a cross-built GNU/Hurd image."

  (define (cross-built thing)
    (with-parameters ((%current-target-system "i586-pc-gnu"))
      thing))

  (define (cross-built-entry entry)
    (manifest-entry
      (inherit entry)
      (item (cross-built (manifest-entry-item entry)))
      (dependencies (map cross-built-entry
                         (manifest-entry-dependencies entry)))))

  (define (cross-bootstrap thing)
    (with-parameters ((%current-system "i586-gnu"))
      thing))

  (define (cross-bootstrap-entry entry)
    (manifest-entry
      (inherit entry)
      (item (cross-bootstrap (manifest-entry-item entry)))))

  (define system-profile
    (concatenate-manifests
     (list (map-manifest-entries cross-built-entry
                                 (packages->manifest (operating-system-packages os)))
           (map-manifest-entries cross-bootstrap-entry
                                 (packages->manifest (list %bootstrap-gcc
                                                           %bootstrap-binutils
                                                           %bootstrap-glibc))))))

  (define grub.cfg
    (hurd-grub-configuration-file #f '() #:system "i586-pc-gnu"))

  (define fstab
    (plain-file "fstab"
                "# This file was generated from your Guix configuration.  Any changes
# will be lost upon reboot or reconfiguration.

/dev/hd0s1	/	ext2	defaults
"))

  (define passwd
    (plain-file "passwd"
                "root:x:0:0:root:/root:/bin/sh
guixbuilder:x:1:1:guixbuilder:/var/empty:/bin/no-sh
sshd:x:2:2:sshd:/var/empty:/bin/no-sh
"))

  (define group
    (plain-file "group"
                "guixbuild:x:1:guixbuilder
"))

  (define shadow
    (plain-file "shadow"
                "root::17873::::::
"))

  (define shepherd.conf
    (with-parameters ((%current-target-system "i586-pc-gnu"))
      (shepherd-configuration-file (hurd-shepherd-services os))))

  (define boot-activation
    (with-parameters ((%current-target-system "i586-pc-gnu"))
      (operating-system-activation-script os)))

  (define root-.gitconfig
    (plain-file "root-.gitconfig"
                "\
[url \"git+ssh://git.sv.gnu.org/srv/git/\"]
	insteadof = gnu-ssh:
[url \"git://git.savannah.gnu.org/\"]
	insteadof = gnu:
")) ;" help Emacs

  (define hurd-directives
    `((directory "/servers")
      ,@(map (lambda (server)
               `(file ,(string-append "/servers/" server)))
             '("startup" "exec" "proc" "password"
               "default-pager" "crash-dump-core"
               "kill" "suspend"))
      ("/servers/crash" -> "crash-dump-core")
      (directory "/servers/socket")
      (file "/servers/socket/1")
      (file "/servers/socket/2")
      (file "/servers/socket/16")
      ("/servers/socket/local" -> "1")
      ("/servers/socket/inet" -> "2")
      ("/servers/socket/inet6" -> "16")
      (directory "/boot")
      ("/boot/grub.cfg" -> ,grub.cfg)   ;XXX: not strictly needed
      ("/boot/activation" -> ,boot-activation)
      ("/hurd" -> ,(file-append (with-parameters ((%current-target-system
                                                   "i586-pc-gnu"))
                                  hurd)
                                "/hurd"))

      ;; TODO: Create those during activation, eventually.
      (directory "/root")
      (file "/root/.guile"
            ,(object->string
              '(begin
                 (use-modules (ice-9 readline) (ice-9 colorized))
                 (activate-readline) (activate-colorized))))
      (directory "/run")
      (directory "/run/current-system")
      ("/run/current-system/profile" -> ,system-profile)
      ("/etc/fstab" -> ,fstab)
      ;; Hmm, someone runs chown which before (or while?) we run /boot/activation
      ("/etc/group" -> ,group)
      ("/etc/passwd" -> ,passwd)
      ("/etc/shadow" -> ,shadow)
      (file "/etc/resolv.conf"
            "nameserver 10.0.2.3\n")
      ("/etc/motd" -> ,(file-append (with-parameters ((%current-target-system
                                                       "i586-pc-gnu"))
                                      hurd)
                                    "/etc/motd"))
      ("/etc/login" -> ,(file-append (with-parameters ((%current-target-system
                                                        "i586-pc-gnu"))
                                       hurd)
                                     "/etc/login"))
      ;; XXX can we instead, harmlessly set _PATH_TTYS (from glibc) in runttys.c?
      ("/etc/ttys" -> ,(file-append (with-parameters ((%current-target-system
                                                       "i586-pc-gnu"))
                                      hurd)
                                    "/etc/ttys"))
      (directory "/etc/ssh")
      ("/etc/shepherd.conf" -> ,shepherd.conf)
      ("/bin/sh" -> ,(file-append (with-parameters ((%current-target-system
                                                     "i586-pc-gnu"))
                                    bash)
                                  "/bin/sh"))
      ("/root/.gitconfig" -> ,root-.gitconfig)))

  (qemu-image #:file-system-type "ext2"
              #:file-system-options '("-o" "hurd")
              #:device-nodes 'hurd
              #:inputs `(("system" ,system-profile)
                         ("grub.cfg" ,grub.cfg)
                         ("fstab" ,fstab)
                         ("passwd" ,passwd)
                         ("group" ,group)
                         ("shadow" ,shadow)
                         ("shepherd.conf" ,shepherd.conf)
                         ("boot-activation" ,boot-activation)
                         ("root-.gitconfig" ,root-.gitconfig))
              #:copy-inputs? #t
              #:os system-profile
              #:bootcfg-drv grub.cfg
              #:bootloader grub-bootloader
              #:register-closures? #f
              #:device-nodes 'hurd
              #:disk-image-size (* 10 (expt 2 30)) ;10GiB
              #:extra-directives hurd-directives))

;; Return this thunk so one can type "guix build -f gnu/system/hurd.scm".
cross-hurd-image

;; Return the development thunk so "guix build -f gnu/system/hurd.scm"
(lambda _ (cross-hurd-image #:os %hurd-os-development))
