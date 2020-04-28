;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu system image)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu image)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system uuid)
  #:use-module (gnu system vm)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages genimage)
  #:use-module (gnu packages guile)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module ((srfi srfi-1) #:prefix scm:)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export (esp-partition
            root-partition

            efi-disk-image
            iso9660-image

            find-image
            system-image))


;;;
;;; Images definitions.
;;;

(define esp-partition
  (partition
   (size (* 40 (expt 2 20)))
   (label "GNU-ESP") ;cosmetic only
   ;; Use "vfat" here since this property is used
   ;; when mounting. The actual FAT-ness is based
   ;; on file system size (16 in this case).
   (file-system "vfat")
   (flags '(esp))
   (initializer (gexp initialize-efi-partition))))

(define root-partition
  (partition
   (size 'guess)
   (label "Guix_image")
   (file-system "ext4")
   (flags '(boot))
   (initializer (gexp initialize-root-partition))))

(define efi-disk-image
  (image
   (format 'disk-image)
   (partitions (list esp-partition root-partition))))

(define iso9660-image
  (image
   (format 'iso9660)
   (partitions
    (list (partition
           (size 'guess)
           (label "GUIX_IMAGE")
           (flags '(boot)))))
   ;; XXX: Temporarily disable compression to speed-up the tests.
   (compression? #f)))


;;
;; Helpers.
;;

(define (root-uuid image)
  ;; UUID of the root file system, computed in a deterministic fashion.
  ;; This is what we use to locate the root file system so it has to be
  ;; different from the user's own file system UUIDs.
  (let ((os (image-operating-system image))
        (type (if (eq? (image-format image) 'iso9660)
                  'iso9660
                  'dce)))
    (operating-system-uuid os type)))

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix rest ...) #t)
    (('gnu rest ...) #t)
    (rest #f)))

(define (partition->gexp partition)
  "Turn PARTITION, a <partition> object, into a list-valued gexp suitable for
'make-partition-image'."
  #~'(#$@(list (partition-size partition))
      #$(partition-file-system partition)
      #$(partition-label partition)
      #$(and=> (partition-uuid partition)
               uuid-bytevector)))

(define gcrypt-sqlite3&co
  ;; Guile-Gcrypt, Guile-SQLite3, and their propagated inputs.
  (scm:append-map
   (lambda (package)
     (cons package
           (match (package-transitive-propagated-inputs package)
             (((labels packages) ...)
              packages))))
   (list guile-gcrypt guile-sqlite3)))

(define-syntax-rule (with-imported-modules* exp ...)
  (with-extensions gcrypt-sqlite3&co
    (with-imported-modules `(,@(source-module-closure
                                '((gnu build vm)
                                  (gnu build image)
                                  (guix store database))
                                #:select? not-config?)
                             ((guix config) => ,(make-config.scm)))
      #~(begin
          (use-modules (gnu build vm)
                       (gnu build image)
                       (guix store database)
                       (guix build utils))
          exp ...))))


;;
;; Disk image.
;;

(define* (system-disk-image image
                            #:key
                            (name "disk-image")
                            bootcfg
                            bootloader
                            register-closures?
                            (inputs '()))
  "Return the derivation of the disk-image described by IMAGE. Said image can
be copied on a USB stick as is. BOOTLOADER is the bootloader that will be
installed and configured according to BOOTCFG parameter.

Raw images of the IMAGE partitions are first created. Then, genimage is used
to assemble the partition images into a disk-image without resorting to a
virtual machine.

INPUTS is a list of inputs (as for packages).  When REGISTER-CLOSURES? is
true, register INPUTS in the store database of the image so that Guix can be
used in the image."

  (define genimage-name "image")

  (define (image->genimage-cfg image)
    "Return as a file-like object, the genimage configuration file describing
the given IMAGE."
    (define (format->image-type format)
      "Return the genimage format corresponding to FORMAT. For now, only the
hdimage format (raw disk-image) is supported."
      (case format
        ((disk-image) "hdimage")
        (else
         (error
          (format #f "Unsupported image type ~a~%." format)))))

    (define (partition->dos-type partition)
      "Return the MBR partition type corresponding to the given PARTITION.
See: https://en.wikipedia.org/wiki/Partition_type."
      (let ((flags (partition-flags partition)))
        (cond
         ((member 'esp flags) "0xEF")
         (else "0x83"))))

    (define (partition-image partition)
      "Return as a file-like object, an image of the given PARTITION.  A
directory, filled by calling the PARTITION initializer procedure, is first
created within the store.  Then, an image of this directory is created using
tools such as 'mke2fs' or 'mkdosfs', depending on the partition file-system
type."
      (let* ((os (image-operating-system image))
             (schema (local-file (search-path %load-path
                                              "guix/store/schema.sql")))
             (graph (match inputs
                      (((names . _) ...)
                       names)))
             (root-builder
              (with-imported-modules*
               (let* ((initializer #$(partition-initializer partition)))
                 (sql-schema #$schema)

                 ;; Allow non-ASCII file names--e.g., 'nss-certs'--to be
                 ;; decoded.
                 (setenv "GUIX_LOCPATH"
                         #+(file-append glibc-utf8-locales "/lib/locale"))
                 (setlocale LC_ALL "en_US.utf8")

                 (initializer #$output
                              #:references-graphs '#$graph
                              #:deduplicate? #f
                              #:system-directory #$os
                              #:bootloader-package
                              #$(bootloader-package bootloader)
                              #:bootcfg #$bootcfg
                              #:bootcfg-location
                              #$(bootloader-configuration-file bootloader)))))
             (image-root
              (computed-file "partition-image-root" root-builder
                             #:options `(#:references-graphs ,inputs)))
             (type (partition-file-system partition))
             (image-builder
              (with-imported-modules*
               (let ((inputs '#$(list e2fsprogs dosfstools mtools)))
                 (set-path-environment-variable "PATH" '("bin" "sbin") inputs)
                 (make-partition-image #$(partition->gexp partition)
                                       #$output
                                       #$image-root)))))
        (computed-file "partition.img" image-builder)))

    (define (partition->config partition)
      "Return the genimage partition configuration for PARTITION."
      (let ((label (partition-label partition))
            (dos-type (partition->dos-type partition))
            (image (partition-image partition)))
        #~(format #f "~/partition ~a {
~/~/partition-type = ~a
~/~/image = \"~a\"
~/}"  #$label #$dos-type #$image)))

    (let* ((format (image-format image))
           (image-type (format->image-type format))
           (partitions (image-partitions image))
           (partitions-config (map partition->config partitions))
           (builder
            #~(begin
                (let ((format (@ (ice-9 format) format)))
                  (call-with-output-file #$output
                    (lambda (port)
                      (format port
                              "\
image ~a {
~/~a {}
~{~a~^~%~}
}~%" #$genimage-name #$image-type (list #$@partitions-config))))))))
      (computed-file "genimage.cfg" builder)))

  (let* ((substitutable? (image-substitutable? image))
         (builder
          (with-imported-modules*
           (let ((inputs '#$(list genimage coreutils findutils)))
             (set-path-environment-variable "PATH" '("bin" "sbin") inputs)
             (genimage #$(image->genimage-cfg image) #$output))))
         (image-dir (computed-file "image-dir" builder)))
    (gexp->derivation name
                      #~(symlink
                         (string-append #$image-dir "/" #$genimage-name)
                         #$output)
                      #:substitutable? substitutable?)))


;;
;; ISO9660 image.
;;

(define (has-guix-service-type? os)
  "Return true if OS contains a service of the type GUIX-SERVICE-TYPE."
  (not (not (scm:find (lambda (service)
                        (eq? (service-kind service) guix-service-type))
                      (operating-system-services os)))))

(define* (system-iso9660-image image
                               #:key
                               (name "iso9660-image")
                               bootcfg
                               bootloader
                               register-closures?
                               (inputs '())
                               (grub-mkrescue-environment '()))
  "Return a bootable, stand-alone iso9660 image.

INPUTS is a list of inputs (as for packages).  When REGISTER-CLOSURES? is
true, register INPUTS in the store database of the image so that Guix can be
used in the image. "
  (define root-label
    (match (image-partitions image)
      ((partition)
       (partition-label partition))))

  (define root-uuid
    (match (image-partitions image)
      ((partition)
       (uuid-bytevector (partition-uuid partition)))))

  (let* ((os (image-operating-system image))
         (bootloader (bootloader-package bootloader))
         (compression? (image-compression? image))
         (substitutable? (image-substitutable? image))
         (schema (local-file (search-path %load-path
                                          "guix/store/schema.sql")))
         (graph (match inputs
                  (((names . _) ...)
                   names)))
         (root-builder
          (with-imported-modules*
           (sql-schema #$schema)

           ;; Allow non-ASCII file names--e.g., 'nss-certs'--to be decoded.
           (setenv "GUIX_LOCPATH"
                   #+(file-append glibc-utf8-locales "/lib/locale"))
           (setlocale LC_ALL "en_US.utf8")

           (initialize-root-partition #$output
                                      #:references-graphs '#$graph
                                      #:deduplicate? #f
                                      #:system-directory #$os)))
         (image-root
          (computed-file "image-root" root-builder
                         #:options `(#:references-graphs ,inputs)))
         (builder
          (with-imported-modules*
           (let* ((inputs '#$(list parted e2fsprogs dosfstools xorriso
                                   sed grep coreutils findutils gawk)))
             (set-path-environment-variable "PATH" '("bin" "sbin") inputs)
             (make-iso9660-image #$xorriso
                                 '#$grub-mkrescue-environment
                                 #$bootloader
                                 #$bootcfg
                                 #$os
                                 #$image-root
                                 #$output
                                 #:references-graphs '#$graph
                                 #:register-closures? #$register-closures?
                                 #:compression? #$compression?
                                 #:volume-id #$root-label
                                 #:volume-uuid #$root-uuid)))))
    (gexp->derivation name builder
                      #:references-graphs inputs
                      #:substitutable? substitutable?)))


;;
;; Image creation.
;;

(define (root-partition? partition)
  "Return true if PARTITION is the root partition, false otherwise."
  (member 'boot (partition-flags partition)))

(define (find-root-partition image)
  "Return the root partition of the given IMAGE."
  (scm:find root-partition? (image-partitions image)))

(define (image->root-file-system image)
  "Return the IMAGE root partition file-system type."
  (let ((format (image-format image)))
    (if (eq? format 'iso9660)
        "iso9660"
        (partition-file-system (find-root-partition image)))))

(define (root-size image)
  "Return the root partition size of IMAGE."
  (let* ((image-size (image-size image))
         (root-partition (find-root-partition image))
         (root-size (partition-size root-partition)))
    (cond
     ((and (eq? root-size 'guess) image-size)
      image-size)
     (else root-size))))

(define* (image-with-os base-image os
                        #:key root-uuid root-size)
  "Return an image based on BASE-IMAGE but with the operating-system field set
to OS. Also set the UUID and the size of the root partition to ROOT-UUID and
ROOT-SIZE respectively."
  (let*-values (((partitions) (image-partitions base-image))
                ((root-partition other-partitions)
                 (scm:partition root-partition? partitions)))
    (image
     (inherit base-image)
     (operating-system os)
     (partitions
      (cons (partition
             (inherit (car root-partition))
             (uuid root-uuid)
             (size root-size))
            other-partitions)))))

(define* (make-system-image image)
  "Return the derivation of IMAGE. It can be a raw disk-image or an ISO9660
image, depending on IMAGE format."
  (let* ((image-os (image-operating-system image))
         (format (image-format image))
         (file-systems-to-keep
          (scm:remove
           (lambda (fs)
             (string=? (file-system-mount-point fs) "/"))
           (operating-system-file-systems image-os)))
         (root-file-system-type (image->root-file-system image))
         (substitutable? (image-substitutable? image))
         (volatile-root? (image-volatile-root? image))
         (os (operating-system
               (inherit image-os)
               (initrd (lambda (file-systems . rest)
                         (apply (operating-system-initrd image-os)
                                file-systems
                                #:volatile-root? volatile-root?
                                rest)))
               (bootloader (if (eq? format 'iso9660)
                               (bootloader-configuration
                                (inherit
                                 (operating-system-bootloader image-os))
                                (bootloader grub-mkrescue-bootloader))
                               (operating-system-bootloader image-os)))
               (file-systems (cons (file-system
                                     (mount-point "/")
                                     (device "/dev/placeholder")
                                     (type root-file-system-type))
                                   file-systems-to-keep))))
         (uuid (root-uuid image))
         (os (operating-system
               (inherit os)
               (file-systems (cons (file-system
                                     (mount-point "/")
                                     (device uuid)
                                     (type root-file-system-type))
                                   file-systems-to-keep))))
         (image* (image-with-os image os
                                #:root-uuid uuid
                                #:root-size (root-size image)))
         (register-closures? (has-guix-service-type? os))
         (bootcfg (operating-system-bootcfg os))
         (bootloader (bootloader-configuration-bootloader
                      (operating-system-bootloader os))))
    (case (image-format image)
      ((disk-image)
       (system-disk-image image*
                          #:bootcfg bootcfg
                          #:bootloader bootloader
                          #:register-closures? register-closures?
                          #:inputs `(("system" ,os)
                                     ("bootcfg" ,bootcfg))))
      ((iso9660)
       (system-iso9660-image image*
                             #:bootcfg bootcfg
                             #:bootloader bootloader
                             #:register-closures? register-closures?
                             #:inputs `(("system" ,os)
                                        ("bootcfg" ,bootcfg))
                             #:grub-mkrescue-environment
                             '(("MKRESCUE_SED_MODE" . "mbr_hfs")))))))

(define (find-image file-system-type)
  "Find and return an image that could match the given FILE-SYSTEM-TYPE. This
is useful to adapt to interfaces written before the addition of the <image>
record."
  ;; XXX: Add support for system and target here, or in the caller.
  (match file-system-type
    ("iso9660" iso9660-image)
    (_ efi-disk-image)))

(define (system-image image)
  "Wrap 'make-system-image' call, so that it is used only if the given IMAGE
is supported. Otherwise, fallback to image creation in a VM. This is temporary
and should be removed once 'make-system-image' is able to deal with all types
of images."
  (let* ((image-os (image-operating-system image))
         (image-root-filesystem-type (image->root-file-system image))
         (bootloader (bootloader-configuration-bootloader
                      (operating-system-bootloader image-os)))
         (bootloader-name (bootloader-name bootloader))
         (size (image-size image))
         (substitutable? (image-substitutable? image))
         (volatile? (image-volatile-root? image))
         (format (image-format image)))
    (if (and (or (eq? bootloader-name 'grub)
                 (eq? bootloader-name 'extlinux))
             (eq? format 'disk-image))
        ;; Fallback to image creation in a VM when it is not yet supported by
        ;; this module.
        (system-disk-image-in-vm image-os
                                 #:disk-image-size size
                                 #:file-system-type image-root-filesystem-type
                                 #:volatile? volatile?
                                 #:substitutable? substitutable?)
        (make-system-image image))))

;;; image.scm ends here
