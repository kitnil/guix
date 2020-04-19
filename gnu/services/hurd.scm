;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu services hurd)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (hurd-console-configuration
            hurd-console-service-type
            hurd-loopback-service-type
            hurd-service->shepherd-service
            hurd-ttys-configuration
            hurd-ttys-service-type))

;;; Commentary:
;;;
;;; This module implements services for the Hurd.
;;;
;;; Code:

;; XXX Gradually bootstrap (gnu services) framework.
(define (hurd-service->shepherd-service service)
  (let ((config (service-value service)))
    (match config
      (($ <guix-configuration>) (guix-shepherd-service config))
      (($ <hurd-console-configuration>) (hurd-console-shepherd-service config))
      (($ <hurd-ttys-configuration>) (hurd-ttys-shepherd-service config))
      (($ <openssh-configuration>) (openssh-shepherd-service config))
      (($ <syslog-configuration>) (syslog-shepherd-service config))
      (('loopback) (hurd-loopback-shepherd-service #f))
      (('user-processes) (user-processes-shepherd-service '()))
      (_ '()))))

(define (first-of-two first second)
  first)


;;;
;;; Bridge for guix-daemon.
;;;

(define <guix-configuration>
  (@@ (gnu services base) <guix-configuration>))

(define guix-shepherd-service
  (@@ (gnu services base) guix-shepherd-service))


;;;
;;; Bridge for syslog.
;;;

(define <syslog-configuration>
  (@@ (gnu services base) <syslog-configuration>))

(define (syslog-shepherd-service config)
  (append-map (cut <> config)
              (map service-extension-compute
                   (service-type-extensions syslog-service-type))))


;;;
;;; Bridge for user-processes service, required for guix-daemon.
;;;

(define user-processes-shepherd-service
  (@@ (gnu services shepherd) user-processes-shepherd-service))


;;;
;;; Simple wrapper for <hurd>/bin/console.
;;;

(define-record-type* <hurd-console-configuration>
  hurd-console-configuration make-hurd-console-configuration
  hurd-console-configuration?
  (hurd   hurd-console-configuration-hurd ;package
          (default hurd)))

(define (hurd-console-shepherd-service config)
  "Return a <shepherd-service> for a Hurd console with CONFIG."

  (define console-command
    #~(list
       (string-append #$(hurd-console-configuration-hurd config) "/bin/console")
       "-c" "/dev/vcs"
       "-d" "vga"
       "-d" "pc_kbd"
       "-d" "generic_speaker"))

  (list (shepherd-service
         (documentation "Hurd console.")
         (provision '(console))
         (requirement '())
         (start #~(lambda _ (fork+exec-command #$console-command) #t))
         (stop #~(make-kill-destructor)))))

(define hurd-console-service-type
  (service-type
   (name 'console)
   (description
    "Run a hurd console, @command{console}.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             hurd-console-shepherd-service)))
   (compose concatenate)
   (extend first-of-two)
   (default-value (hurd-console-configuration))))


;;;
;;; Dummy hurd-loopback service, required for guix-daemon.
;;;

(define (hurd-loopback-shepherd-service _)
  "Return the 'loopback' Shepherd service."

  (list (shepherd-service
         (documentation "Dummy for bootstrapping (gnu services) on the Hurd.")
         (provision '(loopback))
         (requirement '())
         (start #~(const #t))
         (stop #~(const #t))
         (respawn? #f))))

(define hurd-loopback-service-type
  (service-type
   (name 'loopback)
   (extensions (list (service-extension shepherd-root-service-type
                                        hurd-loopback-shepherd-service)))
   (compose concatenate)
   (extend first-of-two)
   (default-value '(loopback)) ;canary for hurd-service->shepherd-service
   (description "Dummy service to bootstrap (gnu services) on the
Hurd.")))


;;;
;;; Simple wrapper for <hurd>/libexec/runttys.
;;;

(define-record-type* <hurd-ttys-configuration>
  hurd-ttys-configuration make-hurd-ttys-configuration
  hurd-ttys-configuration?
  (hurd   hurd-ttys-configuration-hurd ;package
          (default hurd)))

(define (hurd-ttys-shepherd-service config)
  "Return a <shepherd-service> for the Hurd ttys with CONFIG."

  (define runttys-command
    #~(list
       (string-append #$(hurd-ttys-configuration-hurd config) "/libexec/runttys")))

  (list (shepherd-service
         (documentation "Hurd ttys.")
         (provision '(ttys))
         (requirement '(console))
         (start #~(lambda _ (fork+exec-command #$runttys-command) #t))
         (stop #~(make-kill-destructor)))))

(define hurd-ttys-service-type
  (service-type
   (name 'tty)
   (description
    "Run a hurd ttys, @command{runttys}.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             hurd-ttys-shepherd-service)))
   (compose concatenate)
   (extend first-of-two)
   (default-value (hurd-ttys-configuration))))


;;;
;;; Bridge for OpenSSH.
;;;

(define <openssh-configuration>
  (@@ (gnu services ssh) <openssh-configuration>))

(define openssh-shepherd-service
  (@@ (gnu services ssh) openssh-shepherd-service))

;;; hurd.scm ends here
