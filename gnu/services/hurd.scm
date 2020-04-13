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
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (hurd-console-service-type
            hurd-service->shepherd-service))

;;; Commentary:
;;;
;;; This module implements services for the Hurd.
;;;
;;; Code:

;; XXX Gradually bootstrap (gnu services) framework.
(define (hurd-service->shepherd-service service)
  (let ((config (service-value service)))
    (match config
      (($ <hurd-console-configuration>) (hurd-console-shepherd-service config))
      (_ '()))))

(define (first-of-two first second)
  first)


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

;;; hurd.scm ends here
