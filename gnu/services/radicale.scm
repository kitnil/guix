;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu services radicale)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages dav)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (radicale-service
            radicale-service-type
            radicale-configuration
            radicale-configuration?))

;;; Commentary:
;;;
;;; Radicale Control related services.
;;;
;;; Code:

(define-record-type* <radicale-configuration>
  radicale-configuration
  make-radicale-configuration
  radicale-configuration?
  (package          radicale-configuration-package        ;package
                    (default radicale))
  (port             radicale-configuration-port           ;number | #f
                    (default 5232)))

(define radicale-shepherd-service
  (match-lambda
    (($ <radicale-configuration>
        package port)
     (let* ((radicale     (file-append package "/bin/radicale"))
            (command `(,radicale
                       ,@(if port
                             `(,(string-append
                                 "--port=" (number->string port)))
                             '()))))
       (list (shepherd-service
              (documentation "Run the radicale.")
              (requirement '(networking))
              (provision '(radicale))
              (start #~(make-forkexec-constructor '#$command
                                                  #:user "radicale"
                                                  #:group "radicale"))
              (stop #~(make-kill-destructor))))))))

(define %radicale-accounts
  ;; User account and group for radicale.
  (list (user-group
         (name "radicale")
         (system? #t))
        (user-account
         (name "radicale")
         (system? #t)
         (group "radicale")
         (comment "Radicale daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (radicale-activation config)
  "Return the activation gexp for radicale using CONFIG."
  #~(begin
      (use-modules (guix build utils)) 

(define radicale-service-type
  (service-type
   (name 'radicale)
   (extensions
    (list (service-extension shepherd-root-service-type
                             radicale-shepherd-service)
          (service-extension account-service-type
                             (const %radicale-accounts))
          (service-extension activation-service-type
                             radicale-activation)))))

(define* (radicale-service #:key (config (radicale-configuration)))
  "Return a service that runs @command{radicale daemon}, a simple DNA
server to support CalDav and CardDav protocols."
  (service radicale-service-type config))
