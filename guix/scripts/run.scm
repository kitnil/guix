;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts run)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module ((guix build utils) #:select (which mkdir-p))
  #:use-module (gnu build linux-container)
  #:use-module (gnu system file-systems)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (guix-run))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix run")))))

(define (show-help)
  (display (G_ "Usage: guix run PACKAGE COMMAND...
Run COMMAND from PACKAGE in a container.\n"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))



(define (bind-mount-spec/ro item)
  (and (file-exists? item)
       (file-system
         (device item)
         (mount-point item)
         (title 'device)
         (type "none")
         (flags '(bind-mount read-only))
         (check? #f))))

(define (bind-mount-spec/rw item)
  (and (file-exists? item)
       (file-system
         (inherit (bind-mount-spec/ro item))
         (flags '(bind-mount)))))

(define %writable-things
  (list "/var/run/nscd/socket"
        (string-append (getenv "HOME") "/.Xauthority")
        "/tmp/.X11-unix"
        "/etc/machine-id"))

(define (guix-run . args)
  (with-error-handling
    (match (reverse (parse-command-line args %options '(())
                                        #:argument-handler cons))
      ((spec command args ...)
       (with-store store
         (let-values (((package output)
                       (specification->package+output spec)))
           (let* ((drv    (package-derivation store package))
                  (prefix (derivation->output-path drv output)))
             (show-what-to-build store (list drv))
             (build-derivations store (list drv))

             (let* ((items (requisites store (list prefix)))
                    (env   (environ))
                    (full  (search-path (list (string-append prefix "/bin")
                                              (string-append prefix "/sbin"))
                                        command)))
               (unless full
                 (leave (G_ "command '~a' not found in package '~a'~%")
                        command (package-name package)))

               (call-with-container
                   (append (filter-map bind-mount-spec/ro items)
                           (filter-map bind-mount-spec/rw %writable-things))
                 (lambda ()
                   (environ env)                  ;TODO: filter ENV
                   (mkdir-p (getenv "HOME"))

                   (newline)
                   (catch #t
                     (lambda ()
                       (apply execl full command args))
                     (lambda (key . args)
                       (print-exception (current-error-port) #f key args)
                       (exit 1))))
                 #:namespaces (delq 'net %namespaces))))))))))
