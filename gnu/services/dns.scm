;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
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

(define-module (gnu services dns)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages dns)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (knot-service-type
            knot-acl-configuration
            knot-key-configuration
            knot-keystore-configuration
            knot-zone-configuration
            knot-remote-configuration
            knot-policy-configuration
            knot-configuration
            define-zone-entries
            zone-file
            zone-entry

            dnsmasq-service-type
            dnsmasq-configuration))

;;;
;;; Knot DNS.
;;;

(define-record-type* <knot-key-configuration>
  knot-key-configuration make-knot-key-configuration
  knot-key-configuration?
  (id        knot-key-configuration-id
             (default ""))
  (algorithm knot-key-configuration-algorithm
             (default #f)); one of #f, or an algorithm name
  (secret    knot-key-configuration-secret
             (default "")))

(define-record-type* <knot-acl-configuration>
  knot-acl-configuration make-knot-acl-configuration
  knot-acl-configuration?
  (id      knot-acl-configuration-id
           (default ""))
  (address knot-acl-configuration-address
           (default '()))
  (key     knot-acl-configuration-key
           (default '()))
  (action  knot-acl-configuration-action
           (default '()))
  (deny?   knot-acl-configuration-deny?
           (default #f)))

(define-record-type* <zone-entry>
  zone-entry make-zone-entry
  zone-entry?
  (name  zone-entry-name
         (default "@"))
  (ttl   zone-entry-ttl
         (default ""))
  (class zone-entry-class
         (default "IN"))
  (type  zone-entry-type
         (default "A"))
  (data  zone-entry-data
         (default "")))

(define-record-type* <zone-file>
  zone-file make-zone-file
  zone-file?
  (entries zone-file-entries
           (default '()))
  (origin  zone-file-origin
           (default ""))
  (ns      zone-file-ns
           (default "ns"))
  (mail    zone-file-mail
           (default "hostmaster"))
  (serial  zone-file-serial
           (default 1))
  (refresh zone-file-refresh
           (default (* 2 24 3600)))
  (retry   zone-file-retry
           (default (* 15 60)))
  (expiry  zone-file-expiry
           (default (* 2 7 24 3600)))
  (nx      zone-file-nx
           (default 3600)))
(define-record-type* <knot-keystore-configuration>
  knot-keystore-configuration make-knot-keystore-configuration
  knot-keystore-configuration?
  (id knot-keystore-configuration-id
      (default ""))
  (backend knot-keystore-configuration-backend
           (default 'pem))
  (config  knot-keystore-configuration-config
           (default "/var/lib/knot/keys/keys")))

(define-record-type* <knot-policy-configuration>
  knot-policy-configuration make-knot-policy-configuration
  knot-policy-configuration?
  (id                   knot-policy-configuration-id
                        (default ""))
  (keystore             knot-policy-configuration-keystore
                        (default "default"))
  (manual?              knot-policy-configuration-manual?
                        (default #f))
  (single-type-signing? knot-policy-configuration-single-type-signing?
                        (default #f))
  (algorithm            knot-policy-configuration-algorithm
                        (default "ecdsap256sha256"))
  (ksk-size             knot-policy-configuration-ksk-size
                        (default 256))
  (zsk-size             knot-policy-configuration-zsk-size
                        (default 256))
  (dnskey-ttl           knot-policy-configuration-dnskey-ttl
                        (default 'default))
  (zsk-lifetime         knot-policy-configuration-zsk-lifetime
                        (default (* 30 24 3600)))
  (propagation-delay    knot-policy-configuration-propagation-delay
                        (default (* 24 3600)))
  (rrsig-lifetime       knot-policy-configuration-rrsig-lifetime
                        (default (* 14 24 3600)))
  (rrsig-refresh        knot-policy-configuration-rrsig-refresh
                        (default (* 7 24 3600)))
  (nsec3?               knot-policy-configuration-nsec3?
                        (default #f))
  (nsec3-iterations     knot-policy-configuration-nsec3-iterations
                        (default 5))
  (nsec3-salt-length    knot-policy-configuration-nsec3-salt-length
                        (default 8))
  (nsec3-salt-lifetime  knot-policy-configuration-nsec3-salt-lifetime
                        (default (* 30 24 3600))))

(define-record-type* <knot-zone-configuration>
  knot-zone-configuration make-knot-zone-configuration
  knot-zone-configuration?
  (domain           knot-zone-configuration-domain
                    (default ""))
  (file             knot-zone-configuration-file
                    (default "")) ; the file where this zone is saved.
  (zone             knot-zone-configuration-zone
                    (default (zone-file))) ; initial content of the zone file
  (master           knot-zone-configuration-master
                    (default '()))
  (ddns-master      knot-zone-configuration-ddns-master
                    (default #f))
  (notify           knot-zone-configuration-notify
                    (default '()))
  (acl              knot-zone-configuration-acl
                    (default '()))
  (semantic-checks? knot-zone-configuration-semantic-checks?
                    (default #f))
  (disable-any?     knot-zone-configuration-disable-any?
                    (default #f))
  (zonefile-sync    knot-zone-configuration-zonefile-sync
                    (default 0))
  (dnssec-policy    knot-zone-configuration-dnssec-policy
                    (default #f))
  (serial-policy    knot-zone-configuration-serial-policy
                    (default 'increment)))

(define-record-type* <knot-remote-configuration>
  knot-remote-configuration make-knot-remote-configuration
  knot-remote-configuration?
  (id  knot-remote-configuration-id
       (default ""))
  (address knot-remote-configuration-address
           (default '()))
  (via     knot-remote-configuration-via
           (default '()))
  (key     knot-remote-configuration-key
           (default #f)))

(define-record-type* <knot-configuration>
  knot-configuration make-knot-configuration
  knot-configuration?
  (knot          knot-configuration-knot
                 (default knot))
  (run-directory knot-configuration-run-directory
                 (default "/var/run/knot"))
  (listen-v4     knot-configuration-listen-v4
                 (default "0.0.0.0"))
  (listen-v6     knot-configuration-listen-v6
                 (default "::"))
  (listen-port   knot-configuration-listen-port
                 (default 53))
  (keys          knot-configuration-keys
                 (default '()))
  (keystores     knot-configuration-keystores
                 (default '()))
  (acls          knot-configuration-acls
                 (default '()))
  (remotes       knot-configuration-remotes
                 (default '()))
  (policies      knot-configuration-policies
                 (default '()))
  (zones         knot-configuration-zones
                 (default '())))

(define-syntax define-zone-entries
  (syntax-rules ()
    ((_ id (name ttl class type data) ...)
     (define id (list (make-zone-entry name ttl class type data) ...)))))

(define (error-out msg)
  (raise (condition (&message (message msg)))))

(define (verify-knot-key-configuration key)
  (unless (knot-key-configuration? key)
    (error-out "keys must be a list of only knot-key-configuration."))
  (let ((id (knot-key-configuration-id key)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "key id must be a non empty string.")))
  (unless (memq '(#f hmac-md5 hmac-sha1 hmac-sha224 hmac-sha256 hmac-sha384 hmac-sha512)
                (knot-key-configuration-algorithm key))
          (error-out "algorithm must be one of: #f, 'hmac-md5, 'hmac-sha1,
'hmac-sha224, 'hmac-sha256, 'hmac-sha384 or 'hmac-sha512")))

(define (verify-knot-keystore-configuration keystore)
  (unless (knot-keystore-configuration? keystore)
    (error-out "keystores must be a list of only knot-keystore-configuration."))
  (let ((id (knot-keystore-configuration-id keystore)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "keystore id must be a non empty string.")))
  (unless (memq '(pem pkcs11)
                (knot-keystore-configuration-backend keystore))
          (error-out "backend must be one of: 'pem or 'pkcs11")))

(define (verify-knot-policy-configuration policy)
  (unless (knot-policy-configuration? policy)
    (error-out "policies must be a list of only knot-policy-configuration."))
  (let ((id (knot-policy-configuration-id policy)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "policy id must be a non empty string."))))

(define (verify-knot-acl-configuration acl)
  (unless (knot-acl-configuration? acl)
    (error-out "acls must be a list of only knot-acl-configuration."))
  (let ((id (knot-acl-configuration-id acl))
        (address (knot-acl-configuration-address acl))
        (key (knot-acl-configuration-key acl))
        (action (knot-acl-configuration-action acl)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "acl id must be a non empty string."))
    (unless (and (list? address)
                 (fold (lambda (x1 x2) (and (string? x1) (string? x2))) "" address))
      (error-out "acl address must be a list of strings.")))
  (unless (boolean? (knot-acl-configuration-deny? acl))
    (error-out "deny? must be #t or #f.")))

(define (verify-knot-zone-configuration zone)
  (unless (knot-zone-configuration? zone)
    (error-out "zones must be a list of only knot-zone-configuration."))
  (let ((domain (knot-zone-configuration-domain zone)))
    (unless (and (string? domain) (not (equal? domain "")))
      (error-out "zone domain must be a non empty string."))))

(define (verify-knot-remote-configuration remote)
  (unless (knot-remote-configuration? remote)
    (error-out "remotes must be a list of only knot-remote-configuration."))
  (let ((id (knot-remote-configuration-id remote)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "remote id must be a non empty string."))))

(define (verify-knot-configuration config)
  (unless (package? (knot-configuration-knot config))
    (error-out "knot configuration field must be a package."))
  (unless (string? (knot-configuration-run-directory config))
    (error-out "run-directory must be a string."))
  (unless (list? (knot-configuration-keys config))
    (error-out "keys must be a list of knot-key-configuration."))
  (for-each (lambda (key) (verify-knot-key-configuration key))
            (knot-configuration-keys config))
  (unless (list? (knot-configuration-keystores config))
    (error-out "keystores must be a list of knot-keystore-configuration."))
  (for-each (lambda (keystore) (verify-knot-keystore-configuration keystore))
            (knot-configuration-keystores config))
  (unless (list? (knot-configuration-acls config))
    (error-out "acls must be a list of knot-acl-configuration."))
  (for-each (lambda (acl) (verify-knot-acl-configuration acl))
            (knot-configuration-acls config))
  (unless (list? (knot-configuration-zones config))
    (error-out "zones must be a list of knot-zone-configuration."))
  (for-each (lambda (zone) (verify-knot-zone-configuration zone))
            (knot-configuration-zones config))
  (unless (list? (knot-configuration-policies config))
    (error-out "policies must be a list of knot-policy-configuration."))
  (for-each (lambda (policy) (verify-knot-policy-configuration policy))
            (knot-configuration-policies config))
  (unless (list? (knot-configuration-remotes config))
    (error-out "remotes must be a list of knot-remote-configuration."))
  (for-each (lambda (remote) (verify-knot-remote-configuration remote))
            (knot-configuration-remotes config))
  #t)

(define (format-string-list l)
  "Formats a list of string in YAML"
  (if (eq? l '())
      ""
      (let ((l (reverse l)))
        (string-append
          "["
          (fold (lambda (x1 x2)
                  (string-append (if (symbol? x1) (symbol->string x1) x1) ", "
                                 (if (symbol? x2) (symbol->string x2) x2)))
                (car l) (cdr l))
          "]"))))

(define (knot-acl-config acls)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (acl-config)
          (let ((id (knot-acl-configuration-id acl-config))
                (address (knot-acl-configuration-address acl-config))
                (key (knot-acl-configuration-key acl-config))
                (action (knot-acl-configuration-action acl-config))
                (deny? (knot-acl-configuration-deny? acl-config)))
            (format #t "    - id: ~a\n" id)
            (unless (eq? address '())
              (format #t "      address: ~a\n" (format-string-list address)))
            (unless (eq? key '())
              (format #t "      key: ~a\n" (format-string-list key)))
            (unless (eq? action '())
              (format #t "      action: ~a\n" (format-string-list action)))
            (format #t "      deny: ~a\n" (if deny? "on" "off"))))
        acls))))

(define (knot-key-config keys)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (key-config)
          (let ((id (knot-key-configuration-id key-config))
                (algorithm (knot-key-configuration-algorithm key-config))
                (secret (knot-key-configuration-secret key-config)))
            (format #t     "    - id: ~a\n" id)
            (if algorithm
                (format #t "      algorithm: ~a\n" (symbol->string algorithm)))
            (format #t     "      secret: ~a\n" secret)))
        keys))))

(define (knot-keystore-config keystores)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (keystore-config)
          (let ((id (knot-keystore-configuration-id keystore-config))
                (backend (knot-keystore-configuration-backend keystore-config))
                (config (knot-keystore-configuration-config keystore-config)))
            (format #t "    - id: ~a\n" id)
            (format #t "      backend: ~a\n" (symbol->string backend))
            (format #t "      config: \"~a\"\n" config)))
        keystores))))

(define (knot-policy-config policies)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (policy-config)
          (let ((id (knot-policy-configuration-id policy-config))
                (keystore (knot-policy-configuration-keystore policy-config))
                (manual? (knot-policy-configuration-manual? policy-config))
                (single-type-signing? (knot-policy-configuration-single-type-signing?
                                        policy-config))
                (algorithm (knot-policy-configuration-algorithm policy-config))
                (ksk-size (knot-policy-configuration-ksk-size policy-config))
                (zsk-size (knot-policy-configuration-zsk-size policy-config))
                (dnskey-ttl (knot-policy-configuration-dnskey-ttl policy-config))
                (zsk-lifetime (knot-policy-configuration-zsk-lifetime policy-config))
                (propagation-delay (knot-policy-configuration-propagation-delay
                                     policy-config))
                (rrsig-lifetime (knot-policy-configuration-rrsig-lifetime
                                  policy-config))
                (nsec3? (knot-policy-configuration-nsec3? policy-config))
                (nsec3-iterations (knot-policy-configuration-nsec3-iterations
                                    policy-config))
                (nsec3-salt-length (knot-policy-configuration-nsec3-salt-length
                                     policy-config))
                (nsec3-salt-lifetime (knot-policy-configuration-nsec3-salt-lifetime
                                       policy-config)))
            (format #t "    - id: ~a\n" id)
            (format #t "      keystore: ~a\n" keystore)
            (format #t "      manual: ~a\n" (if manual? "on" "off"))
            (format #t "      single-type-signing: ~a\n" (if single-type-signing?
                                                             "on" "off"))
            (format #t "      algorithm: ~a\n" algorithm)
            (format #t "      ksk-size: ~a\n" (number->string ksk-size))
            (format #t "      zsk-size: ~a\n" (number->string zsk-size))
            (unless (eq? dnskey-ttl 'default)
              (format #t "      dnskey-ttl: ~a\n" dnskey-ttl))
            (format #t "      zsk-lifetime: ~a\n" zsk-lifetime)
            (format #t "      propagation-delay: ~a\n" propagation-delay)
            (format #t "      rrsig-lifetime: ~a\n" rrsig-lifetime)
            (format #t "      nsec3: ~a\n" (if nsec3? "on" "off"))
            (format #t "      nsec3-iterations: ~a\n"
                    (number->string nsec3-iterations))
            (format #t "      nsec3-salt-length: ~a\n"
                    (number->string nsec3-salt-length))
            (format #t "      nsec3-salt-lifetime: ~a\n" nsec3-salt-lifetime)))
        policies))))

(define (knot-remote-config remotes)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (remote-config)
          (let ((id (knot-remote-configuration-id remote-config))
                (address (knot-remote-configuration-address remote-config))
                (via (knot-remote-configuration-via remote-config))
                (key (knot-remote-configuration-key remote-config)))
            (format #t "    - id: ~a\n" id)
            (unless (eq? address '())
              (format #t "      address: ~a\n" (format-string-list address)))
            (unless (eq? via '())
              (format #t "      via: ~a\n" (format-string-list via)))
            (if key
              (format #t "      key: ~a\n" key))))
        remotes))))

(define (serialize-zone-entries entries)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (entry)
          (let ((name (zone-entry-name entry))
                (ttl (zone-entry-ttl entry))
                (class (zone-entry-class entry))
                (type (zone-entry-type entry))
                (data (zone-entry-data entry)))
            (format #t "~a ~a ~a ~a ~a\n" name ttl class type data)))
        entries))))

(define (serialize-zone-file zone domain)
  (computed-file (string-append domain ".zone")
    #~(begin
        (call-with-output-file #$output
          (lambda (port)
            (format port "$ORIGIN ~a.\n"
                    #$(zone-file-origin zone))
            (format port "@ IN SOA ~a ~a (~a ~a ~a ~a ~a)\n"
                    #$(zone-file-ns zone)
                    #$(zone-file-mail zone)
                    #$(zone-file-serial zone)
                    #$(zone-file-refresh zone)
                    #$(zone-file-retry zone)
                    #$(zone-file-expiry zone)
                    #$(zone-file-nx zone))
            (format port "~a\n"
                    #$(serialize-zone-entries (zone-file-entries zone))))))))

(define (knot-zone-config zone)
  (let ((content (knot-zone-configuration-zone zone)))
    #~(with-output-to-string
        (lambda ()
          (let ((domain #$(knot-zone-configuration-domain zone))
                (file #$(knot-zone-configuration-file zone))
                (master (list #$@(knot-zone-configuration-master zone)))
                (ddns-master #$(knot-zone-configuration-ddns-master zone))
                (notify (list #$@(knot-zone-configuration-notify zone)))
                (acl (list #$@(knot-zone-configuration-acl zone)))
                (semantic-checks? #$(knot-zone-configuration-semantic-checks? zone))
                (disable-any? #$(knot-zone-configuration-disable-any? zone))
                (dnssec-policy #$(knot-zone-configuration-dnssec-policy zone))
                (serial-policy '#$(knot-zone-configuration-serial-policy zone)))
            (format #t "    - domain: ~a\n" domain)
            (if (eq? master '())
                ;; This server is a master
                (if (equal? file "")
                  (format #t "      file: ~a\n"
                    #$(serialize-zone-file content
                                           (knot-zone-configuration-domain zone)))
                  (format #t "      file: ~a\n" file))
                ;; This server is a slave (has masters)
                (begin
                  (format #t "      master: ~a\n"
                          #$(format-string-list
                              (knot-zone-configuration-master zone)))
                  (if ddns-master (format #t "      ddns-master ~a\n" ddns-master))))
            (unless (eq? notify '())
              (format #t "      notify: ~a\n"
                      #$(format-string-list
                          (knot-zone-configuration-notify zone))))
            (unless (eq? acl '())
              (format #t "      acl: ~a\n"
                      #$(format-string-list
                          (knot-zone-configuration-acl zone))))
            (format #t "      semantic-checks: ~a\n" (if semantic-checks? "on" "off"))
            (format #t "      disable-any: ~a\n" (if disable-any? "on" "off"))
            (if dnssec-policy
                (begin
                  (format #t "      dnssec-signing: on\n")
                  (format #t "      dnssec-policy: ~a\n" dnssec-policy)))
            (format #t "      serial-policy: ~a\n"
                    (symbol->string serial-policy)))))))

(define (knot-config-file config)
  (verify-knot-configuration config)
  (computed-file "knot.conf"
    #~(begin
        (call-with-output-file #$output
          (lambda (port)
            (format port "server:\n")
            (format port "    rundir: ~a\n" #$(knot-configuration-run-directory config))
            (format port "    user: knot\n")
            (format port "    listen: ~a@~a\n"
                    #$(knot-configuration-listen-v4 config)
                    #$(knot-configuration-listen-port config))
            (format port "    listen: ~a@~a\n"
                    #$(knot-configuration-listen-v6 config)
                    #$(knot-configuration-listen-port config))
            (format port "\nkey:\n")
            (format port #$(knot-key-config (knot-configuration-keys config)))
            (format port "\nkeystore:\n")
            (format port #$(knot-keystore-config (knot-configuration-keystores config)))
            (format port "\nacl:\n")
            (format port #$(knot-acl-config (knot-configuration-acls config)))
            (format port "\nremote:\n")
            (format port #$(knot-remote-config (knot-configuration-remotes config)))
            (format port "\npolicy:\n")
            (format port #$(knot-policy-config (knot-configuration-policies config)))
            (unless #$(eq? (knot-configuration-zones config) '())
              (format port "\nzone:\n")
              (format port "~a\n"
                      (string-concatenate
                        (list #$@(map knot-zone-config
                                      (knot-configuration-zones config)))))))))))

(define %knot-accounts
  (list (user-group (name "knot") (system? #t))
        (user-account
          (name "knot")
          (group "knot")
          (system? #t)
          (comment "knot dns server user")
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))

(define (knot-activation config)
  #~(begin
      (use-modules (guix build utils))
      (define (mkdir-p/perms directory owner perms)
        (mkdir-p directory)
        (chown directory (passwd:uid owner) (passwd:gid owner))
        (chmod directory perms))
      (mkdir-p/perms #$(knot-configuration-run-directory config)
                     (getpwnam "knot") #o755)
      (mkdir-p/perms "/var/lib/knot" (getpwnam "knot") #o755)
      (mkdir-p/perms "/var/lib/knot/keys" (getpwnam "knot") #o755)
      (mkdir-p/perms "/var/lib/knot/keys/keys" (getpwnam "knot") #o755)))

(define (knot-shepherd-service config)
  (let* ((config-file (knot-config-file config))
         (knot (knot-configuration-knot config)))
    (list (shepherd-service
            (documentation "Run the Knot DNS daemon.")
            (provision '(knot dns))
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                       (list (string-append #$knot "/sbin/knotd")
                             "-c" #$config-file)))
            (stop #~(make-kill-destructor))))))

(define knot-service-type
  (service-type (name 'knot)
                (extensions
                  (list (service-extension shepherd-root-service-type
                                           knot-shepherd-service)
                        (service-extension activation-service-type
                                           knot-activation)
                        (service-extension account-service-type
                                           (const %knot-accounts))))))


;;;
;;; Dnsmasq.
;;;

(define-record-type* <dnsmasq-configuration>
  dnsmasq-configuration make-dnsmasq-configuration
  dnsmasq-configuration?
  (package          dnsmasq-configuration-package
                    (default dnsmasq))  ;package
  (no-hosts?        dnsmasq-configuration-no-hosts?
                    (default #f))       ;boolean
  (port             dnsmasq-configuration-port
                    (default 53))       ;integer
  (local-service?   dnsmasq-configuration-local-service?
                    (default #t))       ;boolean
  (listen-addresses dnsmasq-configuration-listen-address
                    (default '()))      ;list of string
  (resolv-file      dnsmasq-configuration-resolv-file
                    (default "/etc/resolv.conf")) ;string
  (no-resolv?       dnsmasq-configuration-no-resolv?
                    (default #f))       ;boolean
  (servers          dnsmasq-configuration-servers
                    (default '()))      ;list of string
  (cache-size       dnsmasq-configuration-cache-size
                    (default 150))      ;integer
  (negative-cache?  dnsmasq-configuration-negative-cache?
                    (default #t)))      ;boolean

(define dnsmasq-shepherd-service
  (match-lambda
    (($ <dnsmasq-configuration> package
                                no-hosts?
                                port local-service? listen-addresses
                                resolv-file no-resolv? servers
                                cache-size negative-cache?)
     (shepherd-service
      (provision '(dnsmasq))
      (requirement '(networking))
      (documentation "Run the dnsmasq DNS server.")
      (start #~(make-forkexec-constructor
                '(#$(file-append package "/sbin/dnsmasq")
                  "--keep-in-foreground"
                  "--pid-file=/run/dnsmasq.pid"
                  #$@(if no-hosts?
                         '("--no-hosts")
                         '())
                  #$(format #f "--port=~a" port)
                  #$@(if local-service?
                         '("--local-service")
                         '())
                  #$@(map (cut format #f "--listen-address=~a" <>)
                          listen-addresses)
                  #$(format #f "--resolv-file=~a" resolv-file)
                  #$@(if no-resolv?
                         '("--no-resolv")
                         '())
                  #$@(map (cut format #f "--server=~a" <>)
                          servers)
                  #$(format #f "--cache-size=~a" cache-size)
                  #$@(if negative-cache?
                         '()
                         '("--no-negcache")))
                #:pid-file "/run/dnsmasq.pid"))
      (stop #~(make-kill-destructor))))))

(define dnsmasq-service-type
  (service-type
   (name 'dnsmasq)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list dnsmasq-shepherd-service))))
   (default-value (dnsmasq-configuration))
   (description "Run the dnsmasq DNS server.")))


;;;
;;; Bind.
;;;

(define-record-type* <bind-options-configuration>
  bind-options-configuration make-bind-options-configuration
  bind-options-configuration?
  (user             bind-options-configuration-user             ; string
                    (default "named"))
  (group            bind-options-configuration-group            ; string
                    (default "named"))
  (run-directory    bind-options-configuration-run-directory    ; string
                    (default "/var/run/named"))
  (pid-file         bind-options-configuration-pid-file         ; string
                    (default "/var/run/named/named.pid"))
  (log-file         bind-options-configuration-log-file         ; string
                    (default "/var/log/named.log"))
  (listen-v4        bind-options-configuration-listen-v4        ; string
                    (default "0.0.0.0"))
  (listen-v6        bind-options-configuration-listen-v6        ; string
                    (default "::"))
  (listen-port      bind-options-configuration-listen-port      ; integer
                    (default 53))
  (allow-recursion? bind-options-configuration-allow-recursion? ; list
                    (default (list "127.0.0.1")))
  (allow-transfer?  bind-options-configuration-allow-transfer?  ; list
                    (default (list "none")))
  (allow-update?    bind-options-configuration-allow-update?    ; list
                    (default (list "none")))
  (version          bind-options-configuration-version          ; string
                    (default "none"))
  (hostname         bind-options-configuration-hostname         ; string
                    (default (gethostname)))
  (server-id        bind-options-configuration-server-id        ; string
                    (default "none")))

(define-record-type* <bind-zone-configuration>
  bind-zone-configuration make-bind-zone-configuration
  bind-zone-configuration?
  (network bind-zone-configuration-network ; string
           (default "localhost"))
  (class   bind-zone-configuration-class   ; string
           (default "IN"))
  (type    bind-zone-configuration-type    ; string
           (default "master"))
  (file    bind-zone-configuration-file    ; <zone-file>
           (default (zone-file (origin "@")
                               (ns "localhost.")
                               (mail "root.localhost.")
                               (entries (list (zone-entry
                                               (name "")
                                               (ttl  "1D")
                                               (type "NS")
                                               (data "localhost."))
                                              (zone-entry
                                               (name "localhost.")
                                               (ttl  "1D")
                                               (data "127.0.0.1"))))))))

(define-record-type* <bind-configuration-file>
  bind-configuration-file make-bind-configuration-file
  bind-configuration-file?
  ;; <bind-options-configuration>
  (config-options bind-configuration-file-config-options
                  (default (bind-options-configuration)))
  ;; list of <bind-zone-configuration>
  (config-zones bind-configuration-file-config-zones
                (default (list (bind-zone-configuration)))))

(define-record-type* <bind-configuration>
  bind-configuration make-bind-configuration
  bind-configuration?
  (config-file bind-configuration-config-file       ; <bind-configuration-file>
               (default (bind-configuration-file)))
  (package     bind-configuration-package           ; <package>
               (default isc-bind)))

(define-gexp-compiler (zone-file-compiler
                       (file <zone-file>) system target)
  (match-record
   file <zone-file>
   (entries origin ns mail serial refresh retry expiry nx)
   (apply text-file* (string-append ns "zone")
          (format #f  "@ IN SOA ~a ~a (~a ~a ~a ~a ~a)\n"
                  ns mail serial refresh retry expiry nx)
          (map (lambda (zone-entry)
                 (match-record
                  zone-entry <zone-entry> (name ttl class type data)
                  (format #f "~a ~a ~a ~a ~a\n" name class type ttl data)))
               entries))))

(define-gexp-compiler (bind-configuration-file-compiler
                       (file <bind-configuration-file>) system target)
  (match-record
   file <bind-configuration-file> (config-options config-zones)
   (define options-config
      (match-record
       config-options <bind-options-configuration>
       (user group run-directory pid-file log-file listen-v4 listen-v6
        listen-port allow-recursion? allow-transfer? allow-update?
        version hostname server-id)
       (letrec ((block (lambda (statements)
                         (format #f "{ ~a ;}" (string-join statements "; ")))))
         (list "options {\n"
               "    directory \"" run-directory "\";\n"
               "    pid-file \"" pid-file "\";\n"
               "    allow-recursion " (block allow-recursion?) ";\n"
               "    allow-transfer " (block allow-transfer?) ";\n"
               "    allow-update " (block allow-update?) ";\n"
               "    version " version ";\n"
               "    hostname \"" hostname "\";\n"
               "    server-id " server-id ";\n"
               "};\n"))))

   (define zones-config
     (map (lambda (config)
            (match-record
             config <bind-zone-configuration> (network class type file)
             (list "zone \"" network "\" " class " {\n"
                   "    type " type ";\n"
                   "    file \"" file "\";\n"
                   "};\n")))
          config-zones))

   (apply text-file* "named.conf"
          (apply string-append options-config)
          (fold append '() zones-config))))

(define (match-bind-options-configuration bind-configuration-file)
  "Return `<bind-options-configuration>' from `<bind-configuration-file>'."
  (match-record
   bind-configuration-file <bind-configuration-file> (config-options)
   config-options))

(define (match-bind-configuration-config-file bind-configuration)
  "Return a `bind-configuration-config-file' from `<bind-configuration>'."
  (match-record
   bind-configuration <bind-configuration> (config-file)
   config-file))

(define (bind-account config)
  "Return a `<user-group>' from `<bind-configuration>'."
  (match-record
   ((compose match-bind-options-configuration
                   match-bind-configuration-config-file)
    config)
   <bind-options-configuration> (user group run-directory)
   (let ((bind-group group))
     (list (user-group
            (name bind-group)
            (system? #t))
           (user-account
            (name user)
            (group bind-group)
            (system? #t)
            (comment "Bind dns server user")
            (home-directory run-directory)
            (shell (file-append shadow "/sbin/nologin")))))))

(define (bind-activation config)
  "Return the activation GEXP for CONFIG."
  (match-record
   ((compose match-bind-options-configuration
             match-bind-configuration-config-file)
    config)
   <bind-options-configuration> (user group run-directory)
   (with-imported-modules '((guix build utils))
     #~(begin
         (mkdir-p #$run-directory)
         (chown #$run-directory
                (passwd:uid (getpw #$user))
                (group:gid (getpw #$group)))))))

(define (bind-shepherd-service config)
  (match-record
   config
   <bind-configuration> (config-file package)
   (match-record
    (match-bind-options-configuration config-file)
    <bind-options-configuration> (user group pid-file)
    (list (shepherd-service
           (documentation "Run the Bind DNS daemon.")
           (provision '(bind dns))
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     (list (string-append #$package "/sbin/named")
                           "-c" #$config-file)
                     #:user #$user
                     #:group #$group
                     #:pid-file #$pid-file))
           (stop #~(make-kill-destructor)))))))

(define bind-service-type
  (service-type (name 'bind)
                (description "Run the Bind DNS server.")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          bind-shepherd-service)
                       (service-extension account-service-type
                                          bind-account)
                       (service-extension activation-service-type
                                          bind-activation)))
                (default-value (bind-configuration))))
