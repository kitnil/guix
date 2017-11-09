(use-modules (guix store)
             (guix derivations))

(define %foo-zone
  (zone-file (origin "@")
             (ns     "localhost.")
             (mail   "root.localhost.")
             (entries (list (zone-entry
                             (ttl  "1D")
                             (type "NS")
                             (data "localhost."))))))

(define foo-derivation
  (run-with-store (open-connection)
    (gexp->file "foo.zone" %foo-zone)))

(build-derivations (open-connection) (list foo-derivation))

(define %foo-bind-zone-long
  (bind-configuration-file
   (config-zones
    (list (bind-zone-configuration
           (network "localhost")
           (class   "IN")
           (type    "master")
           (file    (zone-file
                     (origin "@")
                     (ns     "localhost.")
                     (mail   "root.localhost.")
                     (entries (list (zone-entry
                                     (ttl  "1D")
                                     (type "NS")
                                     (data "localhost."))
                                    (zone-entry
                                     (name "localhost.")
                                     (ttl  "1D")
                                     (data "127.0.0.1"))
                                    (zone-entry
                                     (name "localhost.")
                                     (ttl  "1D")
                                     (type "AAAA")
                                     (data "::1")))))))
          (bind-zone-configuration
           (network "0.0.127.in-addr.arpa")
           (class   "IN")
           (type    "master")
           (file    (zone-file
                     (origin "@")
                     (ns     "localhost.")
                     (mail   "root.localhost.")
                     (entries (list (zone-entry
                                     (ttl  "1D")
                                     (type "NS")
                                     (data "localhost."))
                                    (zone-entry
                                     (name "1.0.0.127.in-addr.arpa.")
                                     (ttl  "1D")
                                     (type "PTR")
                                     (data "localhost.")))))))))))

(define %foo-bind-zone
  (bind-configuration-file
   (config-zones
    (bind-zone-configuration
     (network "localhost")
     (class   "IN")
     (type    "master")
     (file    (zone-file
               (origin "@")
               (ns     "localhost.")
               (mail   "root.localhost.")
               (entries (list (zone-entry
                               (ttl  "1D")
                               (type "NS")
                               (data "localhost."))))))))))

(define foo-bind-derivation
  (run-with-store (open-connection)
    (gexp->file "foo-bind-zone" %foo-bind-zone)))

(build-derivations (open-connection) (list foo-bind-derivation))

(define example-zone-file
  (zone-file
   (origin "@")
   (ns     "localhost.")
   (mail   "root.localhost.")
   (entries (list (zone-entry
                   (ttl  "1D")
                   (type "NS")
                   (data "localhost."))))))

(build-derivations (open-connection)
                   (list (run-with-store (open-connection) 
                           (gexp->derivation "zooder"
                                             #~#$(bind-configuration-file))))
                   (build-mode check))

(define (wi-build drv) (build-derivations (open-connection) (list drv) (build-mode check)))
(define (wi-store monadic-proc) (run-with-store (open-connection) monadic-proc))

(define (wi-build-check drv) (build-derivations (open-connection) (list drv)) (build-mode check))
(wi-build (wi-store (text-file* "zoo" "foo-zone: " %foo-bind-zone-long)))



;;;

(define (foo x)
  (format #f " ; ~a\n" (symbol->string x)))

(map foo '(serial refresh retry expiry nx))

(letrec-syntax
    ((fields (syntax-rules ()
               ((_ string rest ...)
                (append `("~a ; " ,@string "\n")
                        (fields rest ...)))
               ((_)
                '()))))
  (fields (map symbol->string '(serial refresh retry expiry nx))))

  ;; (format #f (string-join `("@ IN SOA ~a ~a ("
  ;;                          ,@(fields "SERIAL"
  ;;                                    "REFRESH"
  ;;                                    "RETRY"
  ;;                                    "EXPIRE"
  ;;                                    "MINIMUM"))
  ;;                        "\n")
  ;;         10 2000 300000 4000000 50000 600000 70)

(map (lambda (zone-entry)
       (match-record
        zone-entry <zone-entry> (name ttl class type data)
        (format #f "~a ~a ~a ~a ~a" name class type ttl data)))
     (list (zone-entry (name "") (ttl  "1D") (type "NS") (data "localhost."))
           (zone-entry (name "localhost.") (ttl  "1D") (data "127.0.0.1"))))

;; TODO:
;; (bind-zone-configuration
;;  (network "0.0.127.in-addr.arpa")
;;  (class   "IN")
;;  (type    "master")
;;  (file    (zone-file
;;            (ns     "localhost.")
;;            (mail   "root.localhost.")
;;            (entries (list (zone-entry
;;                            (ttl  "1D")
;;                            (type "NS")
;;                            (data "localhost."))
;;                           (zone-entry
;;                            (name "1.0.0.127.in-addr.arpa.")
;;                            (ttl  "1D")
;;                            (type "PTR")
;;                            (data "localhost.")))))))

((lambda (statements)
   ;; (string-append "{ " (string-join statements ";\n") "; }")
   (list "{" (string-join statements "; ") ";}")
   )
 (list "127.0.0.1" "::1"))
