;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu services version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (git-daemon-service
            git-daemon-service-type
            git-daemon-configuration
            git-daemon-configuration?

            <cgit-repo>
            cgit-repo
            cgit-repo?
            cgit-repo-branch-sort
            cgit-repo-description
            cgit-repo-email-filter
            cgit-repo-enable-commit-graph
            cgit-repo-enable-html-serving
            cgit-repo-enable-log-filecount
            cgit-repo-enable-log-linecount
            cgit-repo-enable-remote-branches
            cgit-repo-enable-subject-links
            cgit-repo-file-name
            cgit-repo-hide
            cgit-repo-homepage
            cgit-repo-ignore
            cgit-repo-logo
            cgit-repo-logo-link
            cgit-repo-max-stats
            cgit-repo-module-link
            cgit-repo-module-link-path
            cgit-repo-name
            cgit-repo-owner
            cgit-repo-owner-filter
            cgit-repo-path
            cgit-repo-path
            cgit-repo-readme
            cgit-repo-section
            cgit-repo-section-from-path
            cgit-repo-section-sort
            cgit-repo-side-by-side-diffs
            cgit-repo-snapshots
            cgit-repo-source-filter
            cgit-repo-url

            <cgit-conf>
            cgit-conf
            cgit-conf?
            cgit-conf-about-filter
            cgit-conf-agefile
            cgit-conf-auth-filter
            cgit-conf-branch-sort
            cgit-conf-cache-root
            cgit-conf-cache-static-ttl
            cgit-conf-cache-dynamic-ttl
            cgit-conf-cache-repo-ttl
            cgit-conf-cache-root-ttl
            cgit-conf-cache-scanrc-ttl
            cgit-conf-cache-about-ttl
            cgit-conf-cache-snapshot-ttl
            cgit-conf-cache-size
            cgit-conf-case-sensitive-sort
            cgit-conf-clone-prefix
            cgit-conf-clone-url
            cgit-conf-commit-filter
            cgit-conf-commit-sort
            cgit-conf-css
            cgit-conf-email-filter
            cgit-conf-embedded
            cgit-conf-enable-commit-graph
            cgit-conf-enable-filter-overrides
            cgit-conf-enable-follow-links
            cgit-conf-enable-git-config
            cgit-conf-enable-html-serving
            cgit-conf-enable-http-clone
            cgit-conf-enable-index-links
            cgit-conf-enable-index-owner
            cgit-conf-enable-log-filecount
            cgit-conf-enable-remote-branches
            cgit-conf-enable-subject-links
            cgit-conf-enable-tree-linenumbers
            cgit-conf-extra-options
            cgit-conf-favicon
            cgit-conf-footer
            cgit-conf-header
            cgit-conf-head-include
            cgit-conf-include
            cgit-conf-index-header
            cgit-conf-index-info
            cgit-conf-local-time
            cgit-conf-logo
            cgit-conf-logo-link
            cgit-conf-max-atom-items
            cgit-conf-max-blob-size
            cgit-conf-max-commit-count
            cgit-conf-max-message-length
            cgit-conf-max-repo-count
            cgit-conf-max-repodesc-length
            cgit-conf-max-stats
            cgit-conf-mimetype-file
            cgit-conf-module-link
            cgit-conf-nocache
            cgit-conf-noplainemail
            cgit-conf-noheader
            cgit-conf-owner-filter
            cgit-conf-readme
            cgit-conf-repository-directory
            cgit-conf-robots
            cgit-conf-root-desc
            cgit-conf-root-readme
            cgit-conf-root-title
            cgit-conf-snapshots
            cgit-conf-source-filter
            cgit-conf-summary-branches
            cgit-conf-virtual-root
            cgit-conf-mimetypes
            cgit-conf-project-list
            cgit-conf-remove-suffix
            cgit-conf-renamelimit
            cgit-conf-repositories
            cgit-conf-repository-sort
            cgit-conf-root-title
            cgit-conf-scan-hidden-path
            cgit-conf-strict-export
            cgit-conf-summary-log
            cgit-conf-summary-tags

            <cgit-configuration>
            cgit-configuration
            cgit-configuration?
            cgit-configuration-config-file
            cgit-configuration-package

            %cgit-configuration-nginx
            cgit-configuration-nginx-config

            cgit-service-type

            git-http-configuration
            git-http-configuration?
            git-http-nginx-location-configuration))

;;; Commentary:
;;;
;;; Version Control related services.
;;;
;;; Code:


;;;
;;; Git daemon.
;;;

(define-record-type* <git-daemon-configuration>
  git-daemon-configuration
  make-git-daemon-configuration
  git-daemon-configuration?
  (package          git-daemon-configuration-package        ;package
                    (default git))
  (export-all?      git-daemon-configuration-export-all     ;boolean
                    (default #f))
  (base-path        git-daemon-configuration-base-path      ;string | #f
                    (default "/srv/git"))
  (user-path        git-daemon-configuration-user-path      ;string | #f
                    (default #f))
  (listen           git-daemon-configuration-listen         ;list of string
                    (default '()))
  (port             git-daemon-configuration-port           ;number | #f
                    (default #f))
  (whitelist        git-daemon-configuration-whitelist      ;list of string
                    (default '()))
  (extra-options    git-daemon-configuration-extra-options  ;list of string
                    (default '())))

(define git-daemon-shepherd-service
  (match-lambda
    (($ <git-daemon-configuration>
        package export-all? base-path user-path
        listen port whitelist extra-options)
     (let* ((git     (file-append package "/bin/git"))
            (command `(,git
                       "daemon" "--syslog" "--reuseaddr"
                       ,@(if export-all?
                             '("--export-all")
                             '())
                       ,@(if base-path
                             `(,(string-append "--base-path=" base-path))
                             '())
                       ,@(if user-path
                             `(,(string-append "--user-path=" user-path))
                             '())
                       ,@(map (cut string-append "--listen=" <>) listen)
                       ,@(if port
                             `(,(string-append
                                 "--port=" (number->string port)))
                             '())
                       ,@extra-options
                       ,@whitelist)))
       (list (shepherd-service
              (documentation "Run the git-daemon.")
              (requirement '(networking))
              (provision '(git-daemon))
              (start #~(make-forkexec-constructor '#$command
                                                  #:user "git-daemon"
                                                  #:group "git-daemon"))
              (stop #~(make-kill-destructor))))))))

(define %git-daemon-accounts
  ;; User account and group for git-daemon.
  (list (user-group
         (name "git-daemon")
         (system? #t))
        (user-account
         (name "git-daemon")
         (system? #t)
         (group "git-daemon")
         (comment "Git daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (git-daemon-activation config)
  "Return the activation gexp for git-daemon using CONFIG."
  (let ((base-path (git-daemon-configuration-base-path config)))
    #~(begin
        (use-modules (guix build utils))
        ;; Create the 'base-path' directory when it's not '#f'.
        (and=> #$base-path mkdir-p))))

(define git-daemon-service-type
  (service-type
   (name 'git-daemon)
   (extensions
    (list (service-extension shepherd-root-service-type
                             git-daemon-shepherd-service)
          (service-extension account-service-type
                             (const %git-daemon-accounts))
          (service-extension activation-service-type
                             git-daemon-activation)))
   (description
    "Expose Git respositories over the insecure @code{git://} TCP-based
protocol.")
   (default-value (git-daemon-configuration))))

(define* (git-daemon-service #:key (config (git-daemon-configuration)))
  "Return a service that runs @command{git daemon}, a simple TCP server to
expose repositories over the Git protocol for annoymous access.

The optional @var{config} argument should be a
@code{<git-daemon-configuration>} object, by default it allows read-only
access to exported repositories under @file{/srv/git}."
  (service git-daemon-service-type config))


;;;
;;; Cgit
;;;

(define-record-type* <cgit-repo> cgit-repo make-cgit-repo cgit-repo?
  (about-filter           cgit-repo-about-filter            ; string
                          (default '()))
  (branch-sort            cgit-repo-branch-sort             ; string
                          (default '()))
  (commit-filter          cgit-repo-commit-filter           ; string
                          (default '()))
  (commit-sort            cgit-repo-commit-sort             ; string
                          (default '()))
  (description            cgit-repo-description             ; string
                          (default '()))
  (email-filter           cgit-repo-email-filter
                          (default '()))
  (enable-commit-graph?   cgit-repo-enable-commit-graph     ; bool
                          (default #f))
  (enable-html-serving?   cgit-repo-enable-html-serving     ; bool
                          (default #f))
  (enable-log-filecount?  cgit-repo-enable-log-filecount    ; bool
                          (default #f))
  (enable-log-linecount?  cgit-repo-enable-log-linecount    ; bool
                          (default #f))
  (enable-remote-branches? cgit-repo-enable-remote-branches ; bool
                          (default #f))
  (enable-subject-links?  cgit-repo-enable-subject-links    ; bool
                          (default '()))
  (file-name              cgit-repo-file-name
                          (default '()))
  (hide                   cgit-repo-hide
                          (default #f))
  (homepage               cgit-repo-homepage
                          (default '()))
  (ignore                 cgit-repo-ignore
                          (default #f))
  (logo                   cgit-repo-logo
                          (default '()))
  (logo-link              cgit-repo-logo-link
                          (default '()))
  (max-stats              cgit-repo-max-stats               ; string
                          (default "month"))
  (module-link            cgit-repo-module-link
                          (default '()))
  (module-link-path       cgit-repo-module-link-path
                          (default '()))
  (name                   cgit-repo-name
                          (default '()))
  (owner                  cgit-repo-owner                   ; string
                          (default '()))
  (owner-filter           cgit-repo-owner-filter
                          (default '()))
  (path                   cgit-repo-path
                          (default '()))
  (readme                 cgit-repo-readme                  ; string
                          (default '()))
  (section                cgit-repo-section                 ; string
                          (default '()))
  (section-from-path      cgit-repo-section-from-path       ; bool
                          (default #f))
  (section-sort           cgit-repo-section-sort            ; bool
                          (default #t))
  (side-by-side-diffs?    cgit-repo-side-by-side-diffs      ; bool
                          (default #f))
  (snapshots?             cgit-repo-snapshots               ; bool
                          (default #f))
  (source-filter          cgit-repo-source-filter
                          (default '()))
  (url                    cgit-repo-url
                          (default '())))

(define-record-type* <cgit-conf> cgit-conf make-cgit-conf cgit-conf?
  ;; list of <cgit-configuration-file-repo>
  (about-filter             cgit-conf-about-filter                ; string
                            (default '()))
  (agefile                  cgit-conf-agefile                     ; string
                            (default '()))
  (auth-filter              cgit-conf-auth-filter                 ; string
                            (default '()))
  (branch-sort              cgit-conf-branch-sort                 ; string
                            (default "name"))
  (cache-root               cgit-conf-cache-root                  ; string
                            (default "/var/cache/cgit"))
  (cache-static-ttl         cgit-conf-cache-static-ttl            ; integer
                            (default -1))
  (cache-dynamic-ttl        cgit-conf-cache-dynamic-ttl           ; integer
                            (default 5))
  (cache-repo-ttl           cgit-conf-cache-repo-ttl              ; integer
                            (default 5))
  (cache-root-ttl           cgit-conf-cache-root-ttl              ; integer
                            (default 5))
  (cache-scanrc-ttl         cgit-conf-cache-scanrc-ttl            ; integer
                            (default 15))
  (cache-about-ttl          cgit-conf-cache-about-ttl             ; integer
                            (default 15))
  (cache-snapshot-ttl       cgit-conf-cache-snapshot-ttl          ; integer
                            (default 5))
  (cache-size               cgit-conf-cache-size                  ; integer
                            (default 0))
  (case-sensitive-sort?     cgit-conf-case-sensitive-sort         ; bool
                            (default #t))
  (clone-prefix             cgit-conf-clone-prefix                ; list strings
                            (default '()))
  (clone-url                cgit-conf-clone-url                   ; list strings
                            (default '()))
  (commit-filter            cgit-conf-commit-filter               ; string
                            (default '()))
  (commit-sort              cgit-conf-commit-sort                 ; string
                            (default "git log"))
  (css                      cgit-conf-css                         ; string
                            (default "/share/cgit/cgit.css"))
  (email-filter             cgit-conf-email-filter
                            (default '()))
  (embedded                 cgit-conf-embedded                    ; bool
                            (default '()))
  (enable-commit-graph?     cgit-conf-enable-commit-graph         ; bool
                            (default #f))
  (enable-filter-overrides? cgit-conf-enable-filter-overrides     ; bool
                            (default #f))
  (enable-follow-links?     cgit-conf-enable-follow-links         ; bool
                            (default #f))
  (enable-git-config?       cgit-conf-enable-git-config           ; bool
                            (default #f))
  (enable-html-serving?     cgit-conf-enable-html-serving         ; bool
                            (default #f))
  (enable-http-clone?       cgit-conf-enable-http-clone           ; bool
                            (default #t))
  (enable-index-links?      cgit-conf-enable-index-links          ; bool
                            (default #f))
  (enable-index-owner?      cgit-conf-enable-index-owner          ; bool
                            (default #t))
  (enable-log-filecount?    cgit-conf-enable-log-filecount        ; bool
                            (default #f))
  (enable-log-linecount?    cgit-conf-enable-log-linecount        ; bool
                            (default #f))
  (enable-remote-branches?  cgit-conf-enable-remote-branches      ; bool
                            (default #f))
  (enable-subject-links?    cgit-conf-enable-enable-subject-links ; bool
                            (default #f))
  (enable-tree-linenumbers? cgit-conf-enable-tree-linenumbers     ; bool
                            (default #t))
  (extra-options            cgit-conf-extra-options               ; list strings
                            (default '()))
  (favicon                  cgit-conf-favicon                     ; string
                            (default "/favicon.ico"))
  (footer                   cgit-conf-footer                      ; string
                            (default '()))
  (head-include             cgit-conf-head-include                ; string
                            (default '()))
  (header                   cgit-conf-header                      ; string
                            (default '()))
  (include                  cgit-conf-include                     ; string
                            (default '()))
  (index-header             cgit-conf-index-header                ; string
                            (default '()))
  (index-info               cgit-conf-index-info                  ; string
                            (default '()))
  (local-time               cgit-conf-local-time                  ; bool
                            (default #f))
  (logo                     cgit-conf-logo                        ; string
                            (default "/share/cgit/cgit.png"))
  (logo-link                cgit-conf-logo-link                   ; string
                            (default '()))
  (max-atom-items           cgit-conf-max-atom-items              ; integer
                            (default 10))
  (max-blob-size            cgit-conf-max-blob-size               ; integer
                            (default 0))
  (max-commit-count         cgit-conf-max-commit-count            ; integer
                            (default 50))
  (max-message-length       cgit-conf-max-message-length          ; integer
                            (default 80))
  (max-repo-count           cgit-conf-max-repo-count              ; integer
                            (default 50))
  (max-repodesc-length      cgit-conf-max-repodesc-length         ; integer
                            (default 80))
  (max-stats                cgit-conf-max-stats                   ; string
                            (default "quarter"))
  (mimetype-file            cgit-conf-mimetype-file               ; string
                            (default '()))
  (mimetypes                cgit-conf-mimetypes                   ; alist
                            (default `((mimetype-gif  "image/gif")
                                       (mimetype-html "text/html")
                                       (mimetype-jpg  "image/jpeg")
                                       (mimetype-jpeg "image/jpeg")
                                       (mimetype-pdf  "application/pdf")
                                       (mimetype-png  "image/png")
                                       (mimetype-svg  "svg+xml"))))
  (module-link              cgit-conf-module-link                 ; string
                            (default '()))
  (nocache                  cgit-conf-nocache                     ; bool
                            (default #f))
  (noplainemail?            cgit-conf-noplainemail                ; bool
                            (default #f))
  (noheader?                cgit-conf-noheader                    ; bool
                            (default #f))
  (owner-filter             cgit-conf-owner-filter                ; string
                            (default '()))
  (project-list             cgit-conf-project-list                ; list strings
                            (default '()))
  (readme                   cgit-conf-readme                      ; list strings
                            (default (list "README.md"    "readme.md"
                                           "README.mkd"   "readme.mkd"
                                           "README.rst"   "readme.rst"
                                           "README.html"  "readme.html"
                                           "README.htm"   "readme.htm"
                                           "README.txt"   "readme.txt"
                                           "README"       "readme"
                                           "INSTALL.md"   "install.md"
                                           "INSTALL.mkd"  "install.mkd"
                                           "INSTALL.rst"  "install.rst"
                                           "INSTALL.html" "install.html"
                                           "INSTALL.htm"  "install.htm"
                                           "INSTALL.txt"  "install.txt"
                                           "INSTALL"      "install")))
  (remove-suffix?           cgit-conf-remove-suffix               ; bool
                            (default #f))
  (renamelimit              cgit-conf-renamelimit                 ; integer
                            (default -1))

  (repositories             cgit-conf-repositories                ; <cgit-repo>
                            (default '()))
  (repository-directory     cgit-conf-repository-directory        ; string
                            (default "/srv/git"))
  (repository-sort          cgit-conf-repository-sort             ; string
                            (default "name"))
  (robots                   cgit-conf-robots                      ; list of
                            (default '("noindex" "nofollow")))
  (root-desc                cgit-conf-root-desc                   ; string
                            (default "A webinterface for the Git DSCM"))
  (root-readme              cgit-conf-root-readme                 ; file
                            (default '()))
  (root-title               cgit-conf-root-title                  ; string
                            (default '()))
  (scan-hidden-path         cgit-conf-scan-hidden-path            ; bool
                            (default 0))
  (snapshots                cgit-conf-snapshots                   ; list strings
                            (default '()))
  (source-filter            cgit-conf-source-filter               ; string
                            (default '()))
  (strict-export            cgit-conf-strict-export               ; string
                            (default '()))
  (summary-branches         cgit-conf-summary-branches            ; integer
                            (default 10))
  (summary-log              cgit-conf-summary-log                 ; integer
                            (default 10))
  (summary-tags             cgit-conf-summary-tags                ; integer
                            (default 10))
  (virtual-root             cgit-conf-virtual-root                ; string
                            (default "/")))

(define (format-cgit-config-entry entry)
  "Return a list representing Cgit configuration line."
  (let ((key   (car  entry))
        (value (cadr entry)))
    (cond ((string?  value) (list key "=" value "\n"))
          ((boolean? value) (list key "=" (if value "1" "0") "\n"))
          ((list?    value) (fold (lambda (x xs) (cons* x xs)) '() value))
          ((number?  value) (list key "=" (number->string value) "\n"))
          ((or (nil? key) (nil? value)) '()))))

(define (format-cgit-config-line alist)
  "Return a list representing Cgit configuration lines."
  (fold (lambda (x xs) (append (format-cgit-config-entry x) xs)) '() alist))

(define-gexp-compiler (cgit-repo-compiler (file <cgit-repo>) system target)
  (match-record
   file <cgit-repo>
   (about-filter commit-filter commit-sort description email-filter
enable-commit-graph? enable-html-serving? enable-log-filecount?
enable-log-linecount? enable-remote-branches? enable-subject-links? file-name
hide homepage ignore logo logo-link max-stats module-link module-link-path
name owner owner-filter path readme section section-from-path section-sort
side-by-side-diffs? snapshots? source-filter url)
   (apply text-file* file-name
          "# Generated by cgit-service.\n\n"
          (format-cgit-config-line
           `(("about-filter"           ,about-filter)
             ("commit-filter"          ,commit-filter)
             ("commit-sort"            ,commit-sort)
             ("description"            ,description)
             ("email-filter"           ,email-filter)
             ("enable-commit-graph"    ,enable-commit-graph?)
             ("enable-html-serving"    ,enable-html-serving?)
             ("enable-log-filecount"   ,enable-log-filecount?)
             ("enable-log-linecount"   ,enable-log-linecount?)
             ("enable-remote-branches" ,enable-remote-branches?)
             ("enable-subject-links"   ,enable-subject-links?)
             ("file-name"              ,file-name)
             ("hide"                   ,hide)
             ("homepage"               ,homepage)
             ("ignore"                 ,ignore)
             ("logo"                   ,logo)
             ("logo-link"              ,logo-link)
             ("max-stats"              ,max-stats)
             ("module-link"            ,module-link)
             ("module-link-path"       ,module-link-path)
             ("name"                   ,name)
             ("owner"                  ,owner)
             ("owner-filter"           ,owner-filter)
             ("path"                   ,path)
             ("readme"                 ,readme)
             ("section"                ,section)
             ("section-from-path"      ,section-from-path)
             ("section-sort"           ,section-sort)
             ("side-by-side-diffs"     ,side-by-side-diffs?)
             ("snapshots"              ,snapshots?)
             ("source-filter"          ,source-filter)
             ("url"                    ,url))))))

(define-gexp-compiler (cgit-conf-compiler (file <cgit-conf>) system target)
  (match-record
   file <cgit-conf>
   (about-filter agefile auth-filter branch-sort cache-root cache-static-ttl
cache-dynamic-ttl cache-repo-ttl cache-root-ttl cache-scanrc-ttl
cache-about-ttl cache-snapshot-ttl cache-size case-sensitive-sort?
clone-prefix clone-url commit-filter commit-sort css email-filter embedded
enable-commit-graph? enable-filter-overrides? enable-follow-links?
enable-git-config? enable-html-serving? enable-http-clone?
enable-index-links? enable-index-owner? enable-log-filecount?
enable-log-linecount? enable-remote-branches? enable-subject-links?
enable-tree-linenumbers? extra-options favicon footer head-include header
include index-header index-info local-time logo logo-link max-atom-items
max-blob-size max-commit-count max-message-length max-repo-count
max-repodesc-length max-stats mimetype-file mimetypes module-link nocache
noplainemail? noheader? owner-filter project-list readme remove-suffix?
renamelimit repositories repository-directory repository-sort robots root-desc
root-readme root-title scan-hidden-path snapshots source-filter strict-export
summary-branches summary-log summary-tags virtual-root)

   (define (format-cgit-config-line-extended alist)
     (define (format-key-value-extended entry)

       (define (uglify-field-name field-name)
         (let ((str (symbol->string field-name)))
           (string-join (string-split (if (string-suffix? "?" str)
                                          (substring str 0
                                                     (1- (string-length str)))
                                          str)
                                      #\-)
                        "-")))

       (let ((key   (car  entry))
             (value (cadr entry)))

         (cond ((string=? key "readme")
                (fold (lambda (x xs) (cons* "readme" "=:" x "\n" xs))
                      '() value))

               ((string=? key "robots")
                (list "robots=" (string-join value ", ") "\n"))

               ((string=? key "snapshots")
                (list "snapshots=" (string-join value) "\n"))

               ((string=? key "repositories")
                (fold (lambda (x xs) (cons* "include=" x "\n" xs)) '() value))

               ((string=? key "extra-options")
                (map (lambda (option) (string-append option "\n")) value))

               ((string=? key "mimetypes")
                (fold (lambda (x xs) (cons* (uglify-field-name (car x))
                                       "=" (cadr x) "\n" xs))
                      '() value))

               ((string=? key "repository-directory")
                (list "scan-path" "=" value "\n"))

               (else (format-cgit-config-entry entry)))))

     (fold (lambda (x xs) (append (format-key-value-extended x) xs)) '() alist))

   (apply text-file* "cgitrc"
          "# Generated by cgit-service.\n\n"
          (format-cgit-config-line-extended
           `(("about-filter"            ,about-filter)
             ("agefile"                 ,agefile)
             ("auth-filter"             ,auth-filter)
             ("branch-sort"             ,branch-sort)
             ("cache-root"              ,cache-root)
             ("cache-static-ttl"        ,cache-static-ttl)
             ("cache-dynamic-ttl"       ,cache-dynamic-ttl)
             ("cache-repo-ttl"          ,cache-repo-ttl)
             ("cache-root-ttl"          ,cache-root-ttl)
             ("cache-scanrc-ttl"        ,cache-scanrc-ttl)
             ("cache-about-ttl"         ,cache-about-ttl)
             ("cache-snapshot-ttl"      ,cache-snapshot-ttl)
             ("cache-size"              ,cache-size)
             ("case-sensitive-sort"     ,case-sensitive-sort?)
             ("clone-prefix"            ,clone-prefix)
             ("clone-url"               ,clone-url)
             ("commit-filter"           ,commit-filter)
             ("commit-sort"             ,commit-sort)
             ("css"                     ,css)
             ("email-filter"            ,email-filter)
             ("embedded"                ,embedded)
             ("enable-commit-graph"     ,enable-commit-graph?)
             ("enable-filter-overrides" ,enable-filter-overrides?)
             ("enable-follow-links"     ,enable-follow-links?)
             ("enable-git-config"       ,enable-git-config?)
             ("enable-html-serving"     ,enable-html-serving?)
             ("enable-http-clone"       ,enable-http-clone?)
             ("enable-index-links"      ,enable-index-links?)
             ("enable-index-owner"      ,enable-index-owner?)
             ("enable-log-filecount"    ,enable-log-filecount?)
             ("enable-log-linecount"    ,enable-log-linecount?)
             ("enable-remote-branches"  ,enable-remote-branches?)
             ("enable-subject-links"    ,enable-subject-links?)
             ("enable-tree-linenumbers" ,enable-tree-linenumbers?)
             ("extra-options"           ,extra-options)
             ("favicon"                 ,favicon)
             ("footer"                  ,footer)
             ("head-include"            ,head-include)
             ("header"                  ,header)
             ("include"                 ,include)
             ("index-header"            ,index-header)
             ("index-info"              ,index-info)
             ("local-time"              ,local-time)
             ("logo"                    ,logo)
             ("logo-link"               ,logo-link)
             ("max-atom-items"          ,max-atom-items)
             ("max-blob-size"           ,max-blob-size)
             ("max-commit-count"        ,max-commit-count)
             ("max-message-length"      ,max-message-length)
             ("max-repo-count"          ,max-repo-count)
             ("max-repodesc-length"     ,max-repodesc-length)
             ("max-stats"               ,max-stats)
             ("mimetype-file"           ,mimetype-file)
             ("mimetypes"               ,mimetypes)
             ("module-link"             ,module-link)
             ("nocache"                 ,nocache)
             ("noplainemail"            ,noplainemail?)
             ("noheader"                ,noheader?)
             ("owner-filter"            ,owner-filter)
             ("project-list"            ,project-list)
             ("readme"                  ,readme)
             ("remove-suffix"           ,remove-suffix?)
             ("renamelimit"             ,renamelimit)
             ("repositories"            ,repositories)
             ("repository-directory"    ,repository-directory)
             ("repository-sort"         ,repository-sort)
             ("robots"                  ,robots)
             ("root-desc"               ,root-desc)
             ("root-readme"             ,root-readme)
             ("root-title"              ,root-title)
             ("scan-hidden-path"        ,scan-hidden-path)
             ("snapshots"               ,snapshots)
             ("source-filter"           ,source-filter)
             ("strict-export"           ,strict-export)
             ("summary-branches"        ,summary-branches)
             ("summary-log"             ,summary-log)
             ("summary-tags"            ,summary-tags)
             ("virtual-root"            ,virtual-root))))))

(define %cgit-configuration-nginx
  (list
   (nginx-server-configuration
    (root cgit)
    (locations
     (list
      (nginx-location-configuration
       (uri "@cgit")
       (body '("fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
               "fastcgi_param PATH_INFO $uri;"
               "fastcgi_param QUERY_STRING $args;"
               "fastcgi_param HTTP_HOST $server_name;"
               "fastcgi_pass 127.0.0.1:9000;")))))
    (try-files (list "$uri" "@cgit"))
    (https-port #f)
    (ssl-certificate #f)
    (ssl-certificate-key #f))))

(define-record-type* <cgit-configuration>
  cgit-configuration make-cgit-configuration
  cgit-configuration?
  (config-file cgit-configuration-config-file        ; <cgit-conf>
               (default (cgit-conf)))
  (package     cgit-configuration-package            ; <package>
               (default cgit))
  ;; <nginx-server-configuration>
  (nginx       cgit-configuration-nginx
               (default %cgit-configuration-nginx)))

(define (cgit-activation config)
  (match-record
   config <cgit-configuration> (config-file)
   (match-record
    config-file <cgit-conf> (cache-root)
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$cache-root)
        ;; Cgit compiled with default configuration path.
        (copy-file #$config-file "/etc/cgitrc")))))

(define (cgit-configuration-nginx-config config)
  (cgit-configuration-nginx config))

(define cgit-service-type
  (service-type
   (name 'cgit)
   (extensions
    (list (service-extension activation-service-type
                             cgit-activation)
          (service-extension nginx-service-type
                             cgit-configuration-nginx-config)))
   (default-value (cgit-configuration))
   (description
    "Run the Cgit web interface, which allows users to browse Git
repositories.")))


;;;
;;; HTTP access.  Add the result of calling
;;; git-http-nginx-location-configuration to an nginx-server-configuration's
;;; "locations" field.
;;;

(define-record-type* <git-http-configuration>
  git-http-configuration
  make-git-http-configuration
  git-http-configuration?
  (package          git-http-configuration-package        ;package
                    (default git))
  (git-root         git-http-configuration-git-root       ;string
                    (default "/srv/git"))
  (export-all?      git-http-configuration-export-all?    ;boolean
                    (default #f))
  (uri-path         git-http-configuration-uri-path       ;string
                    (default "/git/"))
  (fcgiwrap-socket  git-http-configuration-fcgiwrap-socket ;string
                    (default "127.0.0.1:9000")))

(define* (git-http-nginx-location-configuration #:optional
                                                (config
                                                 (git-http-configuration)))
  (match config
    (($ <git-http-configuration> package git-root export-all?
                                 uri-path fcgiwrap-socket)
     (nginx-location-configuration
      (uri (string-append "~ /" (string-trim-both uri-path #\/) "(/.*)"))
      (body
       (list
        (list "fastcgi_pass " fcgiwrap-socket ";")
        (list "fastcgi_param SCRIPT_FILENAME "
              package "/libexec/git-core/git-http-backend"
              ";")
        "fastcgi_param QUERY_STRING $query_string;"
        "fastcgi_param REQUEST_METHOD $request_method;"
        "fastcgi_param CONTENT_TYPE $content_type;"
        "fastcgi_param CONTENT_LENGTH $content_length;"
        (if export-all?
            "fastcgi_param GIT_HTTP_EXPORT_ALL \"\";"
            "")
        (list "fastcgi_param GIT_PROJECT_ROOT " git-root ";")
        "fastcgi_param PATH_INFO $1;"))))))
