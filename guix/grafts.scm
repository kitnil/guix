;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix grafts)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module ((guix utils) #:select (%current-system))
  #:use-module (guix sets)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (graft?
            graft
            graft-origin
            graft-replacement
            graft-origin-output
            graft-replacement-output

            graft-derivation
            graft-derivation/shallow

            %graft?
            set-grafting
            grafting?))

(define-record-type* <graft> graft make-graft
  graft?
  (origin             graft-origin)               ;derivation | store item
  (origin-output      graft-origin-output         ;string | #f
                      (default "out"))
  (replacement        graft-replacement)          ;derivation | store item
  (replacement-output graft-replacement-output    ;string | #f
                      (default "out")))

(define (write-graft graft port)
  "Write a concise representation of GRAFT to PORT."
  (define (->string thing output)
    (if (derivation? thing)
        (derivation->output-path thing output)
        thing))

  (match graft
    (($ <graft> origin origin-output replacement replacement-output)
     (format port "#<graft ~a ==> ~a ~a>"
             (->string origin origin-output)
             (->string replacement replacement-output)
             (number->string (object-address graft) 16)))))

(set-record-type-printer! <graft> write-graft)

(define (graft-origin-file-name graft)
  "Return the output file name of the origin of GRAFT."
  (match graft
    (($ <graft> (? derivation? origin) output)
     (derivation->output-path origin output))
    (($ <graft> (? string? item))
     item)))

(define* (graft-derivation/shallow store drv grafts
                                   #:key
                                   (name (derivation-name drv))
                                   (outputs (derivation-output-names drv))
                                   (guile (%guile-for-build))
                                   (system (%current-system)))
  "Return a derivation called NAME, which applies GRAFTS to the specified
OUTPUTS of DRV.  This procedure performs \"shallow\" grafting in that GRAFTS
are not recursively applied to dependencies of DRV."
  ;; XXX: Someday rewrite using gexps.
  (define mapping
    ;; List of store item pairs.
    (map (match-lambda
          (($ <graft> source source-output target target-output)
           (cons (if (derivation? source)
                     (derivation->output-path source source-output)
                     source)
                 (if (derivation? target)
                     (derivation->output-path target target-output)
                     target))))
         grafts))

  (define output-pairs
    (map (lambda (output)
           (cons output
                 (derivation-output-path
                  (assoc-ref (derivation-outputs drv) output))))
         outputs))

  (define build
    `(begin
       (use-modules (guix build graft)
                    (guix build utils)
                    (ice-9 match))

       (let* ((old-outputs ',output-pairs)
              (mapping (append ',mapping
                               (map (match-lambda
                                      ((name . file)
                                       (cons (assoc-ref old-outputs name)
                                             file)))
                                    %outputs))))
         (graft old-outputs %outputs mapping))))

  (define add-label
    (cut cons "x" <>))

  (define properties
    `((type . graft)
      (graft (count . ,(length grafts)))))

  (match grafts
    ((($ <graft> sources source-outputs targets target-outputs) ...)
     (let ((sources (zip sources source-outputs))
           (targets (zip targets target-outputs)))
       (build-expression->derivation store name build
                                     #:system system
                                     #:guile-for-build guile
                                     #:modules '((guix build graft)
                                                 (guix build utils)
                                                 (guix build debug-link)
                                                 (guix elf))
                                     #:inputs `(,@(map (lambda (out)
                                                         `("x" ,drv ,out))
                                                       outputs)
                                                ,@(append (map add-label sources)
                                                          (map add-label targets)))
                                     #:outputs outputs

                                     ;; Grafts are computationally cheap so no
                                     ;; need to offload or substitute.
                                     #:local-build? #t
                                     #:substitutable? #f

                                     #:properties properties)))))

(define (non-self-references references drv outputs)
  "Return the list of references of the OUTPUTS of DRV, excluding self
references.  Call REFERENCES to get the list of references."
  (let ((refs (append-map (compose references
                                   (cut derivation->output-path drv <>))
                          outputs))
        (self (match (derivation->output-paths drv)
                (((names . items) ...)
                 items))))
    (remove (cut member <> self) refs)))

(define (references-oracle store input)
  "Return a one-argument procedure that, when passed the output file names of
INPUT, a derivation input, or their dependencies, returns the list of
references of that item.  Use either local info or substitute info; build
INPUT if no information is available."
  (define (references* items)
    ;; Return the references of ITEMS.
    (guard (c ((store-protocol-error? c)
               ;; ITEMS are not in store so build INPUT first.
               (and (build-derivations store (list input))
                    (map (cut references store <>) items))))
      (map (cut references store <>) items)))

  (let loop ((items (derivation-input-output-paths input))
             (result vlist-null))
    (match items
      (()
       (lambda (item)
         (match (vhash-assoc item result)
           ((_ . refs) refs)
           (#f         #f))))
      (_
       (let* ((refs   (references* items))
              (result (fold vhash-cons result items refs)))
         (loop (remove (cut vhash-assoc <> result)
                       (delete-duplicates (concatenate refs) string=?))
               result))))))

(define-syntax-rule (with-cache key exp ...)
  "Cache the value of monadic expression EXP under KEY."
  (mlet %state-monad ((cache (current-state)))
    (match (vhash-assoc key cache)
      ((_ . result)                               ;cache hit
       (return result))
      (#f                                         ;cache miss
       (mlet %state-monad ((result (begin exp ...))
                           (cache  (current-state)))
         (mbegin %state-monad
           (set-current-state (vhash-cons key result cache))
           (return result)))))))

(define (reference-origin drv item)
  "Return the derivation/output pair among the inputs of DRV, recursively,
that produces ITEM.  Return #f if ITEM is not produced by a derivation (i.e.,
it's a content-addressed \"source\"), or if it's not produced by a dependency
of DRV."
  ;; Perform a breadth-first traversal of the dependency graph of DRV in
  ;; search of the derivation that produces ITEM.
  (let loop ((drv (list drv))
             (visited (setq)))
    (match drv
      (()
       #f)
      ((drv . rest)
       (if (set-contains? visited drv)
           (loop rest visited)
           (let ((inputs (derivation-inputs drv)))
             (or (any (lambda (input)
                        (let ((drv (derivation-input-derivation input)))
                          (any (match-lambda
                                 ((output . file)
                                  (and (string=? file item)
                                       (cons drv output))))
                               (derivation->output-paths drv))))
                      inputs)
                 (loop (append rest (map derivation-input-derivation inputs))
                       (set-insert drv visited)))))))))

(define* (cumulative-grafts store drv grafts
                            references
                            #:key
                            (outputs (derivation-output-names drv))
                            (guile (%guile-for-build))
                            (system (%current-system)))
  "Augment GRAFTS with additional grafts resulting from the application of
GRAFTS to the dependencies of DRV; REFERENCES must be a one-argument procedure
that returns the list of references of the store item it is given.  Return the
resulting list of grafts.

This is a monadic procedure in %STATE-MONAD where the state is a vhash mapping
derivations to the corresponding set of grafts."
  (define (graft-origin? drv graft)
    ;; Return true if DRV corresponds to the origin of GRAFT.
    (match graft
      (($ <graft> (? derivation? origin) output)
       (match (assoc-ref (derivation->output-paths drv) output)
         ((? string? result)
          (string=? result
                    (derivation->output-path origin output)))
         (_
          #f)))
      (_
       #f)))

  (define (dependency-grafts item)
    (match (reference-origin drv item)
      ((drv . output)
       ;; If GRAFTS already contains a graft from DRV, do not override it.
       (if (find (cut graft-origin? drv <>) grafts)
           (state-return grafts)
           (cumulative-grafts store drv grafts references
                              #:outputs (list output)
                              #:guile guile
                              #:system system)))
      (#f
       (state-return grafts))))

  (with-cache (cons (derivation-file-name drv) outputs)
    (match (non-self-references references drv outputs)
      (()                                         ;no dependencies
       (return grafts))
      (deps                                       ;one or more dependencies
       (mlet %state-monad ((grafts (mapm %state-monad dependency-grafts deps)))
         (let ((grafts (delete-duplicates (concatenate grafts) equal?)))
           (match (filter (lambda (graft)
                            (member (graft-origin-file-name graft) deps))
                          grafts)
             (()
              (return grafts))
             ((applicable ..1)
              ;; Use APPLICABLE, the subset of GRAFTS that is really
              ;; applicable to DRV, to avoid creating several identical
              ;; grafted variants of DRV.
              (let* ((new    (graft-derivation/shallow store drv applicable
                                                       #:outputs outputs
                                                       #:guile guile
                                                       #:system system))
                     (grafts (append (map (lambda (output)
                                            (graft
                                              (origin drv)
                                              (origin-output output)
                                              (replacement new)
                                              (replacement-output output)))
                                          outputs)
                                     grafts)))
                (return grafts))))))))))

(define* (graft-derivation store drv grafts
                           #:key
                           (guile (%guile-for-build))
                           (outputs (derivation-output-names drv))
                           (system (%current-system)))
  "Apply GRAFTS to the OUTPUTS of DRV and all their dependencies, recursively.
That is, if GRAFTS apply only indirectly to DRV, graft the dependencies of
DRV, and graft DRV itself to refer to those grafted dependencies."

  ;; First, pre-compute the dependency tree of the outputs of DRV.  Do this
  ;; upfront to have as much parallelism as possible when querying substitute
  ;; info or when building DRV.
  (define references
    (references-oracle store (derivation-input drv outputs)))

  (match (run-with-state
             (cumulative-grafts store drv grafts references
                                #:outputs outputs
                                #:guile guile #:system system)
           vlist-null)                            ;the initial cache
    ((first . rest)
     ;; If FIRST is not a graft for DRV, it means that GRAFTS are not
     ;; applicable to DRV and nothing needs to be done.
     (if (equal? drv (graft-origin first))
         (graft-replacement first)
         drv))))


;; The following might feel more at home in (guix packages) but since (guix
;; gexp), which is a lower level, needs them, we put them here.

(define %graft?
  ;; Whether to honor package grafts by default.
  (make-parameter #t))

(define (set-grafting enable?)
  "This monadic procedure enables grafting when ENABLE? is true, and disables
it otherwise.  It returns the previous setting."
  (lambda (store)
    (values (%graft? enable?) store)))

(define (grafting?)
  "Return a Boolean indicating whether grafting is enabled."
  (lambda (store)
    (values (%graft?) store)))

;; Local Variables:
;; eval: (put 'with-cache 'scheme-indent-function 1)
;; End:

;;; grafts.scm ends here
