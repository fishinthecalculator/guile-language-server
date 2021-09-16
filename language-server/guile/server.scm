;;;; Copyright (C) 2018 Jonas Herzig <me@johni0702.de>
;;;; Copyright (C) 2021 Giacomo Leidi <goodoldpaul@autistici.org>
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;;

(define-module (language-server guile server)
  #:use-module (language-server guile xref)
  #:use-module (language-server protocol)
  #:use-module (language-server scm-utils)
  #:use-module (language-server guile compile)
  #:use-module (language-server guile extended-scheme)
  #:use-module (language-server guile formatter)
  #:use-module (json-rpc)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (main))

(define-immutable-record-type <state>
  (make-state root-uri documents shutdown?)
  state?
  (root-uri state-root-uri set-state-root-uri)
  (documents state-documents set-state-documents)
  (shutdown? state-shutdown? set-state-shutdown?))

(define (update-document out state uri text)
  (define old-documents (state-documents state))
  (define old-document (or (vhash-ref old-documents uri)
                           (make-empty-document uri)))
  (define new-document (set-document-text old-document text))
  (define new-documents (compile-document old-documents new-document))
  (vhash-for-each
   (lambda (uri document)
     ;; FIXME: only send for changed documents
     (display "Sending diagnostics for ")
     (display (document-uri document)) (newline)
     (display (document-diagnostics document)) (newline)
     (send-diagnostics out uri (document-diagnostics document)))
   new-documents)
  (set-state-documents state new-documents))

(define (handle-initialize in out id params)
  (define root-uri (hash-ref params "rootUri")) ;; FIXME: handle rootPath
  ;; TODO handle client capabilities
  (define server-capabilities
    '((textDocumentSync . ((openClose . #t)
                           (change . 1)))))
  ;; FIXME: always assumes uris of file:// kind
  (add-to-load-path (string-drop root-uri 7))
  (send-result out id `((capabilities . ,server-capabilities)))
  (make-state root-uri vlist-null #f))

(define (handle-initialized out)
  (define guile-document-selector `(((language . "guile"))))
  (send-register-capability
   out
   `((id . "1")
     (method . "textDocument/didChange")
     (registerOptions . ((textDocumentSelector . ,guile-document-selector)
                         (syncKind . 1))))))

(define (wait-for-init in out)
  (match (read-message in)
    (($ <request> id "initialize" params)
     (handle-initialize in out id params))
    (($ <request> _ "exit" _) #f)
    (($ <request> #nil _ _)
     (wait-for-init in out))
    (($ <request> id _ _)
     (send-error out id server-not-initialized "Expected 'initialize' message.")
     (wait-for-init in out))))

(define (main-loop in out state)
  ;; TODO handle batches
  (define result
    (match (read-message in)
      (($ <response> id result error)
       ;; TODO handle responses
       state)
      (($ <request> _ "initialized" _)
       (handle-initialized out)
       state)
      (($ <request> _ "exit" _) #f)
      (($ <request> id "shutdown" _)
       (send-result out id #nil)
       (set-state-shutdown? state #t))
      (($ <request> #f "textDocument/didOpen" params)
       (define text-document (scm->text-document (hash-ref params "textDocument")))
       (define uri (text-document-uri text-document))
       (define text (text-document-text text-document))
       (display "textDocument/didOpen: ") (display uri) (newline)
       (update-document out state uri text))
      (($ <request> #f "textDocument/didChange" params)
       (define versioned-text-document-id (hash-ref params "textDocument"))
       (define version (hash-ref versioned-text-document-id "version"))
       (define uri (hash-ref versioned-text-document-id "uri"))
       ;; TODO implement incremental updates
       (define text (hash-ref (car (hash-ref params "contentChanges")) "text"))
       (display "textDocument/didChange: ") (display uri) (newline)
       (update-document out state uri text))
      (($ <request> id "textDocument/definition" params)
       (define uri (hash-ref (hash-ref params "textDocument") "uri"))
       (define position (scm->position (hash-ref params "position")))
       (define documents (state-documents state))
       (define document (vhash-ref documents uri))
       ;; FIXME: handle missing document
       (display "textDocument/definition ") (display uri)
       (display " at ") (display position) (newline)
       (let ((location (find-definition documents document position)))
         (send-result out id (if location (location->scm location) #nil)))
       state)
      (($ <request> id "textDocument/formatting" params)
       (define uri (hash-ref (hash-ref params "textDocument") "uri"))
       (define documents (state-documents state))
       (define document (vhash-ref documents uri))
       ;; FIXME: handle missing document
       (display "textDocument/formatting ") (display uri) (newline)
       (let* ((old-text (document-text document))
              (full-range (make-range
                           (make-position 0 0)
                           (make-position (+ 1 (string-count old-text
                                                             #\newline))
                                          0)))
              (escm (string->escm-list old-text))
              (new-text (escm-list->indented-string
                         escm
                         ;; FIXME: always assumes uris of file:// kind
                         #:emacs-dir-locals-path (dirname
                                                  (string-drop uri 7))))
              (text-edit (make-text-edit full-range new-text)))
         (send-result out id (list (text-edit->scm text-edit))))
       state)
      (($ <request> #f method _)
       (display (string-append "Got notification with unknown method: " method "\n"))
       state)
      (($ <request> id method _)
       (display (string-append "Got request for unknown method: " method "\n"))
       (send-error out id method-not-found "Method not found")
       state)))
  (if (eq? result #f)
      #f
      (main-loop in out result)))

(define (main in out)
  (define initial-state (wait-for-init in out))
  (if (eq? initial-state #f)
      #f
      (main-loop in out initial-state)))
