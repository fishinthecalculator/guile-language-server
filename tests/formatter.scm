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

(define-module (tests formatter)
  #:use-module (language-server guile extended-scheme)
  #:use-module (language-server guile formatter)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(eval-when (expand load eval)
  (define (read-custom-hash chr port)
    (read-char port)
    (read port)
    (read-char port)
    #f)
  (read-hash-extend #\~ read-custom-hash))

(define* (test-indent base-indent-str strip-input . args)
  (define base-indent (string-length (string-drop base-indent-str 1)))
  (define (fix-indent str)
    (match (string-split str #\nl)
      ((first . rest)
       (cons first
             (map (lambda (line) (string-drop line base-indent))
                  rest)))))
  (for-each (lambda (lines)
              (define expected (string-join lines "\n"))
              (define input (if strip-input
                              (string-join
                               (map (lambda (it) (string-trim it #\space))
                                    lines)
                               "\n")
                              expected))
              (define escm-list (string->escm-list input))
              (define actual
                (escm-list->indented-string
                 escm-list
                 #:emacs-dir-locals-path (string-append
                                          (dirname (current-filename))
                                          "/dir-locals/testing")
                 #:newline-on-eof? (string-suffix? "\n" expected)))
              (test-equal expected actual))
            (map fix-indent args)))

(test-group "indent"
            (test-indent
             "
    "
             #t
             "()"
             "(test)"
             "(test a)"
             "(test a b)"
             "(test a b c)"
             "(test a
          b)"
             "(test a b
          c)"
             "(test
     a
     b)"
             "((test) a
            b)"
             "((test)
     a
     b)"
             "((test a
           b)
     b
     b)"
             "(begin a
           b)"
             "(begin a b
           c)"
             "(begin
      a
      b)"
             "(a b (c d
            e)
       f)"
             "(define a
      1)"
             "(define
        a
      1)"
             "(define ; comment
        a
      1)"
             "(define (a b
               c
               d)
      (b c
         d))"
             "(let ((a 1)
          (b 2)
          (c 3))
      a
      b)"
             "(a
     \"test\")"
             "'a"
             "`(a)"
             "`(a
      ,b
      ,@(list
         a
         b))"
             "(a b)
    (c d)
    
    (e f)"
             "\f
    
    \f"
             "(unless (a? 1)
      (b 1))"
             ;; Check that parenthesis are never moved into the same line as a comment
             "(a
     ; comment
     )
    (a b
       ; comment
       )"
             ;; Check emacs-magic parsing and application
             "(qwe
     ;; emacs: (put 'test 'scheme-indent-function 2)
     (test a
         b
       c)
     ;;;;    eval:(put 'testing 'scheme-indent-function 1)
     (testing a
       b
       c))"
             ;; Test emacs .dir-locals.el parsing and application
             "(dir-locals-nil a
      (dir-locals-scheme a b
        c)
      (dir-locals-parent a
        b))"
             ;; Ensure keyword argument lists are indented correctly
             ;; This isn't vanilla emacs behavior but guix has a patch for it.
             "(arguments
     `(#:some-key #t
       #:other-key 1
       #:rest '()))"
             ;; Check formatting inside special hash readers
             ";; emacs: (modify-syntax-entry ?~ \"'\")
    (proc
     #~pcontent) ;; Note: the closing paren here is part of the #~ syntax
     #~p(proc
         (scheme inside)
         #:other-key 1
         #:rest '())s)"
             "")

            (test-indent
             " "
             #f
             ;; Check that no spaces are inserted on empty lines
             "(test a\n\n      b)"
             ;; Check live Guile files
             ;; (not included in unit tests because we have no control over
             ;;  them; purely here for manual testing out of curiosity)
             ;(call-with-input-file
             ;  ((@ (language-server scm-utils) find-name-in-load-path) "language/tree-il.scm")
             ;  get-string-all)
             ;; Check this file
             (call-with-input-file (current-filename) get-string-all)))
