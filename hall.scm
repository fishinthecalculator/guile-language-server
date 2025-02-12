(hall-description
 (name "language-server")
 (prefix "guile")
 (version "0.1")
 (author "Jonas Herzig")
 (copyright (2018))
 (synopsis "LSP server for GNU Guile")
 (description "")
 (home-page "")
 (license gpl3+)
 (dependencies
  `(("guile-json" (json) ,guile-json-4)))
 (skip ("wrapper"
        "tests/dir-locals"))
 (files (libraries
         ((scheme-file "json-rpc")
          (scheme-file "format")
          (directory
           "language-server"
           ((directory
             "guile"
             ((scheme-file "xref")
              (scheme-file "server")
              (scheme-file "formatter")
              (scheme-file "extended-scheme")
              (scheme-file "compile")))
            (scheme-file "scm-utils")
            (scheme-file "protocol")
            (scheme-file "emacs")))))
        (tests ((directory
                 "tests"
                 ((scheme-file "formatter")
                  (scheme-file "extended-scheme")
                  (scheme-file "compile")))))
        (programs
         ((directory
           "scripts"
           ((in-file "guile-ls")))))
        (documentation
         ((org-file "README")
          (symlink "README" "README.org")
          (text-file "HACKING")
          (text-file "COPYING")
          (directory "doc" ((texi-file "language-server")))
          (text-file "NEWS")
          (text-file "AUTHORS")
          (text-file "ChangeLog")))
        (infrastructure
         ((scheme-file "guix")
          (text-file ".gitignore")
          (scheme-file "hall")
          (directory
           "build-aux"
           ((scheme-file "test-driver")))
          (autoconf-file "configure")
          (automake-file "Makefile")
          (in-file "pre-inst-env")))))
