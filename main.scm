;;
;; Province Web engine
;; (c) 2017 Alexander Sharikhin
;;

(use awful 
    awful-sql-de-lite
    sql-de-lite
    spiffy
    utf8
    srfi-69
    message-digest
    sha2
    srfi-13
    srfi-28
    sxml-transforms
    http-session
    spiffy-cookies)

(root-path (string-append (current-directory) "/htdocs/"))

(enable-session #t)
(include "config.scm")
(include "base/base.scm")
(include "admin/main.scm")

(simple-page "/"
    (lambda ()
        "Hello world!"))

(enable-web-repl "web-repl")                
(web-repl-access-control 
    (lambda () #t))

