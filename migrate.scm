#!/usr/bin/env csi
;;
;; Province Web engine
;; Installation script
;; (c) 2017 Alexander Sharikhin
;;

(use sql-de-lite message-digest sha2)
    
(include "config.scm")

(define migrations 
    (list "CREATE TABLE users( 
        id integer primary key autoincrement,
        name text not null unique,
        password text not null,
        role text)"
        "insert into users(name, password, role) 
            values 
            (\"admin\", \"8c6976e5b5410415bde908bd4dee15dfb167a9c873fc4bb8a81f6f2ab448a918\", \"admin\")"))

(let ((db-conn (open-database db-file)))
    (map 
        (lambda (stmt)
            (condition-case 
                (exec (sql db-conn stmt))
                [(exn) "already made"])) 
        migrations)
    (close-database db-conn))

(exit 0)
