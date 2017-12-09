;;
;; Auth module
;; (c) 2017 Alexander Sharikhin
;; Part of province web engine
;;

;; Login trampoline with storing user name in session
(define-login-trampoline login-tramp 
    hook: (lambda (user)
            ($session-set! 'user-id (user-name->user-id user))))

;; Page that's doesn't need authorization            
(define (simple-page url handler)
    (define-page url handler no-session: #t))

;; check user's role  
(define (must-be role handler)
    (let ([my-role (get-user-role ($session 'user-id))])    
         (cond
            [(equal? my-role role) (handler)]
            [else #f])))

(simple-page (login-page-path)
    (lambda ()
        (file-tpl->string   "login.html"
                            `(("login-url" ,login-tramp)
                              ("message" ,($ 'reason ""))))))

(define-page "/logout"
    (lambda ()
        (session-destroy! (or (read-cookie (session-cookie-name)) ($ 'sid)))
        (redirect-to "/")))                              

(valid-password?
    (lambda (user password)
        (let ([user-data (auth-user user password)])
             (cond
                [(equal? user-data #f) #f]
                [else #t]))))
        
(define (password-hash password)
    (message-digest-string (sha256-primitive) password))

(define (create-user name password role)
    ($db "insert into users(name, password, role) values (?, ?, ?)"
        values: (list name (password-hash password) (?? role ""))))

(define (user-name->user-id name)
    (?? (caar ($db "select id from users where name=?" values: (list name)))
        0))

(define (change-password id password)
    ($db "update users set password=? where id=?" values: (list (password-hash password) id)))

(define (delete-user id)
    ($db "delete from users where id=?" values: (list id)))

(define (get-users-list)
    ($db "select id, name, role from users"))

(define (get-user id)
    ($db "select name, role from users where id=?" values: (list id)))

(define (update-user id name role)
    ($db "update users set name=?, role=? where id=?" values: (list name role id)))

(define (auth-user name password)
    (let ([user-record 
            ($db "select name from users where name=? and password=?" 
                values: (list name (password-hash password)))])
        (cond
            [(equal? user-record '()) #f]
            [else (car user-record)])))

(define (get-user-role id)
    (caar ($db "select role from users where id=?" values: (list id))))
