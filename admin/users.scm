;;
;; User administration component
;; (c) 2017 Alexander Sharikhin
;; Part of province web engine
;;

(push! admin-menu-items (cons "/admin/users/" "Users"))

;; User edition form
(define (user-form id name role)
    `(form  (@ (method "post")
              (action "/admin/users/upd-api"))
            ,(if id (hidden-input "id" id) "") 
            ,(text-input "name" "User name:" name)
            ,(text-input "role" "User role:" role)
            ,(password-input "password" "User password:" "")
            (div    (@ (class "row"))
                    (div (@ (class "col-sm-6")) ,(submit-button "Save"))
                    (div (@ (class "col-sm-6")) ,(if (and id (not (equal? (string->number id) ($session 'user-id)))) 
                                                     (danger-button-link (format #f "/admin/users/delete/?id=~a" id) "Delete")
                                                     "")))))

;; Users list
(admin-page "/admin/users/"
    (lambda () 
        (render-sxml `((div     (@ (class "button-group"))
                                ,(button-link "/admin/users/edit" "Create user")
                        (hr))
                       ,(grid-db   "users" 
                                    '("id" "name" "role") 
                                    "/admin/users/" 
                                    headers: '("Id" "Имя" "Роль")
                                    render?: #f)))))

;; Entry-point for updating users
(admin-page "/admin/users/upd-api"
    (lambda ()
        (let    ([name ($ 'name)]
                 [role ($ 'role)]
                 [password ($ 'password)]
                 [id ($ 'id)]) 
                (cond
                    [(not id)   (create-user name password role)]
                    [else       (admin-update-user id name role password)]))
        (redirect-to "/admin/users/")))

(define (admin-update-user id name role password)
    (update-user id name role)
    (when password (change-password id password)))

;; User  deletion
(admin-page "/admin/users/delete"
    (lambda ()
        (let ([id ($ 'id)])
             (cond
                [id (delete-user id)
                    (redirect-to "/admin/users/")]
                [else (redirect-to "/admin/users/")]))))        
        
(admin-page "/admin/users/edit"
    (lambda ()
        (let ([id ($ 'id)]
              [name ""]
              [role "user"])
             (if id 
                 (let ([user-data (get-user id)])
                      (set! name (caar user-data))
                      (set! role (cadar user-data))))
             (render-sxml (user-form id name role)))))
