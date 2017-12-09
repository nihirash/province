(include "admin/users.scm")

(admin-page "/admin/"
    (lambda ()
        "Admin page"))

(admin-page "/admin/reload"
    (lambda ()
        (reload-apps (awful-apps))
        (redirect-to "/admin/")))
