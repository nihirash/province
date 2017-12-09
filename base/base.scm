;;
;; Basic initialization
;; (c) 2017 Alexander Sharikhin
;; Part of province web engine
;;

(db-credentials "province.db")
(enable-db)

;; submodules        
(include "base/utils.scm")
(include "base/view.scm")
(include "base/auth.scm")
(include "base/admin.scm")