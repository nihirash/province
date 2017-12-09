;;
;; View layer helpers
;; (c) 2017 Alexander Sharikhin
;; Part of province web engine 
;;

;; Mine logicless templating 
(define (tpl->string template #!optional list-of-params)
    (unless (string? template) (abort '"Template must be a string"))
    (unless (or (list? list-of-params) (eq? #f list-of-params)) (abort '"List-of-params must be #f or list"))
    (when (list? list-of-params)
         (let ((replacements (map        
                                (lambda (element) 
                                    (cons 
                                        (string-append "<% " (car element) " %>") 
                                        (cadr element)))
                                list-of-params)))
            (set! template (string-translate* template replacements)))) 
    (string-substitute* template '(("<% .+ %>" . ""))))

(define tpl-cache (make-hash-table))

;; Read file and use it as template
(define (file-tpl->string file-name #!optional list-of-params)
    (unless (string? file-name) (abort '"File name must be string"))
    (cond 
        ((hash-table-ref/default tpl-cache file-name #f) 
         (tpl->string (hash-table-ref tpl-cache file-name) list-of-params))
        (else 
            (hash-table-set! tpl-cache file-name (file->string (string-append "tpl/" file-name)))
            (file-tpl->string file-name list-of-params))))

;; Layouted view
(define (layouted-view layout-file-name file-name list-of-params #!key layout-params)
    (unless (string? layout-file-name) (abort '"Layout file name must be a string"))
    (unless (string? file-name) (abort '"File name of template must be a string"))
    (let* ((content (file-tpl->string file-name list-of-params)))
          (layout-fields `(("Content" ,content)))
          (when layout-params (merge! layout-fields layout-params))
          (file-tpl->string (string-append "layouts/" layout-file-name) layout-fields)))
