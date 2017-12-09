;;
;; Basic admin-pages helpers and UI components
;; (c) 2017 Alexander Sharikhin
;; Part of province web engine
;;

(define rights-error-message "<h1>Error</h1> <p>Not enought rights to do this</p>")
(define admin-menu-items (list))

;; Admin page handler
(define (admin-page url handler)
  (define-page url 
      (lambda () 
          (let ([content (must-be "admin" handler)])
              (cond 
                   [(equal? content #f) rights-error-message]
                   [else (admin-view content)])))
      method: '(get post))) 
;; Render menu
(define (admin-menu)
  (string-join (map 
                  (lambda (element) 
                      (format #f 
                            "<li class=\"nav-item active\"><a class=\"nav-link\" href=\"~a\">~a</a></li>"
                            (car element) (cdr element)))
                  admin-menu-items)))

;; Render template for admin's pages
(define (admin-view content)
  (file-tpl->string "layouts/admin.html"
                      `(("Content" ,content)
                        ("Menu" ,(admin-menu)))))

(define (ui-table headers values)
  (unless (list? headers) (abort '"Headers must be a list"))
  (unless (list? values) (abort '"Values must be a list")))

;; It's a awful grid component
(define (grid-db table-name columns url #!key callback headers render?)
  ;; Draw table from data
  (define (tabularize data #!key header callback)
    (append '(table (@ (class "table table-striped table-hover table-bordered")))
            (if header
                `((thead (@ (class "thead-dark"))
                    (tr ,@(map (lambda (item)
                                 `(th ,item))
                               header))))
                '())
            (let ((body (map (lambda (line)
                               (append '(tr)
                                       (map (lambda (cell)
                                              `(td ,(cond
                                                      [callback (callback line cell)]
                                                      [else cell])))
                                            line)))
                             data)))
              body)))
  
  ;; Get data from DB
  (define (build-sql-select table-name columns limit offset)
    (format #f
            "SELECT ~a FROM ~a LIMIT ~a, ~a"
            (string-intersperse (map qq columns) ",")
            (qq table-name)
            offset
            limit))
  
  ;; just to same url with edit action added
  (define (default-callback row cell)
    `(a (@ (href ,(string-append url "edit/" (format #f "?~a=~a" (car columns) (car row)))))
        ,cell))
  
  ;; Sorry for this. This is awful very paginator
  (define (paginator limit)
    (when (string? limit)
      (set! limit (string->number limit)))
    (let* ([total-records (caar ($db (format #f "SELECT COUNT(*) FROM ~a" table-name)))]
           [pages (/ total-records limit)]
           [counter 0]
           [page-offsets (iota pages 0 limit)])
      (if (< limit total-records)
        `((ul (@ (class "pagination"))
            ,(map
              (lambda (page)
                `(li (@ (class ,(if (equal? page (if ($ 'offset) 
                                                     (string->number ($ 'offset)) 
                                                     (car page-offsets)))
                                    "page-item active"
                                    "page-item"))) 
                     (a 
                        (@ (href ,(format #f "?offset=~a&limit=~a" page limit)) 
                           (class "page-link"))
                        ,(begin
                              (set! counter (+ counter 1))
                              counter))))
              page-offsets)))
        '())))
  
  ;; And... Here we renders
  (let* ([limit (?? ($ 'limit) 10)]
         [offset (?? ($ 'offset) 0)]
         [data ($db (build-sql-select table-name columns limit offset))]
         [sxml (list
                  (tabularize data header: (?? headers columns) callback: (?? callback default-callback))
                  (paginator limit))])
    (if render? (render-sxml sxml) sxml)))
     
(define (alert message)
  (format #f "<div class=\"alert alert-dismissible alert-warning\">
    <button type=\"button\" class=\"close\" data-dismiss=\"alert\">&times;</button>
    <h4>Warning!</h4>
    <p>~a</p>
  </div>" message))

;;
;; Produces sxml input-controls
;;

(define (text-input name label value)
    `(div (@ (class "form-group"))
          (label (@ (for ,name))
                 ,label)
          (input (@ (class "form-control")
                    (type "text")
                    (name ,name)
                    (id ,name)
                    (value ,value)))))

(define (hidden-input name value)
    `(input (@  (type "hidden")
                (name ,name)
                (value ,value))))

(define (password-input name label value)                
  `(div (@ (class "form-group"))
        (label (@ (for ,name))
               ,label)
        (input (@ (class "form-control")
                  (type "password")
                  (name ,name)
                  (id ,name)
                  (value ,value)))))
                  
(define (submit-button text)
    `(input (@  (type "submit")
                (class "btn btn-primary")
                (value ,text))))

(define (danger-button-link link text)
    `(a (@  (href ,link)
            (class "btn btn-danger"))
        ,text))

(define (button-link link text)
    `(a (@  (href ,link)
            (class "btn btn-primary"))
        ,text))

        