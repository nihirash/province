;;
;; Some basic utils
;; (c) 2017 Alexander Sharikhin
;; Part of province web engine
;;

;; Add element to list on the fly
(define-syntax push!
    (syntax-rules ()
        ((push! place item)
         (set! place (append place 
                             (list item))))))

;; Merge 2 lists to first
(define-syntax merge!
    (syntax-rules ()
        ((merge! place items)
         (set! place (append place items)))))
    

;; If element empty - takes alternative
(define-syntax ??
    (syntax-rules ()
        ((?? element alternative)
         (cond 
            (element element)
            (else alternative)))))

;; Simple quoting
(define (qq identifier)
 (format #f
         "\"~a\""
         (string-translate* (format #f "~a" identifier) '(("\"" . "\\\"")))))        
            
;; I don't want render anything with sxml, but something i will
(define (render-sxml sxml)
    (with-output-to-string (lambda ()
                            (SXML->HTML sxml))))


;; Read file to char_list
(define (file->char_list path)
    (call-with-input-file path
        (lambda (input-port)
            (let loop ((x (read-char input-port)))
                (cond 
                   ((eof-object? x) '())
                   (#t (begin (cons x (loop (read-char input-port))))))))))

;; Reads file to string
(define (file->string path)
    (apply string (file->char_list path)))
