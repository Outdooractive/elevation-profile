(define-module elprows
  (use elpro)
  (use www.cgi)
  (use www.fastcgi)
  (use util.list)
  (use google-elevation)
  (use gauche.sequence)
  (use svg-plot)
  (use sxml.serializer)
  (export
   elprows-main))

(select-module elprows)

(define (create-context config)
  (let1 get-z (apply dem-stack->xy->z* config)
    (alist->hash-table
     `((get-z . ,get-z)
       (polyline->3d . ,(get-polyline->3d get-z))
       (upsample-polyline->4d . ,(get-upsample-polyline->4d get-z))
       (sample-polyline->4d . ,(get-sample-polyline->4d get-z))
       ))))

;; todo: also in ...
(define (render-sxml sxml)
  (list (cgi-header :content-type "application/x-sxml")
	(write-to-string sxml)))

;; todo: also in ...
(define (render-xml sxml)
  (list (cgi-header :content-type "application/xml")
	(srl:sxml->xml ;; -noindent
	 `(*TOP*
	   (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
	   ,sxml))))

(define (render-sexpr sexpr)
  (list (cgi-header :content-type "text/scheme")
	(write-to-string sexpr)))

(define (render-svg pl)
  (list (cgi-header :content-type "image/svg+xml")
        (with-output-to-string
          (lambda()
            (current-output-port)
            (svg-plot (list
                       (if (= (length (car pl)) 4)
                         (map (cut permute <> '(3 2)) pl)
                         (map-with-index (lambda(idx x) (list idx (ref x 2))) pl))))))))

(define (points->sxml pl)
  `(result . ,(map (lambda(p)
		     (list 'p (string-join (map x->string p) ",")))
		   pl)))

(define-macro (hack x)
  `(guard (e (else
              (error
               (with-output-to-string
                 (lambda()
		   (report-error e)
                   (with-error-to-port (current-output-port)
                     (lambda()
                       (report-error e))))))))
          ,x))

(define (cgi-elpro context params)
  (hack
   (let ((query (google-elevation-query params)))
     ((assoc-ref `(("js"    . ,google-elevation-v3-out)
		   ("sjs"   . ,google-elevation-simple-out)
		   ("xml"   . ,(compose render-xml points->sxml))
		   ("sxml"  . ,(compose render-sxml points->sxml))
		   ("sexpr" . ,render-sexpr)
		   ("svg"   . ,render-svg))
		 (cgi-get-parameter "format" params :default "js"))
      (case (car query)
	[(path-elevation-sample)
	 (apply (ref context 'sample-polyline->4d) (cdr query))]
	[(path-elevation-upsample)
	 (apply (ref context 'upsample-polyline->4d) (cdr query))]
	[(elevation)
	 (apply (ref context 'polyline->3d) (cdr query))]
	[else
	 (error "todo")])))))

(define (elprows-main config . args)
  (when (eq? (port-buffering (current-error-port)) :none)
    (set! (port-buffering (current-error-port)) :line))
  ;; todo: why? too late anyway?!
  (sys-putenv "PATH" "/usr/local/bin:/usr/bin:/bin")
  (let* ((context (create-context (config)))
	 (handle-request (cut cgi-elpro context <>)))
    (with-fastcgi (cut cgi-main handle-request)))
  0)
