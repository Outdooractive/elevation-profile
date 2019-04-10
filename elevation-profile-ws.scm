;;;
;;; elevation profile web-service client
;;;
;;;   Copyright (c) 2012,2013 Jens Thiele <karme@karme.de>
;;;   http post workaround in ifdef taken from gauche
;;;   Copyright (c) 2000-2012  Shiro Kawai  <shiro@acm.org>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
(define-module elevation-profile-ws
  (use elevation-profile)
  (use www.cgi)
  (use www.fastcgi)
  (use util.list)
  (use google-elevation)
  (use gauche.sequence)
  (use svg-plot)
  (use sxml.serializer)
  (use gauche.version)
  (use rfc.json)
  (export
   elevation-profile-ws-main))

(select-module elevation-profile-ws)

;; todo: there must be a better way!
(define-macro (ifdef c x)
  (cond [(boolean? c)
         (if c x '#t)]
        [else
         `(ifdef ,(eval c
                        (current-module) ;; ouch
                        ) ,x)]))

(ifdef (version<? (gauche-version) "0.9.1")
       ;; ugly workaround for old gauche (debian/squeeze)
       ;; see also gauche-devel mailing list
       ;; Message-ID: <87ocjnhids.fsf@thialfi.karme.de>
       (with-module www.cgi
         (with-output-to-port (current-error-port)
           (lambda()
             (print "WARNING: using http post hack for old gauche version")))
         (define (get-mime-parts part-handlers ctype clength inp)
           (define (part-ref info name)
             (rfc822-header-ref (ref info 'headers) name))

           (define (make-file-handler prefix honor-origfile? mode)
             (lambda (name filename part-info inp)
               (receive (outp tmpfile) (sys-mkstemp prefix)
                 (cgi-add-temporary-file tmpfile)
                 (mime-retrieve-body part-info inp outp)
                 (close-output-port outp)
                 (when mode (sys-chmod tmpfile mode))
                 (if honor-origfile?
                   (list tmpfile filename)
                   tmpfile))))

           (define (string-handler name filename part-info inp)
             (mime-body->string part-info inp))
           
           (define (ignore-handler name filename part-info inp)
             (let loop ((ignore (read-line inp #t)))
               (if (eof-object? ignore) #f (loop (read-line inp #t)))))
           
           (define (get-action&opts part-name)
             (let1 clause (find (lambda (entry)
                                  (or (eq? (car entry) #t)
                                      (and (regexp? (car entry))
                                           (rxmatch (car entry) part-name))
                                      (and (list? (car entry))
                                           (member part-name (map x->string (car entry))))
                                      (string=? (x->string (car entry)) part-name)))
                                part-handlers)
               (cond
                ((or (not clause)
                     (not (pair? (cdr clause))))
                 (list string-handler)) ;; default action
                ((and (= (length clause) 3)
                      (memq (cadr clause) '(file file+name))
                      (string? (caddr clause)))
                 ;; backward compatibility - will be deleted soon
                 (list (cadr clause) :prefix (caddr clause)))
                (else (cdr clause)))))
           
           (define (get-handler action . opts)
             (cond
              ((not action) string-handler)
              ((memq action '(file file+name))
               (make-file-handler (get-keyword* :prefix opts
                                                (build-path (temporary-directory)
                                                            "gauche-cgi-"))
                                  (eq? action 'file+name)
                                  (get-keyword :mode opts #f)))
              ((eq? action 'ignore) ignore-handler)
              (else action)))
           
           ;; The value of content-disposition must be a properly quoted string
           ;; according to RFC2183 and RFC2045.  However, IE sends a pathname including
           ;; backslashes without quoting them.  As a compromise, we only consider
           ;; backslash escape if the following character is either #\" or #\\.
           ;; (This fix is provided by Tatsuya BIZENN).
           (define (content-disposition-string input)
             (read-char input)                   ; discard beginning DQUOTE
             (let1 r (open-output-string :private? #t)
               (define (finish) (get-output-string r))
               (let loop ((c (read-char input)))
                 (cond ((eof-object? c) (finish)) ; tolerate missing closing DQUOTE
                       ((char=? c #\")  (finish)) ; discard ending DQUOTE
                       ((char=? c #\\)
                        (let1 c (read-char input)
                          (cond ((eof-object? c) (finish)) ;; tolerate stray backslash
                                (else
                                 (unless (char-set-contains? #[\\\"] c)
                                   (write-char #\\ r))
                                 (write-char c r)
                                 (loop (read-char input))))))
                       (else (write-char c r) (loop (read-char input)))))))
           
           (define (parse-content-disposition field)
             (if field
               (rfc822-field->tokens field
                                     `((#[\"] . ,content-disposition-string)
                                       (,*rfc822-atext-chars* . ,rfc822-dot-atom)))
               '()))
           
           (define (get-option optname optregex opts)
             (cond [(member optname opts) => cadr]
                   [(any (lambda (token)
                           (and (string? token) (rxmatch optregex token)))
                         opts) => (cut rxmatch-after <>)]
                   [else #f]))
           
           (define (handle-part part-info inp)
             (let* ([cd   (part-ref part-info "content-disposition")]
                    [opts (parse-content-disposition cd)]
                    [name (get-option "name=" #/^name=/ opts)]
                    [filename (get-option "filename=" #/^filename=/ opts)])
               (cond
                ((not name)      ;; if no name is given, just ignore this part.
                 (ignore-handler name filename part-info inp)
                 #f)
                ((not filename)  ;; this is not a file uploading field.
                 (list name (string-handler name filename part-info inp)))
                ((string-null? filename) ;; file field is empty
                 (list name (ignore-handler name filename part-info inp)))
                (else
                 (let* ((action&opts (get-action&opts name))
                        (handler (apply get-handler action&opts))
                        (result (handler name filename part-info inp)))
                   (list name result))))))
           
           (let* ((inp (if (and clength (<= 0 (x->integer clength)))
                         (open-input-limited-length-port inp (x->integer clength))
                         inp))
                  (result (mime-parse-message inp `(("content-type" ,ctype))
                                              handle-part)))
             (filter-map (cut ref <> 'content) (ref result 'content))))))

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

(define (render-geojson pl)
  (list (cgi-header :content-type "text/javascript"
		    :|Access-Control-Allow-Origin| "*")
	(construct-json-string
	 `(("header" . (("status" . "ok")))
	   ("answer" . (("type" . "elevation")
			("contents" . #((("type" . "FeatureCollection")
					 ("features" . #((("type" . "Feature")
							  ("properties" . ())
							  ("geometry" . (("type" . "MultiPoint")
									 ("coordinates" . ,(map-to <vector>
												   (lambda (p)
												     (vector (car p) (cadr p) (caddr p)))
												   pl))))))))))))))))

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

(define (cgi-elevation-profile context params)
  (hack
   (let ((query (google-elevation-query params))
         ;; todo: restrict allowed values
         (jscallback (cgi-get-parameter "callback" params :default "")))
     ((assoc-ref `(("js"      . ,(cut google-elevation-v3-out jscallback <>))
		   ("sjs"     . ,(cut google-elevation-simple-out jscallback <>))
		   ("xml"     . ,(compose render-xml points->sxml))
		   ("sxml"    . ,(compose render-sxml points->sxml))
		   ("sexpr"   . ,render-sexpr)
		   ("svg"     . ,render-svg)
		   ("geojson" . ,render-geojson))
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

(define (elevation-profile-ws-main config . args)
  (when (eq? (port-buffering (current-error-port)) :none)
    (set! (port-buffering (current-error-port)) :line))
  ;; todo: why? too late anyway?!
  (sys-putenv "PATH" "/usr/local/bin:/usr/bin:/bin")
  (let* ((context (create-context (config)))
	 (handle-request (cut cgi-elevation-profile context <>)))
    (with-fastcgi (cut cgi-main handle-request)))
  0)
