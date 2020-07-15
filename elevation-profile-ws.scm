;;;
;;; elevation profile web-service client
;;;
;;;   Copyright (c) 2012,2013 Jens Thiele <karme@karme.de>
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
  (export elevation-profile-ws-main))

(select-module elevation-profile-ws)

(define (create-context config)
  (let ((get-z (apply dem-stack->xy->z* config))
	(get-z-debug (apply dem-stack->xy->z-debug* config)))
    (alist->hash-table
     `((get-z . ,get-z)
       (polyline->3d . ,(get-polyline->3d get-z))
       (polyline->3d-debug . ,(get-polyline->3d-debug get-z-debug))
       (upsample-polyline->4d . ,(get-upsample-polyline->4d get-z))
       (upsample-polyline->4d-debug . ,(get-upsample-polyline->4d-debug get-z-debug))
       (sample-polyline->4d . ,(get-sample-polyline->4d get-z))
       (sample-polyline->4d-debug . ,(get-sample-polyline->4d-debug get-z-debug))
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

;; todo: also in ...
(define (round-at-func p)
  (let* ((x (expt 10 p))
         ;; gauche uses ieee 64 bit double with 53 bits mantissa
         ;; => calculate smallest number where ulp(number) >= 1/10^p
         (rmax (expt 2 (ceiling (- 52 (/ (* (log 10) p) (log 2)))))))
    (lambda(n)
      ;; if ulp(n) is bigger than wanted precision there is no point in rounding at all
      (if (>= (abs n) rmax)
        n
        (let1 r (/. (round (*. n x)) x)
          (if (integer? r)
            (exact r)
            r))))))

(define round-z (round-at-func 2))
(define round-d (round-at-func 2))
(define round-res (round-at-func 2))

(define (render-geojson pl debug)
  (list (cgi-header :content-type "text/javascript"
		    :|Access-Control-Allow-Origin| "*")
	(construct-json-string
	 `(("header" . (("status" . "ok")))
	   ("answer" . (("type" . "elevation")
			("contents"
                         . #((("type" . "FeatureCollection")
                              ("features" . #((("type" . "Feature")
                                               ("properties" . ())
                                               ("geometry" . (("type" . "MultiPoint")
                                                              ("coordinates" . ,(map-to <vector>
                                                                                        (lambda (p)
                                                                                          (cond [(not debug)
                                                                                                 ;; note: we always drop d
                                                                                                 (vector (car p)
                                                                                                         (cadr p)
                                                                                                         (round-z (caddr p)))]
                                                                                                [(= (length p) 4)
                                                                                                 (vector (car p)
                                                                                                         (cadr p)
                                                                                                         (round-z (caddr p))
                                                                                                         (round-res (cadddr p)))]
                                                                                                [(= (length p) 5)
                                                                                                 (vector (list-ref p 0)
                                                                                                         (list-ref p 1)
                                                                                                         (round-z (list-ref p 2))
                                                                                                         (round-d (list-ref p 3))
                                                                                                         (round-res (list-ref p 4)))]
                                                                                                [else
                                                                                                 (error "todo")]))
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
         (jscallback (cgi-get-parameter "callback" params :default ""))
	 (debug (cgi-get-parameter "debug" params)))
     ((assoc-ref `(("js"      . ,(cut google-elevation-v3-out jscallback <>))
		   ("sjs"     . ,(cut google-elevation-simple-out jscallback <>))
		   ("xml"     . ,(compose render-xml points->sxml))
		   ("sxml"    . ,(compose render-sxml points->sxml))
		   ("sexpr"   . ,render-sexpr)
		   ("svg"     . ,render-svg)
		   ("geojson" . ,(cut render-geojson <> debug)))
                 (cgi-get-parameter "format" params :default "js"))
      (case (car query)
	[(path-elevation-sample)
	 (apply (ref context (string->symbol (string-append "sample-polyline->4d" (if debug "-debug" "")))) (cdr query))]
	[(path-elevation-upsample)
	 (apply (ref context (string->symbol (string-append "upsample-polyline->4d" (if debug "-debug" "")))) (cdr query))]
	[(elevation)
	 (apply (ref context (string->symbol (string-append "polyline->3d" (if debug "-debug" "")))) (cdr query))]
	[else
	 (error "todo")])))))

(define (elevation-profile-ws-main config . args)
  (when (version<? (gauche-version) "0.9.1")
    (error "gauche too old"))
  (when (eq? (port-buffering (current-error-port)) :none)
    (set! (port-buffering (current-error-port)) :line))
  ;; todo: why? too late anyway?!
  (sys-putenv "PATH" "/usr/local/bin:/usr/bin:/bin")
  (let* ((context (create-context (config)))
	 (handle-request (cut cgi-elevation-profile context <>)))
    (with-fastcgi (cut cgi-main handle-request)))
  0)
