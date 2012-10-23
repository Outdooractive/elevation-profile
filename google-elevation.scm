;;; google elevation api-like functions 
;;;
;;;   Copyright (c) 2010-2012 Jens Thiele <karme@karme.de>
;;;   based on debugger.scm
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

(define-module google-elevation
  (use format-json) ;; todo: replace with rfc.json
  (use sxml.sxpath)
  (use gauche.sequence)
  (use www.cgi)
  (export google-elevation-query
          google-elevation-v3-out
          google-elevation-simple-out))

(select-module google-elevation)

(define (parse-coordinates s)
  (map
   (lambda(p)
     (permute
      (map x->number
	   (string-split p ","))
      '(1 0)))
   (string-split s "|")))

(define (google-elevation-query params)
  (let ((path (cgi-get-parameter "path" params :default #f))
	(samples (x->number (cgi-get-parameter "samples" params :default #f)))
	(upsample (x->number (cgi-get-parameter "upsample" params :default #f))) ;; not part of google api
	(locations (cgi-get-parameter "locations" params :default #f))
        (spheroid (string->symbol (cgi-get-parameter "spheroid" params :default "wgs84")))  ;; not part of google api? looks like google uses sphere
        )
    (let ((points (if path
                    (parse-coordinates path)
                    (parse-coordinates locations))))
      (if path
        (if (not (zero? samples))
          (list 'path-elevation-sample spheroid points samples)
          (list 'path-elevation-upsample spheroid points upsample))
        (list 'elevation points)))))

;; todo: also in ...
(define json-format-number
  (let1 f (format-number-fixed-point 7)
	(lambda(n)
	  (if (nan? n)
	      'NaN
	      (f n)))))

;; todo: also in ...
(define (json-display-number x)
  (display (json-format-number x)))

;; todo: also in ...
(define to-json (cut ->jsonf <> `((number . ,json-display-number))))

(define (google-elevation-v3-out pl)
  (list (cgi-header :content-type "text/javascript")
	(to-json
	 `((status . "OK")
	   (results . ,(map-to <vector>
			       (lambda(p)
				 (append
				  `((location . ((lat . ,(ref p 1))
						 (lng . ,(ref p 0))))
				    (elevation . ,(ref p 2)))
				  (if (> (size-of p) 3)
				      `((distance . ,(ref p 3)))
				      '())))
			       pl))))))

;; todo: not related to google api in any way
;; remove? rename?
(define (google-elevation-simple-out pl)
  (list (cgi-header :content-type "text/javascript")
	(to-json
	 `((status . "OK")
	   (results . ,(map-to <vector>
			       (cut coerce-to <vector> <>)
			       pl))))))
