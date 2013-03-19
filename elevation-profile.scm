;;;
;;; elevation profile
;;;
;;;   Copyright (c) 2012 Jens Thiele <karme@karme.de>
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
(define-module elevation-profile
  (use srfi-1)
  (use util.list)
  (use gauche.sequence)
  (use dem-gdal)
  (use geod)
  (export dem-stack->xy->z*
          get-polyline->3d
          get-upsample-polyline->4d
          get-sample-polyline->4d))

(select-module elevation-profile)

(define (keyword-exists? key kv-list)
  (or (get-keyword key kv-list #f)
      (not (equal? 1 (get-keyword key kv-list 1)))))
  
(define (default-dem-stack)
  (map (lambda(l)
         (if (keyword-exists? :next (cdr l))
             ;; replace
             (let1 ov (get-keyword :next (cdr l))
               (cons (car l) (append
                              (delete-keyword :next (cdr l))
                              `(:next ,(cond [(number? ov)
                                              (lambda _ ov)]
                                             [else
                                              ;; todo: maybe just eval?!
                                              (error "not allowed" ov)])))))
             l))
       (with-input-from-file "/etc/elevation-profile" read)))

(define (dem-stack->xy->z* . args)
  (let-optionals* args ((projection "epsg:4326")
                        (dem-stack (default-dem-stack)))
    (let1 f (cute apply (dem-stack->xy->z projection dem-stack) <>)
      (lambda l
        (map f l)))))

;; todo: also in ...
(define-macro (debug-assert e)
  `(when (not ,e)
     (error "Debug assertion failed: " ,(x->string e))))

;; todo: also in ...
(define (zip-append-elt ll el)
  ;;(debug-assert (= (size-of ll) (size-of el)))
  (map (lambda(l e)
         ;;(debug-assert (list? l))
         ;;(debug-assert (not (list? e)))
         (append l (list e)))
       ll el))

(define (polyline->3d get-z pl)
  (zip-append-elt pl (apply get-z pl)))

(define (get-polyline->3d get-z)
  (cut polyline->3d get-z <>))

(define (get-upsample-polyline->4d get-z)
  (lambda(s pl max-dist)
    (geod-add-measure s (polyline->3d get-z (geod-upsample-polyline s pl max-dist)))))

;; for profiling use:
;; (define (get-upsample-polyline->4d get-z)
;;   (lambda(s pl max-dist)
;;     (let1 up #?=(geod-upsample-polyline s pl max-dist)
;;           (let1 z #?=(polyline->3d get-z up)
;;                 #?=(geod-add-measure s z)))))

(define (get-sample-polyline->4d get-z)
  (lambda(s pl samples)
    (let1 plm (geod-sample-polyline-with-measure s pl samples)
      (zip-append-elt (polyline->3d get-z
                                    (map (cute subseq <> 0 2) plm))
                      (map (cute ref <> 2) plm)))))
