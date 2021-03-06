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
  (use math.const)
  (export dem-stack->xy->z*
          dem-stack->xy->z-debug*
          get-polyline->3d
          get-polyline->3d-debug
          get-upsample-polyline->4d
          get-upsample-polyline->4d-debug
          get-sample-polyline->4d
          get-sample-polyline->4d-debug))

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

(define (default-dem-stack-debug)
  (map (lambda(l)
         (if (keyword-exists? :next (cdr l))
             ;; replace
             (let1 ov (get-keyword :next (cdr l))
               (cons (car l) (append
                              (delete-keyword :next (cdr l))
                              `(:next ,(cond [(number? ov)
                                              (lambda (x y :optional (depth 0)) (values ov
                                                                          ;; resolution in m
                                                                          (* 6378137 2 pi)
                                                                          depth))]
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

(define (dem-stack->xy->z-debug* . args)
  (let-optionals* args ((projection "epsg:4326")
                        (dem-stack (default-dem-stack-debug)))
    (let1 f (cute apply (dem-stack->xy->z-debug projection dem-stack) <>)
      (lambda l
        (map (lambda(x) (values->list (f x))) l)))))

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

(define (polyline->3d-debug get-z-debug pl)
  (map append pl (apply get-z-debug pl)))

(define (get-polyline->3d get-z)
  (cut polyline->3d get-z <>))

(define (get-polyline->3d-debug get-z-debug)
  (cut polyline->3d-debug get-z-debug <>))

(define (get-upsample-polyline->4d get-z)
  (lambda(s pl max-dist)
    (geod-add-measure s (polyline->3d get-z (geod-upsample-polyline s pl max-dist)))))

(define (get-upsample-polyline->4d-debug get-z-debug)
  (lambda(s pl max-dist)
    ;; we want x,y,z,d,res... instead of x,y,z,res,...,d
    (map (lambda(p)
           (append (subseq p 0 3)
                   (list (last p))
                   (subseq p 3 (- (length p) 1))))
         (geod-add-measure s (polyline->3d-debug get-z-debug (geod-upsample-polyline s pl max-dist))))))

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

(define (get-sample-polyline->4d-debug get-z-debug)
  (lambda(s pl samples)
    (let1 plm (geod-sample-polyline-with-measure s pl samples)
      (map (lambda(xyz-debug xyd)
             (append (subseq xyz-debug 0 3)
                     (list (last xyd))
                     (subseq xyz-debug 3)))
           (polyline->3d-debug get-z-debug (map (cute subseq <> 0 2) plm))
           plm))))
