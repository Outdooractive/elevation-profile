;;;
;;; dem (digital elevation model) via gdal (http://www.gdal.org)
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

;; notes/todo:
;; - quite a hack
;; - get rid of c-wrapper / speedup
;; - leaks memory => call procedures only once if possible!
;; - you can use gdal's vrt format to merge images
;;   see also: http://www.gdal.org/gdal_vrttut.html
;;   and in general (WMS,....)
;;   http://www.gdal.org/formats_list.html
;; - a more general purpose gdal wrapper would be nice
;;   (upload to gl texture ...)
;; - use GDAL_CACHEMAX?
(define-module dem-gdal
  (use srfi-1)
  (use gauche.collection) ;; use after srfi-1 to make find work as expected!
  (use gauche.sequence)
  (use srfi-13)
  (use c-wrapper)
  (use gauche.array)
  (use gauche.uvector)
  (use gauche.process)
  ;;(use sxml.adaptor)
  (export dem->xy->z   
          dem->xy-project->z
          dem-stack->xy->z
          ))

(select-module dem-gdal)

(c-load '("gdal/gdal.h" "gdal/ogr_srs_api.h") :libs-cmd "gdal-config --libs")

;; todo: hmm
(CPLSetErrorHandler 0)

(define-macro (assert e)
  `(when (not ,e)
     (error "assertion failed: " ,(x->string e))))

(define (gdal-open-dataset name)
  (assert (string? name))
  (with-output-to-port (current-error-port)
    (lambda()
      (gdal-init)
      (let ((dataset (GDALOpen name GA_ReadOnly)))
        (cond [(not (null-ptr? dataset))
               (let ((driver (GDALGetDatasetDriver dataset)))
                 ;; (print #`"Driver ,(GDALGetDriverShortName driver)/,(GDALGetDriverLongName driver)")
                 ;; (print #`"Size is ,(GDALGetRasterXSize dataset)x,(GDALGetRasterYSize dataset)x,(GDALGetRasterCount dataset)")
                 (when (not (null-ptr? (GDALGetProjectionRef dataset)))
                   ;; (print #`"Projection is ',(GDALGetProjectionRef dataset)'")
                   (let ((transform (make (c-array <c-double> 6))))
                     (when (= (GDALGetGeoTransform dataset transform) CE_None)
                       ;;#?=(map (cut cast <number> <>) transform)
                       ;; (print #`"Origin = ,(ref transform 0), ,(ref transform 3)")
                       ;; (print #`"Pixel Size = ,(ref transform 1), ,(ref transform 5)")
                       ))))
               dataset]
              [else
               (error "Unsupported format")])))))

(define (osr-from-user-input s)
  (let ((hSRS (OSRNewSpatialReference NULL))) ;; todo: leak!
    (when (not (= (OSRSetFromUserInput hSRS s) OGRERR_NONE))
      (error "OSRSetFromUserInput failed"))
    hSRS))

(define (osr-from-dataset dataset)
  (let ((hSRS (OSRNewSpatialReference NULL)))
    (when (not (= (OSRImportFromWkt hSRS (ptr (GDALGetProjectionRef dataset))) OGRERR_NONE))
      (error "OSRImportFromWkt failed"))
    hSRS))

(define (c-int->bool x)
  (not (zero? (cast <number> x))))

(define (osr-is-same? from to)
  (c-int->bool (OSRIsSame from to)))

(define-condition-type <transform-error> <error>
  transform-error?
  (pos transform-error-pos))

(define (osr-transform from to)
  (if (osr-is-same? from to)
    identity
    (let ((ct (OCTNewCoordinateTransformation from to))
          (xa (make (c-array <c-double> 1)))
          (ya (make (c-array <c-double> 1)))
          (za (make (c-array <c-double> 1))))
      (assert (not (null-ptr? ct)))
      (lambda(l)
        (set! (ref xa 0) (ref l 0))
        (set! (ref ya 0) (ref l 1))
        (set! (ref za 0) (ref l 2 0))
        (when (not (c-int->bool (OCTTransform ct 1 xa ya za)))
          (error <transform-error> :pos l))
        (list (ref xa 0) (ref ya 0) (ref za 0))))))

(define (osr-is-geographic? osr)
  (let1 r (c-int->bool (OSRIsGeographic osr))
    (assert (eq? r (not (osr-is-projected? osr))))
    r))

;; note: same as (not osr-is-geographic?)
(define (osr-is-projected? osr)
  (c-int->bool (OSRIsProjected osr)))

;; not used and not available in older gdal versions
;; (define (osr-is-compound? osr)
;;   (c-int->bool (OSRIsCompound osr)))

(define (gdal-get-projection dataset)
  (let ((hSRS (osr-from-dataset dataset)))
    (if (osr-is-projected? hSRS)
      (osr-transform (OSRCloneGeogCS hSRS) hSRS)
      identity))) ;; (lambda(l) l))))

(define (gdal-get-projection⁻¹ dataset)
  (let ((hSRS (osr-from-dataset dataset)))
    (if (osr-is-projected? hSRS)
      (osr-transform hSRS (OSRCloneGeogCS hSRS))
      identity))) ;; (lambda(l) l))))

(define (gdal-get-geotransform-matrix dataset)
  (let ((m (make (c-array <c-double> 6))))
    (GDALGetGeoTransform dataset (ptr m))
    (apply array (cons (shape 0 3 0 3)
                       (append (map (cut ref m <>) '(1 2 0))
                               (map (cut ref m <>) '(4 5 3))
                               '(0.0 0.0 1.0))))))

;; todo:
;; - gdal already should provide that, no?
;; - slow
(define (gdal-get-geotransform⁻¹ dataset)
  (let ((A (array-inverse (array-mul (gdal-get-geotransform-matrix dataset)
                                     (array (shape 0 3 0 3)
                                            1.0 0.0 0.5
                                            0.0 1.0 0.5
                                            0.0 0.0 1.0)))))
    (lambda(l)
      (let1 r (array-mul A (array (shape 0 3 0 1) (ref l 0) (ref l 1) 1))
        (list (array-ref r 0 0) (array-ref r 1 0))))))

;; todo:
;; - gdal already should provide that, no?
;; - slow
(define (get-geotransform dataset)
  (let ((A (array-mul (gdal-get-geotransform-matrix dataset)
                      (array (shape 0 3 0 3)
                             1.0 0.0 0.5
                             0.0 1.0 0.5
                             0.0 0.0 1.0))))
    (lambda(l)
      (let1 r (array-mul A (array (shape 0 3 0 1) (ref l 0) (ref l 1) 1))
        (list (array-ref r 0 0) (array-ref r 1 0))))))

(define (gdal-open-band dataset band)
  (let ((hband (GDALGetRasterBand dataset band))
        ;; (block-size-x (make <c-int>))
        ;; (block-size-y (make <c-int>))
        ;; (gotMin (make <c-int>))
        ;; (gotMax (make <c-int>))
        ;; (adfMinMax (make (c-array <c-double> 2)))
        )
    ;; (GDALGetBlockSize hband (ptr block-size-x) (ptr block-size-y))
    ;; (print #`"Block=,(cast <number> block-size-x)x,(cast <number> block-size-y) Type=,(GDALGetDataTypeName (GDALGetRasterDataType hband)), ColorInterp=,(GDALGetColorInterpretationName (GDALGetRasterColorInterpretation hband))")
    ;; (set! (ref adfMinMax 0) (GDALGetRasterMinimum hband (ptr gotMin)))
    ;; (set! (ref adfMinMax 1) (GDALGetRasterMaximum hband (ptr gotMax)))
    ;; (when (not (and (c-int->bool gotMin) (c-int->bool gotMax)))
    ;;   (GDALComputeRasterMinMax hband TRUE adfMinMax))
    ;;        (print #`"Min=,(ref adfMinMax 0), Max=,(ref adfMinMax 1)")
    ;; (when (< 0 (GDALGetOverviewCount hband))
    ;;   (print "Band has ,(GDALGetOverviewCount hband) overviews."))
    ;; (when (not (null-ptr? (GDALGetRasterColorTable hband)))
    ;;   (print #`"Band has a color table with ,(GDALGetColorEntryCount (GDALGetRasterColorTable hband)) entries."))
    hband))

(define (gdal-band-nodata hband)
  (let ((gotNoData (make <c-int>)))
    (GDALGetRasterNoDataValue hband (ptr gotNoData))
    (and (c-int->bool gotNoData)
         (GDALGetRasterNoDataValue hband (ptr gotNoData)))))

(define (gdal-read-band-row band row . args)
  (let-optionals* args ((start 0)
                        (end (GDALGetRasterBandXSize band)))
    (assert (<= start end))
    (let1 count (- end start)
      (cond [(and (> count 0)
                  (>= row 0)
                  (< row (GDALGetRasterBandYSize band)))
             (let ((rstart (max 0 start))
                   (rend   (min end (GDALGetRasterBandXSize band))))
               (let ((lfill (- rstart start))
                     ;; (rfill (- end rend))
                     (rcount (- rend rstart)))
                 (let1 scanline (make-f32vector count +nan.0)
                   ;; todo: just return #f if rcount is zero?
                   (when (and (> rcount 0)
                              (not (zero? (GDALRasterIO band GF_Read rstart row rcount 1 (c-ptr+ (cast (ptr <c-float>) scanline) lfill) rcount 1 GDT_Float32 0 0))))
                     (error "todo"))
                   scanline)))]
            [else
             ;; todo: just return #f ?
             (make-f32vector count +nan.0)]))))

;; note: not used at the moment
;; todo: sometimes first and last column are the same
;; (in this case we would have to ignore the last column)
;; (define (gdal-read-band-row-wrap band row start end)
;;   (assert (<= start end))
;;   (let ((count (- end start))
;;         (xsize (GDALGetRasterBandXSize band)))
;;     (cond [(and (> count 0)
;;                 (>= row 0)
;;                 (< row (GDALGetRasterBandYSize band)))
;;            (let ((rstart (modulo start xsize))
;;                  (rend   (modulo end xsize))
;;                  (scanline (make-f32vector count +nan.0)))
;;              (cond [(< rstart rend)
;;                     (assert (= count (- rend rstart)))
;;                     (when (not (zero? (GDALRasterIO band GF_Read rstart row count 1 scanline count 1 GDT_Float32 0 0)))
;;                       (error "todo")
;;                       scanline)]
;;                    [else
;;                     ;; wrap-around => join 2 seperate parts
;;                     (let1 c1 (- xsize rstart)
;;                       (assert (< c1 count))
;;                       (when (not (zero? (GDALRasterIO band GF_Read rstart row c1 1 scanline c1 1 GDT_Float32 0 0)))
;;                         (error "todo"))
;;                       (let1 c2 rend
;;                         (assert (< c2 count))
;;                         (when (not (zero? (GDALRasterIO band GF_Read rstart row c2 1 (c-ptr+ (cast (ptr <c-float>) scanline) c1) c2 1 GDT_Float32 0 0)))
;;                           (error "todo"))))
;;                     scanline]))]
;;           [else
;;            ;; todo: wrap around in y direction?!
;;            (make-f32vector count +nan.0)])))

;; taken from grass (interp.c)
;;     return (u * (u * (u * (c3 + -3 * c2 + 3 * c1 - c0) +
;;	      (-c3 + 4 * c2 - 5 * c1 + 2 * c0) + (c2 - c0)) + 2 * c1) / 2;
(define (interp-cubic u c0 c1 c2 c3)
  (/ (+ (* u (+ (* u (+ (* u (+ c3 (* -3 c2) (* 3 c1) (- c0)))
                        (- c3)
                        (* 4 c2)
                        (* -5 c1)
                        (* 2 c0)))
                c2
                (- c0)))
        (* 2 c1))
     2))

(define (interp-linear u c0 c1)
  (+ (* u (- c1 c0)) c0))

(define (range s e) (iota (- e s) s))

(define (bi-interp u v f rows)
  (apply f
         (cons v
               (map (lambda(x)
                      (apply f
                             (cons u
                                   (f32vector->list (ref rows x)))))
                    (iota (size-of rows))))))

(define (interp-bicubic u v rows)
  (assert (= (size-of rows) 4))
  (bi-interp u v interp-cubic rows))

(define (interp-bilinear u v rows)
  (assert (= (size-of rows) 2))
  (bi-interp u v interp-linear rows))

(define (raster-pos->4x4-box raster-pos)
  (let1 tl (map (lambda(x) (- (floor->exact x) 1)) raster-pos)
    (list tl (map (cut + <> 4) tl))))

(define (raster-pos->2x2-box raster-pos)
  (assert (list? raster-pos))
  (let1 tl (map floor->exact raster-pos)
    (list tl (map (cut + <> 2) tl))))

(define (raster-pos->1x1-box raster-pos)
  (assert (list? raster-pos))
  (let1 tl (map round->exact raster-pos)
    (list tl (map (cut + <> 1) tl))))

(define (raster-pos->uv raster-pos)
  (map (lambda(x) (- x (floor x))) raster-pos))

(define gdal-init
  (let1 called #f
    (lambda()
      (cond [(not called)
             (set! called #t)
             (GDALAllRegister)
             #t]
            [else
             #f]))))

(define (f32vector-replace vec from to)
  (map-to <f32vector> (lambda(x) (if (= x from) to
                                     x))
          vec))

(define (p-fmod x y) (let1 r (fmod x y) (if (< r 0) (+ r y) r)))

;; wrap longitude to [-180,180[
(define (wrap-long x)
  (let1 r (p-fmod x 360.0)
    (if (>= r 180.0)
      (- r 360.0)
      r)))

;; todo: improve
(define (mod4 x m minx maxx)
  (cond [(and (< x minx)
              (or (>= (- maxx minx) m)
                  (<= (+ x m) maxx)))
         (mod4 (+ x m) m minx maxx)]
        [(and (> x maxx)
              (or (>= (- maxx minx) m)
                  (>= (- x m) minx)))
         (mod4 (- x m) m minx maxx)]
        [else
         x]))
  
(define wrap-long-to (cut mod4 <> 360 <> <>))

;; todo: improve / or maybe just clip?!
(define (wrap-lat x y . l)
  (cond [(< y -90)
         (apply wrap-lat (append (list (+ x 180) (- -180 y))
                                 l))]
        [(> y 90)
         (apply wrap-lat (append (list (+ x 180) (- 180 y))
                                 l))]
        [else
         (append (list x y) l)]))

(define (gdal-raster-size dataset)
  (map x->number (list (GDALGetRasterXSize dataset) (GDALGetRasterYSize dataset))))

(define (gdal-geographic-bbox dataset)
  (let ((osr (osr-from-dataset dataset))
        (rsize (gdal-raster-size dataset)))
    (assert (osr-is-geographic? osr))
    (let* ((l1 (map (get-geotransform dataset)
                    (list '(-1/2 -1/2)
                          (map (cut - <> 1/2) rsize))))
           (l2 (append
                (receive lx (apply min&max (map car l1))
                  lx)
                (receive ly (apply min&max (map cadr l1))
                  ly))))
      (list (permute l2 '(0 2))
            (permute l2 '(1 3))))))
      
(define (nan-to-#f n)
  (if (nan? n)
    #f
    n))

;; return function to get z value at position x y
;; (using coordinate system described by projection)
;; note: empty projection => input cs is _geographic cs_ of dataset
;; todo: maybe disallow empty value? or special symbols? 'geographic 'projected ?!
(define (dem->xy-project->z projection name . args)
  (let-keywords args ((next (lambda _ +nan.0))
                      (interpolation 'bi-cubic)
                      (band 1))
    (let* ((dataset (gdal-open-dataset name))
           (band (gdal-open-band dataset band))
           (width (GDALGetRasterBandXSize band))
           (height (GDALGetRasterBandYSize band))
           (osr (osr-from-dataset dataset))
           (wrap (if (osr-is-geographic? osr)
                   (let1 geobox (gdal-geographic-bbox dataset)
                     (lambda(xy)
                       (let1 xy (apply wrap-lat xy)
                         (list (wrap-long-to (car xy)
                                             (ref* geobox 0 0)
                                             (ref* geobox 1 0))
                               (cadr xy)))))
                   (lambda(xy)
                     (let1 xy (apply wrap-lat xy)
                       (list (fmod (car xy) 360)
                             (cadr xy))))))
           (xy->z (lambda(fi get-box)
                    (let ((rasterpos (apply compose
                                            (reverse ;; just for readability
                                             (filter (lambda(f) (not (eq? f identity)))
                                                        (list
                                                         (if (not (string-null? projection))
                                                           (osr-transform (osr-from-user-input projection)
                                                                          (OSRCloneGeogCS osr))
                                                           identity)
                                                         wrap
                                                         (gdal-get-projection dataset)
                                                         (gdal-get-geotransform⁻¹ dataset))))))
                          ;; todo: only what I want if projection is a geographic cs?
                          (rasterpos⁻¹ (apply compose
                                              (reverse
                                               (filter (lambda(f) (not (eq? f identity)))
                                                       (list
                                                        (get-geotransform dataset)
                                                        (gdal-get-projection⁻¹ dataset)
                                                        (if (not (string-null? projection))
                                                          (osr-transform (OSRCloneGeogCS osr)
                                                                         (osr-from-user-input projection))
                                                          identity)
                                                        (cut subseq <> 0 2))))))
                          (read-row (let1 r (cut gdal-read-band-row band <...>)
                                      (if (gdal-band-nodata band)
                                        (compose (cut f32vector-replace <> (gdal-band-nodata band) +nan.0)
                                                 r)
                                        r))))
                      (let ((read-box
                             ;; todo: if dataset is geographic and grid alignment is fine we could
                             ;; support raster wrap-around
                             ;; s.a. gdal-read-band-row-wrap
                             (lambda(box)
                               (map (lambda(y)
                                      (read-row y (caar box) (caadr box)))
                                    (range (cadar box) (cadadr box))))))
                        (lambda(x y)
                          (guard (e [(transform-error? e)
                                     ;; todo: maybe not what i want!
                                     ;; #?=(list e (transform-error-pos e) x y)
                                     (next x y)])
                                 (let* ((rp (rasterpos (list x y)))
                                        (box (get-box rp)))
                                   ;; bbox test
                                   (if (or (<= (caadr box) 0)  (>= (caar box) width)
                                           (<= (cadadr box) 0) (>= (cadar box) height))
                                     (next x y)
                                     (let ((uv (raster-pos->uv rp))
                                           (rows (read-box box)))
                                       (cond [(every (lambda(r)
                                                       (every nan? (f32vector->list r)))
                                                     rows)
                                              ;; all nan
                                              #?="all nan!"
                                              (next x y)]
                                             [else
                                              ;; try to replace nan
                                              (call/cc
                                               (lambda(break)
                                                 (for-each-with-index
                                                  (lambda(ry r)
                                                    (for-each-with-index
                                                     (lambda(rx v)
                                                       (when (nan? v)
                                                         (receive (cx cy)
                                                             (apply values
                                                                    (rasterpos⁻¹ (list (+ (caar box) rx)
                                                                                       (+ (cadar box) ry))))
                                                           (if-let1 nv
                                                               (or (and (osr-is-geographic? osr)
                                                                        (let* ((xy (lambda(x y)
                                                                                     ;; todo: only allow close match / or interpolate?!
                                                                                     ;; (but then i could use the offset stack ...)
                                                                                     (let ((x (round->exact x))
                                                                                           (y (round->exact y)))
                                                                                       (ref (read-row y x (+ x 1)) 0))))
                                                                               (p (lambda l
                                                                                    (nan-to-#f (apply xy (rasterpos l))))))
                                                                          (or (p (+ cx 360.0) cy)
                                                                              (p (- cx 360.0) cy)
                                                                              (and (or (> cy 90.0) (< cy -90.0))
                                                                                   (p cx cy)))))
                                                                   (nan-to-#f (next cx cy)))
                                                             (set! (ref r rx) nv)
                                                             (break (next x y))))))
                                                     r))
                                                  rows)
                                                 (assert (not (any (cut find nan? <>) rows)))
                                                 (fi (car uv) (cadr uv) rows)))])))))))))))
      (case interpolation
        ((bi-cubic)  (xy->z interp-bicubic raster-pos->4x4-box))
        ((bi-linear) (xy->z interp-bilinear raster-pos->2x2-box))
        ((nearest)   (xy->z (lambda(u v rows) (ref* rows 0 0)) raster-pos->1x1-box))
        (else (error "Unknown interpolation:" interpolation))))))

;; return function to get z value at position x y (using coordinate system of the dataset)
(define (dem->xy->z name . args)
  (apply dem->xy-project->z (append (list "" name) args)))

(define (keyword-exists? key kv-list)
  (or (get-keyword key kv-list #f)
      (not (equal? 1 (get-keyword key kv-list 1)))))
  
(define (dem-stack->xy->z projection dem-stack)
  (let1 l (reverse dem-stack)
    (fold (lambda(n o)
            ;; note: maybe we should use delete-keyword on n instead
            ;; of assuming let-keywords takes the last value
            ;; even better: throw an error if there is a next keyword!
            ;; there is no such thing as keyword-exists?
            (when (keyword-exists? :next (cdr n))
              (error ":next only allowed in last element"))
            (apply dem->xy-project->z (cons projection (append n (list :next o)))))
          (apply dem->xy-project->z (cons projection (car l)))
          (cdr l))))
