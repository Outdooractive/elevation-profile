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
  (use runtime-compile)
  (use binary.pack)
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

(cond-expand
 (no-runtime-compile
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

  ;; todo:
  ;; - gdal already should provide that, no?
  ;; - slow
  (define (gdal-get-geotransform⁻¹ dataset)
    (let1 A (array-inverse (array-mul (gdal-get-geotransform-matrix dataset)
                                      (array (shape 0 3 0 3)
                                             1.0 0.0 0.5
                                             0.0 1.0 0.5
                                             0.0 0.0 1.0)))
      (lambda(l)
        (let1 r (array-mul A (array (shape 0 3 0 1) (ref l 0) (ref l 1) 1))
          (list (array-ref r 0 0) (array-ref r 1 0))))))

  (define (f32vector-replace! vec from to)
    (let1 s (f32vector-length vec)
      (dotimes (i s)
        (when (= (f32vector-ref vec i) from)
          (f32vector-set! vec i to))))
    vec)

  (define (get-gdal-read-band-row! band nodata)
    (let ((xsize (GDALGetRasterBandXSize band))
          (ysize (GDALGetRasterBandYSize band)))
      (lambda(scanline row . args)
        (let-optionals* args ((start 0)
                              (end xsize))
          (assert (<= start end))
          (let1 count (- end start)
            (assert (>= (size-of scanline) count))
            (f32vector-fill! scanline +nan.0)
            (cond [(and (> count 0)
                        (>= row 0)
                        (< row ysize))
                   (let ((rstart (max 0 start))
                         (rend   (min end xsize)))
                     (let ((lfill (- rstart start))
                           ;; (rfill (- end rend))
                           (rcount (- rend rstart)))
                       (when (and (> rcount 0)
                                  (not (zero? (GDALRasterIO band GF_Read rstart row rcount 1
                                                            (c-ptr+ (cast (ptr <c-float>) scanline) lfill)
                                                            rcount 1 GDT_Float32 0 0))))
                         (error "todo"))
                       (assert (or (boolean? nodata) (number? nodata)))
                       ;; replace nodata with nan
                       (when nodata
                         (f32vector-replace! scanline nodata +nan.0))
                       ;; count nan
                       (let ((s (f32vector-length scanline))
                             (r 0))
                         (dotimes (i s)
                           (when (nan? (f32vector-ref scanline i))
                             (inc! r)))
                         r)))]
                  [else
                   (f32vector-length scanline)]))))))

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

  (define (get-bbox-geo-wrap geobox)
    (lambda(xy)
      (let1 xy (apply wrap-lat xy)
        (list (wrap-long-to (car xy)
                            (ref* geobox 0 0)
                            (ref* geobox 1 0))
              (cadr xy)))))

  (define (geo-wrap xy)
    (let1 xy (apply wrap-lat xy)
      ;; note: (fmod (car xy) 360) can't be expressed using wrap-long-to :(
      (list (fmod (car xy) 360)
            (cadr xy))))

  (define (raster-pos->4x4-box raster-pos)
    (let1 tl (map (lambda(x) (- (floor->exact x) 1)) raster-pos)
      (list tl (map (cut + <> 4) tl))))

  (define (raster-pos->2x2-box raster-pos)
    ;;(assert (list? raster-pos))
    (let1 tl (map floor->exact raster-pos)
      (list tl (map (cut + <> 2) tl))))

  (define (raster-pos->1x1-box raster-pos)
    ;;(assert (list? raster-pos))
    (let1 tl (map round->exact raster-pos)
      (list tl (map (cut + <> 1) tl))))

  (define (get-rasterpos projection dataset)
    (let1 osr (osr-from-dataset dataset)
      (let1 f (apply compose
                     (reverse ;; just for readability
                      (filter (lambda(f) (not (eq? f identity)))
                              (list
                               (if (not (string-null? projection))
                                 (osr-transform (osr-from-user-input projection)
                                                (OSRCloneGeogCS osr))
                                 identity)
                               (if (osr-is-geographic? osr)
                                 ;; todo: at the moment we can only get the
                                 ;; geographic bbox if the dataset osr is
                                 ;; geographic
                                 (get-bbox-geo-wrap (gdal-geographic-bbox dataset))
                                 ;; note: input always geographic!
                                 geo-wrap)
                               (gdal-get-projection dataset)
                               (gdal-get-geotransform⁻¹ dataset)))))
        (lambda(x y)
          (f (list x y))))))

  (define (get-rasterpos&bbox! projection dataset get-box width height)
    (let1 rasterpos (get-rasterpos  projection dataset)
      (lambda(x y rp box)
        (guard (e [(transform-error? e)
                   #f])
               (let* ((rp2 (rasterpos x y))
                      (box2 (get-box rp2)))
                 (cond [(or (<= (caadr box2) 0)  (>= (caar box2) width)
                            (<= (cadadr box2) 0) (>= (cadar box2) height))
                        #f]
                       [else
                        (set! (ref rp 0) (car rp2))
                        (set! (ref rp 1) (cadr rp2))
                        ;; #?=rp
                        ;; #?=box2
                        (set! (ref box 0) (car (car box2)))
                        (set! (ref box 1) (cadr (car box2)))
                        (set! (ref box 2) (car (cadr box2)))
                        (set! (ref box 3) (cadr (cadr box2)))
                        #t]))))))

  )
 (else
  (compile-and-load
   `((inline-stub
      (declcode
       (.include "gauche/uvector.h")
       (.include "gdal/gdal.h")
       (.include "gdal/ogr_srs_api.h")
       "static ScmClass *osrn_transform_class = NULL;"
       "/* stolen from cwcompile output */
      static void cw_unbox(void *dest, ScmObj obj, size_t size)
      {
        static ScmObj bufferof_proc = NULL;
        ScmObj buf;
        if (!bufferof_proc) {
          bufferof_proc = SCM_SYMBOL_VALUE(\"c-wrapper.c-ffi\", \"buffer-of\");
        }
        buf = Scm_ApplyRec(bufferof_proc, SCM_LIST1(obj));
        memcpy(dest, SCM_UVECTOR_ELEMENTS(buf), size);
      }"
       )
      
      (define-cproc make-osrn-transform (fromp top)
        (let* ((from::OGRSpatialReferenceH NULL)
               (to::OGRSpatialReferenceH NULL))
          (cw_unbox (& from) fromp (sizeof OGRSpatialReferenceH))
          (cw_unbox (& to) top (sizeof OGRSpatialReferenceH))
          (when (not from)
            (Scm_Error "failed to set from"))
          (when (not to)
            (Scm_Error "failed to set to"))
          (return (Scm_MakeForeignPointer osrn_transform_class (OCTNewCoordinateTransformation from to)))))

      (define-cproc osrn-apply-transform (it x::<double> y::<double>)
        (unless (SCM_XTYPEP it osrn_transform_class) (SCM_TYPE_ERROR it "<osrn:transform>"))
        (let* ((t::OGRCoordinateTransformationH (SCM_FOREIGN_POINTER_REF OGRCoordinateTransformationH it))
               (xr::double x)
               (yr::double y)
               (zr::double 0))
          (when (not (OCTTransform t 1 (& xr) (& yr) (& zr)))
            (Scm_Error "transform failed")) ;; todo: use Scm_Raise ?
          (result (SCM_LIST3 (Scm_MakeFlonum xr)
                             (Scm_MakeFlonum yr)
                             (Scm_MakeFlonum zr)))))

      (define-cfn osrn-transform-cleanup (h) ::void :static
        (OCTDestroyCoordinateTransformation (SCM_FOREIGN_POINTER_REF OGRCoordinateTransformationH h)))
      
      (define-cfn osrn-transform-print (h p::ScmPort* c::ScmWriteContext*) ::void :static
        (Scm_Printf p "#<osrn:transform @%p->%p>" h (SCM_FOREIGN_POINTER_REF OGRCoordinateTransformationH h)))

      (define-cproc c-gdal-read-band-row!
        (bandp nodata xsize::<int> ysize::<int> scanline::<f32vector> row::<int> start::<int> end::<int>)
        (let* ((band::GDALRasterBandH NULL))
          (cw_unbox (& band) bandp (sizeof GDALRasterBandH))
          (unless (<= start end) (Scm_Error "(<= start end)")) ;; todo: c-level assert?!
          (let* ((count::int (- end start)))
            (unless (>= (SCM_UVECTOR_SIZE scanline) count) (Scm_Error "(>= (SCM_UVECTOR_SIZE scanline) count)"))
            (Scm_F32VectorFill scanline NAN 0 (SCM_UVECTOR_SIZE scanline))
            (cond [(and (> count 0)
                        (>= row 0)
                        (< row ysize))
                   (let* ((rstart::int (?: (< start 0) 0 start))
                          (rend::int   (?: (< end xsize) end xsize))
                          (lfill::int (- rstart start))
                          ;; (rfill (- end rend))
                          (rcount::int (- rend rstart)))
                     (when (and (> rcount 0)
                                (not (== (GDALRasterIO band GF_Read rstart row rcount 1
                                                       (+ (SCM_F32VECTOR_ELEMENTS scanline) lfill)
                                                       rcount 1 GDT_Float32 0 0)
                                         0)))
                       (Scm_Error "todo"))
                     (let* ((r::int 0)
                            (i::int 0))
                       ;; replace nodata with nan
                       (unless (or (SCM_BOOLP nodata) (SCM_FLONUMP nodata))
                         (Scm_Error "(or (SCM_BOOLP nodata) (SCM_FLONUMP nodata))"))
                       (when (and (not (SCM_BOOLP nodata))
                                  (SCM_FLONUMP nodata))
                         (for [(set! i 0) (< i (SCM_UVECTOR_SIZE scanline)) (pre++ i)]
                              (when (== (aref (SCM_F32VECTOR_ELEMENTS scanline) i) (SCM_FLONUM_VALUE nodata))
                                (set! (aref (SCM_F32VECTOR_ELEMENTS scanline) i) NAN))))
                       ;; count nan
                       (for [(set! i 0) (< i (SCM_UVECTOR_SIZE scanline)) (pre++ i)]
                            (when (isnan (aref (SCM_F32VECTOR_ELEMENTS scanline) i))
                              (pre++ r)))
                       (result (SCM_MAKE_INT r))))]
                  [else
                   (result (SCM_MAKE_INT (SCM_UVECTOR_SIZE scanline)))]))))

      (initcode (= osrn_transform_class (Scm_MakeForeignPointerClass
                                         (Scm_CurrentModule)
                                         "<osrn:transform>" osrn-transform-print osrn-transform-cleanup
                                         SCM_FOREIGN_POINTER_KEEP_IDENTITY)))
      ))
   '(make-osrn-transform osrn-apply-transform c-gdal-read-band-row!)
   :libs (process-output->string "gdal-config --libs"))

  (define (osr-transform from to)
    (if (osr-is-same? from to)
      identity
      (let1 fp (make-osrn-transform from to)
        (lambda(l)
          (guard (e [else
                     ;;#?=e
                     (error <transform-error> :pos l)])
                 (osrn-apply-transform fp (car l) (cadr l)))))))
  (with-module gauche.array
    (define (symbolic-array-mul a b) ; NxM * MxP => NxP
      (let ([a-start (start-vector-of a)]
            [a-end (end-vector-of a)]
            [b-start (start-vector-of b)]
            [b-end (end-vector-of b)])
        (unless (= 2 (s32vector-length a-start) (s32vector-length b-start))
          (error "array-mul matrices must be of rank 2"))
        (let* ([a-start-row (s32vector-ref a-start 0)]
               [a-end-row (s32vector-ref a-end 0)]
               [a-start-col (s32vector-ref a-start 1)]
               [a-end-col (s32vector-ref a-end 1)]
               [b-start-col (s32vector-ref b-start 1)]
               [b-end-col (s32vector-ref b-end 1)]
               [n (- a-end-row a-start-row)]
               [m (- a-end-col a-start-col)]
               [p (- b-end-col b-start-col)]
               [a-col-b-row-off (- a-start-col (s32vector-ref b-start 0))]
               [res (make-minimal-backend-array (list a b) (shape 0 n 0 p))])
          (unless (= m (- (s32vector-ref b-end 0) (s32vector-ref b-start 0)))
            (errorf "dimension mismatch: can't mul shapes ~S and ~S"
                    (array-shape a) (array-shape b)))
          (do ([i a-start-row (+ i 1)])       ; for-each row of a
              [(= i a-end-row) res]
            (do ([k b-start-col (+ k 1)])     ; for-each col of b
                [(= k b-end-col)]
              (let1 tmp (list '+)
                (do ([j a-start-col (+ j 1)]) ; for-each col of a & row of b
                    [(= j a-end-col)]
                  (append! tmp (list (list '* (array-ref a i j) (array-ref b (- j a-col-b-row-off) k)))))
                (array-set! res (- i a-start-row) (- k b-start-col) tmp)))))))
    (export symbolic-array-mul)
    )

  ;; todo: use macro?!
  (define (compile-cise-function args body)
    (let1 mod (compile-and-load
               `((inline-stub
                  (declcode
                   (.include "gdal/gdal.h")
                   (.include "gdal/ogr_srs_api.h"))
                  (define-cproc foo ,args . ,body)))
               `()
               :libs (process-output->string "gdal-config --libs"))
      (global-variable-ref mod 'foo)))

  (define (gdal-get-geotransform-cise⁻¹ dataset)
    (let* ((A (array-inverse (array-mul (gdal-get-geotransform-matrix dataset)
                                        (array (shape 0 3 0 3)
                                               1.0 0.0 0.5
                                               0.0 1.0 0.5
                                               0.0 0.0 1.0))))
           (sr (symbolic-array-mul A (array (shape 0 3 0 1) 'x 'y 1))))
      `((set! x ,(array-ref sr 0 0))
        (set! y ,(array-ref sr 1 0)))))

  ;; todo:
  ;; - gdal already should provide that, no?
  (define (gdal-get-geotransform⁻¹ dataset)
    (let1 nf (compile-cise-function '(x::<double> y::<double>)
                                    (append (gdal-get-geotransform-cise⁻¹ dataset)
                                            `((return (SCM_LIST2 (Scm_MakeFlonum x) (Scm_MakeFlonum y))))))
      (lambda(l)
        (nf (ref l 0) (ref l 1)))))

  (define (get-gdal-read-band-row! band nodata)
    (let ((xsize (GDALGetRasterBandXSize band))
          (ysize (GDALGetRasterBandYSize band)))
      (lambda(scanline row . args)
        (let-optionals* args ((start 0)
                              (end xsize))
          (c-gdal-read-band-row! band nodata xsize ysize scanline row start end)))))

  (compile-and-load
   `((inline-stub
      (define-cproc interp-cubic (u::<double> c0::<double> c1::<double> c2::<double> c3::<double>)
        ::<number> ;; :fast-flonum :constant
        (result (Scm_MakeFlonum (/ (+ (* u (+ (* u (+ (* u (+ c3 (* -3 c2) (* 3 c1) (- c0)))
                                                      (- c3)
                                                      (* 4 c2)
                                                      (* -5 c1)
                                                      (* 2 c0)))
                                              c2
                                              (- c0)))
                                      (* 2 c1))
                                   2))))))
   '(interp-cubic))

  (define (bbox-geo-wrap-cise-2 minx maxx)
    `((while 1
        (cond [(< y -90)
               (+= x 180)
               (set! y (- -180 y))]
              [(> y 90)
               (+= x 180)
               (set! y (- 180 y))]
              [else
               (break)]))
      ;; todo: improve
      (while 1
        (cond [(and (< x ,minx)
                    (or (>= ,(- maxx minx) 360)
                        (<= (+ x 360) ,maxx)))
               (+= x 360)]
              [(and (> x ,maxx)
                    (or (>= ,(- maxx minx) 360)
                        (>= (- x 360) ,minx)))
               (-= x 360)]
              [else
               (break)]))))

  (define (get-bbox-geo-wrap-cise geobox)
    (bbox-geo-wrap-cise-2 (ref* geobox 0 0) (ref* geobox 1 0)))

  (define (get-bbox-geo-wrap geobox)
    (let1 f (compile-cise-function '(x::<double> y::<double>)
                                   (append
                                    (get-bbox-geo-wrap-cise geobox)
                                    '((result (SCM_LIST2 (Scm_MakeFlonum x) (Scm_MakeFlonum y))))))
      (lambda(xy)
        (f (car xy) (cadr xy)))))

  (define (geo-wrap-cise)
    '((while 1
        (cond [(< y -90)
               (+= x 180)
               (set! y (- -180 y))]
              [(> y 90)
               (+= x 180)
               (set! y (- 180 y))]
              [else
               (break)]))
      (set! x (fmod x 360))))

  (define (geo-wrap)
    (let1 f (compile-cise-function '(x::<double> y::<double>)
                                   (append
                                    (geo-wrap-cise)
                                    '((result (SCM_LIST2 (Scm_MakeFlonum x) (Scm_MakeFlonum y))))))
      (lambda(xy)
        (f (car xy) (cadr xy)))))

  ;; todo: really ugly hack
  (define (c-wrapper-ptr-value p)
    (car (unpack  ;; no pointer?!
          (case (c-sizeof (ptr <c-void>))
            [(8) "Q"]
            [(4) "L"]
            [else
             (error "pointer size not supported")])
          :from-string (u8vector->string (slot-ref (cast (ptr <c-void>) p) 'buffer)))))

  (define (c-wrapper-ptr->cise-ptr p)
    (gc)
    (gc)
    `(cast (void *) ,(string->symbol (format "0x~x" (c-wrapper-ptr-value p)))))

  (define (osr-transform-cise fromp top . args)
    (let-optionals* args ((transform-error '(Scm_Error "transform failed")))
      (cond [(osr-is-same? fromp top)
             identity]
            [else
             (assert (not (null-ptr? fromp)))
             (assert (not (null-ptr? top)))
             `((let* ((from::(static OGRSpatialReferenceH) NULL)
                      (to::(static OGRSpatialReferenceH) NULL)
                      (t::(static OGRCoordinateTransformationH) NULL))
                 (when (not t)
                   (set! from ,(c-wrapper-ptr->cise-ptr fromp))
                   (set! to ,(c-wrapper-ptr->cise-ptr top))
                   (set! t (OCTNewCoordinateTransformation from to))
                   (when (not t) (Scm_Error "failed to set up t")))
                 (let* ((z::double 0))
                   (when (not (OCTTransform t 1 (& x) (& y) (& z)))
                     ,transform-error) ;; todo: use Scm_Raise ?
                   )))])))

  (define (gdal-get-projection-cise dataset . args)
    (let-optionals* args ((transform-error '(Scm_Error "transform failed")))
      (let ((hSRS (osr-from-dataset dataset)))
        (if (osr-is-projected? hSRS)
          (osr-transform-cise (OSRCloneGeogCS hSRS) hSRS transform-error)
          identity)))) ;; (lambda(l) l))))

  (define (get-rasterpos projection dataset)
    (let* ((osr (osr-from-dataset dataset))
           (f (compile-cise-function
               '(x::<double> y::<double>)
               (append (apply append (filter (lambda(f) (not (eq? f identity)))
                                             (list
                                              (if (not (string-null? projection))
                                                (osr-transform-cise (osr-from-user-input projection)
                                                                    (OSRCloneGeogCS osr))
                                                identity)
                                              (if (osr-is-geographic? osr)
                                                ;; todo: at the moment we can only get the
                                                ;; geographic bbox if the dataset osr is
                                                ;; geographic
                                                (get-bbox-geo-wrap-cise (gdal-geographic-bbox dataset))
                                                ;; note: input always geographic!
                                                (geo-wrap-cise))
                                              (gdal-get-projection-cise dataset)
                                              (gdal-get-geotransform-cise⁻¹ dataset))))
                       '((result (SCM_LIST2 (Scm_MakeFlonum x) (Scm_MakeFlonum y))))))))
      (lambda(x y)
        (guard (e
                [else
                 ;; todo: check it really is a transform error?!
                 (error <transform-error> :pos (list x y))])
               (f x y)))))

  (define raster-pos->4x4-box
    `((set! tl_x (- (cast int (floor x)) 1))
      (set! tl_y (- (cast int (floor y)) 1))
      (set! br_x (+ tl_x 4))
      (set! br_y (+ tl_y 4))))

  (define raster-pos->2x2-box
    `((set! tl_x (cast int (floor x)))
      (set! tl_y (cast int (floor y)))
      (set! br_x (+ tl_x 2))
      (set! br_y (+ tl_y 2))))

  (define raster-pos->1x1-box
    ;; rint to match round->exact
    `((set! tl_x (cast int (rint x)))
      (set! tl_y (cast int (rint y)))
      (set! br_x (+ tl_x 1))
      (set! br_y (+ tl_y 1))))

  (define (get-rasterpos&bbox! projection dataset get-box width height)
    (let* ((osr (osr-from-dataset dataset))
           (f (compile-cise-function
               '(x::<double> y::<double> rp::<f64vector> box::<s64vector>)
               `((let* ((tl_x::int64_t)
                        (tl_y::int64_t)
                        (br_x::int64_t)
                        (br_y::int64_t))
                   . ,(append
                       ;; `((Scm_Printf SCM_CURERR "huhu\n"))
                       (apply append (filter (lambda(f) (not (eq? f identity)))
                                             (list
                                              (if (not (string-null? projection))
                                                (osr-transform-cise (osr-from-user-input projection)
                                                                    (OSRCloneGeogCS osr)
                                                                    '(return SCM_FALSE))
                                                identity)
                                              (if (osr-is-geographic? osr)
                                                ;; todo: at the moment we can only get the
                                                ;; geographic bbox if the dataset osr is
                                                ;; geographic
                                                (get-bbox-geo-wrap-cise (gdal-geographic-bbox dataset))
                                                ;; note: input always geographic!
                                                (geo-wrap-cise))
                                              (gdal-get-projection-cise dataset '(return SCM_FALSE))
                                              (gdal-get-geotransform-cise⁻¹ dataset))))
                       get-box
                       `((cond [(or (<= br_x 0) (>= tl_x ,width)
                                    (<= br_y 0) (>= tl_y ,height))
                                (result SCM_FALSE)]
                               [else
                                (when (or (< (SCM_UVECTOR_SIZE rp) 2)
                                          (< (SCM_UVECTOR_SIZE box) 4))
                                  (Scm_Printf SCM_CURERR "abort\n")
                                  (abort))
                                (set! (aref (SCM_F64VECTOR_ELEMENTS rp) 0) x)
                                (set! (aref (SCM_F64VECTOR_ELEMENTS rp) 1) y)
                                (set! (aref (SCM_S64VECTOR_ELEMENTS box) 0) tl_x)
                                (set! (aref (SCM_S64VECTOR_ELEMENTS box) 1) tl_y)
                                (set! (aref (SCM_S64VECTOR_ELEMENTS box) 2) br_x)
                                (set! (aref (SCM_S64VECTOR_ELEMENTS box) 3) br_y)
                                (result SCM_TRUE)]))))))))
      (lambda(x y rp box)
        (guard (e
                [else
                 ;; #?=e
                 (error <transform-error> :pos (list x y))])
               (f x y rp box)))))

  ))

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
;; - slow, but typically not called very often 
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

(define (interp-linear u c0 c1)
  (+ (* u (- c1 c0)) c0))

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

;; (benchmark 10000 (lambda _ (interp-bicubic 0.2 0.2 '(#f32(0 1 0 0) #f32(0 2 0 0)#f32(0 0 0 0)#f32(0 0 0 0)))))

(define (interp-bilinear u v rows)
  (assert (= (size-of rows) 2))
  (bi-interp u v interp-linear rows))

(define (raster-pos->uv x y)
  (values (- x (floor x))
          (- y (floor y))))

(define gdal-init
  (let1 called #f
    (lambda()
      (cond [(not called)
             (set! called #t)
             (GDALAllRegister)
             #t]
            [else
             #f]))))

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
           (band (gdal-open-band dataset band)))
      (let ((width (GDALGetRasterBandXSize band))
            (height (GDALGetRasterBandYSize band))
            (osr (osr-from-dataset dataset)))
        (let1 xy->z (lambda(fi get-box box-width box-height)
                      (let ((rasterpos (get-rasterpos projection dataset)) ;; todo: get rid of rasterpos
                            (rp (make-f64vector 2))
                            (box (make-s64vector 4))
                            (rasterpos&bbox! (get-rasterpos&bbox! projection dataset get-box width height))
                            ;; todo:
                            ;; - only what I want if projection is a geographic cs?
                            ;; - slow, but typically not called very often
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
                            (read-row! (get-gdal-read-band-row! band (gdal-band-nodata band)))
                            (rows (map (lambda(y) (make-f32vector box-width)) (iota box-height)))
                            (geographic-dataset? (osr-is-geographic? osr)))
                        (let ((read-row (lambda(y xs xe)
                                          (let1 row (make-f32vector (- xe xs))
                                            (read-row! row y xs xe)
                                            row)))
                              (read-box! (lambda()
                                           (let ((start (s64vector-ref box 0))
                                                 (end   (s64vector-ref box 2))
                                                 (y (s64vector-ref box 1))
                                                 (r 0))
                                             (dotimes (idx box-height)
                                               (inc! r (read-row! (ref rows idx) (+ y idx) start end)))
                                             r)))
                              )
                          (let* ((read-pixel (lambda(x y)
                                               (let1 x (round->exact x)
                                                 (f32vector-ref (read-row (round->exact y) x (+ x 1)) 0))))
                                 (read-geo-pixel (lambda(x y)
                                                   (guard (e [(transform-error? e)
                                                              #f])
                                                          (nan-to-#f (apply read-pixel (rasterpos x y)))))))
                            (lambda(x y)
                              (if (not (rasterpos&bbox! x y rp box))
                                (next x y)
                                (let ((nans (read-box!)))
                                  (cond [(= nans (* box-width box-height))
                                         (next x y)]
                                        [(> nans 0)
                                         ;; try to replace nan
                                         ;; todo: maybe split into geographic and non-geographic case?
                                         (call/cc
                                          (lambda(break)
                                            (for-each-with-index
                                             (lambda(ry r)
                                               (for-each-with-index
                                                (lambda(rx v)
                                                  (when (nan? v)
                                                    (receive (cx cy)
                                                        (apply values (rasterpos⁻¹ (list (+ (s64vector-ref box 0) rx)
                                                                                         (+ (s64vector-ref box 1) ry))))
                                                      (if-let1 nv
                                                          (or (and geographic-dataset?
                                                                   (or (read-geo-pixel (+ cx 360.0) cy)
                                                                       (read-geo-pixel (- cx 360.0) cy)
                                                                       (and (or (> cy 90.0) (< cy -90.0))
                                                                            (read-geo-pixel cx cy))))
                                                              (nan-to-#f (next cx cy)))
                                                        (set! (ref r rx) nv)
                                                        ;; failed to replace nan
                                                        (break (next x y))))))
                                                r))
                                             rows)
                                            ;; (assert (not (any (cut find nan? <>) rows)))
                                            (receive (u v) (raster-pos->uv (f64vector-ref rp 0) (f64vector-ref rp 1))
                                              (fi u v rows))))]
                                        [else
                                         ;; (assert (zero? nans))
                                         (receive (u v) (raster-pos->uv (f64vector-ref rp 0) (f64vector-ref rp 1))
                                           (fi u v rows))]))))))))
          (case interpolation
            ((bi-cubic)  (xy->z interp-bicubic raster-pos->4x4-box 4 4))
            ((bi-linear) (xy->z interp-bilinear raster-pos->2x2-box 2 2))
            ((nearest)   (xy->z (lambda(u v rows) (ref* rows 0 0)) raster-pos->1x1-box 1 1))
            (else (error "Unknown interpolation:" interpolation))))))))

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
