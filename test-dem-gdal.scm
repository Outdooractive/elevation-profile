#!/bin/bash
#| -*- mode: scheme; coding: utf-8; -*-
USE_RUNTIME_COMPILE="$1"
test -z "$USE_RUNTIME_COMPILE" || USE_RUNTIME_COMPILE="-Fuse-runtime-compile"
# |#
:; exec gosh -I. -I../runtime-compile -I../profile -I../gc-hack -uprofile $USE_RUNTIME_COMPILE -- $0 "$@"

;;disable debug print
;; (define-syntax debug-print
;;   (syntax-rules ()
;;     ((_ ?form)
;;      ?form)))
;; (let1 x debug-print 
;;   (with-module gauche.vm.debugger (set! debug-print x)))

;; (use gc-hack)
;; (gc-set-warn-proc
;;           (lambda(msg arg)
;;             (with-output-to-port (current-error-port)
;;               (lambda()
;;                 (set! called #t)
;;                 (print "SCHEME GC WARNING HANDLER HACK:\n" msg arg)
;;                 (%vm-show-stack-trace (vm-get-stack-trace-lite))))))

(use dem-gdal)
(use gauche.test)
(use rfc.http)
(use gauche.process)
(use srfi-1)
(use gauche.collection)

(test-start "dem-gdal")

(define *test-files* '("N48E008.hgt" "N48E009.hgt"))
(define *vrt-file* "all.vrt")

(define (download host path dest)
  (call-with-output-file dest
    (lambda (out)
      (http-get host path :sink out :flusher (lambda _ #t)))))

(define (run l)
  (let1 p (run-process l)
    (process-wait p #f #t)))

(define (unzip x)
  (run `(unzip ,x))
  (sys-unlink x))

;;; download and unzip data files if needed

(for-each
 (lambda(file)
   (when (not (file-exists? file))
     (with-output-to-port (current-error-port)
       (cut print "downloading " file))
     (download "dds.cr.usgs.gov"
               (string-append "/srtm/version2_1/SRTM3/Eurasia/"
                              file
                              ".zip")
               (string-append file ".zip"))
     (unzip (string-append file ".zip"))))
 *test-files*)

;;; build test vrt file

(when (not (file-exists? *vrt-file*))
  (with-output-to-port (current-error-port)
    (cut print "build vrt file " *vrt-file*))
  (run (append `(gdalbuildvrt ,|*vrt-file*|)
               *test-files*)))

;;; project N48E008 to utm 32N

(when (not (file-exists? "N48E008_utm.tif"))
  (run `(gdalwarp -dstnodata -32748 -t_srs ,#`"epsg:,(+ 32600 32)"
                  "N48E008.hgt"
                  "N48E008_utm.tif")))

;; ;;; void filled

;; (when (not (file-exists? "N48E008_filled.tif"))
;;   (run `(gdal_fillnodata.py "N48E008.hgt" "N48E008_filled.tif")))

;;; low resolution averaged version

(when (not (file-exists? "lores.tif"))
  (run `(gdaladdo -r average ,|*vrt-file*| 2 4))
  (run `(gdal_translate -outsize 25% 25% ,|*vrt-file*| "lores.tif")))

;; (run `(gdaldem hillshade -s 111120 ,|*vrt-file*| hillshade.tif))

;;; the real tests

;; make sure find works as expected on collections
;; see also gauche mailing list:
;; Message-ID: <87d34i9smp.fsf@karme.de>
(test* "find"
       -10000.0
       (find (cut = -10000.0 <>) #f32(-10000.0 -10000.0)))

(test* "dem->xy->z"
       735
       (let1 z (dem->xy->z *vrt-file*)
         (round->exact (z 8.5 48.5))))

(test* "dem->xy-project->z (1)"
       735
       (let1 z (dem->xy-project->z "epsg:4326" *vrt-file*)
         (round->exact (z 8.5 48.5))))

(test* "dem->xy-project->z (2)"
       735
       (let1 z (dem->xy-project->z "epsg:25832" *vrt-file*)
         (round->exact (z 463064.1314 5371996.2822))))

(test* "dem->xy-project->z nodata(1)?"
       #f
       (let1 z (dem->xy-project->z "epsg:4326" *vrt-file*)
         (nan? (z 8.5 48.5))))

(test* "dem->xy-project->z nodata(2)?"
       #t
       (let1 z (dem->xy-project->z "epsg:4326" *vrt-file*)
         (nan? (z 8.29151 48.87847))))

(define-condition-type <nodata-error> <error>
  nodata-error?)

(test* "dem->xy-project->z test error"
       (test-error <nodata-error>)
       ((dem->xy-project->z "epsg:4326" *vrt-file* :next (lambda _ (error <nodata-error>)))
        8.29151 48.87847))

(test* "nearest"
       '(158.0 158.0 155.0 560.0 738.0 649.0)
       (let1 z (dem->xy-project->z "epsg:4326" *vrt-file*
                                   :next (lambda _ (error <nodata-error>))
                                   :interpolation 'nearest)
         (map (cut apply z <>) '((8 49) (8.000416666666666 49) (8.000833333333333 49)
                                 (8 48) (9 48) (10 48)))))

(test* "dem->xy-project->z :next (simple)"
       #f
       (let1 z (dem->xy-project->z "epsg:4326" *vrt-file* :next (lambda _ 0))
         (nan? (z 8.29151 48.87847))))

(test* "dem->xy-project->z (stacked)"
       #t
       (let* ((z1 (dem->xy-project->z "epsg:4326" "lores.tif" :next (lambda _ (error "foo"))))
              (z (dem->xy-project->z "epsg:4326" "N48E008_utm.tif" :next z1))
              (d (abs (apply - (map (cut apply <> '(8.29151 48.87847)) (list z z1))))))
         (and (> d 0) (< d 1))))

(test* "dem-stack->xy->z"
       #t
       (let* ((z1 (dem->xy-project->z "epsg:4326" "lores.tif" :next (lambda _ (error "foo"))))
              (z (dem->xy-project->z "epsg:4326" "N48E008_utm.tif" :next z1))
              (zs (dem-stack->xy->z "epsg:4326" '(("N48E008_utm.tif")
                                                  ("lores.tif" :next (lambda _ (error "foo")))))))
         (apply = (map (cut apply <> '(8.29151 48.87847)) (list z zs)))))


(test* "dem-stack->xy->z (2)"
       (test-error <nodata-error>)
       ((dem-stack->xy->z "epsg:4326" `(("N48E008_utm.tif")
                                        ("lores.tif" :next ,(lambda _ (error <nodata-error>)))))
        0 0))

(test* "wrap-around"
       (list 2801 2801 2801 2801 2801
             0 0 0 0 0 0
             0 0
             242 242
             163 163
             95 95
             0
             676)
       (map (compose round->exact
                     (cute apply (dem->xy-project->z "epsg:4326" "gmted2010_mn30@480.tif" :next (lambda _ 0)) <>))
            '((0 -90) (0 -90.0001) (180.0001 -90.00001) (-180.0001 -90.00001) (360.0001 -90.00001)
              (0 90) (0 90.0001) (180 90) (180.0001 90) (720 90) (-720 90)
              (180 0) (181 0)
              (180 66) (-180 66)
              (180.1 66) (-179.9 66)
              (179 66) (-181 66)
              (47 90.00001)
              (8.5 48.5))))

(test* "wrap-around (stack)"
       (list 2801 2801 2801 2801 2801
             0 0 0 0 0 0
             0 0
             242 242
             163 163
             95 95
             0
             735)
       (map (compose round->exact
                     (cute apply (dem-stack->xy->z "epsg:4326"
                                                   `(("N48E008_utm.tif")
                                                     ("gmted2010_mn30@480.tif" :next ,(lambda _ 0)))) <>))
            '((0 -90) (0 -90.0001) (180.0001 -90.00001) (-180.0001 -90.00001) (360.0001 -90.00001)
              (0 90) (0 90.0001) (180 90) (180.0001 90) (720 90) (-720 90)
              (180 0) (181 0)
              (180 66) (-180 66)
              (180.1 66) (-179.9 66)
              (179 66) (-181 66)
              (47 90.00001)
              (8.5 48.5))))

;; old gauche versions don't support keywords
;; (test-end :exit-on-failure #t)

(test-end)
