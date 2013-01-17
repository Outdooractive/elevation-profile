#| -*- mode: scheme; coding: utf-8; -*- |#
;;;
;;; simple geodesic calculations on wgs84 spheroid or sphere using
;;; geographiclib >=1.6 (http://geographiclib.sourceforge.net/)
;;;
;;; Copyright (C) 2010-2012 Jens Thiele <karme@karme.de>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(define-module geod
  (use gauche.collection)
  (use gauche.sequence)
  (use srfi-1)
  (use file.util)
  (use runtime-compile)
  (export geod-direct
          geod-inverse
	  geod-distance
          geod-upsample-line
          geod-upsample-polyline
	  geod-sample-polyline-with-measure
          geod-add-measure))

(select-module geod)

;; todo: also in ...
(define-macro (assert e)
  `(if (not ,e)
     (error "Assertion failed: " ,(x->string e))))

(define (spheroid->int s)
  (case s
    [(wgs84)
     0]
    [(sphere)
     1]
    [else
     (error "Unknown spheroid" s)]))

(compile-and-load
 `((inline-stub
    (declcode
     (.include "GeographicLib/Geodesic.hpp")
     "static const GeographicLib::Geodesic sphere(6378137,0);"
     "static const GeographicLib::Geodesic* spheroid[]={&GeographicLib::Geodesic::WGS84, &sphere};"
     "double geod_direct(unsigned s,
	                 double lat1, double lon1, double azi1, double s12,
	                 double* lat2, double* lon2, double* azi2,
	                 double* m12, double* M12, double* M21, double* S12) {
        try {
          return spheroid[s]->Direct(lat1, lon1, azi1, s12,
	  		             *lat2, *lon2, *azi2,
	  		             *m12, *M12, *M21, *S12);
        }catch(...){
          // todo:
          return std::numeric_limits<double>::quiet_NaN();
        }
      }"
      "double geod_inverse(unsigned s,
	                   double lat1, double lon1, double lat2, double lon2,
	                   double* s12, double* azi1, double* azi2, double* m12,
	                   double* M12, double* M21, double* S12) {
        try{
          return spheroid[s]->Inverse(lat1, lon1, lat2, lon2,
			              *s12, *azi1, *azi2, *m12,
			              *M12, *M21, *S12);
        }catch(...){
          // todo:
          return std::numeric_limits<double>::quiet_NaN();
        }
      }")
    (define-cproc geod_direct_c (s::<int>
                                 lat1::<double> lon1::<double>
                                 azi1::<double> s12::<double>)
      (let* ((lat2::double  0)
             (lon2::double 0)
             (azi2::double 0)
             (m12::double 0)
             (M12::double 0)
             (M21::double 0)
             (S12::double 0))
        (geod_direct s lat1 lon1 azi1 s12
                     (& lat2) (& lon2)
                     ;; todo: don't use them
                     (& azi2) (& m12) (& M12) (& M21) (& S12)
                     )
        (result (SCM_LIST2 (Scm_MakeFlonum lon2)
                           (Scm_MakeFlonum lat2)))))
    (define-cproc geod_inverse_c (s::<int>
                                  lat1::<double> lon1::<double>
                                  lat2::<double> lon2::<double>)
      (let* ((s12::double  0)
             (azi1::double 0)
             (azi2::double 0)
             (m12::double 0)
             (M12::double 0)
             (M21::double 0)
             (S12::double 0))
        (geod_inverse s lat1 lon1 lat2 lon2
                      (& s12) (& azi1)
                      ;; todo: don't use them
                      (& azi2) (& m12) (& M12) (& M21) (& S12)
                      )
        (result (SCM_LIST2 (Scm_MakeFlonum azi1)
                           (Scm_MakeFlonum s12)))))))
 '(geod_direct_c geod_inverse_c)
 :cc "c++"
 :libs "-lGeographic")

(define (geod-direct s p az dist)
  (geod_direct_c (spheroid->int s) (cadr p) (car p) az dist))

(define (geod-inverse s p1 p2)
  (geod_inverse_c (spheroid->int s)
                  (cadr p1) (car p1)
                  (cadr p2) (car p2)))

(define geod-distance (compose cadr (cut geod-inverse <> <> <>)))

(define (sample x)
  (map (lambda(i)
         (/ i (- x 1)))
       (iota x)))

;; todo:
;; - use GeodesicLine or parallel variant
;; - return distance measurements?
(define (geod-upsample-line s b e max-dist)
  (let* ((inv (geod-inverse s b e))
         (az  (car inv))
         (d   (cadr inv)))
    (if (<= d max-dist)
      (list b e)
      (let1 samples (+ (ceiling->exact (/ d max-dist)) 1)
        (append (list b)
                (map (lambda(x)
                       (geod-direct s b az (* x d)))
                     (subseq (sample samples) 1 -1))
                (list e))))))

;; todo: also in ...
(define (uniq l)
  (if (null? l)
    '()
    (reverse! (fold (lambda(n o)
                      (if (equal? n (car o))
                        o
                        (cons n o)))
                    (list (car l))
                    (cdr l)))))

;; todo: parallel variant!
(define (geod-add-measure s l)
  (if (null? l)
    '()
    (reverse
     (fold
      (lambda(n o)
        (cons (append n
                      (list
                       (+ (geod-distance s (car o) n)
                          (last (car o)))))
              o))
      (list (append (car l) (list 0)))
      (cdr l)))))

;; todo:
;; - return distance measurements?
;; - usage of uniq not really good
;; - parallel variant!
(define (geod-upsample-polyline s l max-dist)
  (if (< (size-of l) 2)
    l 
    (uniq (append-map (lambda(b e)
                        (geod-upsample-line s b e max-dist))
                      (subseq l 0 -1)
                      (subseq l 1)))))

;; port from python
;; note: don't use it with lists ;-)
;; (srfi-43 only defines vector-binary-search)
(define (bisect-right seq x . args)
  (let-optionals* args ((cmpfn <)
			(lo 0)
			(hi (size-of seq)))
    (if (>= lo hi)
      lo
      (let1 mid (quotient (+ lo hi) 2)
        (if (cmpfn x (ref seq mid))
          (bisect-right seq x cmpfn lo mid)
          (bisect-right seq x cmpfn (+ mid 1) hi))))))

(define bisect bisect-right)

(define (seq-empty? seq)
  (zero? (size-of seq)))

(define (seq-first seq)
  (assert (not (seq-empty? seq)))
  (ref seq 0))

(define (seq-last seq)
  (assert (not (seq-empty? seq)))
  (ref seq (- (size-of seq) 1)))

;; returns resampled 3d polyline with distance measurements of
;; original polyline as third component
;; note: usually resulting polyline will have different length, if you
;; remeasure!
(define (geod-sample-polyline-with-measure s pl samples)
  (let* ((plm (coerce-to <vector> (geod-add-measure s pl)))
	 (total-length (last (seq-last plm)))
	 (pl-at (lambda(t)
		  (let* ((pos (- (bisect-right plm
                                               t
                                               (lambda(x elt)
                                                 (< x (last elt))))
                                 1))
			 (segment-offset (if (< pos 0)
                                           t
                                           (- t (last (ref plm pos))))))
		    (assert (<= t total-length))
		    (subseq (geod-direct s
					 (subseq (ref plm pos) 0 2)
					 (car (geod-inverse s
                                                            (ref plm pos)
                                                            (ref plm
                                                                 (+ pos 1))))
					 segment-offset)
			    0
			    2)))))
    (assert (not (seq-empty? plm)))
    (append (list (seq-first plm))
	    (map (lambda(t)
		   (let1 d (* t total-length)
                     (append (pl-at d)
                             (list d))))
		 (subseq (sample samples) 1 -1))
	    (list (seq-last plm)))))
