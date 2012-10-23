;;;
;;; format-json.scm - JSON output formater
;;;
;;; Copyright (c) 2009-2012 Jens Thiele <karme@karme.de>
;;; based on:
;;; json.scm - JSON (RFC4627) Parser
;;;
;;;   Copyright (c) 2006 Rui Ueyama (rui314@gmail.com)
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

;;; http://www.ietf.org/rfc/rfc4627.txt

(define-module format-json
  (use srfi-13)
  (use srfi-14)
  (use srfi-43)
  (use util.list)
  (use gauche.collection)
  (use gauche.sequence)
  (export ->jsonf
          format-number-fixed-point
          print-fixed-point))
(select-module format-json)

;;;============================================================
;;; Writer
;;;

(define (print-value obj format)
  (cond ((eq? obj 'false) (display "false"))
        ((eq? obj 'null)  (display "null"))
        ((eq? obj 'true)  (display "true"))
        ((pair? obj)      (print-object obj format)) ;; todo: empty list?
        ((vector? obj)    (print-array obj format))
        ((number? obj)    ((ref format 'number print-number) obj))
        ((string? obj)    (print-string obj))
        (else (error "format-json expects list or vector, but got:" obj))))

(define (print-object obj format)
  (display "{")
  (fold (lambda (attr comma)
          (display comma)
          (print-string (x->string (car attr)))
          (display ":")
          (print-value (cdr attr) format)
          ",")
        "" obj)
  (display "}"))

(define (print-array obj format)
  (display "[")
  (vector-for-each (lambda (i val)
                     (unless (zero? i) (display ","))
                     (print-value val format))
                   obj)
  (display "]"))

(define (print-number num)
  (cond [(or (not (real? num)) (not (finite? num)))
         (error "real number expected, but got" num)]
        [(and (rational? num) (not (integer? num)))
         (write (exact->inexact num))]
        [else (write num)]))

;; uh - quite ugly - and likely does not conform to spec
(define (print-string str)
  (display
   (string-append
    "\""
    (string-join
     (map
      (lambda(c)
	(if (char-set-contains? char-set:ascii c)
	    (subseq (write-to-string (string c)) 1 -1)
	    (format "\\u~4,'0x" (char->ucs c))))
      str)
     "")
    "\"")))

(define (->jsonf x . args)
  (let-optionals* args ((format '()))
    (let ((f (alist->hash-table format)))
      (with-output-to-string
        (lambda ()
          (cond ((pair? x)   (print-object x f))
                ((vector? x) (print-array x f))
                (else (error "format-json expects list or vector, but got" x))))))))

(define (fixed-point precision)
  (let ((p (expt 10 precision)))
    (lambda(x)
      (receive (q r) #?=(quotient&remainder #?=(round->exact (* x p)) p)
        ((if (negative? x) - +)
         (+ (abs q) (/ (abs r) p)))))))

;; todo:
;; - did not expect it to be that complicated
;; - handle nan/infinity?
(define (format-number-fixed-point precision)
  (let ((p (expt 10 precision)))
    (lambda(x)
      ;; note: round in scheme behaves strange:
      ;; If fractional part of X is exactly 0.5, ROUND returns the closest even integer.
      (receive (q r) (quotient&remainder (round->exact (* x p)) p)
        (string-append
         (if (negative? x) "-" "")
         (if (zero? r)
           ;; todo: it is probably a bad idea to skip the point (exact vs inexact)
           (number->string (abs q))
           (string-join (list (number->string (abs q))
                              (string-trim-right (string-pad (number->string (abs r)) precision #\0) #\0)) ".")))))))

(define (string->fixed-point s)
  (let* ((qr (string-split s "."))
         (q (string->number (car qr)))
         (r (if (null? (cdr qr)) "0" (cadr qr))))
    (when (string-null? s)
      (error "empty string not allowed"))
    ((if (char=? (ref s 0) #\-) - +)
     q
     (/ (string->number r) (expt 10 (string-length r))))))

(define (test-format-number-fixed-point)
  (let ((f (format-number-fixed-point 4)))
    (for-each
     (lambda(x)
       (let* ((n #?=(string->fixed-point #?=(f x)))
              (e #?=(abs (- n x))))
         (when (> e (/ 1 (expt 10 4)))
           (error "test failed: " x (exact->inexact e)))))
     #?=((lambda(l)
           (append l (map - l)))
         `(10 ,(+ 10000000000000 1/3) 10000.3299 1003.3 1E100 0 0.00005
              0.1 0.02 999999999999999999.00005)))))

(define (print-fixed-point precision)
  (let ((f (format-number-fixed-point precision)))
    (lambda(x)
      (display (f x)))))

(define (main args)
  #?=(->jsonf #(0 0 0))
  #?=(->jsonf #(0 0 0) `((number . ,(print-fixed-point 4)))))

(provide "format-json")
