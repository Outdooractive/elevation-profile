;;;
;;; simple SVG plot using gnuplot
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
(define-module svg-plot
  (use gauche.process)
  (use file.util)
  (use util.list)
  (use gauche.sequence)
  (export svg-plot
          svg-plot-3d))

(select-module svg-plot)

;; (define (with-output-to-gnuplot x)
;;   (let* ((p (run-process '(gnuplot) :input :pipe :output (current-output-port)))
;;          (r (with-output-to-port (process-input p) x)))
;;     (process-wait p)
;;     r))

;; work around string output port troubles
(define (with-output-to-gnuplot x)
  (let* ((p (run-process '(gnuplot) :input :pipe :output :pipe))
         (r (with-output-to-port (process-input p) x)))
    (copy-port (process-output p) (current-output-port))
    (process-wait p)
    r))

(define (list->data-file l)
  ;; todo:
  ;; - better nan handling?
  ;; - what about rational/complex numbers?
  ;; - better raise an error?
  ;; - use binary data-file format?
  (for-each (lambda(x)
              (apply print
                     (intersperse " "
                                  (map (lambda(n) (if (or (not (number? n))
                                                          (and (number? n)
                                                               (nan? n)))
                                                    0
                                                    n)) x))))
            l))

(define (svg-plot ll . args)
  (let-keywords args ((titles #f)
                      (hook #f)
                      (size #f))
    (with-output-to-gnuplot
     (lambda()
       (print "set terminal svg mouse" (if size (string-append " size " (string-join (map x->string size) ",")) ""))
       (print "set style data linespoints")
       ;;(print "set grid")
       (when (procedure? hook) (hook))
       (print (string-append "plot "
                             (string-join
                              (map-with-index (lambda(idx l)
                                                (string-append "\"-\""
                                                               (if (and titles
                                                                        (string? (ref titles idx #f)))
                                                                 #`" title \",(ref titles idx)\""
                                                                 " notitle")))
                                              ll)
                              ",")))
       (for-each (lambda(l)
                   (list->data-file l)
                   (print "e"))
                 ll)
       (print "exit")))))

;; todo: nearly duplicate
(define (svg-plot-3d ll . args)
  (let-keywords args ((titles #f)
                      (hook #f)
                      (size #f))
    (let1 proc (lambda()
                 (print "set terminal svg mouse" (if size (string-append " size " (string-join (map x->string size) ",")) ""))
                 ;; (print "set linetype 1 lc rgb \"red\"")
                 ;; (print "set linetype 2 lc rgb \"black\"")
                 (print "set hidden3d trianglepattern 7")
                 (print "set style data lines")
                 (when #f
                   (print "set pm3d at s depthorder hidden3d")
                   (print "unset surf"))
                 (when (procedure? hook) (hook))
                 (print (string-append "splot "
                                       (string-join
                                        (map-with-index (lambda(idx l)
                                                          (string-append "\"-\""
                                                                         (if (and titles
                                                                                  (string? (ref titles idx #f)))
                                                                           #`" title \",(ref titles idx)\""
                                                                           " notitle")))
                                                        ll)
                                        ",")))
                 (for-each (lambda(l)
                             (for-each (lambda(x)
                                         (list->data-file x)
                                         (print))
                                       l)
                             (print "e"))
                           ll)
                 (print "exit"))
      (with-output-to-gnuplot proc)
      ;; (with-output-to-file "/tmp/debug" proc)
      )))
