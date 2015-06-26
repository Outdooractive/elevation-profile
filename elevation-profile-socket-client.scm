;;;
;;; elevation profile socket client
;;;
;;;   Copyright (c) 2013 Jens Thiele <karme@karme.de>
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
(define-module elevation-profile-socket-client
  (use util.list)
  (use gauche.sequence)
  (use gauche.time)
  (use gauche.net)
  (export polyline->3d
          upsample-polyline->4d
          sample-polyline->4d))

(select-module elevation-profile-socket-client)

(define (retry f count . args)
  (let-optionals* args ((onerrorf (lambda(e)
                                    (with-output-to-port (current-error-port)
                                      (lambda()
                                        (print "error: " (ref e 'message e))
                                        (sys-sleep 1))))))
    (let loop ((count count))
      (guard (e
              [else
               ;; (guard (e
               ;;         [else
               ;;          #t])
               (onerrorf e)
               ;;)
               (when (not (> count 0))
                 (raise e))
               (loop (- count 1))])
             (f)))))

;; based on:
;; http://practical-scheme.net/wiliki/wiliki.cgi?cut-sea%3Alog
(define-syntax define-memoized
  (syntax-rules ()
    ((_ (fn . arg) body ...)
     (define-memoized fn (lambda arg body ...)))
    ((_ fn lambda-body)
     (define fn (let ((cache (make-hash-table 'equal?))
                      (rawfn lambda-body))
                  (lambda args
                    (if (hash-table-exists? cache args)
                      (apply values (hash-table-get cache args))
                      (receive val
                          (apply rawfn args)
                        (hash-table-put! cache args val)
                        (apply values val)))))))))

(define-memoized (get-functions . args)
  (let-optionals* args ((socket-spec '(inet "localhost" 2223)))
    (let1 s #f
      (define (connect-2)
        (apply make-client-socket socket-spec))
      (define (connect)
        (set! s (retry connect-2 5))
        (assert-protocol 1 0))
      (define  (reconnect)
        (when s (socket-close s))
        (connect))
      (define (remote-eval-2 sexp)
        (with-ports (socket-input-port s) (socket-output-port s) #f
                    (lambda()
                      (write sexp)
                      (newline)
                      (flush)
                      (let1 r (read) ;; todo: security!
                        (when (eof-object? r)
                          (error r))
                        r))))
      (define (remote-eval sexp)
        (let1 r (retry (cut remote-eval-2 sexp)
                       5
                       (lambda(e)
                         (with-output-to-port (current-error-port)
                           (lambda()
                             (print "error: " (ref e 'message e) " => reconnect")
                             (reconnect)))))
          (when (and (list? r)
                     (eq? (car r) 'error))
            (error r))
          r))
      (define (assert-protocol . l)
        (let1 r (remote-eval '(protocol-version))
          (when (not (and (list? r)
                          (equal? l r)))
            (error "protocol mismatch"))))
      (connect)
      `((z . ,(lambda(pl) (remote-eval (cons 'z pl))))
        (polyline->3d . ,(lambda(pl) (remote-eval (list 'polyline->3d pl))))
        (upsample-polyline->4d . ,(lambda(pl dist) (remote-eval (list 'upsample-polyline->4d 'wgs84 pl dist))))
        (sample-polyline->4d . ,(lambda(pl count) (remote-eval (list 'sample-polyline->4d 'wgs84 pl count))))))))

(define (polyline->3d service pl)
  ((assoc-ref (get-functions service) 'polyline->3d) pl))

(define (upsample-polyline->4d service pl upsample . args)
  ((assoc-ref (get-functions service) 'upsample-polyline->4d) pl upsample))

(define (sample-polyline->4d service pl samples . args)
  ((assoc-ref (get-functions service) 'sample-polyline->4d) pl upsample))
