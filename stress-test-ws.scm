#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.test)
(use srfi-1)
(use gauche.collection)
(use gauche.sequence)
(use util.list)
(use gauche.net)
(use control.thread-pool)
(use util.queue)
(use control.job)
(use elevation-profile-client)
(use sxml.adaptor) ;; for assert

(define *host* "localhost")
(define *path* "/cgi-bin/elevation-profile.fcgi")
(define *url* `(,|*host*| ,|*path*|))

(define *polyline* '((9.05723 48.51388) (9.05723 48.51389) (9.05778 48.51395) (9.05749 48.51411) (9.05742 48.51439) (9.05565 48.51464) (9.0555 48.51526) (9.05529 48.51588) (9.05557 48.51618) (9.05555 48.51644) (9.05552 48.51665) (9.05526 48.51654) (9.05571 48.51653) (9.05614 48.51678) (9.05636 48.5167)))

(debug-print-width 4000)

(define *expected* #?=(upsample-polyline->4d *url* *polyline* 50))

(assert (list? *expected*))
(assert (list? (car *expected*)))
(assert (= (length (car *expected*)) 4))
#?=(last (last *expected*))
(assert (= (round->exact (last (last *expected*))) 584))
        
(define (request)
  (let1 r (upsample-polyline->4d *url* *polyline* 50)
    (equal? r *expected*)))

(define (aborted-request oformat)
  (call-with-client-socket (make-client-socket 'inet *host* 80)
    (lambda(in out)
      (format out #`"GET ,|*path*|?format=,|oformat|&path=48.5,,9|48.5,,9.1&upsample=10 HTTP/1.1\r\n")
      (format out "User-Agent: gosh\r\n")
      (format out #`"Host: ,|*host*|\r\n")
      (format out "Accept: */*\r\n\r\n")
      (flush out)
      (close-input-port in))))

(define (main args)
  (dotimes (j 8)
    (let1 pool (make-thread-pool 16)
      (dotimes (i (* 16 2))
        (add-job! pool (lambda()
                         (aborted-request "js")
                         (request))
                  #t))
      (when (not (and #?=(wait-all pool)
                      (every (compose (lambda(r)
                                        (when (not (equal? #t r))
                                          #?=r)
                                        (when (<error> r)
                                          #?=(~ r 'message))
                                        (equal? #t r))
                                      job-result)
                             (queue->list (~ pool 'result-queue)))))
        (error "error"))))
  0)
