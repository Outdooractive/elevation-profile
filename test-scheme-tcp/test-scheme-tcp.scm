#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I.. -- $0 "$@"
(use sxml.adaptor) ;; for assert
(use elevation-profile-socket-client)
(define (main args)
  (dotimes (i 10)
    (assert (= (length (upsample-polyline->4d '(inet "localhost" 10000) '((8.5 48.5) (8.6 48.5)) 1000)) 9)))
  0)
