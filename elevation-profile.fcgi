#!/bin/bash -xe
#| -*- mode: scheme; coding: utf-8; -*-
PATH=$PATH:/usr/bin
export PATH
exec gosh -I. -- $0 "$@"
|#

(use elevation-profile-ws)

;; empty list => use default config
(define (config)
  '())

;; example config
;; (define (config)
;;   '("epsg:4326"
;;     (("/usr/share/dem-data/germany.tif")
;;      ("/usr/share/elevation-profile/gmted2010_mn30@480.tif" :next 0))))

(define (main args)
  (apply elevation-profile-ws-main (cons config args)))
