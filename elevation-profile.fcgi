#!/bin/bash -xe
#| -*- mode: scheme; coding: utf-8; -*-
PATH=$PATH:/usr/bin
export PATH
# newer gdal versions use too much cache (5% of available RAM)
# see also:
# https://trac.osgeo.org/gdal/wiki/ConfigOptions#GDAL_CACHEMAX
# use old default of 40MB
export GDAL_CACHEMAX=40
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
