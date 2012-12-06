#!/bin/bash -xe
#| -*- mode: scheme; coding: utf-8; -*-
PATH=$PATH:/usr/bin
export PATH
exec gosh -Fuse-runtime-compile -I. -- $0 "$@"
|#

(use elprows)

(define (config)
  '())

(define (main args)
  (apply elprows-main (cons config args)))
