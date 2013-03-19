#!/bin/bash -xe
#| -*- mode: scheme; coding: utf-8; -*-
PATH=$PATH:/usr/bin
export PATH
exec gosh -I. -- $0 "$@"
|#

(use elevation-profile-ws)

(define (config)
  '())

(define (main args)
  (apply elevation-profile-ws-main (cons config args)))
