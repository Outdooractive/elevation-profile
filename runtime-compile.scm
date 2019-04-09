;;;
;;; runtime compilation / loading of c(ise)-code
;;;
;;;   Copyright (c) 2011-2019 Jens Thiele <karme@karme.de>
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

#!no-fold-case

;; notes:
;; - using gauche 0.9 modules marked as *EXPERIMENTAL* => maybe will
;;   break with other versions!
;; - each compilation loads a module that is never unloaded
;; - likely doesn't work on windows
;; - there is also dyncomp
;;   <http://www.koguro.net/prog/dyncomp/index.html>
;;   using tiny c compiler (tcc) for compilation
(define-module runtime-compile
  (use gauche.package.compile)
  (use gauche.cgen.precomp)
  (use gauche.version)
  (use file.util)
  (use srfi-27)
  ;; (use gauche.process) ;; only needed for assembly output
  (export compile-and-load
          cise-compile-and-load))

(select-module runtime-compile)

(random-source-randomize! default-random-source)

(define (random-string)
  (format "~x" (random-integer #xfffffffff)))

(define (random-symbol)
  (string->symbol (string-append "grtc" (random-string))))

(define (eprint . l)
  (with-output-to-port (current-error-port)
    (cut apply print l)))

;; todo: name is somewhat misleading/ambiguous / should be in file.util?
(define (with-temporary-directory thunk)
  (let1 dir (sys-mkdtemp (string-append (temporary-directory) "/grtc"))
    (unwind-protect
     (thunk dir)
     (guard (e
             [else
              ;;(eprint "rmdir " dir " failed") ;; todo
              ])
            (sys-rmdir dir)))))

(define (with-current-directory name thunk)
  (let1 old-dir (current-directory)
    (unwind-protect
     (begin
       ;; fake make to let emacs know we changed directory
       ;;(eprint #`"runtime-compile: Entering directory `,|name|'")
       (current-directory name)
       (thunk))
     (begin
       ;;(eprint #`"runtime-compile: Leaving directory `,|name|'")
       (current-directory old-dir)))))

(define (cat x)
  (copy-port (open-input-file x) (current-output-port)))

;; (define (cat-asm c-file)
;;   (gauche-package-compile c-file
;;                           :verbose #t
;;                           :cppflags "-v -fverbose-asm -O3 -S")
;;   (cat (path-swap-extension c-file ".o")))

(define (%compile-and-load module stub imports . args)
  ;; todo
  (define (pprint x)
    (for-each (cut format #t "~s\n" <>)
              x))

  (define (new-module-name)
    (let loop ((try 0))
      (let1 r (random-symbol)
        (cond
         [(not (find-module r))
          r]
         [(and (find-module r)
               (< try 10000))
          (loop (+ try 1))]
         [else
          (error "couldn't create module name")]))))
  
  (with-temporary-directory
   (lambda(tmpdir)
     ;; todo: really change to that directory?! likely bad idea
     ;; note: didn't manage to tell cgen-precompile to put the c-file
     ;; into different directory
     ;; (at some point sys-basename is used on file named passed via :out.c)
     (with-current-directory
      tmpdir
      (lambda()
          (let* ((new-mod  (new-module-name))
                 (scm-file #`",|new-mod|.scm")
                 (c-file   #`",|new-mod|.c")
                 (o-file   #`",|new-mod|.o")
                 (sci-file #`",|new-mod|.sci")
                 ;; todo: on windows this would be dll?
                 (so-file  #`",|new-mod|.so"))
            (with-output-to-file scm-file
              (cut pprint
                   `((define-module ,new-mod
                       (export . ,imports))
                     (select-module ,new-mod)
                     . ,stub)))
            ;; (cat scm-file)
            ;; create .c and .sci from .scm
            (cgen-precompile scm-file
			     :ext-initializer #t
			     :out.c c-file
			     :out.sci sci-file
			     :dso-name (string-append tmpdir "/" so-file))

            ;;(cat #?=c-file)

            ;; take a look at the assembly
            ;;(run-process `(gcc -S -fverbose-asm -ggdb -dA ,(process-output->string "gauche-config -I") -O2 ,|c-file|) :wait #t)
            ;;(cat #`",|new-mod|.s")
            ;; another version
            ;;(run-process `(gcc "-Wa,-adhln" -g -c -O2 -o /dev/null ,(process-output->string "gauche-config -I") ,(process-output->string "gauche-config --so-cflags") ,|c-file|) :wait #t)

            ;; compile .so
            (with-output-to-port (current-error-port)
              (lambda()
                (apply gauche-package-compile-and-link
                       (append (list new-mod
                                     (list c-file))
                               args))))
            ;; (cat sci-file)
            (for-each (cut eval <> module)
                      `((load ,sci-file :paths (list ,tmpdir))
                        (import ,|new-mod|)))
            (for-each sys-unlink (list c-file sci-file scm-file))
            ;; todo: on windows you can't delete libraries in use?
            (gauche-package-clean new-mod (list c-file))
            new-mod))))))

;; todo:
;; - be closer to c-wrapper c-load api?
(define-macro (compile-and-load . args)
  `((with-module runtime-compile %compile-and-load) (current-module) . ,args))

;; todo:
;; - be closer to c-wrapper c-load api?
(define-macro (cise-compile-and-load stub . args)
  `(compile-and-load (list (cons 'inline-stub ,stub)) . ,args))
