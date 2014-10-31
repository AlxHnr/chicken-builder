; Copyright (c) 2014 Alexander Heinrich <alxhnr@nudelpost.de>
;
; This software is provided 'as-is', without any express or implied
; warranty. In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
;    1. The origin of this software must not be misrepresented; you must
;       not claim that you wrote the original software. If you use this
;       software in a product, an acknowledgment in the product
;       documentation would be appreciated but is not required.
;
;    2. Altered source versions must be plainly marked as such, and must
;       not be misrepresented as being the original software.
;
;    3. This notice may not be removed or altered from any source
;       distribution.

;; This module contains all syntactical extensions which are usable inside
;; chicken-bundle targets. This module is implicitly included by all
;; ch-targets.

(module ch-syntax
  (ch-import function)
  (import chicken scheme)

  ;; Unifies imports and uses declarations. This will be used by the build
  ;; system to determine dependencies.
  (define-syntax ch-import
    (syntax-rules ()
      ((_ mod1 mod2 ...)
       (begin
         (declare (uses mod1 mod2 ...))
         (import mod1 mod2 ...)))))

  ;; A syntactical extension, which allows defining functions with optional
  ;; type annotations. Such functions will be exported from ch-modules and
  ;; can be used by other modules. Here are the allowed forms:
  ;;
  ;; (function (name return-type) ((arg1 type) (arg2 type)) body ...)
  ;; (function name ((arg1 type) (arg2 type)) body ...)
  ;; (function name (arg1 arg2) body ...)
  (define-syntax function
    (syntax-rules ()
      ((_ (name return-type) ((arg1 arg-type1) (arg2 arg-type2) ...)
          exp1 exp2 ...)
       (begin
         (export name)
         (declare (enforce-argument-types name))
         (: name (arg-type1 arg-type2 ... -> return-type))
         (define name
           (lambda (arg1 arg2 ...)
             exp1 exp2 ...))))
      ((_ name ((arg1 arg-type1) (arg2 arg-type2) ...)
          exp1 exp2 ...)
       (begin
         (export name)
         (declare (enforce-argument-types name))
         (: name (arg-type1 arg-type2 ... -> undefined))
         (define name
           (lambda (arg1 arg2 ...)
             exp1 exp2 ...))))
      ((_ name (arg1 arg2 ...) exp1 exp2 ...)
       (begin
         (export name)
         (define name
           (lambda (arg1 arg2 ...)
             exp1 exp2 ...)))))))
