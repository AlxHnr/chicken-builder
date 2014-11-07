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
;; chicken-builder targets. This module is implicitly included by all
;; ch-targets.

(module ch-syntax (ch-import)
  (import chicken scheme)

  ;; Unifies imports and uses declarations. This will be used by the build
  ;; system to determine dependencies.
  (define-syntax ch-import
    (syntax-rules ()
      ((_ mod1 mod2 ...)
       (begin
         (declare (uses mod1 mod2 ...))
         (import mod1 mod2 ...))))))