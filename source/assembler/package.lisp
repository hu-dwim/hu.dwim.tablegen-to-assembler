;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(uiop:define-package :hu.dwim.genassem/asm-base
  (:use :alexandria
        :anaphora
        :common-lisp
        )
  (:export
   #:emit-byte
   #:with-asm
   #:buffer-of
   #:register-name->encoding-bits
   ))

(uiop:define-package :hu.dwim.genassem/x86
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.genassem/asm-base
        )
  (:export
   #:emit-byte
   #:with-asm
   #:buffer-of
   #:register-name->encoding-bits
   ))
