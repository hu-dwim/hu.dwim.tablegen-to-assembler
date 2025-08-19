;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(uiop:define-package :hu.dwim.genassem/asm-base
  (:use :common-lisp
        :alexandria
        )
  (:export
   #:emit-byte
   #:with-asm
   ))

(uiop:define-package :hu.dwim.genassem/x86
  (:use :common-lisp
        :alexandria
        :hu.dwim.genassem/asm-base
        )
  (:export
   #:emit-byte
   #:with-asm
   ))
