;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/asm-base)

(defvar *asm-context*)

#+nil
(hu.dwim.defclass-star:defclass* asm-context ()
  ((mode :type (member 16 32 64))
   (buffer (make-array 64 :element-type (unsigned-byte 8) :adjustable t)
           :type (vector '(unsigned-byte 8)))))

(defclass asm-context nil
  ((mode :accessor mode-of :initarg :mode :type (member 16 32 64))
   (buffer :initform
           (make-array 64 :element-type '(unsigned-byte 8) :adjustable t
                          :fill-pointer 0)
           :accessor buffer-of :initarg :buffer :type
           (vector '(unsigned-byte 8)))))

(defmacro with-asm (() &body body)
  `(let ((*asm-context* (make-instance 'asm-context)))
     ,@body
     *asm-context*))

(defun emit-byte (byte)
  (vector-push-extend byte (buffer-of *asm-context*)))
