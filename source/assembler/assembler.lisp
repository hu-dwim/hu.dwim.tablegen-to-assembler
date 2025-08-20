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

(define-constant +x86-registers/16+ '(ax cx dx bx sp bp si di) :test 'equal)
(define-constant +x86-registers/32+ '(eax ecx edx ebx esp ebp esi edi) :test 'equal)
(define-constant +x86-registers/64+ '(rax rcx rdx rbx rsp rbp rsi rdi) :test 'equal)

(declaim (inline register-index))
(defun register-index (name registers &optional (default nil default?))
  (declare (type symbol name))
  (or (position name registers :test 'string=)
      (if default?
          default
          (error "Unexpected register name: ~S" name))))

(defun register-name->encoding-bits (name)
  (declare (optimize (debug 3))
           (type symbol name))
  (let ((name/s (symbol-name name)))
    (case (elt name/s 0)
      (#\R (aif (register-index name +x86-registers/64+ nil)
                (values it 64 nil)
                (values (let ((length (length name/s))
                              (index (- (char-code (elt name/s 1))
                                        (char-code #\0))))
                          (ecase length
                            (2 (values))
                            (3 (setf index (+ (* index 10)
                                              (- (char-code (elt name/s 2))
                                                 (char-code #\0))))))
                          (unless (<= 8 index 15)
                            (error "Unexpected register name: ~S" name))
                          index)
                        64 1)))
      (#\E (values (register-index name +x86-registers/32+) 32 nil))
      (t (values (register-index name +x86-registers/16+) 16 nil)))))
