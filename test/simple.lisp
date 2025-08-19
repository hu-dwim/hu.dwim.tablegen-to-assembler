;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(in-suite test)

(defun ndisasm-output (asm-context)
  (uiop:run-program
   ;; "ndisasm -b 64 -"
   '("/gnu/store/hmi4v827sq0r8c7qpz7an8405xm5zwwf-nasm-2.15.05/bin/ndisasm"
     "-b" "64" "-p" "intel" "-")
   :external-format :iso-8859-1
   :input (make-string-input-stream
           (babel:octets-to-string
            (hu.dwim.genassem/asm-base::buffer-of asm-context)
            :encoding :iso-8859-1))
   :output :string
   :error-output :string))

(defparameter *simple-tests*
'(((_ret64)
   (_hlt)
   (_hlt)
   "00000000  C3                ret
00000001  F4                hlt
00000002  F4                hlt
")))

(deftest simple ()
  (loop :for entry :in *simple-tests*
        :for instrs = (butlast entry)
        :for expected = (car (last entry))
        :for context = (eval `(with-asm () ,@instrs))
        :do (is (equal expected (ndisasm-output context)))))
