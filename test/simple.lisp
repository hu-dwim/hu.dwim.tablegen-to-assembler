;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(in-suite test)

(defun disasm-output/stdin (cmdline asm-context)
  (uiop:run-program
   cmdline
   :external-format :iso-8859-1
   :input (make-string-input-stream
           (babel:octets-to-string
            (buffer-of asm-context)
            :encoding :iso-8859-1))
   :output :string
   :error-output :string))

(defun disasm-output/tmp-file (cmdline asm-context)
  (check-type cmdline list)
  (uiop:with-temporary-file (:stream stream :pathname path
                             :element-type '(unsigned-byte 8))
    (write-sequence (buffer-of asm-context) stream)
    (finish-output stream)
    (uiop:run-program
     (append cmdline (list (namestring path)))
     :output :string
     :error-output :string)))

(defun ndisasm-output (asm-context)
  (disasm-output/stdin
   '("/gnu/store/hmi4v827sq0r8c7qpz7an8405xm5zwwf-nasm-2.15.05/bin/ndisasm"
     "-b" "64" "-p" "intel" "-")
   asm-context))

(defun xed-output (asm-context)
  (disasm-output/tmp-file
   '("/gnu/store/vdhg7r1s24cbwgh4jv46937bfs0zjcgl-intel-xed-2025.03.02/bin/xed"
     "-64" "-ir")
   asm-context))

(defun zydis-output (asm-context)
  (disasm-output/tmp-file
   '("/gnu/store/d0a5xrkss23rg8jqzf5pwcvr04pml6z0-zydis-4.1.0/bin/ZydisDisasm"
       "-64")
   asm-context))

(defparameter *simple-tests*
'(((_ret64)
   (_hlt)
   (_pause)
   (_bswap32r edx)
   (_bswap64r r14)
   (_noop)
   "00000000  C3                ret
00000001  F4                hlt
00000002  F390              pause
00000004  0FCA              bswap edx
00000006  490FCE            bswap r14
00000009  90                nop
")))

(deftest simple ()
  (loop :for entry :in *simple-tests*
        :for instrs = (butlast entry)
        :for expected = (car (last entry))
        :for context = (eval `(with-asm () ,@instrs))
        :do (is (equal expected (ndisasm-output context)))
        ;; :do (print (xed-output context))
        ;; :do (print (zydis-output context))
        ))
