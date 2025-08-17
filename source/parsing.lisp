(in-package :hu.dwim.tablegen-to-assembler)

(defun json-value (obj key)
  (cdr (assoc key obj :test 'equal)))

(defun true-json-value? (obj key)
  (equal (json-value obj key) 1))

(defun json-bitfield-value (obj key)
  (let ((bits (json-value obj key)))
    (assert (eq :array (pop bits)))
    (json-bitfield-to-integer bits)))

(defun pseudo-instruction? (obj)
  (or (true-json-value? obj "isPseudo")
      (true-json-value? obj "isCodeGenOnly")))

(defun asm-mnemonic (obj)
  (let ((value (json-value obj "AsmString")))
    (check-type value string)
    (when (plusp (length value))
      value)))

(defun get-encoding (obj)
  (let ((value (json-value obj "Opcode")))
    (when (eq :array (pop value))
      (assert (eql 8 (length value)))
      value)))

(defun json-bitfield-to-integer (bit-list &optional (count (length bit-list)))
  (let ((result 0))
    (loop :for i = 0 :then (incf i)
          :while (and bit-list
                      (< i count))
          :for bit = (pop bit-list)
          :do (progn
                (setf result (ash result 1))
                (eswitch (bit)
                  (1 (setf result (logior result 1)))
                  (0 (values)))))
    result))

(defun get-instr-flag (obj flag-name)
  (let ((value (getf (getf obj :flags) flag-name)))
    ;; return unset flags as zero
    (or value 0)))

(defun normalize-instruction (obj)
  (assert (eq :object (pop obj)))
  (let ((result ())
        (tsflags (json-value obj "TSFlags")))
    (assert (eq :array (pop tsflags)))
    (macrolet ((pop-bits (count)
                 (with-unique-names (val)
                   `(let ((,val (json-bitfield-to-integer tsflags ,count)))
                      (setf tsflags (nthcdr ,count tsflags))
                      ,val)))
               (set-field (name value)
                 `(setf (getf result ,name) ,value))
               (set-flag (name value)
                 (once-only (value)
                   `(unless (zerop ,value)
                      (setf (getf (getf result :flags) ,name) ,value)))))
      (set-field :mnemonic (asm-mnemonic obj))

      ;; see: https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/X86/MCTargetDesc/X86BaseInfo.h#L692
      (set-flag :form-mask  (pop-bits 6))
      (set-flag :op-size    (pop-bits 2))
      (set-flag :ad-size    (pop-bits 2))
      (set-flag :op-prefix  (pop-bits 2))
      (set-flag :op-map     (pop-bits 4))
      (set-flag :rex        (pop-bits 1))
      (set-flag :imm        (pop-bits 4))
      (set-flag :fp-type    (pop-bits 3))
      (set-flag :lock       (pop-bits 1))
      (set-flag :rep        (pop-bits 1))
      (set-flag :sse-domain (pop-bits 2))
      (set-flag :encoding   (pop-bits 2))
      (set-flag :opcode     (pop-bits 8))

      ;; :vector contains all the rest of the flags for easy checking for complex insts
      (set-flag :vector     (json-bitfield-to-integer tsflags))

      (set-flag :vex-4v     (pop-bits 1))
      (set-flag :vex-l      (pop-bits 1))
      (set-flag :evex-k     (pop-bits 1))
      (set-flag :evex-z     (pop-bits 1))
      (set-flag :evex-l2    (pop-bits 1))
      (set-flag :evex-b     (pop-bits 1))
      (set-flag :cd8-scale  (pop-bits 3))
      (set-flag :evex-rc    (pop-bits 1))
      (set-flag :no-track   (pop-bits 1))
      (set-flag :explicit-op-prefix (pop-bits 2))
      (set-flag :evex-nf    (pop-bits 1))
      (set-flag :two-conditional (pop-bits 1))
      (set-flag :evex-u     (pop-bits 1))

      (let ((preds (json-value obj "Predicates")))
        (assert (eq :array (pop preds)))
        (loop :with kwpkg = (find-package :keyword)
              :for pred :in preds
              :do (progn
                    (assert (eq :object (pop pred)))
                    (let ((name (json-value pred "def")))
                      (push (intern name kwpkg)
                            (getf result :predicates))))))
      result)))

;; (defparameter *supported-encodings* '("EncNormal"))

;; called on the raw json
(defun json/include-instruction? (obj)
  (and (asm-mnemonic obj)
       (not (pseudo-instruction?  obj))
       (get-encoding obj)))

;; called on the normalized plist form
(defun include-instruction? (obj)
  (zerop (get-instr-flag obj :vector)))

(defun process-x86-json (path instruction-emitter)
  (declare (optimize (speed 3))) ; needed to force-enable tail call optimization
  (with-input-from-file (file path)
    (json-streams:with-open-json-stream
        (stream (json-streams:make-json-input-stream file))
      (let ((instruction-names ())
            (emit-counter 0))
        (block parsing
          (labels
              ((next-token ()
                 (let ((token (json-streams:json-read stream)))
                   ;; (print token) ; TODO
                   (when (eq token :eof)
                     (return-from parsing (values)))
                   token))
               (toplevel ()
                 (ecase (next-token)
                   (:begin-object (toplevel-key)))
                 (values))
               (toplevel-key ()
                 (let ((token (next-token)))
                   (unless (eq token :end-object)
                     (assert (stringp token))
                     (let ((obj (json-streams::parse-single stream)))
                       (cond
                         ((string= token "!instanceof")
                          (assert (eq :object (pop obj)))
                          (assert (not instruction-names))
                          (let ((entry (find "X86Inst" obj :test 'equal :key 'first)))
                            (assert (eq :array (second entry)))
                            (setf instruction-names (make-hash-table :test 'equal))
                            (map nil (lambda (name)
                                       (setf (gethash name instruction-names) t))
                                 (nthcdr 3 entry))
                            (format *error-output* "~&; Found ~S instructions~%"
                                    (hash-table-size instruction-names))))
                         ((and (gethash token instruction-names) ; this toplevel key is one of the listed instructions
                               (json/include-instruction? (rest obj)))
                          (let ((instr (normalize-instruction obj)))
                            (when (include-instruction? instr)
                              (assert (eq :object (pop obj)))
                              (assert (equal "X86" (json-value obj "Namespace")))
                              (incf emit-counter)
                              (format *error-output* "~&; calling emitter for ~S~%" (getf instr :mnemonic))
                              (funcall instruction-emitter instr :raw-json obj))))))))
                 (toplevel-key))

               )
            (toplevel)))
        (format *error-output* "~&; called the emitter for ~S instructions~%" emit-counter)))))

(defun x ()
  (let (;;(insts '("IMUL" "IDIV" "MOV" "XOR" "ADD" "SUB" "PUSH" "POP"))
        (predicate-blacklist '(:|Mode16|
                               :|Not64BitMode|
                               :|UseSSE1|
                               :|UseSSE2|
                               :|UseSSSE3|
                               :|UseSSE41|
                               :|UseSSE42|
                               :|HasSSEPrefetch|
                               :|UseAVX|
                               :|HasAVX|
                               :|HasAVX2|
                               :|HasAVX512|
                               :|HasF16C| ; SSE; half-precision (16-bit) fp -> single-precision (32-bit) fp
                               :|HasRTM|
                               :|HasTSXLDTRK|
                               :|HasWBNOINVD|
                               :|HasAMXTILE|
                               :|HasAMXMOVRS|
                               :|HasAMXTRANSPOSE|
                               :|HasAMXCOMPLEX|
                               :|HasMOVRS|
                               :|HasPCLMUL|
                               :|HasTBM|
                               :|HasCLDEMOTE|
                               :|HasUINTR|
                               ))
        (instr-counter 0)
        (emit-counter 0))
    (process-x86-json "/home/alendvai/common-lisp/maru/source/assembler/x86.json"
                      (lambda (obj &key raw-json)
                        (incf instr-counter)
                        (let* ((predicates (getf obj :predicates))
                               (include? (loop :for blacklisted :in predicate-blacklist
                                               :always (not (member blacklisted predicates :test 'eq)))))
                          (when include?
                            (incf emit-counter)
                            (print obj)
                            (print raw-json)))

                        ;; (let ((mnemonic (getf obj :mnemonic)))
                        ;;   (when (starts-with-subseq "unpck" mnemonic)
                        ;;     (print obj)
                        ;;     (print raw-json)))
                        ))
    (format *error-output* "~&; emitted ~S of ~S instructions~%" emit-counter instr-counter)))
