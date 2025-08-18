(in-package :hu.dwim.tablegen-to-assembler)

(defun json-value (obj key &optional of-type)
  (let ((result (cdr (assoc key obj :test 'equal))))
    (when of-type
      (assert (eq of-type (pop result))))
    result))

(defun json/true-value? (obj key)
  (equal (json-value obj key) 1))

(defun json/bitfield-value (obj key)
  (let ((bits (json-value obj key)))
    (assert (eq :array (pop bits)))
    (json-bitfield-to-integer bits)))

(defun json/def-like-value (obj key)
  (let* ((entry (json-value obj key :object))
         (value (json-value entry "def")))
    (intern value :keyword)))

(defun pseudo-instruction? (obj)
  (or (json/true-value? obj "isPseudo")
      (json/true-value? obj "isCodeGenOnly")))

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

;; https://llvm.org/docs/TableGen/BackEnds.html#json-reference
;; "A bits array is ordered from least-significant bit to most-significant."
(defun json-bitfield-to-integer (bit-list)
  (let ((result 0))
    (loop :for i = 0 :then (incf i)
          :for bit :in (reverse bit-list)
          :do (progn
                (setf result (ash result 1))
                (eswitch (bit)
                  (1 (setf result (logior result 1)))
                  (0 (values)))))
    result))

(defun get-instr-flag (obj flag-name)
  (let ((value (getf (getf obj :flags) flag-name)))
    ;; return unset flags as zero (optimization against noise)
    (or value 0)))

;; NOTE: no, this won't work because obj should be a place...
;; (defun (setf get-instr-flag) (value obj flag-name)
;;   (setf (getf (getf obj :flags) flag-name) value))

(defun normalize-instruction (obj)
  (assert (eq :object (pop obj)))
  (let ((result ())
        ;; TODO You don’t usually need to decode TSFlags by hand,
        ;; because the higher-level fields (OpPrefix, OpMap, OpEnc)
        ;; already abstract it.
        (tsflags (json-value obj "TSFlags" :array)))
    (macrolet ((pop-bits (count)
                 (once-only (count)
                   (with-unique-names (bits val)
                     `(let* ((,bits (subseq tsflags 0 ,count))
                             (,val (json-bitfield-to-integer ,bits)))
                        (setf tsflags (nthcdr ,count tsflags))
                        ,val))))
               (set-field (name value)
                 `(setf (getf result ,name) ,value))
               (get-field (name)
                 `(getf result ,name))
               (set-flag (name value)
                 (once-only (value)
                   `(unless (zerop ,value)
                      (setf (getf (getf result :flags) ,name) ,value)))))
      (set-field :mnemonic (asm-mnemonic obj))
      (set-field :name (json-value obj "!name"))

      (let ((form-entry (json-value obj "Form" :object)))
        (set-field :form (intern (json-value form-entry "def") :keyword)))

      (set-field :opcode (json/bitfield-value obj "Opcode"))

      ;; see: https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/X86/MCTargetDesc/X86BaseInfo.h#L692
      (set-flag :form-bits  (pop-bits 7)) ; 0-7
      (set-flag :op-size    (pop-bits 2)) ; 8-9
      (set-flag :ad-size    (pop-bits 2)) ; 10-11
      (set-flag :op-prefix  (pop-bits 2)) ; 12-13
      (set-flag :op-map     (pop-bits 4)) ; 14-17
      (set-flag :rex        (pop-bits 1)) ; 18
      (set-flag :imm        (pop-bits 4)) ; 19-22
      (set-flag :fp-type    (pop-bits 3)) ; 23-25
      (set-flag :lock       (pop-bits 1)) ; 26
      (set-flag :rep        (pop-bits 1)) ; 27
      (set-flag :sse-domain (pop-bits 2)) ; 28-29
      (set-flag :encoding   (pop-bits 2)) ; 30-31
      (let ((opcode (pop-bits 8)))        ; 32-39
        ;; assert that the extracted json opcode is the same as the
        ;; one encoded into the tsflags.
        (assert (equal opcode (get-field :opcode))))

      ;; :vector contains all the rest of the flags for easy checking for complex instructions
      (set-flag :vector     (json-bitfield-to-integer tsflags)) ; 40-

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

      (let ((preds (json-value obj "Predicates" :array)))
        (loop :with kwpkg = (find-package :keyword)
              :for pred :in preds
              :do (progn
                    (assert (eq :object (pop pred)))
                    (let ((name (json-value pred "def")))
                      (push (intern name kwpkg)
                            (getf result :predicates))))))

      (let ((value (json/def-like-value obj "OpSize")))
        (assert (member value '(:|OpSize16| :|OpSize32| :|OpSizeFixed|)))
        (set-field :op-size value))

      (let ((value (json/def-like-value obj "OpMap")))
        (assert (member value '(:|OB| :|TA| :|TB| :|T8|
                                :|T_MAP4| :|T_MAP5| :|T_MAP6| :|T_MAP7|
                                :|XOPA| :|XOP8| :|XOP9| :|ThreeDNow|)))
        (set-field :op-map value))

      (let ((value (json/def-like-value obj "OpPrefix")))
        (unless (eq value :|NoPrfx|)
          (set-field :op-prefix value)))

      (let ((value (json/def-like-value obj "explicitOpPrefix")))
        (unless (eq value :|NoExplicitOpPrefix|)
          (assert (member value '(:|PD| :|PS| :|SD| :|ExplicitEVEX|
                                  :|ExplicitREX2| :|ExplicitVEX|)
                          :test 'eq))
          ;; PD: 0x66
          ;; PS: 0xF3
          ;; SD: 0xF2
          (set-field :op-prefix/explicit value)))

      ;;;
      ;;; args
      ;;;

      ;; InOperandList/OutOperandList = visible, explicit operands.
      ;; Uses/Defs = implicit operands (hidden side effects: registers, flags, memory, ports).
      (let* ((inputs     (json-value obj "InOperandList" :object))
             (json-args  (json-value inputs "args" :array)))
        (assert (equal "dag" (json-value inputs "kind")))
        (let ((args (loop :for (json-type type name) :in json-args
                          :do (assert (eq :array json-type))
                          :unless (eq :null name)
                            :collect (cons name type))))
          (set-field :args args)))

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
  (declare (optimize (speed 3)) ; needed to force-enable tail call optimization
           (type function instruction-emitter))
  (with-input-from-file (file path)
    (json-streams:with-open-json-stream
        (stream (json-streams:make-json-input-stream file))
      (let ((instruction-names ())
            (emit-counter 0))
        (declare (type fixnum emit-counter))
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
                   (:begin-object (toplevel-key))))
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
                            (format *error-output* "~&; Found ~S instruction descriptors~%"
                                    (hash-table-size instruction-names))))
                         ((and (gethash token instruction-names) ; this toplevel key is one of the listed instructions
                               (json/include-instruction? (rest obj)))
                          (let ((instr (normalize-instruction obj)))
                            (when (include-instruction? instr)
                              (assert (eq :object (pop obj)))
                              (assert (equal "X86" (json-value obj "Namespace")))
                              (incf emit-counter)
                              ;; (format *error-output* "~&; calling emitter for ~S~%" (getf instr :mnemonic))
                              ;; TODO delme
                              (when (starts-with-subseq "RET" token)
                                (format t "~&; ~A" obj))
                              (funcall instruction-emitter instr :raw-json obj))))))))
                 (toplevel-key)))
            (toplevel)))))))

(defun emit-form (form)
  (pprint form)
  ;;(terpri)
  (force-output))

;; (defun to-hex-byte-string (val)
;;   (assert (<= val #xff))
;;   (format nil "~x" val))

(defun intern/asm (str)
  (intern (string-upcase str)))

(defun generate-x86-instruction-emitter (instr)
  (destructuring-bind (&key name args mnemonic opcode
                         op-prefix ;op-prefix/explicit
                         form
                       &allow-other-keys)
      instr
    ;;(format *error-output* "~&; emitting ~S / ~S~%" name mnemonic)
    ;; (when (equal mnemonic "adc{w}	{$src, %ax|ax, $src}")
    ;;   (break))
    (when (equal name "RETI64")
      (break))
    ;; (when (starts-with-subseq "OUT" name)
    ;;   (break))
    (labels
        (
         ;; (skip-instruction ()
         ;;   (format *error-output* "; skipping ~S~%" name)
         ;;   (return-from generate-x86-instruction-emitter))
         )
      (let ((prefix (ecase op-prefix
                      ((nil)     ())
                      (:pd       #x66)
                      ((:sd :xd) #xf2)
                      ((:ps :xs) #xf3))))
        (case form
          (:|RawFrm|
           ;; RawFrm specifically means raw form: the instruction has no special ModR/M or opcode map handling—it’s just a fixed sequence of bytes.
           (assert (< opcode 256))
           (emit-form
            `(defun ,(intern/asm (concatenate 'string "_" name))
                 ,(remove nil
                   (mapcar (lambda (arg)
                             (destructuring-bind (name . type)
                                 arg
                               (declare (ignore type))
                               (assert (stringp name))
                               (intern/asm name)))
                    args))
               ,@(when prefix
                   `((emit-byte ,prefix)))
               (emit-byte ,opcode)))))))))

(defun generate-assembler/x86_64 (&key
                                    (print-source? nil)
                                    (package :hu.dwim.x86-assembler))
  (unless (packagep package)
    (setf package (find-package package)))
  (with-output-to-file
      (out-stream (asdf:system-relative-pathname
                   :hu.dwim.tablegen-to-assembler
                   "source/assembler/x86-instructions.lisp")
                  :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*standard-output* out-stream)
            (*print-readably* nil)
            (*print-case* :downcase)
            (*package* package))
        (write-string ";;; This file is generated, editing it is unwise.")
        (pprint `(in-package ,(intern (package-name package) :keyword)))
        (let ((predicate-blacklist '(:|Mode16|
                                     :|Not64BitMode|
                                     :|UseSSE1|
                                     :|UseSSE2|
                                     :|UseSSE3|
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
                                (if include?
                                    (progn
                                      (write-char #\. *error-output*)
                                      (incf emit-counter)
                                      (when print-source?
                                        (print obj)
                                        (print raw-json))
                                      (generate-x86-instruction-emitter obj))
                                    (write-char #\x *error-output*)))))
          (format *error-output* "~&; emitted ~S of ~S instructions~%" emit-counter instr-counter))))))
