;;; This file is generated, editing it is unwise.
(in-package :hu.dwim.genassem/x86)
(defmacro _adc16i16 (src) `(progn ,@'nil ,@'nil (emit-byte ,'21)))
(defmacro _adc32i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'21)))
(defmacro _adc64i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'21)))
(defmacro _adc8i8 (src) `(progn ,@'nil ,@'nil (emit-byte ,'20)))
(defmacro _add16i16 (src) `(progn ,@'nil ,@'nil (emit-byte ,'5)))
(defmacro _add32i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'5)))
(defmacro _add64i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'5)))
(defmacro _add8i8 (src) `(progn ,@'nil ,@'nil (emit-byte ,'4)))
(defmacro _and16i16 (src) `(progn ,@'nil ,@'nil (emit-byte ,'37)))
(defmacro _and32i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'37)))
(defmacro _and64i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'37)))
(defmacro _and8i8 (src) `(progn ,@'nil ,@'nil (emit-byte ,'36)))
(defmacro _bswap32r (src)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits src :expected-mode 32)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'((emit-byte 15))
      (emit-byte (logior ,'200 ,hu.dwim.genassem::reg-index)))))
(defmacro _bswap64r (src)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits src :expected-mode 64)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'((emit-byte 15))
      (emit-byte (logior ,'200 ,hu.dwim.genassem::reg-index)))))
(defmacro _call64pcrel32 (dst) `(progn ,@'nil ,@'nil (emit-byte ,'232)))
(defmacro _cbw () `(progn ,@'nil ,@'nil (emit-byte ,'152)))
(defmacro _cdq () `(progn ,@'nil ,@'nil (emit-byte ,'153)))
(defmacro _cdqe () `(progn ,@'nil ,@'nil (emit-byte ,'152)))
(defmacro _clc () `(progn ,@'nil ,@'nil (emit-byte ,'248)))
(defmacro _cld () `(progn ,@'nil ,@'nil (emit-byte ,'252)))
(defmacro _cli () `(progn ,@'nil ,@'nil (emit-byte ,'250)))
(defmacro _clts () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'6)))
(defmacro _cmc () `(progn ,@'nil ,@'nil (emit-byte ,'245)))
(defmacro _cmp16i16 (src) `(progn ,@'nil ,@'nil (emit-byte ,'61)))
(defmacro _cmp32i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'61)))
(defmacro _cmp64i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'61)))
(defmacro _cmp8i8 (src) `(progn ,@'nil ,@'nil (emit-byte ,'60)))
(defmacro _cpuid () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'162)))
(defmacro _cqo () `(progn ,@'nil ,@'nil (emit-byte ,'153)))
(defmacro _cwd () `(progn ,@'nil ,@'nil (emit-byte ,'153)))
(defmacro _cwde () `(progn ,@'nil ,@'nil (emit-byte ,'152)))
(defmacro _femms () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'14)))
(defmacro _getsec () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'55)))
(defmacro _hlt () `(progn ,@'nil ,@'nil (emit-byte ,'244)))
(defmacro _in16ri (port) `(progn ,@'nil ,@'nil (emit-byte ,'229)))
(defmacro _in16rr () `(progn ,@'nil ,@'nil (emit-byte ,'237)))
(defmacro _in32ri (port) `(progn ,@'nil ,@'nil (emit-byte ,'229)))
(defmacro _in32rr () `(progn ,@'nil ,@'nil (emit-byte ,'237)))
(defmacro _in8ri (port) `(progn ,@'nil ,@'nil (emit-byte ,'228)))
(defmacro _in8rr () `(progn ,@'nil ,@'nil (emit-byte ,'236)))
(defmacro _int (trap) `(progn ,@'nil ,@'nil (emit-byte ,'205)))
(defmacro _int3 () `(progn ,@'nil ,@'nil (emit-byte ,'204)))
(defmacro _invd () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'8)))
(defmacro _iret16 () `(progn ,@'nil ,@'nil (emit-byte ,'207)))
(defmacro _iret32 () `(progn ,@'nil ,@'nil (emit-byte ,'207)))
(defmacro _iret64 () `(progn ,@'nil ,@'nil (emit-byte ,'207)))
(defmacro _jecxz (dst) `(progn ,@'nil ,@'nil (emit-byte ,'227)))
(defmacro _jmp_1 (dst) `(progn ,@'nil ,@'nil (emit-byte ,'235)))
(defmacro _jrcxz (dst) `(progn ,@'nil ,@'nil (emit-byte ,'227)))
(defmacro _lahf () `(progn ,@'nil ,@'nil (emit-byte ,'159)))
(defmacro _leave64 () `(progn ,@'nil ,@'nil (emit-byte ,'201)))
(defmacro _loop (dst) `(progn ,@'nil ,@'nil (emit-byte ,'226)))
(defmacro _loope (dst) `(progn ,@'nil ,@'nil (emit-byte ,'225)))
(defmacro _loopne (dst) `(progn ,@'nil ,@'nil (emit-byte ,'224)))
(defmacro _lret16 () `(progn ,@'nil ,@'nil (emit-byte ,'203)))
(defmacro _lret32 () `(progn ,@'nil ,@'nil (emit-byte ,'203)))
(defmacro _lret64 () `(progn ,@'nil ,@'nil (emit-byte ,'203)))
(defmacro _lreti16 (amt) `(progn ,@'nil ,@'nil (emit-byte ,'202)))
(defmacro _lreti32 (amt) `(progn ,@'nil ,@'nil (emit-byte ,'202)))
(defmacro _lreti64 (amt) `(progn ,@'nil ,@'nil (emit-byte ,'202)))
(defmacro _mmx_emms () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'119)))
(defmacro _mov16ri (dst src)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits dst :expected-mode 16)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'184 ,hu.dwim.genassem::reg-index)))))
(defmacro _mov32ri (dst src)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits dst :expected-mode 32)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'184 ,hu.dwim.genassem::reg-index)))))
(defmacro _mov64ri (dst src)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits dst :expected-mode 64)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'184 ,hu.dwim.genassem::reg-index)))))
(defmacro _mov8ri (dst src)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits dst :expected-mode 8)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'176 ,hu.dwim.genassem::reg-index)))))
(defmacro _noop () `(progn ,@'nil ,@'nil (emit-byte ,'144)))
(defmacro _or16i16 (src) `(progn ,@'nil ,@'nil (emit-byte ,'13)))
(defmacro _or32i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'13)))
(defmacro _or64i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'13)))
(defmacro _or8i8 (src) `(progn ,@'nil ,@'nil (emit-byte ,'12)))
(defmacro _out16ir (port) `(progn ,@'nil ,@'nil (emit-byte ,'231)))
(defmacro _out16rr () `(progn ,@'nil ,@'nil (emit-byte ,'239)))
(defmacro _out32ir (port) `(progn ,@'nil ,@'nil (emit-byte ,'231)))
(defmacro _out32rr () `(progn ,@'nil ,@'nil (emit-byte ,'239)))
(defmacro _out8ir (port) `(progn ,@'nil ,@'nil (emit-byte ,'230)))
(defmacro _out8rr () `(progn ,@'nil ,@'nil (emit-byte ,'238)))
(defmacro _pause () `(progn ,@'((emit-byte 243)) ,@'nil (emit-byte ,'144)))
(defmacro _pop16r (reg)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits reg :expected-mode 16)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'88 ,hu.dwim.genassem::reg-index)))))
(defmacro _pop64r (reg)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits reg :expected-mode 64)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'88 ,hu.dwim.genassem::reg-index)))))
(defmacro _popf16 () `(progn ,@'nil ,@'nil (emit-byte ,'157)))
(defmacro _popf64 () `(progn ,@'nil ,@'nil (emit-byte ,'157)))
(defmacro _popfs16 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'161)))
(defmacro _popfs64 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'161)))
(defmacro _popgs16 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'169)))
(defmacro _popgs64 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'169)))
(defmacro _push16i (imm) `(progn ,@'nil ,@'nil (emit-byte ,'104)))
(defmacro _push16i8 (imm) `(progn ,@'nil ,@'nil (emit-byte ,'106)))
(defmacro _push16r (reg)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits reg :expected-mode 16)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'80 ,hu.dwim.genassem::reg-index)))))
(defmacro _push64i32 (imm) `(progn ,@'nil ,@'nil (emit-byte ,'104)))
(defmacro _push64i8 (imm) `(progn ,@'nil ,@'nil (emit-byte ,'106)))
(defmacro _push64r (reg)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits reg :expected-mode 64)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'80 ,hu.dwim.genassem::reg-index)))))
(defmacro _pushf16 () `(progn ,@'nil ,@'nil (emit-byte ,'156)))
(defmacro _pushf64 () `(progn ,@'nil ,@'nil (emit-byte ,'156)))
(defmacro _pushfs16 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'160)))
(defmacro _pushfs64 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'160)))
(defmacro _pushgs16 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'168)))
(defmacro _pushgs64 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'168)))
(defmacro _rdmsr () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'50)))
(defmacro _rdpmc () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'51)))
(defmacro _rdtsc () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'49)))
; ((!anonymous) (!fields array) (!locs array X86InstrControl.td:27) (!name . RET16) (!superclasses array InstructionEncoding Instruction X86Inst I OpSize16) (AdSize object (def . AdSizeX) (kind . def) (printable . AdSizeX)) (AdSizeBits array 0 0) (AddedComplexity . 0) (AsmMatchConverter . ) (AsmString . ret{w}) (AsmVariantName . ) (CD8_EltSize . 0) (CD8_Form array 0 0 0) (CD8_Scale array 0 0 0 0 0 0 0) (CodeSize . 0) (Constraints . ) (DecoderMethod . ) (DecoderNamespace . ) (Defs array) (DisableEncoding . ) (EncodingInfos . null) (ExeDomain object (def . GenericDomain) (kind . def) (printable . GenericDomain)) (FPForm object (def . SpecialFP) (kind . def) (printable . SpecialFP)) (FastISelShouldIgnore . 0) (ForceDisassemble . 0) (Form object (def . RawFrm) (kind . def) (printable . RawFrm)) (FormBits array 1 0 0 0 0 0 0) (HasPositionOrder . 1) (IgnoresW . 0) (ImmT object (def . NoImm) (kind . def) (printable . NoImm)) (InOperandList object (args array) (kind . dag) (operator object (def . ins) (kind . def) (printable . ins)) (printable . (ins))) (Itinerary object (def . NoItinerary) (kind . def) (printable . NoItinerary)) (Namespace . X86) (OpEnc object (def . EncNormal) (kind . def) (printable . EncNormal)) (OpEncBits array 0 0) (OpMap object (def . OB) (kind . def) (printable . OB)) (OpMapBits array 0 0 0 0) (OpPrefix object (def . NoPrfx) (kind . def) (printable . NoPrfx)) (OpPrefixBits array 0 0 0) (OpSize object (def . OpSize16) (kind . def) (printable . OpSize16)) (OpSizeBits array 1 0) (Opcode array 1 1 0 0 0 0 1 1) (OutOperandList object (args array) (kind . dag) (operator object (def . outs) (kind . def) (printable . outs)) (printable . (outs))) (Pattern array) (PostEncoderMethod . ) (Predicates array) (SchedRW array (object (def . WriteJumpLd) (kind . def) (printable . WriteJumpLd))) (Size . 0) (TSFlags array 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (TwoOperandAliasConstraint . ) (UseLogicalOperandMappings . 0) (UseNamedOperandTable . 0) (Uses array) (VectSize array 0 0 0 0 1 0 0) (canFoldAsLoad . 0) (explicitOpPrefix object (def . NoExplicitOpPrefix) (kind . def) (printable . NoExplicitOpPrefix)) (explicitOpPrefixBits array 0 0) (hasCompleteDecoder . 1) (hasCtrlDep . 1) (hasDelaySlot . 0) (hasEVEX_B . 0) (hasEVEX_K . 0) (hasEVEX_L2 . 0) (hasEVEX_NF . 0) (hasEVEX_RC . 0) (hasEVEX_U . 0) (hasEVEX_Z . 0) (hasExtraDefRegAllocReq . 0) (hasExtraSrcRegAllocReq . 0) (hasLockPrefix . 0) (hasNoSchedulingInfo . 0) (hasNoTrackPrefix . 0) (hasPostISelHook . 0) (hasREPPrefix . 0) (hasREX_W . 0) (hasSideEffects . null) (hasTwoConditionalOps . 0) (hasVEX_4V . 0) (hasVEX_L . 0) (ignoresVEX_L . 0) (isAdd . 0) (isAsCheapAsAMove . 0) (isAsmParserOnly . 0) (isAuthenticated . 0) (isBarrier . 1) (isBitcast . 0) (isBranch . 0) (isCall . 0) (isCodeGenOnly . 0) (isCommutable . 0) (isCompare . 0) (isConvergent . 0) (isConvertibleToThreeAddress . 0) (isEHScopeReturn . 0) (isExtractSubreg . 0) (isIndirectBranch . 0) (isInsertSubreg . 0) (isMeta . 0) (isMoveImm . 0) (isMoveReg . 0) (isNotDuplicable . 0) (isPreISelOpcode . 0) (isPredicable . 0) (isPseudo . 0) (isReMaterializable . 0) (isRegSequence . 0) (isReturn . 1) (isSelect . 0) (isTerminator . 1) (isTrap . 0) (isUnpredicable . 0) (mayLoad . null) (mayRaiseFPException . 0) (mayStore . null) (usesCustomInserter . 0) (variadicOpsAreDefs . 0))
(defmacro _ret16 () `(progn ,@'nil ,@'nil (emit-byte ,'195)))
; ((!anonymous) (!fields array) (!locs array X86InstrControl.td:23) (!name . RET32) (!superclasses array InstructionEncoding Instruction X86Inst I OpSize32 Requires) (AdSize object (def . AdSizeX) (kind . def) (printable . AdSizeX)) (AdSizeBits array 0 0) (AddedComplexity . 0) (AsmMatchConverter . ) (AsmString . ret{l}) (AsmVariantName . ) (CD8_EltSize . 0) (CD8_Form array 0 0 0) (CD8_Scale array 0 0 0 0 0 0 0) (CodeSize . 0) (Constraints . ) (DecoderMethod . ) (DecoderNamespace . ) (Defs array) (DisableEncoding . ) (EncodingInfos . null) (ExeDomain object (def . GenericDomain) (kind . def) (printable . GenericDomain)) (FPForm object (def . SpecialFP) (kind . def) (printable . SpecialFP)) (FastISelShouldIgnore . 0) (ForceDisassemble . 0) (Form object (def . RawFrm) (kind . def) (printable . RawFrm)) (FormBits array 1 0 0 0 0 0 0) (HasPositionOrder . 1) (IgnoresW . 0) (ImmT object (def . NoImm) (kind . def) (printable . NoImm)) (InOperandList object (args array (array (object (def . variable_ops) (kind . def) (printable . variable_ops)) null)) (kind . dag) (operator object (def . ins) (kind . def) (printable . ins)) (printable . (ins variable_ops))) (Itinerary object (def . NoItinerary) (kind . def) (printable . NoItinerary)) (Namespace . X86) (OpEnc object (def . EncNormal) (kind . def) (printable . EncNormal)) (OpEncBits array 0 0) (OpMap object (def . OB) (kind . def) (printable . OB)) (OpMapBits array 0 0 0 0) (OpPrefix object (def . NoPrfx) (kind . def) (printable . NoPrfx)) (OpPrefixBits array 0 0 0) (OpSize object (def . OpSize32) (kind . def) (printable . OpSize32)) (OpSizeBits array 0 1) (Opcode array 1 1 0 0 0 0 1 1) (OutOperandList object (args array) (kind . dag) (operator object (def . outs) (kind . def) (printable . outs)) (printable . (outs))) (Pattern array) (PostEncoderMethod . ) (Predicates array (object (def . Not64BitMode) (kind . def) (printable . Not64BitMode))) (SchedRW array (object (def . WriteJumpLd) (kind . def) (printable . WriteJumpLd))) (Size . 0) (TSFlags array 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (TwoOperandAliasConstraint . ) (UseLogicalOperandMappings . 0) (UseNamedOperandTable . 0) (Uses array) (VectSize array 0 0 0 0 1 0 0) (canFoldAsLoad . 0) (explicitOpPrefix object (def . NoExplicitOpPrefix) (kind . def) (printable . NoExplicitOpPrefix)) (explicitOpPrefixBits array 0 0) (hasCompleteDecoder . 1) (hasCtrlDep . 1) (hasDelaySlot . 0) (hasEVEX_B . 0) (hasEVEX_K . 0) (hasEVEX_L2 . 0) (hasEVEX_NF . 0) (hasEVEX_RC . 0) (hasEVEX_U . 0) (hasEVEX_Z . 0) (hasExtraDefRegAllocReq . 0) (hasExtraSrcRegAllocReq . 0) (hasLockPrefix . 0) (hasNoSchedulingInfo . 0) (hasNoTrackPrefix . 0) (hasPostISelHook . 0) (hasREPPrefix . 0) (hasREX_W . 0) (hasSideEffects . null) (hasTwoConditionalOps . 0) (hasVEX_4V . 0) (hasVEX_L . 0) (ignoresVEX_L . 0) (isAdd . 0) (isAsCheapAsAMove . 0) (isAsmParserOnly . 0) (isAuthenticated . 0) (isBarrier . 1) (isBitcast . 0) (isBranch . 0) (isCall . 0) (isCodeGenOnly . 0) (isCommutable . 0) (isCompare . 0) (isConvergent . 0) (isConvertibleToThreeAddress . 0) (isEHScopeReturn . 0) (isExtractSubreg . 0) (isIndirectBranch . 0) (isInsertSubreg . 0) (isMeta . 0) (isMoveImm . 0) (isMoveReg . 0) (isNotDuplicable . 0) (isPreISelOpcode . 0) (isPredicable . 0) (isPseudo . 0) (isReMaterializable . 0) (isRegSequence . 0) (isReturn . 1) (isSelect . 0) (isTerminator . 1) (isTrap . 0) (isUnpredicable . 0) (mayLoad . null) (mayRaiseFPException . 0) (mayStore . null) (usesCustomInserter . 0) (variadicOpsAreDefs . 0))
; ((!anonymous) (!fields array) (!locs array X86InstrControl.td:25) (!name . RET64) (!superclasses array InstructionEncoding Instruction X86Inst I OpSize32 Requires) (AdSize object (def . AdSizeX) (kind . def) (printable . AdSizeX)) (AdSizeBits array 0 0) (AddedComplexity . 0) (AsmMatchConverter . ) (AsmString . ret{q}) (AsmVariantName . ) (CD8_EltSize . 0) (CD8_Form array 0 0 0) (CD8_Scale array 0 0 0 0 0 0 0) (CodeSize . 0) (Constraints . ) (DecoderMethod . ) (DecoderNamespace . ) (Defs array) (DisableEncoding . ) (EncodingInfos . null) (ExeDomain object (def . GenericDomain) (kind . def) (printable . GenericDomain)) (FPForm object (def . SpecialFP) (kind . def) (printable . SpecialFP)) (FastISelShouldIgnore . 0) (ForceDisassemble . 0) (Form object (def . RawFrm) (kind . def) (printable . RawFrm)) (FormBits array 1 0 0 0 0 0 0) (HasPositionOrder . 1) (IgnoresW . 0) (ImmT object (def . NoImm) (kind . def) (printable . NoImm)) (InOperandList object (args array (array (object (def . variable_ops) (kind . def) (printable . variable_ops)) null)) (kind . dag) (operator object (def . ins) (kind . def) (printable . ins)) (printable . (ins variable_ops))) (Itinerary object (def . NoItinerary) (kind . def) (printable . NoItinerary)) (Namespace . X86) (OpEnc object (def . EncNormal) (kind . def) (printable . EncNormal)) (OpEncBits array 0 0) (OpMap object (def . OB) (kind . def) (printable . OB)) (OpMapBits array 0 0 0 0) (OpPrefix object (def . NoPrfx) (kind . def) (printable . NoPrfx)) (OpPrefixBits array 0 0 0) (OpSize object (def . OpSize32) (kind . def) (printable . OpSize32)) (OpSizeBits array 0 1) (Opcode array 1 1 0 0 0 0 1 1) (OutOperandList object (args array) (kind . dag) (operator object (def . outs) (kind . def) (printable . outs)) (printable . (outs))) (Pattern array) (PostEncoderMethod . ) (Predicates array (object (def . In64BitMode) (kind . def) (printable . In64BitMode))) (SchedRW array (object (def . WriteJumpLd) (kind . def) (printable . WriteJumpLd))) (Size . 0) (TSFlags array 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (TwoOperandAliasConstraint . ) (UseLogicalOperandMappings . 0) (UseNamedOperandTable . 0) (Uses array) (VectSize array 0 0 0 0 1 0 0) (canFoldAsLoad . 0) (explicitOpPrefix object (def . NoExplicitOpPrefix) (kind . def) (printable . NoExplicitOpPrefix)) (explicitOpPrefixBits array 0 0) (hasCompleteDecoder . 1) (hasCtrlDep . 1) (hasDelaySlot . 0) (hasEVEX_B . 0) (hasEVEX_K . 0) (hasEVEX_L2 . 0) (hasEVEX_NF . 0) (hasEVEX_RC . 0) (hasEVEX_U . 0) (hasEVEX_Z . 0) (hasExtraDefRegAllocReq . 0) (hasExtraSrcRegAllocReq . 0) (hasLockPrefix . 0) (hasNoSchedulingInfo . 0) (hasNoTrackPrefix . 0) (hasPostISelHook . 0) (hasREPPrefix . 0) (hasREX_W . 0) (hasSideEffects . null) (hasTwoConditionalOps . 0) (hasVEX_4V . 0) (hasVEX_L . 0) (ignoresVEX_L . 0) (isAdd . 0) (isAsCheapAsAMove . 0) (isAsmParserOnly . 0) (isAuthenticated . 0) (isBarrier . 1) (isBitcast . 0) (isBranch . 0) (isCall . 0) (isCodeGenOnly . 0) (isCommutable . 0) (isCompare . 0) (isConvergent . 0) (isConvertibleToThreeAddress . 0) (isEHScopeReturn . 0) (isExtractSubreg . 0) (isIndirectBranch . 0) (isInsertSubreg . 0) (isMeta . 0) (isMoveImm . 0) (isMoveReg . 0) (isNotDuplicable . 0) (isPreISelOpcode . 0) (isPredicable . 0) (isPseudo . 0) (isReMaterializable . 0) (isRegSequence . 0) (isReturn . 1) (isSelect . 0) (isTerminator . 1) (isTrap . 0) (isUnpredicable . 0) (mayLoad . null) (mayRaiseFPException . 0) (mayStore . null) (usesCustomInserter . 0) (variadicOpsAreDefs . 0))
(defmacro _ret64 () `(progn ,@'nil ,@'nil (emit-byte ,'195)))
; ((!anonymous) (!fields array) (!locs array X86InstrControl.td:33) (!name . RETI16) (!superclasses array InstructionEncoding Instruction X86Inst Ii16 OpSize16) (AdSize object (def . AdSizeX) (kind . def) (printable . AdSizeX)) (AdSizeBits array 0 0) (AddedComplexity . 0) (AsmMatchConverter . ) (AsmString . ret{w}	$amt) (AsmVariantName . ) (CD8_EltSize . 0) (CD8_Form array 0 0 0) (CD8_Scale array 0 0 0 0 0 0 0) (CodeSize . 0) (Constraints . ) (DecoderMethod . ) (DecoderNamespace . ) (Defs array) (DisableEncoding . ) (EncodingInfos . null) (ExeDomain object (def . GenericDomain) (kind . def) (printable . GenericDomain)) (FPForm object (def . SpecialFP) (kind . def) (printable . SpecialFP)) (FastISelShouldIgnore . 0) (ForceDisassemble . 0) (Form object (def . RawFrm) (kind . def) (printable . RawFrm)) (FormBits array 1 0 0 0 0 0 0) (HasPositionOrder . 1) (IgnoresW . 0) (ImmT object (def . Imm16) (kind . def) (printable . Imm16)) (InOperandList object (args array (array (object (def . i16imm) (kind . def) (printable . i16imm)) amt)) (kind . dag) (operator object (def . ins) (kind . def) (printable . ins)) (printable . (ins i16imm:$amt))) (Itinerary object (def . NoItinerary) (kind . def) (printable . NoItinerary)) (Namespace . X86) (OpEnc object (def . EncNormal) (kind . def) (printable . EncNormal)) (OpEncBits array 0 0) (OpMap object (def . OB) (kind . def) (printable . OB)) (OpMapBits array 0 0 0 0) (OpPrefix object (def . NoPrfx) (kind . def) (printable . NoPrfx)) (OpPrefixBits array 0 0 0) (OpSize object (def . OpSize16) (kind . def) (printable . OpSize16)) (OpSizeBits array 1 0) (Opcode array 0 1 0 0 0 0 1 1) (OutOperandList object (args array) (kind . dag) (operator object (def . outs) (kind . def) (printable . outs)) (printable . (outs))) (Pattern array) (PostEncoderMethod . ) (Predicates array) (SchedRW array (object (def . WriteJumpLd) (kind . def) (printable . WriteJumpLd))) (Size . 0) (TSFlags array 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (TwoOperandAliasConstraint . ) (UseLogicalOperandMappings . 0) (UseNamedOperandTable . 0) (Uses array) (VectSize array 0 0 0 0 1 0 0) (canFoldAsLoad . 0) (explicitOpPrefix object (def . NoExplicitOpPrefix) (kind . def) (printable . NoExplicitOpPrefix)) (explicitOpPrefixBits array 0 0) (hasCompleteDecoder . 1) (hasCtrlDep . 1) (hasDelaySlot . 0) (hasEVEX_B . 0) (hasEVEX_K . 0) (hasEVEX_L2 . 0) (hasEVEX_NF . 0) (hasEVEX_RC . 0) (hasEVEX_U . 0) (hasEVEX_Z . 0) (hasExtraDefRegAllocReq . 0) (hasExtraSrcRegAllocReq . 0) (hasLockPrefix . 0) (hasNoSchedulingInfo . 0) (hasNoTrackPrefix . 0) (hasPostISelHook . 0) (hasREPPrefix . 0) (hasREX_W . 0) (hasSideEffects . null) (hasTwoConditionalOps . 0) (hasVEX_4V . 0) (hasVEX_L . 0) (ignoresVEX_L . 0) (isAdd . 0) (isAsCheapAsAMove . 0) (isAsmParserOnly . 0) (isAuthenticated . 0) (isBarrier . 1) (isBitcast . 0) (isBranch . 0) (isCall . 0) (isCodeGenOnly . 0) (isCommutable . 0) (isCompare . 0) (isConvergent . 0) (isConvertibleToThreeAddress . 0) (isEHScopeReturn . 0) (isExtractSubreg . 0) (isIndirectBranch . 0) (isInsertSubreg . 0) (isMeta . 0) (isMoveImm . 0) (isMoveReg . 0) (isNotDuplicable . 0) (isPreISelOpcode . 0) (isPredicable . 0) (isPseudo . 0) (isReMaterializable . 0) (isRegSequence . 0) (isReturn . 1) (isSelect . 0) (isTerminator . 1) (isTrap . 0) (isUnpredicable . 0) (mayLoad . null) (mayRaiseFPException . 0) (mayStore . null) (usesCustomInserter . 0) (variadicOpsAreDefs . 0))
(defmacro _reti16 (amt) `(progn ,@'nil ,@'nil (emit-byte ,'194)))
; ((!anonymous) (!fields array) (!locs array X86InstrControl.td:29) (!name . RETI32) (!superclasses array InstructionEncoding Instruction X86Inst Ii16 OpSize32 Requires) (AdSize object (def . AdSizeX) (kind . def) (printable . AdSizeX)) (AdSizeBits array 0 0) (AddedComplexity . 0) (AsmMatchConverter . ) (AsmString . ret{l}	$amt) (AsmVariantName . ) (CD8_EltSize . 0) (CD8_Form array 0 0 0) (CD8_Scale array 0 0 0 0 0 0 0) (CodeSize . 0) (Constraints . ) (DecoderMethod . ) (DecoderNamespace . ) (Defs array) (DisableEncoding . ) (EncodingInfos . null) (ExeDomain object (def . GenericDomain) (kind . def) (printable . GenericDomain)) (FPForm object (def . SpecialFP) (kind . def) (printable . SpecialFP)) (FastISelShouldIgnore . 0) (ForceDisassemble . 0) (Form object (def . RawFrm) (kind . def) (printable . RawFrm)) (FormBits array 1 0 0 0 0 0 0) (HasPositionOrder . 1) (IgnoresW . 0) (ImmT object (def . Imm16) (kind . def) (printable . Imm16)) (InOperandList object (args array (array (object (def . i16imm) (kind . def) (printable . i16imm)) amt) (array (object (def . variable_ops) (kind . def) (printable . variable_ops)) null)) (kind . dag) (operator object (def . ins) (kind . def) (printable . ins)) (printable . (ins i16imm:$amt, variable_ops))) (Itinerary object (def . NoItinerary) (kind . def) (printable . NoItinerary)) (Namespace . X86) (OpEnc object (def . EncNormal) (kind . def) (printable . EncNormal)) (OpEncBits array 0 0) (OpMap object (def . OB) (kind . def) (printable . OB)) (OpMapBits array 0 0 0 0) (OpPrefix object (def . NoPrfx) (kind . def) (printable . NoPrfx)) (OpPrefixBits array 0 0 0) (OpSize object (def . OpSize32) (kind . def) (printable . OpSize32)) (OpSizeBits array 0 1) (Opcode array 0 1 0 0 0 0 1 1) (OutOperandList object (args array) (kind . dag) (operator object (def . outs) (kind . def) (printable . outs)) (printable . (outs))) (Pattern array) (PostEncoderMethod . ) (Predicates array (object (def . Not64BitMode) (kind . def) (printable . Not64BitMode))) (SchedRW array (object (def . WriteJumpLd) (kind . def) (printable . WriteJumpLd))) (Size . 0) (TSFlags array 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (TwoOperandAliasConstraint . ) (UseLogicalOperandMappings . 0) (UseNamedOperandTable . 0) (Uses array) (VectSize array 0 0 0 0 1 0 0) (canFoldAsLoad . 0) (explicitOpPrefix object (def . NoExplicitOpPrefix) (kind . def) (printable . NoExplicitOpPrefix)) (explicitOpPrefixBits array 0 0) (hasCompleteDecoder . 1) (hasCtrlDep . 1) (hasDelaySlot . 0) (hasEVEX_B . 0) (hasEVEX_K . 0) (hasEVEX_L2 . 0) (hasEVEX_NF . 0) (hasEVEX_RC . 0) (hasEVEX_U . 0) (hasEVEX_Z . 0) (hasExtraDefRegAllocReq . 0) (hasExtraSrcRegAllocReq . 0) (hasLockPrefix . 0) (hasNoSchedulingInfo . 0) (hasNoTrackPrefix . 0) (hasPostISelHook . 0) (hasREPPrefix . 0) (hasREX_W . 0) (hasSideEffects . null) (hasTwoConditionalOps . 0) (hasVEX_4V . 0) (hasVEX_L . 0) (ignoresVEX_L . 0) (isAdd . 0) (isAsCheapAsAMove . 0) (isAsmParserOnly . 0) (isAuthenticated . 0) (isBarrier . 1) (isBitcast . 0) (isBranch . 0) (isCall . 0) (isCodeGenOnly . 0) (isCommutable . 0) (isCompare . 0) (isConvergent . 0) (isConvertibleToThreeAddress . 0) (isEHScopeReturn . 0) (isExtractSubreg . 0) (isIndirectBranch . 0) (isInsertSubreg . 0) (isMeta . 0) (isMoveImm . 0) (isMoveReg . 0) (isNotDuplicable . 0) (isPreISelOpcode . 0) (isPredicable . 0) (isPseudo . 0) (isReMaterializable . 0) (isRegSequence . 0) (isReturn . 1) (isSelect . 0) (isTerminator . 1) (isTrap . 0) (isUnpredicable . 0) (mayLoad . null) (mayRaiseFPException . 0) (mayStore . null) (usesCustomInserter . 0) (variadicOpsAreDefs . 0))
; ((!anonymous) (!fields array) (!locs array X86InstrControl.td:31) (!name . RETI64) (!superclasses array InstructionEncoding Instruction X86Inst Ii16 OpSize32 Requires) (AdSize object (def . AdSizeX) (kind . def) (printable . AdSizeX)) (AdSizeBits array 0 0) (AddedComplexity . 0) (AsmMatchConverter . ) (AsmString . ret{q}	$amt) (AsmVariantName . ) (CD8_EltSize . 0) (CD8_Form array 0 0 0) (CD8_Scale array 0 0 0 0 0 0 0) (CodeSize . 0) (Constraints . ) (DecoderMethod . ) (DecoderNamespace . ) (Defs array) (DisableEncoding . ) (EncodingInfos . null) (ExeDomain object (def . GenericDomain) (kind . def) (printable . GenericDomain)) (FPForm object (def . SpecialFP) (kind . def) (printable . SpecialFP)) (FastISelShouldIgnore . 0) (ForceDisassemble . 0) (Form object (def . RawFrm) (kind . def) (printable . RawFrm)) (FormBits array 1 0 0 0 0 0 0) (HasPositionOrder . 1) (IgnoresW . 0) (ImmT object (def . Imm16) (kind . def) (printable . Imm16)) (InOperandList object (args array (array (object (def . i16imm) (kind . def) (printable . i16imm)) amt) (array (object (def . variable_ops) (kind . def) (printable . variable_ops)) null)) (kind . dag) (operator object (def . ins) (kind . def) (printable . ins)) (printable . (ins i16imm:$amt, variable_ops))) (Itinerary object (def . NoItinerary) (kind . def) (printable . NoItinerary)) (Namespace . X86) (OpEnc object (def . EncNormal) (kind . def) (printable . EncNormal)) (OpEncBits array 0 0) (OpMap object (def . OB) (kind . def) (printable . OB)) (OpMapBits array 0 0 0 0) (OpPrefix object (def . NoPrfx) (kind . def) (printable . NoPrfx)) (OpPrefixBits array 0 0 0) (OpSize object (def . OpSize32) (kind . def) (printable . OpSize32)) (OpSizeBits array 0 1) (Opcode array 0 1 0 0 0 0 1 1) (OutOperandList object (args array) (kind . dag) (operator object (def . outs) (kind . def) (printable . outs)) (printable . (outs))) (Pattern array) (PostEncoderMethod . ) (Predicates array (object (def . In64BitMode) (kind . def) (printable . In64BitMode))) (SchedRW array (object (def . WriteJumpLd) (kind . def) (printable . WriteJumpLd))) (Size . 0) (TSFlags array 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (TwoOperandAliasConstraint . ) (UseLogicalOperandMappings . 0) (UseNamedOperandTable . 0) (Uses array) (VectSize array 0 0 0 0 1 0 0) (canFoldAsLoad . 0) (explicitOpPrefix object (def . NoExplicitOpPrefix) (kind . def) (printable . NoExplicitOpPrefix)) (explicitOpPrefixBits array 0 0) (hasCompleteDecoder . 1) (hasCtrlDep . 1) (hasDelaySlot . 0) (hasEVEX_B . 0) (hasEVEX_K . 0) (hasEVEX_L2 . 0) (hasEVEX_NF . 0) (hasEVEX_RC . 0) (hasEVEX_U . 0) (hasEVEX_Z . 0) (hasExtraDefRegAllocReq . 0) (hasExtraSrcRegAllocReq . 0) (hasLockPrefix . 0) (hasNoSchedulingInfo . 0) (hasNoTrackPrefix . 0) (hasPostISelHook . 0) (hasREPPrefix . 0) (hasREX_W . 0) (hasSideEffects . null) (hasTwoConditionalOps . 0) (hasVEX_4V . 0) (hasVEX_L . 0) (ignoresVEX_L . 0) (isAdd . 0) (isAsCheapAsAMove . 0) (isAsmParserOnly . 0) (isAuthenticated . 0) (isBarrier . 1) (isBitcast . 0) (isBranch . 0) (isCall . 0) (isCodeGenOnly . 0) (isCommutable . 0) (isCompare . 0) (isConvergent . 0) (isConvertibleToThreeAddress . 0) (isEHScopeReturn . 0) (isExtractSubreg . 0) (isIndirectBranch . 0) (isInsertSubreg . 0) (isMeta . 0) (isMoveImm . 0) (isMoveReg . 0) (isNotDuplicable . 0) (isPreISelOpcode . 0) (isPredicable . 0) (isPseudo . 0) (isReMaterializable . 0) (isRegSequence . 0) (isReturn . 1) (isSelect . 0) (isTerminator . 1) (isTrap . 0) (isUnpredicable . 0) (mayLoad . null) (mayRaiseFPException . 0) (mayStore . null) (usesCustomInserter . 0) (variadicOpsAreDefs . 0))
(defmacro _reti64 (amt) `(progn ,@'nil ,@'nil (emit-byte ,'194)))
(defmacro _rsm () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'170)))
(defmacro _sahf () `(progn ,@'nil ,@'nil (emit-byte ,'158)))
(defmacro _sbb16i16 (src) `(progn ,@'nil ,@'nil (emit-byte ,'29)))
(defmacro _sbb32i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'29)))
(defmacro _sbb64i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'29)))
(defmacro _sbb8i8 (src) `(progn ,@'nil ,@'nil (emit-byte ,'28)))
(defmacro _stc () `(progn ,@'nil ,@'nil (emit-byte ,'249)))
(defmacro _std () `(progn ,@'nil ,@'nil (emit-byte ,'253)))
(defmacro _sti () `(progn ,@'nil ,@'nil (emit-byte ,'251)))
(defmacro _sub16i16 (src) `(progn ,@'nil ,@'nil (emit-byte ,'45)))
(defmacro _sub32i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'45)))
(defmacro _sub64i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'45)))
(defmacro _sub8i8 (src) `(progn ,@'nil ,@'nil (emit-byte ,'44)))
(defmacro _syscall () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'5)))
(defmacro _sysenter () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'52)))
(defmacro _sysexit () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'53)))
(defmacro _sysexit64 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'53)))
(defmacro _sysret () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'7)))
(defmacro _sysret64 () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'7)))
(defmacro _test16i16 (src) `(progn ,@'nil ,@'nil (emit-byte ,'169)))
(defmacro _test32i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'169)))
(defmacro _test64i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'169)))
(defmacro _test8i8 (src) `(progn ,@'nil ,@'nil (emit-byte ,'168)))
(defmacro _trap () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'11)))
(defmacro _wait () `(progn ,@'nil ,@'nil (emit-byte ,'155)))
(defmacro _wbinvd ()
  `(progn ,@'((emit-byte 243)) ,@'((emit-byte 15)) (emit-byte ,'9)))
(defmacro _wrmsr () `(progn ,@'nil ,@'((emit-byte 15)) (emit-byte ,'48)))
(defmacro _xchg16ar (src)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits src :expected-mode 16)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'144 ,hu.dwim.genassem::reg-index)))))
(defmacro _xchg32ar (src)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits src :expected-mode 32)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'144 ,hu.dwim.genassem::reg-index)))))
(defmacro _xchg64ar (src)
  (multiple-value-bind
      (hu.dwim.genassem::reg-index hu.dwim.genassem::reg-mode
       hu.dwim.genassem::reg-extra-bit)
      (register-name->encoding-bits src :expected-mode 64)
    `(progn
      ,@'nil
      (when (eql ,hu.dwim.genassem::reg-mode 64)
        (emit-byte
         (logior 72
                 ,(if hu.dwim.genassem::reg-extra-bit
                      hu.dwim.genassem::rex.b
                      0))))
      ,@'nil
      (emit-byte (logior ,'144 ,hu.dwim.genassem::reg-index)))))
(defmacro _xlat () `(progn ,@'nil ,@'nil (emit-byte ,'215)))
(defmacro _xor16i16 (src) `(progn ,@'nil ,@'nil (emit-byte ,'53)))
(defmacro _xor32i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'53)))
(defmacro _xor64i32 (src) `(progn ,@'nil ,@'nil (emit-byte ,'53)))
(defmacro _xor8i8 (src) `(progn ,@'nil ,@'nil (emit-byte ,'52)))
(export
 '(_xor8i8 _xor64i32 _xor32i32 _xor16i16 _xlat _xchg64ar _xchg32ar _xchg16ar
           _wrmsr _wbinvd _wait _trap _test8i8 _test64i32 _test32i32 _test16i16
           _sysret64 _sysret _sysexit64 _sysexit _sysenter _syscall _sub8i8
           _sub64i32 _sub32i32 _sub16i16 _sti _std _stc _sbb8i8 _sbb64i32
           _sbb32i32 _sbb16i16 _sahf _rsm _reti64 _reti16 _ret64 _ret16 _rdtsc
           _rdpmc _rdmsr _pushgs64 _pushgs16 _pushfs64 _pushfs16 _pushf64
           _pushf16 _push64r _push64i8 _push64i32 _push16r _push16i8 _push16i
           _popgs64 _popgs16 _popfs64 _popfs16 _popf64 _popf16 _pop64r _pop16r
           _pause _out8rr _out8ir _out32rr _out32ir _out16rr _out16ir _or8i8
           _or64i32 _or32i32 _or16i16 _noop _mov8ri _mov64ri _mov32ri _mov16ri
           _mmx_emms _lreti64 _lreti32 _lreti16 _lret64 _lret32 _lret16 _loopne
           _loope _loop _leave64 _lahf _jrcxz _jmp_1 _jecxz _iret64 _iret32
           _iret16 _invd _int3 _int _in8rr _in8ri _in32rr _in32ri _in16rr
           _in16ri _hlt _getsec _femms _cwde _cwd _cqo _cpuid _cmp8i8 _cmp64i32
           _cmp32i32 _cmp16i16 _cmc _clts _cli _cld _clc _cdqe _cdq _cbw
           _call64pcrel32 _bswap64r _bswap32r _and8i8 _and64i32 _and32i32
           _and16i16 _add8i8 _add64i32 _add32i32 _add16i16 _adc8i8 _adc64i32
           _adc32i32 _adc16i16))