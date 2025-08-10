(in-package :hu.dwim.tblgen-to-assembler)

;;;; Grammar based on LLVM TableGen Programmer's Reference.
;;;; See: https://llvm.org/docs/TableGen/ProgRef.html

;;; ---------------------------
;;; Whitespace & Comments
;;; ---------------------------

;; Single whitespace character (space, tab, newline, return, formfeed)
(defrule ws-char
  (or " " "\t" #\Newline #\Return #\Page)
  (:constant nil))

;; ;; Forward declare block-comment to support nesting
;; (declare (special *block-comment*))
;; (defparameter *block-comment* nil)

(defrule line-comment
  ;; '//' until newline or EOF (newline optional at EOF)
  (and "//"
       (* (not (or #\Newline #\Return)))
       (? (or #\Newline #\Return)))
  (:constant nil))

;; Nested C-style block comment:
;; "/*" ( (not "*/") | block-comment )* "*/"
(defrule block-comment
  (and "/*"
       (* (or (not (and "*" "/"))
              ;; recursively allow nested block-comment
              block-comment))
       "*/")
  (:constant nil))

;; opt-ws / ws that treat comments as whitespace
(defrule opt-ws
  (* (or ws-char line-comment block-comment))
  (:constant nil))

(defrule ws
  (+ (or ws-char line-comment block-comment))
  (:constant nil))

;;; ---------------------------
;;; Character classes
;;; ---------------------------

(defrule alpha
  (character-ranges (#\a #\z) (#\A #\Z) #\_))

(defrule digit
  (character-ranges (#\0 #\9)))

;;; ---------------------------
;;; Keywords (from ProgRef)
;;; We capture them as tokens so they don't get parsed as identifiers.
;;; ---------------------------

;; (defparameter *keywords*
;;   '("assert" "bit" "bits" "class" "code" "dag" "def" "defm" "defset"
;;     "defvar" "deftype" "dump" "else" "foreach" "field" "if" "in" "include"
;;     "int" "let" "list" "multiclass" "string" "then" "true" "false"))

;; (defrule keyword
;;   ;; pick any reserved keyword as a whole word; match the literal
;;   (or "assert" "bits" "bit" "class" "code" "dag" "defm" "defset" "defvar"
;;       "deftype" "def" "dump" "else" "foreach" "field" "if" "include" "int"
;;       "in" "let" "list" "multiclass" "string" "then" "true" "false")
;;   (:constant nil))

;;; ---------------------------
;;; Literals
;;; ---------------------------

;; DecimalInteger: optional sign + digits
(defrule decimal-integer
  (and (? (or "+" "-")) (+ digit))
  (:lambda (chars) (parse-integer (coerce chars 'string))))

;; Hex integer: 0x... (allow uppercase X)
(defrule hex-integer
  (and (or "0x" "0X") (+ (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F))))
  (:lambda (chars)
    (parse-integer (coerce chars 'string) :radix 16)))

;; Binary integer: 0b...
(defrule bin-integer
  (and (or "0b" "0B") (+ (or "0" "1")))
  (:lambda (chars)
    (parse-integer (coerce chars 'string) :radix 2)))

(defrule integer
  (or hex-integer bin-integer decimal-integer))

;; String literal: " ... " with escapes \n \t \\ \' \"
(defrule string-char
  (or
   (and "\\" (or "\\" "\"" "'" "n" "t"))
   (not "\"")))

(defrule one-string
  (and "\"" (* string-char) "\"")
  (:lambda (elements)
    ;; assemble the string, process escapes simply
    (let* ((str (coerce (second elements) 'string)))
      ;; unescape minimal set: \\ \" \' \n \t
      (let ((result (with-output-to-string (out)
                      (loop for i from 0 below (length str)
                            for c = (aref str i)
                            do (if (and (char= c #\\) (< i (1- (length str))))
                                   (let ((nxt (aref str (1+ i))))
                                     (incf i)
                                     (case nxt
                                       (#\\ (write-char #\\ out))
                                       (#\" (write-char #\" out))
                                       (#\' (write-char #\' out))
                                       (#\n (write-char #\Newline out))
                                       (#\t (write-char #\Tab out))
                                       (t (write-char nxt out))))
                                 (write-char c out))))))
        result))))

;; Code literal: "[{ ... }]" (TokCode) — keep contents verbatim (no parse)
(defrule code-literal
  (and "[{" (* (not (and "}" "]"))) "}]")
  (:lambda (chars)
    ;; return the raw contents between "[{" and "}]"
    (let ((s (coerce chars 'string)))
      (subseq s 2 (- (length s) 2)))))

;; Paste operator: ProgRef defines a paste operator '.' used between tokens.
;; We'll recognize it as a token in expressions.
(defrule paste-op
  ".")

;; Question mark for uninitialized '?'
(defrule question-mark
  "?")

;;; ---------------------------
;;; Identifiers & VarNames
;;; TokIdentifier allows digits at start, but if it matches integer token, treat as integer.
;;; TokVarName: $ + alpha + alnum*
;;; ---------------------------

(defrule varname
  (and "$" (+ (or alpha digit)))
  (:lambda (chars) (coerce chars 'string)))

(defrule identifier
  ;; per ProgRef: ("0"..."9")* ualpha (ualpha | "0"..."9")*
  ;; Implement permissive: sequence that contains at least one alpha/_ and may start with digits.
  (and (* digit) alpha (* (or alpha digit)))
  (:lambda (elements)
    (coerce (list* (second elements) (third elements)) 'string)))

;;; ---------------------------
;;; Bang operators (!op) and Cond operator
;;; ---------------------------

(defrule bang-op
    (or "!eq" "!if" "!head" "!tail" "!cond" "!con" "!add" "!shl" "!sra" "!srl"
        "!and" "!or" "!empty" "!subst" "!foreach" "!strconcat"
        "!cast" "!listconcat" "!size" "!foldl" "!isa" "!dag" "!getop"
        "!le" "!lt" "!ge" "!gt" "!ne" "!mul"
        "!listsplat" "!not" "!range" "!setop" "!listremove"
        "!interleave" "!tolower" "!toupper")
  (:lambda (name)
    (list :bang-op name)))

;;; ---------------------------
;;; Types
;;;  bit | int | string | code | dag | bits< N > | list< T > | ClassID
;;; ---------------------------

(defrule type
  (and
   (or "bit" "int" "string" "code" "dag"
       (and "bits" opt-ws "<" opt-ws integer opt-ws ">")
       (and "list" opt-ws "<" opt-ws type opt-ws ">")
       identifier)
   ;; whitespace after a top-level type token
   (? ws))
  (:lambda (x) x))

;;; ---------------------------
;;; Expressions / Values
;;; TableGen supports many expression forms. We provide a practical superset:
;;;  - concatenated string literals
;;;  - integers
;;;  - varnames ($foo)
;;;  - identifiers (maybe class names)
;;;  - lists [ a, b, ... ]
;;;  - DAGs: (op, arg1, arg2, ...)
;;;  - parenthesized expressions
;;;  - unary ops + - ~ !
;;;  - binary ops: arithmetic, bitwise, comparisons, logical
;;;  - bang-ops: !foo(args)
;;;  - paste operator '.' for token concatenation (we accept it between strings/idents)
;;;  - code-literals "[{ ... }]"
;;;  - the special '?' uninitialized
;;; ---------------------------

;; string-list: adjacent string tokens are concatenated
(defrule string-literal
  (and one-string (* (and opt-ws one-string opt-ws)))
  (:lambda (elements)
    (apply 'concatenate 'string (list* (first elements)
                                       (mapcar 'second (second elements))))))

;; basic atom value
(defrule atom
  (or string-literal code-literal integer varname identifier question-mark)
  (:lambda (v) v))

;; ;; forward decl for expr to allow recursion
;; (declare (special *expr*))
;; (defparameter *expr* nil)

;; parenthesized expression list or solitary expr
(defrule paren-expr
  (and "(" opt-ws (? expr-list) opt-ws ")")
  (:lambda (x) (nth 2 x)))

;; expr-list: expr, (',' expr)*
(defrule expr-list
  (and expr
       (* (and opt-ws "," opt-ws expr))
       (? ","))
  (:lambda (x)
    (cons (first x)
          (mapcar #'fourth (second x)))))

;; list literal: [ expr, expr, ... ]
(defrule list-literal
  (and "[" opt-ws (? expr-list) opt-ws "]")
  (:lambda (x) (nth 2 x)))

;; DAG: (op, arg1, arg2, ...) — op may be a string or identifier
(defrule dag
  (and "(" opt-ws (or string-literal identifier) opt-ws
       (* (and "," opt-ws expr opt-ws))
       ")")
  (:lambda (x)
    (cons (second x) (mapcar #'third (nthcdr 4 x)))))

;; function-style bang operator usage: !op(arg1, arg2, ...)
(defrule bang-call
  (and bang-op opt-ws "(" opt-ws (? expr-list) opt-ws ")")
  (:lambda (x)
    (cons (first x) (nth 4 x))))

;;; Operators precedence:
;; We'll implement a standard precedence climbing in grammar pieces:
;; unary -> power -> mul/div -> add/sub -> shift -> bitwise -> comparisons -> logical
;; For ESrap, simpler approach: implement binary operator sequences and fold in a :lambda.
;; For maintainable parser, we produce nested binary sequences by parsing left-associative chains.

;; operator token helpers
(defrule op-mul (or "*" "/" "%"))
(defrule op-add (or "+" "-"))
(defrule op-shift (or "<<" ">>"))
(defrule op-bitwise (or "&" "|" "^"))
(defrule op-compare (or "==" "!=" "<=" ">=" "<" ">"))
(defrule op-logical (or "&&" "||"))

;; primary: atom | paren-expr | list-literal | dag | bang-call
(defrule primary
  (or atom paren-expr list-literal dag bang-call)
  (:lambda (x) x))

;; power (no exponent operator in TableGen but keep generic)
(defrule power
  primary
  (:lambda (x) x))

;; unary: optional unary operator(s) before primary
(defrule unary
  (and (! bang-op) (* (or "+" "-" "~" "!")) power)
  (:lambda (x)
    (let ((ops (second x))
          (val (third x)))
      (if ops
          ;; return a simple form: (:unary (list-of-ops) val)
          (list :unary ops val)
          val))))

;; multiplicative chain: left-associative
(defrule multiplicative
  (and unary (* (and opt-ws op-mul opt-ws unary)))
  (:lambda (x)
    (let ((head (first x))
          (tail (mapcar #'fourth (second x))))
      (if (null tail) head (cons :binop (cons head tail))))))

;; additive chain
(defrule additive
    (and multiplicative (* (and opt-ws op-add opt-ws multiplicative)))
  (:lambda (x) (if (null (second x)) (first x) (cons :binop (cons (first x) (mapcar #'fourth (second x)))))))

;; shift chain
(defrule shift
    (and additive
         (* (and opt-ws
                 op-shift
                 opt-ws
                 additive)))
  (:lambda (x) (if (null (second x)) (first x) (cons :binop (cons (first x) (mapcar #'fourth (second x)))))))

;; bitwise chain
(defrule bitwise
    (and shift (* (and opt-ws op-bitwise opt-ws shift)))
  (:lambda (x) (if (null (second x)) (first x) (cons :binop (cons (first x) (mapcar #'fourth (second x)))))))

;; comparisons
(defrule comparison
    (and bitwise (* (and opt-ws op-compare opt-ws bitwise)))
  (:lambda (x)
    (if (null (second x))
        (first x)
      (cons :cmp (cons (first x) (mapcar #'fourth (second x)))))))

;; logical
(defrule logical
    (and comparison (* (and opt-ws op-logical opt-ws comparison)))
  (:lambda (x)
    (if (null (second x))
        (first x)
        (cons :logic (cons (first x) (mapcar #'fourth (second x)))))))

;; final expr rule
(defrule expr
    (or logical
        primary) ;; cover any leftover primary forms
  (:lambda (x)
    x))

;; make *expr* refer to expr for recursive uses above
;;(setf *expr* #'expr)

;;; ---------------------------
;;; Statements (top-level)
;;; ---------------------------

;; class Decl: class NAME <params>? ( : Parent<args> )? { body } ;
(defrule record-body
    (and "{" opt-ws (* field-assign) opt-ws "}")
  (:lambda (x) (nth 2 x)))

(defrule field-assign
    (and type identifier opt-ws "=" opt-ws expr opt-ws (? ";") opt-ws)
  (:lambda (x) (list (first x) (nth 4 x))))

(defrule inherit
    (and ":" opt-ws identifier opt-ws (? (and "<" opt-ws (? arg-list) opt-ws ">")))
  (:lambda (x) (list :inherit (second x) (fourth x))))

(defrule template-params
    (and "<" opt-ws (? param-list) opt-ws ">")
  (:lambda (x) (nth 2 x)))

(defrule template-args
    (and "<" opt-ws (? arg-list) opt-ws ">")
  (:lambda (x) (nth 2 x)))

(defrule arg-list
    (and expr (* (and opt-ws "," opt-ws expr)))
  (:lambda (x) (cons (first x) (mapcar #'fourth (second x)))))

(defrule param-list
    (and identifier (* (and opt-ws "," opt-ws identifier)))
  (:lambda (x) (cons (first x) (mapcar #'fourth (second x)))))

(defrule class-decl
    (and "class" ws identifier opt-ws (? template-params) opt-ws (? inherit) opt-ws (? record-body) opt-ws ";")
  (:lambda (x) (list :class (nth 2 x) :params (nth 4 x) :inherit (nth 6 x) :body (nth 8 x))))

(defrule def-decl
    (and "def" ws identifier opt-ws (? inherit) opt-ws (? template-args) opt-ws
         (? record-body) opt-ws ";")
  (:lambda (x)
    (list :def (nth 2 x)
          :inherit (nth 4 x)
          :template (nth 6 x)
          :body (nth 8 x))))

(defrule multiclass-decl
    (and "multiclass" ws identifier opt-ws (? template-params) opt-ws (? inherit) opt-ws (? record-body) opt-ws ";")
  (:lambda (x) (list :multiclass (nth 2 x) :params (nth 4 x) :inherit (nth 6 x) :body (nth 8 x))))

(defrule defm-decl
    (and "defm" ws identifier opt-ws (? template-args) opt-ws (? record-body) opt-ws ";")
  (:lambda (x) (list :defm (nth 2 x) :template (nth 4 x) :body (nth 6 x))))

(defrule let-decl
    (and "let" ws identifier opt-ws "=" opt-ws expr opt-ws ";")
  (:lambda (x) (list :let (nth 2 x) (nth 6 x))))

(defrule include-decl
    (and "include" ws one-string opt-ws)
  (:lambda (x) (list :include (nth 2 x))))

(defrule defset-decl
    (and "defset" ws identifier opt-ws (? record-body) opt-ws ";")
  (:lambda (x) (list :defset (nth 2 x) :body (nth 4 x))))

(defrule deftype-decl
    (and "deftype" ws identifier opt-ws "=" opt-ws type opt-ws ";")
  (:lambda (x) (list :deftype (nth 2 x) (nth 6 x))))

(defrule defvar-decl
    (and "defvar" ws identifier opt-ws "=" opt-ws expr opt-ws ";")
  (:lambda (x) (list :defvar (nth 2 x) (nth 6 x))))

;; foreach, if, dump, assert have tree-like forms — rough parse here:
(defrule foreach-decl
    (and "foreach" ws identifier ws "in" ws expr opt-ws record-body opt-ws ";")
  (:lambda (x) (list :foreach (nth 2 x) :in (nth 6 x) :body (nth 8 x))))

(defrule if-decl
    (and "if" ws expr ws "then" opt-ws (? statement-list) opt-ws (? (and "else" opt-ws (? statement-list))) opt-ws "endif")
  (:lambda (x) (list :if (nth 2 x) :then (nth 4 x) :else (nth 6 x))))

(defrule dump-decl
    (and "dump" ws (? expr) opt-ws ";")
  (:lambda (x) (list :dump (nth 2 x))))

(defrule assert-decl
    (and "assert" ws expr opt-ws ";")
  (:lambda (x) (list :assert (nth 2 x))))

(defrule statement
    (and opt-ws (or class-decl def-decl multiclass-decl defm-decl let-decl include-decl defset-decl deftype-decl defvar-decl foreach-decl dump-decl assert-decl) opt-ws)
  (:lambda (x) (second x)))

(defrule statement-list
    (and (* statement) opt-ws)
  (:lambda (x) (mapcar #'second (first x))))

(defrule tablegen-file
    (and opt-ws (* statement) opt-ws)
  (:lambda (x) (mapcar #'second (second x))))

(defun x ()
  (let ((input (uiop:read-file-string "/home/alendvai/common-lisp/maru/source/assembler/X86/X86.td")))
    (esrap:parse 'tablegen-file input)))

(defun x2 ()
  (let ((input "\"first \" \"second \" \"third \""))
    (esrap:parse 'string-literal input)))

(defun x3 ()
  (let ((input "\"Remove speculation of indirect branches from the \""))
    (esrap:parse 'one-string input)))

(defun x4 ()
  (let ((input "def FeatureRetpoline
    : SubtargetFeature<\"retpoline\", \"DeprecatedUseRetpoline\", \"true\",
                       \"Remove speculation of indirect branches from the \"
                       \"generated code, either by avoiding them entirely or \"
                       \"lowering them with a speculation blocking construct\",
                       [FeatureRetpolineIndirectCalls,
                        FeatureRetpolineIndirectBranches]>;"))
    (esrap:parse 'def-decl input)))

(defun x5 ()
  (let ((input "[FeatureCX16, FeatureLAHFSAHF64,]"))
    (esrap:parse 'expr input)))

(defun x6 ()
  (let ((input "!listconcat(X86_64V1Features, [
    FeatureCX16, FeatureLAHFSAHF64, FeatureCRC32, FeaturePOPCNT,
    FeatureSSE42
  ])"))
    (esrap:parse 'expr input)))
