;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.genassem
  :description ""
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :depends-on (:alexandria
               :anaphora
               :hu.dwim.genassem/asm-base
               :json-streams
               :uiop)
  :components ((:module "source"
                :components
                ((:module "generator"
                  :components ((:file "package")
                               (:file "parsing")))))))

(defsystem :hu.dwim.genassem/asm-base
  :description ""
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :depends-on (:alexandria
               :anaphora
               :uiop)
  :components ((:module "source"
                :components
                ((:module "assembler"
                  :components ((:file "package")
                               (:file "assembler")
                               ))))))

(defsystem :hu.dwim.genassem/x86
  :description ""
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :depends-on (:alexandria
               :anaphora
               :hu.dwim.genassem/asm-base
               :uiop)
  :in-order-to ((test-op (test-op :hu.dwim.genassem/x86.test)))
  :components ((:module "source"
                :components
                ((:module "assembler"
                  :components ((:file "x86-instructions")
                               ))))))

(defsystem :hu.dwim.genassem/x86.test
  :depends-on (:alexandria
               :anaphora
               :hu.dwim.stefil
               :hu.dwim.genassem
               :uiop
               ;; you probably also want to load :hu.dwim.stefil+swank one way or another
               )
  ;; Unfortunately ASDF swallows the return value (i.e. it cannot be
  ;; inspected in Slime), so we at least print it.
  :perform (test-op (o c) (print (funcall (intern (string '#:test)
                                                  (find-package :hu.dwim.genassem/x86.test)))))
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "simple" :depends-on ("package" "suite"))))))
