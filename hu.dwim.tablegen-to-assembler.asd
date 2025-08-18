(defsystem :hu.dwim.tablegen-to-assembler
  :description ""
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :depends-on (:alexandria
               :json-streams
               :uiop)
;;  :in-order-to ((test-op (test-op :hu.dwim.tablegen-to-assembler/test)))
  :components ((:module "source"
                :components
                ((:module "generator"
                  :components ((:file "package")
                               (:file "parsing")))
                 (:module "assembler"
                  :components ((:file "package")
                               (:file "assembler")
                               ;; (:file "x86-instructions")
                               ))))))

;; (defsystem :hu.dwim.tablegen-to-assembler/test
;;   :depends-on (:hu.dwim.stefil
;;                :hu.dwim.tablegen-to-assembler
;;                ;; you probably also want to load :hu.dwim.stefil+swank one way or another
;;                )
;;   ;; Unfortunately ASDF swallows the return value (i.e. it cannot be
;;   ;; inspected in Slime), so we at least print it.
;;   :perform (test-op (o c) (print (funcall (intern (string '#:test)
;;                                                   (find-package :hu.dwim.zlib/test)))))
;;   :components ((:module "test"
;;                 :components ((:file "package")
;;                              (:file "suite" :depends-on ("package"))
;;                              (:file "zlib" :depends-on ("suite"))
;;                              (:file "random" :depends-on ("suite"))))))
