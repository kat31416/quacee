(in-package :asdf)
(asdf:defsystem "quacee"
  :description "A Quantum Computing language"
  :version "0"
  :author "Katherine Hudek"
  :licence "MIT"
  :serial t
  :components ((:file "code/circ-defs")
	       (:file "code/gate-defs")
	       (:file "code/quacee-defs")
	       (:file "code/quacee-simbasic")	       
	       (:file "code/quacee-util"
                    :depends-on ("code/gate-defs" "code/circ-defs" "code/quacee-defs"))

	       (:file "code/quacee"
                    :depends-on ("code/gate-defs" "code/circ-defs" "code/quacee-defs" "code/quacee-util" "code/quacee-simbasic"))
	       (:file "examples/GenerateUsefulCircs"
                    :depends-on ("code/quacee"))

	       (:file "examples/HelloWorld"
                    :depends-on ("code/quacee"))
	       
	       ))
