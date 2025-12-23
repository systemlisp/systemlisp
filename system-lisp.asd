(in-package :asdf-user)

(defsystem "system-lisp"
  :depends-on ("cl-generator" "cl-containers" "closer-mop" "sqlite" "iterate" "trivia" "alexandria" "local-time" "iterate" "anaphora" "fare-quasiquote")
  :components ((:module "src" :components
			((:file "package")
			 (:file "abstracts")
			 (:file "sl-syn-sugar")
			 (:file "sl-logger")
			 (:file "sl-component" :depends-on ("sl-logger"))
			 (:file "sl-sim" :depends-on ("sl-logger" "sl-time"))
			 (:file "sl-integers" :depends-on ("sl-logger"))
			 (:file "sl-resumable" :depends-on ("sl-logger"))
			 (:file "sl-sim-process" :depends-on ("sl-logger"))
			 (:file "sl-sim-event-update" :depends-on ("sl-logger"))
			 (:file "sl-sim-event-region" :depends-on ("sl-logger"))
			 (:file "sl-sim-event-eval" :depends-on ("sl-logger"))
			 (:file "sl-sync-event" :depends-on ("sl-logger"))
			 (:file "sl-sync-event-group" :depends-on ("sl-logger"))
			 (:file "sl-signal-object" :depends-on ("sl-logger"))
			 (:file "sl-signal-number" :depends-on ("sl-logger" "sl-sync-event"))
			 (:file "sl-signal-lambda" :depends-on ("sl-logger"))
			 (:file "sl-signal-number-lambda" :depends-on ("sl-logger"))
			 (:file "sl-signal-binary" :depends-on ("sl-logger" "sl-signal-number" "sl-resumable"))
			 (:file "sl-signal-binary-lambda" :depends-on ("sl-logger"))
			 (:file "sl-signal-logic" :depends-on ("sl-logger"))
			 (:file "sl-signal-proxy" :depends-on ("sl-logger" "sl-signal-binary"))
			 (:file "sl-vcd-tracing" :depends-on ("sl-logger"))
			 (:file "sl-time-slot" :depends-on ("sl-logger"))
			 (:file "sl-timing-control-wait-delay" :depends-on ("sl-logger" "sl-sim"))
			 (:file "sl-timing-control-wait-event" :depends-on ("sl-logger" "sl-sim"))
			 (:file "sl-ports" :depends-on ("sl-logger" "sl-sim" "sl-resumable" "sl-sim-process" "sl-component" "sl-sync-event"))
			 (:file "sl-dsa" :depends-on ("sl-logger" "sl-syn-sugar"))
			 (:file "sl-memory" :depends-on ("sl-logger" "sl-syn-sugar" "sl-integers" "sl-signal-binary"))
			 (:file "sl-time" :depends-on ("sl-logger" "sl-syn-sugar" "sl-integers"))))))

(defsystem "system-lisp/tests"
  :depends-on ("system-lisp" "fiveam")
  :components ((:module "tests" :components
			((:file "package")
			 (:file "sl-main-test" )
			 )))
  :perform (test-op (op c) (uiop:symbol-call :fiveam :run!
					     (find-symbol* :sl-main-test :system-lisp-test))))

(defsystem "system-lisp/examples"
  :depends-on ("system-lisp")
  :serial t
  :components ((:module "examples/package" :components
			((:file "package")))
	       (:module "examples/counter" :components
			((:file "counter")
			 (:file "counter-tb")
			 (:file "counter-run-sim")))
	       (:module "examples/rand-num-gen" :components
			((:file "rand-num-gen")
			 (:file "rand-num-gen-tb")))
	       (:module "examples/spi" :components
			((:file "spi-config")
			 (:file "spi-transaction")
			 (:file "spi-interfaces")
			 (:file "spi-master" :depends-on ("spi-config" "spi-transaction" "spi-interfaces"))
			 (:file "spi-slave" :depends-on ("spi-config" "spi-transaction" "spi-interfaces"))
			 (:file "spi-test-1master-1slave" :depends-on ("spi-master" "spi-slave" "spi-config" "spi-interfaces" "spi-transaction"))))
	       (:module "examples/swap-nb" :components
			((:file "swap-nb")))
	       (:module "examples/flip-flops" :components
			((:file "d-flip-flop")))))

