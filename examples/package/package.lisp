(defpackage :system-lisp-examples
  (:nicknames :sl-examples)
  (:use
   :cl
   :sl)
  
  (:shadowing-import-from :sl :bit)

  (:export
   :counter-run-sim
   :d-flip-flop-run-sim
   :rand-num-gen-run-sim
   :run-spi-test-1master-1slave-sim
   :swap-tb-run-sim))

(in-package :system-lisp-examples)
