(in-package :system-lisp-examples)

(defclass swap-tb (sl-component)
  ((a :accessor a
      :initform (make-instance 'sl-signal-binary :bit-width 8))
   (b :accessor b
      :initform (make-instance 'sl-signal-binary :bit-width 8))))

(defmethod-res swap-test ((self swap-tb))
  (setb (a self) 4)
  (setb (b self) 7)

  (sim-delay 10)

  (setnb (a self) (b self))
  (setnb (b self) (a self))

  (sim-delay 20)

  (setnb (a self) 0)
  (setnb (b self) 0)

  (sim-delay 20))

(defmethod run ((self swap-tb))
  (spawn #'swap-test self))

(defun swap-tb-run-sim ()
  (let ((*sim* (make-instance 'sl-sim))
	(tb-inst (create-component swap-tb nil)))
    (reset-sim)
    (load-component tb-inst)
    (elaborate)
    (vcd-trace-component tb-inst "top")
    (run 500)))
