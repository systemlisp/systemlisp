(in-package :system-lisp)

;;; Logic signal
(defclass sl-signal-logic (sl-number-signal)
  ((bit-width :initarg :bit-width :accessor bit-width :type unsigned-byte :initform 1)
   (x-mask :accessor x-mask :initform (make-array 0 :adjustable t :fill-pointer 0))
   (z-mask :accessor z-mask :initform (make-array 0 :adjustable t :fill-pointer 0))))


;;; TODO finish implementation 
