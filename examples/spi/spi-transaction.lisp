(in-package :system-lisp-examples)

(defclass spi-transaction-base () ())

(defclass spi-generic-transaction (spi-transaction-base)
  ((slave-index :accessor slave-index :initarg :slave-index :initform 0)
   (mosi-bits :accessor mosi-bits :initarg :mosi-bits :initform (make-array 0 :adjustable t :fill-pointer 0))
   (miso-bits :accessor miso-bits :initarg :miso-bits :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass spi-nor-flash-transaction (spi-transaction-base)
  ((slave-index :accessor slave-index :initarg :slave-index :initform 0)
   (opcode :accessor opcode :initarg :opcode :initform (uint 0 :bits 8))
   (address :accessor address :initarg :address :initform (uint 0 :bits 24))
   (data :accessor data :initarg :data :initform (uint 0 :bits (* 8 4)))))


