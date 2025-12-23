(in-package :system-lisp-examples)

;;; SPI interface base
(defclass spi-interface-base (sl-component) ())

;;; SPI master interface
(defclass spi-master-interface (spi-interface-base)
  (;; Parameters
   (nr-slaves :accessor nr-slaves :initarg :nr-slaves :initform 1)
   ;; Clock and reset
   (ref-clk :accessor ref-clk :initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset :accessor reset :initform (make-instance 'sl-signal-binary :bit-width 1))
   ;; SPI signals
   (sclk :accessor sclk :initform (make-instance 'sl-signal-binary :bit-width 1))
   (mosi :accessor mosi :initform (make-instance 'sl-signal-binary :bit-width 1))
   (miso :accessor miso :initform (make-instance 'sl-signal-binary :bit-width 1))
   (ssn :accessor ssn :type sl-signal-binary)))

(defmethod initialize-instance :after ((obj spi-master-interface) &key)
  (setf (ssn obj) (make-instance 'sl-signal-binary :bit-width (nr-slaves obj))))

;;; SPI slave interface
(defclass spi-slave-interface (spi-interface-base)
  (;; Clock and reset
   (ref-clk :accessor ref-clk :initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset :accessor reset :initform (make-instance 'sl-signal-binary :bit-width 1))
   ;; SPI signals
   (sclk :accessor sclk :initform (make-instance 'sl-signal-binary :bit-width 1))
   (mosi :accessor mosi :initform (make-instance 'sl-signal-binary :bit-width 1))
   (miso :accessor miso :initform (make-instance 'sl-signal-binary :bit-width 1))
   (ssn :accessor ssn :initform (make-instance 'sl-signal-binary :bit-width 1))))
