(in-package :system-lisp-examples)

(defclass spi-config ()
  ((cpol :accessor cpol :initarg :cpol :initform #u1b_0)
   (cpha :accessor cpha :initarg :cpha :initform #u1b_0)
   (wait-bef-start :accessor wait-bef-start :initarg :wait-bef-start :initform 0)
   (sclk-half-period :accessor sclk-half-period :initarg :sclk-half-period :initform 5)))

(defmethod get-sampling-edge ((self spi-config))
  (let ((cpol (-> self cpol))
	(cpha (-> self cpha)))
    (cond
      ((and (sl= cpha 0) (sl= cpol 0)) 'posedge)
      ((and (sl= cpha 0) (sl= cpol 1)) 'negedge)
      ((and (sl= cpha 1) (sl= cpol 0)) 'negedge)
      ((and (sl= cpha 1) (sl= cpol 1)) 'posedge)
      (t (error "Invalid configuration cpha=~a cpol=~a" cpha cpol)))))

(defmethod get-driving-edge ((self spi-config))
  (let ((cpol (-> self cpol))
	(cpha (-> self cpha)))
    (cond
      ((and (sl= cpha 0) (sl= cpol 0)) 'negedge)
      ((and (sl= cpha 0) (sl= cpol 1)) 'posedge)
      ((and (sl= cpha 1) (sl= cpol 0)) 'posedge)
      ((and (sl= cpha 1) (sl= cpol 1)) 'negedge)
      (t (error "Invalid configuration cpha=~a cpol=~a" cpha cpol)))))
