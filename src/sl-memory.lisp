(in-package :system-lisp)

(defclass sl-memory ()
  ((memory :accessor memory :initform (make-hash-table :test 'eql))
   (data-bits :accessor data-bits :initarg :data-bits :initform 8)
   (addr-bits :accessor addr-bits :initarg :addr-bits :initform 64)
   (word-count :accessor word-count :initarg :word-count :initform (ash 1 64))
   (endianess :accessor endianess :initarg :endianess :initform 'le)))

(defmethod initialize-instance :after ((obj sl-memory) &key)
  (assert (< (word-count obj) (ash 1 (addr-bits obj))) () "make-memory: word-count ~a exceeds the ~abit address range" (word-count obj) (addr-bits obj))
  (assert (member (endianess obj) '(le be)) () "make-memory: invalid endianess - ~a" (endianess obj)))

(defun make-memory (&key (data-bits 8) (addr-bits 64) (word-count (ash 1 64)))
  (declare (type integer data-bits))
  (declare (type integer addr-bits))
  (declare (type integer word-count))
  (make-instance 'sl-memory :data-bits data-bits :addr-bits addr-bits :word-count word-count))

(defgeneric sl-memory-write (mem addr data))
(defgeneric sl-memory-burst-write (mem addr data))

(defgeneric sl-memory-read (mem addr))
(defgeneric sl-memory-read-uint (mem addr))
(defgeneric sl-memory-read-int (mem addr))

(defgeneric sl-memory-burst-read (mem addr count))
(defgeneric sl-memory-burst-read-uint (mem addr count))
(defgeneric sl-memory-burst-read-int (mem addr count))

;;; TODO: implement these
(defgeneric sl-memory-write-to-file (mem fpath))
(defgeneric sl-memory-read-form-file (mem fpath))

(defmethod sl-memory-write ((self sl-memory) (addr integer) (data integer))
  (assert (< addr (1- (word-count self))) (addr) "sl-memory-write: addres ~X exceeds memory word count of ~a" addr (addr-bits self))
  (setf (gethash addr self) (logand data (1- (ash 1 (data-bits self))))))

(defmethod sl-memory-write ((self sl-memory) (addr integer) (data sl-uint))
  (assert (< addr (1- (word-count self))) (addr) "sl-memory-write: addres ~X exceeds memory word count of ~a" addr (addr-bits self))
  (setf (gethash addr self) (sl-uint-value data)))

(defmethod sl-memory-write ((self sl-memory) (addr integer) (data sl-int))
  (assert (< addr (1- (word-count self))) (addr) "sl-memory-write: addres ~X exceeds memory word count of ~a" addr (addr-bits self))
  (setf (gethash addr self) (sl-int-value data)))

(defmethod sl-memory-burst-write ((self sl-memory) (addr integer) (data vector))
  (let ((write-addr addr))
    (loop for val across data do
      (sl-memory-write self addr val)
      (incf write-addr))))

(defmethod sl-memory-burst-write ((self sl-memory) (addr integer) (data sl-uint))
  (let ((write-addr addr)
	(val-to-write 0)
	(words-to-write (ceiling (sl-uint-bits data) (data-bits self))))
    (if (eql (endianess self) 'le)
	(loop for i from 1 to words-to-write do
	  (setf val-to-write (ldb (byte (data-bits self) (* (1- i) (data-bits self))) (sl-uint-value data)))
	  (sl-memory-write self write-addr val-to-write)
	  (incf write-addr))
	;; else
	(loop for i from words-to-write downto 1 do
	  (setf val-to-write (ldb (byte (data-bits self) (* (1- i) (data-bits self))) (sl-uint-value data)))
	  (sl-memory-write self write-addr val-to-write)
	  (incf write-addr)))))

(defmethod sl-memory-burst-write ((self sl-memory) (addr integer) (data sl-int))
  (let ((write-addr addr)
	(val-to-write 0)
	(words-to-write (ceiling (sl-int-bits data) (data-bits self))))
    (if (eql (endianess self) 'le)
	(loop for i from 1 to words-to-write do
	  (setf val-to-write (ldb (byte (data-bits self) (* (1- i) (data-bits self))) (sl-int-value data)))
	  (sl-memory-write self write-addr val-to-write)
	  (incf write-addr))
	;; else
	(loop for i from words-to-write downto 1 do
	  (setf val-to-write (ldb (byte (data-bits self) (* (1- i) (data-bits self))) (sl-int-value data)))
	  (sl-memory-write self write-addr val-to-write)
	  (incf write-addr)))))

(defmethod sl-memory-read ((self sl-memory) (addr integer))
  (assert (<= addr (1- (word-count self))) (addr) "sl-memory-write: addres ~X exceeds memory word count of ~a" addr (addr-bits self))
  (let ((val (gethash addr self)))
    (if (null val) 0 val)))

(defmethod sl-memory-read-uint ((self sl-memory) (addr integer))
  (assert (<= addr (1- (word-count self))) (addr) "sl-memory-write: addres ~X exceeds memory word count of ~a" addr (addr-bits self))
  (let ((val (gethash addr self)))
    (if (null val) 0 (uint val :bits (data-bits self)))))

(defmethod sl-memory-read-int ((self sl-memory) (addr integer))
  (assert (<= addr (1- (word-count self))) (addr) "sl-memory-write: addres ~X exceeds memory word count of ~a" addr (addr-bits self))
  (let ((val (gethash addr self)))
    (if (null val) 0 (uint val :bits (data-bits self)))))

(defmethod sl-memory-burst-read ((self sl-memory) (addr integer) (count integer))
  (let ((result (make-array 0 :adjustable t :fill-pointer 0 :element-type 'integer))
	(read-addr addr))
    (loop for i from 1 to count do
      (vector-push-extend (sl-memory-read self read-addr) self)
      (incf read-addr))
    result))

(defmethod sl-memory-burst-read-uint ((self sl-memory) (addr integer) (count integer))
  (let ((result (uint 0 :bits (* count (data-bits self))))
	(read-addr addr))
    (if (eql (endianess self) 'le)
	(loop for i from 1 to count do
	      (setf (ldb (byte (data-bits self) (* (1- i) (data-bits self))) (sl-uint-value result)) (sl-memory-read self read-addr))
	      (incf read-addr))
	;; else
	(loop for i from count downto 1 do
	      (setf (ldb (byte (data-bits self) (* (1- i) (data-bits self))) (sl-uint-value result)) (sl-memory-read self read-addr))
	      (incf read-addr)))))

(defmethod sl-memory-burst-read-int ((self sl-memory) (addr integer) (count integer))
  (let ((result (int 0 :bits (* count (data-bits self))))
	(read-addr addr))
    (if (eql (endianess self) 'le)
	(loop for i from 1 to count do
	      (setf (ldb (byte (data-bits self) (* (1- i) (data-bits self))) (sl-int-value result)) (sl-memory-read self read-addr))
	      (incf read-addr))
	;; else
	(loop for i from count downto 1 do
	      (setf (ldb (byte (data-bits self) (* (1- i) (data-bits self))) (sl-int-value result)) (sl-memory-read self read-addr))
	      (incf read-addr)))))

;;; Buffers and allocators
(defclass sl-buffer ()
  ((memory :accessor memory :initarg :memory :type sl-memory)
   (offset-start :accessor offset-start :initarg offset-start :type integer)
   (offset-end :accessor offset-end :initarg offset-end :type integer)))

(defclass sl-allocator ()
  ((memory :accessor memory :initarg :memory :type sl-memory)
   (buffers :accessor buffers :initform '() :type list)))

;;; TODO: implement these
(defgeneric sl-memory-allocate (allocator size))
(defgeneric sl-memory-free (allocator buffer))
(defgeneric sl-memory-defragment (allocator))
