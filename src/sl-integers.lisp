(in-package :system-lisp)

;;; Convert a lisp integer to a fixed bitwidth unsigned number (represented still as a lisp integer)
(declaim (inline to-uint))
(defun to-uint (x n)
  (cl:logand x (cl:1- (cl:ash 1 n))))

;;; Convert a lisp integer to a fixed bitwidth signed integer (represented still as a lisp integer)
(declaim (inline to-int))
(defun to-int (x n)
  (let* ((base-max-num (cl:ash 1 n))
	 (x-masked (cl:logand x (cl:- base-max-num 1))))
    (if (cl:logbitp (cl:1- n) x)
	(cl:- x-masked base-max-num)
	x-masked)))

;;; Fixed bit width integer structs
(declaim (inline make-sl-uint))
(defstruct (sl-uint (:constructor make-sl-uint (bits value)))
  (bits 1 :type fixnum)
  (value 0 :type integer))

(defun new-sl-uint (&key (bits 1) (value 0))
  (let ((result (make-sl-uint bits 0)))
    (setf (sl-uint-value result) (to-uint value bits))
    result))

(declaim (inline uint))
(defun uint (value &key (bits 32))
  (let ((result (make-sl-uint bits 0)))
    (setf (sl-uint-value result) (to-uint value bits))
    result))

(declaim (inline make-sl-int))
(defstruct (sl-int (:constructor make-sl-int (bits value)))
  (bits 1 :type fixnum)
  (value 0 :type integer))

(defun new-sl-int (&key (bits 1) (value 0))
  (let ((result (make-sl-int bits 0)))
    (setf (sl-int-value result) (to-int value bits))
    result))

(declaim (inline int))
(defun int (value &key (bits 32))
  (let ((result (make-sl-int bits 0)))
    (setf (sl-int-value result) (to-int value bits))
    result))

(defmethod uint-cast ((value sl-uint) &key (bits 32))
  (uint (sl-uint-value value) :bits bits))

(defmethod uint-cast ((value sl-int) &key (bits 32))
  (uint (sl-int-value value) :bits bits))

(defmethod uint-cast ((value number) &key (bits 32))
  (uint value :bits bits))

(defmethod int-cast ((value sl-uint) &key (bits 32))
  (int (sl-uint-value value) :bits bits))

(defmethod int-cast ((value sl-int) &key (bits 32))
  (int (sl-int-value value) :bits bits))

(defmethod int-cast ((value number) &key (bits 32))
  (int value :bits bits))

(defmethod setb ((obj sl-uint) (val number))
  (setf (sl-uint-value obj) (to-uint val (sl-uint-bits obj)))
  obj)

(defmethod setb ((obj sl-uint) (val sl-uint))
  (setb obj (sl-uint-value val))
  obj)

(defmethod setb ((obj sl-int) (val number))
  (setf (sl-int-value obj) (to-int val (sl-int-bits obj)))
  obj)

(defmethod setb ((obj sl-int) (val sl-int))
  (setb obj (sl-int-value val))
  obj)

(defmethod setb ((obj sl-uint) (val sl-int))
  (setb obj (to-uint (sl-int-value val) (sl-uint-bits obj)))
  obj)

(defmethod setb ((obj sl-int) (val sl-uint))
  (setb obj (to-int (sl-uint-value val) (sl-int-bits obj)))
  obj)

;;; Reader macros for fbwis
(defun read-chars-to-string (stream allow-char-p &key (end-char-p (lambda (c) (null c))))
  "Reads characters from the given stream until:
   - An illegal  character is encountered, which causes an error.
   - The end of the stream is reached, in which case the collected digits are returned as a string."
  (let ((result (make-string-output-stream))) ; Create an output stream to collect characters
    (loop
       for char = (read-char stream nil nil) ; Read the next character, return nil at EOF
       do
         (cond
           ((funcall end-char-p char)
            (return (get-output-stream-string result)))
           ((funcall allow-char-p char) ; Check if it's a valid char
            (write-char char result)) ; Append to the result
	   (t
	    (if (cl:eq char #\)) (progn (unread-char char stream)
				     (return (get-output-stream-string result)))
		(error "Illegal character encountered: ~a" char))))))) ; Issue an error on illegal chars

(defun is-bin (char)
  (let ((code (char-code char)))
    (or (and (cl:>= code (char-code #\0)) (cl:<= code (char-code #\1)))
	(cl:= code (char-code #\_)))))

(defun is-oct (char)
  (let ((code (char-code char)))
    (or (and (cl:>= code (char-code #\0)) (cl:<= code (char-code #\7)))
	(cl:= code (char-code #\_)))))

(defun is-dec (char)
  (let ((code (char-code char)))
    (or (and (cl:>= code (char-code #\0)) (cl:<= code (char-code #\9)))
	(cl:= code (char-code #\_)))))

(defun is-hex (char)
  (let ((code (char-code char)))
    (or (and (cl:>= code (char-code #\0)) (cl:<= code (char-code #\9)))
	(and (cl:>= code (char-code #\a)) (cl:<= code (char-code #\f)))
	(and (cl:>= code (char-code #\A)) (cl:<= code (char-code #\F)))
	(cl:= code (char-code #\_)))))

(defun is-null-or-ws (char)
  (or (null char) (member char '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed ))))

(defun char-to-radix (c)
  (case c
    ((#\b #\B) 2)
    ((#\o #\O) 8)
    ((#\d #\D) 10)
    ((#\h #\H #\x #\X) 16)
    (otherwise (error (format nil "Invalid radix character: ~a" c)))))

(defun read-int (stream)
  (let ((result (make-string-output-stream))
	(radix 10)
	(length 32)
	(value 0)
	(sign #\+))
    ;; Read radix
    (write-string (read-chars-to-string stream #'is-dec
					:end-char-p (lambda (c)
						      (if (member c '(#\b #\B #\o #\O #\d #\D #\h #\H #\x #\X))
							  (progn
							    (setf radix (char-to-radix c))
							    t)
							  nil)))
		  result)
    (setf length (parse-integer (get-output-stream-string result)))
    ;; Try to read sign
    (setf sign (read-char stream nil nil))
    (when (not (member sign '(#\+ #\-)))
      (unread-char sign stream)
      (setf sign #\+))
    ;; Read value
    (case radix
      (2 (write-string (read-chars-to-string stream #'is-bin  :end-char-p #'is-null-or-ws) result))
      (8 (write-string (read-chars-to-string stream #'is-oct  :end-char-p #'is-null-or-ws) result))
      (10 (write-string (read-chars-to-string stream #'is-dec :end-char-p #'is-null-or-ws) result))
      (16 (write-string (read-chars-to-string stream #'is-hex :end-char-p #'is-null-or-ws) result))
      (otherwise (error (format nil "Invalid radix: ~a" radix))))
    (setf value (parse-integer (concatenate 'string (string sign) (remove #\_ (get-output-stream-string result))) :radix radix))
    ;; Return result
    (make-sl-int length (to-int value length))))

(defun read-uint (stream)
  (let ((result (make-string-output-stream))
	(radix 10)
	(length 32)
	(value 0))
    ;; Read radix
    (write-string (read-chars-to-string stream #'is-dec
					:end-char-p (lambda (c)
						      (if (member c '(#\b #\B #\o #\O #\d #\D #\h #\H #\x #\X))
							  (progn
							    (setf radix (char-to-radix c))
							    t)
							  nil)))
		  result)
    (setf length (parse-integer (get-output-stream-string result)))
    ;; Read value
    (case radix
      (2 (write-string (read-chars-to-string stream #'is-bin  :end-char-p #'is-null-or-ws) result))
      (8 (write-string (read-chars-to-string stream #'is-oct  :end-char-p #'is-null-or-ws) result))
      (10 (write-string (read-chars-to-string stream #'is-dec :end-char-p #'is-null-or-ws) result))
      (16 (write-string (read-chars-to-string stream #'is-hex :end-char-p #'is-null-or-ws) result))
      (otherwise (error (format nil "Invalid radix: ~a" radix))))
    (setf value (parse-integer (remove #\_ (get-output-stream-string result)) :radix radix))
    ;; Return result
    (make-sl-uint length (to-uint value length))))

(set-dispatch-macro-character
 #\# #\i
 (lambda (stream sub-char infix)
   (declare (ignore sub-char infix))
   (read-int stream)))

(set-dispatch-macro-character
 #\# #\u
 (lambda (stream sub-char infix)
   (declare (ignore sub-char infix))
   (read-uint stream)))

(defmethod make-load-form ((obj sl-uint) &optional environment)
  (declare (ignore environment))
  `(make-sl-uint ,(sl-uint-bits obj) ,(sl-uint-value obj)))

(defmethod make-load-form ((obj sl-int) &optional environment)
  (declare (ignore environment))
  `(make-sl-int ,(sl-int-bits obj) ,(sl-int-value obj)))

;;; print-object for fbwi
(defmethod print-object ((obj sl-uint) stream)
  (format stream "SL-UINT ~a (~a bits, #x~a, #o~a, #b~a)"
	  (to-string obj :format 'd)
	  (sl-uint-bits obj)
	  (to-string obj :format 'x)
	  (to-string obj :format 'o)
	  (to-string obj :format 'b)))

(defmethod print-object ((obj sl-int) stream)
  (format stream "SL-INT ~a (~a bits, #x~a, #o~a, #b~a)"
	  (to-string obj :format 'd)
	  (sl-int-bits obj)
	  (to-string (uint (sl-int-value obj) :bits (sl-int-bits obj)) :format 'x)
	  (to-string (uint (sl-int-value obj) :bits (sl-int-bits obj)) :format 'o)
	  (to-string (uint (sl-int-value obj) :bits (sl-int-bits obj)) :format 'b)))

;;; bit-get - get nth bit from number
(declaim (inline uint-bit-get))
(defun uint-bit-get (bn x n)
  (let ((ux (to-uint x n)))
    (if (cl:>= bn n) 0
	(if (cl:logbitp bn ux) 1 0))))

(declaim (inline int-bit-get))
(defun int-bit-get (bn x n)
  (let ((sx (to-int x n)))
    (if (cl:>= bn n)
	(if (cl:logbitp (cl:1- n) sx) 1 0)
	(if (cl:logbitp bn sx) 1 0))))

(defmethod bit ((obj sl-uint) (n number))
  (uint-bit-get n (sl-uint-value obj) (sl-uint-bits obj)))

(defmethod bit ((obj sl-int) (n number))
  (int-bit-get n (sl-int-value obj) (sl-int-bits obj)))

(defmethod bit ((obj bit-vector) (n number))
  (cl:bit obj n))

;;; bit-set - set nth bit from number
(declaim (inline uint-bit-set))
(defun uint-bit-set (bn x n &optional (val t))
  (let ((ux (to-uint x n)))
    (if (cl:>= bn n) ux
	(if val
	    (cl:logior ux (cl:ash 1 bn))
	    (cl:logand ux (cl:lognot (cl:ash 1 bn)))))))

(declaim (inline int-bit-set))
(defun int-bit-set (bn x n &optional (val t))
  (let ((sx (to-int x n)))
    (if (cl:>= bn n) sx
	(to-int (if val
		     (cl:logior sx (cl:ash 1 bn))
		     (cl:logand sx (cl:lognot (cl:ash 1 bn))))
		n))))

(defmethod (setf bit) ((val number) (obj sl-uint) (n number))
  (setb obj (uint-bit-set n (sl-uint-value obj) (sl-uint-bits obj) (not (zerop val)))))

(defmethod (setf bit) ((val number) (obj sl-int) (n number))
  (setb obj (int-bit-set n (sl-int-value obj) (sl-int-bits obj) (not (zerop val)))))

;;; bit-get-slice - get bit slice from number, unsigned
(declaim (inline uint-get-slice))
(defun uint-get-slice (hi lo x n)
  (let* ((rhi (cl:max hi lo))
	 (rlo (cl:min hi lo))
	 (slice-len (cl:1+ (cl:- rhi rlo)))
	 (ux (to-uint x n)))
    (values (if (or (cl:>= lo n) (cl:>= hi n))
		0
		(if (cl:> slice-len n)
		    (progn
		      (warn (format nil "uint-get-slice: Slice length ~a is larger than bit length ~a, returning 0." slice-len n))
		      ux)
		    (cl:logand (cl:1- (cl:ash 1 slice-len)) (cl:ash ux (cl:- rlo)))))
	    slice-len)))

(declaim (inline int-get-slice))
(defun int-get-slice (hi lo x n)
  (let* ((rhi (cl:max hi lo))
	 (rlo (cl:min hi lo))
	 (slice-len (cl:1+ (cl:- rhi rlo)))
	 (sx (to-int x n)))
    (values (if (or (cl:>= lo n) (cl:>= hi n))
		0
		(if (cl:> slice-len n)
		    (progn
		      (warn (format nil "uint-get-slice: Slice length ~a is larger than bit length ~a, returning 0." slice-len n))
		      sx)
		    (cl:logand (cl:1- (cl:ash 1 slice-len)) (cl:ash sx (cl:- rlo)))))
	    slice-len)))

;; TODO: do some checks on the indices
(defmethod bits ((obj sl-uint) (hi number) (lo number))
  (multiple-value-bind (value length)
      (uint-get-slice hi lo (sl-uint-value obj) (sl-uint-bits obj))
    (make-sl-uint length value)))

(defmethod bits ((obj sl-int) (hi number) (lo number))
  (multiple-value-bind (value length)
      (int-get-slice hi lo (sl-int-value obj) (sl-int-bits obj))
      (make-sl-int length value)))

(defmethod group ((obj sl-uint) (size number) (index number))
  (let* ((hi (cl:1- (cl:* size (1+ index))))
	 (lo (cl:- hi (cl:1- size))))
    (bits obj hi lo)))

(defmethod group ((obj sl-int) (size number) (index number))
  (let* ((hi (cl:1- (cl:* size (1+ index))))
	 (lo (cl:- hi (cl:1- size))))
    (bits obj hi lo)))

;;; TODO: these don't work, debug them
;;; TODO: debug cases when lo > hi for all of these
(defmethod slice ((obj sl-uint) (start number) (size number))
  (let* ((hi start)
	 (lo (cl:1- (cl:+ start size))))
    (bits obj hi lo)))

(defmethod slice ((obj sl-int) (start number) (size number))
  (let* ((hi start)
	 (lo (cl:1- (cl:+ start size))))
    (bits obj hi lo)))

;;; bit-set-slice - set bit slice in number
(declaim (inline uint-set-slice))
(defun uint-set-slice (hi lo val x n)
  (let* ((rhi (cl:max hi lo))	 
	 (rlo (cl:min hi lo))
	 (slice-len (cl:1+ (cl:- rhi rlo)))
	 (masked-val (cl:logand val (cl:1- (cl:ash 1 slice-len))))
	 (ux (to-uint x n)))
    (if (or (cl:>= lo n) (cl:>= hi n))
	ux
      (if (cl:> slice-len n)
	  (progn
	    (warn (format nil "uint-set-slice: Slice length ~a is larger than bit length ~a, returning unchanged value" slice-len n))
	    ux)
	(cl:logior
	 (cl:logand ux (cl:lognot (cl:ash (cl:1- (cl:ash 1 slice-len)) rlo)))
	 (cl:ash masked-val rlo))))))

(declaim (inline int-set-slice))
(defun int-set-slice (hi lo val x n)
  (let* ((rhi (cl:max hi lo))	 
	 (rlo (cl:min hi lo))
	 (slice-len (cl:1+ (cl:- rhi rlo)))
	 (masked-val (cl:logand val (cl:1- (cl:ash 1 slice-len))))
	 (sx (to-int x n)))
    (if (or (cl:>= lo n) (cl:>= hi n))
	sx
      (if (cl:> slice-len n)
	  (progn
	    (warn (format nil "int-set-slice: Slice length ~a is larger than bit length ~a, returning unchanged value" slice-len n))
	    sx)
	(to-int (cl:logior
			(cl:logand sx (cl:lognot (cl:ash (cl:1- (cl:ash 1 slice-len)) rlo)))
			(cl:ash masked-val rlo))
		       n)))))

(defmethod (setf bits) ((val number) (obj sl-uint) (hi number) (lo number))
  (setf (sl-uint-value obj)
	(uint-set-slice hi lo val (sl-uint-value obj) (sl-uint-bits obj))))

(defmethod (setf bits) ((val number) (obj sl-int) (hi number) (lo number))
  (setf (sl-int-value obj)
	(int-set-slice hi lo val (sl-int-value obj) (sl-int-bits obj))))

;;; bit-and
(declaim (inline uint-and))
(defun uint-and (a an b bn)
  (let ((n (cl:max an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (to-uint (cl:logand ua ub) n)))

(declaim (inline uint-and))
(defun int-and (a an b bn)
  (let ((n (cl:max an bn))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (to-int (cl:logand sa sb) n)))

(defun sl-logand (&rest opnds)
  (reduce 'logand-binary (cdr opnds) :initial-value (car opnds)))

(defgeneric logand-binary (opnd1 opnd2))

(defmethod logand-binary ((opnd1 sl-uint) (opnd2 sl-uint))
  (let ((n (cl:max (sl-uint-bits opnd1)
		   (sl-uint-bits opnd2))))
    (make-sl-uint n (uint-and (sl-uint-value opnd1) (sl-uint-bits opnd1)
			      (sl-uint-value opnd2) (sl-uint-bits opnd2)))))

(defmethod logand-binary ((opnd1 sl-int) (opnd2 sl-int))
  (let ((n (cl:max (sl-int-bits opnd1)
		   (sl-int-bits opnd2))))
    (make-sl-int n (int-and (sl-int-value opnd1) (sl-int-bits opnd1)
		    (sl-int-value opnd2) (sl-int-bits opnd2)))))

(defmethod logand-binary ((opnd1 sl-int) (opnd2 sl-uint))
  (let ((n (cl:max (sl-int-bits opnd1)
		   (sl-uint-bits opnd2))))
    (make-sl-int n (uint-and (to-uint (sl-int-value opnd1)
				    (sl-int-bits opnd1))
			   (sl-int-bits opnd1)
		    (sl-uint-value opnd2) (sl-uint-bits opnd2)))))

(defmethod logand-binary ((opnd1 sl-uint) (opnd2 sl-int))
  (logand-binary opnd2 opnd1))

(defmethod logand-binary ((opnd1 sl-uint) (opnd2 integer))
  (make-sl-uint (sl-uint-bits opnd1) (uint-and (sl-uint-value opnd1)
			  (sl-uint-bits opnd1)
			  opnd2
			  (sl-uint-bits opnd1))))

(defmethod logand-binary ((opnd1 integer) (opnd2 sl-uint))
  (logand-binary opnd2 opnd1))

(defmethod logand-binary ((opnd1 sl-int) (opnd2 integer))
  (make-sl-uint (sl-int-bits opnd1) (int-and (sl-int-value opnd1)
			  (sl-int-bits opnd1)
			  opnd2
			  (sl-int-bits opnd1))))

(defmethod logand-binary ((opnd1 integer) (opnd2 sl-int))
  (logand-binary opnd2 opnd1))

(defmethod logand-binary ((opnd1 number) (opnd2 number))
  (cl:logand opnd1 opnd2))

;;; bit-or
(declaim (inline uint-or))
(defun uint-or (a an b bn)
  (let ((n (cl:max an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (to-uint (cl:logior ua ub) n)))

(declaim (inline int-or))
(defun int-or (a an b bn)
  (let ((n (cl:max an bn))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (to-int (cl:logior sa sb) n)))

(defun sl-logior (&rest opnds)
  (reduce 'logior-binary (cdr opnds) :initial-value (car opnds)))

(defgeneric logior-binary (opnd1 opnd2))

(defmethod logior-binary ((opnd1 sl-uint) (opnd2 sl-uint))
  (let ((n (cl:max (sl-uint-bits opnd1)
		   (sl-uint-bits opnd2))))
    (make-sl-uint n (uint-or (sl-uint-value opnd1) (sl-uint-bits opnd1)
			     (sl-uint-value opnd2) (sl-uint-bits opnd2)))))

(defmethod logior-binary ((opnd1 sl-int) (opnd2 sl-int))
  (let ((n (cl:max (sl-int-bits opnd1)
		   (sl-int-bits opnd2))))
    (make-sl-int n (int-or (sl-int-value opnd1) (sl-int-bits opnd1)
		    (sl-int-value opnd2) (sl-int-bits opnd2)))))

(defmethod logior-binary ((opnd1 sl-int) (opnd2 sl-uint))
  (let ((n (cl:max (sl-int-bits opnd1)
		   (sl-uint-bits opnd2))))
    (make-sl-int n (uint-or (to-uint (sl-int-value opnd1)
				    (sl-int-bits opnd1))
			   (sl-int-bits opnd1)
		    (sl-uint-value opnd2) (sl-uint-bits opnd2)))))

(defmethod logior-binary ((opnd1 sl-uint) (opnd2 sl-int))
  (logior-binary opnd2 opnd1))

(defmethod logior-binary ((opnd1 sl-uint) (opnd2 integer))
  (make-sl-uint (sl-uint-bits opnd1) (uint-or (sl-uint-value opnd1)
			  (sl-uint-bits opnd1)
			  opnd2
			  (sl-uint-bits opnd1))))

(defmethod logior-binary ((opnd1 integer) (opnd2 sl-uint))
  (logior-binary opnd2 opnd1))

(defmethod logior-binary ((opnd1 sl-int) (opnd2 integer))
  (make-sl-uint (sl-int-bits opnd1) (int-or (sl-int-value opnd1)
			  (sl-int-bits opnd1)
			  opnd2
			  (sl-int-bits opnd1))))

(defmethod logior-binary ((opnd1 integer) (opnd2 sl-int))
  (logior-binary opnd2 opnd1))

(defmethod logior-binary ((opnd1 number) (opnd2 number))
  (cl:logior opnd1 opnd2))

;;; bit-xor
(declaim (inline uint-xor))
(defun uint-xor (a an b bn)
  (let ((n (cl:max an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (to-uint (cl:logxor ua ub) n)))

(declaim (inline int-xor))
(defun int-xor (a an b bn)
  (let ((n (cl:max an bn))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (to-int (cl:logxor sa sb) n)))

(defun sl-logxor (&rest opnds)
  (reduce 'logxor-binary (cdr opnds) :initial-value (car opnds)))

(defgeneric logxor-binary (opnd1 opnd2))

(defmethod logxor-binary ((opnd1 sl-uint) (opnd2 sl-uint))
  (let ((n (cl:max (sl-uint-bits opnd1)
		   (sl-uint-bits opnd2))))
    (make-sl-uint n (uint-xor (sl-uint-value opnd1) (sl-uint-bits opnd1)
			      (sl-uint-value opnd2) (sl-uint-bits opnd2)))))

(defmethod logxor-binary ((opnd1 sl-int) (opnd2 sl-int))
  (let ((n (cl:max (sl-int-bits opnd1)
		   (sl-int-bits opnd2))))
    (make-sl-int n (int-xor (sl-int-value opnd1) (sl-int-bits opnd1)
		    (sl-int-value opnd2) (sl-int-bits opnd2)))))

(defmethod logxor-binary ((opnd1 sl-int) (opnd2 sl-uint))
  (let ((n (cl:max (sl-int-bits opnd1)
		   (sl-uint-bits opnd2))))
    (make-sl-int n (uint-xor (to-uint (sl-int-value opnd1)
				    (sl-int-bits opnd1))
			   (sl-int-bits opnd1)
		    (sl-uint-value opnd2) (sl-uint-bits opnd2)))))

(defmethod logxor-binary ((opnd1 sl-uint) (opnd2 sl-int))
  (logxor-binary opnd2 opnd1))

(defmethod logxor-binary ((opnd1 sl-uint) (opnd2 integer))
  (make-sl-uint (sl-uint-bits opnd1) (uint-xor (sl-uint-value opnd1)
					       (sl-uint-bits opnd1)
					       opnd2
					       (sl-uint-bits opnd1))))

(defmethod logxor-binary ((opnd1 integer) (opnd2 sl-uint))
  (logxor-binary opnd2 opnd1))

(defmethod logxor-binary ((opnd1 sl-int) (opnd2 integer))
  (make-sl-uint (sl-int-bits opnd1) (int-xor (sl-int-value opnd1)
			  (sl-int-bits opnd1)
			  opnd2
			  (sl-int-bits opnd1))))

(defmethod logxor-binary ((opnd1 integer) (opnd2 sl-int))
  (logxor-binary opnd2 opnd1))

(defmethod logxor-binary ((opnd1 number) (opnd2 number))
  (cl:logxor opnd1 opnd2))

;;; bit-not
(declaim (inline uint-not))
(defun uint-not (x n)
  (to-uint (cl:lognot x) n))

(declaim (inline int-not))
(defun int-not (x n)
  (to-int (cl:lognot x) n))

(defgeneric lognot-unary (opnd))

(defun sl-lognot (x)
  (funcall #'lognot-unary x))

(defmethod lognot-unary ((opnd sl-uint))
  (make-sl-uint (sl-uint-bits opnd)
		(uint-not (sl-uint-value opnd)
			  (sl-uint-bits opnd))))

(defmethod lognot-unary ((opnd number))
  (cl:lognot opnd))

;;; bit-and-reduce
(declaim (inline bit-and-reduce))
(defun bit-and-reduce (x n)
  (let ((ux (to-uint x n)))
    (if (cl:/= ux (1- (cl:ash 1 n))) 0 1)))

(defmethod and-reduce ((obj sl-uint))
  (bit-and-reduce (sl-uint-value obj) (sl-uint-bits obj)))

(defmethod and-reduce ((obj sl-int))
  (bit-and-reduce (sl-int-value obj) (sl-int-bits obj)))

;;; bit-or-reduce
(declaim (inline bit-or-reduce))
(defun bit-or-reduce (x n)
  (let ((ux (to-uint x n)))
    (if (cl:/= ux 0) 1 0)))

(defmethod or-reduce ((obj sl-uint))
  (bit-or-reduce (sl-uint-value obj) (sl-uint-bits obj)))

(defmethod or-reduce ((obj sl-int))
  (bit-or-reduce (sl-int-value obj) (sl-int-bits obj)))

;; bit-one-count
(declaim (inline bit-one-count))
(defun bit-one-count (x n)
  (let ((ux (to-uint x n))
	(count 0))
    (loop for i from 0 to (cl:1- n) do
      (if (cl:logbitp i ux) (incf count)))
    count))

(defmethod one-count ((obj sl-uint))
  (bit-one-count (sl-uint-value obj)
		 (sl-uint-bits obj)))

(defmethod one-count ((obj sl-int))
  (bit-one-count (sl-int-value obj)
		 (sl-int-bits obj)))

;;; bit-xor-reduce
(declaim (inline bit-xor-reduce))
(defun bit-xor-reduce (x n)
  (let ((ux (to-uint x n)))
    (if (or (cl:equal ux 0) (cl:eq (cl:mod (bit-one-count ux n) 2) 0)) 0 1)))

(defmethod xor-reduce ((obj sl-uint))
  (bit-xor-reduce (sl-uint-value obj) (sl-uint-bits obj)))

(defmethod xor-reduce ((obj sl-int))
  (bit-xor-reduce (sl-int-value obj) (sl-int-bits obj)))

;;; bit-rsh
(declaim (inline uint-rsh))
(defun uint-rsh (count x n)
  (let ((ux (to-uint x n)))
    (to-uint (cl:ash ux (cl:- count)) n)))

(declaim (inline int-rsh))
(defun int-rsh (count x n)
  (let ((sx (to-int x n)))
    (to-int (cl:ash sx (cl:- count)) n)))

(defmethod rsh ((obj sl-uint) (count number))
  (make-sl-uint (sl-uint-bits obj)
		(uint-rsh count (sl-uint-value obj) (sl-uint-bits obj))))

(defmethod rsh ((obj sl-int) (count number))
  (make-sl-int (sl-int-bits obj)
		(int-rsh count (sl-int-value obj) (sl-int-bits obj))))

;;; bit-lsh
(declaim (inline uint-lsh))
(defun uint-lsh (count x n)
  (let ((ux (to-uint x n)))
    (to-uint (cl:ash ux count) n)))

(declaim (inline int-lsh))
(defun int-lsh (count x n)
  (let ((sx (to-int x n)))
    (to-int (cl:ash sx count) n)))

(defmethod lsh ((obj sl-uint) (count number))
  (make-sl-uint (sl-uint-bits obj)
		(uint-lsh count (sl-uint-value obj) (sl-uint-bits obj))))

(defmethod lsh ((obj sl-int) (count number))
  (make-sl-int (sl-int-bits obj)
		(int-lsh count (sl-int-value obj) (sl-int-bits obj))))

;;; bit-rotr
(declaim (inline uint-rotr))
(defun uint-rotr (count x n)
  (let* ((ux (to-uint x n))
	 (lost-slice (uint-get-slice (cl:1- count) 0 ux n))
	 (ux-rsh (uint-rsh count ux n)))
    (uint-set-slice (cl:1- n) (cl:1+ (cl:- (cl:1- n) count)) lost-slice ux-rsh n)))

(declaim (inline int-rotr))
(defun int-rotr (count x n)
  (let* ((sx (to-int x n))
	 (lost-slice (int-get-slice (cl:1- count) 0 sx n))
	 (sx-rsh (int-rsh count sx n)))
    (int-set-slice (cl:1- n) (cl:1+ (cl:- (cl:1- n) count)) lost-slice sx-rsh n)))

(defmethod rotr ((obj sl-uint) (count number))
  (make-sl-uint (sl-uint-bits obj)
		(uint-rotr count (sl-uint-value obj) (sl-uint-bits obj))))

(defmethod rotr ((obj sl-int) (count number))
  (make-sl-int (sl-int-bits obj)
		(int-rotr count (sl-int-value obj) (sl-int-bits obj))))

;;; bit-rotl
(declaim (inline uint-rotl))
(defun uint-rotl (count x n)
  (let* ((ux (to-uint x n))
	 (lost-slice (uint-get-slice (cl:1- n) (cl:1+ (cl:- (cl:1- n) count)) ux n))
	 (ux-lsh (uint-lsh count x n)))
    (uint-set-slice (cl:1- count) 0 lost-slice ux-lsh n)))

(declaim (inline int-rotl))
(defun int-rotl (count x n)
  (let* ((sx (to-int x n))
	 (lost-slice (uint-get-slice (cl:1- n) (cl:1+ (cl:- (cl:1- n) count)) sx n))
	 (sx-lsh (int-lsh count sx n)))
    (int-set-slice (cl:1- count) 0 lost-slice sx-lsh n)))

(defmethod rotl ((obj sl-uint) (count number))
  (make-sl-uint (sl-uint-bits obj)
		(uint-rotl count (sl-uint-value obj) (sl-uint-bits obj))))

(defmethod rotl ((obj sl-int) (count number))
  (make-sl-int (sl-int-bits obj)
		(int-rotl count (sl-int-value obj) (sl-int-bits obj))))

;;; Concatenate bits
(declaim (inline uint-concat))
(defun uint-concat (a an b bn)
  (let ((n (cl:+ an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (values (to-uint (cl:+ (uint-set-slice (cl:1- n) (cl:1+ (cl:- (cl:1- n) an)) ua 0 n)
				(uint-set-slice (cl:1- bn) 0 ub 0 n))
			     n)
	    n)))

(declaim (inline int-concat))
(defun int-concat (a an b bn)
  (let ((n (cl:+ an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (values (to-int (cl:+ (int-set-slice (cl:1- n) (cl:1+ (cl:- (cl:1- n) an)) ua 0 n)
			      (int-set-slice (cl:1- bn) 0 ub 0 n))
			   n)
	    n)))

(defgeneric concat-binary (opnd1 opnd2))

(defun concat (&rest opnds)
  (reduce 'concat-binary (cdr opnds) :initial-value (car opnds)))

(defmethod concat-binary ((opnd1 sl-uint) (opnd2 sl-uint))
  (multiple-value-bind (value bits) (uint-concat (sl-uint-value opnd1) (sl-uint-bits opnd1)
						 (sl-uint-value opnd2) (sl-uint-bits opnd2))
    (make-sl-uint bits value)))

(defmethod concat-binary ((opnd1 sl-int) (opnd2 sl-int))
  (multiple-value-bind (value bits) (int-concat (sl-int-value opnd1) (sl-int-bits opnd1)
				      (sl-int-value opnd2) (sl-int-bits opnd2))
    (make-sl-int bits value)))

(defmethod concat-binary ((opnd1 sl-uint) (opnd2 sl-int))
  (multiple-value-bind (value bits) (uint-concat (sl-uint-value opnd1) (sl-uint-bits opnd1)
				       (to-uint (sl-int-value opnd2) (sl-int-bits opnd2)) (sl-int-bits opnd2))
    (make-sl-uint bits value)))

(defmethod concat-binary ((opnd1 sl-int) (opnd2 sl-uint))
  (multiple-value-bind (value bits) (uint-concat (to-uint (sl-int-value opnd1) (sl-int-bits opnd1)) (sl-int-bits opnd1)
				       (sl-uint-value opnd2) (sl-uint-bits opnd2))
      (make-sl-int bits value)))

(defmethod concat-binary ((opnd1 sl-uint) (opnd2 integer))
  (let ((opnd2-length (if (zerop opnd2) 1 (integer-length opnd2))))
    (multiple-value-bind (value bits) (uint-concat (sl-uint-value opnd1) (sl-uint-bits opnd1)
						    opnd2 opnd2-length)
    (make-sl-uint bits value))))

(defmethod concat-binary ((opnd1 integer) (opnd2 sl-uint))
  (let ((opnd1-length (if (zerop opnd1) 1 (integer-length opnd1))))
    (multiple-value-bind (value bits) (uint-concat  opnd1 opnd1-length
						    (sl-uint-value opnd2) (sl-uint-bits opnd2))
    (make-sl-uint bits value))))

(defmethod concat-binary ((opnd1 sl-int) (opnd2 integer))
  (let ((opnd2-length (if (zerop opnd2) 1 (integer-length opnd2))))
    (multiple-value-bind (value bits) (int-concat (sl-int-value opnd1) (sl-int-bits opnd1)
						    opnd2 opnd2-length)
    (make-sl-int bits value))))

(defmethod concat-binary ((opnd1 integer) (opnd2 sl-int))
  (let ((opnd1-length (if (zerop opnd1) 1 (integer-length opnd1))))
    (multiple-value-bind (value bits) (int-concat  opnd1 opnd1-length
						    (sl-int-value opnd2) (sl-int-bits opnd2))
    (make-sl-int bits value))))

(defmethod concat-binary ((opnd1 integer) (opnd2 integer))
  (let ((opnd1-length (if (zerop opnd1) 1 (integer-length opnd1)))
	(opnd2-length (if (zerop opnd2) 1 (integer-length opnd2))))
    (multiple-value-bind (value bits) (int-concat  opnd1 opnd1-length
						   opnd2 opnd2-length)
    (make-sl-uint bits value))))

;;; + - * / rem mod div pow/** > < >= <= == != floor ceiling
;;; Operator overloading macros

;;; For commutative operators
(defmacro overload-com-op (op cl-op unary-val binmet uint-llf int-llf)
  `(progn
     (defgeneric ,binmet (a b))

     (defun ,op (&rest opnds)
       (if (null (cdr opnds))
	   ;; then
	   (,op ,unary-val (car opnds))
	   ;; else
	   (reduce (quote ,binmet) (cdr opnds) :initial-value (car opnds))))
     
     (defmethod ,binmet ((a sl-uint) (b sl-uint))
       (multiple-value-bind (value bits) (,uint-llf (sl-uint-value a) (sl-uint-bits a)
						    (sl-uint-value b) (sl-uint-bits b))
	 (make-sl-uint bits value)))

     (defmethod ,binmet ((a sl-int) (b sl-int))
       (multiple-value-bind (value bits) (,int-llf (sl-int-value a) (sl-int-bits a)
						   (sl-int-value b) (sl-int-bits b))
	 (make-sl-int bits value)))

     (defmethod ,binmet ((a sl-uint) (b sl-int))
       (multiple-value-bind (value bits) (,uint-llf (sl-uint-value a) (sl-uint-bits a)
						    (to-uint (sl-int-value b) (sl-int-bits b)) (sl-int-bits b))
	 (make-sl-uint bits value)))

     (defmethod ,binmet ((a sl-int) (b sl-uint))
       (,binmet b a))

     (defmethod ,binmet ((a sl-uint) (b number))
       (multiple-value-bind (value bits) (,uint-llf (sl-uint-value a) (sl-uint-bits a)
						    (to-uint b (sl-uint-bits a)) (sl-uint-bits a))
	 (make-sl-uint bits value)))

     (defmethod ,binmet ((a number) (b sl-uint))
       (,binmet b a))

     (defmethod ,binmet ((a sl-int) (b number))
       (multiple-value-bind (value bits) (,int-llf (sl-int-value a) (sl-int-bits a)
						   (to-int b (sl-int-bits a)) (sl-int-bits a))
	 (make-sl-int bits value)))

     (defmethod ,binmet ((a number) (b sl-int))
       (,binmet b a))

     (defmethod ,binmet ((a number) (b number))
       (,cl-op a b))))

;;; For non-commutative operators
(defmacro overload-noncom-op (op cl-op unary-val binmet uint-llf int-llf)
  `(progn
     (defgeneric ,binmet (a b))

     (defun ,op (&rest opnds)
       (if (null (cdr opnds))
	   ;; then
	   (,op ,unary-val (car opnds))
	   ;; else
	   (reduce (quote ,binmet) (cdr opnds) :initial-value (car opnds))))

     (defmethod ,binmet ((a sl-uint) (b sl-uint))
       (multiple-value-bind (value bits) (,uint-llf (sl-uint-value a) (sl-uint-bits a)
						    (sl-uint-value b) (sl-uint-bits b))
	 (make-sl-uint bits value)))

     (defmethod ,binmet ((a sl-int) (b sl-int))
       (multiple-value-bind (value bits) (,int-llf (sl-int-value a) (sl-int-bits a)
						   (sl-int-value b) (sl-int-bits b))
	 (make-sl-int bits value)))

     (defmethod ,binmet ((a sl-uint) (b sl-int))
       (multiple-value-bind (value bits) (,uint-llf (sl-uint-value a) (sl-uint-bits a)
						    (to-uint (sl-int-value b) (sl-int-bits b)) (sl-int-bits b))
	 (make-sl-uint bits value)))

     (defmethod ,binmet ((a sl-int) (b sl-uint))
       (multiple-value-bind (value bits) (,uint-llf (to-uint (sl-int-value a) (sl-int-bits a)) (sl-int-bits a)
						    (sl-uint-value b) (sl-uint-bits b))
	 (make-sl-uint bits value)))

     (defmethod ,binmet ((a sl-uint) (b number))
       (multiple-value-bind (value bits) (,uint-llf (sl-uint-value a) (sl-uint-bits a)
						    (to-uint b (sl-uint-bits a)) (sl-uint-bits a))
	 (make-sl-uint bits value)))

     (defmethod ,binmet ((a number) (b sl-uint))
       (multiple-value-bind (value bits) (,uint-llf (to-uint a (sl-uint-bits b)) (sl-uint-bits b)
						    (sl-uint-value b) (sl-uint-bits b))
	 (make-sl-uint bits value)))

     (defmethod ,binmet ((a sl-int) (b number))
       (multiple-value-bind (value bits) (,int-llf (sl-int-value a) (sl-int-bits a)
						   (to-int b (sl-int-bits a)) (sl-int-bits a))
	 (make-sl-int bits value)))

     (defmethod ,binmet ((a number) (b sl-int))
       (multiple-value-bind (value bits) (,int-llf (to-int a (sl-int-bits b)) (sl-int-bits b)
						   (sl-int-value b) (sl-int-bits b))
	 (make-sl-int bits value)))

     (defmethod ,binmet ((a number) (b number))
       (,cl-op a b))))

;;; For optimized operators
(defmacro gen-optimized-ops (op-sign op-name)
  (let ((uint-binary (read-from-string (concatenate 'string "uint-" (string op-name))))
	(int-binary (read-from-string (concatenate 'string "int-" (string op-name))))
	(uint-unary (read-from-string (concatenate 'string "uint-" (string op-name) "-unary")))
	(int-unary (read-from-string (concatenate 'string "int-" (string op-name) "-unary")))
	(sl-uu (read-from-string (concatenate 'string "sl-uu" (string op-sign))))
	(sl-uu-u (read-from-string (concatenate 'string "sl-uu" (string op-sign) "u")))
	(sl-ii (read-from-string (concatenate 'string "sl-ii" (string op-sign))))
	(sl-ii-u (read-from-string (concatenate 'string "sl-ii" (string op-sign) "u")))
	(sl-ui (read-from-string (concatenate 'string "sl-ui" (string op-sign))))
	(sl-iu (read-from-string (concatenate 'string "sl-iu" (string op-sign))))
	(sl-un (read-from-string (concatenate 'string "sl-un" (string op-sign))))
	(sl-nu (read-from-string (concatenate 'string "sl-nu" (string op-sign))))
	(sl-in (read-from-string (concatenate 'string "sl-in" (string op-sign))))
	(sl-ni (read-from-string (concatenate 'string "sl-ni" (string op-sign))))
	(uu-op (read-from-string (concatenate 'string "uu" (string op-sign))))
	(ii-op (read-from-string (concatenate 'string "ii" (string op-sign)))))
    `(progn
       (declaim (inline ,sl-uu))
       (defun ,sl-uu (a b)
	 (declare (type sl-uint a))
	 (declare (type sl-uint b))
	 (multiple-value-bind (value bits)
	     (,uint-binary (sl-uint-value a) (sl-uint-bits a)
		       (sl-uint-value b) (sl-uint-bits b))
	   (make-sl-uint bits value)))

       (declaim (inline ,sl-uu-u))
       (defun ,sl-uu-u (a)
	 (declare (type sl-uint a))
	 (multiple-value-bind (value bits)
	     (,uint-unary (sl-uint-value a) (sl-uint-bits a))
	   (make-sl-uint bits value)))

       (defmacro ,uu-op (&rest args)
	 (if (null (cdr args))
	     (fare-quasiquote:quasiquote (,sl-uu-u (fare-quasiquote:unquote (car args)))) ;; Base case: if only one argument, return it
	     ;; `(sl-uu+ ,(car args) (uu+ ,@(cdr args)))
	     ;; (fare-quasiquote:quasiquote (,sl-uu (fare-quasiquote:unquote (car args)) (,uu-op (fare-quasiquote:unquote-splicing (cdr args)))))
	     (nest-op-call (quote ,sl-uu) args)))

       (declaim (inline ,sl-ii))
       (defun ,sl-ii (a b)
	 (declare (type sl-int a))
	 (declare (type sl-int b))
	 (multiple-value-bind (value bits)
	     (,int-binary (sl-int-value a) (sl-int-bits a)
		      (sl-int-value b) (sl-int-bits b))
	   (make-sl-int bits value)))

       (declaim (inline ,sl-ii-u))
       (defun ,sl-ii-u (a)
	 (declare (type sl-int a))
	 (multiple-value-bind (value bits)
	     (,int-unary (sl-int-value a) (sl-int-bits a))
	   (make-sl-int bits value)))

       (defmacro ,ii-op (&rest args)
	 (if (null (cdr args))
	     (fare-quasiquote:quasiquote (,sl-ii-u (fare-quasiquote:unquote (car args)))) ;; Base case: if only one argument, return it
	     ;; `(sl-ii+ ,(car args) (ii+ ,@(cdr args)))
	     ;; (fare-quasiquote:quasiquote (,sl-ii (fare-quasiquote:unquote (car args)) (,ii-op (fare-quasiquote:unquote-splicing (cdr args)))))
	     (nest-op-call (quote ,sl-ii) args)))

       (declaim (inline ,sl-ui))
       (defun ,sl-ui (a b)
	 (declare (type sl-uint a))
	 (declare (type sl-int b))
	 (multiple-value-bind (value bits)
	     (,uint-binary (sl-uint-value a) (sl-uint-bits a)
		       (to-uint (sl-int-value b) (sl-int-bits b)) (sl-int-bits b))
	   (make-sl-uint bits value)))

       (declaim (inline ,sl-iu))
       (defun ,sl-iu (a b)
	 (declare (type sl-int a))
	 (declare (type sl-uint b))
	 (multiple-value-bind (value bits)
	     (,uint-binary (to-uint (sl-int-value a) (sl-int-bits a)) (sl-int-bits a)
		       (sl-uint-value b) (sl-uint-bits b))
	   (make-sl-uint bits value)))

       (declaim (inline ,sl-un))
       (defun ,sl-un (a b)
	 (declare (type sl-uint a))
	 (declare (type number b))
	 (multiple-value-bind (value bits)
	     (,uint-binary  (sl-uint-value a) (sl-uint-bits a)
			b (sl-uint-bits a))
	   (make-sl-uint bits value)))

       (declaim (inline ,sl-nu))
       (defun ,sl-nu (a b)
	 (declare (type number a))
	 (declare (type sl-uint b))
	 (multiple-value-bind (value bits)
	     (,uint-binary  a (sl-uint-bits b)
			(sl-uint-value b) (sl-uint-bits b))
	   (make-sl-uint bits value)))

       (declaim (inline ,sl-ni))
       (defun ,sl-in (a b)
	 (declare (type sl-int a))
	 (declare (type number b))
	 (multiple-value-bind (value bits)
	     (,uint-binary  (sl-int-value a) (sl-int-bits a)
			b (sl-int-bits a))
	   (make-sl-int bits value)))

       (declaim (inline ,sl-in))
       (defun ,sl-ni (a b)
	 (declare (type number a))
	 (declare (type sl-int b))
	 (multiple-value-bind (value bits)
	     (,uint-binary  a (sl-int-bits b)
			(sl-int-value b) (sl-int-bits b))
	   (make-sl-uint bits value))))))

(defun nest-op-call (op args)
  (if (null (cdr args))
      (car args)
      (reduce (lambda (a b) `(,op ,a ,b)) args)))

;;; Add
(declaim (inline uint-add))
(defun uint-add (a an b bn)
  (let ((n (cl:max an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (values (to-uint (cl:+ ua ub) n) n)))

(declaim (inline uint-add-unary))
(defun uint-add-unary (a an)
  (values (to-uint a an) an))

(declaim (inline int-add))
(defun int-add (a an b bn)
  (let ((n (cl:max an bn))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (values (to-int (cl:+ sa sb) n) n)))

(declaim (inline int-add-unary))
(defun int-add-unary (a an)
  (values (to-int a an) an))

(gen-optimized-ops + add)

(overload-com-op sl+ cl:+ 0 binary+ uint-add int-add)

;;; Sub
(declaim (inline uint-sub))
(defun uint-sub (a an b bn)
  (let ((n (cl:max an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (values (to-uint (cl:- ua ub) n) n)))

(declaim (inline uint-sub-unary))
(defun uint-sub-unary (a an)
  (values (to-uint (cl:- a) an) an))

(declaim (inline int-sub))
(defun int-sub (a an b bn)
  (let ((n (cl:max an bn))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (values (to-int (cl:- sa sb) n) n)))

(declaim (inline int-sub-unary))
(defun int-sub-unary (a an)
  (values (to-int (cl:- a) an) an))

(gen-optimized-ops - sub)

(overload-noncom-op sl- cl:- 0 binary- uint-sub int-sub)

;;; Mul
(declaim (inline uint-mul))
(defun uint-mul (a an b bn)
  (let ((n (cl:+ an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (values (to-uint (cl:* ua ub) n) n)))

(declaim (inline uint-mul-unary))
(defun uint-mul-unary (a an)
  (values (to-uint a an) an))

(declaim (inline int-mul))
(defun int-mul (a an b bn)
  (let ((n (cl:max an bn))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (values (to-int (cl:* sa sb) n) n)))

(declaim (inline int-mul-unary))
(defun int-mul-unary (a an)
  (values (to-int a an) an))

(gen-optimized-ops * mul)

(overload-com-op sl* cl:* 1 binary* uint-mul int-mul)

;; Div
(declaim (inline uint-div))
(defun uint-div (a an b bn)
  (let ((n (cl:max an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (values (to-uint (cl:truncate ua ub) n) n)))

(declaim (inline uint-div-unary))
(defun uint-div-unary (a an)
  (values (to-uint (cl:truncate 1 a) an) an))

(declaim (inline int-div))
(defun int-div (a an b bn)
  (let ((n (cl:max an bn))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (values (to-int (cl:truncate sa sb) n) n)))

(declaim (inline uint-div-unary))
(defun int-div-unary (a an)
  (values (to-int (cl:truncate 1 a) an) an))

(gen-optimized-ops / div)

(overload-noncom-op sl/ cl:/ 1 binary/ uint-div int-div)

;; 1+
(defgeneric sl1+ (x))

(defmethod sl1+ ((x sl-uint))
  (sl+ x 1))

(defmethod sl1+ ((x sl-int))
  (sl+ x 1))

(defmethod sl1+ ((x number))
  (sl+ x 1))

;; 1-
(defgeneric sl1- (x))

(defmethod sl1- ((x sl-uint))
  (sl- x 1))

(defmethod sl1- ((x sl-int))
  (sl- x 1))

(defmethod sl1- ((x number))
  (sl- x 1))

;; mod
(declaim (inline uint-mod))
(defun uint-mod (a an b bn)
  (let ((n (cl:max an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (values (to-uint (cl:mod ua ub) n) n)))

(declaim (inline int-mod))
(defun int-mod (a an b bn)
  (let ((n (cl:max an bn))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (values (to-int (cl:mod sa sb) n) n)))

(overload-noncom-op sl-mod cl:mod 1 mod-binary uint-mod int-mod)

;; rem
(declaim (inline uint-rem))
(defun uint-rem (a an b bn)
  (let ((n (cl:max an bn))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (values (to-uint (cl:rem ua ub) n) n)))

(declaim (inline int-rem))
(defun int-rem (a an b bn)
  (let ((n (cl:max an bn))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (values (to-int (cl:rem sa sb) n) n)))

(overload-noncom-op sl-rem cl:rem 1 rem-binary uint-rem int-rem)

;; expt
(declaim (inline uint-pow))
(defun uint-pow (a an b bn)
  (let ((n (cl:* bn an))
	(ua (to-uint a an))
	(ub (to-uint b bn)))
    (values (to-uint (cl:expt ua ub) n) n)))

(declaim (inline int-pow))
(defun int-pow (a an b bn)
  (let ((n (cl:* bn an))
	(sa (to-int a an))
	(sb (to-int b bn)))
    (values (cond
	      ((and (cl:> sa 1) (cl:<= sb -1)) 0)
	      (t (to-int (cl:expt sa sb) n)))
	    n)))

(overload-noncom-op sl-expt cl:expt 1 expt-binary uint-pow int-pow)

;; floor

;; ceiling

;; truncate

;;; Comparison operators

(defmacro gen-comp-op (comp-op comp-op-cl)
  (let ((binop (read-from-string (concatenate 'string "binary" (string comp-op))))
	(sl-op (read-from-string (concatenate 'string "sl" (string comp-op)))))
    `(progn
       (defgeneric ,binop (a b))

       (defun ,sl-op (first &rest others)
	 (loop for (a b) on (cons first others) while b always (,binop a b)))

       (defmethod ,binop ((a number) (b number))
	 (,comp-op-cl a b))

       (defmethod ,binop ((a sl-uint) (b sl-uint))
	 (let ((n (cl:max (sl-uint-bits a) (sl-uint-bits b))))
	   (,comp-op-cl (to-uint (sl-uint-value a) n) (to-uint (sl-uint-value b) n))))

       (defmethod ,binop ((a sl-int) (b sl-int))
	 (let ((n (cl:max (sl-int-bits a) (sl-int-bits b))))
	   (,comp-op-cl (to-int (sl-int-value a) n) (to-int (sl-int-value b) n))))

       (defmethod ,binop ((a sl-uint) (b sl-int))
	 (let ((n (cl:max (sl-uint-bits a) (sl-int-bits b))))
	   (,comp-op-cl (to-uint (sl-uint-value a) n) (to-uint (sl-int-value b) n))))

       (defmethod ,binop ((a sl-int) (b sl-uint))
	 (let ((n (cl:max (sl-int-bits a) (sl-uint-bits b))))
	   (,comp-op-cl (to-uint (sl-int-value a) n) (to-uint (sl-uint-value b) n))))

       (defmethod ,binop ((a sl-uint) (b number))
	 (,comp-op-cl (to-uint (sl-uint-value a) (sl-uint-bits a)) b))

       (defmethod ,binop ((a number) (b sl-uint))
	 (,comp-op-cl a (to-uint (sl-uint-value b) (sl-uint-bits b))))

       (defmethod ,binop ((a sl-int) (b number))
	 (,comp-op-cl (to-int (sl-int-value a) (sl-int-bits a)) b))

       (defmethod ,binop ((a number) (b sl-int))
	 (,comp-op-cl a (to-uint (sl-int-value b) (sl-int-bits b)))))))

;;; TODO: generate optimized comparison operators

;;; Equality =
(gen-comp-op = cl:=)

(gen-comp-op > cl:>)

(gen-comp-op >= cl:>=)

(gen-comp-op < cl:<)

(gen-comp-op <= cl:<=)

;;; to-string - method to convert bit vector to string
(defmethod nr-to-string ((x number) (n number) &optional (format "b"))
  (let ((fmt-str (concatenate 'string "~(~" (write-to-string n) ",'0" format  "~)")))
    (format nil fmt-str (to-uint x n))))

(defmethod to-string ((x sl-uint) &key (max-length 64) (format 'b))
  (let* ((n (cl:min max-length (sl-uint-bits x)))
	 (cn (case format
	       (b n)
	       (o (cl:ceiling n 3))
	       (d (cl:ceiling (cl:* n (log 2 10))))
	       (x (cl:ceiling n 4))
	       (otherwise n)))
	 (fmt-str (concatenate 'string "~(~" (write-to-string cn) ",'0" (string format)  "~)")))
    (format nil fmt-str (sl-uint-value x))))

(defmethod to-string ((x sl-int) &key (max-length 64) (format 'b))
  (let* ((n (cl:min max-length (sl-int-bits x)))
	 (cn (case format
	       (b n)
	       (o (cl:ceiling n 3))
	       (d (cl:ceiling (cl:* n (log 2 10))))
	       (x (cl:ceiling n 4))
	       (otherwise n)))
	 (fmt-str (concatenate 'string "~(~" (write-to-string cn) ",'0" (string format)  "~)")))
    (format nil fmt-str (sl-int-value x))))

(defmethod to-string ((x number) &key (max-length 64) (format 'b))
  (let* ((n max-length)
	 (cn (case format
	       (b n)
	       (o (cl:ceiling n 3))
	       (d (cl:ceiling (cl:* n (log 2 10))))
	       (x (cl:ceiling n 4))
	       (otherwise n)))
	 (fmt-str (concatenate 'string "~(~" (write-to-string cn) ",'0" (string format)  "~)")))
    (format nil fmt-str x)))

;;; TODO
;;; optimized operations for 8bit, 16bit and 32bit numbers
;;; from-string - convert string to bit vector
;;; Multidimensional packed array of unsigned bit vectors
;;; Multidimensional packed array of signed bit vectors
;;; write comprehensive test suite for this
;;; add extra checks on arguments

;;; fix bugs in bit selection operators
;;; add unit tests for everything
