(in-package :system-lisp)

;;; VCD tracing
(defvar *vcd-id-cnt* 0 "Counter used to generate VCD signal codes. Incremented after each new id is generated")

(defun vcd-name-code (n)
  (let ((code (make-array 0 :adjustable t :element-type 'character :fill-pointer 0))
	(q 0)
	(r 0))
    (multiple-value-setq (q r) (floor n 94))
    (vector-push-extend (code-char (+ r 33)) code)
    (loop while (> q 0) do
      (multiple-value-setq (q r) (floor q 94))
      (vector-push-extend (code-char (+ r 33)) code))
    code))

(defun get-vcd-name-code ()
  (let ((code (vcd-name-code *vcd-id-cnt*)))
    (incf *vcd-id-cnt*)
    code))

;;; Check if slot is signal or component
(defun is-signal-slot (slot)
  (let ((slot-initform (c2mop:slot-definition-initform slot))
	(slot-type (c2mop:slot-definition-type slot))
	(signal-types '(sl-signal-binary sl-signal-number sl-signal-binary-lambda sl-signal-number-lambda)))
    (or (and (listp slot-initform)
	     (equal (first slot-initform) 'make-instance)
	     (>= (length slot-initform) 2)
	     (member (second (second slot-initform)) signal-types :test #'equal))
	(and (symbolp slot-type)
	     (member slot-type signal-types :test #'equal)))))

(defun is-component-slot (slot)
  (let ((slot-initform (c2mop:slot-definition-initform slot)))
    (and (listp slot-initform)
	 (equal (first slot-initform) 'make-instance)
	 (>= (length slot-initform) 2)
	 (eq (second (second slot-initform)) 'sl-component))))

(defun get-signals (obj)
  (loop for slot in (c2mop:class-slots (class-of obj))
	when (is-signal-slot slot)
	  collect (concatenate 'list (list :name (c2mop:slot-definition-name slot)) '(:type) (rest (c2mop:slot-definition-initform slot)))))

;;; VCD sqlite database

;; Create the database
(defun create-vcd-db (db-name)
  (let ((db (sqlite:connect db-name)))
    (sqlite:execute-non-query db "create table scopes (name text not null, type integer not null, parent integer)")
    (sqlite:execute-non-query db "create table signals (name text not null, vcdid text not null, type integer not null, size integer, scope integer, reset_value text not null)")
    (sqlite:execute-non-query db "create table waves (simtime integer not null, signal integer not null, value text not null)")

    (sqlite:execute-non-query db "pragma journal_mode = WAL")
    (sqlite:execute-non-query db "pragma synchronous = NORMAL")
    db))

(defmethod clear-vcd-db ((db sqlite:sqlite-handle))
  (sqlite:execute-non-query db "delete from scopes")
  (sqlite:execute-non-query db "delete from signals")
  (sqlite:execute-non-query db "delete from waves"))

(defmethod vcd-db-add-scope ((db sqlite:sqlite-handle) (name string) &optional (parent-id nil) (type 0))
  (sqlite:execute-non-query db "insert into scopes (name, type, parent) values (?, ?, ?)" name type parent-id)
  (sqlite:last-insert-rowid db))

(defmethod vcd-db-add-signal ((db sqlite:sqlite-handle) (sig sl-signal-number) sig-name  scope)
  (let ((sig-type (cond
		    ((typep sig 'sl-signal-binary) 0)
		    ((typep sig 'sl-signal-number) 1)))
	(sig-size (cond
		    ((typep sig 'sl-signal-binary) (bit-width sig))
		    ((typep sig 'sl-signal-number) 64)))
	(reset-value (cond
		   ((and (typep sig 'sl-signal-binary) (= (bit-width sig) 1))
		    (format nil "~a" (to-string (uint-cast (value sig) :bits (bit-width sig)) :max-length (bit-width sig) :format 'b)))
		   ((and (typep sig 'sl-signal-binary) (> (bit-width sig) 1))
		    (format nil "b~a" (to-string (uint-cast (value sig) :bits (bit-width sig)) :max-length (bit-width sig) :format 'b)))
		   ((typep sig 'sl-signal-number) (format nil "r~a" (substitute #\e #\d (format nil "~a" (coerce (value sig) 'double-float)))))
		   ))) ;; TODO do it for both signed and unsigned
    (sqlite:execute-non-query db "insert into signals (name, vcdid, type, size, scope, reset_value) values (?, ?, ?, ?, ?, ?)"
			      (string sig-name) (vcd-id sig) sig-type sig-size scope reset-value)
    (setf (vcd-db-id sig) (sqlite:last-insert-rowid db))
    (sqlite:last-insert-rowid db)))

(defmethod vcd-db-add-change ((db sqlite:sqlite-handle) (sig sl-signal-number) time &aux vcd-val)
  (setf vcd-val (format nil "r~a" (substitute #\e #\d (format nil "~a" (coerce (value sig) 'double-float)))))
  (sqlite:execute-non-query db "insert into waves (simtime, signal, value) values (?, ?, ?)" time (vcd-db-id sig) vcd-val)
  (sqlite:last-insert-rowid db))

(defmethod vcd-db-add-change ((db sqlite:sqlite-handle) (sig sl-signal-binary) time &aux vcd-val)
  (setf vcd-val
	(if (> (bit-width sig) 1) 
	    (format nil "b~a" (to-string (uint-cast (value sig) :bits (bit-width sig)) :max-length (bit-width sig) :format 'b)) ;; TODO: do it for both signed and unsigned
	    (format nil "~a" (to-string (uint-cast (value sig) :bits (bit-width sig)) :max-length (bit-width sig) :format 'b) )))
  (sqlite:execute-non-query db "insert into waves (simtime, signal, value) values (?, ?, ?)" time (vcd-db-id sig) vcd-val)
  (sqlite:last-insert-rowid db))

(defmethod vcd-db-add-component
    ((db sqlite:sqlite-handle) (comp sl-component) &optional (parent-scope-db-id nil) &key (comp-name "" comp-name-p))
  (when (not comp-name-p)
    (setf comp-name (gensym "top-comp")))
  (setf (comp-db-id comp) (vcd-db-add-scope db (string comp-name) parent-scope-db-id))
  (loop for slot in (c2mop:class-slots (class-of comp)) do
    (let* ((slot-name (c2mop:slot-definition-name slot))
	   (slot-value (slot-value comp slot-name)))
      (cond
	((is-signal-slot slot)
	 (vcd-db-add-signal db slot-value slot-name (comp-db-id comp)))
	((and (not (eql slot-name 'parent)) (typep slot-value 'sl-component))
	 (vcd-db-add-component db slot-value (comp-db-id comp) :comp-name slot-name))))))

;; Functions to convert a vcd db to a vcd file
(defun vcd-write-header (vcd-fn timescale)
  (format vcd-fn "$version Generated by SystemLisp $end ~%")
  (format vcd-fn "$date ~a $end ~%" (print-object (local-time:now) nil))
  (format vcd-fn "$timescale ~a $end ~%~%" timescale))

(defmethod vcd-write-var-decl ((vcd-db sqlite:sqlite-handle) vcd-fn &optional (root nil))
  (let ((scope-name-type (first (sqlite:execute-to-list vcd-db "select name, type from scopes where rowid = ?" root)))
	(child-scopes (if root
			  (sqlite:execute-to-list vcd-db "select rowid, name, type from scopes where parent = ?" root)
			  ;; else
			  (sqlite:execute-to-list vcd-db "select rowid, name, type from scopes where parent is null")))
	(signals (if root
		     (sqlite:execute-to-list vcd-db "select rowid, name, vcdid, type, size from signals where scope = ?" root)
		     ;; else
		     nil)))
    (when root
      (format vcd-fn "$scope module ~a $end ~%" (first scope-name-type))
      (loop for signal in signals do
	(let* ((sig-type-code (nth 3 signal))
	       (sig-type (cond
			   ((= 0 sig-type-code) "wire")
			   ((= 1 sig-type-code) "real")
			   (t (error (format nil "Invalid signal type ~a" sig-type-code)))))
	       (sig-name (nth 1 signal))
	       (sig-vcd-id (nth 2 signal))
	       (sig-size (nth 4 signal)))
	  (format vcd-fn "$var ~a ~a ~a ~a $end ~%" sig-type sig-size sig-vcd-id sig-name))))

    (loop for child in child-scopes do (vcd-write-var-decl vcd-db vcd-fn (first child)))

    (when root
      (format vcd-fn "$upscope $end ~%"))))

(defun vcd-signal-separator (sig-type sig-size)
  (if (or (/= sig-type 0) (> sig-size 1)) " " ""))

(defmethod vcd-write-reset-values ((vcd-db sqlite:sqlite-handle) vcd-fn)
  (iterate:iter
    (for (vcdid sig-size sig-type reset-value)
      in-sqlite-query "select vcdid, size, type,reset_value from signals"
      on-database vcd-db)
    (format vcd-fn "~a~a~a ~%" reset-value (vcd-signal-separator sig-type sig-size) vcdid)))

(defmethod vcd-write-dumpvars ((vcd-db sqlite:sqlite-handle) vcd-fn)
  (let ((timestamps (mapcar #'first (sqlite:execute-to-list vcd-db "select distinct simtime from waves order by simtime"))))
    (format vcd-fn "$dumpvars ~%")
    (vcd-write-reset-values vcd-db vcd-fn)
    (format vcd-fn "$end ~%~%")
    (loop for tstamp in timestamps do
      (format vcd-fn "#~a ~%" tstamp)
      (iterate:iter
	(for (sig-id sig-val sig-vcd-id sig-size sig-type)
	  in-sqlite-query
	  "select waves.signal, waves.value, signals.vcdid, signals.size, signals.type 
           from waves, signals 
           where waves.simtime = ? and signals.rowid = waves.signal 
           order by signals.rowid"
	  on-database vcd-db
	  with-parameters (tstamp))
	(format vcd-fn "~a~a~a ~%" sig-val (vcd-signal-separator sig-type sig-size)  sig-vcd-id)))))

(defmethod vcd-db-handle-to-file ((vcd-db sqlite:sqlite-handle) vcd-fname timescale stop-time)
  (let ((vcd-fn (open vcd-fname :direction :output :if-exists :supersede)))
    (vcd-write-header vcd-fn timescale)
    (vcd-write-var-decl vcd-db vcd-fn nil)
    (format vcd-fn "$enddefinitions $end ~%~%")
    (vcd-write-dumpvars vcd-db vcd-fn)
    (format vcd-fn "#~a ~%" stop-time)    
    (close vcd-fn)))

(defun vcd-db-to-file (db-name vcd-fname timescale)
  (let ((vcd-fn (open vcd-fname :direction :output :if-exists :supersede))
	(vcd-db (sqlite:connect db-name)))
    (vcd-write-header vcd-fn timescale)
    (vcd-write-var-decl vcd-db vcd-fn nil)
    (format vcd-fn "$enddefinitions $end ~%~%")
    (vcd-write-dumpvars vcd-db vcd-fn)
    (close vcd-fn)))
