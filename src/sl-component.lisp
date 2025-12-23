(in-package :system-lisp)

(defclass sl-component ()
  ((comp-db-id :accessor comp-db-id :initform 0)
   (parent-db-id :accessor parend-db-id :initform 0)
   (parent :accessor parent :initform nil)
   (members :accessor members :initform '())))

(defmacro create-component (class parent &rest args)
  `(let ((inst (make-instance (quote ,class) ,@args)))
     (when ,parent
       (setf (parent inst) ,parent)
       (push inst (members ,parent)))
     inst))

(defmethod do-build ((comp sl-component))
  (build comp)
  (loop for member in (members comp) do
    (build member)))

(defmethod build ((comp sl-component)))

(defmethod do-connect ((comp sl-component))
  (loop for member in (members comp) do
    (connect member))
  (connect comp))

(defmethod connect ((comp sl-component)))

(defmethod do-run ((comp sl-component))
  (run comp)
  (loop for member in (members comp) do
    (run member)))

(defmethod run ((comp sl-component)))

