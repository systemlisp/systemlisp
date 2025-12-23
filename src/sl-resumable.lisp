(in-package :system-lisp)

(defun find-sub-form (form expr &optional (container-forms '()))
  (cond
    ((atom expr) (return-from find-sub-form nil))

    ((equal (car expr) form) (return-from find-sub-form t))

    ((member (car expr) container-forms)
     (when (find-sub-form form (cdr expr) container-forms)
       (return-from find-sub-form t)))

    (t (loop for sub-form in expr
	     do (when (find-sub-form form sub-form container-forms)
		  (return-from find-sub-form t))))))

;; Parse lambda lists
(defclass defun-lambda-list-parser ()
  ((required-args  :accessor required-args :initform '())
   (optional-args :accessor optional-args :initform '())
   (rest-arg :accessor rest-arg :initform nil)
   (key-args :accessor key-args :initform '())
   (allow-other-keys :accessor allow-other-keys :initform nil)
   (aux-vars :accessor aux-vars :initform '())))

(defmethod process-non-symbol-required-arg ((parser defun-lambda-list-parser) arg)
  (error "Required defun args can only be symbols"))

;;; Take a list of defun arguments and separate the argment symbols into various types of arguments each with their own lists as slots inside a parser object
(defmethod parse-lambda-list ((parser defun-lambda-list-parser) (llist list) &aux aux-llist stage (rest-cnt 0))
  (setf aux-llist llist)
  (setf stage '&required)
  (setf rest-cnt 0)
  (with-slots (required-args optional-args rest-arg key-args aux-vars allow-other-keys) parser
    ;; FSM for argument parsing
    (symbol-macrolet ((car-llist (car aux-llist))
		      (has-default-value
			(and (listp car-llist)
			     (<= 2 (length car-llist) 3)
			     (symbolp (car car-llist))
			     (symbolp (nth 2 car-llist))))
		      (arg-sym (if (symbolp car-llist) car-llist (car car-llist))))
      (loop while (not (null aux-llist)) do
	(case stage
	  (&required (if (symbolp car-llist)
			 (if (member car-llist '(&optional &rest &key &aux))
			     (setf stage car-llist)
			     ;; else
			     (push car-llist required-args))
			 ;;else
			 (process-non-symbol-required-arg parser car-llist)))
	  (&optional (let ((arg-sym-val arg-sym))
		       (cond ((eq car-llist '&optional) (error "&optional encountered twice"))
			     ((and (or (symbolp car-llist) has-default-value)
				   (member arg-sym-val required-args))
			      (error (format nil "The variable ~a occurs more than once in the lambda list" arg-sym-val)))
			     ((member car-llist '(&rest &key &aux)) (setf stage car-llist))
			     ((eq car-llist '&allow-other-keys) (error (format nil "Misplaced ~a" car-llist)))
			     ((or (symbolp car-llist) has-default-value)
			      (push car-llist optional-args))
			     (t (error (format nil "Invalid optional argument ~a" car-llist))))))
	  (&rest (let ((arg-sym-val arg-sym))
		   (cond ((> rest-cnt 1) (error "Can not have more than one &rest arguments"))
			 ((and (or (symbolp car-llist) has-default-value)
			       (or (member arg-sym-val required-args) (member arg-sym-val optional-args) (member arg-sym-val key-args)))
			  (error (format nil "The variable ~a occurs more than once in the lambda list" arg-sym)))
			 ((member car-llist '(&key &aux)) (setf stage car-llist))
			 ((member car-llist '(&optional &rest &allow-other-keys)) (error (format nil "Misplaced ~a" car-llist)))
			 ((symbolp car-llist) 
			  (progn (setf rest-arg car-llist)
				 (setf rest-cnt (cl:1+ rest-cnt))))
			 (t (error "&rest argument must be a symbol")))))
	  (&key (let ((arg-sym-val arg-sym))
		  (cond ((eq car-llist '&key) (error "&key encountered twice"))
			((and (or (symbolp car-llist) has-default-value)
			      (or (member arg-sym-val required-args) (member arg-sym-val optional-args) (eq arg-sym-val rest-arg)))
			 (error (format nil "The variable ~a occurs more than once in the lambda list" arg-sym-val)))
			((member car-llist '(&optional &rest)) (error (format nil "Misplaced ~a" car-llist)))
			((eq car-llist '&aux) (setf stage '&aux))
			((eq car-llist '&allow-other-keys) (setf allow-other-keys t))
			((or (symbolp car-llist) (and (listp car-llist) (<= 2 (length car-llist) 3) (symbolp (nth 2 car-llist))))
			 (push car-llist key-args))
			(t (error (format nil "Invalid &key argument ~a" car-llist))))))
	  (&aux (cond 
		  ((member car-llist '(&optional &rest &key &allow-other-keys &aux)) (error (format nil "Misplaced ~a after &aux" car-llist)))
		  ((or (symbolp car-llist) (and (listp car-llist) (= (length car-llist) 2)))
		   (push car-llist aux-vars))
		  (t (error (format nil "Invalid &aux argument ~a" car-llist))))))
	(setf aux-llist (cdr aux-llist))))
    ;; Reverse lists to put args in correct order
    (setf required-args (reverse required-args))
    (setf optional-args (reverse optional-args))
    (setf key-args (reverse key-args))
    (setf aux-vars (reverse aux-vars)))
  t)

;; Take a list of values as it would be passed to a function call
;; and a populated lambda list parser object
;; and evaluate the value that each argument will take when the function will be called
(defmethod apply-lambda-list ((parser defun-lambda-list-parser) (vals list))
  (let ((result (make-hash-table))
	(arg-cnt 0))

    ;; Eval required args
    (loop for arg in (required-args parser) do
      (let ((crt-val (pop vals)))
	(when (null crt-val) (error (format nil "Expected ~a required args, got ~a instead" (length (required-args parser)) (+ arg-cnt 1))))
	(setf (gethash arg result) crt-val)
	(incf arg-cnt)))

    ;; Eval optional args
    (when (not (null (optional-args parser)))
      (loop for arg in (optional-args parser) do
	(let ((crt-val (car vals)))
	  (if (symbolp arg)
	      (setf (gethash arg result) (if (null vals) nil crt-val)) 
	      ;; else
	      (setf (gethash (first arg) result) (if (null vals) (second arg) crt-val)))
	  (pop vals))))
    
    ;; Eval rest args
    (when (not (null (rest-arg parser)))
      (setf (gethash (rest-arg parser) result) '())
      (loop while (not (or (and (key-args parser) (keywordp (car vals))) (null vals))) do
	(let ((crt-val (pop vals)))
	  (push crt-val (gethash (rest-arg parser) result))))
      (alexandria:reversef (gethash (rest-arg parser) result)))

    ;; Eval key args
    (when (not (null (key-args parser)))
      (unless (= (rem (length vals) 2) 0)
	(error "Odd number of key argument values"))
      (let ((provided-keys (remove-if-not #'keywordp vals))
	    (expected-keys (mapcar
			    (lambda (it)
			      (if (symbolp it)
				  (intern (string-upcase (symbol-name it)) :keyword)
				  ;; else
				  (intern (string-upcase (symbol-name (car it))) :keyword)))
			    (key-args parser))))
	;; Set provided values for provided keys
	(loop for key in provided-keys do
	  (if (member key expected-keys)
	      (progn
		(setf (gethash (intern (symbol-name key)) result) (getf vals key))
		(alexandria:removef expected-keys key))
	      ;; else
	      (if (allow-other-keys parser)
		  (setf (gethash (intern (symbol-name key)) result) (getf vals key))
		  ;; else
		  (error (format nil "Key argument ~a not allowed" key))))
	  (remf vals key))
	;; Set default values for unprovided keys
	(loop for exp-key in expected-keys do
	  (loop for key-arg in (key-args parser) do
	    (cond
	      ;; Key without default value
	      ((and (symbolp key-arg) (string= exp-key key-arg))
	       (progn
		 (setf (gethash (intern (symbol-name exp-key)) result) nil)
		 (return)))
	      ;; Key with default value
	      ((and (listp key-arg) (string= exp-key (car key-arg)))
	       (progn
		 (setf (gethash (intern (symbol-name exp-key)) result) (second key-arg))
		 (return))))))))

    ;; Eval aux args
    (if (aux-vars parser)
	(loop for aux-var in (aux-vars parser) do
	  (if (symbolp aux-var)
	      (setf (gethash aux-var result) nil)
	      (setf (gethash (first aux-var) result) (second aux-var)))))
    
    ;; Check for unwanted extra arguments
    (if (null vals)
	result
	;; else
	(error (format nil "Unwanted extra argument(s) ~a" vals)))))

(defmethod get-defun-parser-symbols ((parser defun-lambda-list-parser))
  (let* ((optional-arg-symbols (mapcar (lambda (it) (if (symbolp it) it (car it))) (optional-args parser)))
	 (key-arg-symbols (mapcar (lambda (it) (if (symbolp it) it (car it))) (key-args parser)))
	 (rest-arg-symbol (if (rest-arg parser) (list (rest-arg parser)) nil))
	 (symbols (reduce 'union (list (required-args parser) optional-arg-symbols rest-arg-symbol key-arg-symbols)))
	 (aux-symbols (mapcar (lambda (it) (if (symbolp it) it (car it))) (aux-vars parser)))
	 (defaults '()))
    (loop for arg in (optional-args parser) do
	  (if (symbolp arg)
	      (setf (getf defaults arg) nil)
	      ;; else
	      (setf (getf defaults (first arg)) (second arg))))
    (loop for arg in (key-args parser) do
	  (if (symbolp arg)
	      (setf (getf defaults arg) nil)
	      ;; else
	      (setf (getf defaults (first arg)) (second arg))))
    (loop for arg in (aux-vars parser) do
	  (if (symbolp arg)
	      (setf (getf defaults arg) nil)
	      ;; else
	      (setf (getf defaults (first arg)) (second arg))))
    (values symbols aux-symbols defaults)))

(defun eval-defun-args (lambda-list arg-vals)
  (let ((parser (make-instance 'defun-lambda-list-parser)))
    (parse-lambda-list parser lambda-list)
    (apply-lambda-list parser arg-vals)))

;;; defmethod lambda list parse
(defclass defmethod-lambda-list-parser (defun-lambda-list-parser)
  ((required-arg-types
    :accessor required-arg-types
    :initform (make-hash-table))))

(defmethod process-non-symbol-required-arg ((parser defmethod-lambda-list-parser) arg)
  (if (listp arg)
      (progn
	(push (car arg) (required-args parser))
	(setf (gethash (car arg) (required-arg-types parser)) (second arg)))
      ;; else
      (error "Required args for a defmethod can only be symbols or lists")))

;;;;;;

(defclass resumable-state-env ()
  ((resumable-locals :accessor resumable-locals :initform (make-hash-table))
   (resumable-state :accessor resumable-state :initform 0)))

(defmethod resume ((obj resumable-state-env))
  (error "This method must be implemented for a subclass of resumable-state-env"))

(defclass lambda-resumable-state-env (resumable-state-env)
  ((resume-lambda :accessor resume-lambda :initform (lambda ()) :type function)))

(defmethod resume ((obj lambda-resumable-state-env))
  (funcall (resume-lambda obj) obj))

(defclass resumable-macro-state-env ()
  ((var-symbols :accessor var-symbols :initarg :var-symbols)
   (valid-states :accessor valid-states :initform (make-array 1 :initial-contents '(0) :adjustable t :fill-pointer 1))
   (tag-count :accessor tag-count :initform 0)
   (resumable-tagbody :accessor resumable-tagbody :initform '())
   (loop-tags :accessor loop-tags :initform '())
   (loop-start-tags :accessor loop-start-tags :initform '())
   (is-lambda-res :accessor is-lambda-res :initform nil)))

(defmethod get-new-tag ((m-env resumable-macro-state-env))
  (incf (tag-count m-env)))

(defmethod translate-resumable-body ((m-env resumable-macro-state-env) (body list) &key (env-var-name 'env))
  (unless (trivia:match (car body)
	    ;; let statement
	    ((list* 'let locals let-body)
	     (if (or t (find-sub-form 'yield let-body '(if when unless progn let c-for while do-while)))
		 ;; then 
		 (progn
		   (loop for local in locals do
		     (when (not (member (first local) (var-symbols m-env))) (push (first local) (var-symbols m-env)))
		     (uiop:appendf (resumable-tagbody m-env)
				   (list `(push ,(second local) (gethash (quote ,(first local)) (resumable-locals ,env-var-name))))))

		   (translate-resumable-body m-env let-body :env-var-name env-var-name)

		   (loop for local in locals do
		     (uiop:appendf (resumable-tagbody m-env)
				   (list `(pop (gethash (quote ,(first local)) (resumable-locals ,env-var-name))))))
		   t)
		 ;; else ;; TODO: this is dead code until we add support for into to search for yield inside macro calls
		 (progn
		   (uiop:appendf (resumable-tagbody m-env)
				 (list `(let ,locals ,@let-body)))
		   t))
	     t) ;; got match 
	    
	    ;; if statement
	    ((list* 'if cond-expr then-clause else-clause other-stuff)
	     (when other-stuff (error "if statement can have only two clauses")) ;; TODO: add more info in error
	     (let* (;; TODO: then-has-yield and else-has-yield will always be true until we find a way to recursively search inside macro calls
		    (then-has-yield (or t (find-sub-form 'yield then-clause '(if when unless progn let c-for while do-while))))
		    (else-has-yield (or t (find-sub-form 'yield else-clause '(if when unless progn let c-for while do-while))))
		    (then-clause-tag (get-new-tag m-env))
		    (else-clause-tag (get-new-tag m-env))
		    (whatever-is-next-tag (get-new-tag m-env))
		    (then-clause-final (if then-has-yield `(go ,then-clause-tag)
					   (if else-has-yield
					       `(progn ,then-clause (go ,whatever-is-next-tag))
					       then-clause)))
		    (else-clause-final (if else-has-yield `(go ,else-clause-tag) else-clause)))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(if ,cond-expr
					;;(go ,then-clause-tag)
					,then-clause-final
					;;(go ,else-clause-tag)
					,else-clause-final
					)
				   then-clause-tag))

	       (when then-has-yield
		 (translate-resumable-body  m-env (list then-clause) :env-var-name env-var-name)
		 (uiop:appendf (resumable-tagbody m-env)
			       (list `(go ,whatever-is-next-tag))))

	       (when else-has-yield
		 (translate-resumable-body m-env (list else-clause-tag else-clause) :env-var-name env-var-name))
	       
	       (uiop:appendf (resumable-tagbody m-env)
			     (list whatever-is-next-tag))
	       t)) ;; got match
	    
	    ;; when statement
	    ((list* 'when cond-expr then-clause)
	     (let ((then-clause-tag (get-new-tag m-env))
		   (whatever-is-next-tag (get-new-tag m-env)))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(if ,cond-expr
					(go ,then-clause-tag)
					(go ,whatever-is-next-tag))
				   then-clause-tag))
	       
	       (translate-resumable-body  m-env then-clause :env-var-name env-var-name)

	       (uiop:appendf (resumable-tagbody m-env) (list whatever-is-next-tag))
	       t)) ;; got match

	    ;; unless statement
	    ((list* 'unless cond-expr then-clause)
	     (let ((then-clause-tag (get-new-tag m-env))
		   (whatever-is-next-tag (get-new-tag m-env)))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(if (not ,cond-expr)
					(go ,then-clause-tag)
					(go ,whatever-is-next-tag))
				   then-clause-tag))
	       
	       (translate-resumable-body m-env then-clause :env-var-name env-var-name)

	       (uiop:appendf (resumable-tagbody m-env) (list whatever-is-next-tag))
	       t))

	    ;; while statement
	    ((list* 'while cond-expr while-body)
	     (let ((loop-start-tag (get-new-tag m-env))
		   (whatever-is-next-tag (get-new-tag m-env)))
	       (push whatever-is-next-tag (loop-tags m-env))
	       (push loop-start-tag (loop-start-tags m-env))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list loop-start-tag
				   `(if (not ,cond-expr)
					(go ,whatever-is-next-tag))))
	       
	       (translate-resumable-body m-env while-body :env-var-name env-var-name)

	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(go ,loop-start-tag)
				   whatever-is-next-tag))
	       (pop (loop-tags m-env))
	       (pop (loop-start-tags m-env))
	       t))

	    ;; do-while statement
	    ((list* 'do-while cond-expr while-body)
	     (let ((loop-start-tag (get-new-tag m-env))
		   (whatever-is-next-tag (get-new-tag m-env))
		   (cond-eval-tag (get-new-tag m-env)))
	       (push whatever-is-next-tag (loop-tags m-env))
	       (push cond-eval-tag (loop-start-tags m-env))
	       (uiop:appendf (resumable-tagbody m-env) (list loop-start-tag))

	       (translate-resumable-body m-env while-body :env-var-name env-var-name)

	       (uiop:appendf (resumable-tagbody m-env)
			     (list cond-eval-tag
			      `(if ,cond-expr
				   (go ,loop-start-tag)
				   (go ,whatever-is-next-tag))
				   whatever-is-next-tag))
	       (pop (loop-tags m-env))
	       (pop (loop-start-tags m-env))
	       t))
	    
	    ;; forever statement
	    ((list* 'forever while-body)
	     (let ((loop-start-tag (get-new-tag m-env))
		   (whatever-is-next-tag (get-new-tag m-env)))
	       (push whatever-is-next-tag (loop-tags m-env))
	       (push loop-start-tag (loop-start-tags m-env))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list loop-start-tag))
	       
	       (translate-resumable-body m-env while-body :env-var-name env-var-name)

	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(go ,loop-start-tag)
				   whatever-is-next-tag))
	       (pop (loop-tags m-env))
	       (pop (loop-start-tags m-env))
	       t))

	    ;; c-for statement
	    ((list* 'c-for (list* init-clause stop-cond update-clause other-stuff) body)
	     ;; TODO: add more info in error message
	     (when other-stuff (error (format nil "c-for statement can only have 3 clauses. Unexpected statement: ~a" other-stuff))) 
	     (let ((loop-start-tag (get-new-tag m-env))
		   (whatever-is-next-tag (get-new-tag m-env)))
	       (push whatever-is-next-tag (loop-tags m-env))
	       (push loop-start-tag (loop-start-tags m-env))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list init-clause
				   loop-start-tag
				   `(when (not ,stop-cond) (go ,whatever-is-next-tag))))

	       (translate-resumable-body m-env body :env-var-name env-var-name)

	       (uiop:appendf (resumable-tagbody m-env)
			     (list update-clause
				   `(go ,loop-start-tag)
				   whatever-is-next-tag))
	       (pop (loop-tags m-env))
	       (pop (loop-start-tags m-env))
	       t))

	    ;; list-for-each statement
	    ((list* 'list-for-each (list item list) body)
	     (let ((loop-start-tag (get-new-tag m-env))
		   (whatever-is-next-tag (get-new-tag m-env))
		   (list-var-name (gensym "list-var-")))
	       ;; Register local variables
	       (when (not (member item (var-symbols m-env))) (push item (var-symbols m-env)))
	       (when (not (member item (var-symbols m-env))) (push list-var-name (var-symbols m-env)))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(push ,list (gethash (quote ,list-var-name) (resumable-locals ,env-var-name)))
				   `(push (car ,list) (gethash (quote ,item) (resumable-locals ,env-var-name)))))
	       ;; Generate loop code
	       (push whatever-is-next-tag (loop-tags m-env))
	       (push loop-start-tag (loop-start-tags m-env))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(setf ,list-var-name ,list)
				   loop-start-tag
				   `(when (null ,list-var-name) (go ,whatever-is-next-tag))
				   `(setf ,item (car ,list-var-name))))

	       (translate-resumable-body m-env body :env-var-name env-var-name)

	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(setf ,list-var-name (cdr ,list-var-name))
				   whatever-is-next-tag))
	       (pop (loop-tags m-env))
	       (pop (loop-start-tags m-env))
	       ;; Unregister local variables
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(pop (gethash (quote ,item) (resumable-locals ,env-var-name)))
				   `(pop (gethash (quote ,list-var-name) (resumable-locals ,env-var-name)))))
	       t))

	    ;; array-for-each statement
	    ((list* 'array-for-each (list item array) body)
	     (let ((loop-start-tag (get-new-tag m-env))
		   (whatever-is-next-tag (get-new-tag m-env))
		   (array-var-name (gensym "array-var-"))
		   (array-index-name (gensym "array-index-")))
	       ;; Register local variables
	       (when (not (member item (var-symbols m-env))) (push item (var-symbols m-env)))
	       (when (not (member array-var-name (var-symbols m-env))) (push array-var-name (var-symbols m-env)))
	       (when (not (member array-index-name (var-symbols m-env))) (push array-index-name (var-symbols m-env)))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(push ,array (gethash (quote ,array-var-name) (resumable-locals ,env-var-name)))
				   `(push nil (gethash (quote ,item) (resumable-locals ,env-var-name)))
				   `(push 0 (gethash (quote ,array-index-name) (resumable-locals ,env-var-name)))))
	       ;; Generate loop code
	       (push whatever-is-next-tag (loop-tags m-env))
	       (push loop-start-tag (loop-start-tags m-env))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(setf ,array-index-name 0)
				   loop-start-tag
				   `(when (> ,array-index-name (length ,array-var-name)) (go ,whatever-is-next-tag))))

	       (translate-resumable-body m-env body :env-var-name env-var-name)

	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(incf ,array-index-name)
				   whatever-is-next-tag))
	       (pop (loop-tags m-env))
	       (pop (loop-start-tags m-env))
	       ;; Unregister local variables
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(pop (gethash (quote ,item) (resumable-locals ,env-var-name)))
				   `(pop (gethash (quote ,array-var-name) (resumable-locals ,env-var-name)))
				   `(pop (gethash (quote ,array-index-name (resumable-locals ,env-var-name))))))
	       t))
	    
	    ;; hashtable-for-each statement
	    #|((list* 'hashtable-for-each (list item hashtable) body)|#
	    ;; register local variables
	    ;; - hash-table-vas-name
	    ;; - item
	    ;; - current-key
	    ;; - list-of-keys
	    ;; Get list of keys
	    ;; loop-start-tag
	    ;; If list of keys is empty then go to whatever-is-next-tag
	    ;; Set current key to first key in list
	    ;; Set item to the value of the current key
	    ;; Translate tagbody
	    ;; pop current key from list of keys
	    ;; go to loop-start-tag
	    ;; whatever-is-next tag
	    ;; unregister local variables
	    #|(uiop:adppendf (resumable-tagbody m-env)
	    (list `())))|#

	    ;; repeat statement
	    ((list* 'repeat count body)
	     (let ((loop-start-tag (get-new-tag m-env))
		   (whatever-is-next-tag (get-new-tag m-env))
		   (loop-var-name (gensym "loop-var-")))
	       ;; Register local variable
	       (when (not (member loop-var-name (var-symbols m-env))) (push loop-var-name (var-symbols m-env)))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(push 0 (gethash (quote ,loop-var-name) (resumable-locals ,env-var-name)))))
	       ;; Generate loop code
	       (push whatever-is-next-tag (loop-tags m-env))
	       (push loop-start-tag (loop-start-tags m-env))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(setf ,loop-var-name 1)
				   loop-start-tag
				   `(when (> ,loop-var-name ,count) (go ,whatever-is-next-tag))))

	       (translate-resumable-body m-env body :env-var-name env-var-name)

	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(incf ,loop-var-name)
				   `(go ,loop-start-tag)
				   whatever-is-next-tag))
	       (pop (loop-tags m-env))
	       (pop (loop-start-tags m-env))
	       ;; Unregister local variable
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(pop (gethash (quote ,loop-var-name) (resumable-locals ,env-var-name)))))
	       t))

	    ;; break statement
	    ((list 'break-loop)
	     (if (first (loop-tags m-env))
		 (uiop:appendf (resumable-tagbody m-env) (list `(go ,(first (loop-tags m-env)))))
		 (error "Can't call break-loop from here. No enclosing loop"))) ; TODO think of better error message

	    ;; continue statement
	    ((list 'continue-loop)
	     (if (first (loop-tags m-env))
		 (uiop:appendf (resumable-tagbody m-env) (list `(go ,(first (loop-start-tags m-env)))))
		 (error "Can't call continue-loop from here. No enclosing loop")))

	    ;; return statement
	    ((list 'return val)
	     (progn
	       (uiop:appendf (resumable-tagbody m-env) (list `(setf (resumable-state ,env-var-name) nil)
							     (if (is-lambda-res m-env)
								 `(return-from anaphora:self ,val)
								 `(return-from resume ,val))))
	       t)) ;; got match
	    
	    ;; yield statement
	    ((list 'yield yield-expr)
	     (let ((after-yield-tag (get-new-tag m-env)))
	       (uiop:appendf (resumable-tagbody m-env)
			     (list `(setf (resumable-state ,env-var-name) ,after-yield-tag)
				   (if (is-lambda-res m-env)
				       `(return-from anaphora:self ,yield-expr)
				       `(return-from resume ,yield-expr))
				   after-yield-tag))
	       (vector-push-extend after-yield-tag (valid-states m-env))
	       t)) ;; got match
	    
	    ;; progn statement
	    ((list* 'progn progn-body)
	     (translate-resumable-body m-env progn-body :env-var-name env-var-name)
	     t)) ;; got match

    (cond
      ;; no matches => check if we have a macro call then expand it one level
      ((and (not (atom (car body))) (not (eq 'yield (caar body))) (macro-function (caar body)))
       (progn
	 (translate-resumable-body m-env (list (macroexpand-1 (car body))) :env-var-name env-var-name)))

      ;; no matches => ordinary function call, don't translate it just copy it as it is
      (t (setf (resumable-tagbody m-env) (append (resumable-tagbody m-env) (list (car body)))))))
  
  ;; Move on to the rest of the task body
  (when (not (uiop:emptyp (cdr body))) (translate-resumable-body m-env (cdr body) :env-var-name env-var-name)))

(defparameter *sl-name-counter* 0)

(defun sl-genstr (prefix)
  (concatenate 'string prefix (princ-to-string (incf *sl-name-counter*))))

(defun sl-gensym (prefix)
  (make-symbol (concatenate 'string prefix (princ-to-string (incf *sl-name-counter*)))))

;; Define a simple resumable function with a list of symbols as arguments and another list of symbols as local variables
(defmacro defres (name arg-names locals &body body)
  (let* ((env-name (sl-gensym "env-"))
	 (m-env (make-instance 'resumable-macro-state-env :var-symbols (union arg-names (mapcar #'first locals))))
	 ;; Derived class name and declaration
	 (derived-class-name (make-symbol (concatenate 'string "%resumable-state-env-" (sl-genstr (string name)) "%")))
	 (derived-class-decl `(defclass ,derived-class-name (resumable-state-env) ()))
	 ;; Function that creates task class instance and initializes argument values and local variables
	 (resumable-init-fn `(defun ,name (,@arg-names)
			       (let* ((env (make-instance (quote ,derived-class-name))))
				 ,@(loop for arg-name in arg-names collect `(push ,arg-name (gethash (quote ,arg-name) (resumable-locals env))))
				 ,@(loop for local in locals collect `(push ,(second local) (gethash (quote ,(first local)) (resumable-locals env))))
				 env)))
	 ;; Generate the code enclosed by the tagbody
	 (resumable-tagbody (progn (translate-resumable-body m-env body :env-var-name env-name)
				   (resumable-tagbody m-env)))
	 ;; Generate resume method which resumes execution of the task
	 (resume-method `(defmethod resume ((,env-name ,derived-class-name))
			   (symbol-macrolet
			       (,@(loop for var-name in (var-symbols m-env)
					collect
					(list  var-name `(first (gethash (quote ,var-name) (resumable-locals ,env-name))))))
			     (tagbody
				(cond
				  ,@(loop for state across (valid-states m-env) collect `((equal (resumable-state ,env-name) ,state) (go ,state)))
				  ((equal (resumable-state ,env-name) nil) (error (format nil "Task ~a aleady finished." (quote ,name))))
				  (t (error (format nil "Invalid state code ~a" (resumable-state ,env-name)))))
			      0	     ; State 0 where the tasks starts 
				,@resumable-tagbody)
			     (setf (resumable-state ,env-name) nil)))))
    `(progn
       ,derived-class-decl
       ,resumable-init-fn
       ,resume-method)))

;; Define a resumable function that uses a lambda list to describe the arguments
(defmacro defun-res (name lambda-list &body body)
  (let* ((env-name (sl-gensym "env-"))
	 (arg-parser (let ((parser (make-instance 'defun-lambda-list-parser)))
		       (parse-lambda-list parser lambda-list)
		       parser))
	 (m-env
	   (multiple-value-bind (arg-symbols aux-symbols) (get-defun-parser-symbols arg-parser) 
		 (make-instance 'resumable-macro-state-env
				:var-symbols (union arg-symbols aux-symbols))))
	 ;; Derived class name and declaration
	 (derived-class-name (make-symbol (concatenate 'string "%resumable-state-env-" (sl-genstr (string name)) "%")))
	 (derived-class-decl `(defclass ,derived-class-name (resumable-state-env) ()))
	 ;; Function that creates task class instance and initializes argument values and local variables
	 (resumable-init-fn (multiple-value-bind (arg-symbols aux-symbols arg-defaults) (get-defun-parser-symbols arg-parser)
			 `(defun ,name (,@lambda-list)
			    (let* ((env (make-instance (quote ,derived-class-name))))
			      ,@(loop for arg-symbol in arg-symbols collect
				      `(if (null ,arg-symbol)
					   (push ,(getf arg-defaults arg-symbol) (gethash (quote ,arg-symbol) (resumable-locals env)))
					   ;; else
					   (push ,arg-symbol (gethash (quote ,arg-symbol) (resumable-locals env)))))
			      ,@(loop for aux-symbol in aux-symbols collect
				      `(if (null ,aux-symbol)
					   (push ,(getf arg-defaults aux-symbol) (gethash (quote ,aux-symbol) (resumable-locals env)))
					   ;; else
					   (push ,aux-symbol (gethash (quote ,aux-symbol) (resumable-locals env)))))
			      env))))
	 ;; Generate the code enclosed by the tagbody
	 (resumable-tagbody (progn (translate-resumable-body m-env body :env-var-name env-name)
			      (resumable-tagbody m-env)))
	 ;; Generate resume method which resumes execution of the task
	 (resume-method `(defmethod resume ((,env-name ,derived-class-name))
			      (symbol-macrolet
				  (,@(loop for var-name in (var-symbols m-env)
					   collect
					   (list  var-name `(first (gethash (quote ,var-name) (resumable-locals ,env-name))))))
				(tagbody
				   (cond
				     ,@(loop for state across (valid-states m-env) collect `((equal (resumable-state ,env-name) ,state) (go ,state)))
				     ((equal (resumable-state ,env-name) nil) (error (format nil "Resumable function ~a aleady finished." (quote ,name))))
				     (t (error (format nil "Invalid state code ~a" (resumable-state ,env-name)))))
				 0 ; State 0 where the tasks starts 
				   ,@resumable-tagbody)
				(setf (resumable-state ,env-name) nil)))))
    `(progn
       ,derived-class-decl
       ,resumable-init-fn
       ,resume-method)))

;; Define a resumable method that uses a lambda list to describe the arguments
;; TODO: add support for method qualifiers
;; TODO: test it
(defmacro defmethod-res (name lambda-list &body body)
  (let* ((env-name (sl-gensym "env-"))
	 (arg-parser (let ((parser (make-instance 'defmethod-lambda-list-parser)))
		       (parse-lambda-list parser lambda-list)
		       parser))
	 (m-env
	   (multiple-value-bind (arg-symbols aux-symbols) (get-defun-parser-symbols arg-parser) 
		 (make-instance 'resumable-macro-state-env
				:var-symbols (union arg-symbols aux-symbols))))
	 ;; Derived class name and declaration
	 (derived-class-name (make-symbol (concatenate 'string "%resumable-state-env-" (sl-genstr (string name)) "-" (string (sl-gensym "g")) "%")))
	 (derived-class-decl `(defclass ,derived-class-name (resumable-state-env) ()))
	 ;; Function that creates task class instance and initializes argument values and local variables
	 (resumable-init-fn (multiple-value-bind (arg-symbols aux-symbols arg-defaults) (get-defun-parser-symbols arg-parser)
			 `(defmethod ,name (,@lambda-list)
			    (let* ((env (make-instance (quote ,derived-class-name))))
			      ,@(loop for arg-symbol in arg-symbols collect
				      `(if (null ,arg-symbol)
					   (push ,(getf arg-defaults arg-symbol) (gethash (quote ,arg-symbol) (resumable-locals env)))
					   ;; else
					   (push ,arg-symbol (gethash (quote ,arg-symbol) (resumable-locals env)))))
			      ,@(loop for aux-symbol in aux-symbols collect
				      `(if (null ,aux-symbol)
					   (push ,(getf arg-defaults aux-symbol) (gethash (quote ,aux-symbol) (resumable-locals env)))
					   ;; else
					   (push ,aux-symbol (gethash (quote ,aux-symbol) (resumable-locals env)))))
			      env))))
	 ;; Generate the code enclosed by the tagbody
	 (resumable-tagbody (progn (translate-resumable-body m-env body :env-var-name env-name)
			      (resumable-tagbody m-env)))
	 ;; Generate resume method which resumes execution of the task
	 (resume-method `(defmethod resume ((,env-name ,derived-class-name))
			      (symbol-macrolet
				  (,@(loop for var-name in (var-symbols m-env)
					   collect
					   (list  var-name `(first (gethash (quote ,var-name) (resumable-locals ,env-name))))))
				(tagbody
				   (cond
				     ,@(loop for state across (valid-states m-env) collect `((equal (resumable-state ,env-name) ,state) (go ,state)))
				     ((equal (resumable-state ,env-name) nil) (error (format nil "Resumable function ~a aleady finished." (quote ,name))))
				     (t (error (format nil "Invalid state code ~a" (resumable-state ,env-name)))))
				 0 ; State 0 where the tasks starts 
				   ,@resumable-tagbody)
				(setf (resumable-state ,env-name) nil)))))

    `(progn
       ,derived-class-decl
       ,resumable-init-fn
       ,resume-method)))

;; Define a resumable lambda expression that uses a lambda list to describe the arguments
;; TODO: test it
(defmacro lambda-res (lambda-list &body body)
  (let* ((name (sl-gensym "lambda-res-"))
	 (env-name (sl-gensym "env-"))
	 (arg-parser (let ((parser (make-instance 'defun-lambda-list-parser)))
		       (parse-lambda-list parser lambda-list)
		       parser))
	 (m-env
	   (multiple-value-bind (arg-symbols aux-symbols) (get-defun-parser-symbols arg-parser) 
		 (make-instance 'resumable-macro-state-env
				:var-symbols (union arg-symbols aux-symbols))))
	 ;; Generate the code enclosed by the tagbody
	 (resumable-tagbody (progn
			      (setf (is-lambda-res m-env) t)
			      (translate-resumable-body m-env body :env-var-name env-name)
			      (resumable-tagbody m-env)))
	 ;; Function that creates task class instance and initializes argument values and local variables
	 (resumable-init-fn (multiple-value-bind (arg-symbols aux-symbols arg-defaults) (get-defun-parser-symbols arg-parser)
			      `(lambda (,@lambda-list)
				 (let* ((env (make-instance (quote lambda-resumable-state-env)))
					(resume-lmbd (anaphora:alambda (,env-name)
						       (symbol-macrolet
							   (,@(loop for var-name in (var-symbols m-env)
								    collect
								    (list  var-name `(first (gethash (quote ,var-name) (resumable-locals ,env-name))))))
							 (tagbody
							    (cond
							      ,@(loop for state across (valid-states m-env) collect `((equal (resumable-state ,env-name) ,state) (go ,state)))
							      ((equal (resumable-state ,env-name) nil) (error (format nil "Resumable function ~a aleady finished." (quote ,name))))
							      (t (error (format nil "Invalid state code ~a" (resumable-state ,env-name)))))
							  0 ; State 0 where the tasks starts 
							    ,@resumable-tagbody)
							 (setf (resumable-state ,env-name) nil))) ))
				   ,@(loop for arg-symbol in arg-symbols collect
					   `(if (null ,arg-symbol)
						(push ,(getf arg-defaults arg-symbol) (gethash (quote ,arg-symbol) (resumable-locals ,env-name)))
						;; else
						(push ,arg-symbol (gethash (quote ,arg-symbol) (resumable-locals ,env-name)))))
				   ,@(loop for aux-symbol in aux-symbols collect
					   `(if (null ,aux-symbol)
						(push ,(getf arg-defaults aux-symbol) (gethash (quote ,aux-symbol) (resumable-locals ,env-name)))
						;; else
						(push ,aux-symbol (gethash (quote ,aux-symbol) (resumable-locals ,env-name)))))
				   (setf (resume-lambda env) resume-lmbd)
				   env)))))
    resumable-init-fn))
