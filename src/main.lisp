(defpackage amop-impl
  (:use :cl))
(in-package :amop-impl)

;; blah blah blah.

;; to avoid collisions with actual CL-CLOS, suffix 'c/' is used for all macros etc.

(let ((c/class-table (make-hash-table :test #'eq)))
  (defun c/find-class (symbol &optional (errorp t))
    (let ((class (gethash symbol c/class-table nil)))
      (if (and (null class) errorp)
          (error "No class named ~S." symbol)
          class)))

  (defun (setf c/find-class) (new-value symbol)
    (setf (gethash symbol class-table) new-value)))

;; all-keys param receives all the keyword arguments (:direct-superclasses, :direct-slots, and other)
(defun c/ensure-class (name &rest all-keys)
  (if (c/find-class name nil)
      (error "can't redefine the class named ~S." name)
      (let ((class (apply #'c/make-instance 'c/standard-class :name name all-keys)))
        (setf (c/find-class name) class)
        class)))

(defmacro c/defclass (name direct-superclasses direct-slots &rest options)
  `(c/ensure-class ',name
                   :direct-superclasses ,(c/canonicalize-direct-superclasses direct-superclasses)
                   :direct-slots ,(c/canonicalize-direct-slots direct-slots)
                   ,@ (c/canonicalize-direct-options options)))

;; not sure about collisions of accessors (thus the c/ prefix)
;; "defined to centralize the description of what class metaobjects look like"
(c/defclass c/standard-class ()
            ((name :initarg :name
                   :accessor c/class-name)
             (direct-superclasses :initarg :direct-superclasses
                                  :accessor c/class-direct-superclasses)
             (direct-slots :accessor c/class-direct-slots)
             (class-precedence-list :accessor c/class-precedence-list)
             (effective-slots :accessor c/class-slots)
             (direct-subclasses :initform ()
                                :accessor c/class-direct-subclasses)
             (direct-methods :initform ()
                             :accessor c/class-direct-methods)))

;; [Q?]
;; 1. c/make-instance and others are missing
;; 2. assuming they weren't (1) then c/defclass is using c/ensure-class which depends on c/standard-class
;;    existing, but is also defined with c/defclass ...


;; when creating a class metaobject:
;; - provide default value for direct superclasses
;; - add direct subclass links to new class's direct superclasses
;; - convert slot prop lists to direct slot definition metaobjects
;; - define slot accessor methods
;; - do inheritance related stuff
;;
;; the init operations are carried out by after-method on initialize-instance
;; [Q?] what does make-instance do?
(c/defmethod c/initialize-instance :after ((class standard-class) &key direct-superclasses direct-slots)
             (let ((supers (or direct-superclasses (list (c/find-class 'c/standard-object)))))
               (setf (c/class-direct-superclasses class) supers)
               (dolist (superclass supers)
                 (push class (c/class-direct-subclasses superclass))))

             (let ((slots (mapcar #'(lambda (slot-properties)
                                      (apply #'c/make-direct-slot-definition slot-properties))
                                  direct-slots)))
               (setf (c/class-direct-slots class) slots)
               (dolist (direct-slot slots)
                 (dolist (reader (c/slot-definition-readers direct-slot))
                   (c/add-reader-method
                    class reader (c/slot-definition-name direct-slot)))
                 (dolist (writer (c/slot-definition-writers direct-slot))
                   (c/add-writer-method
                    class writer (c/slot-definition-name direct-slot)))))

             (c/finalize-inheritance class))

(defun c/finalize-inheritance (class)
  (setf (c/class-precedence-list class)
        (c/compute-class-precedence-list class))
  (setf (c/class-slots class)
        (c/compute-slots class))
  ;; CL idiom to emphasize that the function returns no usable result
  (values))


;; computing class precedence lists: CLOS spec section entitled "Determining the Class Precedence List"
(defun c/compute-class-precedence-list (class)
  (let ((classes-to-order (c/collect-superclasses* class)))
    (c/topological-sort classes-to-order
                        (remove-duplicates
                         (mapappend #'c/local-precedence-ordering classes-to-order))
                        #'c/std-tie-breaker-rule)))

;; we implement simpler slot inheritance rules than those used by full CLOS: when more than one class
;; in the class precedence list has a slot with a given name, only the direct slot definition metaobject
;; from the most specific class is retained - no mergin of slot properties is supported.

(defun c/collect-superclasses* (class)
  (remove-duplicates (cons class (mapappend #'c/collect-superclasses* (c/class-direct-superclasses class)))))

;; compute-slots responsible for slot inheritance
(defun c/compute-slots (class)
  (mapcar #'(lambda (slot)
              (c/make-effective-slot-definition
               :name (c/slot-definition-name slot)
               :initform (c/slot-definition-initform slot)
               :initfunction (c/slot-definition-initfunction slot)
               :initargs (c/slot-definition-initargs slot)))
          (remove-duplicates
           (mapappend #'c/class-direct-slots (c/class-precedence-list class))
           :key #'c/slot-definition-name
           :from-end t)))

;; PRINTING OBJECTS  '#<Color-Rectangle 34237>'
;; all classes defined with defclass include the class standard-object in their class precedence lists;
;; consequently methods defined on standard-object will, by default, be applicable to instances of these
;; classes.
(c/defgeneric print-object (instance stream))

(c/defmethod print-object ((instance standard-object) stream)
             (c/print-unreadable-object (instance stream :identity t)
                                        (format stream "~:(~S~)" (c/class-name (c/class-of instance))))
             instance)

;; creating:
;; - make-instance

;; initializing:
;; - initialize-instance
;; - reinitialize-instance
;; - shared-initialize

;; interrogating
;; - slot-value
;; - (setf slot-value)
;; - slot-boundp
;; - slot-makunbound
;; - slot-exists-p

;; changing the class of instance
;; - change-class
;; - update-instance-for-different-class

;; ----------

;; ADT called "standartd instance" with following:
;; - (allocate-std-instance <class> <slot-storage>)
;; - (std-instance-p <object>)
;; - (std-instance-class <instance>)
;; - (std-instance-slots <instance>)
;; - (allocate-slot-storage <size> <initial-value>)
;; - (slot-contents <slot-storage> <location>)


(defun c/class-of (object)
  (if (c/std-instance-p object)
      (c/std-instance-class object)
      (c/built-in-class-of object)))


(defun c/slot-location (class slot-name)
  (let ((pos (position slot-name (c/class-slots class) :key #'c/slot-definition-name)))
    (if (null pos)
        (error "The slot ~S is missing from the class ~S." slot-name class)
        pos)))

(defun c/slot-value (instance slot-name)
  (let ((location (c/slot-location (c/class-of instance) slot-name))
        (slots (c/std-instance-slots instance))
        (val (c/slot-contents slots location)))
    (if (eq c/secret-unbound-value val)
        (error "The slot ~S is unbound in the object ~S." slot-name instance)
        val)))

(defun (setf c/slot-value) (new-value instance slot-name)
  (let ((location (c/slot-location (c/class-of instance) slot-name))
        (slots (c/std-instance-slots instance)))
    (setf (c/slot-contents slots location) new-value)))

(defun c/slot-boundp (instance slot-name)
  (let ((location (c/slot-location (c/class-of instance) slot-name))
        (slots (c/std-instance-slots instance)))
    (not (eq c/secret-unbound-value (c/slots-contents slots location)))))

(defun c/slot-makunbound (instance slot-name)
  (let ((location (c/slot-location (c/class-of instance) slot-name))
        (slots (c/std-instance-slots instance)))
    (setf (c/slot-contents slots location) c/secret-unbound-value))
  instance)

(defun c/slot-exists-p (instance slot-name)
  (not (null (find slot-name (c/class-slots (c/class-of instance)) :key #'c/slot-definition-name))))

;; ========== init instances

(c/defgeneric c/make-instance (class &key))
(c/defmethod c/make-instance  ((class symbol) &rest initargs)
             (apply #'c/make-instance (c/find-class class) initargs))

(c/defmethod c/make-instance ((class c/stdandard-class) &rest initargs)
             (let ((instance (c/allocate-instance class)))
               (apply #'c/initialize-instance instance initargs)
               instance))

(c/defgeneric c/initialize-instance (instance &key))
(c/defmethod c/initialize-instance ((instance c/standard-object) &rest initargs)
             (apply #'c/shared-initialize instance t initargs))

(c/defgeneric c/reinitialize-instance (instance &key))
(c/defmethod c/reinitialize-instance ((instance c/standard-object) &rest initargs)
             (apply #'c/shared-initialize instance () initargs))

(c/defgeneric c/shared-initialize (instance slot-names &key))
(c/defmethod c/shared-initialize ((instance standard-object) slot-name &rest all-keys)
             (dolist (slot (c/class-slots (c/class-of instance)))
               (let ((slot-name (c/slot-definition-name slot)))
                 (multiple-value-bind (init-key init-value foundp)
                     (get-properties all-keys (c/slot-definition-initargs slot))
                   (declare (ignore init-key))
                   (if foundp
                       (setf (c/slot-value instance slot-name) init-value)
                       (when (and (not (c/slot-boundp instance slot-name))
                                  (not (null (c/slot-definition-initfunction slot)))
                                  (or (eq slot-names t)
                                      (member slot-name slot-names)))
                         (setf (c/slot-value instance slot-name)
                               (funcall (c/slot-definition-initfunction slot)))))))))

;; pg. 33
