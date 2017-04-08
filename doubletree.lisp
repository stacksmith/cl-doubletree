(in-package :cl-doubletree)
;;
;; It is not without much self-examination that I write this.  I think I truly
;; grasp the consequences of having a double-link-list that actually has the
;; linkage information in the object instead of a separate structure...
;;
;;==============================================================================
;; 
(defclass linkage ()
  ((prev :accessor prev :type linkage)
   (next :accessor next :type linkage)))

(defmethod initialize-instance :after ((obj linkage) &key)
  (setf (prev obj) obj
	(next obj) obj))

(defun link (new other &key (after t))
  "Link new object before or after other object; return new"
  (with-slots (prev next) new
    (if after
      (setf prev other
	    next (next other))
      (setf next other
	    prev (prev other)))
    (setf (next prev) new
	  (prev next) new)))

(defmethod link-before (other (new linkage))
  "Link new object before other object; return new"
  (with-slots (prev next) new
    (setf next other
	  prev (prev other)
	  (next prev) new
	  (prev next) new)))

(defmethod link-after (other (new linkage))
  "Link new object after other object; return new"
  (with-slots (prev next) new
    (setf prev other
	  next (next other)
	  (next prev) new
	  (prev next) new)))

(defun unlink (it)
  "Unlink it, returning prev"
  (with-slots (prev next) it
      (setf (next prev) next
	    (prev next) prev)))
(defmethod terminal ((link linkage)) t)
(defmethod terminator ((link linkage))
  link)
;;==============================================================================
(defclass node (linkage)
  ((children :accessor children 
	     :initform (make-instance 'linkage))
   (dad      :accessor dad :initform nil 
	     )))

(defmethod nodep ((obj t)) nil)
(defmethod nodep ((node node)) node)

(defmethod initialize-instance :after ((obj node) &key)
)

(defun make-root (obj)
    (let ((l (make-instance 'linkage))) ;root objects must have a fake dad
      (setf (next l) obj
	    (prev l) obj
	    (next obj) l
	    (prev obj) l)))

(defmethod link-as-child-of (dad (new node))
  (let ((q (children dad)))
    (setf (dad new) dad
	  (prev new) q
	  (next new) q
	  (prev q) new
	  (next q) new
)))

(defmethod link-before :after (other (new node))
  (setf (dad new) (dad other)))

(defmethod link-after :after (other (new node))
  (setf (dad new) (dad other)))

(defun first (node)
  (next (children node)))

(defun first-node (node)
  (nodep (next (children node))))

(defun next-node (node)
  (nodep (next node)))

(defun prev-node (node)
  (nodep (prev node)))
  
(defmethod terminal ((node node)) nil)
(defmethod terminator ((node node))
  (children (dad node)))
;;==============================================================================
(defun for-next (fun thing)
  "Apply (fun element) to each element in dlist moving in forward.   If :inclusive, include thing as well."
;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type function fun))
  (let  ((term (children (dad thing))))
    (labels ((prim (fun thing)
	       (declare (type function fun))
	       (unless (eq thing term)
		 (funcall fun thing)
		 (prim fun (dad thing)))))
      (prim fun thing)))
)
;;==============================================================================
(defun for-next-until (fun thing)
  "Apply (fun element) to each element in dlist moving in forward.   If :inclusive, include thing as well. The function must return nil to continue; t terminates the loop.
Return element and result of function, or nil if all loop complete."
  ;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type function fun))
  (let ((term (terminator thing); (children (dad thing))
	  ))
    (loop
       for element = thing then (next element)
       when (eq term element) return nil
       do (let ((result (funcall fun element)))
	    (when result
	      (return (values element result)))))))

;;==============================================================================
(defun for-prev (fun thing)
  "Apply (fun element) to each element in dlist moving in backward.   If :inclusive, include thing as well."
  ;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type function fun))
  (let ((term (terminator thing)))
    (loop
       for element =  thing then (prev element)
       when (eq term element) return nil
       do (funcall fun element))))
;;==============================================================================
(defun for-prev-until (fun thing)
  "Apply (fun element) for each element in dlist thing,  moving backward.   If :inclusive, include thing as well.
The function must return nil to continue; t terminates the loop.
Return element and result of function, or nil if all loop complete."
  ;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type function fun))
  (let ((term (terminator thing)))
    (loop
       for element = thing then (prev element)
       when (eq term element) return nil
       do (let ((result (funcall fun element)))
	    (when result
	      (return (values element result)))))))


;;====================================a==========================================
(defun for-up(fun node)
  (declare (type function fun))
  (when node
    (funcall fun node)
    (for-up fun (dad node))))

;;====================================a==========================================
(defun traverse-next (node)
  (labels ((go-next-or-up (node)
	     (when node
	       (let ((n (next-node node)))
		 (if n
		     n
		     (go-next-or-up (dad node))))))
	   (go-deep (node)
	     (when node
	       (let ((n (first-node node)))
		 (if n
		     (go-deep n)
		     node)))))
    (go-deep (go-next-or-up node))))




