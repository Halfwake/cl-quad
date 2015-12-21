;;     Copyright 2015 Andrew "Drew" Dudash
;;
;;     This file is part of cl-quad.
;;
;;     cl-quad is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;;
;;     cl-quad is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with cl-quad.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:cl-quad)

;; Define the top class and exposed methods.

(defclass quad-tree ()
  ((root :initform nil
	 :initarg :root)
   (bound :initform (no-value)
	  :initarg :bound)
   (size :initform 1
	 :initarg :size))
  (:documentation "Represents a point region quad tree with the given bucket size and initial bounds. Bucket size defaults to one."))

(defgeneric insert (tree item)
  (:documentation "Return a new tree with item inserted."))

(defmethod insert ((tree quad-tree) item)
  (with-slots (root bound size) tree
    (make-instance 'quad-tree
		   :root (insert-helper root item size bound)
		   :bound bound
		   :size size)))

(defgeneric locate (tree item &key test)
  (:documentation "Return the matching item."))

(defmethod locate ((tree quad-tree) item &key test)
  (with-slots (root bound size) tree
    (locate-helper root item size bound :test test)))


(defgeneric purge (tree item &key test)
  (:documentation "Return a new tree with item removed."))

(defmethod purge ((tree quad-tree) item &key test)
  (with-slots (root bound size) tree
    (make-instance 'quad-tree
		   :root (purge-helper root item bound :test test)
		   :bound bound
		   :size size)))

;; Define the node classes.

(defclass quad-tree-node-branch ()
  ((north-east :initform (no-value)
	       :initarg :ne
	       :reader ne)
   (north-west :initform (no-value)
	       :initarg :nw
	       :reader nw)
   (south-west :initform (no-value)
	       :initarg :sw
	       :reader sw)
   (south-east :initform (no-value)
	       :initarg :se
	       :reader se)))

(defclass quad-tree-node-leaf ()
  ((max-size :initform 1
	     :initarg :size
	     :reader max-size)
   (elements :initform (list)
	     :initarg :elements
	     :reader elements)))

;; Define the helper methods.

(defgeneric insert-helper (node item size bound)
  (:documentation "Return a new node with the given item inserted."))

(defmethod insert-helper ((node null) item size bound)
  (let ((new-leaf (make-instance 'quad-tree-node-leaf :size size)))
    (insert-helper new-leaf item size bound)))

(defmethod insert-helper ((node quad-tree-node-leaf) item size bound)
  (let ((elements (elements node)))
    (flet ((room-in-node-p ()
	     (< (length elements) size)))
      (if (room-in-node-p)
	  (make-instance 'quad-tree-node-leaf
			 :size size
			 :elements (cons item elements))
	  (reduce (lambda (node item)
		    (insert-helper node item size bound))
		  (cons item elements)
		  :initial-value (make-instance 'quad-tree-node-branch
						:ne nil
						:nw nil
						:sw nil
						:se nil))))))

(defmethod insert-helper ((node quad-tree-node-branch) item size bound)
  (case (quadrant bound item)
    ((ne origin) (make-instance 'quad-tree-node-branch
				:ne (insert-helper (ne node) item size (north-east bound))
				:nw (nw node)
				:sw (sw node)
				:se (se node)))
    ((nw) (make-instance 'quad-tree-node-branch
			 :ne (ne node)
			 :nw (insert-helper (nw node) item size (north-west bound))
			 :sw (sw node)
			 :se (se node)))
    ((sw) (make-instance 'quad-tree-node-branch
			 :ne (ne node)
			 :nw (nw node)
			 :sw (insert-helper (sw node) item size (south-west bound))
			 :se (se node)))
    ((se) (make-instance 'quad-tree-node-branch
			 :ne (ne node)
			 :nw (nw node)
			 :sw (sw node)
			 :se (insert-helper (se node) item size (south-east bound))))))


(defgeneric purge-helper (node item bounds &key test)
  (:documentation "Returns a node that doesn't contain the given item."))

(defmethod purge-helper ((node null) item bound &key test)
  (declare (ignore item bound test))
  node)

(defmethod purge-helper ((node quad-tree-node-leaf) item bound &key test)
  (let ((new-elements (remove item (elements node) :test test)))
    (if new-elements
	(make-instance 'quad-tree-node-leaf :elements new-elements)
	nil)))

(defmethod purge-helper ((node quad-tree-node-branch) item bound &key test)
 (case (quadrant bound item)
   ((ne origin) (make-instance 'quad-tree-node-branch
			       :ne (purge-helper (ne node) item (north-east bound) :test test)
			       :nw (nw node)
			       :sw (sw node)
			       :se (se node)))
   ((nw) (make-instance 'quad-tree-node-branch
			:ne (ne node)
			:nw (purge-helper (nw node) item (north-west bound) :test test)
			:sw (sw node)
			:se (se node)))
   ((sw) (make-instance 'quad-tree-node-branch
			:ne (ne node)
			:nw (nw node)
			:sw (purge-helper (sw node) item (south-west bound) :test test)
			:se (se node)))
   ((se) (make-instance 'quad-tree-node-branch
			:ne (ne node)
			:nw (nw node)
			:sw (sw node)
			:se (purge-helper (se node) item (south-east bound) :test test)))))


(defgeneric locate-helper (node item size bound &key test)
  (:documentation "Returns two values. The first value is item found or nil if it could not be found and the second value is true if the item is found otherwise nil."))

(defmethod locate-helper ((node null) item size bound &key test)
  (declare (ignore item size bound test))
  (values nil nil))

(defmethod locate-helper ((node quad-tree-node-leaf) item size bound &key test)
  (find item (elements node) :test test))

(defmethod locate-helper ((node quad-tree-node-branch) item size bound &key test)
  (case (quadrant bound item)
    ((ne origin) (locate-helper (ne node) item size (north-east bound) :test test))
    ((nw) (locate-helper (nw node) item size (north-west bound) :test test))
    ((sw) (locate-helper (sw node) item size (south-west bound) :test test))
    ((se) (locate-helper (se node) item size (south-east bound) :test test))))

;; Define some print methods for debugging.

(defmethod print-object ((node quad-tree) stream)
  (with-slots (root) node
    (format stream "~a" root)))

(defmethod print-object ((node quad-tree-node-leaf) stream)
  (format stream "(leaf :elements ~a)" (elements node)))

(defmethod print-object ((node quad-tree-node-branch) stream)
  (format stream "(branch :ne ~a :nw ~a :sw ~a :se ~a)"
	  (ne node) (nw node) (sw node) (se node)))

;; Tests

(test insert-and-locate
  (flet ((point-test (a b)
	   (and (= (x a) (x b))
		(= (y a) (y b)))))
    (let* ((points (list (make-instance 'point
					:x 5
					:y 5)
			 (make-instance 'point
					:x 7
					:y 7)
			 (make-instance 'point
					:x 3
					:y 3)
			 (make-instance 'point
					:x 7
					:y 3)
			 (make-instance 'point
					:x 3
					:y 7)
			 (make-instance 'point
					:x 2
					:y 2)))
	   (tree (reduce #'insert
			 points
			 :initial-value
			 (make-instance 'quad-tree
					:bound (make-instance 'bound
							      :x-low 0
							      :x-high 8
							      :y-low 0
							      :y-high 8)))))
      (loop for point in points
	 do (is (locate tree point :test #'point-test)))
      (is (not (locate (purge tree
			      (make-instance 'point :x 2 :y 2)
			      :test #'point-test)
		       (make-instance 'point :x 2 :y 2)
		       :test #'point-test))))))
