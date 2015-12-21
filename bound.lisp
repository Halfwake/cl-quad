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

(in-package :cl-quad)

(defclass bound ()
  ((x-low :initform (no-value)
	  :initarg :x-low
	  :reader x-low)
   (x-high :initform (no-value)
	   :initarg :x-high
	   :reader x-high)
   (y-low :initform (no-value)
	  :initarg :y-low
	  :reader y-low)
   (y-high :initform (no-value)
	   :initarg :y-high
	   :reader y-high))
  (:documentation "Represents a 2D bounding box."))

(defmethod print-object ((bound bound) stream)
  (format stream "(bound :x-low ~a :x-high ~a :y-low ~a :y-high ~a)"
	  (x-low bound) (x-high bound) (y-low bound) (y-high bound)))

(defgeneric quadrant (bound item)
  (:documentation "Get the quadrant of a bound an item appears in. Returns 'ne if the item is in the top right corner or on the positive x axis and off the center of the bound. Returns 'nw if the item is in the top left corner or on the positive y axis and off the center of the bound. Returns 'sw if the item is in the bottom left corner or on the negative x axis and off the center of the bound. Returns 'se if the item is in the bottom right corner or on the negative y axis and off the center of the bound. Returns 'origin if the item is at the center of the bound."))

(defmethod quadrant ((bound bound) item)
  (let* ((x (x item))
	 (y (y item))
	 (x-low (x-low bound))
	 (x-high (x-high bound))
	 (y-low (y-low bound))
	 (y-high (y-high bound))
	 (x-center (/ (+ x-low x-high) 2))
	 (y-center (/ (+ y-low y-high) 2)))
    (cond ((and (< x-center x)
		(<= x x-high)
		(<= y-center y)
		(<= y y-high))
	   'ne)
	  ((and (<= x-low x)
		(<= x x-center)
		(< y-center y)
		(<= y y-high))
	   'nw)
	  ((and (<= x-low x)
		(< x x-center)
		(<= y-low y)
		(<= y y-center))
	   'sw)
	  ((and (<= x-center x)
		(<= x x-high)
		(<= y-low y)
		(< y y-center))
	   'se)
	  (t 'origin))))

(defgeneric north-east (bound)
  (:documentation "Returns a new bound containing the north-east bound of the given bound."))

(defmethod north-east ((bound bound))
  (make-instance 'bound
		 :x-low (/ (+ (x-low bound) (x-high bound)) 2)
		 :x-high (x-high bound)
		 :y-low (/ (+ (y-low bound) (y-high bound)) 2)
		 :y-high (y-high bound)))

(defgeneric north-west (bound)
  (:documentation "Returns a new bound containing the north-west bound of the given bound."))

(defmethod north-west ((bound bound))
  (make-instance 'bound
		 :x-low (x-low bound)
		 :x-high (/ (+ (x-low bound) (x-high bound)) 2)
		 :y-low (/ (+ (y-low bound) (y-high bound)) 2)
		 :y-high (y-high bound)))

(defgeneric south-west (bound)
  (:documentation "Returns a new bound containing the south-west bound of the given bound."))

(defmethod south-west ((bound bound))
  (make-instance 'bound
		 :x-low (x-low bound)
		 :x-high (/ (+ (x-low bound) (x-high bound)) 2)
		 :y-low (y-low bound)
		 :y-high (/ (+ (y-low bound) (y-high bound)) 2)))

(defgeneric south-east (bound)
  (:documentation "Returns a new bound containing the south-east bound of the given bound."))

(defmethod south-east ((bound bound))
  (make-instance 'bound
		 :x-low (x-low bound)
		 :x-high (/ (+ (x-low bound) (x-high bound)) 2)
		 :y-low (y-low bound)
		 :y-high (/ (+ (y-low bound) (y-high bound)) 2)))


(test quadrant
  (let ((bound (make-instance 'bound
			      :x-low -4
			      :y-low -4
			      :x-high 4
			      :y-high 4)))
    (is (eq 'origin
	    (quadrant bound (make-instance 'point
					   :x 0
					   :y 0))))
    (is (eq 'ne
	    (quadrant bound (make-instance 'point
					   :x 2
					   :y 2))))
    (is (eq 'ne
	    (quadrant bound (make-instance 'point
					   :x 2
					   :y 0))))
    (is (eq 'nw
	    (quadrant bound (make-instance 'point
					   :x -2
					   :y 2))))
    (is (eq 'nw
	    (quadrant bound (make-instance 'point
					   :x 0
					   :y 2))))
    (is (eq 'sw
	    (quadrant bound (make-instance 'point
					   :x -2
					   :y -2))))
    (is (eq 'sw
	    (quadrant bound (make-instance 'point
					   :x -2
					   :y 0))))
    (is (eq 'se
	    (quadrant bound (make-instance 'point
					   :x 2
					   :y -2))))
    (is (eq 'se
	    (quadrant bound (make-instance 'point
					   :x 0
					   :y -2))))))
