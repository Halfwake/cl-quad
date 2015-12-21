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

(defmacro no-value ()
  "An initial value for mandatory slots."
  '(error "No value provided."))

(defclass point ()
  ((x :initform (no-value)
       :initarg :x
       :reader x)
   (y :initform (no-value)
      :initarg :y
      :reader y))
  (:documentation "Represents a point in 2D space. Used for testing."))

(defmethod print-object ((point point) stream)
  (format stream "(point :x ~a :y ~a)"
	  (x point) (y point)))
