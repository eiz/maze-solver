(in-package :maze-solver)

(defclass map ()
  ((data :reader map-data
         :initarg :data)
   (start-point :accessor map-start-point)
   (end-point :accessor map-end-point)))

(defun string-lines (string)
  (with-input-from-string (stream string)
    (loop for line = (read-line stream nil)
         until (null line) collect line)))

(defun parse-map (string)
  (let* ((lines (string-lines string))
         (height (length lines))
         (width (reduce #'max (mapcar #'length lines)))
         (data (make-array (list height width))))
    (make-instance 'map :data data)))

