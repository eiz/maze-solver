(in-package :maze-solver)

(defclass map ()
  ((data :reader map-data
         :initarg :data)
   (start-point :accessor map-start-point)
   (end-point :accessor map-end-point)))

(defun stream-lines (stream)
  (loop for line = (read-line stream nil)
       until (null line) collect line))

(defun string-lines (string)
  (with-input-from-string (stream string)
    (stream-lines stream)))

(defun file-lines (path)
  (with-open-file (stream path)
    (stream-lines stream)))

(defun symbol-class (char)
  (case char
    (#\# 'wall)
    (#\S 'start)
    (#\E 'end)
    (otherwise 'floor)))

(defun parse-map-lines (lines)
  (let* ((height (length lines))
         (width (reduce #'max (mapcar #'length lines)))
         (data (make-array (list height width)))
         (i 0))
    (dolist (line lines)
      (do ((j 0 (1+ j)))
          ((= j (length line)))
        (setf (aref data i j)
              (symbol-class (aref line j))))
      (incf i))
    (make-instance 'map :data data)))

(defun parse-map-string (string)
  (parse-map-lines (string-lines string)))

(defun parse-map-file (path)
  (parse-map-lines (file-lines path)))
