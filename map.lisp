(in-package :maze-solver)

(defclass map ()
  ((data :reader map-data
         :initarg :data)
   (start-point :accessor map-start-point)
   (end-point :accessor map-end-point)))

(defun map-width (map)
  (cadr (array-dimensions (map-data map))))

(defun map-height (map)
  (car (array-dimensions (map-data map))))

(defun map-at (map x y)
  (aref (map-data map) y x))

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
    ((#\S #\s) 'start)
    ((#\E #\e) 'end)
    (otherwise 'floor)))

(defun parse-map-lines (lines)
  (let* ((height (length lines))
         (width (reduce #'max (mapcar #'length lines)))
         (data (make-array (list height width)))
         (y 0)
         (map (make-instance 'map :data data)))
    (dolist (line lines)
      (do ((x 0 (1+ x)))
          ((= x (length line)))
        (let ((class (symbol-class (aref line x))))
          (setf (aref data y x) class)
          (when (eq class 'start)
            (setf (map-start-point map) (cons x y)))
          (when (eq class 'end)
            (setf (map-end-point map) (cons x y)))))
      (incf y))
    map))

(defun parse-map-string (string)
  (parse-map-lines (string-lines string)))

(defun parse-map-file (path)
  (parse-map-lines (file-lines path)))
