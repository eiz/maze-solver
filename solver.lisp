(in-package :maze-solver)

(defun solve-maze-from-string (string)
  (let ((map (parse-map-string string)))
    (find-path map (map-start-point map) (map-end-point map))))

(defun solve-maze-from-file (path)
  (let ((map (parse-map-file path)))
    (find-path map (map-start-point map) (map-end-point map))))

(defun draw-solution (map path visited-list)
  (let* ((data (map-data map))
         (result (make-array (array-dimensions data) :initial-element #\Space)))
    (with-output-to-string (stream)
      (loop for y from 0 below (map-height map) do
           (progn
             (loop for x from 0 below (map-width map) do
                  (progn
                    (when (eq (aref data y x) 'wall)
                      (setf (aref result y x) #\#))
                    (when (find-if (lambda (point)
                                     (and (= (car point) x) (= (cdr point) y)))
                                   visited-list)
                      (setf (aref result y x) #\?))
                    (when (find-if (lambda (point)
                                     (and (= (car point) x) (= (cdr point) y)))
                                   path)
                      (setf (aref result y x) #\x))
                    (princ (aref result y x) stream)))
             (princ #\Newline stream))))))

(5am:test open-path-examines-optimal-node-count
  (multiple-value-bind (path visited total-nodes)
      (solve-maze-from-file (%test-case-path "open"))
    (declare (ignore total-nodes))
    (5am:is (= (length path) (length visited)))))
