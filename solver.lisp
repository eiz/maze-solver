(in-package :maze-solver)

(defun solve-maze-from-string (string &key draw)
  (let ((map (parse-map-string string)))
    (multiple-value-bind (path visited-list total-nodes)
        (find-path map (map-start-point map) (map-end-point map))
      (values path visited-list total-nodes
              (and draw (draw-solution map path visited-list))))))

(defun solve-maze-from-file (path &key draw)
  (let ((map (parse-map-file path)))
    (multiple-value-bind (path visited-list total-nodes)
        (find-path map (map-start-point map) (map-end-point map))
      (values path visited-list total-nodes
              (and draw (draw-solution map path visited-list))))))

(defun draw-solution (map path visited-list)
  (let* ((data (map-data map))
         (visited-map (make-array
                       (array-dimensions data) :initial-element nil)))
    (with-output-to-string (stream)
      (loop for visited in visited-list do
           (setf (aref visited-map (cdr visited) (car visited)) 'visited))
      (loop for step in path do
           (setf (aref visited-map (cdr step) (car step)) 'in-path))
      (loop for y from 0 below (map-height map) do
           (progn
             (loop for x from 0 below (map-width map) do
                  (princ
                   (cond
                     ((eq (aref data y x) 'start) #\S)
                     ((eq (aref data y x) 'end) #\E)
                     ((eq (aref visited-map y x) 'in-path) #\x)
                     ((eq (aref visited-map y x) 'visited) #\?)
                     ((eq (aref data y x) 'wall) #\#)
                     (t #\Space))
                   stream))
             (princ #\Newline stream))))))

(5am:test open-path-examines-optimal-node-count
  (multiple-value-bind (path visited total-nodes)
      (solve-maze-from-file (%test-case-path "open"))
    (declare (ignore total-nodes))
    (5am:is (= (length path) (length visited)))))
