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
  (let* ((data (map-data map)))
    (with-output-to-string (stream)
      (loop for y from 0 below (map-height map) do
           (progn
             (loop for x from 0 below (map-width map) do
                  (let ((char #\Space))
                    (when (eq (aref data y x) 'wall)
                      (setf char #\#))
                    (when (find-if (lambda (point)
                                     (and (= (car point) x) (= (cdr point) y)))
                                   visited-list)
                      (setf char #\?))
                    (when (find-if (lambda (point)
                                     (and (= (car point) x) (= (cdr point) y)))
                                   path)
                      (setf char #\x))
                    (princ char stream)))
             (princ #\Newline stream))))))

(5am:test open-path-examines-optimal-node-count
  (multiple-value-bind (path visited total-nodes)
      (solve-maze-from-file (%test-case-path "open"))
    (declare (ignore total-nodes))
    (5am:is (= (length path) (length visited)))))
