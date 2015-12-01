(in-package :maze-solver)

(defun solve-maze-from-string (string)
  (let ((map (parse-map-string string)))
    (find-path map (map-start-point map) (map-end-point map))))

(defun solve-maze-from-file (path)
  (let ((map (parse-map-file path)))
    (find-path map (map-start-point map) (map-end-point map))))
