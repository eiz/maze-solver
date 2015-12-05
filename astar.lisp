(in-package :maze-solver)

(defclass node ()
  ((point :reader node-point
          :initarg :point
          :initform (error "Point is required."))
   (best-cost :accessor node-best-cost :initform 'infinity)
   (best-source :accessor node-best-source)
   (original :accessor node-original :initarg :original)))

(defun manhattan-distance (lhs rhs)
  (+ (abs (- (car lhs) (car rhs)))
     (abs (- (cdr lhs) (cdr rhs)))))

(defun node-cost (node destination)
  (if (eq (node-best-cost node) 'infinity)
      most-positive-fixnum ; TODO
      (+ (node-best-cost node)
         (manhattan-distance (node-point node) destination))))

(defun point-equals (lhs rhs)
  (and (= (car lhs) (car rhs))
       (= (cdr lhs) (cdr rhs))))

(defun node-equals (lhs rhs)
  (point-equals (node-point lhs) (node-point rhs)))

(defun node-parent (nodes node)
  (aref nodes (cdr (node-best-source node)) (car (node-best-source node))))

(defun build-path (nodes source destination)
  (let ((result nil))
    (do ((current destination (node-parent nodes current)))
        ((point-equals (node-point current) source))
      (push (node-point current) result))
    (cons source result)))

(defun find-path (map source destination)
  (let ((queue (make-instance 'priority-queue
                              :comparer (lambda (l r)
                                          (> (node-cost l destination)
                                             (node-cost r destination)))))
        (visited (make-hash-table :test #'eq))
        (visited-list nil)
        (total-spaces 0)
        (discovered (make-hash-table :test #'eq))
        (nodes (make-array (list (map-height map) (map-width map))
                           :initial-element nil)))
    (loop for y from 0 below (map-height map) do
         (loop for x from 0 below (map-width map) do
              (when (not (eq (map-at map x y) 'wall))
                (incf total-spaces)
                (setf (aref nodes y x)
                      (make-instance 'node :point (cons x y))
                      (node-original (aref nodes y x)) (aref nodes y x)))))
    (when (null (aref nodes (cdr destination) (car destination)))
      (error "Destination must not be a wall"))
    (when (null (aref nodes (cdr source) (car source)))
      (error "Source must not be a wall"))
    (let ((start-node (aref nodes (cdr source) (car source))))
      (setf (node-best-cost start-node) 0)
      (priority-queue-insert queue start-node)
      (setf (gethash start-node discovered) t))
    (loop
       (when (zerop (priority-queue-size queue))
         (return (values nil (nreverse visited-list) total-spaces)))
       (let* ((current (priority-queue-pop queue))
              (current-x (car (node-point current)))
              (current-y (cdr (node-point current))))
         (push (node-point current) visited-list)
         (when (point-equals (node-point current) destination)
           (return (values (build-path nodes source current)
                           (nreverse visited-list)
                           total-spaces)))
         (setf (gethash current visited) t)
         (labels ((maybe-add-neighbor (x y)
                    (let ((neighbor (aref nodes y x)))
                      (when (and neighbor
                                 (not (gethash (node-original neighbor)
                                               visited)))
                        (let ((new-cost (1+ (node-best-cost current))))
                          (when (and (gethash (node-original neighbor)
                                              discovered)
                                     (< new-cost (node-best-cost neighbor)))
                            ;; Our priority queue doesn't allow changing
                            ;; priorities, so if we find a new best cost for a
                            ;; node that's in the queue, we have to duplicate it
                            (setf 
                             neighbor (make-instance
                                       'node
                                       :point (cons x y)
                                       :original (node-original neighbor))
                             (aref nodes y x) neighbor))
                          (when (eq (node-best-cost neighbor) 'infinity)
                            (setf
                             (node-best-cost neighbor) new-cost
                             (node-best-source neighbor) (node-point current)
                             (gethash (node-original neighbor) discovered) t)
                            (priority-queue-insert queue neighbor)))))))
           (when (< current-y (1- (map-height map)))
            (maybe-add-neighbor current-x (1+ current-y)))
           (when (> current-y 0)
            (maybe-add-neighbor current-x (1- current-y)))
           (when (< current-x (1- (map-width map)))
            (maybe-add-neighbor (1+ current-x) current-y))
           (when (> current-x 0)
            (maybe-add-neighbor (1- current-x) current-y)))))))

(defun %test-case-path (name)
  (cl-fad:merge-pathnames-as-file
   (asdf:system-source-directory :maze-solver)
   "test/"
   (concatenate 'string name ".maze")))

(5am:test no-solution
  (let* ((map (parse-map-file (%test-case-path "no-solution")))
         (path (find-path map (map-start-point map) (map-end-point map))))
    (5am:is (null path))))

(5am:test finds-solutions
  (dolist (test '("complex" "fake-shortcut-mirror" "fork" "trivial"
                  "equidistant" "fake-shortcut" "mean" "side-path" "zero"))
    (let* ((map (parse-map-file (%test-case-path test)))
           (path (find-path map (map-start-point map) (map-end-point map))))
      (5am:is (not (null path))))))
