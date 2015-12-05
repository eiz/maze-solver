(in-package :maze-solver)

(defclass priority-queue ()
  ((entries :reader priority-queue-entries
            :initform (make-array '(0) :adjustable t))
   (comparer :reader priority-queue-comparer
             :initarg :comparer
             :initform (error "Priority queue must have a comparer."))))

(defgeneric priority-queue-insert (queue value))
(defgeneric priority-queue-top (queue))
(defgeneric priority-queue-pop (queue))
(defgeneric priority-queue-size (queue))

(defmethod priority-queue-size ((queue priority-queue))
  (car (array-dimensions (priority-queue-entries queue))))

(defmethod priority-queue-insert ((queue priority-queue) value)
  (let ((insertion-point (priority-queue-size queue))
        (entries (priority-queue-entries queue)))
    (adjust-array entries (list (1+ insertion-point)))
    (setf (aref entries insertion-point) value)
    (do ((index insertion-point (floor (/ (1- index) 2))))
        ((= index 0))
      (let ((parent-index (floor (/ (1- index) 2))))
        (if (funcall (priority-queue-comparer queue)
                     (aref entries parent-index) value)
            (let ((temp (aref entries index)))
              (setf (aref entries index) (aref entries parent-index)
                    (aref entries parent-index) temp))
            (return-from priority-queue-insert value))))
    value))

(defmethod priority-queue-top ((queue priority-queue))
  (when (zerop (priority-queue-size queue))
    (error "Priority queue is empty."))
  (aref (priority-queue-entries queue) 0))

(defmethod priority-queue-pop ((queue priority-queue))
  (when (zerop (priority-queue-size queue))
    (error "Priority queue is empty."))
  (let* ((entries (priority-queue-entries queue))
         (comparer (priority-queue-comparer queue))
         (top (aref entries 0))
         (last (aref entries (1- (priority-queue-size queue))))
         (n 0)
         (c 2))
    (adjust-array entries (list (1- (priority-queue-size queue))))
    (if (= 0 (priority-queue-size queue))
        top
        (prog1 top
          (loop
             (unless (< c (priority-queue-size queue))
               (return))
             (when (funcall comparer (aref entries c) (aref entries (1- c)))
               (decf c))
             (if (funcall comparer last (aref entries c))
                 (setf (aref entries n) (aref entries c))
                 (return))
             (setf n c
                   c (+ 2 (* 2 c))))
          (when (and (= c (priority-queue-size queue))
                     (funcall comparer last (aref entries (decf c))))
            (setf (aref entries n) (aref entries c)
                  n c))
          (setf (aref entries n) last)))))

(5am:test max-heap-sorts
  (let ((q (make-instance 'priority-queue :comparer #'<)))
    (dolist (i '(1 2 8 36 5 4 7 9 12))
      (priority-queue-insert q i))
    (let ((sorted (loop while (> 0 (priority-queue-size q))
                     collect (priority-queue-pop q))))
      (5am:is (every #'= sorted '(36 12 9 8 7 5 4 2 1))))))

(5am:test min-heap-sorts
  (let ((q (make-instance 'priority-queue :comparer #'>)))
    (dolist (i '(1 2 8 36 5 4 7 9 12))
      (priority-queue-insert q i))
    (let ((sorted (loop while (> 0 (priority-queue-size q))
                     collect (priority-queue-pop q))))
      (5am:is (every #'= sorted '(1 2 4 5 7 8 9 12 36))))))

