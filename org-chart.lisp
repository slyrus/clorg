
(in-package #:clorg)

(defclass org-item ()
  ((name :accessor name :initarg :name :initform nil)
   (role :accessor role :initarg :role :initform nil)
   (note :accessor note :initarg :note :initform nil))
  (:documentation "Base class of items to go on an org chart"))

(defclass employee (org-item) ())

(defclass org-chart ()
  ((count :accessor org-chart-employee-counter :initform 1)
   (hash-table :accessor org-chart-hash-table :initform (make-hash-table :test 'equal))
   (graph :accessor org-chart-graph :initform (make-instance 'graph:digraph))))

(defun get-employee (org-chart id)
  (gethash id (org-chart-hash-table org-chart)))

(defun add-employee (org-chart emp-spec)
  (with-assoc:with-assoc (name role note reports)
      (alexandria:plist-alist emp-spec)
    (let* ((emp (make-instance 'employee :name name :role role :note note))
           (emp-id (org-chart-employee-counter org-chart)))
      (setf (gethash emp-id (org-chart-hash-table org-chart)) emp)
      (incf (org-chart-employee-counter org-chart))
      (graph:add-node (org-chart-graph org-chart) emp-id)
      (loop for report-spec in reports
         do (let ((report-id (add-employee org-chart report-spec)))
              (graph:add-edge (org-chart-graph org-chart) (list emp-id report-id))))
      emp-id)))

(defun make-org-chart (employee-spec-list)
  (let ((org-chart (make-instance 'org-chart)))
    (loop for emp-spec in employee-spec-list
       do (add-employee org-chart emp-spec))
    org-chart))

