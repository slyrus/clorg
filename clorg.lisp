
(in-package #:clorg)

(define-application-frame clorg ()
  ()
  (:pane
   (scrolling ()
     (make-pane :interactor)))
  (:menu-bar nil)
  (:default-initargs :width 1000 :height 800))

(defun running-sum (list)
  (let ((sum 0))
    (mapcar (lambda (x) (incf sum x))
            list)))

(defun org-chart-arc-drawer (stream from-node to-node x1 y1 x2 y2
                             &rest drawing-options
                             &key &allow-other-keys)
  (declare (ignore from-node to-node))
  (let ((mid-y (/ (+ y1 y2) 2)))
    (apply #'draw-line* stream x1 y1 x1 mid-y drawing-options)
    (apply #'draw-line* stream x1 mid-y x2 mid-y drawing-options)
    (apply #'draw-line* stream x2 mid-y x2 y2 drawing-options)))

(define-presentation-type org-chart-node ())

(define-presentation-method present (org-chart-node (type org-chart-node) pane view &key)
  (let* ((org-chart (pane-org-chart pane))
         (emp (get-employee org-chart org-chart-node)))
    (let ((sizes (mapcar (lambda (x) (multiple-value-list (text-size pane x)))
                         (list (name emp)
                               (role emp)))))
      (let* ((widths (mapcar #'first sizes))
             (max-width (apply #'max widths))
             (heights (mapcar #'second sizes))
             (total-height (apply #'+ heights))
             (offsets (running-sum heights))
             (left-margin 12)
             (right-margin 12)
             (top-margin 6)
             (bottom-margin 6)
             (name (name emp)))
        (when name
          (draw-rectangle* pane
                           0
                           0
                           (+ max-width left-margin right-margin)
                           (+ total-height top-margin bottom-margin)
                           :ink +red+ :filled nil :line-thickness 2)
          (draw-text* pane name
                      (+ (/ max-width 2) left-margin)
                      (+ top-margin (first offsets))
                      :align-x :center
                      :align-y :bottom)
          (terpri pane)
          (draw-text* pane (role emp)
                      (+ (/ max-width 2) left-margin)
                      (+ top-margin (second offsets))
                      :align-x :center
                      :align-y :bottom)
          (terpri pane))))))

(define-presentation-type org-chart ())

(define-presentation-method present (org-chart (type org-chart) pane view &key)
  (with-drawing-options (pane :text-style (make-text-style nil nil 12))
    (flet ((node-children (node)
           (graph:neighbors (org-chart-graph org-chart) node)))
    (format-graph-from-roots
     (list 1)
     (lambda (node s)
       (present node 'org-chart-node :stream s))
     #'node-children
     :arc-drawer #'org-chart-arc-drawer
     :arc-drawing-options (list :ink +gray20+ :line-thickness 2)
     :orientation (pane-orientation pane)
     :stream pane))))

(defclass clorg-pane (application-pane)
  ((org-chart :initarg :org-chart :initform nil :accessor pane-org-chart)
   (orientation :initarg :orientation :initform :vertical :accessor pane-orientation)))

(define-application-frame clorg-frame ()
  ((org-chart :accessor clorg-frame-org-chart :initarg :org-chart))
  (:menu-bar nil)
  (:panes
   (org-chart-pane
    (make-pane 'clorg-pane
               :org-chart (clorg-frame-org-chart *application-frame*)
               :display-function #'display))))

(defmethod display ((frame clorg-frame) pane)
  (let* ((org-chart (pane-org-chart pane)))
    (when org-chart
      (present org-chart 'org-chart :stream pane))))

(defun clorg-frame (org-chart &key background)
  (let ((frame (make-application-frame 'clorg-frame :org-chart org-chart)))
    (if background
        (bt:make-thread (lambda ()
                          (run-frame-top-level frame))
                        :initial-bindings `((*default-server-path* . ',*default-server-path*)))
        (run-frame-top-level frame))
    frame))
