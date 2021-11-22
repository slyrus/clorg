
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

(defparameter *box-color* (clim:make-rgb-color 0.84 0.88 0.89))

(define-presentation-method present (emp (type employee) pane view &key)
  (let ((sizes (mapcar (lambda (x)
                         (if x
                             (multiple-value-list (text-size pane x))
                             (list 0 0)))
                       (list (name emp)
                             (role emp)
                             (subrole emp)))))
    (let* ((widths (mapcar #'first sizes))
           (max-width (apply #'max widths))
           (heights (mapcar #'second sizes))
           (offsets (running-sum heights))
           (name (name emp))
           (role (role emp))
           (subrole (subrole emp)))
      (when name
        (climi::invoke-surrounding-output-with-border
	 pane
         (lambda (pane)
           (draw-text* pane name
                       (/ max-width 2)
                       (first offsets)
                       :align-x :center
                       :align-y :bottom
                       :text-face :bold)
           (when role
             (draw-text* pane (role emp)
                         (/ max-width 2)
                         (second offsets)
                         :align-x :center
                         :align-y :bottom))
           (when subrole
             (draw-text* pane (subrole emp)
                         (/ max-width 2)
                         (third offsets)
                         :align-x :center
                         :align-y :bottom
                         :text-face :italic)))
         :shape :rectangle
         :filled t
         :ink *box-color*
         :outline-ink *box-color*
         :line-thickness 3
         :padding-left 12
         :padding-right 12
         :padding-top 4
         :padding-bottom 4)))))

(defclass clorg-pane (application-pane)
  ((org-chart :initarg :org-chart :initform nil :accessor pane-org-chart)
   (orientation :initarg :orientation :initform :vertical :accessor pane-orientation)))

(define-presentation-method present (org-chart-node (type org-chart-node) pane view &key)
  (let* ((org-chart (pane-org-chart pane))
         (emp (get-employee org-chart org-chart-node)))
    (present emp 'employee)))

(define-presentation-type org-chart ())

(define-presentation-method present (org-chart (type org-chart) pane view &key)
  (with-drawing-options (pane :text-style (make-text-style nil nil 12))
    (flet ((node-children (node)
           (graph:neighbors (org-chart-graph org-chart) node)))
    (format-graph-from-roots
     (list 1)
     (lambda (node s)
       (present (get-employee org-chart node) 'employee :stream s))
     #'node-children
     :arc-drawer #'org-chart-arc-drawer
     :arc-drawing-options (list :ink +gray20+ :line-thickness 2)
     :orientation (typecase pane
                    (clorg-pane (pane-orientation pane))
                    (t :vertical))
     :stream pane))))

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

(defun write-org-chart-to-pdf-file (org-chart file
                                    &key
                                      (device-type '(1600 1000)))
  (with-open-file (file-stream file :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (clim-pdf:with-output-to-pdf-stream
        (stream file-stream
                :header-comments '(:title (name task))
                :scale-to-fit t
                :device-type device-type)
      (let ((*standard-output* stream))
        (present org-chart)))))
