(defpackage simple-graph
  (:use :cl)
  (:export #:graph #:make-graph #:graph-nodes #:graph-edges
           #:add-node #:add-edge
           #:to-dot)
  (:documentation "A very simple Graph library."))

(defstruct graph
  "Lists of nodes and edges. No attempts to be clever are made."
  (nodes nil :type list)
  (edges nil :type list))

(defun add-node (graph node)
  "Mutably add some NODE data to a GRAPH."
  (push node (graph-nodes graph)))

(defun add-edge (graph from to)
  "Add an edge between two nodes. The edge ids should be `equal' to the
corresponding node values themselves, but nothing enforces this."
  (let ((pair (cons from to)))
    (push pair (graph-edges graph))))

(defun to-dot (graph)
  "Write the graph to DOT format."
  (with-output-to-string (stream)
    (format stream "graph {~%")
    (dolist (node (graph-nodes graph))
      (format stream "  \"~a\";~%" node))
    (dolist (edge (graph-edges graph))
      (destructuring-bind (a . b) edge
        (format stream "  \"~a\" -- \"~a\";~%" a b)))
    (format stream "}")))

#++
(let ((g (make-graph)))
  (add-node g "A")
  (add-node g "B")
  (add-node g "C")
  (add-node g "D")
  (add-edge g "A" "B")
  (add-edge g "A" "C")
  (add-edge g "B" "D")
  (add-edge g "C" "D")
  (to-dot g))
