(defpackage simple-graph
  (:use :cl)
  (:export #:graph #:make-graph #:graph-nodes #:graph-edges
           #:add-node #:add-edge
           #:to-dot #:to-dot-with-stream)
  (:documentation "A very simple Graph library."))

(in-package :simple-graph)

(defstruct graph
  "Hash Tables of nodes and edges."
  (nodes (make-hash-table :test #'equal) :type hash-table)
  (edges (make-hash-table :test #'equal) :type hash-table))

(defun add-node (graph node)
  "Mutably add some NODE data to a GRAPH. Avoids adding the same node twice. Yields
a truthy value when a new node was successfully added, NIL otherwise."
  (unless (gethash node (graph-nodes graph))
    (setf (gethash node (graph-nodes graph)) t)))

#++
(let ((g (make-graph)))
  (add-node g "A"))

(defun add-edge (graph from to)
  "Add an edge between two nodes. The edge ids should be `equal' to the
corresponding node values themselves, but nothing enforces this. Edges between
nodes can be added before the nodes themselves exist. Multiple edges between the
same nodes can be added."
  (cond ((not (gethash from (graph-edges graph)))
         (setf (gethash from (graph-edges graph)) (list to)))
        (t (push to (gethash from (graph-edges graph))))))

#++
(let ((g (make-graph)))
  (add-node g :a)
  (add-node g :b)
  (add-edge g :a :b)
  (add-edge g :a :c)
  (add-edge g :c :d)
  g)

(defun leaf? (graph node)
  "Is the given NODE a leaf node?"
  (and (gethash node (graph-nodes graph))
       (null (gethash node (graph-edges graph)))))

#++
(let ((g (make-graph)))
  (add-node g :a)
  (leaf? g :a))

(defun leaves (graph)
  "A list of all the current leaf nodes."
  (let ((ls '()))
    (maphash (lambda (node _)
               (declare (ignore _))
               (when (leaf? graph node)
                 (push node ls)))
             (graph-nodes graph))
    ls))

#++
(let ((g (make-graph)))
  (add-node g :a)
  (add-node g :b)
  (add-node g :c)
  (add-edge g :a :c)
  (leaves g))

(defun to-dot-with-stream (graph stream)
  "Write the GRAPH in dot format to some STREAM."
  (format stream "graph {~%")
  (maphash (lambda (node _)
             (declare (ignore _))
             (format stream "  \"~a\";~%" node))
           (graph-nodes graph))
  (maphash (lambda (node edges)
             (dolist (edge edges)
               (format stream "  \"~a\" -- \"~a\";~%" node edge)))
           (graph-edges graph))
  (format stream "}"))

(defun to-dot (graph)
  "Write the GRAPH to dot format as a string."
  (with-output-to-string (stream)
    (to-dot-with-stream graph stream)))

#++
(let ((g (make-graph)))
  (add-node g "A")
  (add-node g "A")
  (add-node g "A")
  (add-node g "B")
  (add-node g "C")
  (add-node g "D")
  (add-edge g "A" "B")
  (add-edge g "A" "C")
  (add-edge g "B" "D")
  (add-edge g "C" "D")
  (to-dot g))
