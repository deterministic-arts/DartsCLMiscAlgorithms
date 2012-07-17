#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Algorithms Library
  Copyright (c) 2011 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#


(in-package "DARTS.LIB.ALGORITHMS")

(defun compute-distances-in-graph (cardinality start-node list-neighbours)

  "Compute the distances in a graph

This function computes the distance between a start node and all other
nodes in a graph. The function assumes, that the graph has CARDINALITY
number of nodes, and that each node is uniquely identified by a single
integer number in the range [0, CARDINALITY).

The value of START-NODE is the number of node, all distances will be
computed for. LIST-NEIGHBOURS is a function, which will be called with
a node number, and which must return a list ((ID1 . DISTANCE1) ... (IDn . DISTANCEn))
where each IDk is the number of a neighbouring node, and each DISTANCEk
is the distance from the node passed in to the node identified by 
IDk.

The function returns an array, which contains the distance from the
START-NODE to the node, whose number is the slot index."

  (labels ((queue-item-< (item-1 item-2)
			 (let ((distance-1 (car item-1))
				   (distance-2 (car item-2)))
			   (cond
				 ((< distance-1 distance-2) t)
				 ((< distance-2 distance-1) nil)
				 (t (< (cdr item-1) (cdr item-2)))))))

    (let ((visited (make-array cardinality :element-type 'bit :initial-element 0))
		  (distances (make-array cardinality :element-type 'real :initial-element 0))
		  (queue (make-heap :initial-size cardinality :comparator #'queue-item-<)))
	  
      (labels ((enqueue-neighbours (node distance)
				 (dolist (neighbour (funcall list-neighbours node))
				   (let ((number (car neighbour))
						 (weight (cdr neighbour)))
					 (heap-push (cons (+ weight distance) number) queue))))
			   (dequeue-item ()
				 (loop 
					:for item = (heap-pop queue nil) :then (heap-pop queue nil)
					:while item
					:when (= 0 (bit visited (cdr item)))
					:do (return-from dequeue-item item)
					:finally (return-from dequeue-item nil))))
		
		(setf (bit visited start-node) 1)
		(enqueue-neighbours start-node 0)
		
		(loop 
		   :for item = (dequeue-item)
		   :while item
		   :do (let ((distance (car item))
					 (number (cdr item)))
				 (setf (aref distances number) distance)
				 (setf (bit visited number) 1)
				 (enqueue-neighbours number distance)))
		
		distances))))


(defun find-shortest-path-in-graph (cardinality start-node target-node list-neighbours)
  
  "Finds the shortest path between two nodes

This function finds the shortest path between START-NODE and TARGET-NODE.
The graph is assumed to have CARDINALITY number of nodes, and each node in
the graph is represented uniquely by an integer number in the range 
[0, CARDINALITY).

The value of LIST-NEIGHBOURS must be a function, which when called with a 
node number, returns a list ((ID1 . DISTANCE1) ... (IDn . DISTANCEn)). Each
IDk value is a node number and each DISTANCEk is the corresponding distance
from the input node number to node IDk.

The function returns two values PATH and DISTANCE, where PATH is a list
of node numbers representing the path found, and DISTANCE is the overall
distance between START-NODE and TARGET-NODE."
  
  (labels ((queue-item*-< (item-1 item-2)
			 (let ((distance-1 (car item-1))
				   (distance-2 (car item-2)))
			   (cond
				 ((< distance-1 distance-2) t)
				 ((< distance-2 distance-1) nil)
				 (t (< (cadr item-1) (cadr item-2)))))))
	
    (let ((visited (make-array cardinality :element-type 'bit :initial-element 0))
		  (queue (make-heap :initial-size cardinality :comparator #'queue-item*-<)))
	  
      (labels ((enqueue-neighbours (node distance path)
				 (dolist (neighbour (funcall list-neighbours node))
				   (let ((number (car neighbour))
						 (weight (cdr neighbour)))
					 (let ((item (list* (+ weight distance) number path)))
					   (heap-push item queue)))))
			   (dequeue-item ()
				 (loop 
					:for item = (heap-pop queue nil) :then (heap-pop queue nil)
					:while item
					:when (= 0 (bit visited (cadr item)))
					:do (return-from dequeue-item item)
					:finally (return-from dequeue-item nil))))
		
		(setf (bit visited start-node) 1)
		(enqueue-neighbours start-node 0 '())
		
		(loop :for item = (dequeue-item)
		   :while item
		   :do (let ((distance (car item))
					 (number (cadr item))
					 (path (cdr item)))
				 (if (= number target-node) 
					 (return-from find-shortest-path-in-graph (values (nreverse path) distance))
					 (progn
					   (setf (bit visited number) 1)
					   (enqueue-neighbours number distance path)))))
		
		(values nil nil)))))
