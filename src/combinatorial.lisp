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

#|
See 
- http://msdn.microsoft.com/en-us/magazine/cc163957.aspx
- http://msdn.microsoft.com/en-us/library/aa289166%28VS.71%29.aspx
|#


(defun count-subset-combinations (set-size subset-size)
  "count-subset-combinations SET-SITE SUBSET-SIZE => NUMBER

Determines the number of subsets of size SUBSET-SIZE of a set of size
SET-SIZE. This number is equal to

     n!
 -----------
  k! (n-k)!

with n being SET-SIZE and k being SUBSET-SIZE.

This implementation is, however, more efficient than the naive 
calculation."
  (check-type set-size (integer 0))
  (check-type subset-size (integer 0))
  (let ((n set-size) 
		(k subset-size))
    (declare (type (integer 0) n k))
    (cond ((< n k) 0)
		  ((= n k) 1)
		  (t (let* ((diff (the (integer 0) (- n k)))
					(delta (the (integer 0) (max diff k)))
					(imax (the (integer 0) (min diff k))))
			   (declare (type (integer 0) diff delta imax))
			   (loop 
				  :with answer :of-type (integer 0) = (1+ delta)
				  :for i :of-type (integer 0) :upfrom 2 :to imax
				  :do (setf answer (/ (* answer (+ delta i)) i))
				  :finally (return answer)))))))


(defun compute-lexicographic-subset (nth set-size subset-size)
  "compute-lexicographic-subset M N K => SEQUENCE

Computes the NTH subset of size SUBSET-SIZE of the set of natural numbers
from 0 to SET-SIZE - 1. The subsets are assumed to be in lexicographic order
and this function selects and returns the NTH one (with 0 indicating the 
first)."
  (let ((m nth) (n set-size) (k subset-size))
    (let ((result (make-array k :element-type '(integer 0) :adjustable nil :fill-pointer nil))
		  (a n) (b k) (x (- (count-subset-combinations n k) 1 m)))
      (flet ((largest-value (a b x)
			   (let ((v (- a 1)))
				 (loop
					:while (> (count-subset-combinations v b) x)
					:do (decf v)
					:finally (return v)))))
		(loop 
		   :for i :upfrom 0 :below k
		   :do (setf (aref result i) (largest-value a b x)
					 x (- x (count-subset-combinations (aref result i) b))
					 a (aref result i)
					 b (- b 1)))
		(loop 
		   :for i :upfrom 0 :below k
		   :do (setf (aref result i) (- (- n 1) (aref result i))))
		result))))


(defun factorial (k)
  "factorial K => NUMBER

Calculates the factorial of K. The value K must be a positive integer
number. The factorial is defined as

  factorial(k) = 1                       if k <= 1
  factorial(k) = k * factorial(k - 1)    otherwise

and usually written as k!"
  (check-type k (integer 0))
  (loop
     :with f :of-type (integer 0) = 1
     :for i :of-type (integer 0) :downfrom k :to 1
     :do (setf f (* f i))
     :finally (return f)))
