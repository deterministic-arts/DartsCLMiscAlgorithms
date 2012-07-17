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

(in-package "DARTS.LIB.ARRAY-UTILITIES")

(defun binary-search (element vector predicate
					  &key (key #'identity)
					       (start 0) (end nil))
  "Search for ELEMENT in VECTOR using binary search. The vector is assumed
to be sorted using PREDICATE as the ordering predicate; PREDICATE must honour
the rules defined by the standard function sort.

If KEY is present, then it must be a function taking a single argument. It
is called to extract the key of an element of VECTOR prior to comparing that
key against the ELEMENT to find. If START is supplied, it is the index of the
first element in VECTOR to search; the value defaults to 0. If END is supplied,
it is the index of the first element in VECTOR where the search should stop.
The value defaults to the length of VECTOR.

This function returns two values; the primary value is the element found
or nil, if no matching element exists. The second value is a boolean, which
indicates, whether the search was successful (t) or not (nil)."
  (loop
	 :with lower :of-type fixnum := start 
	 :and upper :of-type fixnum := (- (or end (length vector)) 1)
	 :while (<= lower upper)
	 :do (let* ((middle (ash (+ lower upper) -1))
				(present (aref vector middle))
				(key (funcall key present)))
		   (declare (type fixnum middle))
		   (cond
			 ((funcall predicate key element) (setf lower (+ middle 1)))
			 ((funcall predicate element key) (setf upper (- middle 1)))
			 (t (return (values present t)))))
	 :finally (return (values nil nil))))
