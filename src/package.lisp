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

(defpackage "DARTS.LIB.ALGORITHMS"
  (:use "COMMON-LISP" "DARTS.LIB.QUEUES")
  (:export 
     "COUNT-SUBSET-COMBINATIONS"
	 "COMPUTE-LEXICOGRAPHIC-SUBSET"
	 "COMPUTE-DISTANCES-IN-GRAPH"
	 "FIND-SHORTEST-PATH-IN-GRAPH"
   ))

(defpackage "DARTS.LIB.LIST-UTILITIES"
  (:use "COMMON-LISP")
  (:export "NPERMUTATIONS" "MAP-PERMUTATIONS" "PERMUTATIONS"
		   "DROP" "TAKE" "SPLIT" "MAP-SPLITS" "NDROP" "NTAKE" "NSPLIT" 
		   "NMAP-SPLITS"))

(defpackage "DARTS.LIB.ARRAY-UTILITIES"
  (:use "COMMON-LISP")
  (:export "BINARY-SEARCH"))