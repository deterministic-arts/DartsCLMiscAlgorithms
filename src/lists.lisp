
(in-package "DARTS.LIB.LIST-UTILITIES")


;;; All non-destructive functions defined here may return values (or pass
;;; values to callback functions) which share an arbitrary amount of structure
;;; with one of their input arguments. They will never modify their inputs.
;;;
;;; All destructive functions defined here may return values or pass values
;;; to their callbacks which share an arbitrary amount of structure with one
;;; of their input arguments. Certain destructive functions (e.g., nmap-splits) 
;;; will also modify values already handed out to their callbacks. See the
;;; function documentation for details.



(defun npermutations (list)
  "Computes all permutations of the elements in the input list. This function
may destructively modify the input list (hence the n prefix), but if it returns
normally, the original list is fully restored. If the function is left due to
either a signalled condition or some other kind of non-local exit, the state
of the input list is undefined.

This implementation is curtesy to Erik Naggum."
  (if (cdr list)
      (loop with rotation = list
          do (setq rotation (nconc (last rotation) (nbutlast rotation)))
          nconc (loop for list in (npermutations (rest rotation))
                      collect (cons (first rotation) (copy-list list)))
          until (eq rotation list))
      (list list)))


(defun drop (n list)
  "Returns the sublist of LIST, which is generated by removing the first
N elements from LIST. If LIST is shorter than N, this function returns
nil."
  (if (or (<= n 0) (null list)) 
	  list
	  (drop (- n 1) (cdr list))))


(defun take (n list)
  "Returns a list containing the first N elements of LIST. The value
returned is always a fresh list. Note, that the result may be shorter 
than N elements, if LIST itself is shorter than N."
  (declare (optimize (speed 3) (debug 0)))
  (if (or (<= n 0) (null list)) '()
	  (labels ((recur (n list head tail)
				 (if (or (<= n 0) (null list)) 
					 head
					 (let ((link (cons (car list) nil)))
					   (setf (cdr tail) link)
					   (recur (- n 1) (cdr list) head link)))))
		(let ((head (cons (car list) nil)))
		  (recur (- n 1) (cdr list) head head)))))


(defun split (n list)
  "Splits a list. This function returns two values. The primary value
being a list of at most N elements (the first N elements in LIST), and the 
secondary value being the remaining elements. Basically, this is a slightly
more efficient version of (values (take n) (drop n))."
  (declare (optimize (speed 3) (debug 0)))
  (if (or (<= n 0) (null list)) (values '() list)
	  (labels ((recur (n list head tail)
				 (if (or (<= n 0) (null list)) 
					 (values head list)
					 (let ((link (cons (car list) nil)))
					   (setf (cdr tail) link)
					   (recur (- n 1) (cdr list) head link)))))
		(let ((head (cons (car list) nil)))
		  (recur (- n 1) (cdr list) head head)))))


(defun nsplit (n list)
  "Splits a list. This function returns two values. The primary value
being a list of at most N elements (the first N elements in LIST), and the 
secondary value being the remaining elements. Basically, this is a slightly
more efficient version of split. Note, that this function may modify the 
input list."
  (cond 
	((minusp n) (error "index value must not be negative"))
	((not (consp list)) (values nil list))
	((zerop n) (values nil list))
	((= n 1) (let ((tail (cdr list)))
			   (setf (cdr list) nil)
			   (values list tail)))
	(t (loop
		  :for p := list :then c
		  :for c :on (cdr list)
		  :for k :upfrom 1 :below n
		  :finally (progn 
					 (setf (cdr p) nil)
					 (return (values list c)))))))


(defun ntake (n list)
  "Returns a list containing the first N elements of LIST. The value
returned is always a fresh list. Note, that the result may be shorter 
than N elements, if LIST itself is shorter than N. This function may
modify its input list, so use with care."
  (multiple-value-bind (head tail) (nsplit n list)
	(declare (ignore tail))
	head))


(defun ndrop (n list)
  "Returns the sublist of LIST, which is generated by removing the first
N elements from LIST. If LIST is shorter than N, this function returns
nil. This function may modify its input list so use with care."
  (multiple-value-bind (head tail) (nsplit n list)
	(declare (ignore head))
	tail))


(defun nmap-splits (function list)
  "Apply FUNCTION to each possible split of LIST. A split is defined as
a pair of lists of lists, the first being the first N elements of the original
list, and the second being the remaining length - N elements. A list of length
N has N + 1 possible ways to be split.

This function may alter the the lists passed on to FUNCTION after each invocation,
so FUNCTION must either process them directly or copy them. The function will
not modify the input list, but any list passed as argument to FUNCTION may share
structure with LIST.

Using this function instead of map-splits may be more efficient for large lists,
since it avoids the overhead of copying the 'head' list before each call to 
FUNCTION. However, due to the possible modification of the lists passed on
to FUNCTION in each invokation, this function may not be as broadly applicable
as map-splits is."
  (declare (optimize (speed 3) (debug 0))
		   (dynamic-extent function)
		   (type list list))
  (unless (null list)
	(labels ((recur (list head tail)
			   (funcall function head list)
			   (unless (null list)
				   (let ((link (cons (car list) nil)))
					 (setf (cdr tail) link)
					 (recur (cdr list) head link)))))
	  (funcall function '() list)
	  (if (null (cdr list))
		  (funcall function list '())
		  (let ((head (cons (car list) nil)))
			(recur (cdr list) head head)))))
  (values))


(defun map-splits (function list)
  "Apply FUNCTION to each possible split of LIST. A split is defined as
a pair of lists of lists, the first being the first N elements of the original
list, and the second being the remaining length - N elements. A list of length
N has N + 1 possible ways to be split.

This function is guaranteed not to modify its input element. It also guarantees,
that the lists supplied as input to FUNCTION will not be modified ever again (by
this function itself), though they may actually share structure with the input
list."
  (loop
	 :for head := nil :then (cons (car cons) head)
	 :for cons :on list
	 :do (funcall function (reverse head) cons)
	 :finally (funcall function (reverse head) nil))
  (values))


(defun map-permutations (function list)
  "Maps FUNCTION across all possible permutations of the elements in LIST. 
The input list is not modified in this process. The FUNCTION is invoked with
each permutation represented as list. The result of this function is 
undefined.

Careful: This function needs stack space proportional to the length of the
input list."
  (cond
	((null list) (values))
	((null (cdr list)) (funcall function list) (values))
	(t (map-permutations 
		 #'(lambda (permutation)
			 (nmap-splits 
			   #'(lambda (head tail)
				   (if (null head) 
					   (funcall function (cons (car list) tail))
					   (funcall function (append head (cons (car list) tail)))))
			   permutation))
		 (cdr list))
	   (values))))


(defun permutations (list)
  (let ((head nil) (tail nil))
	(map-permutations 
	  #'(lambda (permutation)
		  (if (null head)
			  (setf head (setf tail (cons permutation nil)))
			  (setf tail (setf (cdr tail) (cons permutation nil)))))
	  list)
	head))

