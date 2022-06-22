;; computes the dot-product of any two equally-shaped matrices 
;; represented as lists of numbers (including ones w/ sub-lists
;; to represent the different rows). 
;; returns a single list.

(defun dot-prod (list1 list2)
  (dot-prod-aux list1 list2 '() ))

(defun dot-prod-aux (list1 list2 list3)
  (cond
    ((null list1) (reverse list3))
    ((and (and (atom (car list1))
	       (atom (car list2)))
	  (= (length  list1)
	     (length  list2)))
       (push (reduce #'+ (mapcar #'* list1 list2)) list3)) 
    ((and (and (listp (car list1))
	       (listp (car list2)))
	  (= (length (car list1))
	     (length (car list2))))
     (progn
     (push (reduce #'+ (mapcar #'* (car list1) (car list2))) list3)
     (dot-prod-aux (cdr list1) (cdr list2) list3)))))
