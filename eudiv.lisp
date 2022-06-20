;; simple division algorithm that produces a list of quotient + remainder
;; d1: dividend
;; d2: divisor
;; a: accumulator
;; when function completes, d1 = remainder
;; (c.f. https://en.wikipedia.org/wiki/Division_algorithm#Division_by_repeated_subtraction)


(defun eudiv-aux (d1 d2 a)
  (cond
    ((< d1 d2) (list a d1))
    ((> d1 d2) (eudiv-aux (- d1 d2) d2 (1+ a)))))
    
 (defun eudiv (d1 d2 &aux (a 0))
  (eudiv-aux d1 d2 a))
  
  ;; combined w/ labels
  
  (defun eulab (d1 d2 &aux (a 0))
  (labels
      ((eudiv-aux (d1 d2 a)
	 (cond
	   ((< d1 d2) (list a d1))
	   ((> d1 d2) (eudiv-aux (- d1 d2) d2 (1+ a)))))
       (eudiv (d1 d2)
	 (eudiv-aux d1 d2 a)))
    (eudiv d1 d2)))
    
    ;; w/ some primitive type-checking because I think that remainder is supposed to be an integer
    
    (defun eulab-proper (d1 d2 &aux (a 0))
  (labels
      ((eudiv-aux (d1 d2 a)
	 (cond
	    ((null (integerp d1)) (format nil "d1 is not an integer"))
	    ((null (integerp d2)) (format nil "d2 is not an integer"))
	    ((< d1 d2) (list a d1))
	    ((> d1 d2) (eudiv-aux (- d1 d2) d2 (1+ a)))))
	 (eudiv (d1 d2)
		(eudiv-aux d1 d2 a)))
       (eudiv d1 d2)))
  
  
