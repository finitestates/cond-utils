;;; the first 3 and the last 3 produce the same result.
;;; (if-even '(a b c d e f)) → (A C E)
;;; (if-odd '(a b c d e f)) → (B D F)


(defun if-even (list)
  (if (null list) nil
      (cons (car list) (if-even (cddr list)))))

(defun evens-list (list)
  (cond
    ((null list) nil)
    ((atom list) list)
    (t (cons (car list)
	     (evens-list (cddr list))))))

(defun evensl (list)
  (labels
      ((aux0 (list)
	 (cond
	   ((null list) nil)
	   ((atom list) list)
	   (t (aux1 list))))
       (aux1 (list)
	     (cons (car list)
		   (aux0 (cddr list)))))
    (aux0 list)))
    
 (defun if-odd (list)
  (if (null list) nil
      (cons (cadr list) (if-odd (cddr list))))) 
    
  (defun odds-list (list)
  (cond
    ((null list) nil)
    ((atom list) list)
    (t (cons (cadr list)
	     (odds-list (cddr list))))))

(defun oddsl (list)
    (labels
      ((aux0 (list)
	 (cond
	   ((null list) nil)
	   ((atom list) list)
	   (t (aux1 list))))
       (aux1 (list)
	     (cons (cadr list)
		   (aux0 (cddr list)))))
      (aux0 list)))
