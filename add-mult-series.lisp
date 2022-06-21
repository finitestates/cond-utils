;; additive and multiplicative series on lists
;; e.g. (additive-list '(1 2 3 4)) → 10
;; can take optional index parameter init, so that
;; e.g. (additive-list '(1 2 3 4) 10) → 20 
;; but defaults to 0 for addition & 1 for multiplication

(defun additive-list (seq &optional (init 0))
    (labels
      ((add-aux1 (seq init)
	 (add-aux2 seq init))
       (add-aux2 (seq init)
	 (if (null seq)
	     init
	     (add-aux2 (cdr seq) (+ (car seq) init)))))
      (add-aux1 seq init)))

(defun multiply-list (seq &optional (init 1))
  (labels
      ((mult-aux1 (seq init)
	 (mult-aux2 seq init))
       (mult-aux2 (seq init)
	 (if (null seq)
	     init
	     (mult-aux2 (cdr seq) (* (car seq) init)))))
    (mult-aux1 seq init)))
