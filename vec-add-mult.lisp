;; adds or multiplies all the elements in a vector

(defun vector-add (vec)
  (let
      ((size (array-dimension vec 0)))
    (labels
	((v-aux (i)
	   (if (= i size)
	       0
	       (+ (aref vec i) (v-aux (1+ i))))))
      (v-aux 0))))

(defun vector-multiply (vec)
  (let
      ((size (array-dimension vec 0)))
    (labels
	((v-aux (i)
	   (if (= i size)
	       1
	       (* (aref vec i) (v-aux (1+ i))))))
      (v-aux 0))))
