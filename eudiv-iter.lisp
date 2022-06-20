;; the same simple algorithm as eudiv.lisp, but executed w/
;; "primitive" iterative constructs

(defun eu-iter (x1 y1 &aux (z1 0))
  (do
   ((x x1 (- x y))
    (y y1)
    (z z1 (1+ z)))
   ((< x y) (list z x))))

(defun eu-loop (x y &aux (z 0))
  (loop
    (setq x (- x y))
    (setq z (1+ z))
    (when (< x y) (return (list z x)))))

(defun eu-prog (x1 y1)
  (prog ((x x1) (y y1) (z 0))
   start
     (setq x (- x y))
     (incf z)
     (when (> x y) (go start))
     (when (< x y) (go end))
   end
     (return (list z x))))
