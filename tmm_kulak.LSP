
(defun kas_func (massiv ugol / ugol1 first_ug cosinus i point22)
  (setq first_ug (angle (nth 0 massiv) (nth 1 massiv))
	ugol1 (min_ugol ugol)
	cosinus (cos (abs (- first_ug ugol1)))
	i 0
  ); setq
  (if (<= ugol1 (* 0.5 pi))
      (repeat (1- (length massiv))
	(if (and
	      (> (abs (cos (abs (- ugol1 (angle (nth i massiv) (nth (1+ i) massiv)))))) (abs cosinus))
	      (> (abs (cos ugol1)) (abs (cos (angle (nth i massiv) (nth (1+ i) massiv)))))
	    ); and
	    (setq cosinus (cos (abs (- ugol1 (angle (nth i massiv) (nth (1+ i) massiv)))))
		  point22 (nth i massiv)
	    ); setq
        ); if
	(setq i (1+ i))
      ); repeat
      (repeat (1- (length massiv))
	(if (and
	      (> (abs (cos (abs (- ugol1 (angle (nth i massiv) (nth (1+ i) massiv)))))) (abs cosinus))
	      (> (abs (cos ugol1)) (abs (cos (angle (nth i massiv) (nth (1+ i) massiv)))))
	    ); and
	    (setq cosinus (cos (abs (- ugol1 (angle (nth i massiv) (nth (1+ i) massiv)))))
		  point22 (nth (1+ i) massiv)
	    ); setq
        ); if
	(setq i (1+ i))
      ); repeat
  ); if
  (setq point22 point22)
)

(defun min_ugol (ugolk)
  (if (> ugolk pi)
    (setq ugolk (min_ugol (- ugolk pi)))
    (setq ugolk ugolk)
  )
)