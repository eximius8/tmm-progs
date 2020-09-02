(defun mak_mirr (obj p1 p2 /)
  (vla-mirror (vlax-ename->vla-object obj)
	      (vlax-3d-point (trans p1 1 0))
    	      (vlax-3d-point (trans p2 1 0))
  ); vla-mirror
); defun
