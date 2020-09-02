(defun convert-26 (x1 / x das_1 das_2)
  (setq x (fix x1))
  (if (= (/ x 26) 0) (chr (+ 65 x))
    (if (and (> (/ x 26) 0) (<= (/ x 26) 26)) 
        (strcat (chr (+ 65 (1- (/ x 26)))) 
                (chr (+ 65 (- x (* 26 (/ x 26)))))
	);_ strcat
        (if (and (> (/ (1- (/ x 26)) 26) 0) (<= x 16383)) 
            (progn (setq das_1 (1- (/ (1- (/ x 26)) 26))
                         das_2 (+ 26 (- x (* (1+ (* (1+ das_1) 26)) 26)))
		   );_ setq
                   (strcat (chr (+ 65 das_1)) 
                           (chr (+ 65 (1- (/ das_2 26))))
                           (chr (+ 65 (- das_2 (* 26 (/ das_2 26)))))
		   );_ strcat
	    );_ progn
        );_ if
    );_ if
  );_ if
);_ defun

(defun ex_get (ro col / addres a)
    (if (numberp col)
      (setq addres (strcat
		   (convert-26 col)
		   (itoa ro)		   
		 ); strcat
      ); setq
      (setq addres (strcat		   
		     col
		     (itoa ro)
		   ); strcat
      ); setq
    )
    (setq a (vlax-get-property
	      (vlax-get-property
	        g_sheet
	        "Range" addres
	      ); vlax-get-property
	      "Value"
	    ); vlax-get-property
    ); setq
    (vlax-variant-value a)
  ); defun


(defun ex_put (znachen ro col put_sheet / celk addres)
  (if (numberp col)
    (setq addres (strcat
		   (convert-26 col)
		   (itoa ro)		   
		 );_ strcat
    );_ setq
    (setq addres (strcat		   
		   col
		   (itoa ro)
		 ); strcat
    ); setq
  )
  (vlax-put-property (vlax-get-property put_sheet
	       			        "Range" addres
		     ); vlax-get-property
    		     "value2" znachen
  ); vlax-put-property
  (gc)
); defun