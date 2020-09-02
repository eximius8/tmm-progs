(defun otnoshen (z1 z2 pr index1 index2)
  ;pr=t -
  ;pr=nil +
  (add_text (getpoint (strcat "\nОтношение " index1 " к " index2 ":"))
    (strcat "\\A1;U\\H0.7x;\\S^" index1 index2
		    ";\\H1.4286x;=n\\H0.7x;\\S^" index1
		    ";\\H1.4286x;/n\\H0.7x;\\S^" index2
		    ";\\H1.4286x;="(if pr "-" "")
		    "Z\\H0.7x;\\S^" index2
		    ";\\H1.4286x;/Z\\H0.7x;\\S^" index1
		    ";\\H1.4286x;="(if pr "-" "")
		    "\\H0.7x;\\S" (rtos z2 2 0)"/"(rtos z1 2 0)";"
		    "\\H1.4286x;="(if pr "-" "")(rtos (/ z2 z1) 2 2)
	    ); strcat
  ); add_text
)

(defun koleso (point z fixed vnesh)
  (if fixed
    (progn (li (list
		 (polar point (* 0.5 pi) )
	       )
	   )
    ); progn1
  )  
)