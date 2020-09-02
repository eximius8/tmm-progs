(defun tablica_va (mas_b mas_v mas_a mas_om mas_ep / i gde)
  (vl-load-com)
  (vla-addtable (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
    (vlax-3d-point (trans (getpoint "\nГде чертить таблицу линейных скоростей и ускорений") 1 0))
    (1+ (LENGth mas_b)) 3 10. 12
  ); vla-addtable
  (vla-UnmergeCells (vlax-ename->vla-object (entlast))
    0 0 1 2
  ); vla-UnmergeCells
  (mapcar '(lambda (x)
	     (vla-settextheight (vlax-ename->vla-object (entlast))
   		x 2.5
             ); vla-settextheight
	     (vla-settextstyle (vlax-ename->vla-object (entlast))
    		x "Таймз"
  	     ); vla-settextstyle
	     (vla-setalignment (vlax-ename->vla-object (entlast))
   		x acMiddleCenter
  	     ); vla-setalignment
	   ); lambda
	   (list actitlerow acheaderrow acdatarow)
  ); mapcar
  (setq i -1)
  (repeat 3
    (setq i (1+ i))
    (vla-settext (vlax-ename->vla-object (entlast))
     0 i (nth i (list "Точки" "\U+03C5\nм/с" "\\A1;а\nм/с\\H0.7x;\\S2^;"))
    ); vla-settext
  ); repeat
  (setq i -1)
  (repeat (LENGth mas_b)
    (setq i (1+ i))
    (vla-settext (vlax-ename->vla-object (entlast))
      (1+ i) 0 (nth i mas_b)
    ); vla-settext
    (vla-settext (vlax-ename->vla-object (entlast))
      (1+ i) 1 (rtos (nth i mas_v) 2 2)
    ); vla-settext
    (vla-settext (vlax-ename->vla-object (entlast))
      (1+ i) 2 (rtos (nth i mas_a) 2 2)
    ); vla-settext
  ); repeat
  (vla-addtable (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
    (vlax-3d-point (trans (getpoint "\nГде чертить таблицу угловых скоростей и ускорений") 1 0))
    (1+ (LENGth mas_ep)) 3 10. 12
  ); vla-addtable
  (vla-UnmergeCells (vlax-ename->vla-object (entlast))
    0 0 1 2
  ); vla-UnmergeCells
  (mapcar '(lambda (x)
	     (vla-settextheight (vlax-ename->vla-object (entlast))
   		x 2.5
             ); vla-settextheight
	     (vla-settextstyle (vlax-ename->vla-object (entlast))
    		x "Таймз"
  	     ); vla-settextstyle
	     (vla-setalignment (vlax-ename->vla-object (entlast))
   		x acMiddleCenter
  	     ); vla-setalignment
	   ); lambda
	   (list actitlerow acheaderrow acdatarow)
  ); mapcar
  (setq i -1)
  (repeat 3
    (setq i (1+ i))
    (vla-settext (vlax-ename->vla-object (entlast))
     0 i (nth i (list "Звено" "\\A1;\U+03C9,\nс\\H0.7x;\\S-1^;" "\\A1;\U+03B5,\nс\\H0.7x;\\S-2^;"))
    ); vla-settext
  ); repeat
  (setq i -1)
  (repeat (LENGth mas_ep)
    (setq i (1+ i))
    (vla-settext (vlax-ename->vla-object (entlast))
      (1+ i) 0 (itoa (+ i 2))
    ); vla-settext
    (vla-settext (vlax-ename->vla-object (entlast))
      (1+ i) 1 (rtos (nth i mas_om) 2 1)
    ); vla-settext
    (vla-settext (vlax-ename->vla-object (entlast))
      (1+ i) 2 (rtos (nth i mas_ep) 2 1)
    ); vla-settext
  ); repeat
)

;Пишет формулу вида xa=muv(a)*Pa, где dlina=pa
;;;bool = t индексы для скоростей = nil индексы для ускорений
(defun formsk2 (muuu dlina simv simv2 bool)
;;;  simv = нижний индекс у скорости (или ускорения)
;;;  simv2 = символ отрезка
  (add_text (getpoint (strcat "\nУкажите где писать " (if bool "v" "a") simv))
	    (if bool
	      (strcat "\\A1;\U+03C5\\H0.7x;\\S^" simv
		      ";\\H1.4286x;=\U+03BC\\H0.7x;\\S^\U+03C5;\\H1.4286x;\U+00D7" simv2
		      "="(rtos muuu)"\U+00D7"(rtos dlina 2 0)"="(rtos (* muuu dlina))" м/с"
	      ); v
	      (strcat "\\A1;a\\H0.7x;\\S^" simv
		      ";\\H1.4286x;=\U+03BC\\H0.7x;\\S^a;\\H1.4286x;\U+00D7" simv2
		      "="(rtos muuu)"\U+00D7"(rtos dlina 2 0)"="(rtos (* muuu dlina))
		      " м/с\\H0.7x;\\S2^;\\H1.4286x;"
	      ); ф
	    ); if
  ); add_text
)

(defun formsk (dlina simv ch);Пишет формулу вида xa=muv(a)*Pa, где dlina=pa
  (add_text (getpoint (strcat "\nУкажите где писать " simv));Ch="a" =>mua
 	   (strcat "\\A1;"
		   (if (equal ch "a")
		     "a"
		     "\U+03C5"
		   )
		   "\\H0.7x;"
		   "\\S^"(substr simv 2)";=\\H1.4286x;"
                   (if (equal ch "a")
		       (rtos mua)
		       (rtos muv)
		   )
		   "\U+00D7"
		   (rtos dlina 2 0)"="
		   (rtos (* (if (equal ch "a")
			        mua
			        muv
			    )
			    (atof (rtos dlina 2 0))
			 )
			 2 3
		   ); rtos
		   (if (equal ch "a")
		       " м/c\\H0.7x;\\S2^;\\H1.4286x;"
		       " м/c"
		   )
	   );end of strcat
  ); add_tex
);End of formsk

(defun formusk (simv v l );Пишет формулу для нормальных ускорений
  (add_text (getpoint (strcat "\nУкажите где писать " simv"n"))
	   (strcat "\\A1;a\\H0.7x;\\Sn^"(substr simv 2)
		   ";\\H1.4286x;=\U+03C5\\H0.7x;\\S2^"(substr simv 2)
		   ";\\H1.4286x;/l\\H0.7x;\\S^"(substr simv 2)
		   ";\\H1.4286x;="(rtos v 2 3)"\\H0.7x;\\S2^;\\H1.4286x;/"
		   (rtos l 2 3)"="
		   (rtos (/ (expt
			      (atof (rtos v 2 3))
			      2
			    ); expt
			    l
			 ); /
			 2 3
		   ); rtos
		   "м/с\\H0.7x;\\S2^;\\H1.4286x; ("
		   (rtos (/ (/ (expt (atof (rtos v 2 3)) 2)
			       l
			    ); /
			    mua
			 ); /
			 2 1
		   ); rtos
		   " мм на плане)"
	   ); strcat
 ); add_tex
); defun formusk

(defun formuskt (simv dlina modul /)
  (add_text (getpoint (strcat "\nУкажите где писать " simv))
	    (strcat "\\A1;\a\\H0.6125x;\\o\\S\U+03C4^"
		    (substr simv 2)";\\H1.4286x;="
		    (rtos dlina 2 0)"\U+00D7"(rtos mua)
		    "="(rtos modul)"м/с\\H0.7x;\\S2^;"
	   );end of strcat
  );end of add_tex
)

(defun formuskt2 (simv dlina mua /)
  (add_text (getpoint (strcat "\nУкажите где писать " simv))
	    (strcat "\\A1;a\\H0.7x;\\S\U+03C4^" simv
		    ";\\H1.4286x;=" (rtos dlina 2 0) "\U+00D7"
		    (rtos mua) "=" (rtos (* dlina mua) 2 3)
		    " м/с\\H0.7x;\\S2^;\\H1.4286x;"
	   );end of strcat
  );end of add_tex
)

(defun ugsu (simv chislit znamenat / le gde);Пишет формулы для омег и эпсилонов
  (add_text (getpoint (strcat "\nУкажите где написать чему равно " simv))
	   (strcat "\\A1;\\H2.2;"
		   (if (equal (substr simv 1 1) "o")
		     (strcat "\U+03C9"
			     "\\H0.7x;\\S^"(substr simv 4 1)";"
			     "\\H2.2;=\U+03C5"
			     "\\H1.5;\\S^"(substr simv 5 2)";"
			     "\\H2.2;/l"
			     "\\H0.7x;\\S^"(substr simv 5 2)";"
			     "\\H2.2;=\\H0.8x;"
			     "\\S"(rtos chislit 2 3)"/"(rtos znamenat 2 3)";"
			     "\\H2.2;="
			     (rtos (eval (read (substr simv 1 4))) 2 3);Значение омеги
			     " c\\H1.3;\\S-1^;"
		     );end of strcat
		     (strcat "\U+03B5"
			     "\\H0.7x;\\S^"(substr simv 4 1)";"
			     "\\H2.2;=a"
			     "\\H0.7x;\\S\U+03C4^"(substr simv 5 2)";"
			     "\\H2.2;/l"
			     "\\H0.7x;\\S^"(substr simv 5 2)";"
			     "\\H2.2;=\\H0.8x;"
			     "\\S"(rtos chislit 2 3)"/"(rtos znamenat 2 3)";"
			     "\\H2.2;="
			     (rtos (eval (read (substr simv 1 4))) 2 3);Значение эпсилон
			     " c\\H1.3;\\S-2^;"
		     );end of strcat
		   );end of if
	   );end of strcat
  );end of add_tex
);end of ugsu

(defun koriolis (omega vr ch omnomer)
  (add_text (getpoint (strcat "\nУкажите где писать " ch " Кориолиса: "))
	   (strcat "\\A1;a\\H0.7x;\\Sk^"
		   ch ";\\H1.4286x;=2\U+00D7"
		   "\U+03C9\\H0.7x;\\S^" omnomer
		   ";\\H1.4286x;\U+00D7" "\U+03C5\\H0.7x;\\S^" ch
		   ";\\H1.4286x;=2\U+00D7" (rtos omega 2 2) "\U+00D7"
		   (rtos vr 2 3) "=" (rtos (* 2 omega vr) 2 3)		   
		   " м/с\\H0.7x;\\S2^;\\H1.4286x; ("
		   (rtos (/ (* 2. omega vr)
			    mua
			 ); /
			 2 1
		   ); rtos
		   " мм на плане)"
	   ); strcat
  ); add_tex
)

(defun tabl_razmer (list_names list_mm mul / main_tabl i j mas0 mas1 mas2)
  (vl-load-com)
  (setq main_tabl (vla-addtable (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
		    (vlax-3d-point (trans (getpoint "\nГде чертить таблицу размеров звеньев: ") 1 0))
		    3 (1+ (length list_mm)) 10. 11.5
		  ); vla-addtable
;;;	list_names (mapcar '(lambda (x)
;;;			      (strcat "\\A1;" (substr x 1 1)
;;;				      "\\H0.7x;\\S^" (substr x 2)
;;;				      ";\\H1.4286x;"
;;;			      )
;;;			    )
;;;			   list_names
;;;		   )
  )
  (vla-UnmergeCells main_tabl
    0 0 1 9
  ); vla-UnmergeCells
  (mapcar '(lambda (x)
	     (vla-settextheight main_tabl
   		x 2.5
             ); vla-settextheight
	     (vla-settextstyle main_tabl
    		x "Таймз"
  	     ); vla-settextstyle
	     (vla-setalignment main_tabl
   		x acMiddleCenter
  	     ); vla-setalignment
	   ); lambda
	   (list actitlerow acheaderrow acdatarow)
  ); mapcar
  (setq i 0
	j 0
	mas0 (cons "Размеры" list_names)
	mas2 (cons "мм" (mapcar '(lambda (x)
				   (rtos x 2 1)
				 )
				 list_mm
			)
	     )
	mas1 (cons "м" (mapcar '(lambda (x)
				   (rtos (* x mul) 2 3)
				 )
				 list_mm
			)
	     )
  )
  (repeat 3
    (repeat (length mas0)
      (vla-settext main_tabl
        j i (nth i (eval (read (strcat "mas" (itoa j)))))
      ); vla-settext
      (setq i (1+ i))
    ); repeat
    (setq j (1+ j)
	  i 0
    )
  ); repeat
)

(defun kor_cher (uskor1 uskor2 skor1 skor2 v_ind a_ind om_ind / a0 a1 a2)
  (setq a0 (getpoint "\nНапрвление кориолисова ускорения: ")
	a1 (polar a0 (angle skor1 skor2) 15.)
	a2 (polar a0 (angle uskor1 uskor2) 15.)	
  )
  (risskor a0 a1 nil v_ind)(risskor a0 a2 nil a_ind)
  (duga a0 a1 a1 a2 0.5 om_ind)
)

(defun muul (ldist_ dist_)
  (add_text (getpoint "\nУкажите где писать масштаб для плана механизма: ") 
	   (strcat "\\A1;\U+03BC\\H0.7x;\\S^l;\\H1.4286x;="
		   "l\\H0.7x;\\S^AB;\\H1.4286x;/AB="
		   "\\H0.7x;\\S"(rtos ldist_)"/"(rtos dist_)
		   ";\\H1.4286x;="
		   (rtos (/ ldist_ dist_))" \\H0.7x;\\Sм/мм;\\H1.4286x;"
	   );end of strcat
  ); add_tex
)

(defun muuv (skor pskor)
  (add_text (getpoint "\nУкажите где писать масштаб для плана скоростей: ")
	   (strcat "\\A1;\\H2.2;\U+03BC"
		   "\\H0.7x;\\S^\U+03C5;"
		   "\\H2.2;=\U+03C5"
		   "\\H0.7x;\\S^B;"
		   "\\H2.2;/pb="
		   "\\H0.75x;\\S"
		   (rtos skor)"/"(rtos pskor)
		   ";\\H2.2;="
		   (rtos (/ skor pskor))
		   " \\H0.7x;\\Sм с/мм;\\H1.4286x;"
	   );strcat
  ); add_tex
)

(defun muua (uskor puskor)
  (add_text (getpoint "\nУкажите где писать масштаб для плана ускорений: ")  
	   (strcat "\\A1;\\H2.2;\U+03BC"
		   "\\H0.7x;\\S^a;"
		   "\\H2.2;=a\\H0.7x;\\S^B;"
		   "\\H2.2;/p\\H0.7x;\\S^1;\\H1.4286x;b="
		   "\\H0.8x;\\S"
		   (rtos uskor)"/"(rtos puskor)";"
		   "\\H2.2;="
		   (rtos (/ uskor puskor))
		   " \\H0.7x;\\Sм с/мм;\\H1.4286x;"
	   );strcat
  ); add_tex
)

(defun ishodn (omeg lab f)
  (add_text (getpoint "\nУкажите где писать исходные данные: ")
	   (strcat "\\A1;\U+03C9\\H0.7x;\\S^1;\\H1.4286x;="
		   (rtos omeg)" с\\H0.7x;\\S-1^;\n\\H1.4286x;"
		   "l\\H0.7x;\\S^AB;\\H1.4286x;="
		   (rtos lab)" м\n\U+03C6\\H0.7x;\\S^1;\\H1.4286x;="
		   (rtos f)"\%%D"
	   ); strcat
  ); add_tex
)

(defun formv (omeg lab)
  (add_text (getpoint "\nУкажите где писать формулу для определения скорости: ")
	    (strcat "\\A1;\\H2;\U+03C5"
		    "\\H0.7x;\\S^B;"
		    "\\H2;=\U+03C9"
		    "\\H0.7x;\\S^1;"
		    "\\H2;\U+00D7l"
		    "\\H0.7x;\\S^AB;"
		    "\\H2;="
		    (rtos omeg)"\U+00D7"(rtos lab)
		    "="(rtos (* omeg lab))" м/с"				    
	    );end of strcat
  ); add_tex
)

(defun forma (omeg lab)
  (add_text (getpoint "\nУкажите где писать формулу для определения ускорения: ")
	   (strcat "\\A1;\\H2;a"
		   "\\H0.7x;\\S^B;"
		   "\\H2;=\U+03C9"
		   "\\H0.7x;\\S2^1;"
		   "\\H2;\U+00D7l"
		   "\\H0.7x;\\S^AB;"
		   "\\H2;="			    
		   (rtos omeg)
		   "\\H0.7x;\\S2^;"
		   "\\H2;\U+00D7"(rtos lab)
		   "="(rtos (* omeg omeg lab) )" м/с"
		   "\\H0.7x;\\S2^;"
	   );end of strcat
  ); add_tex
)


(defun redopora (point pos dlin shir rasches / a b c)
;;;  point - point of inserting
;;;  pos: -1 - insert in the beginning of opor
;;;        0 - insert in the middle of opor
;;;        1 - insert in the end of opor
;;;  dlin - dlina opor
;;;  shir - width of opor
;;;  rasches - t or nil put rasches or not
  (if (= pos -1)
    (setq a point)
    (if (= pos 0)
      (setq a (list (- (car point) (* 0.5 dlin)) (cadr point)))
      (setq a (list (- (car point) dlin) (cadr point)))
    )
  )
  (li (list (setq b (polar a (* 0.5 pi) (* 0.5 shir))) (polar b 0. dlin)))
  (li (list (setq b (polar a (* 1.5 pi) (* 0.5 shir))) (polar b 0. dlin)))
  (if rasches
    (progn
      (setq c 2.13)
      (while (<= c dlin)
	(li (list (list (+ (car a) c) (- (cadr a) (* 0.5 shir))) (polar (list (+ (car a) c) (- (cadr a) (* 0.5 shir))) (* 1.25 pi) 3.)))
	(setq c (+ c 2.3))
      )
    )
  )
  (princ)
)

(defun zubch (point pos dlin shir rasches l / up niz)
;;;  point - point of inserting
;;;  pos: -1 - insert in the beginning of opor
;;;        0 - insert in the middle of opor
;;;        1 - insert in the end of opor
;;;  dlin - dlina opor
;;;  shir - width of opor
;;;  rasches - t or nil put rasches or not
;;;  l - length of wheel
  (setq up (polar point (* pi 0.5) (* l 0.5))
  	niz (polar point (* pi 1.5) (* l 0.5))
  )
  (if shir
    (progn
      (redopora point pos dlin shir rasches)
      (li (list (polar point (* pi 0.5) (* shir 0.5)) up))
      (li (list (polar point (* pi 1.5) (* shir 0.5)) niz))
    )
    (li (list (polar point (* pi 1.5) (* l 0.5)) (polar point (* pi 0.5) (* l 0.5))))
  )
  (li (list (polar niz 0. 2.7) (polar niz pi 2.7)))
  (li (list (polar up 0. 2.7) (polar up pi 2.7)))
)

(defun zubchvn (point pos dlin shir rasches l rad vprvl / up niz up2 niz2)
;;;  point - point of inserting
;;;  pos: -1 - insert in the beginning of opor
;;;        0 - insert in the middle of opor
;;;        1 - insert in the end of opor
;;;  dlin - dlina opor
;;;  shir - width of opor
;;;  rasches - t or nil put rasches or not
;;;  l - length of wheel
;;;  rad - radius inner wheel
;;;  vprvl - t - turn right, nil turn left
  (setq up (polar point (* pi 0.5) (* l 0.5))
  	niz (polar point (* pi 1.5) (* l 0.5))
  )
  (if shir
    (progn
      (redopora point pos dlin shir rasches)
      (li (list (polar point (* pi 0.5) (* shir 0.5)) up))
      (li (list (polar point (* pi 1.5) (* shir 0.5)) niz))
    )
    (li (list (polar point (* pi 1.5) (* l 0.5)) (polar point (* pi 0.5) (* l 0.5))))
  )
  (if vprvl
    (progn
      (add_arc (polar up 0 rad) (setq up2 (polar up 0 (* 2. rad))) up)
      (add_arc (polar niz 0 rad) niz (setq niz2 (polar niz 0 (* 2. rad))))
      (li (list (polar niz2 0. 2.7) (polar niz2 pi 2.7)))
      (li (list (polar up2 0. 2.7) (polar up2 pi 2.7)))
    )
    (progn
      (add_arc (polar up pi rad) up (setq up2 (polar up pi (* 2. rad))))
      (add_arc (polar niz pi rad) (setq niz2 (polar niz pi (* 2. rad))) niz)
      (li (list (polar niz2 0. 2.7) (polar niz2 pi 2.7)))
      (li (list (polar up2 0. 2.7) (polar up2 pi 2.7)))
    )
  )
)

(defun chastota (insert_p dlina angle_l name_list /
polus_plus hor1 hor2 points i muomeg mun)
  (setq	polus_plus (polar insert_p (* 0.5 pi) dlina)
	hor1 (polar polus_plus pi 150)
	hor2 (polar polus_plus 0. 150)
	points (mapcar
		 '(lambda (x)
		    (inters insert_p (polar insert_p x 100.) hor1 hor2 nil)
		  )
		 angle_l
	       )
	i 0
	muomeg (/ 10. (distance (car points) polus_plus))
	;mun (/ (* 10. ) (distance (car points) polus_plus))
  )
  (mapcar '(lambda (x) (li (list insert_p x))) points)
  (li (list (list (- (apply 'min (mapcar '(lambda (x) (car x)) points)) 7) (cadr (car points)))
	    (list (+ (apply 'max (mapcar '(lambda (x) (car x)) points)) 7) (cadr (car points)))
      )
  )  
  (repeat (length points)
    (add_text (polar (nth i points) (* 109. (/ pi 180.)) 4.1) (nth i name_list))
    (setq i (1+ i))
  )
  (muu "\U+03C9" muomeg "c/мм")
  (princ "\n")
  (add_text (getpoint (strcat "U" (car name_list) (last name_list) "="))
	    (strcat "\\A1;U\\H0.7x;\\S^" (car name_list) (last name_list) ";\\H1.4286x;="
		    "n\\H0.7x;\\S^"(car name_list)
		    ";\\H1.4286x;/n\\H0.7x;\\S^" (last name_list)
		    ";\\H1.4286x;=\\H0.7x;\\S0" (car name_list) "/0"(last name_list)
		    ";\\H1.4286x;="
		    "\\H0.7x;\\S"(rtos (distance polus_plus (car points)) 2 0)"/"
		    (rtos (distance polus_plus (last points)) 2 0)";\\H1.4286x;="
		    (rtos (/ (distance polus_plus (car points))
			     (distance polus_plus (last points))
			  ) 2 3)
				      
		    )
	    )
)