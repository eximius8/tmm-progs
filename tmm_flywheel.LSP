;;;=========================================
;;;Программа для графического интегрирования
;;;=========================================
;;;graf_list - список с координатами функции
;;;для интегрирования
;;;r1, r2, r3 - точки для построения линий интегрирования
;;;graf_int - точки проинтегрированного графика
;;;s - точка для проинтегрованного графика
;;;i - счетчик
(defun grafint (graf_list vniz / r1 r2 r3
		i s mas_uglov linii)
  (initget 7)
  (setq h (getdist "\nВведите H: ")
	r1 (polar (car graf_list) pi h)
  )
  (initget 7)
  (setq	s (list (car (car graf_list)) vniz)
	i 0
	graf_int (list s)
  ); setq
  (repeat (1- (LENGTH graf_list))
    (setq r2 (list (car (car graf_list))
		   (* (+ (cadr (nth i graf_list))
			 (cadr (nth (1+ i) graf_list))
		      ); +
		      0.5
		   ); *
	     ); list
	  r3 (polar r2 0 (* (+ (car (nth i graf_list))
			       (car (nth (1+ i) graf_list))
			    ); +
			    0.5
			 ); *
	     ); polar
	  s (inters (polar s (angle r1 r2) 100) s
		    (list (car (nth (1+ i) graf_list)) 0)
		    (list (car (nth (1+ i) graf_list)) 10)
		    nil
	    ); inters
	  graf_int (cons s graf_int)
	  linii (cons (list r1 r2 r3) linii)
	  i (1+ i)
    ); setq
  ); repeat
  (initget 1 "Yes No")
  (if (eq "Yes" (getkword "\nПрочертить построение линий интегрирования? [y/n]: "))
    (mapcar '(lambda (a) (li a))
	    linii
    )
  )
  (add_pl graf_int)  
); grafint

(defun deltat (listik tochka / y)
  (setq dett nil)
  (vl-every
    '(lambda (tochka)
       (setq
	 y (- (cadr tochka)
	      (+ (/ (* (- (car tochka) (car (car listik))); x-x1
		       (- (cadr (last listik)) (cadr (car listik))); y2-y1
		    ); *
		    (- (car (last listik)) (car (car listik))); x2-x1
	 	 ); /
		 (cadr (car listik))
	      ); +
	   ); -
	 dett (cons (list (car tochka) y) dett)	 
       );_ setq
     );_ lambda
    listik
  );_ mapcar
  (command "._ucs" tochka "")
  (add_pl dett)
)

(defun vitten (gde_chertit vverx vpravo / i)
  (setq i 0
	mas_vit nil
  )
  (repeat (length graf_int)
    (setq
      mas_vit (cons (list (car (nth i mas_i)) (cadr (nth i dett))) mas_vit)
      i (1+ i)
    );_ setq
  );_ repeat
  (VL-CMDF "._ucs" gde_chertit "") 
  (add_pl mas_vit)
  (setq i 0)
  (initget 1 "Yes No")
  (if (equal (getkword "\nНачертить линии построения [Y/N]: ") "Yes")    
      (repeat (length graf_int)
      (li (list (list (car (nth i mas_vit)) (+ (cadr (nth i (reverse mas_i))) vverx))
		(nth i mas_vit)
		(list (+ (car (nth i (reverse dett))) vpravo) (cadr (nth i mas_vit)))		
	  ); list
        ;mas_vit (cons (list (car (nth i mas_i)) (cadr (nth i dett))) mas_vit)
      ); li
      (setq i (1+ i))
    ); repeat
  )
)

(defun tzvezda (listik vniz mua / x y)
  (setq tzv nil)
  (vl-every '(lambda (tochka)
	       (setq x (cadr tochka)
		     y (* 0.5 omeg1 omeg1 (car tochka) (/ 1. muat))
		     tzv (cons (list x y) tzv)
	       )
	    )
	    listik
  ); vl-every
  (vl-cmdf "._ucs" (list 0 vniz) "")
  (add_pl tzv)
); defun tzvezda

(defun detzvez (listik1 listik2 vniz / x y i)
  (setq detzv nil
	i 0
  )
  (repeat (length listik2)
    (setq x (car (nth i listik2))
	  y (- (cadr (nth i listik1)) (cadr (nth i listik2)))
	  detzv (cons (list x y) detzv)
	  i (1+ i)
    )
  ); repeat
  (vl-cmdf "._ucs" (list 0 vniz) "")
  (add_pl detzv)
)

(defun momentpr (n_x ugol0 os_x plus_minus1 mpr / polo os_x  max_mom fi shag_ugl shag_os mdpr_sum tekush_ug
		  fs i polus)
  (setq mom_mas nil
	mufi (/ pi os_x 0.25)  
  	shag_ugl (/ pi n_x 0.5)
	shag_os (/ os_x n_x)
	fi ugol0
	tekush_ug 0.	
	i 1
	polus '(0 0)
  );_ setq
  (li (list '(0 50) '(0 -500)))  
  (repeat (fix (1+ n_x))    
    (setq mdpr_sum (mpr fi)
	  mom_mas (cons (list tekush_ug mdpr_sum) mom_mas)
	  tekush_ug (+ tekush_ug shag_os)
	  fi (+ fi (* plus_minus1 shag_ugl))
    ); setq
  );_ repeat
  (initget 7)
  (setq	max_mom (car (vl-sort (mapcar '(lambda (p) (abs (cadr p))) mom_mas) '>))
	mumoment (/ max_mom (getdist "\nВведите Ymax для моментов: "))
	mom_mas (mapcar '(lambda (lis)
			   	 (list (car lis) (/ (cadr lis) mumoment))
			 ) mom_mas)
  ); setq
  (add_pl mom_mas)
  (add_tex (getpoint "\nМасштаб моментов: ") mumoment "M" "м/мм")
  (add_tex (getpoint "\nМасштаб углов: ") mufi "\U+03C6" "рад/мм")
)

(defun vibor_algor (plus_minus1 ipr ugol0 n_x os_x / key_cher )
  (initget 1 "M V")
  (setq key_cher (getkword "\nЧертим по Мерцалову или Виттенбауэру [M/V]: "))
  (grafint (reverse mom_mas) -150)
  (setq muat (* h mufi mumoment))
  (add_tex (getpoint "\nМасштаб работ: ") muat "A" "Дж/мм")
  (deltat graf_int '(0. -200))
  (inercia)
)

(defun inercia1 ( / masshtab fi cv dv i)  
  (setq fi ugol0
	tekush_ug 0.
	mas_i nil
	i 1
	shag_ugl (/ pi n_x 0.5)
	shag_os (/ os_x n_x)
  )  
  (repeat (fix (1+ n_x))
    (setq ipr_sum (ipr fi)
	  mas_i (cons (list ipr_sum tekush_ug) mas_i)
	  tekush_ug (+ tekush_ug shag_os)
	  fi (+ fi (* plus_minus1 shag_ugl))
    ); setq
  ); repeat
  (initget 7)
  (setq	max_ii (car (vl-sort (mapcar '(lambda (p) (car p)) mas_i) '>))
	muii (/ max_ii (if (equal key_cher "M")
			 max_ii
			 (getdist "\nВведите Ymax для момента инерции: ")
		       )
	     )
	mas_i (mapcar '(lambda (lis)
			   	 (list (/ (car lis) muii) (* -1 (cadr lis)))
			 ) mas_i)
  ); setq
  (if (equal key_cher "V")
    (progn (command "._ucs" (list -150 400) "")
      (add_pl mas_i)
      (add_tex (getpoint "\nМасштаб приведённого момента инерции: ") muii "I" "кг*м2^/мм")
      (vitten '(0 -400) 400 150)
    )
  )
); inercia