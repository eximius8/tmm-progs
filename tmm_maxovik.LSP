(defun maxovik (/ os_mom max_mom i koltochek
lis_mpr rabota deltatkin iprived
list_toch_mom list_toch_rabota list_toch_deltat
pointmom pointrabota pointdett
		typ_max   hhh bool_nach ii point11 point12 iii summm
nufi mumom murabot maxmomkn)
  (initget 7)
  (setq lis_mpr (mpr (setq koltochek (getint "\nКоличество точек: ")));'(1 2 3 4 5 10. 54 24 -12 -5 -87 -13 -10);
	list_toch_mom nil
  )
  (initget 7)
  (setq	os_mom (getdist "\nДлина оси углов: "))
  (initget 7)
  (setq	max_mom (getdist "\nМаксимальная величина момента: ")	
	i 0
  )
  (initget 7)
  (setq	mufi (/ (* (/ pi 180.) (getreal "\nСколько углов [град]: "))
		os_mom
	     )
  )
  (initget 7)
  (setq	bool_nach (getint "\nСколько линий начертить: "))
  (initget 1)
  (setq	pointmom (getpoint "\nГрафик момента: ")
	maxmomkn (abs (car (vl-sort lis_mpr (function (lambda (x1 x2) (> (abs x1) (abs x2)))))))
	mumom (/ maxmomkn max_mom)	
	ii 1
  )
  (if bool_nach
    (repeat bool_nach
      (setq point11 (polar pointmom 0. (* ii (/ os_mom bool_nach)))
	    point12 (polar point11 (* 0.5 pi) max_mom)
	    ii (1+ ii)
      )
      (li (list point11 point12))
    )    
  )
  (repeat (length lis_mpr)
    (setq list_toch_mom (cons (list (+ (car pointmom) (* (/ os_mom (- (length lis_mpr) 1)) i))
				    (+ (cadr pointmom) (* (nth i lis_mpr) (/ max_mom maxmomkn)))
			      )
			      list_toch_mom
			)
	  i (1+ i)
    ); setq
  ); repeat
  (setq list_toch_mom (cons pointmom (reverse list_toch_mom)))
  (add_pl list_toch_mom)
  (risskor pointmom (polar pointmom 0. (+ os_mom 10.)) nil nil)
  (add_text (polar (polar pointmom 0. (+ os_mom 5.)) (* 0.5 pi) 5.) "\U+03C6")
  (risskor pointmom (polar pointmom (* 0.5 pi) (+ max_mom 10.)) nil nil)
  (add_text (polar (polar pointmom (* 0.5 pi) (+ max_mom 5.)) 0. 2.) "M")
  (muu "M" mumom "Н\U+00D7м/мм")
  (muu "\U+03C6" mufi "рад/мм")
  (initget 7)
  (setq rabota (grafint2 list_toch_mom (setq hhh (getdist "\nВведите H: ")))
	rabota (vl-remove (car rabota) rabota)
	ii 1
  )
  (li (list (car list_toch_mom) (polar (car list_toch_mom) pi hhh)))
;;;  Lines of integration
  (if bool_nach
    (repeat bool_nach
      (setq point11 (polar (car list_toch_mom) (* 0.5 pi)
			   (/ (razd lis_mpr ii (+ ii (/ (length lis_mpr) bool_nach))) mumom)
		    )
	    ii (+ (/ (length lis_mpr) bool_nach) ii)	    
      )
      (li (list (polar (car list_toch_mom) pi hhh) point11 (polar point11 0. (* os_mom (/ (* 1. ii) (- (length lis_mpr) 1.))))))
    )
  )
  (initget 1)
  (setq	pointrabota (getpoint "\nУкажите, где чертить график работы: ")
	list_toch_rabota (mapcar '(lambda (x)
				    (polar pointrabota (angle (car rabota) x) (distance (car rabota) x))
;;;				    (list (+ (car pointrabota) (car x) (* -1. (car (car rabota))))
;;;					  (+ (cadr pointrabota) (cadr x) (* -1. (car (cadr rabota))))
;;;				    )
				  ); lambda
				 rabota
			 ); mapcar
  )
  (setq murabot (* hhh mufi mumom))
  (add_pl list_toch_rabota)
  (li (list (car list_toch_rabota)  (last list_toch_rabota)))
  (risskor pointrabota (polar pointrabota 0. (+ os_mom 10.)) nil nil)
  (risskor pointrabota (polar pointrabota (* 0.5 pi) 60.) nil nil)
  (add_text (polar (polar pointrabota (* 0.5 pi) 55.) 0. 2.) "A")
  (add_text (polar (polar pointrabota 0. (+ os_mom 5.)) (* 0.5 pi) 5.) "\U+03C6")
  (muu "A" murabot "Дж/мм")
  (initget 1)
  (setq pointdett (getpoint "\nУкажите, где чертить график избыточной энергии: ")
	deltatkin (deltat2 list_toch_rabota)
	list_toch_deltat (mapcar '(lambda (x)
				    (polar pointdett (angle (car deltatkin) x) (distance (car deltatkin) x))
				  ); lambda
				  deltatkin
			 ); mapcar
	
  )
  (add_pl list_toch_deltat)
  (risskor pointdett (polar pointdett 0. (+ os_mom 10.)) nil nil)
  (risskor pointdett (polar pointdett (* 0.5 pi) 60.) nil nil)
  (add_text (polar (polar pointdett 0. (+ os_mom 5.)) (* 0.5 pi) 5.) "\U+03C6")
  (add_text (polar (polar pointdett (* 0.5 pi) 55.) 0. 2.) "\U+0394T")
  (muu "\U+0394T" murabot "Дж/мм")
  (initget 1 "Mercalov Vittenbauer Gutiar")
  (setq iprived (ipr koltochek)
	typ_max (getkword "\nПо какому методу проводить исследование Mercalov/Vittenbauer/Gutiar: ")
  ); setq
  (cond
    ((equal typ_max "Mercalov") (mercalov_))
    ((equal typ_max "Vittenbauer") (vittenba_))
    ((equal typ_max "Gutiar") (gutiar_))
  ); cond
)

(defun mercalov_ ( / pointtzv i pointtdeltm
		  list_toch_tzv list_toch_deltm
		  delta_ff1 Jmaxov
		  )
  (initget 1)
  (setq pointtzv (getpoint "\nКинетическая энергия без маховика: ")
	i 0
  ); setq
  (repeat (length iprived)
    (setq list_toch_tzv (cons (list (+ (car pointtzv) (* (/ os_mom (- (length iprived) 1.)) i))
				    (+ (cadr pointtzv) (* 0.5 (nth i iprived) omeg1 omeg1 (/ 1. murabot)))
			      )
			      list_toch_tzv
			)
	  i (1+ i)
    )
  ); repeat
  (add_pl list_toch_tzv)
  (risskor pointtzv (polar pointtzv 0. (+ os_mom 10.)) nil nil)
  (risskor pointtzv (polar pointtzv (* 0.5 pi) 60.) nil nil)
  (add_text (polar (polar pointtzv 0. (+ os_mom 5.)) (* 0.5 pi) 5.) "\U+03C6")
  (add_text (polar (polar pointtzv  (* 0.5 pi) 55.) 0. 2.) "\\A1;T\\H0.7x;\\S*^;\\H1.4286x;")
  (muu "T*" murabot "Дж/мм")
  (initget 1)
  (setq i 0
	list_toch_tzv (reverse list_toch_tzv)
	pointtdeltm (getpoint "\nКинетическая энергия без маховика: ")
  )
  (repeat (length iprived)
    (setq list_toch_deltm (cons (list (+ (car pointtdeltm) (* (/ os_mom (- (length iprived) 1.)) i))
				      (+ (cadr pointtdeltm) (- (- (cadr (nth i list_toch_deltat)) (cadr pointdett))
							       (- (cadr (nth i list_toch_tzv)) (cadr pointtzv))
							    ); -
				      ); +
			        ); list
				list_toch_deltm
			  )
	  i (1+ i)
    )
  ); repeat
  (setq delta_ff1 (abs (- (cadr (car (vl-sort list_toch_deltm (function (lambda (x1 x2) (> (cadr x1) (cadr x2)))))))
			  (cadr (car (vl-sort list_toch_deltm (function (lambda (x1 x2) (< (cadr x1) (cadr x2)))))))
		       )
		  ); abs
	Jmaxov (/ (* delta_ff1 murabot) (* omeg1 omeg1 k_nerav))
  )
  (add_pl list_toch_deltm)
  (risskor pointtdeltm (polar pointtdeltm 0. (+ os_mom 10.)) nil nil)
  (risskor pointtdeltm (polar pointtdeltm (* 0.5 pi) 60.) nil nil)
  (add_text (polar (polar pointtdeltm 0. (+ os_mom 5.)) (* 0.5 pi) 5.) "\U+03C6")
  (add_text (polar (polar pointtdeltm  (* 0.5 pi) 55.) 0. 2.) "\\A1;T\\H0.7x;\\S^M;\\H1.4286x;")
  (add_text (getpoint "\nМомент инерции: ") (strcat "\\A1;J\\H0.7x;\\S^M;\\H1.4286x;="
						    "|FF\\H0.7x;\\S^1;\\H1.4286x;|\U+00D7\U+03BC\\H0.7x;"
						    "\\S^A;\\H1.4286x;/(\U+03C9\\H0.7x;\\S2^CP;\\H1.4286x;\U+00D7\U+03B4)="
						    (rtos delta_ff1 2 1)"\U+00D7"(rtos murabot 2 2)"/("
						    (rtos omeg1 2 2)"\\H0.7x;\\S2^;\\H1.4286x;\U+00D7"
						    (rtos k_nerav)")="(rtos Jmaxov 2 3)
						    " кг\U+00D7м\\H0.7x;\\S2^;\\H1.4286x;"
					    )
  )
); defun

(defun vittenba_ (/ i pointipr muipr pointvitt
		  max_ipr_mm max_ipr_kg max_deltat_mm
		  list_toch_ipr list_vitten_toch)
  (initget 7)
  (setq max_ipr_mm (getdist "\nМаксимальная величина момента инерции на графике: "))
  (initget 1)
  (setq	pointipr (getpoint "\nТочка вставки приведённого момента инерции: ")
	max_ipr_kg (abs (car (vl-sort iprived (function (lambda (x1 x2) (> (abs x1) (abs x2)))))))
	muipr (/ max_ipr_kg max_ipr_mm)
	i 0
  )
  (repeat (length lis_mpr)
    (setq list_toch_ipr (cons (list (+ (car pointipr) (/ (nth i iprived) muipr))
				    (- (cadr pointipr) (* (/ os_mom (- (length lis_mpr) 1)) i))
			      )
			      list_toch_ipr
			)
	  i (1+ i)
    ); setq
  ); repeat
  (setq list_toch_ipr (reverse list_toch_ipr))
  (add_pl list_toch_ipr)
  (risskor pointipr (polar pointipr 0. (+ max_ipr_mm 10.)) nil nil)
  (risskor pointipr (polar pointipr (* 1.5 pi) (+ os_mom 10.)) nil nil)
  (add_text (polar (polar pointipr (* pi 1.5) (+ os_mom 5.)) 0. 2.) "\U+03C6")
  (add_text (polar (polar pointipr 0. (+ max_ipr_mm 5.)) (* 0.5 pi) 5.) "\\A1;I, кг\U+00D7м\\H0,7x;\\S2^;\\H1.4286x;")
  (muu "I" muipr "кг\U+00D7м2/мм")
  
  (setq pointvitt (getpoint "\nДиаграмма Виттенбауэра: ")
	i 0
	max_deltat_mm (- (car (car (vl-sort list_toch_deltat (function (lambda (x1 x2) (> (abs (cadr x1)) (abs (cadr x2))))))))
			 (cadr pointdett)
		      )
  )
  (repeat (length list_toch_ipr)
    (setq list_vitten_toch (cons (list (+ (car pointvitt) (* -1. (car pointipr)) (car (nth i list_toch_ipr)))
				       (+ (cadr pointvitt) (* -1. (cadr pointdett)) (cadr (nth i list_toch_deltat)))
				 ); list
			         list_vitten_toch
			   ); cons
	  i (1+ i)
    ); setq
  ); repeat
  (add_pl list_vitten_toch)
  (risskor pointvitt (polar pointvitt 0. (+ max_ipr_mm 10.)) nil nil)
  (add_text (polar (polar pointvitt 0. (+ max_ipr_mm 5.)) (* 0.5 pi) 5.) "I")
  ;(risskor pointvitt (polar pointvitt (* 0.5 pi) (+ max_deltat_mm 10.)) "delT" nil)
)


(defun razd (lis num1 num2 / i lis2)
  (setq i 0)
  (repeat (1+ (- num2 num1))
    (setq lis2 (cons (nth (+ num1 i) lis) lis2)
	  i (1+ i)
    )
  )
  (/ (apply '+ lis2) (length lis2))
)

(defun grafint2 (graf_list h / r1 r2 r3
		i s mas_uglov linii graf_int)
  (setq r1 (polar (car graf_list) pi h)
	i 0
	s (car graf_list)
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
  (setq graf_int (reverse graf_int))
); grafint

(defun deltat2 (listik / y dett)
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
  (setq dett (reverse dett))
)