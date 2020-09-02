; Written by Trunov Mikhail mikhail.trunov@gmail.com
; GPL лицензия - при распространении обязательно указание автора кода.

;Кинематический анализ семестровая 11
(defun vvod (/ f)
 ; (initget 6)
  (setq f (getreal "\nВведите угол фи")
        ab (getdist "\nВведите AB [mm] ")
        omeg (getreal "\nУкажите угловую скорость в [с^-1]")
        lab (getreal "\nВведите AB [m] ")
        mul (/ lab ab)
        lbc (* 3.33 lab) 
        bc (/ lbc mul)
	lef lbc
	ef   bc
	ly  lbc
	y    bc	
        lcd (* 2.23 lab) 
        cd (/ lcd mul)
	led lcd
	ed cd
	lbs (* 2. lab)
	bs (/ lbs mul)
	lx (* 2.33 lab)
	x (/ lx mul)
        fi (* (- 180. f) (/ pi 180.))	
	mvb (* omeg lab)
	mab (* omeg omeg lab)
  );end of setq
  (ishodn omeg lab f)
  (tabl_razmer (list "\\A1;l\\H0.7x;\\S^AB;\\H1.4286x;" "\\A1;l\\H0.7x;\\S^BC;\\H1.4286x;"
		      "\\A1;l\\H0.7x;\\S^EF;\\H1.4286x;" "y" "\\A1;l\\H0.7x;\\S^CD;\\H1.4286x;"
		     "\\A1;l\\H0.7x;\\S^ED;\\H1.4286x;" "x" "\\A1;l\\H0.7x;\\S^BS;\\H1.4286x;"
		)
  (list ab bc ef y cd ed x bs) mul)
);end of vvod


;;;;;;;;;;;;;;;;;;;;;Функции которые считают;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------------------------
;;;;;;---=================Точки механизма ================-------
(defun tochkim ()
  (setq
    a (getpoint "\nМеханизм: ")
    b (polar a fi ab)
    d (list (+ (car a) x) (- (cadr a) y))    
    c (polar d
	     (+ (angle d b)
		(arccos (/ (+ (* cd cd) (expt (distance d b) 2.)  (* bc bc -1.))
			   (* 2. cd (distance d b))
			); /
		); arccos
	     );+
	     cd
      )
    e (polar d (+ (angle d c) (/ pi 12.)) ed)
    f (cadr (dwgru d (polar d 0. 100.) e ef))
    s (polar b (angle b c) bs)
  )
)
;----------------------------------------------------------------------------------
;--==Точки плана скоростей===----
(defun plansk (/ )
  (initget 1)
  (setq pb (getdist "\nУкажите длину скорость Vb на плане скоростей [mm] ")
  	muv (/ mvb pb)
	pol (getpoint "\nГде чертить план скоростей ")  
  
  	bv (polar pol (- (angle a b) (/ pi 2.)) pb)
	cv (inters pol (polar pol (+ (angle d c) (* 0.5 pi)) 100.)
		   bv (polar bv (+ (angle b c) (* 0.5 pi)) 100.)
		   nil
	   )
	ev (polar pol (+ (angle pol cv) (/ pi 12.)) (distance cv pol))
	fv (inters pol (polar pol 0. 100.)
		   ev (polar ev (- (angle f e) (/ pi 2.)) 1005.)
		   nil
	   )
	sv (polar bv (angle bv cv) (* bs (/ (distance cv bv) bc)))  
  ;========Модули скоростей=======
  	mvc (* muv (distance pol cv))	
	mvcb (* muv (distance cv bv))
	mve (* muv (distance pol ev))
	mvf (* muv (distance fv pol))
	mvfe (* muv (distance fv ev))
	mvs (* muv (distance pol sv))
	masv (list mvc mve mvf mvs mvcb mvfe)
	masb (list "C" "E" "F" "S" "CB" "FE")
	;Подсчет угловых скоростей
	ome2 (/ mvcb lbc)
	ome3 (/ mvc lcd)
	ome4 (/ mvfe lef)
	masom (list ome2 ome3 ome4)
	;=====Подсчет нормальных ускорений====
	acbn (/ (expt (* (distance cv bv) muv) 2.) lbc)
	acdn (/ (expt (* (distance pol cv) muv) 2.) lcd)
	afen (/ (expt (* (distance fv ev) muv) 2.) lef)
  )
  (duga d c pol cv 0.4 "\\A1;\\H2;\U+03C9\\H0.7x;\\S^3;\\H2;")
  (duga b c bv cv 0.4 "\\A1;\\H2;\U+03C9\\H0.7x;\\S^2;\\H2;")
  (duga e f ev fv 0.4 "\\A1;\\H2;\U+03C9\\H0.7x;\\S^4;\\H2;")
)
;----------------------------------------------------------------------------------
;---------========План ускорений========---------
(defun planusk ()
  (initget 1)
  (setq pa (getdist "\nДлина ускорения точки B на плане ускорений [mm] ")
	polu (getpoint "\nГде чертить план ускорений ")
	mua (/ mab pa)
	;=====Длины нормальных ускорений на плане=====
	dcbn (/ acbn mua)
	dcdn (/ acdn mua)
	dfen (/ afen mua)	

	ba (polar polu (angle b a) pb)
	cbn (polar ba (angle c b) dcbn)
	cdn (polar polu (angle c d) dcdn)
	ca (inters cbn (polar cbn (+ (angle b c) (* pi 0.5)) 100.)
		   cdn (polar cdn (+ (angle d c) (* pi 0.5)) 100.)
		   nil
	   )
	ea (polar polu (+ (angle polu ca) (/ pi 12)) (distance ca polu))
	fen (polar ea (angle f e) dfen)
	fa (inters polu (polar polu 0. 100.)
		   fen (polar fen (- (angle f e) (/ pi 2.)) 100.)
		   nil
	   )
	sa (polar ba (angle ba ca) (* bs (/ (distance ca ba) bc)))
	;=====Модули ускорений=========
	mac (* mua (distance polu ca))
	mae (* mua (distance polu ea))
	maf (* mua (distance polu fa))
	macbt (* mua (distance cbn ca))
	macdt (* mua (distance cdn ca))
	mas (* mua (distance polu sa))
	macb (* mua (distance ca ba))
	mafet (* mua (distance fen fa))
	mafe (* mua (distance fa ea))
	masa (list mac mae maf mas macb mafe)	
	eps2 (/ macbt lbc)
	eps3 (/ macdt lcd)
	eps4 (/ mafet lef)
	masep (list eps2 eps3 eps4)
  );_end of setq
  (duga b c cbn ca 0.6 "\\A1;\\H2;\U+03B5\\H0.7x;\\S^2;\\H2;")
  (duga d c cdn ca 0.6 "\\A1;\\H2;\U+03B5\\H0.7x;\\S^3;\\H2;")
  (duga e f fen fa 0.6 "\\A1;\\H2;\U+03B5\\H0.7x;\\S^4;\\H2;")
);_end of defun
;----------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;Функции которые чертят;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cherm (/ i)
  (li (list a b c d e f))  
  (add_block f 0.)
  (add_opor a (+ (* 0.5 pi) fi))
  (add_opor d pi)
  (duga a b b (polar b (- (angle a b) (* 0.5 pi)) 10.) 0.5 "\\A1;\\H2;\U+03C9\\H0.7x;\\S^1;\\H2;")
  (mapcar '(lambda (t1) (add_circ 0.7 t1)) (list a b c d e f s))
  (setq i 65)
  (mapcar '(lambda (t1) (progn
			  (add_text t1 (chr i))
			  (setq i (1+ i))
			)
	   );end of lambda
           (list a b c d e f)
  );end of mapcar
  (add_text s "S")
  (muul lab ab)
);_end of defun

(defun cherv () 
  (risskor pol bv "b" "v^B")
  (risskor bv cv nil "v^CB")
  (risskor pol cv "c" "v^C")
  (risskor pol ev "e" "v^E")
  (risskor pol fv "f" "v^F")
  (risskor ev fv nil "v^FE")
  (risskor pol sv "s" "v^S")
  (add_text pol "p,a,d")
  (muuv mvb pb)  
  (formv omeg lab)
)

(defun chera () 
  (risskor polu ba "b" "a^B")
  (risskor ba cbn nil "an^CB")
  (risskor polu cdn nil "an^CD")
  (risskor cbn ca nil "a\U+03C4^CB")
  (risskor cdn ca nil "a\U+03C4^CD")
  (risskor polu ca "c" "a^C")
  (risskor polu ea "e" "a^E")
  (risskor polu fa "f" "a^F")
  (risskor ea fen nil "an^FE")
  (risskor ea fa nil "a^FE")
  (risskor fen fa nil "a\U+03C4^FE")
  (risskor polu sa "s" "a^S")
  (risskor ba ca nil "a^CB")
  (add_text polu "\\A1;p\\H0.7x;\\S^1;\\H1.4286x;,a,d")
  (muua mab pa)
  (forma omeg lab)
);_end of defun

(defun omep ()
  
  (formsk2 muv (distance pol cv) "C" "pc" t)
  (formsk2 muv (distance pol ev) "E" "pe" t)  
  (formsk2 muv (distance pol fv) "F" "pf" t)   
  (formsk2 muv (distance pol sv) "S" "ps" t)
 
  (formsk2 muv (distance cv bv) "CB" "bc" t)
  (formsk2 muv (distance ev fv) "FE" "fe" t)
  (add_text (getpoint "\nVspom speed point E: ")
	    (strcat "dc=de="(rtos (distance pol ev) 2 0)" мм")
  )
  ;Г“Г±ГЄГ®Г°ГҐГ­ГЁГї
  (formsk2 mua (distance polu ca) "C" "p\\H0.7x;\\S^1;\\H1.4286x;c" nil)
  (formsk2 mua (distance polu fa) "F" "p\\H0.7x;\\S^1;\\H1.4286x;f" nil)  
  (formsk2 mua (distance polu ea) "E" "p\\H0.7x;\\S^1;\\H1.4286x;e" nil)
  (formsk2 mua (distance polu sa) "S" "p\\H0.7x;\\S^1;\\H1.4286x;s" nil)
  (formsk2 mua (distance ba ca) "CB" "bc" nil)
  (formsk2 mua (distance fa ea) "FE" "fe" nil)
  (add_text (getpoint "\nVspom acel point E: ")
	    (strcat "dc=de="(rtos (distance polu ea) 2 0)" мм")
  )
 
  (formusk "aCB" mvcb lbc)
  (formusk "aCD" mvc lcd)
  (formusk "aFE" mvFE lef)
 
  (formuskt2 "CB" (distance cbn ca) mua)
  (formuskt2 "CD" (distance cdn ca) mua)
  (formuskt2 "FE" (distance fen fa) mua)

  (ugsu "ome2CB" mvcb lbc)
  (ugsu "ome3CD" mvc lcd)
  (ugsu "ome4FE" mvFE lef)
 
  (ugsu "eps2CB" macbt lbc)
  (ugsu "eps3CD" macdt lcd)
  (ugsu "eps4FE" maFEt lef)  
)




(defun c:sem11_2 (/ osm le ab omeg lab mul lbc bc lef ef ly y lcd cd led ed lbs bs lx x fi 
mvb mab a b d c e f s pb muv pol bv cv ev fv sv mvc mvcb mve mvf mvfe mvs masv masb ome2 ome3 ome4 
masom acbn acdn afen pa polu mua dcbn dcdn dfen ba cbn cdn ca ea fen fa sa mac mae maf macbt macdt 
mas macb mafet mafe masa eps2 eps3 eps4 masep)
  (vl-load-com) 
  (setq osm (getvar "OSMODE"))
  (setvar "OSMODE" 20919)
  (load "funkcii_tmm.LSP")
  (load "tmm_semi.lsp")
  (vvod)
  (tochkim)
  (cherm)  
  (plansk)
  (cherv)
  (planusk)
  (chera)
  (omep)
  (tablica_va masb masv masa masom masep)
  (setvar "osmode" osm)  
)
(c:sem11_2)