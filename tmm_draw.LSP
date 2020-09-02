;-----------=============рисует скорость из точки t1 в t2============--------------
(defun risskor (t1 t2 simv simv1 / apr a1 a2 wid)
  (setq a1 (trans t1 1 0)
	a2 (trans t2 1 0)
  )
  (if (> (distance a1 a2) 12)
    (setq apr (polar a2 (angle a2 a1) 5)
	  wid 1.1
    ); setq
    (setq apr (polar a2 (angle a2 a1) (* 0.27 (distance a1 a2)))
	  wid (* (distance a1 a2) 0.07)
    ); setq
  ); if
  (entmakex (list '(0 . "LWPOLYLINE")
		  '(100 . "AcDbEntity")
		  '(100 . "AcDbPolyline")
		  '(90 . 3)
		  '(70 . 0)
		  (cons 10 a2)
		  '(40 . 0.)
		  (cons 41 wid)
		  '(42 . 0.)
		  (cons 10 apr)		  
		  (cons 10 a1)		  
	    ); list
  ); entmakex
  (if simv
    (entmakex (list '(0 . "MTEXT")
		    '(100 . "AcDbEntity")
		    '(100 . "AcDbMText")
		    '(41 . 20)
		    (cons 7 (getvar "TEXTSTYLE"))
		    (cons 10 a2)
		    (cons 1 simv
;;;			  (strcat "\\A1;"
;;;			  	    (substr simv 1 1)
;;;			 	    "\\H0.7x;\\S"
;;;				    (substr simv 2 2)";"
;;;				    "\\H1.42857x;"
;;;				    (substr simv 4)
;;;		 	    );_end of strcat
		    ); cons
	      ); list
    ); entmakex
  ); if
  (if simv1
    (entmakex (list '(0 . "MTEXT")
		    '(100 . "AcDbEntity")
		    '(100 . "AcDbMText")
		    (cons 7 (getvar "TEXTSTYLE"))
		    '(41 . 20)
		    (cons 10 (polar a1 (angle a1 a2) (* 0.7 (distance a1 a2))))
		    (cons 1 (strcat "\\A1;\\O"
			 	    (if (equal (substr simv1 1 1) "v")
			  	      "\U+03C5"
			              (substr simv1 1 1)
			            ); if
				    "\\o\\H0.7x;\\S"
				    (substr simv1 2)";"
	       		    );_end of strcat
		    ); cons
	      ); list
    ); entmakex
  ); if
); defun
;;;;*********************************************************************************
;;;;*********************************************************************************
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun li (list_of / i list_new)
  (if (> (length list_of) 1)
    (progn
      (entmakex (list '(0 . "line")		  
		'(48 . 0.59)
		(cons 8 (getvar "CLAYER"))
		(cons 10 (trans (nth 0 list_of) 1 0))
		(cons 11 (trans (nth 1 list_of) 1 0))
	    ); list
      ); entmakex
      (setq i 1)
      (repeat (1- (length list_of))
	(setq list_new (cons (nth i list_of) list_new)
	      i (1+ i)
	); setq
      ); repeat
      (li list_new)
    ); progn
  ); if
); defun

(defun add_tex (t1 text_ param1 param2 / a1)
  ;param1 = index of mu
  ;param2 = razmernost of mu
  (setq a1 (trans t1 1 0))
  (entmakex (list '(0 . "MTEXT")
		  '(100 . "AcDbEntity")
		  '(100 . "AcDbMText")
		  '(41 . 40)
		  (cons 7 (getvar "TEXTSTYLE"))
		  (cons 8 (getvar "CLAYER"))
		  (cons 10 a1)
		  (cons 1   (if (null param1)
			        text_    			      
			        (strcat "\\A1;\\H3.0;\U+03BC"
	 				"\\H0.7x;\\S^"
					param1
					";\\H3.2;="
		   			(rtos text_)
		                        " \\H0.7x;\\S"
					param2 ";"
				); strcat
			    ); if
		  ); cons
	        ); list
      ); entmakex
); defun

(defun muu (index_ znachen razmern / a1)  
  (setq a1 (trans (getpoint (strcat "\nМасштабный коэффициент " index_ " [" razmern "]" )) 1 0))
  (entmakex (list '(0 . "MTEXT")
		  '(100 . "AcDbEntity")
		  '(100 . "AcDbMText")
		  '(41 . 40)
		  (cons 7 (getvar "TEXTSTYLE"))
		  (cons 8 (getvar "CLAYER"))
		  (cons 10 a1)
		  (cons 1 (strcat "\\A1;\\H3.0;\U+03BC"
				  "\\H0.7x;\\S^"
				  index_
				  ";\\H3.2;="
				  (rtos znachen)
				  " \\H0.7x;\\S"
				  razmern ";"
			  ); strcat
		  ); cons
	        ); list
      ); entmakex
); defun

(defun add_pl (list_1 / list_2)
  (setq list_2 (mapcar '(lambda (a) (trans a 1 0)) list_1))
  (entmakex (append	      
	      (list '(0 . "LWPOLYLINE")
		    '(100 . "AcDbEntity")
		    '(100 . "AcDbPolyline")
		    (cons 90 (length list_1))
		    '(70 . 0)
	      ); list
	      (mapcar '(lambda (a) (cons 10 a))
		      list_2
	      ); mapcar
	    ); append
  ); entmakex
); defun


(defun add_circ (rad centr1 / centr)
  (setq centr (trans centr1 1 0))
  (entmakex (list '(0 . "CIRCLE")		  
		  (cons 8 (getvar "CLAYER"))
		  (cons 10 centr)
		  (cons 40 rad)
	    );list
  ); entmakex
)

(defun add_text (t1 text_ / a1 sear nom1 nom2 nom3 b)
  (setq a1 (trans t1 1 0)
	nom1 0
	nom2 0
	nom3 0
  )
  (defun sear (nom tex_ / a tex_t)
    (if (setq a (vl-string-search "\U+" tex_))
      (progn
	(setq nom1 (1+ nom)
	      tex_t (substr tex_ (+ a 3))
	)
	(sear nom1 tex_t)
      ); progn
    ); if
    (if (setq a (vl-string-search "\\S" tex_))
      (progn
	(setq nom2 (1+ nom)
	      tex_t (substr tex_ (+ a 3))
	)
	(sear nom2 tex_t)
       ); progn
    ); if
    (if (setq a (vl-string-search "\\H" tex_))
      (progn
	(setq nom3 (1+ nom)
	      tex_t (substr tex_ (+ a 3))
	)
	(sear nom3 tex_t)
       ); progn
    ); if
  ); defun
  (sear 0 text_)  
  (setq b (fix (* 3 (- (strlen text_) (* 6 nom1) (* 4 nom2) (* nom3 7)))))
  (entmakex (list '(0 . "MTEXT")
		  '(100 . "AcDbEntity")
		  '(100 . "AcDbMText")
		  (cons 41 b)
		  (cons 7 (getvar "TEXTSTYLE"))
		  (cons 8 (getvar "CLAYER"))
		  (cons 10 a1)
		  (cons 1 text_)
	        ); list
      ); entmakex
); defun





(defun add_block (t1 ug / lis)
  (setq lis (list (polar t1 (- ug (atan 0.5)) (sqrt 31.25))
		  (polar t1 (+ ug (atan 0.5)) (sqrt 31.25))
		  (polar t1 (+ ug (atan 0.5) (* 2. (atan 2.))) (sqrt 31.25))
		  (polar t1 (- ug (atan 0.5) (* 2. (atan 2.))) (sqrt 31.25))
		  (polar t1 (- ug (atan 0.5)) (sqrt 31.25))
	    )
  )
  (add_pl lis)
)

(defun add_opor (t1 ug / a1 a2 a3 a4
b1 b2 b3 b4 b5 b6 b7 b8 b9)
  (setq a1 (polar t1 ug 0.7)
	a4 (polar t1 (+ pi ug) 0.7)
	a2 (polar a1 (+ ug (/ pi 3.)) 5.)
	a3 (polar a4 (+ ug (* pi (/ 2. 3.))) 5.)
	b1 (polar a2 (angle a2 a3) 1.28)
	b2 (polar b1 (+ ug (* pi 0.25)) 2.)
	b3 (polar b1 (angle a2 a3) 1.28)
	b4 (polar b3 (+ ug (* pi 0.25)) 2.)
	b5 (polar b3 (angle a2 a3) 1.28)
	b6 (polar b5 (+ ug (* pi 0.25)) 2.)
	b7 (polar b5 (angle a2 a3) 1.28)
	b8 (polar b7 (+ ug (* pi 0.25)) 2.)
	b9 (polar a3 (+ ug (* pi 0.25)) 2.)
  )
  (add_pl (list a1 a2 a3 a4))
  (li (list b1 b2))(li (list b3 b4))(li (list b5 b6))(li (list b7 b8))
  (li (list a3 b9))
)

(defun duga (pol roo p00 p11 param tex_ / polus ro p0 p1 a1 a2 a3 a4 tan1 tan2)
  (setq polus (trans pol 1 0)
	ro (trans roo 1 0)
	p0 (trans p00 1 0)
	p1 (trans p11 1 0)
  	a1 (polar polus (angle polus ro) (- (* param (distance polus ro)) 18.))
  )
  (if (clockwise-p polus ro (polar ro (angle p0 p1) 10.))
    (setq tan1 (/ (sin (/ pi 24.)) (cos (/ pi 24.)))
	  tan2 (/ (sin (/ pi 72.)) (cos (/ pi -72.)))
	  a2 (polar a1 (- (angle a1 ro) (/ pi 9.)) 18.)
	  a3 (polar a1 (+ (angle a1 ro) (/ pi 18.)) 18.)
	  a4 (polar a1 (+ (angle a1 ro) (/ pi 9.)) 18.)
    )
    (setq tan1 (/ (sin (/ pi -24.)) (cos (/ pi 24.)))
	  tan2 (/ (sin (/ pi -72.)) (cos (/ pi -72.)))
	  a2 (polar a1 (+ (angle a1 ro) (/ pi 9.)) 18.)
	  a3 (polar a1 (- (angle a1 ro) (/ pi 18.)) 18.)
	  a4 (polar a1 (- (angle a1 ro) (/ pi 9.)) 18.)
    )
  ) 
  (entmakex (list '(0 . "LWPOLYLINE")
		  '(100 . "AcDbEntity")
		  '(100 . "AcDbPolyline")
		  '(90 . 3)
		  '(70 . 0)
		  '(38 . 0.)
		  '(39 . 0.)
		  (cons 10 a2)
		  '(40 . 0.)
		  '(41 . 0.)
		  (cons 42 tan1)
		  (cons 10 a3)
		  '(40 . 1.15)
		  '(41 . 0.)
		  (cons 42 tan2)
		  (cons 10 a4)		  
	    ); list
   ); entmakex
  (add_text (trans a2 0 1) tex_)
)

(defun add_arc (center a1 a2 / b1 b2 cent)
  (setq cent (trans center 1 0)
	b1 (trans a1 1 0)
	b2 (trans a2 1 0)
  ); setq
  (entmakex (list '(0 . "ARC")
		  '(100 . "AcDbEntity")
		  '(67 . 0)
		  '(100 . "AcDbCircle")
		  (cons 10 cent)
		  (cons 40 (distance cent b1))
		  '(210 0.0 0.0 1.0)
		  '(100 . "AcDbArc") 
		  (cons 50 (angle cent b1))
		  (cons 51 (angle cent b2))
	    ); list
  ); entmakex
); defun

(defun add_ang (center a1 a2 tex)
  (vla-adddimangular (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
     (vlax-3d-point (trans center 1 0))
     (vlax-3d-point (trans a1 1 0))
     (vlax-3d-point (trans a2 1 0))
     (vlax-3d-point (trans (polar a1 (angle a1 a2) (* 0.5 (distance a1 a2))) 1 0))
  ); vla-adddimangular
  (vla-put-textprefix (vlax-ename->vla-object (entlast))
    tex
  ); vla-put-textprefix
  (vla-put-Textgap (vlax-ename->vla-object (entlast))
    1.2
  ); vla-put-Textgap
); defun


(defun new_ucs (name / gde newus)
  (setq gde (trans (getpoint (strcat "Укажите где будет "name":")) 1 0)
        newus (vla-add ucsi
		       (vlax-3d-point gde)
		       (vlax-3d-point (polar gde 0 5))
		       (vlax-3d-point (polar gde (* pi 0.5) 5))
		       name
	      );_end of vla-add
  );_end of setq
  (vla-put-activeucs document newus)
  (vlax-release-object newus)
)