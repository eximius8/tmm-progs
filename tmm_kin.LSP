(defun new_funcia_kin (ugol0 fiplus graf dia_name / polus_mexan fii vektorva vektoraa
		   	 polus_skor polus_uskor ugolf pol) 
  (initget 1 "Yes No")
  (if (equal "Yes" (getkword "\nНачертить все 12 планов механизма? [Yes/No]: "))
    (progn
      (initget 65)
      (setq fii ugol0
	    polus_mexan (getpoint "\nУкажите где чертить механизм: ")
      )
      (repeat 12
         (tochkim polus_mexan fii)
         (cherm polus_mexan)
         (setq fii (+ fii (/ pi 6. fiplus)))
      )
      (initget 65)
      (add_tex (getpoint "\nУкажите где писать масштаб для плана механизма: ") mul "l" "м/мм")
      (if graf
	(progn
	 (initget 1 "Yes No")
	 (if (equal (getkword (strcat "\nНачертить диаграммы " dia_name "? [Yes/No]: ")) "Yes")
	   (grafik)
	 ); if
	)
      )
    ); progn
  )
  (initget 7)
  (setq vektorva (getdist "\nВведите длину вектора скорости точки A: ")
	fii ugol0
	muv (/ va vektorva)
  )
  (initget 1 "Yes No")
  (if (equal "Yes" (getkword "\nНачертить все 12 планов скоростей? [Yes/No]: "))
    (progn
      (repeat 12        
        (initget 65)    
        (plansk (setq pol (getpoint "\nУкажите где чертить план скоростей: ")) vektorva fii)
        (cherv pol)
        (setq fii (+ fii (/ pi 6. fiplus)))
      ); repeat
      (initget 65)
      (add_tex (getpoint "\nУкажите где писать масштаб для плана скоростей: ") muv "\U+03C5" "м с/мм")
    ); progn
  ); if
  (initget 7)
  (setq vektoraa (getdist "\nВведите длину вектора ускорения точки A: ")
	mua (/ aan vektoraa)
  )
  (initget 65)  
  (planusk (setq polus_uskor (getpoint "\nУкажите где чертить план ускорений: "))
	   vektoraa (+ ugol0 (* fiplus fi)))
  (chera polus_uskor)
  (initget 65)
  (add_tex (getpoint "\nУкажите где писать масштаб для плана ускорений: ") mua "a" "м с/мм")
  (initget 1 "Yes No")
  (if (equal (getkword "\nЭкспорт в excel? [Yes/No]: ") "Yes")
    (excel_export)
  )
)
