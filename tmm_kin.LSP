(defun new_funcia_kin (ugol0 fiplus graf dia_name / polus_mexan fii vektorva vektoraa
		   	 polus_skor polus_uskor ugolf pol) 
  (initget 1 "Yes No")
  (if (equal "Yes" (getkword "\n��������� ��� 12 ������ ���������? [Yes/No]: "))
    (progn
      (initget 65)
      (setq fii ugol0
	    polus_mexan (getpoint "\n������� ��� ������� ��������: ")
      )
      (repeat 12
         (tochkim polus_mexan fii)
         (cherm polus_mexan)
         (setq fii (+ fii (/ pi 6. fiplus)))
      )
      (initget 65)
      (add_tex (getpoint "\n������� ��� ������ ������� ��� ����� ���������: ") mul "l" "�/��")
      (if graf
	(progn
	 (initget 1 "Yes No")
	 (if (equal (getkword (strcat "\n��������� ��������� " dia_name "? [Yes/No]: ")) "Yes")
	   (grafik)
	 ); if
	)
      )
    ); progn
  )
  (initget 7)
  (setq vektorva (getdist "\n������� ����� ������� �������� ����� A: ")
	fii ugol0
	muv (/ va vektorva)
  )
  (initget 1 "Yes No")
  (if (equal "Yes" (getkword "\n��������� ��� 12 ������ ���������? [Yes/No]: "))
    (progn
      (repeat 12        
        (initget 65)    
        (plansk (setq pol (getpoint "\n������� ��� ������� ���� ���������: ")) vektorva fii)
        (cherv pol)
        (setq fii (+ fii (/ pi 6. fiplus)))
      ); repeat
      (initget 65)
      (add_tex (getpoint "\n������� ��� ������ ������� ��� ����� ���������: ") muv "\U+03C5" "� �/��")
    ); progn
  ); if
  (initget 7)
  (setq vektoraa (getdist "\n������� ����� ������� ��������� ����� A: ")
	mua (/ aan vektoraa)
  )
  (initget 65)  
  (planusk (setq polus_uskor (getpoint "\n������� ��� ������� ���� ���������: "))
	   vektoraa (+ ugol0 (* fiplus fi)))
  (chera polus_uskor)
  (initget 65)
  (add_tex (getpoint "\n������� ��� ������ ������� ��� ����� ���������: ") mua "a" "� �/��")
  (initget 1 "Yes No")
  (if (equal (getkword "\n������� � excel? [Yes/No]: ") "Yes")
    (excel_export)
  )
)
