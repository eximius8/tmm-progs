(defun asin (sine / cosine)
    (setq cosine (sqrt (- 1.0 (expt sine 2))))
    (if (zerop cosine)
      (setq cosine 1e-30)
    );if
    (atan (/ sine cosine))
);defun asin

;;;;;;;;;;;;;;;;;��������������� �������;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arccos (cosalf)
   (cond
     ((> (abs cosalf) 1) (alert (strcat "\n������!\n� ������� ����������� �������� �������� "(rtos cosalf)
					"\n ��� >1 !")))
     ((= cosalf 0.) (/ pi 2.))
     ((>= cosalf 0.) (atan (/ (sqrt (- 1. (* cosalf cosalf))) cosalf)))
     ((< cosalf 0.) (+ pi (atan (/ (sqrt (- 1. (* cosalf cosalf))) cosalf)))) 
   )
)

(defun tg (alfa)
  (/ (sin alfa) (cos alfa))
)