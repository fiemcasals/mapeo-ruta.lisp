
      #| UNICA FUNCION, CALCULOS LOS COSTOS TOTALES A CADA PUNTO GEOGRAFICO |#


(defvar *puestos-ag*)
(defvar *puestos-qbn*)
(defvar *puestos-suministros*)
(defvar *puestos-eno*)
(defvar *magnitud_elem_ppio*)
(defvar *puestos-refuerzos*)


#| CALCULO DEL AGUA |#

(defun calculo-costo-ag (posicion)
  (let ((costo 0))
    (dolist (puesto *puestos-ag* costo)
      (let* ((lat-puesto (second puesto))
             (long-puesto (fourth puesto))
             (alcance (sixth puesto))
             (distancia (calculate-distance (first posicion) (second posicion)
                                            lat-puesto long-puesto))
             (valor-costo (if (< distancia alcance)
                              (+ costo 10000)
                              costo)))
        (setq costo valor-costo))) 
    costo))

#| CALCULO DEL QBN |#

(defun calculo-costo-qbn (posicion)
  (let ((costo 0))
    (dolist (puesto *puestos-qbn* costo)
      (let* ((lat-puesto (second puesto))
             (long-puesto (fourth puesto))
             (alcance (sixth puesto))
             (distancia (calculate-distance (first posicion) (second posicion)
                                            lat-puesto long-puesto))
             (valor-costo (if (< distancia alcance)
                              (+ costo (- alcance distancia))
                              costo)))
        (setq costo valor-costo))) ; Corrige el paréntesis aquí
    costo)) ; Corrige el paréntesis aquí


#| CALCULO ENO |#

(defun calculo-costo-eno (posicion)
  (let ((costo 0))
    (dolist (puesto *puestos-eno* costo)
      (let* ((lat-puesto (second puesto))
             (long-puesto (fourth puesto))
             (alcance (sixth puesto))
             (magnitud_elem_eno (eighth puesto))
             (distancia (calculate-distance (first posicion) (second posicion)
                                            lat-puesto long-puesto))
             (valor-costo (if (< distancia alcance)
                              (+ (* (- alcance distancia)
                                    (/ magnitud_elem_eno *magnitud_elem_ppio*)))
                              0))
             )
        (setq costo (+ costo valor-costo)))
    costo)))


    #| CALCULO REFUERZO |#

(defun calculo-costo-refuerzos (posicion)
  (let ((costo 0))
    (dolist (puesto *puestos-refuerzos* costo)
      (let* ((lat-puesto (second puesto))
             (long-puesto (fourth puesto))
             (alcance (sixth puesto))
             (magnitud_elem_eno (eighth puesto))
             (distancia (calculate-distance (first posicion) (second posicion)
                                            lat-puesto long-puesto))
             (valor-costo (if (< distancia alcance)
                              -5
                              ;(- (* (- alcance distancia) 10))
                              0))
             )
        (setq costo (+ costo valor-costo)))
    costo)))



(defun calculo_costo_total (pos)
      (+ (calculo-costo-ag pos)
        (calculo-costo-qbn pos)
        (calculo-costo-eno pos)
        (calculo-costo-refuerzos pos)
        10));;un 10 fijo por desplazamiento
      
  

  