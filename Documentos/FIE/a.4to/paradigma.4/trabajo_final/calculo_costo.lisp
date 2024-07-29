
;;debe entrar bajo el nombre de una funcion
;;variables de entrada -> pos_actual, costo_acarreado, lista_pos_ant, 
;;variable global -> costo_maximo_destino, destino


    ;;necesito una funcion que calcule utilizando una lista con todos los registros de ag existentes en la tabla Ag, si la pos_siguiente, se encuentra dentro del alcance que figura en la tabla ag, para cada punto de agua registrado en dicha tabla. De encontrarse en dicho radio, se le carga a la variable costo, un numero tendiente a infinito, de no estar en el radio no se le carga ningun valor.

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
                              (- (* (- alcance distancia) 10))
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
      
  

  
    ;;de igual manera necesito otra funcion que calcule utilizando una lista con todos los registros existentes en la tabla Eno, si la pos_siguiente, se encuentra dentro del alcance que figura en la tabla eno, para cada registro de la tabla. de encontrarse en dicho radio, se le suma el (alcance registrado en la tabla eno menos la distancia a la que se encuentra) multiplicado por el cociente entre la magnitud del eno y la magnitud del propio elemento (la magnitud del propio elemento es una variable global ya definida)

    ;;de igual manera necesito otra funcion que calcule de igual modo que con el eno, si estamos en el radio de los registros de refuerzos, de estarlo se le restara a costo, el (alcance menos la distancia a la ubicacion) multiplicado por la magnitud del elemento (tambien registrado en los registros)

    ;;y finalmente necesito una cuarta funcion que calcule si estamos dentro del radio de un registro qbn, esto se va a calcular si el alcance registrado en la tabla es mayor que la distancia hasta el punto, de ser asi se sumara (el alcance registrado - la distancia entre los puntos). de no estar en el radio, no se sumara nada


#|(defun camino-recursivo-costos (pos-actual costo-acarreo lista-pos-ant)
  ;; Definir todos los puntos adyacentes
  (let* ((pos-obl-izq (list (- (first pos-actual) 1) (- (second pos-actual) 1)))
         (cos (calculo-costo-ag pos-obl-izq))
         (nuevo-costo (+ costo-acarreo cos)))
    
    ;; Actualizar la lista de posiciones anteriores
    (push pos-obl-izq lista-pos-ant)
    
    ;; Actualizar la posición actual
    (setq pos-actual pos-obl-izq) ;; la idea es que pos-actual valga lo que vale pos-obl-izq
    
    ;; Llamada recursiva
    (camino-recursivo-costos pos-actual nuevo-costo lista-pos-ant))) |#

