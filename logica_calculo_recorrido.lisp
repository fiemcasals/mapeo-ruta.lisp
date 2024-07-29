(defvar *iteraciones* 0
  "Cantidad máxima de iteraciones permitidas.")

(defun recorrer-y-ejecutar (pos costo_acarreo puestos_acarreo)
  ;; Salir de la función si el costo_acarreo es mayor o igual a *costo_max_admisible*
  ;; o si se ha superado el número máximo de iteraciones permitidas
  (when (or (>= costo_acarreo *costo_max_admisible*)
            (> *iteraciones* 50000))
    (print "costo_acarreo es mayor o igual a *costo_max_admisible* o se ha alcanzado el máximo de iteraciones, saliendo de la función")
    (print *iteraciones*)
    (return-from recorrer-y-ejecutar))

  ;; Loop para iterar sobre los posibles movimientos
  (loop for i from -1 to 1
        do (loop for j from -1 to 1
                 unless (and (= i 0) (= j 0)) ; Solo si no ambas variables son 0
                 do (let* ((nuevo-pos (list (+ (first pos) i) (+ (second pos) j)))
                           (nuevo-costo (calculo_costo_total nuevo-pos))
                           (nuevo-costo-acarreo (float (+ costo_acarreo nuevo-costo)))
                           (costo-max (float *costo_max_admisible*))
                           (latitud (first nuevo-pos))
                           (longitud (second nuevo-pos))
                           (query "SELECT valor FROM registros WHERE latitud = ~a AND longitud = ~a")
                           (resultado (execute-query query latitud longitud))
                           (valor-registro (and (not (null resultado))
                                               (cdr (assoc 'valor (car resultado)))))

                   ;; Si no hay valor registrado, insertar el nuevo valor
                   (when (null valor-registro)
                     (insert-data "registros" '("latitud" "longitud" "valor")
                                  (list latitud longitud nuevo-costo-acarreo)))

                   ;; Incrementar el contador de iteraciones
                   (incf *iteraciones*)

                   ;; Si el nuevo costo es menor que el costo máximo admisible y menor al ya registrado en la base de datos
                   (when (and (< nuevo-costo-acarreo costo-max)
                               (or (not valor-registro) (< nuevo-costo-acarreo valor-registro)))
                     (update-data-coordenadas "registros" latitud longitud '("valor")
                                               (list nuevo-costo-acarreo)))

                   ;; Si el nuevo costo es menor que el costo máximo admisible y la nueva posición no es el destino
                   (when (and (< nuevo-costo-acarreo *costo_max_admisible*)
                               (not (equal nuevo-pos *destino*)))
                     (push nuevo-pos puestos_acarreo)
                     ;; Llama recursivamente
                     (recorrer-y-ejecutar nuevo-pos nuevo-costo-acarreo puestos_acarreo))

                   ;; Verifica si la nueva posición es el destino
                   (when (and (equal nuevo-pos *destino*)
                               (< nuevo-costo-acarreo *costo_max_admisible*))
                     (print "SE REGISTRA UN NUEVO VALOR Y POSICION PARA DESTINO")
                     (print nuevo-costo-acarreo)
                     (setf *costo_max_admisible* nuevo-costo-acarreo)
                     (setf *recorrido_recomendable* puestos_acarreo)
                     (print "se cargó un nuevo *costo_max_admisible*")
                     (print *costo_max_admisible*)))))
  ) )