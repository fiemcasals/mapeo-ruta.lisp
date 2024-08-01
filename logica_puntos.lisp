
;tengo que tratar a los puestos de suministros como coord
;partida siempre va a ser la coordenada con menor valor distinta a nil
;destino va a ser la cabeza de los ptos-sum-filtrados;
;se llama recursivamente a la funcion mandando (cdr ptos-sum-filtrados)
;al ser ptos-sum-filtrados nil. se busca un nuevo punto de partida entre todos los ptos, buscando el de valor mas bajo
;partiendo desde ese nuevo punto de partida se buscan los ptos-sum-filtrados y se repite el proceso
;a medida que voy calculando los costos entre puntos de suministros, voy cargando las estaciones intermedias y los costos totales a un vector que contendra: pto partida, destino, lista de puntos intermedios, y costo.
;al querer volver desde el final hacia adelante, busco los ptos-sum-filtrados, colocando a destino como punto de partida. una vez que ya tengo todos los puntos cercanos, busco el de menor valor, una vez que ya se desde donde y hasta donde debo ir, busco de manera invertida en mi lista. y sumo el valor a un valor_total. Repitiendo el procedimiento desde el siguiente punto de suministro.


;;puedo generar una funcion que genere una lista filtrada desde una posicion inicial, calcule la distancia hasta el primer elemento de la lista filtrada. y descabece uno a uno los puntos buscando la ruta mas corta a cada punto.
;;seguidamente desde cada punto debe hacer lo mismo. 





      #| DA INICIO AL PROGRAMA BUSCANDO LA POSICION DE MENOR VALOR -> OSEA ES EL INICIO |#

      #| ACA ES DONDE ME TENGO QUE PARAR PARA LOGRAR EL INICIO EN CADA PUNTO DE SUMINISTROS PORTERIORMENTE AL PUNTO DE PARTIDA |#


     


(defun empezar 
          ;; Buscar el registro con el menor costo
(let* ((pos-menor-costo (obtener-posicion-menor-costo-suministros)))

;if para cortar al llegar a *destino_final*

  (when pos-menor-costo
    ;; Obtener latitud y longitud
    (let* ((latitud (car pos-menor-costo))
           (longitud (second pos-menor-costo)))
      ;; Actualizar la columna "reconocida" a TRUE para el registro con menor costo
      (update-data-coordenadas "puestos_suministros" latitud longitud '("reconocida") '(1)  )
      
      ;; Continuar el recorrido usando la posición con el menor costo de los puestos de suministros
      
  (setf *partida* pos-menor-costo)


;; Procesar los puestos de suministros a una lista mas "amigable"
(let ((puestos-procesados (procesar-puestos-suministros *puestos-suministros*)))

;; Filtrar los puestos procesados basados en la distancia máxima
 (let ((puestos-filtrados (filtrar-puestos-procesados puestos-procesados *partida*  *dist-max-sin-suminis*)))
      ;; Imprimir los puestos filtrados
      (format t "Puestos filtrados por distancia:~%")
      (imprimir-puestos-suministros-filtrados puestos-filtrados)
    (terpri)  ;; Imprime una línea en blanco

    ;borra todos los registros de registros
    (delete-all-data "registros")


    (dotimes *destino* puestos-filtrados
        
      (recorrer-y-ejecutar pos-menor-costo))
      
      (empezar)))

))
))