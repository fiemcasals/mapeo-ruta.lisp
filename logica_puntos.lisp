
(defvar *partida* '()   
  "Partida.") 
(defvar *destino* '()   
  "Destino.") 
(defvar *puestos-filtrados* '()
    "puestos-filtrados.")


      #| DA INICIO AL PROGRAMA BUSCANDO LA POSICION DE MENOR VALOR -> OSEA ES EL INICIO |#

      #| ACA ES DONDE ME TENGO QUE PARAR PARA LOGRAR EL INICIO EN CADA PUNTO DE SUMINISTROS PORTERIORMENTE AL PUNTO DE PARTIDA |#


(defun empezar ()
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
  (print "se cargo como partida a la coordenada:")
  (print *partida*) 

(print "se va a procesar los puestos de puestos_suministros")
;; Procesar los puestos de suministros a una lista mas "amigable"
(let ((puestos-procesados (procesar-puestos-suministros *puestos-suministros*)))

;; Filtrar los puestos procesados basados en la distancia máxima
 (let ((puestos-filtrados (filtrar-puestos-procesados puestos-procesados *partida*  *dist-max-sin-suminis*)))
      ;; Imprimir los puestos filtrados
    (terpri)  ;; Imprime una línea en blanco
      (format t "Puestos filtrados por distancia:~%")
      (imprimir-puestos-suministros-filtrados puestos-filtrados)
    (terpri)  ;; Imprime una línea en blanco



(setf *puestos-filtrados* puestos-filtrados)        

(setf *destino* (cdr (cdr (car *puestos-filtrados*))))
(setf *puestos-filtrados* (cdr *puestos-filtrados*))

  (print "se reporta nuevo destino")    
  (print *destino*)

(print *partida*)
          (recorrer-y-ejecutar *partida*)

(empezar)
      ))

))
))


    (empezar)