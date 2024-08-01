



(defvar *resultado-global* '(nil))  ; Variable global para guardar el resultado
(defvar *destino* nil)  ; Variable global para guardar el destino

 (defun calculo-adyacente-menor (destino)
  "Busca la coordenada adyacente con el menor costo desde el destino."
  (let* ((destino-latitud (car destino))
         (destino-longitud (cdr destino))
         (costo-pos (list most-positive-fixnum nil)))  ; Inicializa con un valor alto
    (loop for i from -1 to 1
          do (loop for j from -1 to 1
                   unless (and (= i 0) (= j 0))  ; Excluye la posición central
                   do (let* ((nueva-pos (list (+ destino-latitud i) (+ destino-longitud j)))
                             (nueva-latitud (car nueva-pos))
                             (nueva-longitud (cdr nueva-pos))
                             (nuevo-costo (verificar-registro nueva-latitud nueva-longitud)))
                        (when (and nuevo-costo (< nuevo-costo (car costo-pos)))
                          (setf costo-pos (list nuevo-costo nueva-pos))))))
    costo-pos))

  
(defun insertar-en-registros-ptos-intermedios (costo latitud longitud reconocida origen destino)
  "Inserta un registro en la tabla `registros_ptos_intermedios`."
  (let* ((table "registros_ptos_intermedios")
         (columns '("costo" "latitud" "longitud" "reconocida" "origen" "destino"))
         (values (list costo latitud longitud reconocida origen destino)))
    (insert-data table columns values)))



  (defun ptos-intermedios ()
  "Persiste en la tabla `registros_ptos_intermedios` el camino por tramos."
  (let* ((actual *destino*)) ; Para no perder la referencia, desde la última posición
    (loop
      do (let* ((adyacente-menor (car (calculo-adyacente-menor actual)))
                (costo (car adyacente-menor))
                (punto (cadr adyacente-menor)))
          
          ;; Insertar el registro en la tabla
          (insertar-en-registros-ptos-intermedios costo (car punto) (cdr punto) 1 *partida* *destino*)
          
          ;; Actualizar la posición actual
          (setq actual punto))
      until (equal actual *partida*)))) ; Terminar cuando se llega a la partida


(defun retroceder (pos) ;verifica que haya llegado al final y pasa a insertar ptos intermedios
  "Cierra el programa si se ha llegado al destino."
  (when (equal pos *destino*)
    (progn
      (print "Terminó el tramo a:")
      (print *destino*)
      
      ;; Persistir los puntos intermedios y el costo del destino en destinos intermedios
      (ptos-intermedios)

      ; necesito borrar todos los puntos de la tabla registros
      
      ;; Termina el programa
      ;;(sb-ext:quit) ;;no termino el programa porque tengo que evaluar todos los tramos
      )))

(defun verificar-registro (latitud longitud);Verifica si existe un registro en la tabla registro con los valores dados de latitud y longitud. Devuelve el costo si existe, de lo contrario devuelve NIL.
  (let* ((query (format nil "SELECT costo FROM registros WHERE latitud = ~a AND longitud = ~a" latitud longitud))
         (result (execute-query query)))
    ;; Depuración: Imprime el resultado de la consulta
    ;;(format t "Resultado de la consulta: ~a~%" result)
    (if (null result)
        ( progn
          nil)  
        (let* ((first-row (first result))
               (costo (if first-row
                          (second first-row)
                          nil)))  ;; Manejo de caso donde first-row puede ser NIL
          ;; Depuración: Imprime el costo
          ;;  (format t "Costo cargado: ~a~%" costo)
          costo))))
 
(defun insertar-en-registros (latitud longitud costo)
  "Inserta datos en la tabla registros con los valores dados para latitud, longitud y costo."
  (insert-data "registros" 
               '("latitud" "longitud" "costo" "reconocida")  ; Columnas en la tabla
               (list latitud longitud costo nil))) ; Valores a insertar

(defun  obtener-posicion-menor-costo ();Obtiene la posición (latitud y longitud) del registro con el menor valor en la columna 'costo' de la tabla 'registros'.

  (let* ((consulta "SELECT latitud, longitud FROM registros WHERE reconocida IS NULL ORDER BY costo ASC LIMIT 1;")
         (resultado (execute-query consulta)))    
    (if (null resultado)
        (progn
          (print "No se encontraron registros.")
          nil)
        ;; Asumir que el resultado tiene al menos una fila y extraer la latitud y longitud
        (let* ((fila (first resultado))
               (latitud (second fila))  
               (longitud (fourth fila)))
          (list latitud longitud)))))

(defun  obtener-posicion-menor-costo-suministros ();Obtiene la posición (latitud y longitud) del registro con el menor valor en la columna 'costo' de la tabla 'puestos_suministros'.


  (let* ((consulta "SELECT latitud, longitud FROM puestos_suministros WHERE reconocida IS NULL and costo is not null ORDER BY costo ASC LIMIT 1;")
         (resultado (execute-query consulta)))    
    (if (null resultado)
        (progn
          (print "No se encontraron registros en puestos_suministros.")
          nil)
        ;; Asumir que el resultado tiene al menos una fila y extraer la latitud y longitud
        (let* ((fila (first resultado))
               (latitud (second fila))  
               (longitud (fourth fila)))
          (list latitud longitud)))))


(defun asegurar-numeros (valor);Convierte el tipo de dato a tipo número, si no lo es.
  (cond ((numberp valor) valor)
        ((stringp valor) (parse-integer valor))
        (t (error "El valor no es un número: ~a" valor))))

  (defun round-to-decimals (number)
    "Redondea NUMBER a un número específico de decimales."
    (let* ((factor (expt 10 2))
          (scaled-number (* number factor)))      
      (/ (round scaled-number) factor)))  
  
  
  
  (defun recorrer-y-ejecutar (pos)       #| FUNCION PRINCIPAL DE ANALISIS DEL CAMINO |#
    "Función recursiva que recorre posiciones y ejecuta cálculos."
    (let* (   
            (pos_latitud (car pos)) 
            (pos_longitud (car(cdr pos)))  
            )        

          (let* ((calculo-pos (verificar-registro pos_latitud pos_longitud)))

      ;; Bucle para explorar posiciones adyacentes
      (loop for i from -1 to 1
            do (loop for j from -1 to 1
                    unless (and (= i 0) (= j 0))
                        do (let* ((nueva-pos (list(+ pos_latitud i) (+ pos_longitud j)))
                              (calculo-nueva-pos (calculo_costo_total nueva-pos)) 
                              (nueva-latitud (car nueva-pos))
                              (nueva-longitud (car(cdr nueva-pos)))
                              (calculo-sin_redondear (+ calculo-pos calculo-nueva-pos))
                              (calculo-total (round-to-decimals calculo-sin_redondear))
                              (costo (verificar-registro nueva-latitud nueva-longitud)
                              ))

                      ;; Evaluar y ejecutar las acciones condicionales
                      ;; Verifica si se ha llegado al destino
                     
                          (retroceder pos)

                      (progn
                        ;; Si no existe el registro, inserta con el costo calculado
                        (if (null costo)
                            (insertar-en-registros nueva-latitud nueva-longitud calculo-total)

                            ;; Si el registro existe, actualiza si el nuevo costo es menor
                            
                            (when (< calculo-total costo)
                              (update-data-coordenadas "registros" nueva-latitud nueva-longitud '("costo") (list (float calculo-total)))))
                          )
                          )))
            ))
  )
      


  