
(defvar *destino* nil)  ; Variable global para guardar el destino
(defvar *partida* nil)  ; Variable global para guardar el partida
(defvar *puestos-filtrados* nil)  ; Variable global para guardar el puestos filtrados
(defvar *destino_final* nil)
(defvar *partida_inicial* nil)

 (defun calculo-adyacente-menor (destino)
  "Busca la coordenada adyacente con el menor costo desde el destino."


  (let* ((destino-latitud (car destino))
         (destino-longitud (car(cdr destino)))
         (costo-pos (list most-positive-fixnum nil)))  ; Inicializa con un valor alto
        
    (loop for i from -1 to 1
          do (loop for j from -1 to 1
                   unless (and (= i 0) (= j 0))  ; Excluye la posición central
                   do (let* ((nueva-pos (list (+ destino-latitud i) (+ destino-longitud j)))
                             (nueva-latitud (car nueva-pos))
                             (nueva-longitud (car(cdr nueva-pos)))
                             (nuevo-costo (verificar-registro nueva-latitud nueva-longitud))
                             ;(nuevo-costo (car(cdr(car result))))
                            )
                           
                        (when (and nuevo-costo (< nuevo-costo (car costo-pos)))
                          (setf costo-pos (list nuevo-costo nueva-pos))
                        )
                          )))
    costo-pos))   


(defun insertar-en-registros-ptos-intermedios (costo latitud longitud #|reconocida|# origen destino)
 
  
  ;; Extraer los valores de origen y destino si son listas CONS
  (let* ((origen-lat (if (and (listp origen) (= (length origen) 2))
                         (float (car origen)) ; Extrae la latitud
                         (float origen))) ; Si no es lista, usa el valor directamente
         (origen-lng (if (and (listp origen) (= (length origen) 2))
                         (float (cadr origen)) ; Extrae la longitud
                         0.0)) ; Si no es lista, usa un valor predeterminado
         (destino-lat (if (and (listp destino) (= (length destino) 2))
                          (float (car destino)) ; Extrae la latitud
                          (float destino))) ; Si no es lista, usa el valor directamente
         (destino-lng (if (and (listp destino) (= (length destino) 2))
                          (float (cadr destino)) ; Extrae la longitud
                          0.0)) ; Si no es lista, usa un valor predeterminado
         (table "registros_ptos_intermedios")
         (columns '("costo" "latitud" "longitud" #| "reconocida" |# "origen_lat" "origen_lng" "destino_lat" "destino_lng"))
         (values (list costo latitud longitud #|reconocida|# origen-lat origen-lng destino-lat destino-lng)))


    (insert-data table columns values)))



(defun convertir-a-decimales (valor)
  "Convierte un valor a una lista de números decimales."
  (if (listp valor)
      (mapcar (lambda (x) (float x)) valor)
      (float valor)))

(defun ptos-intermedios ()
  "Persiste en la tabla `registros_ptos_intermedios` el camino por tramos."
  (let* ((actual (convertir-a-decimales *destino*)) ; Convertir *destino* a formato decimal
         (partida (convertir-a-decimales *partida*))) ; Convertir *partida* a formato decimal
    
    
    (loop
      do (let* ((adyacente-menor (calculo-adyacente-menor actual))
                (costo (car adyacente-menor)) ; Extraer el costo directamente
                (punto (cadr adyacente-menor))
                (latitud (car punto)) ; Extraer la latitud
                (longitud (car (cdr punto))) ; Extraer la longitud
                ;(reconocida 1) ; Valor numérico, no lista;   ESTA AL PEDO PORQUE VA SIEMPRE DESDE UNO CON MAYOR COSTO
                )
    
          ;; Insertar el registro en la tabla
          (insertar-en-registros-ptos-intermedios costo latitud longitud #|reconocida|# *partida* *destino*)
                  
          ;; Actualizar la posición actual  
          (setq actual (convertir-a-decimales punto)) ; Convertir punto a formato decimal
          )

      until (equal actual partida)))) ; Terminar cuando se llega a la partida




(defun convertir-a-float (valor)
  "Convierte VALOR a float si no lo es."
  (if (numberp valor)
      (float valor)
      (parse-float valor)))


  
  
(defun retroceder (pos calculo-total) ;verifica que haya llegado al final y pasa a insertar ptos intermedios
  
    
      (let* (
         (lat-pos (convertir-a-float (car pos)))
         (lon-pos (convertir-a-float (second pos)))
         (lat-dest (convertir-a-float (car *destino*)))
         (lon-dest (convertir-a-float (second *destino*)))
        )

    (when (and(= lat-dest lat-pos)(= lon-dest lon-pos))
    (progn
      (print "##############Terminó el tramo a:############################")
      (print *destino*)
      (print "#############################################################")   

      (if (or (not (verificar-registro-de-suministros lat-dest lon-dest)) ; Verifica si el registro no existe
              (< calculo-total (verificar-registro-de-suministros lat-dest lon-dest))) ; Verifica si el nuevo costo es menor

      (update-data-coordenadas "puestos_suministros" lat-dest lon-dest                   
  '("costo" "reconocida") (list (float calculo-total) 'NULL)) 

)   
      ;; Persistir los puntos intermedios y el costo del destino en destinos intermedios
      (ptos-intermedios)

                                    #| ALERTA |#
      
      
      ;antes de volver a empezar. tengo que asegurarme de que los puntos de suministros estan cargados con un valor. distinto de null. para poder ser levantados

        ;borra todos los registros de registros
        (delete-all-data "registros")
        (print "se borraron todos los registros")

           

        (setf *destino* (cdr (cdr (car *puestos-filtrados*))))
        (setf *puestos-filtrados* (cdr *puestos-filtrados*))

        (print "cambio el destino dentro del mismo punto de partida por")
        (print *destino*) 


      (if *destino*
    (progn
      ;; Insertar en registros y recorrer
      (insertar-en-registros 
        (car *partida*) 
        (cadr *partida*) 
        (verificar-registro-de-suministros 
          (car *partida*) 
          (cadr *partida*)))
      (recorrer-y-ejecutar *partida*))
    
    ;; Si no hay destino, actualiza la tabla y procede
    (progn
    (setq *partida* (obtener-posicion-menor-costo-suministros))
      ; Actualizar registros y empezar nuevamente
      (insertar-en-registros 
        (car *partida*) 
        (cadr *partida*) 
        (verificar-registro-de-suministros 
          (car *partida*) 
          (cadr *partida*)))
      
      
      ;; Verificar si la nueva partida es el destino final
      (if (equal *partida* *destino_final*)
          (progn
          (print "se llego al final")   
          (entregar-ruta *destino_final*)
          (sb-ext:exit)) ;; Usa 'sb-ext:exit' para terminar el programa
      (empezar))
      
      )) 

      ))))

(defun verificar-registro (latitud longitud);Verifica si existe un registro en la tabla registro con los valores dados de latitud y longitud. Devuelve el costo si existe, de lo contrario devuelve NIL.
  (let* ((query (format nil "SELECT costo FROM registros WHERE latitud = ~a AND longitud = ~a" latitud longitud))
         (result (execute-query query)))
    ;; Depuración: Imprime el resultado de la consulta
    ;;(format t "Resultado de la consulta: ~a~%" (car(cdr(car result))))


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

(defun verificar-registro-de-suministros (latitud longitud);Verifica si existe un registro en la tabla registro con los valores dados de latitud y longitud. Devuelve el costo si existe, de lo contrario devuelve NIL.
  (let* ((query (format nil "SELECT costo FROM puestos_suministros WHERE latitud = ~a AND longitud = ~a" latitud longitud))
         (result (execute-query query)))
    ;; Depuración: Imprime el resultado de la consulta
    ;;(format t "Resultado de la consulta: ~a~%" (car(cdr(car result))))


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

    (defun pos_menor ()
            
      (let* ((consulta "SELECT latitud, longitud FROM registros WHERE reconocida IS NULL ORDER BY costo ASC LIMIT 1;")
            (resultado (execute-query-single consulta)))
        
        (if (null resultado)
            (progn
              (print "No se encontraron registros.")
              nil)
            ;; Asumir que el resultado tiene al menos una fila y extraer la latitud y longitud
            (let* ((latitud (second resultado))
                  (longitud (fourth resultado)))
              
              (list latitud longitud))))) 

                        
          
  


  (defun  obtener-posicion-menor-costo-suministros ();Obtiene la posición (latitud y longitud) del registro con el menor valor en la columna 'costo' de la tabla 'puestos_suministros'.


    (let* ((consulta "SELECT latitud, longitud FROM puestos_suministros WHERE reconocida IS NULL and costo is not NULL ORDER BY costo ASC LIMIT 1;")      
          (resultado (execute-query-single consulta))) 

      (if (null resultado)
          (progn  
            (print "No se encontraron registros en puestos_suministros.")
            nil)
          (let* ((latitud (car(cdr resultado)))  
                (longitud (car (cdr(cdr (cdr resultado)))))
                )
            
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
      (let* (
              (calculo-pos (verificar-registro pos_latitud pos_longitud))
            )

        ;; Bucle para explorar posiciones adyacentes
        (loop for i from -1 to 1
              do (loop for j from -1 to 1
                      unless (and (= i 0) (= j 0))
                          do (let* ((nueva-pos (list(+ pos_latitud i) (+ pos_longitud j)))
                                    (calculo-nueva-pos (calculo_costo_total nueva-pos)) 
                                    (nueva-latitud (car nueva-pos))
                                    (nueva-longitud (car(cdr nueva-pos)))
                                    #|(calculo-sin_redondear (if calculo-pos
                                                (+ calculo-pos calculo-nueva-pos)
                                                calculo-nueva-pos))|#
                                    (calculo-sin_redondear (+ calculo-pos calculo-nueva-pos))
                                    (calculo-total (round-to-decimals calculo-sin_redondear))
                                    (costo (verificar-registro nueva-latitud nueva-longitud))
                                   )
                        (progn
                          ;; Si no existe el registro, inserta con el costo calculado
                          (if (null costo)
                                                           
                              (insertar-en-registros nueva-latitud nueva-longitud calculo-total)  

                              ;; Si el registro existe, actualiza si el nuevo costo es menor
                             
                              (when (< calculo-total costo)
                                ( -data-coordenadas "registros" nueva-latitud nueva-longitud '("costo") (list (float calculo-total)))
                              )
                          )
                            )
                                     ;; Evaluar y ejecutar las acciones condicionales
                                            ;; Verifica si se ha llegado al destino
                                     
                                            (retroceder nueva-pos calculo-total)
                            ))
                            
                            );termina el loop
              ))
              (let* ((pos-menor (pos_menor)))

  ;Inserta '1' en reconocido en la tabla 'registros'
(update-data-coordenadas "registros" (car pos-menor) (cdr pos-menor) '("reconocida") '(1))(recorrer-y-ejecutar pos-menor)          
    
    ))  


(defun entregar-ruta (pos)
  (print "se procede a imprimir la ruta desde el destino hasta el punto inicial")
  
  (let* ((pos_latitud (car pos))   ;; Extrae la latitud de la posición actual
         (pos_longitud (cadr pos)) ;; Extrae la longitud de la posición actual
         (referente_costo 88888)   ;; Costo inicial de referencia
         (referente_pos '()))     ;; Posición de referencia inicial
    
    ;; Bucle para explorar posiciones adyacentes
    (loop for i from -1 to 1
          do (loop for j from -1 to 1
                   unless (and (= i 0) (= j 0)) ;; Excluye la posición actual
                   do (let* ((nueva-pos (list (+ pos_latitud i) (+ pos_longitud j))) ;; Calcula nueva posición
                             (nueva-latitud (car nueva-pos)) ;; Extrae latitud de nueva posición
                             (nueva-longitud (cadr nueva-pos)) ;; Extrae longitud de nueva posición
                             (costo (verificar-registro nueva-latitud nueva-longitud))) ;; Verifica el costo
                      (when (and (not (null costo)) (< costo referente_costo)) ;; Si el costo es menor
                        (setf referente_costo costo)  ;; Actualiza el costo de referencia
                        (setf referente_pos nueva-pos) ;; Actualiza la posición de referencia
                        ;; Inserta la nueva coordenada y su costo en la base de datos
                        (print "se procede a cargar una coordenada en la tabla ruta")
                        (update-data-coordenadas "ruta" nueva-latitud nueva-longitud
                         costo)))))

    
    
    ;; Continúa si la posición de referencia no es igual a la partida inicial
    (if (not (equal referente_pos *partida_inicial*))
        (entregar-ruta referente_pos)))) ;; Llama recursivamente a entregar-ruta

