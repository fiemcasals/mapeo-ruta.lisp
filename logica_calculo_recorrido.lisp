
(defvar *destino* nil)  ; Variable global para guardar el destino
(defvar *partida* nil)  ; Variable global para guardar el partida
(defvar *puestos-filtrados* nil)  ; Variable global para guardar el puestos filtrados
(defvar *destino_final* nil)
(defvar *partida_inicial* nil)

 """(defun calculo-adyacente-menor (destino)
  


  (let* ((destino-latitud (car destino))
         (destino-longitud (car(cdr destino)))
         (costo-pos (list most-positive-fixnum nil)))  ; Inicializa con un valor alto, y una posicion nil -> se espera que sea una variable pivote que va a tomar distintas posiciones con su valor correspondiente
        
    (loop for i from -1 to 1
          do (loop for j from -1 to 1
                   unless (and (= i 0) (= j 0))  ; Excluye la posición central
                   do (let* ((nueva-pos (list (+ destino-latitud i) (+ destino-longitud j)))
                             (nueva-latitud (car nueva-pos))
                             (nueva-longitud (car(cdr nueva-pos)))
                             (nuevo-costo (verificar-registro nueva-latitud nueva-longitud))
                            )
                           
                        (when (and nuevo-costo (< nuevo-costo (car costo-pos)))
                          (setf costo-pos (list nuevo-costo nueva-pos))
                        )
                          )))
    costo-pos))  """ 

(defun calculo-adyacente-menor (destino)
  "Busca la coordenada adyacente con el menor costo desde el destino. 
   Si el destino es nil, reporta un error y retorna nil."
  (if (null destino)
      (progn
        (print "No hay un lugar claro de destino.")
        nil)  ; Retorna nil si el destino es nil
      (let* ((destino-latitud (car destino))
             (destino-longitud (car (cdr destino)))
             (costo-pos (list most-positive-fixnum nil)))  ; Inicializa con un valor alto y posición nil

        (loop for i from -1 to 1
              do (loop for j from -1 to 1
                       unless (and (= i 0) (= j 0))  ; Excluye la posición central
                       do (let* ((nueva-pos (list (+ destino-latitud i) (+ destino-longitud j)))
                                  (nueva-latitud (car nueva-pos))
                                  (nueva-longitud (car (cdr nueva-pos)))
                                  (nuevo-costo (verificar-registro nueva-latitud nueva-longitud)))
                           
                            (when (and nuevo-costo (< nuevo-costo (car costo-pos)))
                              (setf costo-pos (list nuevo-costo nueva-pos))))))
        costo-pos)))  ; Retorna el costo más bajo encontrado


(defun calculo-adyacente-menor-ptos-intermedios (destino)
  "Busca la coordenada adyacente con el menor costo desde el destino. Es decir, va buscando desde el destino hacia el punto de partida"


  (let* ((destino-latitud (car destino))
         (destino-longitud (car(cdr destino)))
         (costo-pos (list most-positive-fixnum nil)))  ; Inicializa con un valor alto, y una posicion nil -> se espera que sea una variable pivote que va a tomar distintas posiciones con su valor correspondiente
        
    (loop for i from -1 to 1
          do (loop for j from -1 to 1
                   unless (and (= i 0) (= j 0))  ; Excluye la posición central
                   do (let* ((nueva-pos (list (+ destino-latitud i) (+ destino-longitud j)))
                             (nueva-latitud (car nueva-pos))
                             (nueva-longitud (car(cdr nueva-pos)))
                             (nuevo-costo (verificar-registro-ptos-intermedios nueva-latitud nueva-longitud))
                            )
                           
                       (format t "el valor de nuevo-costo es ~a~%" nuevo-costo)
                       (format t "para las coordenadas: ~a ~a~%" nueva-latitud nueva-longitud )
                        (when (and nuevo-costo (< nuevo-costo (car costo-pos)))
                          (setf costo-pos (list nuevo-costo nueva-pos))
                        )
                          )))
    (format t "el costo y posicion seleccionado fue ~a~%" costo-pos)
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


(defun insertar-en-registros-ptos-ruta (peso latitud longitud )

  (print peso)
  ;; Extraer los valores de origen y destino si son listas CONS
  (let* ((table "ruta")
       (columns '("peso" "latitud" "longitud"))
       (values (list peso latitud longitud))) 
  (insert-data table columns values))
)  


(defun convertir-a-decimales (valor)
  "Convierte un valor a una lista de números decimales."
  (if (listp valor)
      (mapcar (lambda (x) (float x)) valor)
      (float valor)))

(defun  ptos-intermedios ()
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
    (format t "el nuevo punto de partida seleccionado es ~a~%" *partida*)
    (if (equal *partida* *destino_final*)
          (progn
          (print "se llego al final:")   
          (entregar-ruta)
          (sb-ext:exit)) ;; Usa 'sb-ext:exit' para terminar el programa
      (progn
      ; Actualizar registros y empezar nuevamente
      (insertar-en-registros 
        (car *partida*) 
        (cadr *partida*) 
        (verificar-registro-de-suministros ;;este punto seguro te da el costo del pto
          (car *partida*) 
          (cadr *partida*)))
      
      
      (empezar))
      
      ))

      )))))

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

(defun verificar-registro-ptos-intermedios (latitud longitud);Verifica si existe un registro en la tabla registro con los valores dados de latitud y longitud. Devuelve el costo si existe, de lo contrario devuelve NIL.
  (let* ((query (format nil "SELECT costo FROM registros_ptos_intermedios WHERE latitud = ~a AND longitud = ~a" latitud longitud))
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
                                                           
                              (insertar-en-registros nueva-latitud nueva-longitud calculo-total)  )

                              ;; Si el registro existe, actualiza si el nuevo costo es menor
                             
                          (when (and (not(null costo)) (< calculo-total costo))
                                ( -data-coordenadas "registros" nueva-latitud nueva-longitud '("costo") (list (float calculo-total)))
                              )
                          )
                            
                                     ;; Evaluar y ejecutar las acciones condicionales
                                            ;; Verifica si se ha llegado al destino
                                     
                                            (retroceder nueva-pos calculo-total)
                            )))
                            
                            );termina el loop
              )

(let* ((pos-menor (pos_menor)))

  ;Inserta '1' en reconocido en la tabla 'registros'
(update-data-coordenadas "registros" (car pos-menor) (cdr pos-menor) '("reconocida") '(1))
(recorrer-y-ejecutar pos-menor)            
) 
)

(defun entregar-ruta ()
  (print "entregando ruta~%")
  "Persiste en la tabla `registros_ptos_intermedios` el camino por tramos."
  (let* ((actual (convertir-a-decimales *destino_final*)) ; Convertir *destino* a formato decimal
         (partida (convertir-a-decimales *partida_inicial*))) ; Convertir *partida* a formato decimal

    ;; Verificar que ambos, actual y partida, no sean NIL antes de continuar
    (when (and (not (null actual)) (not (null partida)))
      (loop until (equal actual partida)
            do (let* ((adyacente-menor (calculo-adyacente-menor-ptos-intermedios actual)))
                 (if (cadr adyacente-menor) ; Validar que no sea NIL
                     (let* ((peso (car adyacente-menor)) ; Extraer el costo directamente
                            (punto (cadr adyacente-menor))
                            (latitud (car punto)) ; Extraer la latitud
                            (longitud (cadr punto))) ; Extraer la longitud

                        (format t "se va a insertar en ruta el peso ~a, la latitud ~a y su longitud ~a~%" peso latitud longitud)
                        ;Insertar el registro en la tabla
                        (insertar-en-registros-ptos-ruta (round peso) latitud longitud)

                       ;; Actualizar la posición actual  
                       (setq actual (convertir-a-decimales punto)) ; Convertir punto a formato decimal
                       
                       (print adyacente-menor)
                       (print punto))
                     
                   ;; Manejar el caso cuando adyacente-menor es NIL
                   (progn
                     (print "No hay adyacentes disponibles, no se pudo llegar exitosamente a destino.")
                     (return))))))))

