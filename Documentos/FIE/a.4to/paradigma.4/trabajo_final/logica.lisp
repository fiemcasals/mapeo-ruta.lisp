;; Declarar la variable global *puestos-suministros*
(defvar *puestos-suministros* nil
  "Lista de todos los puestos de suministros obtenidos de la base de datos.")

#|tengo que reveer estas lineas de la conexion porque ya estan seteadas en finalpp4 |#
;; Definir la conexión a la base de datos
(defvar *db-connection* nil
  "Conexión a la base de datos.")

;; Ejecuta una consulta SQL y devuelve todos los resultados en una lista
(defun execute-query (query)
  "Ejecuta la consulta SQL proporcionada y devuelve todos los resultados en una lista."
  (let ((results '()))
    (when *db-connection*
      (let ((stmt (cl-dbi:prepare *db-connection* query)))
        (cl-dbi:execute stmt)
        (loop for row = (cl-dbi:fetch stmt) while row do
              (push (parse-row row) results))
        (reverse results)))))

;; Convierte una fila en formato adecuado para la aplicación
(defun parse-row (row)
  "Convierte una fila en formato adecuado para la aplicación."
  ;;si no me equivoco, en esta parte separa los valores levantados por comas
  (if (stringp row)
      (split-sequence:split-sequence #\, row)
      row))

#| PRIMER PASO - OBTENER LOS PUESTOS DE SUMINISTROS |#

;; Genera la lista de puestos de suministros desde la base de datos
(defun generar-lista-ptos-suministros ()
  "Ejecuta la consulta para obtener todos los puestos de suministros y almacena los resultados en una variable."
  (let ((query "SELECT id, nombre, latitud, longitud FROM puestos_suministros"))
    (setf *puestos-suministros* (execute-query query))))

;; Imprime los puestos de suministros filtrados
(defun imprimir-puestos-suministros-filtrados (puestos-suministros)
  "Imprime los puestos de suministros almacenados en la lista proporcionada."
  (format t "Puestos de suministros almacenados:~%")
  (dolist (puesto puestos-suministros)
    (format t "ID: ~a, Nombre: ~a, Latitud: ~a, Longitud: ~a~%"
            (first puesto)
            (second puesto)
            (third puesto)
            (fourth puesto))))

;; Imprime los puestos de suministros almacenados en la lista proporcionada
(defun imprimir-puestos-suministros (puestos-suministros)
  "Imprime los puestos de suministros almacenados en la lista proporcionada."
  (format t "Puestos de suministros almacenados:~%")
  (dolist (puesto puestos-suministros)
    (format t "ID: ~a, Nombre: ~a, Latitud: ~a, Longitud: ~a~%"
            (second puesto)
            (fourth puesto)
            (sixth puesto)
            (eighth puesto))))


#| SEGUNDO PASO: GUARDA LOS PUESTOS EN UNA LISTA "AMIGABLE" |#

    ;; Procesa los puestos de suministros y los guarda en una nueva lista
    (defun procesar-puestos-suministros (puestos-suministros)
    "Procesa los puestos de suministros almacenados en la lista proporcionada y los guarda en una nueva lista."
    (let ((puestos-procesados '()))
        (dolist (puesto puestos-suministros puestos-procesados)
        (let* ((id (second puesto))              ;; Extraer el ID
                (nombre (fourth puesto))         ;; Extraer el Nombre
                (latitud (sixth puesto))         ;; Extraer la Latitud
                (longitud (eighth puesto))       ;; Extraer la Longitud
                ;; Agregar los datos procesados a la lista
                (nueva-fila (list id nombre latitud longitud)))
            (push nueva-fila puestos-procesados)))
        ;; Retornar la lista con los datos procesados en orden
        (reverse puestos-procesados))) ;estos puestos procesados no es una variable global

;; Calcula la distancia euclidiana entre dos puntos dados en latitud y longitud
(defun calculate-distance (lat1 long1 lat2 long2)
  "Calcula la distancia euclidiana entre dos puntos dados en latitud y longitud."
  (sqrt (+ (expt (- lat1 lat2) 2) (expt (- long1 long2) 2))))

#| TERCER PASO - FILTRA LOS PUNTOS DE SUMINISTROS CERCANOS |#

;; Filtra los puestos de suministros que están dentro de la distancia máxima desde el punto de referencia
(defun imprimir-tipo-y-dato (label valor)
  "Imprime el valor y el tipo de dato de un valor."
  (format t "~A: ~A (Tipo: ~A)~%" label valor (type-of valor)))

(defun verificar-y-imprimir-tipos (lat-actual long-actual lat-puesto long-puesto)
  "Verifica el tipo de los parámetros y los imprime si no son REAL."
  (when (or (not (realp lat-actual))
            (not (realp long-actual))
            (not (realp lat-puesto))
            (not (realp long-puesto)))
    (format t "Advertencia: Uno o más parámetros no son del tipo esperado REAL.~%")
    (imprimir-tipo-y-dato "Latitud actual" lat-actual)
    (imprimir-tipo-y-dato "Longitud actual" long-actual)
    (imprimir-tipo-y-dato "Latitud del puesto" lat-puesto)
    (imprimir-tipo-y-dato "Longitud del puesto" long-puesto)))

(defun parse-float (string)
  "Convierte una cadena de texto a un número flotante.
   Si la conversión falla, retorna NIL."
  (ignore-errors
    (let ((value (parse-integer string :junk-allowed t)))
      (if (and value (not (string= string (princ-to-string value))))
          (parse-float string)
          value))))


    (defun filtrar-puestos-procesados (puestos-procesados posicion distancia-max)
    "Filtra los puestos de suministros que están dentro de la distancia máxima desde el punto de referencia."
    

    (let* ((lat-actual (first posicion))  ;; Latitud del punto de referencia
            (long-actual (second posicion)) ;; Longitud del punto de referencia
            (puestos-filtrados '()))       ;; Lista para almacenar los puestos filtrados

        ;; Procesar los puestos de suministros
        (dolist (puesto puestos-procesados puestos-filtrados)
        (let* ((id (first puesto))                 ;; Extraer ID
                (nombre (second puesto))             ;; Extraer Nombre
                (lat-puesto (third puesto))          ;; Extraer Latitud del puesto
                (long-puesto (fourth puesto))        ;; Extraer Longitud del puesto
                (distancia-max (parse-float distancia-max)) ;; Convertir distancia-max a REAL

                ;; Verificar y convertir a REAL si es necesario
                (lat-puesto (if (numberp lat-puesto) (float lat-puesto) nil))
                (long-puesto (if (numberp long-puesto) (float long-puesto) nil)))

            ;; Verificar y imprimir tipos
           ;; (verificar-y-imprimir-tipos lat-actual long-actual lat-puesto long-puesto)
            
            ;; Calcular distancia
            (let ((distancia (calculate-distance lat-actual long-actual lat-puesto long-puesto)))   
            
            ;; Verificar si la distancia está dentro del rango permitido
            (when (and lat-puesto long-puesto (not (null distancia))
                        (< distancia distancia-max))
                (push (list id nombre lat-puesto long-puesto) puestos-filtrados)
               ))))

        ;; Retornar la lista con los puestos filtrados en orden
         puestos-filtrados))


#| FUNCION PARA IMPRIMIR UNA LISTA CUALQUIERA |#

(defun imprimir-lista (lista)
  "Imprime los elementos de la lista proporcionada."
  (format t "Lista:~%")
  (dolist (elemento lista)
    (format t "~a~%" elemento))
    
    (terpri)  );; Imprime una línea en blanco