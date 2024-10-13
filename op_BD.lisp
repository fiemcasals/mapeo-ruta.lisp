#| CONEXION CON LA BASE DE DATOS |#


;; Parámetros de conexión a la base de datos
(defparameter *db-params*
  '(:mysql
    :database-name "mi_base_de_datos"
    :username "mi_usuario"
    :password "mi_contraseña"
    :host "localhost"
    :port 3306))

;; Variable global para la conexión
(defparameter *db-connection* nil)

(defun inicializar-conexion ()
  ;Establece una conexión a la base de datos y la almacena en *db-connection*.
  (setf *db-connection* (apply #'cl-dbi:connect *db-params*)))


#| OPERACIONES CON LA BASE DE DATOS |#

                #| DEFINO TODAS LAS FUNCIONES QUE ACTUAN SOBRE LA BD |#

  (defun execute-query-single (query)
  "Ejecuta una consulta SQL y devuelve la primera fila del resultado."
  (let ((stmt (cl-dbi:prepare *db-connection* query))
        (result nil))
    ;; Ejecutar la consulta
    (cl-dbi:execute stmt)
    ;; Obtener la primera fila del resultado
    (setf result (cl-dbi:fetch stmt))
    ;; Devolver el resultado
    result))
  
(defun execute-delete-queries ()
  "Ejecuta una serie de consultas SQL DELETE, actualiza el campo 'reconocida' y borra el 'costo' en la tabla 'puestos_suministros'."
  (dolist (query '("DELETE FROM registros_ptos_intermedios;"
                   "DELETE FROM registros;"
                   "DELETE FROM puestos_suministros WHERE nombre = 'partida';"
                   "DELETE FROM puestos_suministros WHERE nombre = 'destino';"
                   "DELETE FROM ruta;"
                   "UPDATE puestos_suministros SET reconocida = NULL;"
                   "UPDATE puestos_suministros SET costo = NULL;"))
    (let ((stmt (cl-dbi:prepare *db-connection* query)))
      (cl-dbi:execute stmt)))
  ;; Devolver un mensaje indicando que las consultas fueron ejecutadas
  (format t "Las consultas DELETE y UPDATE fueron ejecutadas exitosamente, incluyendo el campo 'costo'."))



      (defun execute-query (query &rest params) 
      ;Ejecuta una consulta SQL con parámetros y devuelve todos los resultados en una lista.
      (let* ((formatted-query (apply #'format nil query params))
            (stmt (cl-dbi:prepare *db-connection* formatted-query))
            (results '())) ;; Lista para almacenar los resultados
        ;; Ejecutar la consulta
        (cl-dbi:execute stmt)
        ;; Iterar sobre los resultados y almacenarlos en la lista
        (loop for row = (cl-dbi:fetch stmt) while row do
          (push row results))
        ;; Devolver la lista de resultados en orden
        (reverse results)))

      (defun insert-data (table columns values)
    ;Inserta datos en la tabla especificada con columnas y valores proporcionados.
    ;Utiliza parámetros para prevenir inyección SQL.
    (let* ((placeholders (make-list (length values) :initial-element "?"))
          (query (format nil "INSERT INTO ~a (~{~a~^, ~}) VALUES (~{~a~^, ~})"
                          table columns placeholders))
          (prepared-query (cl-dbi:prepare *db-connection* query)))
      ;; Asegúrate de que `values` es una lista
      (cl-dbi:execute prepared-query values)))


(defun insert-data-ruta (table columns values)
  ;; Inserta datos en la tabla especificada con columnas y valores proporcionados.
  (let* ((placeholders (mapconcat (lambda (x) "?") columns ", "))
         (query (format nil "INSERT INTO ~a (~{~a~^, ~}) VALUES (~a)"
                        table columns placeholders))
         (prepared-query (cl-dbi:prepare *db-connection* query)))
    ;; Asegúrate de que `values` es una lista
    (cl-dbi:execute prepared-query values)))



(defun update-data-coordenadas (table latitud longitud columns values)
  "Actualiza los datos en la tabla especificada usando latitud y longitud para la condición."
  (let* ((set-clause (mapcar
                      (lambda (col val)
                        (if (eql val 'NULL)
                            (format nil "~a = NULL" col)
                            (format nil "~a = '~a'" col val)))
                      columns values))
         (query (format nil "UPDATE ~a SET ~{~a~^, ~} WHERE latitud = ~a AND longitud = ~a"
                        table set-clause latitud longitud)))
    (cl-dbi:execute (cl-dbi:prepare *db-connection* query))))


  (defun update-data (table id columns values)
      ;; Preparar la consulta de actualización
      (let* ((set-clause (mapcar (lambda (col val) (format nil "~a = '~a'" col val)) columns values))
            (query (format nil "UPDATE ~a SET ~{~a~^, ~} WHERE id = ~a"
                            table set-clause id)))
        (cl-dbi:execute (cl-dbi:prepare *db-connection* query))))

    (defun delete-data (table id)
          ;; Preparar la consulta de eliminación
        (let ((query (format nil "DELETE FROM ~a WHERE id = ~a" table id)))
          (cl-dbi:execute (cl-dbi:prepare *db-connection* query))))

  (defun delete-all-data (table)
  "Elimina todos los registros de la tabla especificada."
  ;; Preparar la consulta de eliminación
  (let ((query (format nil "DELETE FROM ~a" table)))
    (cl-dbi:execute (cl-dbi:prepare *db-connection* query))))

    (defun delete-patida (table)
  "Elimina todos los registros de partida de la tabla especificada."
  ;; Preparar la consulta de eliminación
    (let* ((query (format nil "DELETE FROM ~a WHERE nombre = 'partida'" table)))
    (cl-dbi:execute (cl-dbi:prepare *db-connection* query))))


(defun cerrar-conexion ()
  "Cierra la conexión a la base de datos."
  (when *db-connection*
    (cl-dbi:disconnect *db-connection*)
    (setf *db-connection* nil)))
