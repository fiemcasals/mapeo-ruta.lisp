                       #| EN FINAL: BD , DATOS INICIALES Y LOAD SCRIPT |#
                
                
                #| EMPIEZA LA CONEXION CON LA BASE DE DATOS |#

(defun expand-file-name (path)
  ;; Combina el path proporcionado con el directorio de inicio del usuario,
  ;; sustituyendo las barras invertidas (\) por barras normales (/).
  (merge-pathnames (substitute #\/ #\\ path)
                   (user-homedir-pathname)))

;; Cargar Quicklisp
(load (expand-file-name "~/quicklisp/setup.lisp"))

;; Cargar las bibliotecas requeridas
(ql:quickload :cl-dbi)


;; Importar los paquetes necesarios
(defpackage :my-app
  (:use :cl :cl-dbi)) 

(in-package :my-app)                   

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


                              #|CARGO CASI TODOS LOS SCRIPT |#

(load "calculo_costo.lisp")
(load "logica_calculo_recorrido.lisp")
(load "funciones_interactivas.lisp")
(load "logica_costo_camino.lisp")
(load "logica.lisp")

                #| DEFINO TODAS LAS FUNCI ONES QUE ACTUAN SOBRE LA BD |#

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

;; Inicializar conexión al inicio
(inicializar-conexion)
(load "logica_calculo_recorrido.lisp") 

  ;; Llamar a la función para ejecutar la consulta e imprimir los resultados
  ;; (execute-query "SELECT * FROM Eno")


;; Llamar a la función para actualizar datos
;;(update-data "Eno" 1 '("alcance" "magnitud" "latitud" "longitud") '(150 10 13 58))

;; Llamar a la función para eliminar datos
;;(delete-data "Eno" 12)


                      #| DEFINO Y SETEO LAS VARIABLES GLOBALES |#


;; Definir variable global
(defvar *magnitud_elem_ppio* nil
  "Magnitud del elemento propio.") 

;; Definir variable global
(defvar *dist-max-sin-suminis* nil
  "Distancia máxima sin suministros.")  

;; Llamar a la función y almacenar el resultado en la variable global
(setf *magnitud_elem_ppio* (solicitar-magnitud))

;; Llamar a la función y almacenar el resultado en la variable global
(setf *dist-max-sin-suminis* (dist-max-sin-sumin))

;; Definir variable global
(defvar *destino* nil
  "Destino.")  
(defvar *destino_final* nil
  "Destino final.") 

;; Llamar a la función y almacenar el resultado en la variable global
(format t "Ingrese las coordenadas de destino")
(terpri)  ;; Imprime una línea en blanco
(setf *destino* (solicitar-coordenadas))
(setf *destino_final* *destino*)


;; Definir variable global
(defvar *partida* '()   
  "Partida.")   

;; Llamar a la función y almacenar el resultado en la variable global
(format t "Ingrese las coordenadas de partida")
(delete-patida "puestos_suministros") 
(terpri)  ;; Imprime una línea en blanco
(delete-all-data "registros")   
(setq *partida* (solicitar-coordenadas))
;registro la partida tambien en la base de datos de suministros, para facilitar el proceso   
; (insert-data "registros" '("costo" "latitud" "longitud") (list 0 (car *partida*) (cdr *partida*)))
(insert-data "puestos_suministros" '("nombre" "latitud" "longitud" "costo") (list "partida" (car *partida*) (cdr *partida*) 0))
;incorporo el destino a la lista de puestos de suministros, para agregar un cierre
(insert-data "puestos_suministros" '("nombre" "latitud" "longitud" "costo") (list "destino" (car *destino_final*) (cdr *destino_final*) 9999))
(terpri)  ;; Imprime una línea en blanco



#| REGISTRO EL PUNTO DE PARTIDA EN REGISTROS PARA QUE PUEDA SER BUSCADO COMO PTO INICIAL |#

(let* ((latitud_partida (car *partida*))
       (longitud_partida (cdr *partida*))
       (costo_inicial 0)
      )
(insertar-en-registros latitud_partida longitud_partida costo_inicial))
(terpri)  ;; Imprime una línea en blanco


  #| IMPRIME LAS VARIABLES INGRESADAS, ANTES DE EMPEZAR A EJECUTAR LOS CALCULOS |#

;; Imprimir el valor de la variable global de distancia maxima sin suministros
(format t "Distancia máxima sin suministros ingresada: ~a~%" *dist-max-sin-suminis*)
(terpri)  ;; Imprime una línea en blanco

;; Imprimir el valor de la variable global de destino
(format t "Magnitud del elemento propio: ~a~%" *magnitud_elem_ppio*)
(terpri)  ;; Imprime una línea en blanco

; Imprimir el valor de la variable global de partida
(format t "Partida: ~a~%" *partida*)
(terpri)  ;; Imprime una línea en blanco

;; Imprimir el valor de la variable global de destino
(format t "Destino: ~a~%" *destino*)
(terpri)  ;; Imprime una línea en blanco

(let ((lista-puestos-suministro (puestos-suministros)))
  (format t "Los detalles de los puestos de suministros son:~%")
  (dolist (puesto lista-puestos-suministro)
    (format t "~a: Latitud: ~a, Longitud: ~a~%" 
            (first puesto)
            (first (second puesto))
            (second (second puesto)))
            (terpri)  ;; Imprime una línea en blanco
    ;; Procedo a guardar los puestos de suministros en la base de datos 
    (let ((nombre (first puesto))
           (latitud (first (second puesto)))
           (longitud (second (second puesto))))
        
            ;; Llamada a la función de inserción
            (insert-data "puestos_suministros" '("nombre" "latitud" "longitud") (list nombre latitud longitud)) 
    )))         

(terpri)  ;; Imprime una línea en blanco

(format t "Relevamiento de las zonas QBN:~%")
(let ((zonas (registrar-zonas)))
  ;; Verifica si hay zonas registradas
  (if zonas
      (format t "Detalles de las zonas registradas:~%")
    (format t "No hay zonas registradas.~%"))
  
  ;; Itera sobre cada zona en la lista de zonas
  (dolist (zona zonas)
    ;; Imprime los detalles de cada zona
    (format t "Nombre: ~a, Coordenadas: ~a, Alcance: ~a~%"
            (first zona)
            (second zona)
            (third zona))
            (terpri)  ;; Imprime una línea en blanco
    ;; Procedo a guardar las zonas de suministros en la base de datos 
    (let* ((nombre (first zona))
           (coordenadas (second zona))
           (alcance (third zona))
           ;; Extrae latitud y longitud de coordenadas
           (latitud (first coordenadas))
           (longitud (second coordenadas)))
      
      ;; Mensaje de depuración para verificar los valores y tipos
      (format t "Insertando: Nombre: ~a, Latitud: ~d, Longitud: ~d, Alcance: ~d~%"
              nombre latitud longitud alcance)
      
      ;; Llamada a la función de inserción
      (insert-data "qbn" '("nombre" "latitud" "longitud" "alcance")
                   (list nombre latitud longitud alcance)))))



;;de igual modo lo va a hacer con zonas de agua
(terpri)  ;; Imprime una línea en blanco
(format t "Relevamiento de las zonas inundadas:~%")
(let ((zonas (registrar-zonas)))
  ;; Verifica si hay zonas registradas
  (if zonas
      (format t "Detalles de las zonas registradas:~%")
    (format t "No hay zonas registradas.~%"))
  
  ;; Itera sobre cada zona en la lista de zonas
  (dolist (zona zonas)
    ;; Imprime los detalles de cada zona
    (format t "Nombre: ~a, Coordenadas: ~a, Alcance: ~a~%"
            (first zona)
            (second zona)
            (third zona))
    (terpri)  ;; Imprime una línea en blanco
    ;; Procedo a guardar las zonas de suministros en la base de datos 
    (let* ((nombre (first zona))
           (coordenadas (second zona))
           (alcance (third zona))
           ;; Extrae latitud y longitud de coordenadas
           (latitud (first coordenadas))
           (longitud (second coordenadas)))
      
      ;; Mensaje de depuración para verificar los valores y tipos
      (format t "Insertando: Nombre: ~a, Latitud: ~d, Longitud: ~d, Alcance: ~d~%"
              nombre latitud longitud alcance)
      (terpri)  ;; Imprime una línea en blanco
      ;; Llamada a la función de inserción
      (insert-data "Ag" '("nombre" "latitud" "longitud" "alcance")
                   (list nombre latitud longitud alcance)))))

;;eno
(terpri)  ;; Imprime una línea en blanco
(format t "Relevamiento de las unidades Enemigas:~%")
(let ((unidades (registrar-unidades)))
  ;; Verifica si hay unidades registradas
  (if unidades
      (format t "Detalles de las unidades registradas:~%")
    (format t "No hay unidades registradas.~%"))
  
  ;; Itera sobre cada unidad en la lista de unidades
  (dolist (unidad unidades)
    ;; Imprime los detalles de cada unidad
    (format t "Alcance: ~d, Magnitud: ~d, Latitud: ~d, Longitud: ~d~%"
            (first unidad)
            (second unidad)
            (third unidad)
            (fourth unidad))
    (terpri)  ;; Imprime una línea en blanco
    ;; Procedo a guardar las unidades en la base de datos 
    (let* ((alcance (first unidad))
           (magnitud (second unidad))
           (latitud (third unidad))
           (longitud (fourth unidad)))
      
      ;; Mensaje de depuración para verificar los valores y tipos
      (format t "Insertando: Alcance: ~d, Magnitud: ~d, Latitud: ~d, Longitud: ~d~%"
              alcance magnitud latitud longitud)
      
      ;; Llamada a la función de inserción
      (insert-data "Eno" '("alcance" "magnitud" "latitud" "longitud")
                   (list alcance magnitud latitud longitud)))))


;;refuerzos
(terpri)  ;; Imprime una línea en blanco
(format t "Relevamiento de los refuerzos:~%")
(let ((unidades (registrar-unidades)))
  ;; Verifica si hay unidades registradas
  (if unidades
      (format t "Detalles de las unidades registradas:~%")
    (format t "No hay unidades registradas.~%"))
  (terpri)  ;; Imprime una línea en blanco
  ;; Itera sobre cada unidad en la lista de unidades
  (dolist (unidad unidades)
    ;; Imprime los detalles de cada unidad
    (format t "Alcance: ~d, Magnitud: ~d, Latitud: ~d, Longitud: ~d~%"
            (first unidad)
            (second unidad)
            (third unidad)
            (fourth unidad))
    
    ;; Procedo a guardar las unidades en la base de datos 
    (let* ((alcance (first unidad))
           (magnitud (second unidad))
           (latitud (third unidad))
           (longitud (fourth unidad)))
      (terpri)  ;; Imprime una línea en blanco
      ;; Mensaje de depuración para verificar los valores y tipos
      (format t "Insertando: Alcance: ~d, Magnitud: ~d, Latitud: ~d, Longitud: ~d~%"
              alcance magnitud latitud longitud)
      (terpri)  ;; Imprime una línea en blanco
      ;; Llamada a la función de inserción
      (insert-data "refuerzos" '("alcance" "magnitud" "latitud" "longitud")
                   (list alcance magnitud latitud longitud) ))))



(generar-lista-ptos-suministros) ;setea la variable global "*puestos-suministros*"

;; Imprimir los puestos de suministros
(imprimir-puestos-suministros *puestos-suministros*)
(terpri)  ;; Imprime una línea en blanco


                                          #| AGUA |#
(generar-lista-ptos-ag)
(imprimir-puestos-ag)
(terpri)  ;; Imprime una línea en blanco

                                          #| QBN |#

(generar-lista-ptos-qbn)
(imprimir-puestos-qbn)
(terpri)  ;; Imprime una línea en blanco

                                          #| Eno |#

(generar-lista-ptos-eno)
(imprimir-puestos-eno)
(terpri)  ;; Imprime una línea en blanco

                                          #| REFUERZOS |#

(generar-lista-ptos-refuerzos)
(imprimir-puestos-refuerzos)
(terpri)  ;; Imprime una línea en blanco

(load "logica_puntos.lisp")     
  
;; Cerrar conexión al final
;;(cerrar-conexion)

