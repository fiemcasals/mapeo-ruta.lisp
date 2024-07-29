(defun expand-file-name (path)
  ;; Combina el path proporcionado con el directorio de inicio del usuario,
  ;; sustituyendo las barras invertidas (\) por barras normales (/).
  (merge-pathnames (substitute #\/ #\\ path)
                   (user-homedir-pathname)))

;; Cargar Quicklisp
(load (expand-file-name "~/quicklisp/setup.lisp"))

;; Cargar las bibliotecas requeridas
(ql:quickload :cl-dbi)
(ql:quickload :cl-mysql)

;; Importar los paquetes necesarios
(defpackage :my-app
  (:use :cl :cl-dbi)) ;; No importa ninguna función de cl-mysql para evitar conflictos

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
  "Establece una conexión a la base de datos y la almacena en *db-connection*."
  (setf *db-connection* (apply #'cl-dbi:connect *db-params*)))

    (defun execute-query (query &rest params) 
    "Ejecuta una consulta SQL con parámetros y devuelve todos los resultados en una lista."
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



    #|(defun execute-query (query)
    "Ejecuta la consulta SQL proporcionada y devuelve todos los resultados en una lista."
    (let ((stmt (cl-dbi:prepare *db-connection* query))
          (results '())) ;; Lista para almacenar los resultados
      ;; Ejecutar la consulta
      (cl-dbi:execute stmt)
      ;; Iterar sobre los resultados y almacenarlos en la lista
      (loop for row = (cl-dbi:fetch stmt) while row do
        (push row results))
      ;; Devolver la lista de resultados en orden
      (reverse results))) |# 



    (defun insert-data (table columns values)
  "Inserta datos en la tabla especificada con columnas y valores proporcionados.
   Utiliza parámetros para prevenir inyección SQL."
  (let* ((placeholders (make-list (length values) :initial-element "?"))
         (query (format nil "INSERT INTO ~a (~{~a~^, ~}) VALUES (~{~a~^, ~})"
                        table columns placeholders))
         (prepared-query (cl-dbi:prepare *db-connection* query)))
    ;; Asegúrate de que `values` es una lista
    (cl-dbi:execute prepared-query values)))


  (defun update-data-coordenadas (table latitud longitud columns values)
  ;; Preparar la consulta de actualización
  (let* ((set-clause (mapcar (lambda (col val) (format nil "~a = '~a'" col val)) columns values))
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

(defun cerrar-conexion ()
  "Cierra la conexión a la base de datos."
  (when *db-connection*
    (cl-dbi:disconnect *db-connection*)
    (setf *db-connection* nil)))

;; Inicializar conexión al inicio
(inicializar-conexion)

  ;; Llamar a la función para ejecutar la consulta e imprimir los resultados
  ;; (execute-query "SELECT * FROM Eno")

  

;; Llamar a la función para actualizar datos
;;(update-data "Eno" 1 '("alcance" "magnitud" "latitud" "longitud") '(150 10 13 58))

;; Llamar a la función para eliminar datos
;;(delete-data "Eno" 12)

;; Cerrar conexión al final
;;(cerrar-conexion)


;;empezamos la exposicion en este punto

;;lo primero que debe hacer es solicitar por pantalla la coordenada tanto latitud como longitud en dos digitos cada uno de la posicion actual donde se encuentra

;; Cargar el archivo que contiene la función
(load "funciones_interactivas.lisp")

;;le solicita la distancia maxima capaz de recorrer sin abastecerse en un puesto de suministro

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

#| SE VA A DEFINIR LA VARIABLE GLOBAL DE DESTINO |#

;; Definir variable global
(defvar *destino* nil
  "Destino.")  

;; Llamar a la función y almacenar el resultado en la variable global
(format t "Ingrese las coordenadas de destino")
(terpri)  ;; Imprime una línea en blanco
(setf *destino* (solicitar-coordenadas))
(terpri)  ;; Imprime una línea en blanco


;; Imprimir el valor de la variable global de distancia maxima sin suministros
(format t "Distancia máxima sin suministros ingresada: ~a~%" *dist-max-sin-suminis*)
(terpri)  ;; Imprime una línea en blanco

;; Imprimir el valor de la variable global de destino
(format t "Magnitud del elemento propio: ~a~%" *magnitud_elem_ppio*)
(terpri)  ;; Imprime una línea en blanco

;; Imprimir el valor de la variable global de destino
(format t "Destino: ~a~%" *destino*)
(terpri)  ;; Imprime una línea en blanco

;;le pregunta sobre los puestos de suministros disponibles
;;acorde con la cantidad entra en un for que itera la cantidad de puestos de suministros del punto anterior
;;pide las coordenas de igual modo, de cada puesto de sumunistro

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
        
            ;; Llamada a la función de inserción (comentada para evitar ejecución)
            (insert-data "puestos_suministros" '("nombre" "latitud" "longitud") (list nombre latitud longitud)) 
    )))         



;;va a consultar si se registran zonas QBN
;;de ser afirmativo va a preguntar cuantas zonas se registran
;;va a iterar la cantidad de zonas para preguntar sus posiciones y el alcance de las mismas
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
      (insert-data "Ag" '("nombre" "latitud" "longitud" "alcance")
                   (list nombre latitud longitud alcance)))))

;;de igual manera con el eno y con los refuerzos, solo que en estos casos tambien va a preguntar por la magnitud de los mismos.

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
      
      ;; Mensaje de depuración para verificar los valores y tipos
      (format t "Insertando: Alcance: ~d, Magnitud: ~d, Latitud: ~d, Longitud: ~d~%"
              alcance magnitud latitud longitud)
      
      ;; Llamada a la función de inserción
      (insert-data "refuerzos" '("alcance" "magnitud" "latitud" "longitud")
                   (list alcance magnitud latitud longitud) ))))


;;en este punto el algoritmo puede empezar a trabajar, habiendo guardado toda la informacion en una base de datos. permitiendo la reutilizacion del relevamiento en el planeamiento desde y hacia distitos puntos

;;logica interna del funcionamiento:        

;;1. levanta de la base de datos, todos los puestos de suministros


;; Ejecutar la consulta para obtener todos los puestos de suministros

(load "logica.lisp")


;; Llamadas a las funciones
;; Generar la lista de puestos de suministros
(generar-lista-ptos-suministros) ;genera una variable global "*puestos-suministros*"

;; Imprimir los puestos de suministros
(imprimir-puestos-suministros *puestos-suministros*)
(terpri)  ;; Imprime una línea en blanco

;; Procesar los puestos de suministros
(let ((puestos-procesados (procesar-puestos-suministros *puestos-suministros*)))
  ;; Filtrar los puestos procesados basados en la distancia máxima

;;2. selecciona solamente los que se encuentran a una distancia menor que la capaz de recorrer el elemento sin suministros extras

 (let ((puestos-filtrados (filtrar-puestos-procesados puestos-procesados '(25 25)#|pos|# *dist-max-sin-suminis*)))
      ;; Imprimir los puestos filtrados
      (format t "Puestos filtrados por distancia:~%")
      (imprimir-puestos-suministros-filtrados puestos-filtrados)
    (terpri)  ;; Imprime una línea en blanco
))
;;3. ordena todos los puntos de suministros seleccionados desde el mas cercano al mas lejano

;; no termina siendo realmente util. asiq no los ordene


;;4. calcula el costo que tiene el llegar a primer punto del vector de suministros, desde la posicion actual. y lo carga en un registro(acarreo)(como lo calcula se explica mas abajo)
(load "logica_costo_camino.lisp")



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

                                          #| Refuerzos |#

(generar-lista-ptos-refuerzos)
(imprimir-puestos-refuerzos)
(terpri)  ;; Imprime una línea en blanco



;;5. repite el paso numero 4 pero con el siguiente de la lista, es decir, reutiliza la misma funcion pero primero le corta la cabeza a la lista de puntos de suministros antes de llamar a la funcion. en este momento se van a estar ejecutando paralelamente el paso numero 6 y el 5 con los demas puntos de suministros, hasta terminar con todos.
;;6. al terminar de calcular cada costo a los distintos puntos, lo suma al acarreo y repite el paso numero 2, pero desde su nueva posicion, continuando con los siguientes pasos.
;;en este punto y sin el filtro que voy a detallar a continuacion, seria redundante en calculos y recursividad
;;para evitar redundancia, antes de ejecutar el paso numero 4, verifica que el punto de suministros utilizado para el calculo del costo, no este en su vector de acarreo (que por cierto, ademas de cargar (sumando) los costos, tambien agrega los puntos de suministros para evitar sobrecalculos, o circuitos cerrados)
;;ademas, si algun camino llega al destino, el costo se carga en una variable, costo_maximo, la cual se inicializa en infinito, buscando que se mate cualquier calculo que ya tenga un valor en el acarreo igual o mayor que el que figura en costo_maximo.

;;a continuacion se pasa a explicar como se calcula el costo desde la posicion al punto de suminitro:
;;se calcula el costo desde la posicion hasta todas las posiciones adyascentes, para esto se considera solamente posiciones definidas por dos digitos, por cada latitud y cada longitud, buscando de esta manera evaluar los 8 puntos adyacentes a cada posicion, variando en -1 0 o 1 cada valor de latitud y longitud. de esa manera se carga en el peso que tiene desplazarse desde un punto a otro, el peso de cada punto recorrido. De igual manera cada lista que se genera, se genera con un valor de acarreo actual, y una lista de puntos visitados, para de esta forma evitar explorar rutas que tengan un costo mayor al costo_maximo_a_suministro, y a la vez para evitar que vuelva a un punto ya transitado

;;una vez que se calcula el camino optimo a un   punto de suministro. se calcula a todos los demas, y desde estos puntos de suministros a todos los demas, hasta que uno llega a el punto final, matando todos los posibles caminos con un costo mayor al costo_maximo, dejando vivo solo aquellos posibles caminos que tienen un costo acumulado menor al costo_maximo, dando la posibilidad de que se alcance un camino con un costo menor.

(defparameter *costo_max_admisible* 4000)
(defparameter *recorrido_recomendable* '())

(load "logica_calculo_recorrido.lisp") 
(load "calculo_costo.lisp")
  


(let ((costo_acarreo 0) ; Inicializa el costo en cero
      (puestos_acarreo '())
       ; Inicializa la lista de puestos de acarreo vacía
      (pos '(15 5))) ; Se inicializa una posición cualquiera
  (recorrer-y-ejecutar pos costo_acarreo puestos_acarreo))  


(print "costo maximo admisible")
(print *costo_max_admisible*)
(print "camino a recorrer")
(print *recorrido_recomendable*)