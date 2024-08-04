 
                            #| FUNCIONES COMUNES A TODOS |#


;; Calcula la distancia euclidiana ent   dos puntos dados en latitud y longitud
(defun calculate-distance (lat1 long1 lat2 long2)
  "Calcula la distancia euclidiana entre dos puntos dados en latitud y longitud."
  (sqrt (+ (expt (- lat1 lat2) 2) (expt (- long1 long2) 2))))

#| FUNCION PARA IMPRIMIR UNA LISTA CUALQUIERA |#

(defun imprimir-lista (lista)
  "Imprime los elementos de la lista proporcionada."
  (format t "Lista:~%")
  (dolist (elemento lista)
    (format t "~a~%" elemento))
    
    (terpri)  );; Imprime una línea en blanco


                                      #| PUESTO DE AGUA |#


;; Declarar la variable global *puestos-ag*
(defvar *puestos-ag* nil
  "Lista de todos los puestos de agua obtenidos de la base de datos.")

#| PRIMER PASO - OBTENER LAS POSICIONES DE AGUA |#

;; Genera la lista de puestos de agua desde la base de datos
(defun generar-lista-ptos-ag ()
  "Ejecuta la consulta para obtener todos los puestos de Ag y almacena los resultados en una variable."
  (let ((query "SELECT  latitud, longitud, alcance FROM Ag"))
    (setf *puestos-ag* (execute-query query))))

;; Imprime los puestos de ag almacenados en la lista proporcionada
(defun imprimir-puestos-ag ()
  "Imprime los puestos de ag almacenados en la lista proporcionada."
  (format t "Puestos de ag almacenados:~%")
  (dolist (puesto *puestos-ag*)
    (format t "Latitud: ~a, Longitud: ~a, Alcance: ~a~%"
            (second puesto)
            (fourth puesto)
            (sixth puesto))))

                                   #|        QBN          |#


#| SEGUIMOS CON LA LOGICA PARA LEVANTAR LAS TABLAS DE QBN |#

;; Declarar la variable global *puestos-qbn*
(defvar *puestos-qbn* nil
  "Lista de todos los puestos de qbn obtenidos de la base de datos.")

#| PRIMER PASO - OBTENER LAS POSICIONES DE QBN |#

;; Genera la lista de puestos de qbn desde la base de datos
(defun generar-lista-ptos-qbn ()
  "Ejecuta la consulta para obtener todos los puestos de qbn y almacena los resultados en una variable."
  (let ((query "SELECT  latitud, longitud, alcance FROM qbn"))
    (setf *puestos-qbn* (execute-query query))))

;; Imprime los puestos de qbn almacenados en la lista proporcionada
(defun imprimir-puestos-qbn ()
  "Imprime los puestos de qbn almacenados en la lista proporcionada."
  (format t "Puestos de qbn almacenados:~%")
  (dolist (puesto *puestos-qbn*)
    (format t "Latitud: ~a, Longitud: ~a, Alcance: ~a~%"
            (second puesto)
            (fourth puesto)
            (sixth puesto))))
            


                                 #|        Eno          |#

#| SEGUIMOS CON LA LOGICA PARA LEVANTAR LAS TABLAS DE Eno |#

;; Declarar la variable global *puestos-Eno*
(defvar *puestos-eno* nil
  "Lista de todos los puestos de eno obtenidos de la base de datos.")

#| PRIMER PASO - OBTENER LAS POSICIONES DE ENO |#

;; Genera la lista de puestos de eno desde la base de datos
(defun generar-lista-ptos-eno ()
  "Ejecuta la consulta para obtener todos los puestos de eno y almacena los resultados en una variable."
  (let ((query "SELECT  latitud, longitud, alcance, magnitud FROM Eno"))
    (setf *puestos-eno* (execute-query query))))

;; Imprime los puestos de eno almacenados en la lista proporcionada
(defun imprimir-puestos-eno ()
  "Imprime los puestos de eno almacenados en la lista proporcionada."
  (format t "Puestos de eno almacenados:~%")
  (dolist (puesto *puestos-eno*)
    (format t "Latitud: ~a, Longitud: ~a, Alcance: ~a,Magnitud: ~a~%"
            (second puesto)
            (fourth puesto)
            (sixth puesto)
            (eighth puesto))))




                                 #|    Refuerzos              |#



#| SEGUIMOS CON LA LOGICA PARA LEVANTAR LAS TABLAS DE REFUERZOS |#

;; Declarar la variable global *puestos-refuerzos*
(defvar *puestos-refuerzos* nil
  "Lista de todos los puestos de refuerzos obtenidos de la base de datos.")

#| PRIMER PASO - OBTENER LAS POSICIONES DE REFUERZOS |#

;; Genera la lista de puestos de refuerzos desde la base de datos
(defun generar-lista-ptos-refuerzos ()
  "Ejecuta la consulta para obtener todos los puestos de refuerzos y almacena los resultados en una variable."
  (let ((query "SELECT  latitud, longitud, alcance, magnitud FROM refuerzos"))
    (setf *puestos-refuerzos* (execute-query query))))

;; Imprime los puestos de refuerzos almacenados en la lista proporcionada
(defun imprimir-puestos-refuerzos ()
  "Imprime los puestos de refuerzos almacenados en la lista proporcionada."
  (format t "Puestos de refuerzos almacenados:~%")
  (dolist (puesto *puestos-refuerzos*)
    (format t "Latitud: ~a, Longitud: ~a, Alcance: ~a,Magnitud: ~a~%"
            (second puesto)
            (fourth puesto)
            (sixth puesto)
            (eighth puesto))))




#| LA SIGUIENTE FUNCION DE CALCULO DE DISTANCIA LA VOY A NECESITAR MAS ADELATNE |#
#|(let ((distancia (calculate-distance lat-actual long-actual lat-puesto long-puesto))) |# ;; Calcular distancia
               


