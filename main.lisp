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



                              #|CARGO CASI TODOS LOS SCRIPT |#

(load "logica_calculo_recorrido.lisp")
(load "calculo_costo.lisp")
(load "logica_calculo_recorrido.lisp")
(load "funciones_interactivas.lisp")
(load "logica_costo_camino.lisp")
(load "logica_principal.lisp")
(load "op_BD")
(load "impresiones.lisp")


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

;; Definir variable global
(defvar *partida* '()   
  "Partida.") 


                            #| INICA LA LOGICA DEL PROGRAMA  |#

;; Inicializar conexión al inicio
(inicializar-conexion)

(execute-delete-queries)

;; Llamar a la función y almacenar el resultado en la variable global
(format t "Ingrese las coordenadas de destino~%")

(setf *destino* (solicitar-coordenadas))
(setf *destino_final* *destino*)

  
;; Llamar a la función y almacenar el resultado en la variable global
(format t "Ingrese las coordenadas de partida~%")
(delete-patida "puestos_suministros") 

(delete-all-data "registros")   
(setq *partida* (solicitar-coordenadas))
(setq *partida_inicial* *partida*)


;registro la partida tambien en la base de datos de suministros, para facilitar el proceso   
(insert-data "puestos_suministros" '("nombre" "latitud" "longitud" "costo") (list "partida" (car *partida*) (cdr *partida*) 0))

;incorporo el destino a la lista de puestos de suministros, para agregar un cierre
(insert-data "puestos_suministros" '("nombre" "latitud" "longitud" "costo") (list "destino" (car *destino_final*) (cdr *destino_final*) 9999))


#| REGISTRO EL PUNTO DE PARTIDA EN REGISTROS PARA QUE PUEDA SER BUSCADO COMO PTO INICIAL |#

(let* ((latitud_partida (car *partida*))
       (longitud_partida (cdr *partida*))
       (costo_inicial 0)
      )
(insertar-en-registros latitud_partida longitud_partida costo_inicial))


  #| IMPRIME LAS VARIABLES INGRESADAS, ANTES DE EMPEZAR A EJECUTAR LOS CALCULOS |#

;; Imprimir el valor de la variable global de distancia maxima sin suministros
(format t "Distancia máxima sin suministros ingresada: ~a~%" *dist-max-sin-suminis*)

;; Imprimir el valor de la variable global de destino
(format t "Magnitud del elemento propio: ~a~%" *magnitud_elem_ppio*)

; Imprimir el valor de la variable global de partida
(format t "Partida: ~a~%" *partida*)

;; Imprimir el valor de la variable global de destino
(format t "Destino: ~a~%" *destino*)

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
      (terpri)  ;; Imprime una línea en blanco
      ;; Llamada a la función de inserción
      (insert-data "Ag" '("nombre" "latitud" "longitud" "alcance")
                   (list nombre latitud longitud alcance)))))

;;eno

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
      (terpri)  ;; Imprime una línea en blanco
      ;; Mensaje de depuración para verificar los valores y tipos
      (format t "Insertando: Alcance: ~d, Magnitud: ~d, Latitud: ~d, Longitud: ~d~%"
              alcance magnitud latitud longitud)
      
      ;; Llamada a la función de inserción
      (insert-data "refuerzos" '("alcance" "magnitud" "latitud" "longitud")
                   (list alcance magnitud latitud longitud) ))))



(generar-lista-ptos-suministros) ;setea la variable global "*puestos-suministros*"

;; Imprimir los puestos de suministros
(imprimir-puestos-suministros *puestos-suministros*)



                                          #| AGUA |#
(generar-lista-ptos-ag)
(imprimir-puestos-ag)


                                          #| QBN |#

(generar-lista-ptos-qbn)
(imprimir-puestos-qbn)


                                          #| Eno |#

(generar-lista-ptos-eno)
(imprimir-puestos-eno)


                                          #| REFUERZOS |#

(generar-lista-ptos-refuerzos)
(imprimir-puestos-refuerzos)


(load "logica_puntos.lisp")     
  

(imprimir-cuadrado)
;; Cerrar conexión al final
;;(cerrar-conexion)

