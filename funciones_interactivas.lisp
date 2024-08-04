
                #| DEFINE LAS FUNCIONES QUE SOLICITAN DATOS AL USUARIO |#

(defun solicitar-coordenadas ()
  "Solicita al usuario la latitud y longitud en dos dígitos y devuelve una lista con ambos valores."
  (format t "Ingrese la latitud (en formato XX): ")
  (finish-output)  ; Asegura que el mensaje se imprima de inmediato
  (let* ((latitud (read-line))
         (longitud (progn
                     (format t "Ingrese la longitud (en formato XX): ")
                     (finish-output)  ; Asegura que el mensaje se imprima de inmediato
                     (read-line)))
         (latitud-num (read-from-string latitud))
         (longitud-num (read-from-string longitud)))
    ;; Verifica que latitud-num y longitud-num sean números y retorna la lista
    (if (and (numberp latitud-num) (numberp longitud-num))
        (list latitud-num longitud-num)
        (progn
          (format t "Error: La latitud o longitud ingresada no es válida.~%")
          nil))))


(defun dist-max-sin-sumin  ()
"Solicita la distancia máxima a recorrer sin suministros por parte del elemento."
      (format t "Ingrese la distancia máxima a recorrer por su elemento sin necesidad de ser abastecida: ")

      (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
      (let ((dist_max (read-line)))
    (return-from dist-max-sin-sumin dist_max)))  ;; Devolver el valor leído

(defun puestos-suministros ()
  "Solicita el número de puestos de suministros, y para cada uno, solicita su nombre y coordenadas, devolviendo una lista de sublistas con el nombre del puesto y sus coordenadas."
  (format t "¿Cuantos puestos de suministros nuevo desea cargar? ")
  (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
  (let* ((num-puestos (parse-integer (read-line)))  ;; Leer el número de puestos
         (puestos '()))  ;; Inicializar la lista de puestos
    (dotimes (i num-puestos puestos)  ;; Iterar sobre el número de puestos
      (format t "Ingrese el nombre del puesto ~a: " (1+ i))
      (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
      (let* ((nombre-puesto (read-line))
             (coordenadas (solicitar-coordenadas)))  ;; Obtener coordenadas
        (push (list nombre-puesto coordenadas) puestos)))))  ;; Agregar el puesto a la lista            

(defun solicitar-alcance ()
  "Solicita al usuario el alcance de una zona y devuelve el valor."
  (format t "Ingrese la extension afectada: ")
  (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
  (read-line)) 

(defun solicitar-magnitud ()
  "Solicita al usuario la magnitud del elemento."
  (format t "Ingrese la magnitud del elemento: ")
  (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
  (parse-integer (read-line)))   

(defun registrar-zonas ()
  "Consulta si se registran zonas QBN, y si es afirmativo, solicita cuántas zonas y sus detalles."
  (format t "¿Se registran zonas afectadas? (sí/no): ") 
  (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
  (let* ((respuesta (string-downcase (string-trim '(#\Space #\Tab) (read-line))))
         (respuesta-valida (or (string= respuesta "sí") (string= respuesta "si"))))  ;; Considera "si" como una respuesta válida
    (if respuesta-valida
        (progn
          (format t "¿Cuántas zonas se registran? ")
          (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
          (let* ((num-zonas (parse-integer (read-line)))  ;; Leer el número de zonas
                 (zonas '()))  ;; Inicializar la lista de zonas
            (dotimes (i num-zonas zonas)  ;; Iterar sobre el número de zonas
              (format t "Ingrese el nombre de la zona ~a: " (1+ i))
              (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
              (let* ((nombre-zona (read-line))
                     (coordenadas (solicitar-coordenadas))  ;; Obtener coordenadas
                     (alcance (solicitar-alcance)))  ;; Obtener alcance
                (push (list nombre-zona coordenadas alcance) zonas)))  ;; Agregar la zona a la lista
            zonas))  ;; Devolver la lista de zonas
        (progn
          (format t "No se registran zonas afectadas.")
          (finish-output)))))  ;; Mensaje si no se registran zonas

(defun registrar-unidades ()
  "Consulta si se registran unidades y, si es afirmativo, solicita cuántas y sus detalles."
  (format t "¿Se registran unidades? (sí/no): ")
  (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
  (let* ((respuesta (string-downcase (string-trim '(#\Space #\Tab) (read-line))))
         (respuesta-valida (or (string= respuesta "sí") (string= respuesta "si"))))  ;; Considera "si" como una respuesta válida
    (if respuesta-valida
        (progn
          (format t "¿En cuántos lugares? ")
          (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
          (let* ((num-unidades (parse-integer (read-line)))  ;; Leer el número de unidades
                 (unidades '()))  ;; Inicializar la lista de unidades
            (dotimes (i num-unidades unidades)  ;; Iterar sobre el número de unidades
              (format t "Ingrese las coordenadas para la unidad ~a (latitud y longitud): " (1+ i))
              (finish-output)  ;; Asegura que el mensaje se imprima de inmediato
              (terpri)  ;; Imprime una línea en blanco
              (let* ((coordenadas (solicitar-coordenadas))  ;; Obtener coordenadas
                     (latitud (first coordenadas))
                     (longitud (second coordenadas))
                     (alcance (solicitar-alcance))  ;; Obtener alcance
                     (magnitud (solicitar-magnitud))) ;; Obtener magnitud
                ;; Aquí realizamos el PUSH
                (push (list alcance magnitud latitud longitud) unidades)))  ;; Agregar las unidades a la lista
            unidades))  ;; Devolver la lista de unidades    
        (progn
          (format t "No se registran unidades.")
          (finish-output))))) ;; Mensaje si no se registran unidades


