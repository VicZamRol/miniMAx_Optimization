defun f-e-b- (estado)
  (- (f-e-b estado)))

(defun f-e-c- (estado)
  (- (f-e-c estado)))

(defun f-e-c2- (estado)
  (- (f-e-c2 estado)))
  
 ;;; Hemos modificado partidas-auto para que devuelva si empatan, gana buitre o gana cuervo 
 (defun partidas-auto (jug-b f-e-b f-e-c) ;;; jug-b juega con los buitre
  (let ((fichero (open "partidas.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create)) ;;; creamos el fichero partidas.txt
	(nodo-actual (crear-nodo-inicial 'C)) ;;; nodo-actual es crear-nodo-inicial con el turno de C'
	(movimientos 0) ;;; contador de movimientos
	(jug-c (if (eq jug-b 'MAX) 'MIN 'MAX))
	(res 0));;; Asignamos a jug-b min o max y el contrario a jug-c
    (format fichero "~&Nodo inicial: ~a~%" nodo-actual) ;;; Imprimir nodo inicial
    (loop until (or (es-estado-final (estado nodo-actual)) (>= movimientos 150)) do ;;; Hasta que no sea estado final o no haya llegado al limite de movimientos hacemos
	(setf nodo-actual ;;; nodo-actual es
	      (decision-minimax-A-B nodo-actual (cond ((< movimientos 10) 3) ((< movimientos 50) 4) ((< movimientos 100) 5) (t 6)) (if (eq (turno nodo-actual) 'C) f-e-c f-e-b)) ;;; el resultado de aplicar decision-minimax al nodo-actual con su profundidad, que aumenta durante el juego, y su f-e-e
	      movimientos (1+ movimientos) ;;; actualizamos movimientos
	      (maquina nodo-actual) (turno nodo-actual)) ;;; Cambia la maquina, si antes eran los 'C ahora sera el 'B
	 (format fichero "~%Movimiento ~a:~%La maquina ~a mueve a:~% ~a~%" movimientos (if (eq (turno nodo-actual) 'B) jug-c jug-b) nodo-actual)) ;;; Imprime el nodo-actual despues de hacer el movimiento, el numero de movimientos y MAX o MIN
    (cond ((not (es-estado-final (estado nodo-actual))) ;;; Si no es estado final es porque se ha sobrepasado el limite de movimientos entonces
	  (format fichero "~%LA PARTIDA ACABA EN EMPATE")) ;;; imprimimos ...
	  ((eq (es-estado-final (estado nodo-actual)) 'B) ;;; Si es estado final y gana B entonces
	   (format fichero "~%GANA EL JUGADOR ~a" jug-b)) ;;; imprimimos ...
	  (t (format fichero "~%GANA EL JUGADOR ~a" jug-c))) ;;; Sino es porque gana C
    (close fichero) ;;; Cierra fichero
    (format t "~%El nodo final queda de la siguiente manera:~%~a~%Se han realizado: ~a movimientos." nodo-actual movimientos)
     (cond ((not (es-estado-final (estado nodo-actual))) ;;; Si no es estado final es porque se ha sobrepasado el limite de movimientos entonces
	  (format t "~%LA PARTIDA ACABA EN EMPATE ")(setf res 'empata)) ;;; imprimimos ...
	  ((eq (es-estado-final (estado nodo-actual)) 'B) ;;; Si es estado final y gana B entonces
	   (format t "~%GANA EL JUGADOR ~a " jug-b)(setf res 'buitre)) ;;; imprimimos ...
	  (t (format t "~%GANA EL JUGADOR ~a " jug-c)(setf res 'cuervo)))res))
	  
;;; HEMOS MODIFICADO MAQUINA-BORRACHA PARA QUE DEVUELVA GANA, PIERDE O EMPATA
(defun maquina-borracha (maquina);;; el argumento de entrada es con la ficha que juega la maquina inteligente
  (let ((nodo-actual (crear-nodo-inicial maquina))(movimientos) (mov 0) (res))
    (format t "Juego de los cuervos~%Tablero Inicial:~%~a" nodo-actual) ;;; Imprime el nodo inicial
    (loop until (or (es-estado-final (estado nodo-actual)) (>= mov 150)) do ;;; Hasta que no es estado final, hacer
	 (cond ((eq (turno nodo-actual) maquina) ;;; Si el turno es de la maquina
		(setf nodo-actual (decision-minimax-a-b nodo-actual (cond ((< mov 10) 3) ((< mov 50) 4) ((< mov 100) 5) (t 6))) ;;; Decide movimiento, incrementamos profundidad dependiendo del nº de movimientos y lo aplica
		       mov (1+ mov))  ;;; incrementamos el nº de movimientos
		(format t "~&Movimiento ~a: ~%La maquina inteligente mueve a: ~%~a" mov nodo-actual)) ;;; Imprime el movimiento de la maquina inteligente
	       
	       (t (setf movimientos (genera-movimientos nodo-actual) ;;; movimientos es la lista de movimientos posibles
			mov (1+ mov))  ;;; incrementamos el nº de movimientos
		  (aplica-movimiento (selecciona-aleatorio movimientos) nodo-actual)  ;;; aplicamos el movimiento que seleccionamos de forma aleatoria al nodo actual
		  (format t "~&Movimiento ~a: ~%La maquina borracha ha movido a:~%~a" mov nodo-actual))))  ;;; Imprime el movimiento de la maquina borracha
    (cond ((eq (es-estado-final (estado nodo-actual)) maquina) ;;; Si la maquina inteligente ha llegado al estado final (la maquina inteligente ha ganado)
	(format t "~&HA GANADO LA MAQUINA INTELIGENTE ")(setf res 'gana)) ;;; Imprimimos ...
	  ((<= 150 mov) ;;; Si el nº d movimientos es mayor q 150, empate
	   (format t "~%LA PARTIDA ACABA EN EMPATE ")(setf res 'empata))
	  (t  ;;;Si no gana la otra maquina
	(format t "~&HA GANADO LA MAQUINA BORRACHA, INCREIBLE ")(setf res 'pierde))
	 )res))
;;; GENERA 100 EXPERIMENTOS DE LA FUNCION MAQUINA-BORRACHA
(defun experimentacion-maquina-borracha (maquina)
  (let ((pierde 0) (gana 0) (empata 0) (res))
    (loop for x from 1 to 100 do
	 (setf res (maquina-borracha maquina))
	 (cond ((eq res 'pierde) (setf pierde (1+ pierde)))
	       ((eq res 'gana) (setf gana (1+ gana)))
	       ((eq res 'empata) (setf empata(1+ empata)))))
    (format t "~&Despues de 100 pruebas la maquina inteligente jugando con ~a pierde: ~a, gana: ~a y empata: ~a" maquina pierde gana empata)))
;;; GENERA 100 EXPERIMENTOS DE LA FUNCION PARTIDAS-AUTO
(defun experimentacion-partidas-auto (f-e-b f-e-c)
  (let ((cuervo 0) (buitre 0) (empata 0) (res))
    (loop for x from 1 to 100 do
	 (setf res (partidas-auto 'MAX f-e-b f-e-c))
	 (cond ((eq res 'cuervo) (setf cuervo (1+ cuervo)))
	       ((eq res 'buitre) (setf buitre (1+ buitre)))
	       ((eq res 'empata) (setf empata(1+ empata)))))
    (format t "~&Despues de 100 pruebas la funcion f-e-b enfrentada con f-e-c da como resultado:
Que ganan los cuervos: ~a veces, Que gana el buitre: ~a veces y empatan: ~a veces" cuervo buitre empata)))
    