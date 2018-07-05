;;;---------------
;;; IMPLEMENTACION
;;;---------------


;;; DEFINICIÓN DE LA ESTRUCTURA DE DATOS


(defstruct (estado (:constructor crea-estado)
		   (:conc-name )
		   (:print-function escribe-estado))
  tablero ;;; Matriz de 10 elementos con las posiciones del tablero
  cuervos ;;; Nº de cuervos colocados
  buitre ;;; Indica si el buitre se ha colocado ya, y si es asi, la posicion en la que se encuentra
  comidos ;;; Nº de cuervos comidos
 
)

(defun escribe-estado (estado &optional (canal t) profundidad)
  (format canal "~&           0.~a~%           / \\~%          /   \\~%         /     \\~%1.~a----2.~a-----3.~a----4.~a~% \\     /        \\     /~%  \\   /          \\   /~%   \\ /            \\ /~%   5.~a            6.~a~%   / \\            / \\~%  |   \\          /   |~%  |     \\      /     |~%  |      \\    /      |~%  |       7.~a        |~%  |      /    \\      |~%  |    /        \\    |~%  |   /          \\   |~%   8.~a            9.~a~%Cuervos colocados: ~a~%Cuervos Comidos: ~a" (aref (tablero estado) 0) (aref (tablero estado) 1) (aref (tablero estado) 2) (aref (tablero estado) 3) (aref (tablero estado) 4) (aref (tablero estado) 5) (aref (tablero estado) 6) (aref (tablero estado) 7) (aref (tablero estado) 8) (aref (tablero estado) 9) (cuervos estado) (comidos estado)))
(defun crea-estado-inicial()
  (crea-estado :tablero (make-array '(10) :initial-element 'O)
	       :cuervos 0
	       :buitre nil
	       :comidos 0))

(defstruct (nodo (:constructor crea-nodo)
		 (:conc-name)
		 (:print-function escribe-nodo))
  estado ;;; Estructura anterior
  turno ;;; Jugador al que le toca mover/poner
  maquina ;;; Indica si la maquina juega con los cuervos o con el buitre
)

(defun escribe-nodo (nodo &optional (canal t) profundidad)
  (format canal "~a~%Turno: ~a" (estado nodo) (turno nodo)))

(defun crear-nodo(estado turno maquina)
  (crea-nodo :estado estado
	     :turno turno
	     :maquina maquina))

(defun crear-nodo-inicial(&optional (maquina 'B))
  (crear-nodo (crea-estado-inicial) 'C maquina))

;;; FUNCIONES AUXILIARES

;;; Devuelve un booleano que es cierto si dos posiciones son adyacentes
(defun adyacentes (x y)
  (if (or (and (eq x 0) (eq y 2)) (and (eq x 2) (eq y 0)) (and (eq x 0) (eq y 3)) (and (eq x 3) (eq y 0)) (and (eq x 1) (eq y 2)) (and (eq x 2) (eq y 1)) (and (eq x 2) (eq y 3)) (and (eq x 3) (eq y 2)) (and (eq x 3) (eq y 4)) (and (eq x 4) (eq y 3)) (and (eq x 1) (eq y 5)) (and (eq x 5) (eq y 1)) (and (eq x 2) (eq y 5)) (and (eq x 5) (eq y 2)) (and (eq x 3) (eq y 6)) (and (eq x 6) (eq y 3)) (and (eq x 4) (eq y 6)) (and (eq x 6) (eq y 4)) (and (eq x 5) (eq y 8)) (and (eq x 8) (eq y 5)) (and (eq x 5) (eq y 7)) (and (eq x 7) (eq y 5)) (and (eq x 6) (eq y 7)) (and (eq x 7) (eq y 6)) (and (eq x 6) (eq y 9)) (and (eq x 9) (eq y 6)) (and (eq x 7) (eq y 8)) (and (eq x 8) (eq y 7)) (and (eq x 7) (eq y 9)) (and (eq x 9) (eq y 7))) t nil))

;;; Devuelve la posicion intermedia entre dos posiciones dadas
(defun entre (x y)
  (cond ((or (and (eq x 0) (eq y 5)) (and (eq x 5) (eq y 0))) 2)
	((or (and (eq x 0) (eq y 6)) (and (eq x 6) (eq y 0))) 3)
	((or (and (eq x 1) (eq y 3)) (and (eq x 3) (eq y 1))) 2)
	((or (and (eq x 1) (eq y 7)) (and (eq x 7) (eq y 1))) 5)
	((or (and (eq x 2) (eq y 4)) (and (eq x 4) (eq y 2))) 3)
	((or (and (eq x 2) (eq y 8)) (and (eq x 8) (eq y 2))) 5)
	((or (and (eq x 3) (eq y 9)) (and (eq x 9) (eq y 3))) 6)
	((or (and (eq x 4) (eq y 7)) (and (eq x 7) (eq y 4))) 6)
	((or (and (eq x 5) (eq y 9)) (and (eq x 9) (eq y 5))) 7)
	((or (and (eq x 6) (eq y 8)) (and (eq x 8) (eq y 6))) 7)
	(t nil)))


;;; Devuelve una lista con las posiciones adyacentes a una posicion dada
(defun lista-adyacentes (posicion)
  (loop for x from 0 to 9 
     when (adyacentes x posicion) 
     collect x))

;;; Cuenta los movimientos posibles dada una posicion
(defun movimientos (tablero pos)
  (loop for x in (lista-adyacentes pos)
     when (eq (aref tablero x) 'O) ;;; Que la posicion a la que se mueve este vacia
     count x))

;;; Cuenta los saltos posibles dada una posicion
(defun saltos (tablero pos)
  (loop for x from 0 to 9 
     when (and (entre pos x) ;;; Se puede saltar a x
	       (eq (aref tablero x) 'O) ;;; Que la posicion a la que vamos a saltar este vacia 
	       (eq (aref tablero (entre pos x)) 'C)) ;;; Que la posicion que esta en medio tenga un cuervo
     count x))

;;; En el caso de que la funcion de evaluacion estatica de varias nodos sea la misma usamos esta funcion para elegir un nodo aleatoriamente
(defun selecciona-aleatorio (l)
  (nth (random (length l)) l))

;;; Clonar un nodo
(defun clonar (nodo)
  (crear-nodo (crea-estado :tablero (make-array '(10) 
	       :initial-contents (loop for x from 0 to 9 collect (aref (tablero (estado nodo)) x)))
	       :cuervos (cuervos (estado nodo))
	       :buitre (buitre (estado nodo))
	       :comidos (comidos (estado nodo)))
	      (turno nodo)
	      (maquina nodo)))

;;; Funcion que genera una lista con todos los movimientos posibles que puede realizar el buitre para comerse uno o varios cuervos
(defun mov-saltos (nodo)
    (loop for x from 0 to 9  
       when (and (entre (buitre (estado nodo)) x) ;;; Se puede saltar a x
		 (eq (aref (tablero (estado nodo)) x) 'O) ;;; Que la posicion a la que vamos a saltar este vacia 
		 (eq (aref (tablero (estado nodo)) (entre (buitre (estado nodo)) x)) 'C)) ;;; Que la posicion que esta en medio tenga un cuervo
       collect (list 'comer-cuervo (append (list x) ;;; Unimos en una misma lista los cuervos comidos en un salto 
;;; (COMER-CUERVO (3))
		       (second (first (mov-saltos (comer-cuervo-aux (clonar nodo) x)))))))) ;;; con los que se come en mas de uno ((COMER-CUERVO (9))) y al hacer el append --> ((COMER-CUERVO (3 9)))

;;; clonamos el nodo para ir viendo paso a paso cuales son los posibles saltos, de esta forma el nodo que entra como parametro no se modifica



;;; MOVIMIENTOS

(defun poner-buitre (nodo posicion)
  (cond ((and (eq (turno nodo) 'B) ;;; Turno de B
	      (eq (aref (tablero (estado nodo)) posicion) 'O) ;;; Que la posicion este libre [(estado nodo) --> cogemos de la estructura nodo su estado]
	      (not (buitre (estado nodo)))) ;;; Que el buitre no este colocado

	 (setf (aref (tablero (estado nodo)) posicion) 'B ;;; Colocamos el buitre en la posicion indicada
               (turno nodo) 'C ;;; Cambiamos el turno
               (buitre (estado nodo)) posicion) ;;; Actualizamos posicion del buitre
          nodo)
	(t nil)))

(defun poner-cuervo (nodo posicion)
  (cond ((and (eq (turno nodo) 'C) ;;; Turno de C
	      (eq (aref (tablero (estado nodo)) posicion) 'O) ;;; Que la posicion este libre 
	      (< (cuervos (estado nodo)) 7)) ;;; El numero de cuervos colocados sea menor de 7 (faltan cuervos por poner)

	 (setf (aref (tablero (estado nodo)) posicion) 'C ;;; Colocamos el cuervo en la posicion indicada
               (turno nodo) 'B ;;; Cambiamos el turno
               (cuervos (estado nodo)) (+ (cuervos (estado nodo)) 1)) ;;; Actualizamos el numero de cuervos colocados
           nodo)
	(t nil)))


(defun mover-buitre (nodo posicion)
   (cond ((and (eq (turno nodo) 'B) ;;; Turno de B
               (eq (aref (tablero (estado nodo)) posicion) 'O) ;;; Que la posicion a la que vamos a mover este libre 
	       (buitre (estado nodo)) ;;; Que el buitre este colocado
	       (adyacentes posicion (buitre (estado nodo)))) ;;; Que la posicion a la que vamos a mover el buitre sea adyacente de la antigua

	 (setf (aref (tablero (estado nodo)) posicion) 'B ;;; Colocamos el buitre en la posicion indicada
	       (aref (tablero (estado nodo)) (buitre (estado nodo))) 'O ;;; Borramos de la antigua posicion el buitre
	       (turno nodo) 'C ;;; cambiamos el turno
               (buitre (estado nodo)) posicion) ;;; Actualizamos posicion del buitre
          nodo)
	(t nil)))

(defun mover-cuervo (nodo cuervo posicion)
  (cond ((and (eq (turno nodo) 'C) ;;; Turno de C
              (eq (aref (tablero (estado nodo)) posicion) 'O) ;;; Posicion a la que lo vamos a mover este libre 
              (eq (cuervos (estado nodo)) 7) ;;; Que esten todos los cuervos colocados
              (adyacentes posicion cuervo) ;;; Posicion a la que vamos a mover el cuervo sea adyacente con la antigua
	      (eq (aref (tablero (estado nodo)) cuervo) 'C)) ;;; Que en la posicion cuervo que pasamos como parametro exista un cuervo colocado 

	 (setf (aref (tablero (estado nodo)) posicion) 'C ;;; Colocamos el cuervo en la posicion indicada
	       (aref (tablero (estado nodo)) cuervo) 'O ;;; Borramos de la antigua posicion el cuervo
	       (turno nodo) 'B) ;;; cambiamos el turno
          nodo)
	(t nil)))

(defun comer-cuervo (nodo posicion) ;;; posicion a la que salta el buitre
   (cond ((and (eq (turno nodo) 'B) ;;; Turno de B
               (eq (aref (tablero (estado nodo)) posicion) 'O) ;;; Que la posicion a la que vamos a mover este libre 
	       (buitre (estado nodo)) ;;; Que el buitre este colocado
	       (entre posicion (buitre (estado nodo))) ;;; Que la posicion a la que vamos a mover el buitre este en linea con la antigua
	       (eq (aref (tablero (estado nodo)) (entre posicion (buitre (estado nodo)))) 'C)) ;;; Que entre la posicion antigua del buitre y la nueva exista un cuervo al que comerse 

	 (setf (aref (tablero (estado nodo)) posicion) 'B ;;; Colocamos el buitre en la posicion indicada
	       (aref (tablero (estado nodo)) (buitre (estado nodo))) 'O ;;; Borramos de la antigua posicion el buitre
	       (turno nodo) 'C ;;; cambiamos el turno
               (aref (tablero (estado nodo)) (entre posicion (buitre (estado nodo)))) 'O ;;; Poner vacia la posicion del cuervo comido
	       (comidos (estado nodo)) (+ (comidos (estado nodo)) 1) ;;; Actualizar los cuervos comidos
	       (buitre (estado nodo)) posicion) ;;; Actualizamos posicion del buitre
          nodo)
	(t nil)))

;;;Esta funcion hace lo mismo que comer cuervo pero NO cambia el turno, por si en una misma jugada el buitre se puede comer a mas de un cuervo
(defun comer-cuervo-aux (nodo posicion) ;;; posicion a la que salta el buitre
   (cond ((and (eq (turno nodo) 'B) ;;; Turno de B
               (eq (aref (tablero (estado nodo)) posicion) 'O) ;;; Que la posicion a la que vamos a mover este libre 
	       (buitre (estado nodo)) ;;; Que el buitre este colocado
	       (entre posicion (buitre (estado nodo))) ;;; Que la posicion a la que vamos a mover el buitre este en linea con la antigua
	       (eq (aref (tablero (estado nodo)) (entre posicion (buitre (estado nodo)))) 'C)) ;;; Que entre la posicion antigua del buitre y la nueva exista un cuervo al que comerse 

	 (setf (aref (tablero (estado nodo)) posicion) 'B ;;; Colocamos el buitre en la posicion indicada
	       (aref (tablero (estado nodo)) (buitre (estado nodo))) 'O ;;; Borramos de la antigua posicion el buitre
               (aref (tablero (estado nodo)) (entre posicion (buitre (estado nodo)))) 'O ;;; Poner vacia la posicion del cuervo comido
	       (comidos (estado nodo)) (+ (comidos (estado nodo)) 1) ;;; Actualizar los cuervos comidos
	       (buitre (estado nodo)) posicion) ;;; Actualizamos posicion del buitre
          nodo)
	(t nil)))

;;;--------------------------------------------------------------------
;;; ALGORITMO MINIMAX CON PODA ALFA BETA

;;; Variables con minimo valor y maximo valor
(defparameter *max-valor* 1000)
(defparameter *min-valor* -1000)

;;; Definicion de estado inicial
(defparameter *nodo-inicial* (crear-nodo-inicial))

;;; Indica si es estado final y devuelve si gana 'B o 'C
(defun es-estado-final (estado)
  (cond ((not (buitre estado)) nil)
        ((> (comidos estado) 3) 'B) ;;; Si el numero de cuervos comidos es mayor que tres gana buitre
	((and (eq (movimientos (tablero estado) (buitre estado)) 0) ;;; Si el buitre no puede hacer ningun movimiento
	      (eq (saltos (tablero estado) (buitre estado)) 0)) 'C) ;;; Ni ningun salto, entonces ganan cuervos
	(t nil)))

(defun genera-movimientos (nodo)
  (let ((res))
  (if (eq (turno nodo) 'C) ;;; Cuando el turno es de cuervo hay dos posibilidades poner-cuervo o mover-cuervo
      (cond ((< (cuervos (estado nodo)) 7) ;;; Si hay cuervos por poner --> poner-cuervo
	     (setf res ;;; Asignamos a res el resultado siguiente
		   (loop for x from 0 to 9 ;;; Recorremos el tablero
		      when (eq (aref (tablero (estado nodo)) x) 'O) ;;; Cuando la posicion este vacia, podemos
		      collect (list 'poner-cuervo (list x))))) ;;; aplicar poner-cuervo (x)
	    (t (setf res ;;; En caso de que se hayan colocado todos los cuervos --> mover-cuervo
		     (loop for x from 0 to 9 ;;; Recorremos el tablero
			  when (eq (aref (tablero (estado nodo)) x) 'C) ;;; Cuando encontremos un cuervo
			  append
			  (loop for y in (lista-adyacentes x) ;;; Miramos sus adyacentes
			       when (eq (aref (tablero (estado nodo)) y) 'O) ;;; Cuando sean vacios
			       collect (list 'mover-cuervo (list x y))))))) ;;; Es un posible movimiento
      ;;; Cuando el turno es de buitre hay tres posibilidades poner-buitre, mover-buitre y comer-cuervo
      (cond ((not (buitre (estado nodo))) ;;; Cuando el buitre no esta colocado
	     (setf res ;;; Asignamos a res el resultado siguiente
		   (loop for x from 0 to 9 ;;; Recorremos el tablero
		      when (eq (aref (tablero (estado nodo)) x) 'O) ;;; Cuando la posicion este vacia, 
		      collect (list 'poner-buitre (list x))))) ;;; podemos aplicar poner-buitre (x)
	    (t (setf res ;;; En el caso de que ya este colocado el buitre --> mover-buitre y comer-cuervo
		     (loop for x in (lista-adyacentes (buitre (estado nodo))) ;;; Miramos las posiciones adyacentes al la del buitre colocado
			  when (eq (aref (tablero (estado nodo)) x) 'O) ;;; Cuando esta posicion adyacente esta vacia
			  collect (list 'mover-buitre (list x)))) ;;; Entonces podemos aplicar mover-buitre
	      (setf res 
		    (append res ;;; Unimos mover-buitre con comer-cuervo
			    (mov-saltos nodo))))))))



;;; Funcion aplica-movimiento(movimiento, nodo) que recibe como argumento un movimiento y un nodo y devuelve el nodo resultante de aplicar ese movimiento
(defun aplica-movimiento (movimiento nodo)
  (let ((nombre (first movimiento)) (arg (second movimiento))) ;;; separamos los nombres de los movimientos de sus argum
    (if (eq nombre 'comer-cuervo) ;;; Si el mov es comer-cuervo
	(loop for x from 0 to (- (length arg) 1) ;;; recorremos sus arg
	     do (if (eq x (- (length arg) 1)) ;;; si es el ultimo
		    (apply (symbol-function nombre) (list nodo (nth x arg))) ;;; aplicamos el movim comer-cuervo cambiando el turno
		    (apply #'comer-cuervo-aux (list nodo (nth x arg))))) ;;; si no es el ultimo arg entonces aplicamos el movimiento comer-cuervo-aux que no te cambia el turno
	(apply (symbol-function nombre) (append (list nodo) arg)))nodo)) ;;; si es cualquier otro mov entonces lo aplicamos con sus argumentos correspondientes añadiendole el nodo como una lista para que no de error en el append



;;; Funcion sucesor(nodo, movimiento)
;;; Esta funcion no es util para nuestra implementacion ya que en *movimientos* solo tenemos los movimientos que se pueden aplicar

;;; Funcion sucesores(nodo), Esta funcion nos devuelve una lista con los sucesores del nodo(lista de nodos), que seran los nodos resultantes de aplicar los movimientos
(defun sucesores (nodo)
  (loop for x in (genera-movimientos nodo) collect 
       (aplica-movimiento x (clonar nodo))))

;;; Funcion de evaluacion estatica, dependiendo de que ficha tenga la maquina elegimos una u otra funcion
(defun f-e-estatica (estado maquina)
  (if (eq maquina 'B)
      (f-e-b estado)
      (f-e-c estado)))

;;; Funcion f-e-b(estado), del buitre
(defun f-e-b (estado)
  (cond ((eq (es-estado-final estado) 'B) *max-valor*)
	((eq (es-estado-final estado) 'C) *min-valor*)
	(t (+ (* (comidos estado) 20) ;;; Los cuervos comidos 
	   (* (movimientos (tablero estado) (buitre estado)) 5) ;;; los posibles movimientos del buitre
	   (* (saltos (tablero estado) (buitre estado)) 100))))) ;;; los posibles saltos

;;; Funcion f-e-c(estado), de los cuervos
(defun f-e-c (estado)
  (cond ((eq (es-estado-final estado) 'C) *max-valor*)
	((eq (es-estado-final estado) 'B) *min-valor*)
	(t (+ (* (comidos estado) -100) ;;; Los cuervos comidos penalizan
	   (* (- 4 (movimientos (tablero estado) (buitre estado))) 100) ;;; Cuantos menos movimientos tenga el buitre mejor
	   (* (- 2 (saltos (tablero estado) (buitre estado))) 100))))) ;;; Cuantos menos saltos tenga el buitre mejor

;;; Funcion f-e-c2(estado), de los cuervos, solo intentamos que se pongan juntos para que no se los coma el buitre
(defun f-e-c2 (estado)
  (cond ((eq (es-estado-final estado) 'C) *max-valor*)
	((eq (es-estado-final estado) 'B) *min-valor*)
	(t (loop for x below 10 when (eq (aref (tablero estado) x) 'C) summing (loop for y in (lista-adyacentes x) when (eq (aref (tablero estado) y) 'C) summing 10)))))


;;; Funcion maximizador-A-B-C (sucesores, profundidad, alfa, beta) Como es turno de max cogemos el nodo con mayor valor

;;; 2. Por cada NODO en SUCESORES,
;;;    2.1 Hacer VALOR-ACTUAL igual VALOR-MINIMAX-A-B(NODO,PROFUNDIDAD,ALFA,BETA)
;;;    2.2 Si VALOR-ACTUAL > ALFA, hacer ALFA igual a VALOR-ACTUAL
;;;    2.3 Si ALFA>=BETA, salir del bucle ;;; <--- poda alfa  Como nosotros tenemos que elegir aleatoriamente lo ponemos estrictamente mayor
;;; 3. Devolver ALFA
(defun maximizador-A-B (sucesores profundidad alfa beta &optional (f-e nil))
  (loop for x in sucesores do ;;; 2.
       (let ((valor-actual (valor-minimax-A-B x profundidad alfa beta (if f-e f-e)))) ;;; 2.1
	 (if (> valor-actual alfa) ;;; 2.2
	     (setf alfa valor-actual)) ;;; 2.2
	 (if (> alfa beta) ;;; 2.3
	     (return)))) ;;; 2.3
  alfa) ;;; 3.


;;; Funcion minimizador-A-B-C (sucesores, profundidad, alfa, beta) Como es turno de min cogemos el nodo con menor valor

;;; 2. Por cada NODO en SUCESORES,
;;;    2.1 Hacer VALOR-ACTUAL igual VALOR-MINIMAX-A-B(NODO,PROFUNDIDAD,ALFA,BETA)
;;;    2.2 Si VALOR-ACTUAL < BETA, hacer BETA igual a VALOR-ACTUAL
;;;    2.3 Si ALFA>=BETA, salir del bucle ;;; <--- poda beta  
;;; 3. Devolver BETA
(defun minimizador-A-B (sucesores profundidad alfa beta &optional (f-e nil))
  (loop for x in sucesores do ;;; 2.
       (let ((valor-actual (valor-minimax-A-B x profundidad alfa beta (if f-e f-e)))) ;;; 2.1
	 (if (< valor-actual beta) ;;; 2.2
	     (setf beta valor-actual)) ;;; 2.2
	 (if (> alfa beta) ;;; 2.3
	     (return)))) ;;; 2.3
  beta) ;;; 3.


;;; Funcion valor-minimax-A-B-C(nodo, profundidad, Alfa, Beta) Esta funcion asigna un valor al nodo

;;; 1. Si ES-ESTADO-FINAL(ESTADO(NODO)) o
;;;    PROFUNDIDAD=0 o
;;;    SUCESORES(NODO) igual a vac´ıo,
;;;   1.1 devolver F-E-ESTATICA(ESTADO(NODO),JUGADOR(NODO))
;;; 2. Si no, si JUGADOR(NODO)=MAX, devolver
;;;   2.1 MAXIMIZADOR-A-B(SUCESORES(NODO),PROFUNDIDAD-1,ALFA,BETA)
;;; 3. Si no, devolver MINIMIZADOR-A-B(SUCESORES(NODO),PROFUNDIDAD-1,ALFA,BETA)
(defun valor-minimax-A-B (nodo profundidad alfa beta &optional (f-e nil))
  (if (or (es-estado-final (estado nodo)) (eq profundidad 0) (eq (sucesores nodo) nil)) ;;; 1.
      (if f-e ;;; si introducimos como argumento la f-e entonces usamos esa, sino usamos la f-e-estatica
	  (funcall (symbol-function f-e) (estado nodo)) ;;; 1.1
	  (f-e-estatica (estado nodo) (maquina nodo)))
      (if (eq (turno nodo) (maquina nodo)) ;;; 2.
	  (maximizador-A-B (sucesores nodo) (1- profundidad) alfa beta (if f-e f-e)) ;;; 2.1
	  (minimizador-A-B (sucesores nodo) (1- profundidad) alfa beta (if f-e f-e))))) ;;; 3.
      

;;; Funcion decision-minimax-A-B-C(nodo, produndidad) Esta funcion nos dice que movimiento es mejor para intentar ganar la partida

;;; 1. Hacer ALFA igual a *MINIMO-VALOR*
;;; 2. Por cada NODO en SUCESORES(ACTUAL),
;;;    2.1 Hacer VALOR-ACTUAL igual a
;;;        VALOR-MINIMAX-A-B(NODO,PROFUNDIDAD-1,ALFA,*MAXIMO-VALOR*)
;;;    2.2 Si VALOR-ACTUAL > ALFA,
;;;        hacer ALFA igual a VALOR-ACTUAL y hacer MAX-NODO igual a NODO
;;;    2.3 Si ALFA >= *MAXIMO-VALOR*, salir del bucle ;;; <--- poda alfa
;;; 3. Devolver MAX-NODO 
;;; Para que seleccione aleatoriamente un nodo entre varios de igual valor lo que hacemos es eliminar las podas cuando alfa y beta son iguales, y solo tenemos en cuenta para podar cuando alfa es mayor estricto que beta
(defun decision-minimax-A-B (nodo profundidad &optional (f-e nil))
  (let ((alfa *min-valor*) (max-nodos)) ;;; 1.
    (loop for x in (sucesores nodo) do ;;; 2.
	 (let ((valor-actual (valor-minimax-A-B x (1- profundidad) alfa *max-valor* (if f-e f-e)))) ;;; 2.1
	   (if (> valor-actual alfa) ;;; 2.2
	       (setf alfa valor-actual max-nodos (list x)) ;;; 2.2 Reinicializamos la lista max-nodo con el nodo maximo x, por si anteriormente habia algun otro nodo con un valor inferior 
	       (if (eq valor-actual alfa) ;;; Para seleccionar aleatoriamente un nodo si estos tiene el mismo valor de alfa
		   (setf max-nodos (append max-nodos (list x))))) ;;; añadimos en la lista max-nodos los nodos de valor maximo
	   (if (>= alfa *max-valor*) ;;; 2.3
	       (return)))) ;;; 2.3
    (selecciona-aleatorio max-nodos))) ;;; 3.


;;; Funcion (partidas-auto jug-b, f-e-b, f-e-c) 

(defun partidas-auto (jug-b f-e-b f-e-c) ;;; jug-b juega con los buitre
  (let ((fichero (open "partidas.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create)) ;;; creamos el fichero partidas.txt
	(nodo-actual (crear-nodo-inicial 'C)) ;;; nodo-actual es crear-nodo-inicial con el turno de C'
	(movimientos 0) ;;; contador de movimientos
	(jug-c (if (eq jug-b 'MAX) 'MIN 'MAX))) ;;; Asignamos a jug-b min o max y el contrario a jug-c
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
	  (format t "~%LA PARTIDA ACABA EN EMPATE ")) ;;; imprimimos ...
	  ((eq (es-estado-final (estado nodo-actual)) 'B) ;;; Si es estado final y gana B entonces
	   (format t "~%GANA EL JUGADOR ~a " jug-b)) ;;; imprimimos ...
	  (t (format t "~%GANA EL JUGADOR ~a " jug-c)))'COLEGA))
  
;;;----------------
;;; EXPERIMENTACION
;;;----------------

;;; FUNCIONES DE EVALUACION ESTATICA ENFRENTANDOSE ENTRE SI

;;; HUMANO VS MAQUINA

(defun humano-maquina (maquina) ;;; Recibe como argumento con que ficha juega la maquina
  (let ((nodo-actual (crear-nodo-inicial maquina))(movimientos)(mov-jug)(revancha) (mov 0))
    (format t "Juego de los cuervos~%Tablero Inicial:~%~a" nodo-actual) ;;; Imprime el nodo inicial
    (loop until (es-estado-final (estado nodo-actual)) do ;;; Hasta que no es estado final, hacer
	 (cond ((eq (turno nodo-actual) maquina) ;;; Si el turno es de la maquina
		(setf nodo-actual (decision-minimax-a-b nodo-actual (cond ((< mov 10) 3) ((< mov 50) 4) ((< mov 100) 5) (t 6))) ;;; Decide movimiento, incrementamos profundidad dependiendo del nº de movimientos y lo aplica
		      mov (1+ mov)) ;;; incrementamos el nº de movimientos
		(format t "~&Movimiento ~a:~% La maquina mueve a: ~%~a" mov nodo-actual)) ;;; Imprime el movimiento de la maquina
	       
	       (t (setf movimientos (genera-movimientos nodo-actual)  ;;; movimientos es la lista de movimientos posibles
			mov (1+ mov)) ;;; incrementamos el numero de movimientos
		  (format t "~&Te toca mover, los movimientos posibles son:") ;;; Imprime los movimientos posibles
		  (loop for x below (length movimientos) do ;;; Recorremos la lista de movimientos
		       (format t "~&~a. ~a" x (nth x movimientos))) ;;; Imprimimos el movimiento acompañado de un numero
		  (format t "~&Introduce el numero de tu movimiento o R para rendirte: ") ;;; El jugador introduce el movimiento elegido o R si se rinde
		  (setf mov-jug (read t)) ;;; en mov-jug guardamos el movimiento que elige el jugador
		  (if (eq mov-jug 'R) (return)) ;;; si se rinde entonces PARA 
		  (loop until (< mov-jug (length movimientos)) do ;;;Si el numero de movimiento introducido no existe entonces 
		       (format t "~&Numero Incorrecto, introduzcalo de nuevo:") ;;; Imprimimos que vuelvas a introducirlo
		       (setf mov-jug (read t))) ;;; volvemos a cambiar mov-jug por el movimiento correcto elegido
		  (aplica-movimiento (nth mov-jug movimientos) nodo-actual) ;;; aplicamos el movimiento al nodo actual
		  (format t "~&Movimiento ~a:~%Has movido a:~%~a" mov nodo-actual)))) ;;; imprimimos el nodo con el movimiento aplicado
    (if (or (eq (es-estado-final (estado nodo-actual)) maquina) (not (es-estado-final (estado nodo-actual)))) ;;; Si la maquina ha llegado al estado final(la maquina ha ganado) o si no es estado final (por si elegimos rendirnos)
	(format t "~&HAS PERDIDO, ¿REVANCHA? (S/N)") ;;; entonces el jugador pierde y tambien podemos elegir la revancha
	(format t "~&HAS GANADO, FELICIDADES ¿REVANCHA? (S/N)")) ;;; sino el jugador gana y tambien podemos elegir la revancha
    (setf revancha (read t)) ;;; en revancha se guarda Si o No
    (if (eq revancha 'S) ;;; Si queremos revancha entonces
	(humano-maquina maquina) ;;; Volvemos a ejecutar el algoritmo
	(format t "~&HASTA OTRA ")))'COLEGA) ;;; Si no, se acaba



;;; MAQUINA INTELIGENTE VS MAQUINA ALEATORIA

(defun maquina-borracha (maquina);;; el argumento de entrada es con la ficha que juega la maquina inteligente
  (let ((nodo-actual (crear-nodo-inicial maquina))(movimientos) (mov 0))
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
	(format t "~&HA GANADO LA MAQUINA INTELIGENTE ")) ;;; Imprimimos ...
	  ((<= 150 mov) ;;; Si el nº d movimientos es mayor q 150, empate
	   (format t "~%LA PARTIDA ACABA EN EMPATE "))
	  (t  ;;;Si no gana la otra maquina
	(format t "~&HA GANADO LA MAQUINA BORRACHA, INCREIBLE "))
	 )'COLEGA))


  
;;; PARTIDAS DE 2 JUGADORES


(defun partida2jugadores (&optional (nombre-jugb 'Buitre) (nombre-jugc 'Cuervos))
  (let ((nodo-actual (crear-nodo-inicial))(movimientos)(mov-jug)(revancha) (mov 0))
    (format t "Juego de los cuervos~%Tablero Inicial:~%~a" nodo-actual) ;;; Imprime el nodo inicial
    (loop until (es-estado-final (estado nodo-actual)) do ;;; Hasta que no es estado final, hacer
	       (format t "~&Turno de ~a:" (if (eq (turno nodo-actual) 'C) nombre-jugc nombre-jugb)) ;;; Imprime que le toca a jugc o a jugb
	       
	       (setf movimientos (genera-movimientos nodo-actual)  ;;; movimientos es la lista de movimientos posibles
			mov (1+ mov)) ;;; incrementamos el numero de movimientos
	       (format t "~&Los movimientos posibles son:") ;;; Imprime los movimientos posibles
	       (loop for x below (length movimientos) do ;;; Recorremos la lista de movimientos
		    (format t "~&~a. ~a" x (nth x movimientos))) ;;; Imprimimos el movimiento acompañado de un numero
	       (format t "~&Introduce el numero de tu movimiento o R para rendirte: ") ;;; El jugador introduce el movimiento elegido o R si se rinde
	       (setf mov-jug (read t)) ;;; en mov-jug guardamos el movimiento que elige el jugador
	       (if (eq mov-jug 'R) (return)) ;;; si se rinde entonces PARA 
	       (loop until (< mov-jug (length movimientos)) do ;;;Si el numero de movimiento introducido no existe entonces 
		    (format t "~&Numero Incorrecto, introduzcalo de nuevo:") ;;; Imprimimos que vuelvas a introducirlo
		    (setf mov-jug (read t))) ;;; volvemos a cambiar mov-jug por el movimiento correcto elegido
	       (aplica-movimiento (nth mov-jug movimientos) nodo-actual) ;;; aplicamos el movimiento al nodo actual
	       (format t "~&Movimiento ~a:~%Has movido a:~%~a" mov nodo-actual)) ;;; imprimimos el nodo con el movimiento aplicado
    (if (or (eq (es-estado-final (estado nodo-actual)) 'C) (and (not (es-estado-final (estado nodo-actual))) (eq (turno nodo-actual) 'B))) ;;; Si es estado final vemos quien gana, sino es estado final vemos a quien le toca mover y ese sera el que se ha rendido.
	(format t "~&HA GANADO ~a, ¿REVANCHA? (S/N)" nombre-jugc) ;;; entonces el jugadorc gana y tambien podemos elegir la revancha
	(format t "~&HA GANADO ~a, ¿REVANCHA? (S/N)" nombre-jugb)) ;;; sino el jugadorb gana y tambien podemos elegir la revancha
    (setf revancha (read t)) ;;; en revancha se guarda Si o No
    (if (eq revancha 'S) ;;; Si queremos revancha entonces
	(partida2jugadores nombre-jugb nombre-jugc) ;;; Volvemos a ejecutar el algoritmo
	(format t "~&HASTA OTRA ")))'COLEGA) ;;; Si no, se acaba