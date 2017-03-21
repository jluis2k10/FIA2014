% Archivo: trafico.pl
%
% Este programa  modela un cruce de carreteras, donde cada carril de una
% carretera tiene un semáforo que puede estar en verde o en rojo, permitiendo
% o impidiendo circular coches respectivamente.
% El usuario deberá introducir el número de carriles de los que se compone el
% cruce, la cantidad de coches que están en cada carril al comienzo de la
% operación, y qué semáforos no pueden estar en verde simultáneamente.
%
% @Autor: José Luis Pérez González
% @Fecha: Abril 2015
% @Licencia: GPL
%
% "go."   comienza la ejecución.
% "demo." comienza una ejecución de demostración sin preguntar los hechos al
%         usuario.

:- dynamic
	carril/4,		% Situación de un carril del cruce.
	estado/2,		% Combinación óptima de los semáforos del cruce.
	impide_verde/2,	% Semáforos que no pueden estar en verde simultáneamente.
					% impide_verde(S1,S2) => S2 no puede estar en verde si S1
					% está en verde.
	t_cambio/1.		% Tiempo que un semáforo se mantiene en un color. Supondre-
					% mos que por cada unidad de tiempo que está en verde, pue-
					% de circular un único coche por el carril que controla.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 		BASE DE HECHOS PARA DEMOSTRACIÓN. 		%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Estos hechos modelan el siguiente cruce:
%
% _________________________________________________
%                                   (c) 20C    6
%    (c)
%                                   (a) 80C    5
% ---------------                   ---------------
%  1    30C   (b)
%                                            (b)
%  2    30C   (a)
% ______________                     ______________
%               |         | (c) (b) |
%               |         |         |
%               |         | 40C 60C |
%               |         |         |
%               |   (a)   |  3   4  |
%               |         |         |
%
% Donde X es el identificador del carril, YC el número de coches que están re-
% tenidos en el carril, y (z) la dirección a la que se debe ir desde un carril.

% carril(ID,coches,tiempo_esperando,semaforo).
carril(1,30,0,rojo).
carril(2,30,0,rojo).
carril(3,40,0,rojo).
carril(4,60,0,rojo).
carril(5,80,0,rojo).
carril(6,20,0,rojo).
% Estados óptimos. estado(ID,[Lista con estado de los semáforos]).
estado(1,[verde,verde,rojo,rojo,rojo,verde]).
estado(2,[rojo,verde,verde,verde,rojo,rojo]).
estado(3,[rojo,verde,rojo,verde,rojo,verde]).
estado(4,[rojo,rojo,rojo,verde,verde,verde]).
% Tiempo semáforo. Cada unidad de tiempo en verde permite que circule 1 coche.
t_cambio(5).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 		BASE DE CONOCIMIENTO. 		%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% arbol_binario(+N:num, -A:arbol)
%
% Construye un árbol binario de N niveles. Cada nodo hijo izquierdo será
% 'verde' y cada nodo hijo derecho será 'rojo'.
% Se obtiene así un árbol en el que cada rama representa una combinación de
% los semáforos en el cruce, con todas las combinaciones que pueden darse.
% Por ejemplo, para dos semáforos (3 niveles), tendremos:
%
%	                          raiz
%		S1:         verde               rojo
%		S2:    verde     rojo      verde    rojo
%
% Ojo, el nodo raíz no cuenta. Si queremos construir un árbol para las
% combinaciones de 6 semáforos, deberíamos utilizar el predicado como
% arbol(7,MiArbol).
%
% @param N el número de niveles del árbol.
% @param A (arbol(_,I,D)) el árbol generado.
arbol_binario(N,arbol(raiz,I,D)) :-			% El primer nodo es el nodo raíz.
	N > 0,
	NN is N - 1,
	arbol_binario(NN,verde,I),
	arbol_binario(NN,rojo,D).
arbol_binario(N,Color,arbol(Color,I,D)) :-
	N > 0,
	NN is N - 1,
	arbol_binario(NN,verde,I),
	arbol_binario(NN,rojo,D).
arbol_binario(0,_,nil) :- !.				% No más backtrack.


%% caminos_optimos(+Arbol:arbol, -Caminos:[[atom]])
%
% Encuentra los caminos o estados óptimos de configuración para el conjunto de
% todos los semáforos del cruce.
%
% Un estado o camino óptimo es aquél que es posible y además máximo, siendo:
%  - Estado posible: no contiene dos semáforos en verde simultáneamente si
%    existe un hecho que lo impida.
%  - Estado máximo: no existe otro estado posible que tenga los mismos semáfo-
%    ros en verde y además algún otro/s también en verde. Por ejemplo, si el
%    estado [verde,verde,rojo] es posible, el estado [verde,rojo,rojo] NO es
%    máximo puesto que el 2º semáforo podría estar en verde sin rompor ninguna
%    regla.
%
% @param Arbol el árbol que representa todas las combinaciones de los semáfo-
%		 ros.
% @param Caminos una lista que contiene todos los caminos óptimos encontrados.
caminos_optimos(Arbol,Caminos) :-
	findall(
		Camino,
		caminos_posibles(Arbol,Camino),
		CaminosPosibles									% CaminosPosibles es una lista con todos los caminos posibles del árbol.
	),
	findall(
		Camino,
		(
			member(Camino,CaminosPosibles),
			select(Camino,CaminosPosibles,Posibles2),	% No comparar consigo mismo.
			no_pertenece(Camino,Posibles2)
		),
		Caminos											% Caminos es una lista de todos los caminos óptimos.
	).


%% caminos_posibles(+A:arbol, -Ns:[atom])
%
% Recorre en profundidad y de izquierda a derecha (pre-order) el árbol y en-
% cuentra todos los caminos o ramas que conforman un estado posible.
%
% @param arbol(_,I,D) el árbol sobre el que buscar estados posibles.
% @param Ns una lista que contiene uno de los caminos encontrados.  
caminos_posibles(arbol(N,nil,nil),[N]) :- !.
caminos_posibles(arbol(raiz,I,D),Ns) :-		% Entrada.
	(caminos_posibles(I,Ns)					% Separar en dos árboles eliminando el nodo raiz del original.
	;caminos_posibles(D,Ns)),
	es_posible(Ns).
caminos_posibles(arbol(N,I,_D),[N|Ns]) :-
	N \== raiz,								% Evitar que se comience por este predicado.
	caminos_posibles(I,Ns).
caminos_posibles(arbol(N,_I,D),[N|Ns]) :-
	N \== raiz,								% Ídem.
	caminos_posibles(D,Ns).


%% es_posible(+Camino:[atom])
%
% Pregunta si un camino es un estado posible según las reglas definidas en
% impide_verde/2.
%
% Comprueba si la combinación de colores (i,j) es válida, donde i es el elemen-
% to i-ésimo de la lista, y j es el elemento j-ésimo. Para obtener (i,j) se
% pregunta a impide_verde/2.
%
% Por ejemplo, si la lista es [verde,rojo,rojo] y las reglas son
% impide_verde(1,2) e impide_verde(2,3), se comprobarán los pares (verde,rojo)
% y (rojo,rojo).
%
% @param Camino el camino o estado que se quiere comprobar.
es_posible(Camino) :-
	forall(
		(impide_verde(S1,S2),nth1(S1,Camino,Color1),nth1(S2,Camino,Color2)),
		once(
			Color1 == rojo						% Semáforo rojo, da igual el estado de los demás.
			;(Color1 == verde,Color2 == rojo)	% Semáforo verde, comprobar que el que debe estar en rojo según
		)										% la regla de impide_verde/2 está efectivamente en rojo. 
	).


%% no_pertenece(+P:[atom], +C[[atom]])
%
% Pregunta si un estado no pertenece (no es "subestado") a ninguno de los
% estados de la lista de estados posibles.
%
% @param P estado a comprobar.
% @param C lista de estados posibles con los que realizar la comparación.
no_pertenece(_,[]) :- !.
no_pertenece(P,[C|Cs]) :-
	\+ subestado(P,C),
	no_pertenece(P,Cs),!.


%% subestado(+P:[atom], +C:[atom])
%
% Pregunta si un estado P es "subestado" de otro C. Para que un estado sea
% subestado de otro, todos los elementos 'verde' del primero deben encontrarse
% también en el segundo, y además el segundo debe tener algún elemto 'verde'
% más.
%
% Por ejemplo, [verde,rojo,rojo] es subestado de [verde,verde,rojo] pero no de
% [rojo,rojo,rojo].
%
% @param P estado a comprobar.
% @param C estado con el que comparar.
subestado([],[]).
subestado([verde|Ps],[verde|Cs]) :-
	subestado(Ps,Cs).
subestado([rojo|Ps],[_|Cs]) :-
	subestado(Ps,Cs).


%% caminos_a_hechos(+Caminos:[[atom]])
%
% Convierte el conjunto de estados óptimos obtenido a un conjunto de reglas
% estado/2 que serán usadas posteriormente por otros predicados.
%
% @param Caminos la lista de estados óptimos obtenida.
caminos_a_hechos(Caminos) :-
	retractall(estado(_,_)),			% Eliminar anteriores.
	caminos_a_hechos(1,Caminos).		% Los identificadores de estado son enteros correlativos.
caminos_a_hechos(_,[]).
caminos_a_hechos(N,[Camino|Resto]) :-
	assert(estado(N,Camino)),
	NN is N + 1,
	caminos_a_hechos(NN,Resto).


%% estado_prioritario(-E:num,-Pr:num)
%
% Obtiene el identificador del estado más prioritario junto con la prioridad
% del mismo.
%
% @param E el identificador del estado más prioritario.
% @param Pr la prioridad de dicho estado.
estado_prioritario(E, Pr) :-
	estado(E,Sems),
	prioridad(Sems,Pr),
	\+ (						% Si Pr2 > Pr para todos los estados, esto
		estado(_,Sems2),		% resulta falso, con lo que se produce un
		prioridad(Sems2,Pr2),	% backtracking y se selecciona un nuevo estado
		Pr2 > Pr				% E con otra Pr.
	).


%% prioridad(+Sems:[atom],-Pr:num)
%
% Calcula la prioridad de un estado sumando todas las prioridades de los carri-
% les que pasarían a estar con su semáforo en verde en dicho estado.
%
% @param Sems lista de semáforos que compone el estado.
% @param Pr la prioridad del estado correspondiente a la lista.
prioridad(Sems,Pr) :-
	prioridad(1,Sems,0,Pr).
prioridad(_,[],Pr,Pr).
prioridad(ID,[verde|Sems],Ac,Pr) :-
	carril(ID,Coches,Tiempo,_),
	(
		Coches > 0
		-> NAc is Ac + Coches + (Tiempo*Tiempo)/100
		;  NAc is Ac
	),
	NID is ID + 1,
	prioridad(NID,Sems,NAc,Pr).
prioridad(ID,[rojo|Sems],Ac,Pr) :-
	NID is ID + 1,
	prioridad(NID,Sems,Ac,Pr).

		
%% circular(+Sem:[atom])
%
% Actualiza el estado de los carriles en función de si su semáforo correspon-
% diente está en verde o no.
%
% @param Sems Estado en el que se encuentra el cruce (combinación de semáfo-
%		 ros).
circular(Sems) :-
	circular(1,Sems).						% Empezar por el primer carril!
circular(_,[]).	
circular(ID,[verde|Sems]) :-				% Semáforo en verde, - coches.
	carril(ID,Coches,_,_),
	t_cambio(T),
	NCoches is max(Coches - T, 0),			% No puede haber coches negativos.
	retract(carril(ID,_,_,_)),				% Actualizar hecho.
	assert(carril(ID,NCoches,0,verde)),
	NID is ID + 1,
	circular(NID,Sems).
circular(ID,[rojo|Sems]) :-					% Semáforo en rojo, + tiempo.
	carril(ID,Coches,Tiempo,_),
	t_cambio(T),
	NTiempo is Tiempo + T,
	retract(carril(ID,_,_,_)),				% Actualizar hecho.
	assert(carril(ID,Coches,NTiempo,rojo)),
	NID is ID + 1,
	circular(NID,Sems).


%% quedan_coches
%
% Pregunta si quedan coches en todo el conjunto del cruce.
quedan_coches :-
	findall(Coches,carril(_,Coches,_,_),LCoches),
	sum_list(LCoches,TotCoches),	% Necesita library(lists). Autocarga.
	TotCoches > 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 		MECANISMO DE INFERENCIAS. 		%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% dirigir_trafico
%
% Predicado que resuelve el problema de en qué orden deben ir sucediéndose
% los diferentes estados para que los coches circulen y vayan abandonando su
% carril de manera equitativa.
dirigir_trafico :-
	\+ quedan_coches, !,
	nl,write('*** OPERACION TERMINADA ***'),nl.
dirigir_trafico :-
	quedan_coches,!,
	estado_prioritario(E,Pr),!,						% Si hay empate de prioridades sólo es necesaria una, backtrack aquí innecesario.
	estado(E,Sems),
	circular(Sems),									% Acualizar estado del cruce según los semáforos.
	nl,write('Estado escogido: '),write(E),
	write(' (Prioridad = '),write(Pr),write(') -> '),
	write(Sems),
	nl,write('Situación en el cruce tras circular:'),
	nl,print_cruce,
	get0(_),										% Espera pulsación de intro.
	dirigir_trafico.								% Nueva iteración.


%% demo
%
% Resuelve el problema usando la base de conocimientos o hechos de demostra-
% ción.
% Si se ha ejecutado el programa anteriormente la base habrá cambiado por la
% utilizada en dicha ejecución. 
demo :-
	print_estados,nl,
	write('Situación inicial en el cruce:'),nl,print_cruce,
	write('  >¿Continuar? (s/n) '),
	si_o_no,nl,
	dirigir_trafico,fail.


%% go
%
% Resuelve el problema a partir de los datos proporcionados por el usuario.
go :-
	write('¿Cuántos carriles tiene el cruce?'),nl,
	write('  >Carriles en el cruce '),
	read_line_to_codes(user_input,In),
	atom_codes(Atom,In),
	atom_number(Atom,Carriles),
	Carriles > 0,
	nl,write('¿Cuántos coches se encuentran en cada carril del cruce?'),nl,
	leer_coches_en_carril(Carriles),
	nl,write('Si el semáforo de un carril está en verde, ¿qué otros semá-'),nl,
	write('foros no pueden estar también en verde a la vez que éste?'),nl,
	write('Escriba la lista de los semáforos separados mediante comas (,).'),
	nl,
	retractall(impide_verde(_,_)),	% Borrar anteriores.
	leer_reglas_sems(Carriles),
	nl,write('Indique el tiempo que permanecen los semáforos en un color.'),nl,
	write('(Tenga en cuenta que por cada unidad de tiempo que permanece en '),
	write('verde, puede circular un coche).'),nl,
	leer_tiempo_cambio,
	NivelesArbol is Carriles + 1,
	arbol_binario(NivelesArbol,Arbol),
	caminos_optimos(Arbol,Caminos),
	caminos_a_hechos(Caminos),
	nl,write('Ya disponemos de todo lo necesario.'),nl,
	print_estados,
	!,write('  >¿Continuar? (s/n) '),
	si_o_no,nl,
	write('Situación inicial en el cruce:'),nl,print_cruce,
	dirigir_trafico,
	!,write('  >¿Reiniciar? (s/n) '),
	si_o_no,nl,
	go.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 		INTERACCIÓN CON EL USUARIO. 		%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% leer_coches_en_carril(+Carril:num)
%
% Lee un número tecleado por el usuario y crea un hecho nuevo en carril/4
% con el número de coches que tiene un carril determinado.
%
% @param Carriles el número total de carriles que tiene el cruce.
leer_coches_en_carril(Carriles) :-
	retractall(carril(_,_,_,_)),
	leer_coches_en_carril(1,Carriles).		% En caso de error de entrada necesitamos saber en qué carril se ha producido.
leer_coches_en_carril(C,Carriles) :-
	C > Carriles,!.
leer_coches_en_carril(C,Carriles) :-
	C =< Carriles, !,
	write('  >Coches en carril '),write(C),write(' '),
	read_line_to_codes(user_input,In),
	atom_codes(Atom,In),
	(
		atom_number(Atom,Coches)
		-> assert(carril(C,Coches,0,rojo)), NC is C + 1
		; write('*** ERROR ***'),nl, NC is C
	),
	leer_coches_en_carril(NC, Carriles).

	
%% leer_reglas_sems(+Carril:num)
%
% Lee un número o lista de números separados por comas (,) y crea los hechos
% necesarios en impide_verde/2, los cuales indican qué semáforos no pueden es-
% tar en verde de forma simultánea.
%
% @param Carril el identificador de carril (o de semáforo en este caso).
leer_reglas_sems(0).
leer_reglas_sems(Carril) :-
	Carril > 0,
	NCarril is Carril - 1,
	leer_reglas_sems(NCarril),
	write('  >Carriles cuyo semáforo debe estar en rojo si el '),
	write('semáforo del Carril '),write(Carril),write(' está en verde '),
	read_line_to_codes(user_input,In),
	atom_codes(Atom,In),
	atomic_list_concat(ListAtoms,',',Atom),		% Separar en átomos con cada coma y meterlos en una lista.
	atom_a_nums(ListAtoms,SemRojos),			% Convertir átomos de la lista a números.
	forall(
		member(Sem,SemRojos),
		assert(impide_verde(Carril,Sem))
	).


%% leer_tiempo_cambio
%
% Pregunta al usuario que introduzca el tiempo que permancen los semáforos en
% un color y actualiza el hecho t_cambio/1 con el valor introducido.
leer_tiempo_cambio :-
	write('  >Tiempo de cambio de estado de los semáforos '),
	read_line_to_codes(user_input,In),
	atom_codes(Atom,In),
	atom_number(Atom,T),
	T > 0,
	retractall(t_cambio(_)),
	assert(t_cambio(T)).
	

%% atom_a_nums(+A:[atoms], -N:[nums])
%
% Convierte una lista de átomos a una lista de números.
%
% @param A entrada de una lista de átomos.
% @param N una lista de números.
atom_a_nums([],[]).
atom_a_nums([A|Atoms],[N|Nums]) :-
	atom_number(A,N),
	atom_a_nums(Atoms,Nums).


%% si_o_no
%
% Reconoce las entradas desde el teclado s, S, n y N.
% Falla si es n o N, continúa si es s o S y pide una entrada nueva si no la
% reconoce.
si_o_no :-
	get(Char),
	get0(_),		% Consumir retorno.
	si_o_no(Char).
si_o_no(Char) :-
	once(
	Char == 83		% ASCII 83 es 'S'
	;Char == 115	% ASCII 115 es 's'
	), !.
si_o_no(Char) :-
	once(
	Char == 78		% ASCII 78 es 'N'
	;Char == 110	% ASCII 110 es 'n'
	),
	write('*** HASTA PRONTO ***'),
	!, fail.
si_o_no(_) :-
	write('No reconocido.'),nl,
	write('  >(s/n) '),
	si_o_no.


%% print_cruce
%
% Imprime en pantalla el estado actual del cruce. Para cada carril:
%  - Identificador del carril.
%  - Cantidad de coches que tiene.
%  - Tiempo que lleva con el semáforo en rojo.
%  - Último estado del semáforo.
print_cruce :-
	format('~s~t~9|~s~t~18|~s~t~27|~s~t~36|~n',
		   ['Carril','Coches','Tiempo','Semáforo']),
	carril(ID,Coches,Tiempo,Sem),
	format('~a~`.t~9|~a~`.t~18|~a~`.t~27|~a~t~36|~n',
		   [ID,Coches,Tiempo,Sem]),
	fail;true.


%% print_estados
%
% Imprime en pantalla los estados óptimos que se han encontrado para el cruce,
% donde un estado es una combinación de semáforos en verde/rojo que puede
% existir en el cruce. 
print_estados :-
	write('Estas son las combinaciones óptimas de semáforos que se'),
	write(' han encontrado: '),nl,
	estado(ID,Sems),
	write('  Estado '),write(ID),write(': '),write(Sems),nl,fail;true.