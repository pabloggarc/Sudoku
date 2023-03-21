/**PECL1 CRA
Realizada por: Pablo García García y Álvaro Jesús Martínez Parra
Laboratorio: 12.00h-14.00h
Grupo: 1

    Para ejecutar la práctica bastará con poner sudoku(L) como consulta, siendo L el tablero inicial que se pretende simplificar
    Se imprime el tablero inicial, las posibilidades que tiene cada casilla del tablero y la máxima simplificación a la que se puede llegar
    Si se desea imprimir cómo va simplificando el programa, se podrán quitar los comentarios del predicado simplificar según se prefiera, 
    modificando el operador del punto por una coma si este da problemas
    Para más información acerca de los predicados, consultar la memoria de la práctica
**/

%---UTILIDADES---

%Reemplazar un elemento de una lista dado su índice I y un elemento X
reemplazar([_|T], 0, X, [X|T]).
reemplazar([H|T], I, X, [H|R]):-
    I > 0,
    NA is I - 1,
    reemplazar(T, NA, X, R).

%Contar las apariciones de un elemento en una lista de listas
%(Si aparece n veces en una sub-lista contabilizará como una aparición mas)
contarApariciones([], _, 0).
contarApariciones([X|Y], E, T):-
   member(E, X),
   contarApariciones(Y, E, NT),
   T is 1 + NT.
contarApariciones([X|Y], E, T):-
    not(member(E, X)),
    contarApariciones(Y, E, NT),
    T is NT.

%Contar apariciones de, por ejemplo, [a, b] en las casillas de posibilidades (regla 2)
contarSemejantes([], _, 0).
contarSemejantes([X|Y], E, T):-
    X = E,
    contarSemejantes(Y, E, NT),
    T is 1 + NT.
contarSemejantes([X|Y], E, T):-
    not(X = E),
    contarSemejantes(Y, E, NT),
    T is NT.

%Predicado que cuenta las apariciones de una lista de elementos dentro de una lista de listas de acuerdo con la regla 3
%Si dentro de la lista de listas, un elemento y su diferencia con la lista que se busca es vacía, se cuenta como una aparición
contarAparicionesRegla3([], _, 0).
contarAparicionesRegla3([X|Y], E, T):-
    length(X, LX),
    LX > 1,
    subtract(X, E, D),
    D = [],
    contarAparicionesRegla3(Y, E, NT),
    T is 1 + NT.

contarAparicionesRegla3([_|Y], E, T):-
    contarAparicionesRegla3(Y, E, NT),
    T is NT.

%Predicado para obtener los índices de una fila, dado el índice F de la fila (0-8)
indicesFila(F, LF):-
    F0 is 9*F, F1 is (9*F)+1, F2 is (9*F)+2, F3 is (9*F)+3, F4 is (9*F)+4, F5 is (9*F)+5, F6 is (9*F)+6, F7 is (9*F)+7, F8 is (9*F)+8,
    LF = [F0, F1, F2, F3, F4, F5, F6, F7, F8].

%Predicado para obtener los índices de una columna, dado el índice C de la columna (0-8)
indicesColumna(C, LC):-
    C0 is C, C1 is C+9, C2 is C+18, C3 is C+27, C4 is C+36, C5 is C+45, C6 is C+54, C7 is C+63, C8 is C+72,
    LC = [C0, C1, C2, C3, C4, C5, C6, C7, C8].

%Predicado para obtener los índices del cuadro, dado el índice S del primer elemento del cuadro
indicesCuadro(S, LS):-
    S0 is S, S1 is S+1, S2 is S+2, S3 is S+9, S4 is S+10, S5 is S+11, S6 is S+18, S7 is S+19, S8 is S+20,
    LS = [S0, S1, S2, S3, S4, S5, S6, S7, S8].

%Sacar las posiciones conflictivas de un índice I del sudoku
conflictivos(I, L):-
    F is (I//9),
    C is (I mod 9),
    S is 27 * (F//3) + 3 * (C//3), %sacamos el índice del primer elemento del cuadro correspondiente al índice I
    indicesFila(F, LF),
    indicesColumna(C, LC),
    indicesCuadro(S, LS),
    subtract(LC, LF, LDC),
    append(LF, LDC, LFC),
    subtract(LS, LFC, LDS),
    append(LDS, LFC, LL),
    subtract(LL, [I], L).

%Predicado que, dado unos índices conflictivos ([X|Y]), quita un elemento E dado de ellos (regla 1)
quitarElementoDeConflictivos(TP,[],_,TP).
quitarElementoDeConflictivos(TP, [X|Y], E, NTP):-
    (nth0(X, TP, L)),
    (subtract(L, [E], NL)),
    reemplazar(TP, X, NL, NTP1),
    quitarElementoDeConflictivos(NTP1, Y, E, NTP).

%Dada una lista de indices([X|Y]), y unos elementos a borrar 8L), en cada indice quita los elementos de L (regla 2)
quitarLista(TP, [], _, TP).
quitarLista(TP, [X|Y], L, NTP):-
    nth0(X, TP, E),
    E = L,
    quitarLista(TP, Y, L, NTP).
quitarLista(TP, [X|Y], L, NTP):-
    nth0(X, TP, E),
    not(E = L),
    length(E, LE),
    length(L, LL),
    LE >= LL,
    subtract(E, L, D),
    reemplazar(TP, X, D, NNTP),
    quitarLista(NNTP, Y, L, NTP).
quitarLista(TP, [X|Y], L, NTP):-
    nth0(X, TP, E),
    not(E = L),
    length(E, LE),
    length(L, LL),
    LE < LL,
 	quitarLista(TP, Y, L, NTP).

%Dada una lista de indices([X|Y]) y una lista L, elimina las apariciones de los elementos de L de cada indice tal que la diferencia entre el indice y L no sea vacia (regla 3)
quitarListaRegla3(TP, [], _, TP).
quitarListaRegla3(TP, [X|Y], L, NTP):-
    nth0(X, TP, E),
    subtract(E, L, D),
    D = [],
    quitarListaRegla3(TP, Y, L, NTP).

quitarListaRegla3(TP, [X|Y], L, NTP):-
    nth0(X, TP, E),
    subtract(E, L, D),
    not(D = []),
    reemplazar(TP, X, D, NNTP),
    quitarListaRegla3(NNTP, Y, L, NTP).

%Predicado que recibe una lista de listas de 1 elemento cada una y las unifica en una sola lista. 
%Sirve para conservar el mismo formato de entrada y salida en caso de haber encontrado una solución final
darFormato([], TF, TF).
darFormato([X|Y], L, TF):-
    nth0(0, X, E),
    append(L, [E], NL),
    darFormato(Y, NL, TF).

%Predicado que cuenta los números que hay en una lista de listas
contarNumerosSolucion([],0).
contarNumerosSolucion([X|Y],T):-
  length(X, L),
  contarNumerosSolucion(Y, NT),
  T is L + NT.

%Predicado que determina si dado un tablero de sudoku, este está resuelto completamente (los 81 números en las 81 casillas)
sudokuCompletado(SF):-
    contarNumerosSolucion(SF, T),
    81 is T.



%%%---SUDOKU---

%Predicado que resuelve el sudoku, imprimiendo con ASCII ART el tablero inicial, sacando la lista de posibilidades y la solución final
sudoku([X|Y]) :-
    write('Sudoku a resolver'), nl,
    imprimirTablero([X|Y]),
    hacerPosibilidades([X|Y], TP),
    write('Posibilidades del tablero:'),nl,
    imprimirTablero(TP),nl,
    resolver(TP,[],SF),
    write('Soluci\xF3n Final (o m\xE1s simplificada)'), nl,
    imprimirTablero(SF).



%%%---IMPRESIÓN POR PANTALLA DEL TABLERO (ASCII ART)---
%Imprime un tablero [X|Y] dado
imprimirTablero([X|Y]):-
    imprimirElemento([X|Y],1).

%Predicados que imprimen elemento a elemento un tablero
imprimirElemento([], _).
%Predicado que inicia la impresión del tablero
imprimirElemento([X|Y], 1):-
    write('+-------+-------+-------+'),nl,
    write('| '), write(X), write(' '),
    imprimirElemento(Y, 2).
%Impresión del último elemento de una fila de cuadros
imprimirElemento([X|Y], I):-
    (0 is I mod 27),
    write(X), write(' |'),nl,
    write('+-------+-------+-------+'),nl,
    imprimirElemento(Y, I+1).
%Imprsión del último elemento de una fila
imprimirElemento([X|Y], I):-
    (0 is I mod 9),
    write(X), write(' |'), nl,
    imprimirElemento(Y, I+1).
%Impresión del último elemento de una fila de un cuadro
imprimirElemento([X|Y], I):-
    (1 is I mod 3),
    write('| '), write(X), write(' '),
    imprimirElemento(Y, I+1).
%Impresión del resto de elementos
imprimirElemento([X|Y], I):-
    write(X), write(' '),
    imprimirElemento(Y, I+1).


%---OBTENCIÓN DE ELEMENTOS DE FILA, COLUMNA Y CUADRO---
%Predicado que, dada una lista de índices [X|Y], devuelve los elementos dentro del tablero TP correspondientes a esos índices
cogerElementosIndice(_,[],L,L).
cogerElementosIndice(TP,[X|Y],L,LE):-
    nth0(X, TP, E),
    append(L, [E], NL),
    cogerElementosIndice(TP, Y, NL,LE).

%Predicado que devuelve los elementos de la fila I-ésima dentro del tablero T
fila(T, I, F) :-
    indicesFila(I,IF),
    cogerElementosIndice(T,IF,[],F).

%Predicado que devuelve los elementos de la columna I-ésima dentro del tablero T
columna(T, I, C) :-
    indicesColumna(I, IC),
    cogerElementosIndice(T, IC, [], C).

%Predicado que devuelve los elementos del cuadro I-ésimo dentro del tablero T
cuadro(T, I, S) :-
    X is I mod 3,
    Y is I // 3,
    I1 is (3 * X) + (27 * Y),
    indicesCuadro(I1, IS),
    cogerElementosIndice(T, IS, [], S).

%---DEVOLUCIÓN DE POSIBILIDADES---
%
%Predicado que inicializa la búsqueda de las posibilidades dado un tablero T
hacerPosibilidades(T, TP):-
    hacerPosibilidades(T, 0, TP).

hacerPosibilidades(_, 81, []).
%En caso de tener ya un número en la posición I, las posibilidades de ese índice será únicamente ese mismo número
hacerPosibilidades(T, I, TD):-
    (I < 81),
    (N = [1,2,3,4,5,6,7,8,9]),
    nth0(I, T, X),
    member(X, N),
    (NI is I+1),
    hacerPosibilidades(T, NI, TNN),
    append([[X]], TNN, TD).
%En caso de no tener un número en la posición I, calculamos todas las posibilidades
hacerPosibilidades(T, I, TD):-
    (I < 81),
    (N = [1,2,3,4,5,6,7,8,9]),
    (F is I//9),
    (C is I mod 9),
    (S is 3 * (F // 3) + C // 3),
    fila(T, F, Fila),
    columna(T, C, Columna),
    cuadro(T, S, Cuadro),
    subtract(N, Fila, P1),
    subtract(P1, Columna, P2),
    subtract(P2, Cuadro, P),
    (NI is I+1),
    hacerPosibilidades(T, NI, TNN),
    append([P], TNN, TD).



%---REGLAS---
%--REGLA 0--
%Predicado que simplifica lo más que puede utilizando solo la regla 0
regla0(TP, 81, _,TP).
regla0(TP, I, V, R):-
    (not(member(I,V))),
    (nth0(I, TP, X)),
    (length(X, L)),
    (1 is L),
    (nth0(0, X, X1)),
    (conflictivos(I, C)),
    (quitarElementoDeConflictivos(TP, C, X1, NTP)),
    (regla0(NTP, 0, [I|V], R)).
regla0(TP, I, V, R):-
    (NI is I+1),
    (regla0(TP, NI, V, R)).

%--REGLA 1--
%Predicado que simplifica un tablero de posibilidades utilizando la regla 1
regla1(TP, 81, TP).
regla1(TP, I, NTP):-
    nth0(I, TP, X),
    subregla1(TP, I, X, NNTP),
    NI is I+1,
    regla1(NNTP, NI, NTP).

%Predicados que simplifican de acuerdo con la regla 1 en una única casilla del sudoku
subregla1(TP, _, [], TP).
%Caso en el que un elemento aparece una única vez en una fila
subregla1(TP, I, [X|_], NTP):-
    IF is I//9,
    fila(TP, IF, F),
    contarApariciones(F, X, N),
    1 is N,
    reemplazar(TP, I, [X], NTP).
%Caso en el que un elemento aparece una única vez en una columna
subregla1(TP, I, [X|_], NTP):-
    IC is I mod 9,
    columna(TP, IC, C),
    contarApariciones(C, X, N),
    1 is N,
    reemplazar(TP, I, [X], NTP).
%Caso en el que un elemento aparece una única vez en un cuadro
subregla1(TP, I, [X|_], NTP):-
    (IF is I//9),
    (IC is I mod 9),
    (IS is 3 * (IF // 3) + IC // 3),
    cuadro(TP, IS, S),
    contarApariciones(S, X, N),
    1 is N,
    reemplazar(TP, I, [X], NTP).
%Caso en el que un elemento en específico no se puede simplificar; pasa a la siguiente posibilidad de la casilla
subregla1(TP, I, [_|Y], NTP):-
    subregla1(TP, I, Y, NTP).

%--REGLA 2--
%Predicado que simplifica un tablero de posibilidades utilizando la regla 2
regla2(TP, 81, TP).
regla2(TP, I, NTP):-
    nth0(I, TP, X),
    subregla2(TP, I, X, NNTP),
    NI is I+1,
    regla2(NNTP, NI, NTP).

%Predicados que simplifican de acuerdo con la regla 2 en una única casilla del sudoku
%Caso en el que una parejas de posibilidades aparezcan dos veces en una fila
subregla2(TP, I, L, NTP):-
    length(L, LN),
    2 is LN,
    IF is I//9,
    fila(TP, IF, F),
    contarSemejantes(F, L, T),
    2 is T,
    indicesFila(IF, IFS),
    quitarLista(TP, IFS, L, NTP).
%Caso en el que una parejas de posibilidades aparezcan dos veces en una columna
subregla2(TP, I, L, NTP):-
    length(L, LN),
    2 is LN,
    IC is I mod 9,
    columna(TP, IC, C),
    contarSemejantes(C, L, T),
    2 is T,
    indicesColumna(IC, ICS),
    quitarLista(TP, ICS, L, NTP).
%Caso en el que una parejas de posibilidades aparezcan dos veces en un cuadro
subregla2(TP, I, L, NTP):-
    length(L, LN),
    2 is LN,
    IF is I//9,
    IC is I mod 9,
    IS is 3 * (IF // 3) + IC // 3,
    cuadro(TP, IS, S),
    contarSemejantes(S, L, T),
    2 is T,
    ISI is 27 * (IF//3) + 3 * (IC//3), %calculo del Índice inicial del cuadro correspondiente a la casilla I
    indicesCuadro(ISI, ISS),
    quitarLista(TP, ISS, L, NTP).

subregla2(TP, _, _, TP).

%--REGLA 3--
%Predicado que simplifica un tablero de posibilidades utilizando la regla 3
regla3(TP, 81, TP).
%La regla 3 se podría aplicar si se encuentra una casilla con 2 o 3 posibilidades. Ddas esas posibilidades se pretende encontrar 3 casillas que satisfagan la regla 3
regla3(TP, I, NTP):-
    nth0(I, TP, X),
    length(X, LX),
    LX < 4,
    LX > 1,
    subregla3(TP, I, X, NNTP),
    NI is I+1,
    regla3(NNTP, NI, NTP).
regla3(TP, I, NTP):-
    NI is I+1,
    regla3(TP, NI, NTP).

%Predicado que, tras encontrar una casilla con dos posibilidades, busca si en una fila, columna o cuadro hay otro número diferente que unido a ellos dos cumplan con la regla 3
comprobarCombinacionesRegla3(TP, _, _, [], TP).
comprobarCombinacionesRegla3(TP, I, X, [N|RN], NTP):-
    append(X, [N], NX),
    subregla3(TP, I, NX, NNTP),
    comprobarCombinacionesRegla3(NNTP, I, X, RN, NTP).

%Predicado que simplifica de acuerdo con la regla 3 en una casilla con dos posibilidades
subregla3(TP, I, X, NTP):-
    length(X, LX),
    2 is LX,
    N = [1,2,3,4,5,6,7,8,9],
    subtract(N,X,D),
    comprobarCombinacionesRegla3(TP, I, X, D, NTP).

%Predicado que simplifica de acuerdo con la regla 3 en una casilla con tres posibilidades (fila)
subregla3(TP, I, X, NTP):-
    IF is (I//9),
    fila(TP, IF, F),
    contarAparicionesRegla3(F, X, TA),
    3 is TA,
    indicesFila(IF, IFS),
    quitarListaRegla3(TP, IFS, X, NTP).

%Predicado que simplifica de acuerdo con la regla 3 en una casilla con tres posibilidades (columna)
subregla3(TP, I, X, NTP):-
    IC is (I mod 9),
    columna(TP, IC, C),
    contarAparicionesRegla3(C, X, TA),
    3 is TA,
    indicesColumna(IC, ICS),
    quitarListaRegla3(TP, ICS, X, NTP).

%Predicado que simplifica de acuerdo con la regla 3 en una casilla con tres posibilidades (cuadro)
subregla3(TP, I, X, NTP):-
    IF is I//9,
    IC is I mod 9,
    IS is 3 * (IF // 3) + IC // 3,
    cuadro(TP, IS, S),
    contarAparicionesRegla3(S, X, TA),
    3 is TA,
    ISI is 27 * (IF//3) + 3 * (IC//3), %calculo del Índice inicial del cuadro correspondiente a la casilla I
    indicesCuadro(ISI, ISS),
    quitarListaRegla3(TP, ISS, X, NTP).

subregla3(TP, _, _, TP).



%---SIMPLIFICACIÓN---
%Predicado que simplifica un tablero de posibilidades TP de acuerdo con la regla 0
simplificarConRegla0(TP, TS0):-
    regla0(TP, 0, [], TS0).
%Predicado que simplifica un tablero de posibilidades TP de acuerdo con la regla 1
simplificarConRegla1(TP, P):-
    regla1(TP, 0, P).
%Predicado que simplifica un tablero de posibilidades TP de acuerdo con la regla 2
simplificarConRegla2(TP, P):-
    regla2(TP, 0, P).
%Predicado que simplifica un tablero de posibilidades TP de acuerdo con la regla 3
simplificarConRegla3(TP, P):-
    regla3(TP, 0, P).

%Predicado que simplifica un tablero de posibilidades TP de acuerdo con las 4 reglas enunciadas
%En caso de querer imprimir cada simplificación, eliminar el comentario de la última fila del predicado y reemplazar el punto de la penúltima fila por una coma
simplificar(TP, P):-
    simplificarConRegla0(TP, NTP0),
    %write('Regla0'),imprimirTablero(NTP0),nl,
    simplificarConRegla1(NTP0, NTP1),
    %write('Regla1'),imprimirTablero(NTP1),nl,
    simplificarConRegla2(NTP1, NTP2),
    %write('Regla2'),imprimirTablero(NTP2),nl,
    simplificarConRegla3(NTP2, P).
    %write('Regla3'),imprimirTablero(P),nl.
    %write("Simplificaci\xF3n"), nl, imprimirTablero(P).


%---RESOLUCIÓN DEL SUDOKU---
%Predicado que resuelve el sudoku dadas las posibilidades del mismo, simplificando hasta que se llegue al mismo tablero con las dos últimas simplificaciones realizadas
%Resolución final encontrada, solución final del sudoku
resolver(TN, TA, SF):-
    TA = TN,
    sudokuCompletado(TN),
    darFormato(TN, [],SF).
%Resolución final encontrada, hasta aquí se puede simplificar siguiendo las 4 reglas
resolver(TN,TA,TN):-
  TN = TA.
%Resolución final no encontrada, seguir simplificando
resolver(TN, TA, SF):-
    not(TA=TN),
    simplificar(TN, TS),
    resolver(TS, TN, SF).