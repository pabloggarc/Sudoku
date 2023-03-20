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

%Contar apariciones de, por ejemplo, [a, b] en las casillas de posibilidades (reglas 2 y 3)
contarSemejantes([], _, 0).
contarSemejantes([X|Y], E, T):-
    X = E,
    contarSemejantes(Y, E, NT),
    T is 1 + NT.
contarSemejantes([X|Y], E, T):-
    not(X = E),
    contarSemejantes(Y, E, NT),
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

%Dada una lista de indices([X|Y]), y unos elementos a borrar 8L), en cada indice quita los elementos de L (reglas 2 y 3)
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
    write('Soluci\xF3n Final (o más simplificada)'), nl,
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


fila(T, I, F) :-
    %Calculamos índices en T
    I1 is I * 9 ,
    I2 is I * 9 + 1,
    I3 is I * 9 + 2,
    I4 is I * 9 + 3,
    I5 is I * 9 + 4,
    I6 is I * 9 + 5,
    I7 is I * 9 + 6,
    I8 is I * 9 + 7,
    I9 is I * 9 + 8,

    %Obtenemos los elementos por índice
    nth0(I1, T, X1),
    nth0(I2, T, X2),
    nth0(I3, T, X3),
    nth0(I4, T, X4),
    nth0(I5, T, X5),
    nth0(I6, T, X6),
    nth0(I7, T, X7),
    nth0(I8, T, X8),
    nth0(I9, T, X9),

    %Creamos una nueva lista con los valores
    F = [X1, X2, X3, X4, X5, X6, X7, X8, X9].

%Predicado que al preguntar devuelve en C la columna i-ésima del tablero T (C e [0, 8])
%Si en cada columna hay 9 elementos, el primero de la columna i-ésima será i, el segundo i+9, el tercero i+(9*2), ..., i+(9*k)

columna(T, I, C) :-
    %Calculamos índices en T
    I1 is I ,
    I2 is I + 9,
    I3 is I + (9 * 2),
    I4 is I + (9 * 3),
    I5 is I + (9 * 4),
    I6 is I + (9 * 5),
    I7 is I + (9 * 6),
    I8 is I + (9 * 7),
    I9 is I + (9 * 8),

    %Obtenemos los elementos por índice
    nth0(I1, T, X1),
    nth0(I2, T, X2),
    nth0(I3, T, X3),
    nth0(I4, T, X4),
    nth0(I5, T, X5),
    nth0(I6, T, X6),
    nth0(I7, T, X7),
    nth0(I8, T, X8),
    nth0(I9, T, X9),

    %Creamos una nueva lista con los valores
    C = [X1, X2, X3, X4, X5, X6, X7, X8, X9].

%Predicado que al preguntar devuelve en C el cuadro i-ésimo del tablero T (C e [0, 8])

cuadro(T, I, C) :-
    %Calculamos índices en T
    X is I mod 3,
    Y is I // 3,
    I1 is (3 * X) + (27 * Y),
    I2 is I1 + 1,
    I3 is I2 + 1,
    I4 is I1 + 9,
    I5 is I2 + 9,
    I6 is I3 + 9,
    I7 is I4 + 9,
    I8 is I5 + 9,
    I9 is I6 + 9,

    %Obtenemos los elementos por índice
    nth0(I1, T, X1),
    nth0(I2, T, X2),
    nth0(I3, T, X3),
    nth0(I4, T, X4),
    nth0(I5, T, X5),
    nth0(I6, T, X6),
    nth0(I7, T, X7),
    nth0(I8, T, X8),
    nth0(I9, T, X9),

    %Creamos una nueva lista con los valores
    C = [X1, X2, X3, X4, X5, X6, X7, X8, X9].


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
regla3(TP, I, NTP):-
    nth0(I, TP, X),
    subregla3(TP, I, X, NNTP),
    NI is I+1,
    regla2(NNTP, NI, NTP).

%Predicados que simplifican de acuerdo con la regla 3 en una única casilla del sudoku
%Caso en el que un trío de posibilidades aparezca tres veces en una fila
subregla3(TP, I, L, NTP):-
    length(L, LN),
    3 is LN,
    IF is I//9,
    fila(TP, IF, F),
    contarSemejantes(F, L, T),
    3 is T,
    indicesFila(IF, IFS),
    quitarLista(TP, IFS, L, NTP).
%Caso en el que un trío de posibilidades aparezca tres veces en una columna
subregla3(TP, I, L, NTP):-
    length(L, LN),
    3 is LN,
    IC is I mod 9,
    columna(TP, IC, C),
    contarSemejantes(C, L, T),
    3 is T,
    indicesColumna(IC, ICS),
    quitarLista(TP, ICS, L, NTP).
%Caso en el que un trío de posibilidades aparezca tres veces en un cuadro
subregla3(TP, I, L, NTP):-
    length(L, LN),
    3 is LN,
    IF is I//9,
    IC is I mod 9,
    IS is 3 * (IF // 3) + IC // 3,
    cuadro(TP, IS, S),
    contarSemejantes(S, L, T),
    3 is T,
    ISI is 27 * (IF//3) + 3 * (IC//3), %calculo del Índice inicial del cuadro correspondiente a la casilla I
    indicesCuadro(ISI, ISS),
    quitarLista(TP, ISS, L, NTP).

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