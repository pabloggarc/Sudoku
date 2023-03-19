%sudoku([.,.,3,.,2,.,7,.,.,5,.,.,.,.,.,4,.,3,.,.,.,3,.,.,.,2,5,.,.,5,.,1,.,6,.,.,.,.,4,8,.,7,.,.,.,2,3,7,6,.,4,8,.,.,.,8,.,.,.,2,.,7,.,3,.,.,4,.,.,2,.,8,.,.,9,.,.,.,.,6,.],P).

%---UTILIDADES---

%Reemplazar el primer elemento de una lista por un elemento dado
reemplazar([_|T], 0, X, [X|T]).
reemplazar([H|T], I, X, [H|R]):-
    I > 0,
    NA is I - 1,
    reemplazar(T, NA, X, R).

%Contar las apariciones de un elemento en una lista de listas
%Si aparece n veces en una sub-lista contabilizará como una aparición mas
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

%Comprobar si un elemento está en una lista con números y listas o dentro de esas listas
apareceMixto([X|_], X).
apareceMixto([[X|_]|_], X).
apareceMixto([[_|Y]|_], X):-
    apareceMixto(Y, X).
apareceMixto([_|Y], X):-
    apareceMixto(Y, X).

%Predicado para obtener las filas, columnas, y cuadros dado su indice
indicesFila(F, LF):-
    F0 is 9*F, F1 is (9*F)+1, F2 is (9*F)+2, F3 is (9*F)+3, F4 is (9*F)+4, F5 is (9*F)+5, F6 is (9*F)+6, F7 is (9*F)+7, F8 is (9*F)+8,
    LF = [F0, F1, F2, F3, F4, F5, F6, F7, F8].

indicesColumna(C, LC):-
    C0 is C, C1 is C+9, C2 is C+18, C3 is C+27, C4 is C+36, C5 is C+45, C6 is C+54, C7 is C+63, C8 is C+72,
    LC = [C0, C1, C2, C3, C4, C5, C6, C7, C8].

indicesCuadro(S, LS):-
    S0 is S, S1 is S+1, S2 is S+2, S3 is S+9, S4 is S+10, S5 is S+11, S6 is S+18, S7 is S+19, S8 is S+20,
    LS = [S0, S1, S2, S3, S4, S5, S6, S7, S8].

%Sacar las posiciones conflictivas de un índice del sudoku
conflictivos(I, L):-
    F is (I//9),
    C is (I mod 9),
    S is 27 * (F//3) + 3 * (C//3),
    indicesFila(F, LF),
    indicesColumna(C, LC),
    indicesCuadro(S, LS),
    subtract(LC, LF, LDC),
    append(LF, LDC, LFC),
    subtract(LS, LFC, LDS),
    append(LDS, LFC, LL),
    subtract(LL, [I], L).

%Predicado que, dado un índice, quita un elemento dado del resto de índices conflictivos
quitarElementoDeConflictivos(TP,[],_,TP).

quitarElementoDeConflictivos(TP, [X|Y], E, NTP):-
    (nth0(X, TP, L)),
    (subtract(L, [E], NL)),
    NL = [],
    reemplazar(TP, X, NL, NTP1),
    write('He reemplazado '), write(L), write(' por '), write(NL), write('aqui el tablero que tenia antes'), nl, imprimirElemento(TP,1),
    quitarElementoDeConflictivos(NTP1, Y, E, NTP).
quitarElementoDeConflictivos(TP, [X|Y], E, NTP):-
    (nth0(X, TP, L)),
    (subtract(L, [E], NL)),
    reemplazar(TP, X, NL, NTP1),
    quitarElementoDeConflictivos(NTP1, Y, E, NTP).

%Dado el tablero, una lista de indices, y unos elementos a borrar, en cada indice quita los elementos de la lista (reglas 2 y 3)
quitarLista(TP, [], _, TP).

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

quitarLista(TP, [X|Y], L, NTP):-
    nth0(X, TP, E),
    E = L,
    quitarLista(TP, Y, L, NTP).

%Predicado que recibe una lista de listas de 1 elemento, las cuales unifica en una sola. Sive para conservar el mismo formato de entrada y salida
darFormato([], TF, TF).

darFormato([X|Y], L, TF):-
    nth0(0, X, E),
    append(L, [E], NL),
    darFormato(Y, NL, TF).

%%%---SUDOKU---

%Predicado que declara e imprime el tablero inicial
sudoku([X|Y]) :-
    write('Sudoku a resolver'), nl,
    imprimirElemento([X|Y], 1),
    hacerPosibilidades([X|Y], TP),
    imprimirElemento(TP, 1),nl,
    resolver(TP, 0, SF),
    write('Soluci\xF3n'), nl,
    imprimirElemento(SF, 1).



%%%---IMPRESIÓN POR PANTALLA DEL TABLERO (ASCII ART)---

%Hecho que para la impresión por pantalla del tablero cuando ya no hay más elementos
imprimirElemento([], _).

%Predicado que empieza la impresión del tablero
imprimirElemento([X|Y], I):-
    (1 is I),
    write('+-------+-------+-------+'),nl,
    write('| '), write(X), write(' '),
    imprimirElemento(Y, I+1).

%Predicado que da paso a la impresión de una fila de cuadrados
imprimirElemento([X|Y], I):-
    (0 is I mod 27),
    write(X), write(' |'),nl,
    write('+-------+-------+-------+'),nl,
    imprimirElemento(Y, I+1).

%Predicado que da paso a la impresión de una fila de elementos
imprimirElemento([X|Y], I):-
    (0 is I mod 9),
    write(X), write(' |'), nl,
    imprimirElemento(Y, I+1).

%Predicado que da paso a la impresión de los elementos del cuadro siguiente perteneciente a la fila
imprimirElemento([X|Y], I):-
    (1 is I mod 3),
    write('| '), write(X), write(' '),
    imprimirElemento(Y, I+1).

%Predicado que imprime cualquier otro elemento del tablero
imprimirElemento([X|Y], I):-
    write(X), write(' '),
    imprimirElemento(Y, I+1).

%%%---SACAR UNA LISTA CON LA FILA, COLUMNA Y CUADRO N---
/**fila(T, N, 1, F) :-
    F = N.
fila(T, N, E, F) :-
    P = (9 * F) + E - 1,
    nth0(P, T, X),
    append(N, [X], N1),
    fila(T, N1, E-1, F).**/

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

%Predicado base que devuelve en R la lista de posibilidades de un tablero T
hacerPosibilidades(_, 81, []).


%Predicado que devuelve las probabilidades en el tablero (en el caso de que el siguiente elemento ya haya sido dado)
hacerPosibilidades(T, I, TD):-
    (I < 81),
    (N = [1,2,3,4,5,6,7,8,9]),
    nth0(I, T, X),
    member(X, N),
    (NI is I+1),
    hacerPosibilidades(T, NI, TNN),
    append([[X]], TNN, TD).

%Predicado que devuelve las probabilidades en el tablero
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

hacerPosibilidades(T, TP):-
    hacerPosibilidades(T, 0, TP).

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


%Predicado que simplifica todas las casillas del sudoku
regla1(TP, 81, TP).

regla1(TP, I, NTP):-
    nth0(I, TP, X),
    subregla1(TP, I, X, NNTP),
    NI is I+1,
    regla1(NNTP, NI, NTP).

%Predicados que simplifican de acuerdo con la regla 1 en una única casilla del sudoku
subregla1(TP, I, [X|_], NTP):-
    IF is I//9,
    fila(TP, IF, F),
    contarApariciones(F, X, N),
    1 is N,
    reemplazar(TP, I, [X], NTP).

subregla1(TP, I, [X|_], NTP):-
    IC is I mod 9,
    columna(TP, IC, C),
    contarApariciones(C, X, N),
    1 is N,
    reemplazar(TP, I, [X], NTP).

subregla1(TP, I, [X|_], NTP):-
    (IF is I//9),
    (IC is I mod 9),
    (IS is 3 * (IF // 3) + IC // 3),
    cuadro(TP, IS, S),
    contarApariciones(S, X, N),
    1 is N,
    reemplazar(TP, I, [X], NTP).

subregla1(TP, _, [], TP).

subregla1(TP, I, [_|Y], NTP):-
    subregla1(TP, I, Y, NTP).

regla2(TP, 81, TP).

regla2(TP, I, NTP):-
    nth0(I, TP, X),
    subregla2(TP, I, X, NNTP),
    NI is I+1,
    regla2(NNTP, NI, NTP).

subregla2(TP, I, L, NTP):-
    length(L, LN),
    2 is LN,
    IF is I//9,
    fila(TP, IF, F),
    contarSemejantes(F, L, T),
    2 is T,
    indicesFila(IF, IFS),
    quitarLista(TP, IFS, L, NTP).

subregla2(TP, I, L, NTP):-
    length(L, LN),
    2 is LN,
    IC is I mod 9,
    columna(TP, IC, C),
    contarSemejantes(C, L, T),
    2 is T,
    indicesColumna(IC, ICS),
    quitarLista(TP, ICS, L, NTP).

subregla2(TP, I, L, NTP):-
    length(L, LN),
    2 is LN,
    IC is I mod 9,
    columna(TP, IC, C),
    contarSemejantes(C, L, T),
    2 is T,
    indicesColumna(IC, ICS),
    quitarLista(TP, ICS, L, NTP).

subregla2(TP, I, L, NTP):-
    length(L, LN),
    2 is LN,
    IF is I//9,
    IC is I mod 9,
    IS is 3 * (IF // 3) + IC // 3,
    cuadro(TP, IS, S),
    contarSemejantes(S, L, T),
    2 is T,
    indicesCuadro(IS, ISS),
    quitarLista(TP, ISS, L, NTP).

subregla2(TP, _, _, TP).

regla3(TP, 81, TP).

regla3(TP, I, NTP):-
    nth0(I, TP, X),
    subregla3(TP, I, X, NNTP),
    NI is I+1,
    regla2(NNTP, NI, NTP).

subregla3(TP, I, L, NTP):-
    length(L, LN),
    3 is LN,
    IF is I//9,
    fila(TP, IF, F),
    contarSemejantes(F, L, T),
    3 is T,
    indicesFila(IF, IFS),
    quitarLista(TP, IFS, L, NTP).

subregla3(TP, I, L, NTP):-
    length(L, LN),
    3 is LN,
    IC is I mod 9,
    columna(TP, IC, C),
    contarSemejantes(C, L, T),
    3 is T,
    indicesColumna(IC, ICS),
    quitarLista(TP, ICS, L, NTP).

subregla3(TP, I, L, NTP):-
    length(L, LN),
    3 is LN,
    IF is I//9,
    IC is I mod 9,
    IS is 3 * (IF // 3) + IC // 3,
    cuadro(TP, IS, S),
    contarSemejantes(S, L, T),
    3 is T,
    indicesCuadro(IS, ISS),
    quitarLista(TP, ISS, L, NTP).

subregla3(TP, _, _, TP).

%Predicado que simplifica el sudoku de acuerdo con la regla 0
simplificarConRegla0(TP, TS0):-
    regla0(TP, 0, [], TS0).
%Predicado que simplifica el sudoku de acuerdo con la regla 1
simplificarConRegla1(TP, P):-
    regla1(TP, 0, P).
simplificarConRegla2(TP, P):-
    regla2(TP, 0, P).
simplificarConRegla3(TP, P):-
    regla3(TP, 0, P).

%Predicado que simplifica el sudoku de acuerdo con las 4 reglas enunciadas
simplificar(TP, P):-
    simplificarConRegla0(TP, NTP0),
    simplificarConRegla1(NTP0, NTP1),
    simplificarConRegla2(NTP1, NTP2),
    simplificarConRegla3(NTP2, P),
    write("Simplificaci\xF3n"), nl, imprimirElemento(P, 1).

%Predicado que resuelve el sudoku dadas las posibilidades del mismo, simplificando hasta que todas las casillas tengan posibilidad 1
resolver(TP, 81, TF):-
    darFormato(TP, [], TF).

resolver(TP, I, SF):-
    nth0(I, TP, L),
    length(L, LN),
    1 is LN,
    NI is I+1,
    resolver(TP, NI, SF).

resolver(TP, _, SF):-
    simplificar(TP, TS),
    resolver(TS, 0, SF).