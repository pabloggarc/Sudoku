sudoku([.,.,3,.,2,.,7,.,.,5,.,.,.,.,.,4,.,3,.,.,.,3,.,.,.,2,5,.,.,5,.,1,.,6,.,.,.,.,4,8,.,7,.,.,.,2,3,7,6,.,4,8,.,.,.,8,.,.,.,2,.,7,.,3,.,.,4,.,.,2,.,8,.,.,9,.,.,.,.,6,.],P).

%%%UTILIDADES

%predicados que reemplazan un elemento por otro en una lista
reemplazar([_|T], 0, X, [X|T]).
reemplazar([H|T], I, X, [H|R]):- I > 0, NI is I - 1, reemplazar(T, NI, X, R).

%%%SUDOKU

%Predicado que declara e imprime el tablero inicial
sudoku([X|Y],P) :-
    imprimirElemento([X|Y], 0).

%Hecho que para la impresión por pantalla del tablero
imprimirElemento([], _).

%Predicado que imprime el tablero del sudoku
imprimirElemento([X|Y], I):-
    write(X), tab(1),
    (((0 is I mod 9), (I\=0), write('I'), nl);
    ((0 is I mod 3), write('I'), tab(1));
    (write('|'), tab(1))),
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
%
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

    %Predicado que al preguntar devuelve todas las posibilidades de un elemento en una lista
    %Hazme un predicado que haga una cosa u otra dependiendo de si un elemento es par o impar

    posibilidades(T, F, C, S, P):-
    (I is F * 9 + C,
    nth0(I, T, X),
    member(X, [1, 2, 3, 4, 5, 6, 7, 8, 9]),
    P = [X]);
    (L = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    fila(T, F, Fila),
    columna(T, C, Columna),
    cuadro(T, S, Cuadro),
    subtract(L, Fila, L1),
    subtract(L1, Columna, L2),
    subtract(L2, Cuadro, L3),
    P = L3).

    %Predicado que al preguntar rellena todas las posibilidades de los elementos restantes de una fila
    posibilidadesFila(T, F, P):-
    posibilidades(T, F, 0, 3 * (F // 3), P0),
    posibilidades(T, F, 1, 3 * (F // 3), P1),
    posibilidades(T, F, 2, 3 * (F // 3), P2),
    posibilidades(T, F, 3, 3 * (F // 3) + 1, P3),
    posibilidades(T, F, 4, 3 * (F // 3) + 1, P4),
    posibilidades(T, F, 5, 3 * (F // 3) + 1, P5),
    posibilidades(T, F, 6, 3 * (F // 3) + 2, P6),
    posibilidades(T, F, 7, 3 * (F // 3) + 2, P7),
    posibilidades(T, F, 8, 3 * (F // 3) + 2, P8),
    P = [P0, P1, P2, P3, P4, P5, P6, P7, P8].

    posibilidadesSudoku(T, P):-
    posibilidadesFila(T, 0, P0),
    posibilidadesFila(T, 1, P1),
    posibilidadesFila(T, 2, P2),
    posibilidadesFila(T, 3, P3),
    posibilidadesFila(T, 4, P4),
    posibilidadesFila(T, 5, P5),
    posibilidadesFila(T, 6, P6),
    posibilidadesFila(T, 7, P7),
    posibilidadesFila(T, 8, P8),
    append(P0, P1, P01),
    append(P01, P2, P012),
    append(P012, P3, P0123),
    append(P0123, P4, P01234),
    append(P01234, P5, P012345),
    append(P012345, P6, P0123456),
    append(P0123456, P7, P01234567),
    append(P01234567, P8, P012345678),
    P = P012345678.

    eliminarAparicionesFila(T, F, E, P):-
    fila(T, F, Fila),
    nth0(0, Fila, X0),
    nth0(1, Fila, X1),
    nth0(2, Fila, X2),
    nth0(3, Fila, X3),
    nth0(4, Fila, X4),
    nth0(5, Fila, X5),
    nth0(6, Fila, X6),
    nth0(7, Fila, X7),
    nth0(8, Fila, X8),
    subtract(X0, E, P0),
    subtract(X1, E, P1),
    subtract(X2, E, P2),
    subtract(X3, E, P3),
    subtract(X4, E, P4),
    subtract(X5, E, P5),
    subtract(X6, E, P6),
    subtract(X7, E, P7),
    subtract(X8, E, P8),
    reemplazar(T, (F*9), P0, T0),
    reemplazar(T0, (F*9)+1, P1, T1),
    reemplazar(T1, (F*9)+2, P2, T2),
    reemplazar(T2, (F*9)+3, P3, T3),
    reemplazar(T3, (F*9)+4, P4, T4),
    reemplazar(T4, (F*9)+5, P5, T5),
    reemplazar(T5, (F*9)+6, P6, T6),
    reemplazar(T6, (F*9)+7, P7, T7),
    reemplazar(T7, (F*9)+8, P8, T8),
    P = T8.

    eliminarAparicionesColumna(T, C, E, P):-
    columna(T, C, Columna),
    nth0(0, Columna, X0),
    nth0(1, Columna, X1),
    nth0(2, Columna, X2),
    nth0(3, Columna, X3),
    nth0(4, Columna, X4),
    nth0(5, Columna, X5),
    nth0(6, Columna, X6),
    nth0(7, Columna, X7),
    nth0(8, Columna, X8),
    subtract(X0, E, P0),
    subtract(X1, E, P1),
    subtract(X2, E, P2),
    subtract(X3, E, P3),
    subtract(X4, E, P4),
    subtract(X5, E, P5),
    subtract(X6, E, P6),
    subtract(X7, E, P7),
    subtract(X8, E, P8),
    reemplazar(T, C, P0, T0),
    reemplazar(T0, C+9, P1, T1),
    reemplazar(T1, C+18, P2, T2),
    reemplazar(T2, C+27, P3, T3),
    reemplazar(T3, C+36, P4, T4),
    reemplazar(T4, C+45, P5, T5),
    reemplazar(T5, C+54, P6, T6),
    reemplazar(T6, C+63, P7, T7),
    reemplazar(T7, C+72, P8, T8),
    P = T8.

    eliminarAparicionesCuadro(T, S, E, P):-
    cuadro(T, S, Cuadro),
    nth0(0, Cuadro, X0),
    nth0(1, Cuadro, X1),
    nth0(2, Cuadro, X2),
    nth0(3, Cuadro, X3),
    nth0(4, Cuadro, X4),
    nth0(5, Cuadro, X5),
    nth0(6, Cuadro, X6),
    nth0(7, Cuadro, X7),
    nth0(8, Cuadro, X8),
    subtract(X0, E, P0),
    subtract(X1, E, P1),
    subtract(X2, E, P2),
    subtract(X3, E, P3),
    subtract(X4, E, P4),
    subtract(X5, E, P5),
    subtract(X6, E, P6),
    subtract(X7, E, P7),
    subtract(X8, E, P8),
    reemplazar(T, 3 * S, P0, T0),
    reemplazar(T0, 3 * S + 1, P1, T1),
    reemplazar(T1, 3 * S + 2, P2, T2),
    reemplazar(T2, 3 * S + 9, P3, T3),
    reemplazar(T3, 3 * S + 10, P4, T4),
    reemplazar(T4, 3 * S + 11, P5, T5),
    reemplazar(T5, 3 * S + 18, P6, T6),
    reemplazar(T6, 3 * S + 19, P7, T7),
    reemplazar(T7, 3 * S + 20, P8, T8),
    P = T8.

    simplificacion0(T, F, C, S, P):-
    I is F * 9 + C,
    (nth0(I, T, X)),
    length(X, L),
    L = 1,
    eliminarAparicionesFila(T, F, X, T0),
    eliminarAparicionesColumna(T0, C, X, T1),
    eliminarAparicionesCuadro(T1, S, X, T2),
    reemplazar(T2, I, X, P0),
    P = P0.
    

    

