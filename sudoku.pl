%sudoku([.,.,3,.,2,.,7,.,.,5,.,.,.,.,.,4,.,3,.,.,.,3,.,.,.,2,5,.,.,5,.,1,.,6,.,.,.,.,4,8,.,7,.,.,.,2,3,7,6,.,4,8,.,.,.,8,.,.,.,2,.,7,.,3,.,.,4,.,.,2,.,8,.,.,9,.,.,.,.,6,.],P).

%%%---UTILIDADES---

%Reemplazar el primer elemento de una lista por un elemento dado
reemplazar([_|T], 0, X, [X|T]).

%Reemplazar el elemento i-ésimo de una lista por un elemento dado (recursividad)
reemplazar([H|T], I, X, [H|R]):-
    I > 0,
    NA is I - 1,
    reemplazar(T, NA, X, R).


%%%---SUDOKU---

%Predicado que declara e imprime el tablero inicial
/**sudoku([X|Y],P) :-
    imprimirElemento([X|Y], 1).**/


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

%%%---DEVOLUCIÓN DE POSIBILIDADES---

%Haz un factorial
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

    hacerPosibilidades(T):-
        hacerPosibilidades(T, 0, P), write(P).

    simplificacion(T, _, 81, R):-
        R = T.

    simplificacion(T, P, I, R):-
        (nth0(I, P, X)),
        (length(X, L)),
        (1 is L),
        (nth0(0, X, X1)),
        (reemplazar(T, I, X1, NT)),
        NI is I+1,
        (simplificacion(NT, P, NI, R)).

    simplificacion(T, P, I, R):-
        (NI is I+1),
        (simplificacion(T, P, NI, R)).

%Haz un fact