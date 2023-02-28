%Predicado que declara e imprime el tablero inicial
sudoku([E00, E01, E02, E03, E04, E05, E06, E07, E08,
       E10, E11, E12, E13, E14, E15, E16, E17, E18,
       E20, E21, E22, E23, E24, E25, E26, E27, E28,
       E30, E31, E32, E33, E34, E35, E36, E37, E38,
       E40, E41, E42, E43, E44, E45, E46, E47, E48,
       E50, E51, E52, E53, E54, E55, E56, E57, E58,
       E60, E61, E62, E63, E64, E65, E66, E67, E68,
       E70, E71, E72, E73, E74, E75, E76, E77, E78,
       E80, E81, E82, E83, E84, E85, E86, E87, E88]) :-
    imprimirTablero(E00, E01, E02, E03, E04, E05, E06, E07, E08),
    imprimirTablero(E10, E11, E12, E13, E14, E15, E16, E17, E18),
    imprimirTablero(E20, E21, E22, E23, E24, E25, E26, E27, E28),
    write(-----------------------------------),nl,
    imprimirTablero(E30, E31, E32, E33, E34, E35, E36, E37, E38),
    imprimirTablero(E40, E41, E42, E43, E44, E45, E46, E47, E48),
    imprimirTablero(E50, E51, E52, E53, E54, E55, E56, E57, E58),
    write(-----------------------------------),nl,
    imprimirTablero(E60, E61, E62, E63, E64, E65, E66, E67, E68),
    imprimirTablero(E70, E71, E72, E73, E74, E75, E76, E77, E78),
    imprimirTablero(E80, E81, E82, E83, E84, E85, E86, E87, E88),
    write(-----------------------------------),nl.

%Predicado que imprime una fila del tablero
imprimirTablero(EX0, EX1, EX2, EX3, EX4, EX5, EX6, EX7, EX8) :-
    write(EX0), tab(1), write('|'), tab(1),
    write(EX1), tab(1), write('|'), tab(1),
    write(EX2), tab(1), write('I'), tab(1),
    write(EX3), tab(1), write('|'), tab(1),
    write(EX4), tab(1), write('|'), tab(1),
    write(EX5), tab(1), write('I'), tab(1),
    write(EX6), tab(1), write('|'), tab(1),
    write(EX7), tab(1), write('|'), tab(1),
    write(EX8), tab(1), write('I'),
    nl.


%Predicado que al preguntar devuelve en F la fila i-ésima del tablero T (F e [0, 8])
%El primero de la fila i-ésima será 9xi, el segundo (9xi)+1, ... , (9xi)+k

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

