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