%Predicado que al preguntar devuelve en F la fila i-esima del tablero T (F e [0, 8])
fila(T, I, F) :- 
    I1 is I * 9 , nth0(I1, T, X1),
    I2 is I * 9 + 1, nth0(I2, T, X2),
    I3 is I * 9 + 2, nth0(I3, T, X3),
    I4 is I * 9 + 3, nth0(I4, T, X4),
    I5 is I * 9 + 4, nth0(I5, T, X5),
    I6 is I * 9 + 5, nth0(I6, T, X6),
    I7 is I * 9 + 6, nth0(I7, T, X7),
    I8 is I * 9 + 7, nth0(I8, T, X8),
    I9 is I * 9 + 8, nth0(I9, T, X9),
    F = [X1, X2, X3, X4, X5, X6, X7, X8, X9].