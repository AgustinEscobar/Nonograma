:- module(proylcc,
	[  
		put/8,chequearInicio/5,solucion/4
	]).

:-use_module(library(lists)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY es el resultado de reemplazar la ocurrencia de X en la posición XIndex de Xs por Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

% Retorna X, siendo este el elemento/lista que necesitas.
% getActual(Pos,ListaABuscarPos,Retorno)
getActual(0,[X|_Xs],X).
getActual(N,[_X|Xs],Ret):- N2 is N-1, getActual(N2,Xs,Ret).

% Obtenes la columna que necesitas.
% getCol(ColN,newGrilla,ColRet).
getCol(_N,[],[]).
getCol(N,[X|Xs],[Elem|ColRet]):- getCol(N,Xs,ColRet), getActual(N,X,Elem).

% verificar(Pista,ListaAControlar,ListaAControlarReducida)
verificarPista(0,[],[]).
verificarPista(0, [X|Xs] ,Xs):- X\=="#".
verificarPista(N,[X|Xs],Ret):- X=="#" , N\=0, N2 is N-1 , verificarPista(N2,Xs,Ret).

% controlarSat(Pistas,ListaMovida).
controlarSat([],[Y|Ys]):- Y\=="#" , controlarSat([],Ys).
controlarSat([],[]).
controlarSat([X|Xs],[Y|Ys]):- Y=="#" , (verificarPista(X,[Y|Ys],Red), controlarSat(Xs,Red)).
controlarSat([X|Xs],[Y|Ys]):- Y\=="#", controlarSat([X|Xs],Ys).

% Controla si las filas de la grilla cumplen con sus respectivas pistas y 
% retorna una lista de 0s cuando no cumple y 1s en caso de que si.
% chequearFilas(+Grilla,+PistasF,-CumpleF) 
chequearFilas([],[],[]).
chequearFilas([Fila|RestoF],[Pista|RestoP],[1|CumpleF]):-
    controlarSat(Pista,Fila),
    chequearFilas(RestoF,RestoP,CumpleF).
chequearFilas([_Fila|RestoF],[_Pista|RestoP],[0|CumpleF]):-
    chequearFilas(RestoF,RestoP,CumpleF).

% Controla si las columnas de la grilla cumplen con sus respectivas pistas.
% chequearColumns(+Grilla,+PistasC,+Indice,-CumpleC)
chequearColumns(_Grilla,[],_N,[]).
chequearColumns(Grilla,[Pista|RestoP],N,[1|CumpleC]):-
    NAux is N+1,
    getCol(N,Grilla,ListColumn),
    controlarSat(Pista,ListColumn),
    chequearColumns(Grilla,RestoP,NAux,CumpleC).
chequearColumns(Grilla,[_Pista|RestoP],N,[0|CumpleC]):-
	NAux is N+1,
	chequearColumns(Grilla,RestoP,NAux,CumpleC).

% Recibiendo una grilla, una pista p/columnas y otra p/filas, retorna
% dos listas, que indican que columnas y filas cumplen con sus respectivas pistas.
% chequearInicio(+Grilla,+PistasF,+PistasC,-CumpleF,-CumpleC)
chequearInicio(Grilla,PistasF,PistasC,CumpleF,CumpleC):-
    chequearFilas(Grilla,PistasF,CumpleF),
    chequearColumns(Grilla,PistasC,0,CumpleC).


% -------------------------- PARTE 2 -----------------------

% generarPista(Pista,ListaAControlar,ListaAGenerar)
generarPista(0,[],[]).
generarPista(0, [X|Xs] ,Xs):- X = "X".
generarPista(N,[X|Xs],Ret):- X ="#" , N\=0, N2 is N-1 , generarPista(N2,Xs,Ret).

% generarSat(Pistas,ListaMovida).
generarSat([],[Y|Ys]):- Y = "X" , generarSat([],Ys).
generarSat([],[]).
generarSat([X|Xs],[Y|Ys]):- Y = "#" , (generarPista(X,[Y|Ys],Red), generarSat(Xs,Red)).
generarSat([X|Xs],[Y|Ys]):- Y = "X", generarSat([X|Xs],Ys).

interseccion(Posibles,Length,Salida):-
    AuxLen is Length - 1,
    interseccion_aux(Posibles,AuxLen,[],Salida).
%Caso 1
interseccion_aux(_,-1,Aux,Aux).
%Caso 2
interseccion_aux(Posibles,N,In,Out):-
    findall(Elem,(
                 member(L,Posibles),
                 getActual(N,L,Elem)
                 ),Iesimos),
    	todos_E("#",Iesimos),
    	append(["#"],In,Aux), 
    	NAux is N - 1,
    	interseccion_aux(Posibles,NAux,Aux,Out).

interseccion_aux(Posibles,N,In,Out):-
    findall(Elem,(
                 member(L,Posibles),
                 getActual(N,L,Elem)
                 ),Iesimos),
    	todos_E("X",Iesimos),
    	append(["X"],In,Aux), 
    	NAux is N - 1,
    	interseccion_aux(Posibles,NAux,Aux,Out).

interseccion_aux(Posibles,N,In,Out):-
    append([_],In,Aux),
    NAux is N - 1,
    interseccion_aux(Posibles,NAux, Aux, Out).

% todos_E(+Elemento,+Lista)
todos_E(_E,[]).
todos_E(X,[X|Xs]):- todos_E(X,Xs).

%filaCauta(lista,pistasLista,longitud,filaCautaRetornada)
filaCauta(Actual,Pistas,Length,FilaC):-
    findall(Actual,(length(Actual,Length),generarSat(Pistas,Actual)),TPosibles),
    interseccion(TPosibles,Length,FilaC),!.

% Genera en una pasada las filas cautas
% generarFilas(+Grilla,+PistasF,-GrillaAux)
generarFilas([],[],[]).
generarFilas([Fila|RestoF],[Pista|RestoP],[FilaRet|RestoRet]):-
    length(Fila,LongitudF),
	filaCauta(Fila,Pista,LongitudF,FilaRet),
    generarFilas(RestoF,RestoP,RestoRet).

% Reemplaza en la Grilla, una columna en un indice y retorna la misma grilla 
% pero con la columna indicada modificada.
% reemplazar(+Grilla,+ColumnList,+IndiceCol,-GrillaRes)
reemplazar([],[],_,[]).
reemplazar([Fila|RestoF],[Elem|Xs],ColN,[NewFila|RestoNew]):-
    replace(_Cell,ColN,Elem,Fila,NewFila),
    reemplazar(RestoF,Xs,ColN,RestoNew).

% Genera una pasada las columnas cautas
 % generarColumns(+Grilla,+PistasC,+Indice,-GrillaRes)
generarColumns(Grilla,[],_,Grilla).
generarColumns(Grilla,[Pista|RestoP],N,GrillaRes):-
    Index is N+1,
    getCol(N,Grilla,ColumnList),
    length(ColumnList,LongitudC),
    filaCauta(ColumnList,Pista,LongitudC,ColumnRes),
    reemplazar(Grilla,ColumnRes,N,GrillaAux),
    generarColumns(GrillaAux,RestoP,Index,GrillaRes).

% Genera una pasada de toda la grilla, siempre haciendo pasos "seguros"
% solucionAux(+Grilla,+PistasF,+PistasC,-GrillaRes)  
solucionAux(Grilla,PistasF,PistasC,GrillaRes):-
    % genero las filas
	generarFilas(Grilla,PistasF,GrillaAux),
    % genero las columnas
    generarColumns(GrillaAux,PistasC,0,GrillaRes).

% Retorna una Grilla, siendo esta una solucion valida.
% solucion(+Grilla,+PistasF,+PistasC,-GrillaRes).
solucion(Grilla,PistasF,PistasC,GrillaRes):-
    solucionAux(Grilla,PistasF,PistasC,GrillaRes),
    (forall(member(L,GrillaRes),(forall(member(X,L),not(var(X)))))
    ;solucion(GrillaRes,PistasF,PistasC,GrillaRes)).
    

% put(+Contenido, +Pos, +PistasFilas, +PistasColumnas, +Grilla, -GrillaRes, -FilaSat, -ColSat).
put(Contenido, [FilaN, ColN], PistasFilas, PistasColumnas, Grilla, NewGrilla, FilaSat, ColSat):-
	% NewGrilla es el resultado de reemplazar la Fila en la posición FilaN de Grilla
	% (FilaN-ésima fila de Grilla), por una fila nueva NewFila.
	
	replace(Fila, FilaN, NewFila, Grilla, NewGrilla),

	% NewFila es el resultado de reemplazar la celda Cell en la posición ColN de Fila por _,
	% siempre y cuando Cell coincida con Contenido (Cell se instancia en la llamada al replace/5).
	% En caso contrario (;)
	% NewFila es el resultado de reemplazar lo que sea que haya (_Cell) en la posición ColN de Fila por Conenido.	 
	
	(replace(Cell, ColN, _, Fila, NewFila),
	    Cell == Contenido;
	    replace(_Cell, ColN, Contenido, Fila, NewFila)),
	
	%Controlar si se satisface las pistas de fila.
	( getActual(FilaN,PistasFilas,PistaF), 
	 FilaSat is 1, controlarSat(PistaF,NewFila); FilaSat is 0 ),
	
	%Controlar si se satisface las pistas de columna
	( getCol(ColN,NewGrilla,ColRet) , getActual(ColN,PistasColumnas,PistaC) ,
	 ColSat is 1, controlarSat(PistaC,ColRet) ; ColSat is 0 ).


	



