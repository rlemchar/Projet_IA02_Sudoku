
							%%% SUDOKU-SUDOKU-SUDOKU %%%


	%% ---AUTRES--- %%

sudokuInitial([[[[9,' ',8],[' ',1,7],[' ',5,' ']],
			[[' ',' ',' '],[5,9,' '],[7,' ',8]],
			[[' ',' ',4],[6,' ',' '],[' ',' ',' ']]],
			[[[' ',' ',' '],[' ',7,' '],[2,8,1]],
			[[' ',7,3],[9,' ',8],[5,6,' ']],
			[[2,8,6],[' ',4,' '],[' ',' ',' ']]],
			[[[' ',' ',' '],[' ',' ',4],[9,' ',' ']],
			[[3,' ',7],[' ',6,9],[' ',' ',' ']],
			[[' ',6,' '],[8,3,' '],[4,' ',2]]]]).

sudokuTest([[[[9,2,8],[4,1,7],[6,5,3]],
			[[6,3,1],[5,9,2],[7,4,8]],
			[[7,5,4],[6,8,3],[1,' ',9]]],
			[[[4,9,5],[3,7,6],[2,8,1]],
			[[1,7,3],[9,2,8],[5,6,4]],
			[[2,8,6],[1,4,5],[3,9,7]]],
			[[[8,1,2],[7,5,4],[9,3,6]],
			[[3,4,7],[' ',6,9],[8,1,5]],
			[[5,6,9],[8,3,1],[4,7,2]]]]).

non(A) :- A, !, fail.
non(_).

% appartenance à une liste
element(_,[]):- fail.
element(X,[X|_]):- !.
element(X,[_|Q]):- element(X,Q).

% is list?
isList([]).
isList([_|B]):- isList(B).

% concatenation de listes
concat([],C,C).
concat([A|B],C,[A|E]):- concat(B,C,E),!.

	%% ===AFFICHAGE SUDOKU=== %%

% afficher liste de Trois elements
affT([A|[]]) :- write(A), !.
affT([A|B])  :- write(A), write(' '), affT(B).

% afficher Ligne de listes
affL([A|[]]) :- affT(A), !.
affL([A|B])  :- affT(A), write('|'), affL(B).

% afficher Groupe de lignes
affG([A|[]]) :- affL(A), nl, !.
affG([A|B])  :- affL(A), nl, affG(B).

% afficher Sudoku
affS([A|[]]) :- affG(A), !.
affS([A|B])  :- affG(A), write('-----------------'), nl, affS(B).

	%% ===MODIFICATION DES VALEURS=== %%

%remplacer l'Yeme element de la ligne est remplace par V
changer2(V,0,[_|B],[V|B]).
changer2(V,Y,[A|B],[C|D]):- Y>0, Y1 is (Y-1), changer2(V,Y1,B,D), C = A.
changer1(V,Y,[A|B],[C|D]):- Y<3, changer2(V,Y,A,C), D = B, !.
changer1(V,Y,[A|B],[C|D]):- Y1 is (Y-3), changer1(V,Y1,B,D), C = A.

%remplacer l'element (X,Y) par V: utiliser seulement changer(Valeur,Xcoor,Ycoor,InList,OutList)
changer3(V,0,Y,[A|B],[C|B]):- changer1(V,Y,A,C).
changer3(V,X,Y,[A|B],[C|D]):- X>0, X1 is (X-1), changer3(V,X1,Y,B,D), C = A.
changer(V,X,Y,[A|B],[C|D]):- X<3, changer3(V,X,Y,A,C), D = B, !.
changer(V,X,Y,[A|B],[C|D]):- X1 is (X-3), changer(V,X1,Y,B,D), C = A.

% verifier que les coordonnées entrées par l'utilisateur sont valides 1-9
validUserInputNumber(X):- X>0, X=<9, integer(X), !.
validUserInputNumber(_):- nl, write('Saisie incorrecte !'),nl,nl, fail.

% verifier que les coordonnées sont valides en interne 0-8
validCoord(X):- X>=0, X=<8, integer(X), !.
validCoord(_):- fail.

	%% ===VERIFICATION SUDOKU=== %%

% recuperer Xeme element (valeur ou liste) de la liste
getNlist([A|_],0,A).
getNlist([_|B],X,N) :- X>=1, X1 is X-1, getNlist(B,X1,N).

% recuperer les 9 elements d'un bloc dans une liste
getBlock(-1,_,_,[]):- !.
getBlock(X,Y,[A|B],O):- X<0, X1 is (X+1), getNlist(A,Y,O1), getBlock(X1,Y,B,O2), concat(O1,O2,O).
getBlock(X,Y,I,O):- getNlist(I,X,I1), X1 is -4, getBlock(X1,Y,I1,O), !.

% recuperer les 9 elements d'une ligne dans une liste (Y coordonnée ligne)
getLine(-1,_,[]):- !.
getLine(Y,I,O):- Y<0, S is (Y+4), Y1 is Y+1, getNlist(I,S,O1), getLine(Y1,I,O2), concat(O1,O2,O).
getLine(Y,I,O):- S is floor(Y/3), S1 is (Y rem 3), Y1 is -4, getNlist(I,S,I1), getNlist(I1,S1,I2), getLine(Y1,I2,O), !.

% recuperer les 9 elements d'une colonne dans une liste

	% ---- Fonctions intermediaires ----
rowBlockFromRow([Trow|_], RowBlockIndex, Res):- RowBlockIndex =:= 0,
	Res = Trow, !.
rowBlockFromRow([_|Qrow], RowBlockIndex, Res):- NewIndex is RowBlockIndex -1,
	rowBlockFromRow(Qrow,NewIndex,Res).

columnElementFromRow(ColumnIndex,Row, ElementResult):- RowBlockIndex is floor(ColumnIndex/3),
	ColIndex is (ColumnIndex rem 3),
	rowBlockFromRow(Row, RowBlockIndex, Res),
	getNlist(Res,ColIndex,ElementResult).

columnFromRowTriplet(_,[],[]).
columnFromRowTriplet(Column,[T|Q], Res) :- columnElementFromRow(Column,T,ElementResult),
	columnFromRowTriplet(Column,Q,ElementResult2),
	concat([ElementResult],ElementResult2,Res).
	% ---------------------------------------

getColumn(_,[],[]).
getColumn(Column,[T|Q],Res):- columnFromRowTriplet(Column,T,Res2),
	getColumn(Column,Q,Res3),
	concat(Res2,Res3,Res),!.

% verification d'elements repetés dans une liste (le ' ' ne compte pas)
verif([],[]).
verif([' '|Q],E):- !, verif(Q,E).
verif([X|Q],E):- verif(Q,E), element(X,E), !, fail.
verif([X|Q],[X|E]):- verif(Q,E).

% verification validité d'un ajout
verification(X,Y,Sudoku):- getLine(X,Sudoku,Line),
	getColumn(Y,Sudoku,Column),
	BlocX is floor(X/3),
	BlocY is floor(Y/3),
	getBlock(BlocX,BlocY,Sudoku,Bloc),
	verif(Line,_),
	verif(Column,_),
	verif(Bloc,_).
verification(_,_,_):- write('Cet ajout est invalide.'),nl,nl,fail.

	%% ===ETAT DU SUDOKU=== %%

% Ajout d'un élèment par l'utilisateur dans liste des cases jouées
addPlayerMove(Coord):- current_predicate(jeuxJoueur/1), !,
	jeuxJoueur(X),
	concat(X,[Coord],Res),
	retract(jeuxJoueur(_)),
	assertz(jeuxJoueur(Res)).
addPlayerMove(Coord):- asserta(jeuxJoueur([Coord])).

% Lorsque case est effacée
deleteFromList(X,[T|Q],Output):- X = T, Output = Q.
deleteFromList(X,[T|Q],Output):- X \= T,
	deleteFromList(X,Q,RestOfList),
	concat(T,RestOfList,Output).

deletePlayerMove(Coord):- isJeuJoueur(Coord),
	jeuxJoueur(X),
	deleteFromList(Coord,X,Output),
	retract(jeuxJoueur(_)),
	assertz(jeuxJoueur(Output)).

% Savoir si une coordonnée est jouée par un joueur
isJeuJoueur(Coord):- current_predicate(jeuxJoueur/1),
	jeuxJoueur(X),
	element(Coord,X).

	% ----Savoir si le sudoku est complet----

% predicat intermediaire pr savoir si une liste est complete
listComplete([]).
listComplete([T|Q]):- (T\=' '),!,listComplete(Q).

% sudoku complet
isSudokuComplete(-1,_):-!.

isSudokuComplete(Index,Sudoku):- getLine(Index,Sudoku,Line),
	listComplete(Line),
	NewIndex is Index-1,
	isSudokuComplete(NewIndex,Sudoku).

isSudokuComplete(Sudoku):- getLine(8,Sudoku,Line),
	listComplete(Line),
	isSudokuComplete(7,Sudoku).
	% ---------------------------------------

% recuperer l'element à une certaine coordonée
getElement(X,Y,Sudoku,Element):- validCoord(X),
	validCoord(Y),
	getLine(X,Sudoku,Line),
	getNlist(Line,Y,Element).

% verifier si la case est une case jouée
isPlayableCell(X,Y,Sudoku):- getElement(X,Y,Sudoku,Element), Element = ' ',!.
isPlayableCell(X,Y,_):- isJeuJoueur([X,Y]),!.
isPlayableCell(_,_,_):- write('Cette case n\'est pas jouable!'),nl,fail.

% verifier si une case est complétée (à utiliser apres is playable isPlayableCell)

isCellCompleted(X,Y,Sudoku):- getElement(X,Y,Sudoku,Element),
													Element \= ' ',!.
isCellCompleted(_,_,_):- write('Cette case est deja vide.'),nl,fail.



	%% ===RESOLUTION SUDOKU=== %%

% Ajouter un numero random dans un sudoku ajouterRand(InSudoku,OutSudoku)
ajouterRand(I,O1):- repeat, X is random(0,9), repeat , Y is random(0,9), getElement(X,Y,I,' '), repeat, N is random(0,9),
	changer(X,Y,N,I,O), verificationInput(X,Y,O), O1=O.

% resoudre un sudoku
completerSudoku(I,O):- ajouterRand(I,O1), completerSudoku(O1,O).

% Verifier si le programme doit finir
isProgramFinished(Sudoku,_):- isSudokuComplete(Sudoku), write('Vous avez gagné !'),nl,!.
isProgramFinished(_,Choice):- Choice = 4.

	%% ===PROGRAMME PRINCIPAL=== %%

sudoku :- nl,
	write('====================================================='),nl,
	write('======= Bienvenue dans notre programme sudoku ======='),nl,
	write('====================================================='),nl,nl,
	repeat, menu, !.
menu :- write('\t\t=====  MENU  ====='),nl,nl,
	write(' Que voulez vous faire ?'),nl,nl,
	write('1. Resoudre un sudoku'),nl,
	write('2. Proposer un sudoku'),nl,
	write('4. Quitter'),nl,nl,
	write('Que voulez-vous faire ? : '),
	read(Choice), nl,
	handle(Choice),
	Choice=4, nl.

handle(1):- write('---- RESOLUTION D\'UN SUDOKU ----'),nl,
						sudokuTest(S),
						asserta(sudokuGrid(S)),
						repeat,
						userSolvingSudoku, !.
handle(2):- write('---- PROPOSER UN SUDOKU ----'), nl, !.
handle(4):- write('---- Au revoir ! ----'),!.
handle(_):- write('---- Option invalide ----'),!.

userSolvingSudoku :- nl,write('---- Complétez le sudoku ----'),nl,nl,
 	sudokuGrid(S), affS(S), nl,
	write('1. Definir numero'), nl,
	write('2. Effacer numero'), nl,
	write('4. Quitter'), nl, nl,
	write('Que voulez-vous faire ? : '),
	read(Choice), nl,
	handleResolution(Choice,S),
	isProgramFinished(S,Choice).

% Definir d'un numero
handleResolution(1,S):- write('Ligne de la case à modifier : '), read(X), validUserInputNumber(X),nl,
	write('Colonne de la case à modifier :'), read(Y), validUserInputNumber(Y),nl,
	write('Valeur de la case : '), read(N), validUserInputNumber(N),nl,
	X1 is (X-1), Y1 is (Y-1),
	isPlayableCell(X1,Y1,S),
	changer(N,X1,Y1,S,S1),
	verification(X1,Y1,S1),
	retract(sudokuGrid(S)),
	asserta(sudokuGrid(S1)),
	addPlayerMove([X1,Y1]),
	write('Succès de l\'ajout.'),nl,!.

handleResolution(1,_):- !.

% Effacer un numero
handleResolution(2,S):- write('Ligne de la case à effacer :'), read(X), validUserInputNumber(X),nl,
	write('Colonne de la case à effacer:'), read(Y), validUserInputNumber(Y),nl,
	X1 is (X-1), Y1 is (Y-1),
	isPlayableCell(X1,Y1,S),
	isCellCompleted(X1,Y1,S),
	changer(' ',X1,Y1,S,S1),
	retract(sudokuGrid(S)),
	asserta(sudokuGrid(S1)),
	deletePlayerMove([X,Y]),!.

handleResolution(2,_):- !.

handleResolution(4,_):-!.

handleResolution(_,_):- nl, write('Option invalide'), nl, !, fail.


	%% ---AUTRES2--- %%

prtch(V,X,Y,I):- changer(V,X,Y,I,O), affS(O).
