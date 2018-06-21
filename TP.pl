
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
				[[' ',6,' '],[8,3,' '],[4,' ',' ']]]]).

sudokuTest([[[[9,' ',8],[4,1,7],[6,5,' ']],
			[[6,' ',1],[5,9,2],[7,4,8]],
			[[7,5,4],[6,8,3],[1,' ',9]]],
			[[[4,9,5],[3,7,6],[2,8,1]],
			[[1,7,3],[9,2,8],[5,6,' ']],
			[[2,8,6],[1,4,5],[3,9,7]]],
			[[[8,1,2],[7,5,4],[9,3,6]],
			[[3,4,7],[2,6,9],[8,1,5]],
			[[5,6,9],[8,3,1],[4,7,' ']]]]).

			sudokuComplet([[[[9,2,8],[4,1,7],[6,5,3]],
						[[6,3,1],[5,9,2],[7,4,8]],
						[[7,5,4],[6,8,3],[1,2,9]]],
						[[[4,9,5],[3,7,6],[2,8,1]],
						[[1,7,3],[9,2,8],[5,6,4]],
						[[2,8,6],[1,4,5],[3,9,7]]],
						[[[8,1,2],[7,5,4],[9,3,6]],
						[[3,4,7],[2,6,9],[8,1,5]],
						[[5,6,9],[8,3,1],[4,7,2]]]]).


sudokuEmpty([[[[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']],
[[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']],
[[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']]],
[[[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']],
[[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']],
[[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']]],
[[[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']],
[[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']],
[[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']]]]).

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

%afficher Liste
afficheListe([]):-!.
afficheListe([T|Q]):- write(T),write('-'),
											afficheListe(Q),!.

afficheListe(X):- write('Not a list').

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
	verif(Bloc,_),!.
verification(_,_,_):- write('Cet ajout est invalide.'),nl,nl,fail.

% verification validité d'une case dans le cas de la verification d'un sudoku entier
verification2(X,Y,Sudoku):- getLine(X,Sudoku,Line),
	getColumn(Y,Sudoku,Column),
	BlocX is floor(X/3),
	BlocY is floor(Y/3),
	getBlock(BlocX,BlocY,Sudoku,Bloc),
	verif(Line,_),
	verif(Column,_),
	verif(Bloc,_),!.
verification2(_,_,_):- fail.

	%% ===ETAT DU SUDOKU=== %%

% Ajout d'un élèment par l'utilisateur dans liste des cases jouées
addPlayerMove(Coord):- current_predicate(jeuxJoueur/1), !,
	jeuxJoueur(X),
	concat(X,[Coord],Res),
	retract(jeuxJoueur(_)),
	assertz(jeuxJoueur(Res)).
addPlayerMove(Coord):- asserta(jeuxJoueur([Coord])).

% Lorsque case est effacée
% deleteFromList(X,[T|Q],Output):- X = T, Output = Q.
% deleteFromList(X,[T|Q],Output):- X \= T,
%	deleteFromList(X,Q,RestOfList),
%	concat(T,RestOfList,Output).

deleteFromList(_,[],[]).
deleteFromList(X,[X|Q],Output):- deleteFromList(X,Q,Output), !.
deleteFromList(X,[T|Q],[T|RestOfList]):- deleteFromList(X,Q,RestOfList), !.

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
%listComplete([T|Q]):- (T\=' '),!,listComplete(Q).
listComplete([' '|_]):- !, fail.
listComplete([T|_]):- isList(T), !, fail.
listComplete([_|Q]):- listComplete(Q).

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

% Verifier si la resolution est terminée (choix 4 ou sudoku complet)
isSolvingFinished(Sudoku,_):- isSudokuComplete(Sudoku), write('Vous avez gagné !'),nl,!.
isSolvingFinished(_,Choice):- Choice = 3.

% Verifier si la proposition de sudoku est terminée (choix 3 ou 4)
isPropositionFinished(Choice):- Choice = 3,!.
isPropositionFinished(Choice):- Choice = 4.


	%% ===RESOLUTION SUDOKU PAR L'ORDINATEUR === %%

% Ajouter un numero random dans un sudoku ajouterRand(InSudoku,OutSudoku)
%ajouterRand(I,O1):- repeat, random(0,9,X), repeat , random(0,9,Y), getElement(X,Y,I,' '),
	%repeat, random(0,9,N), changer(X,Y,N,I,O), verification(X,Y,O), O1=O.

% On associe à chaque case vide une liste des possibles avec toutes les valeurs
setPossibles4([],[]).
setPossibles4([' '|B],[[1,2,3,4,5,6,7,8,9]|D]):- setPossibles4(B,D).
setPossibles4([A|B],[A|D]):- setPossibles4(B,D).

setPossibles3([],[]).
setPossibles3([A|B],[C|D]):- setPossibles4(A,C), setPossibles3(B,D).

setPossibles2([],[]).
setPossibles2([A|B],[C|D]):- setPossibles3(A,C), setPossibles2(B,D).

setPossibles([],[]).
setPossibles([A|B],[C|D]):- setPossibles2(A,C), setPossibles(B,D),!.

% on associe à chaque case vides une liste de possibles et on garde ses coordonnees (predicat casesVides([[X,Y],[X1,Y1],...]).)
ajouterPossibles4([],[],_,_).
ajouterPossibles4([' '|B],[[1,2,3,4,5,6,7,8,9]|D],X,Y):- casesVides(L), concat(L,[[X,Y,1]],L1),
	retract(casesVides(_)), asserta(casesVides(L1)), Y1 is (Y+1), ajouterPossibles4(B,D,X,Y1).
ajouterPossibles4([A|B],[A|D],X,Y):- Y1 is (Y+1), ajouterPossibles4(B,D,X,Y1).

ajouterPossibles3([],[],_,_).
ajouterPossibles3([A|B],[C|D],X,Y):- ajouterPossibles4(A,C,X,Y), Y1 is (Y+3), ajouterPossibles3(B,D,X,Y1).

ajouterPossibles2([],[],_).
ajouterPossibles2([A|B],[C|D],X):- ajouterPossibles3(A,C,X,0), X1 is (X+1), ajouterPossibles2(B,D,X1).

ajouterPossibles([],[],_).
ajouterPossibles([A|B],[C|D],X):- ajouterPossibles2(A,C,X), X1 is (X+3), ajouterPossibles(B,D,X1),!.

ajouterPossibles(I,O):- asserta(casesVides([])), ajouterPossibles(I,O,0).

% eliminer tous les elements de la liste E presents dans la liste In: deleteTouteListe(In,E,Out)
deleteTouteListe([A|[]],_,A):- !.
% deleteTouteListe([A|[]],_,[A]):- !.
deleteTouteListe(I,[],I):- !.
deleteTouteListe(I,[A|B],O):- deleteFromList(A,I,O1), deleteTouteListe(O1,B,O),!.

% reduire la liste de posibles en fonctions des numeros dans la colonne, la ligne, et le block
reduireColonne(X,Y,I,O):- getElement(X,Y,I,CurrentList), isList(CurrentList), getColumn(Y,I,Column), deleteFromList(CurrentList,Column,Column2),
	deleteTouteListe(CurrentList,Column2,O1), changer(O1,X,Y,I,O), !.
reduireColonne(_,_,I,I).

reduireLigne(X,Y,I,O):- getElement(X,Y,I,CurrentList), isList(CurrentList), getLine(X,I,Line), deleteFromList(CurrentList,Line,Line2),
	deleteTouteListe(CurrentList,Line2,O1), changer(O1,X,Y,I,O), !.
reduireLigne(_,_,I,I).

reduireBlock(X,Y,I,O):- getElement(X,Y,I,CurrentList), isList(CurrentList), Xblock is floor(X/3), Yblock is floor(Y/3), getBlock(Xblock,Yblock,I,Block),
	deleteFromList(CurrentList,Block,Block2), deleteTouteListe(CurrentList,Block2,O1), changer(O1,X,Y,I,O), !.
reduireBlock(_,_,I,I).

reduirePossibles(X,Y,I,O):- reduireColonne(X,Y,I,O1), reduireLigne(X,Y,O1,O2), reduireBlock(X,Y,O2,O).

% verifier si un changement va etre effectue dans la listes des possibles isChanged(In,Out,X,Y,OldVal,NewVal,Changed)
%% si la liste est reduite a 1 chiffre la case est eliminee de la liste de cases vides, si elle est reduite la
%% case est concatenée dans la fin de la liste avec valeur 1, sinon elle est concatenée au debut avec valeur 0
isChanged(X,Y,Val,Val):- casesVides(L), deleteFromList([X,Y,_],L,L1), retract(casesVides(_)),
	concat([[X,Y,0]],L1,L2), asserta(casesVides(L2)), !.
isChanged(X,Y,_,Val):- isList(Val), casesVides(L), deleteFromList([X,Y,_],L,L1), retract(casesVides(_)),
	concat(L1,[[X,Y,1]],L2), asserta(casesVides(L2)).
isChanged(X,Y,_,_):- casesVides(L), deleteFromList([X,Y,_],L,L1), retract(casesVides(_)), asserta(casesVides(L1)), write('('), write(X), write(';'), write(Y), write(')').

% reduire la liste de possibles en modifiant casesVides()
reducePossibles(X,Y,I,O):- getElement(X,Y,I,OldValue), isList(OldValue), reduireColonne(X,Y,I,O1), reduireLigne(X,Y,O1,O2), reduireBlock(X,Y,O2,O),
	getElement(X,Y,O,NewValue), isChanged(X,Y,OldValue,NewValue), !.
reducePossibles(_,_,I,I).

% reduirePossibles(X,Y,I,O):-  Xblock is floor(X/3), Yblock is floor(Y/3), getElement(X,Y,I,CurrentList),
% 	getBlock(Xblock,Yblock,I,Block), deleteFromList(CurrentList,Block,Block2), deleteTouteListe(CurrentList,Block2,O1),
%	getLine(X,I,Line), deleteFromList(CurrentList, Line, Line2), deleteTouteListe(O1,Line2,O2),
%	getColumn(Y,I,Column), deleteFromList(CurrentList, Column, Column2), deleteTouteListe(O2,Column2,O3),
%	changer(O3,X,Y,I,O).

% completer une case vide tant qu il exite une liste de possibles pouvant etre reduite a 1 chiffre : completer(I,O,CasesVides)
completer(I,I,[]).
completer(I,O,[[X|[Y|_]]|Q]):- reducePossibles(X,Y,I,O1), completer(O1,O,Q).

% verifier si les cases vides ont change
inLoop([]).
inLoop([[_|[_|[1|_]]]|_]):- !, fail.
inLoop([_|Q]):- inLoop(Q).

% complete toutes les cases à valeurs determinables, Fin=1 si le sudoku est complet, Fin=0 si il faut faire un choix
%% ne pas oublier de faire un retract(casesVides(_)) après!!
completerSudoku1(I,I,1):- isSudokuComplete(I), !.
completerSudoku1(I,I,0):- casesVides(L), inLoop(L).
completerSudoku1(I,O,Fin):- casesVides(L), write('-'), completer(I,O1,L), nl, completerSudoku1(O1,O,Fin).
completerSudoku(I,O,Fin):- ajouterPossibles(I,O1), completerSudoku1(O1,O,Fin).

%% Generation aléatoire de sudoku

createRandomGrid:-sudokuEmpty(S),
									 setPossibles(S,GrilleDesPossibles),
									 repeat,
									 iterateToCreateGrid(GrilleDesPossibles,CompleteGrid),
									 takeOffCells(CompleteGrid,FinalGrid),
									 affS(FinalGrid),!.



iterateToCreateGrid(GrilleDesPossibles,Result):-	write('generating grid ...'),nl,
																									iterateToCreateGrid(0,0,GrilleDesPossibles,GrilleResultat),
																									isSudokuValide(GrilleResultat),
																									Result = GrilleResultat.

iterateToCreateGrid(8,8,GrilleDesPossibles,Result):- reduirePossibles(8,8,GrilleDesPossibles,Res),
																											getElement(8,8,Res,ListeDesPossibles),
																											randomInListeDesPossibles(ListeDesPossibles, ChoosenElement),
																											changer(ChoosenElement,8,8,GrilleDesPossibles,Result),!.


iterateToCreateGrid(X,Y,GrilleDesPossibles,Result):- reduirePossibles(X,Y,GrilleDesPossibles,Res),
																											getElement(X,Y,Res,ListeDesPossibles),
																											randomInListeDesPossibles(ListeDesPossibles, ChoosenElement),
																											changer(ChoosenElement,X,Y,GrilleDesPossibles,NewGrilleDesPossibles),
																											nextCoordinatesForIteration(X,Y,X1,Y1,ChangedLine),
																											setNewLine(ChangedLine,NewGrilleDesPossibles,NewGrilleDesPossibles2),
																											iterateToCreateGrid(X1,Y1,NewGrilleDesPossibles2,Result).

%setNewLine fixe les element qui n'ont qu'un possible si on est passé à une nouvelle ligne

setNewLine(0,GrillePossible,GrillePossible):-!.
setNewLine(IndexLine,GrillePossible,Res):- setUniquePossible(GrillePossible, IndexLine, Res).


nextCoordinatesForIteration(8,8,8,8,_):- fail,!.
nextCoordinatesForIteration(OldX,OldY,NewX,NewY,ChangedLine):- OldY =:= 8,
																											NewX is OldX+1,
																											NewY is 0,
																											ChangedLine is NewX,!.
nextCoordinatesForIteration(OldX,OldY,NewX,NewY,ChangedLine):- NewY is OldY+1,
																									 NewX is OldX,
																									 ChangedLine is 0,!.


randomInListeDesPossibles(Liste, ChoosenElement):-  isList(Liste),
																										listLength(Liste,IndexMax),
																										random(0,IndexMax,RandomIndex),
																										getNlist(Liste,RandomIndex,ChoosenElement),!.

randomInListeDesPossibles(Liste, ChoosenElement):-  ChoosenElement = Liste.


listLength([],0).
listLength([_|Q],Length):- listLength(Q,LengthOfTheRest),
											 Length is LengthOfTheRest+1.

% Pour chaque element d'une ligne, si un element ne possède plus qu'un element dans sa liste des possibles,
% Alors on donne cette valeur à la case.
setUniquePossible([]).
setUniquePossible(Grille, X, Res):- iterateUniquePossible(Grille, X,0,Res).

iterateUniquePossible(Grille,_,9,Grille):- !.
iterateUniquePossible(Grille,X,IndexColonne,Res):- reduirePossibles(X,IndexColonne,Grille,GrilleReduite),
																								getElement(X,IndexColonne,GrilleReduite,Element),
																								listLength(Element,Length),
																								Length is 1,
																								getNlist(0,Element,Value),
																								changer(Value,X,IndexColonne,Grille,NewGrille),
																								IndexColonne1 is IndexColonne +1,
																								iterateUniquePossible(NewGrille,X,IndexColonne1,Res),!.

iterateUniquePossible(Grille,X,IndexColonne,Res):- IndexColonne1 is IndexColonne+1,
																									iterateUniquePossible(Grille,X,IndexColonne1,Res).

isSudokuValide(Sudoku):- iterateIsSudokuValide(0,0,Sudoku).

iterateIsSudokuValide(8,8,Sudoku):- verification2(8,8,Sudoku),!.

iterateIsSudokuValide(X,Y,Sudoku):- verification2(X,Y,Sudoku),
																		nextCoordinatesForIteration(X,Y,X1,Y1,_),
																		iterateIsSudokuValide(X1,Y1,Sudoku).

takeOffCells(Grid,NewGrid):- random(1,10,X1),
															random(1,10,Y1),
															random(1,10,X2),
															random(1,10,Y2),
															random(1,10,X3),
															random(1,10,Y3),
															changer(' ',X1,Y1,Grid,Grid1),
															changer(' ',X2,Y2,Grid1,Grid2),
															changer(' ',X3,Y3,Grid2,NewGrid).




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
handle(2):- write('---- PROPOSER UN SUDOKU ----'), nl,
						sudokuEmpty(S),
						asserta(newSudokuProposedByUser(S)),
						repeat,
						obtainSudokuFromUser,!.
handle(4):- write('---- Au revoir ! ----'),!.
handle(_):- write('---- Option invalide ----'),!.


% --------------------------------------------------------------
% ----------- 		Resolution d'un sudoku 	--------------------
% --------------------------------------------------------------

userSolvingSudoku :- nl,write('---- Complétez le sudoku ----'),nl,nl,
 	sudokuGrid(S), affS(S), nl,
	write('1. Definir un numero'), nl,
	write('2. Effacer un numero'), nl,
	write('3. Quitter'), nl, nl,
	write('Que voulez-vous faire ? : '),
	read(Choice), nl,
	handleResolution(Choice,S),
	sudokuGrid(ModifiedGrid),
	isSolvingFinished(ModifiedGrid,Choice).

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

handleResolution(3,_):-!.

handleResolution(_,_):- nl, write('Option invalide'), nl, !, fail.

% --------------------------------------------------------------
% ----------- 		Proposition d'un sudoku 	--------------------
% --------------------------------------------------------------

obtainSudokuFromUser :- nl,write('---- Modifiez le sudoku à soumettre à l\'ordinateur ----'),nl,nl,
 	newSudokuProposedByUser(S), affS(S), nl,
	write('1. Ajouter un numero'), nl,
	write('2. Effacer un numero'), nl,
	write('3. Valider et soumettre le sudoku'), nl,
	write('4. Quitter'), nl, nl,
	write('Que voulez-vous faire ? : '),
	read(Choice), nl,
	handleProposition(Choice,S),
	isPropositionFinished(Choice).

	% Ajouter un numero
	handleProposition(1,S):- write('Ligne de la case à modifier : '), read(X), validUserInputNumber(X),nl,
		write('Colonne de la case à modifier :'), read(Y), validUserInputNumber(Y),nl,
		write('Valeur de la case : '), read(N), validUserInputNumber(N),nl,
		X1 is (X-1), Y1 is (Y-1),
		changer(N,X1,Y1,S,S1),
		verification(X1,Y1,S1),
		retract(newSudokuProposedByUser(S)),
		asserta(newSudokuProposedByUser(S1)),
		write('Numero ajouté'),nl,nl,!.

	handleProposition(1,_):- !.

	% Effacer un numero
	handleProposition(2,S):- write('Ligne de la case à effacer :'), read(X), validUserInputNumber(X),nl,
		write('Colonne de la case à effacer:'), read(Y), validUserInputNumber(Y),nl,
		X1 is (X-1), Y1 is (Y-1),
		changer(' ',X1,Y1,S,S1),
		retract(newSudokuProposedByUser(S)),
		asserta(newSudokuProposedByUser(S1)),
		write('Numero supprimé'),nl,nl,!.

	handleProposition(2,_):- !.

	% Effacer un numero
	handleProposition(3,S):- write('Sudoku validé'),nl,nl, affS(S).

	handleProposition(3,_):- !.

	handleProposition(4,_):-!.

	handleProposition(_,_):- nl, write('Option invalide'), nl, !, fail.

	%% ---AUTRES2--- %%

prtch(V,X,Y,I):- changer(V,X,Y,I,O), affS(O).
