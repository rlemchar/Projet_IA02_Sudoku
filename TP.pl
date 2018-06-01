
							%%% SUDOKU-SUDOKU-SUDOKU %%%

	%% ---AUTRES--- %%
%	[[[[a,b,c],[d,e,f],[g,h,i]],
%	  [[a,b,c],[d,e,f],[g,h,i]],
%	  [[a,b,c],[d,e,f],[g,h,i]]],
%	 [[[a,b,c],[d,e,f],[g,h,i]],
%	  [[a,b,c],[d,e,f],[g,h,i]],
%	  [[a,b,c],[d,e,f],[g,h,i]]],
%	 [[[a,b,c],[d,e,f],[g,h,i]],
%	  [[a,b,c],[d,e,f],[g,h,i]],
%	  [[a,b,c],[d,e,f],[g,h,i]]]]
	
non(A) :- A, !, fail.
non(_).

% appartenance à une liste
element(_,[]):- fail.
element(X,[X|_]).
element(X,[_|Q]):- element(X,Q).

% is list?
isList([]).
isList([_|B]):- isList(B).

% concatenation de listes
concat([],C,C).
concat([A|B],C,[A|E]):- concat(B,C,E),!.

%% resto R is (A rem B).
%% floor() para la division euclidiana

	%% ---AFFICHAGE--- %%

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

	%% ---MODIFICATION DES VALEURS--- %%

% remplacer l'Xeme element de la ligne est remplace par V
changer2(V,0,[_|B],[V|B]).
changer2(V,X,[A|B],[C|D]):- X>0, X1 is (X-1), changer2(V,X1,B,D), C = A.
changer1(V,X,[A|B],[C|D]):- X<3, changer2(V,X,A,C), D = B, !.
changer1(V,X,[A|B],[C|D]):- X1 is (X-3), changer1(V,X1,B,D), C = A.

% remplacer l'element (X,Y) par V (utiliser seulement changer(V,X,Y,List_In,List_Out))
changer3(V,X,0,[A|B],[C|B]):- changer1(V,X,A,C).
changer3(V,X,Y,[A|B],[C|D]):- Y>0, Y1 is (Y-1), changer3(V,X,Y1,B,D), C = A.
changer(V,X,Y,[A|B],[C|D]):- Y<3, changer3(V,X,Y,A,C), D = B, !.
changer(V,X,Y,[A|B],[C|D]):- Y1 is (Y-3), changer(V,X,Y1,B,D), C = A.

% verifier que c'est un chiffre valide
correct(X):- X>0, X=<9, integer(X), !.
correct(_):- fail.


	%% ---VERIFICATION SUDOKU--- %%

% recuperer Xeme element (valeur ou liste) de la liste
getNlist([A|_],0,A).
getNlist([_|B],X,N) :- X>=1, X1 is X-1, getNlist(B,X1,N).

% recuperer les 9 elements d'un bloc dans une liste
getBlock(_,-1,_,[]):- !.
getBlock(X,Y,[A|B],O):- Y<0, Y1 is (Y+1), getNlist(A,X,O1), getBlock(X,Y1,B,O2), concat(O1,O2,O).
getBlock(X,Y,I,O):- getNlist(I,Y,I1), Y1 is -4, getBlock(X,Y1,I1,O), !.

% recuperer les 9 elements d'une ligne dans une liste
getLine(-1,_,[]):- !.
getLine(Y,I,O):- Y<0, S is (Y+4), Y1 is Y+1, getNlist(I,S,O1), getLine(Y1,I,O2), concat(O1,O2,O).
getLine(Y,I,O):- S is floor(Y/3), S1 is (Y rem 3), Y1 is -4, getNlist(I,S,I1), getNlist(I1,S1,I2), getLine(Y1,I2,O), !.

% recuperer les 9 elements d'une colonne dans une liste


% verification d'elements repetés dans une liste (le ' ' ne compte pas)
%verif([],_):- write('fin').
%verif([' '|Q],E):- verif(Q,E).
%verif([X|_],E):- element(X,E), !, fail.
%verif([X|Q],E):- concat([X],E,E1), verif(Q,E1).

verif([],[]).
verif([' '|Q],E):- !, verif(Q,E).
verif([X|Q],E):- verif(Q,E), element(X,E), !, fail.
verif([X|Q],[X|E]):- verif(Q,E).

	%% ---PROGRAMME PRINCIPAL--- %%

menu_sudoku :- repeat, menu, !.
menu :- nl, write('====================================================='),nl,
	write('======= Bienvenue dans notre programme sudoku ======='),nl,
	write('====================================================='),nl,nl,
	write(' Que voulez vous faire ?'),nl,nl,
	write('1. Resoudre un sudoku'),nl,
	write('2. Proposer un sudoku'),nl,
	write('4. Quitter'),nl,nl,
	write('Entrer un choix :'),
	read(Choice), nl,
	handle(Choice),
	Choice=4, nl.

handle(1):- write('---- Resolution sudoku ----'),!.
handle(2):- write('---- Proposition sudoku ----'),!.
handle(4):- write('---- Au revoir ! ----'),!.
handle(_):- write('---- Vous avez mal choisi ----'),!.

	%% ---AUTRES2--- %%

prtch(V,X,Y,I):- changer(V,X,Y,I,O), affS(O).