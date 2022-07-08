/* Лабиринт " поиск залота в пещере"
_________________
|           |                    |          
-   A      |        B         I
|__    _ |__    _____  |
|           |         |          |
|   C      |  D    | Gold  |
|                     |_    __| 
|          |          |          |
|          |              E     |
|_____|_____|_____|

*/

domains
list=char*

predicates
nondeterm room(char,char)
nondeterm way(char,char,list)
nondeterm start (char)
nondeterm member(char,list)
print_way(list)
reverse(list,list)
append(list,list,list)

clauses
% описание лабиринта_________________________
room('A','C'). room('C','D'). room('D','B').
room('D','E'). room('E','G'). 
%Нахождение пути в 'G'_______________________
way(A,'G',L):-
	room(A,'G'),!,
	reverse(L,L1),append(L1,['G'],L2),
	write(L2),nl,nl,
	print_way(L2). % путь найден.
way(X,Y,L):-room(X,Y),not(member(Y,L)), room(Y,Z),way(Y,Z,[Y|L]).       % переходим с другую комнату

% печать решения
print_way([]):-write("золото наше !!!"),nl.
print_way([X|L]):-write(X,"->"),print_way(L).

% вспомогательные предикаты_________________
member(X,[X|_]):-!.
member(X,[_|L]):-member(X,L).


reverse([],[]).
reverse([X|L],L1):-reverse(L,L2),append(L2,[X],L1).

append([],L,L).
append([X|L1],L2,[X|L]):-append(L1,L2,L).

%_________________________________________
start(X) :- way(X,_,[X]),nl,!.
start(_):-write("Пути нет!!").
goal
start ('A').
