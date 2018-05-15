use_module(library(lists)).

word(zombifies, [z,o,m,b,i,f,i,e,s]).
word(akecabele, [a,k,e,c,a,b,e,l,e]).
word(brickwork, [b,r,i,c,k,w,o,r,k]).
word(backcheck, [b,a,c,k,c,h,e,c,k]).
word(acmrremad, [a,c,m,r,r,e,m,a,d]).
word(nhgwpfabz, [n,h,g,w,p,f,a,b,z]).
word(jellybean, [j,e,l,l,y,b,e,a,n]).
word(earreoded, [e,a,r,r,e,o,d,e,d]).

word(aaaaaaaaa,[a,a,a,a,a,a,a,a,a]).

unique([]).
unique([H|T]):-
    \+member(H,T),
    unique(T).

crossword([H1,H2,H3,H4,V1,V2,V3,V4]):-
    word(H1,[_,H12,_,H14,_,H16,_,H18,_]),
    word(H2,[_,H22,_,H24,_,H26,_,H28,_]),
    word(H3,[_,H32,_,H34,_,H36,_,H38,_]),
    word(H4,[_,H42,_,H44,_,H46,_,H48,_]),
    word(V1,[_,H12,_,H22,_,H32,_,H42,_]),
    word(V2,[_,H14,_,H24,_,H34,_,H44,_]),
    word(V3,[_,H16,_,H26,_,H36,_,H46,_]),
    word(V4,[_,H18,_,H28,_,H38,_,H48,_]),
    unique([H1,H2,H3,H4,V1,V2,V3,V4]),
    write('A solution is found'), nl ,
    printAnswer([H1,H2,H3,H4],[V1,V2,V3,V4],1).


printAnswer(_,_,10).
printAnswer(H,V,LineNumber):-
    1 is LineNumber mod 2 ,
    printIthChar(V,LineNumber),
    NxtLine is LineNumber+1,
    printAnswer(H,V,NxtLine).

printAnswer(H,V,LineNumber):-
    0 is LineNumber mod 2,
    Hidx is  LineNumber/2,
    nth1(Hidx,H,Hi),
    write(Hi),
    nl,
    NxtLine is LineNumber+1,
    printAnswer(H,V,NxtLine).



substitute(_, [], _, []):-!.
substitute(X, [X|T], Y, [Y|T1]):-
	substitute(X, T, Y, T1),!.

substitute(X, [H|T], Y, [H|T1]):-
	substitute(X, T, Y, T1).


move([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew,Val):-
  	right([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew,Val).

right([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew,Val):-
  	blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew,Val).

blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9],S,Val):-
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9], Val),
  	Z is N+1,
 	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(Val,Q,R,V),
  	substitute(10,V,Val,S).

move(S,Snew,Val):-
  	left(S,Snew,Val).

left([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew,Val):-
  	blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew,Val).

blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9],S,Val):-
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

move(S,Snew,Val):-
  	down(S,Snew,Val).

down([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew,Val):-
  	blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9],S,Val):-
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

move(S,Snew,Val):-
  	up(S,Snew,Val).

up([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew,Val):-
  	blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew,Val).

blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9],S,Val):-
	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

go(Start):-
	path(start,[]).




 play:-
    crossword(Goal),
    path( [[[z,o,m,b,i,f,i,e,s],[a,k,e,c,a,b,e,l,e],[b,r,i,c,k,w,o,r,k],[b,a,c,k,c,h,e,c,k],[a,c,m,r,r,e,m,a,d],[n,h,g,w,p,f,a,b,z],[j,e,l,l,y,b,e,a,n], [e,a,r,r,e,o,d,e,d],[a,a,a,a,a,a,a,a,a]]],[],Goal).

%main predicate that takes open list, closed list and goal state
path([],_,_):-
		write('No solution'),nl,!.
path([[Goal,Parent] | _], Closed, Goal):-
		write('A solution is found'), nl ,
		printsolution([Goal,Parent],Closed),!.
path(Open, Closed, Goal) :-
		removeFromOpen(Open, [State, Parent], RestOfOpen),
		getchildren(State, RestOfOpen, Closed, Children),
		addListToOpen(Children , Open, NewOpen),
		path(NewOpen, [[State, Parent] | Closed], Goal).



getchildren(State, Open ,Closed , Children):-
		bagof(X, moves(State , Open, Closed, X), Children) , !.
		% write(X),
		% write(Children),nl.
		% getchildren(State, Open ,Closed , [Children,_]).

getchildren( _ , _ , _ , []).

addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).  % except for the root.

removeFromOpen([State|RestOpen], State, RestOpen).

moves(State,  Open, Closed, Next) :-
        member(Val, State),
		move(State,Next,Val),
		\+ member([Next,_],Open),
		\+ member([Next,_],Closed).

read8words(9).
read8words(C):-
    write('please enter the '),
    write(C),
    write(' -th word'),nl,
    read(Word),
    stringToChars(Word,Chars),
    asserta(word(Word,Chars)),
    C1 is C+1,
    read8words(C1).


stringToChars(String,Chars):-
    string_codes(String,Codes),
    codeToChars(Codes,Chars).

codeToChars([],[]).
codeToChars([CodeHead|CodeTail],Chars):-
    name(Char,[CodeHead]),
    codeToChars(CodeTail,NextChars),
    append([Char],NextChars,Chars).


printIthChar([],_):-nl.
printIthChar([H|T],I):-
    % each element is a string
    stringToChars(H,Chars),
    nth1(I,Chars,C),
    write(' '),
    write(C),
    printIthChar(T,I).

