use_module(library(lists)).
word(zombifies, [z,o,m,b,i,f,i,e,s]).
word(akecabele, [a,k,e,c,a,b,e,l,e]).
word(brickwork, [b,r,i,c,k,w,o,r,k]).
word(backcheck, [b,a,c,k,c,h,e,c,k]).
word(acmrremad,[a,c,m,r,r,e,m,a,d]).
word(nhgwpfabz,[n,h,g,w,p,f,a,b,z]).
word(jellybean, [j,e,l,l,y,b,e,a,n]).
word(earreoded, [e,a,r,r,e,o,d,e,d]).

word(aaaaaaaaa,[a,a,a,a,a,a,a,a,a]).

unique([]).
unique([H|T]):-
    \+member(H,T),
    unique(T).

crossword(H1,H2,H3,H4,V1,V2,V3,V4):-
    word(H1,[_,H12,_,H14,_,H16,_,H18,_]),
    word(H2,[_,H22,_,H24,_,H26,_,H28,_]),
    word(H3,[_,H32,_,H34,_,H36,_,H38,_]),
    word(H4,[_,H42,_,H44,_,H46,_,H48,_]),
    word(V1,[_,H12,_,H22,_,H32,_,H42,_]),
    word(V2,[_,H14,_,H24,_,H34,_,H44,_]),
    word(V3,[_,H16,_,H26,_,H36,_,H46,_]),
    word(V4,[_,H18,_,H28,_,H38,_,H48,_]),
    unique([H1,H2,H3,H4,V1,V2,V3,V4]),
    printAnswer([H1,H2,H3,H4],[V1,V2,V3,V4],1).


read8words(8).
read8words(C):-
    write('please enter the '),
    write(C),
    write(' -th word'),nl,
    read(Word),
    stringToChars(Word,Chars),
    asserta(word(X,Chars)),
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

printIthChar([],_):-nl.
printIthChar([H|T],I):-
    % each element is a string
    stringToChars(H,Chars),
    nth1(I,Chars,C),
    write(' '),
    write(C),
    printIthChar(T,I).

