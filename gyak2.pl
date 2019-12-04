%% -*- Mode: Prolog; coding: utf-8 -*- %% example test 1 / 2019

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

:- set_prolog_flag(legacy_char_classification,on).

%% Def.: Azt mondjuk, hogy a Ks lista lexikografikusan kisebb, mint az Ns lista,
%% ha különböznek, és az első pozíciónál, ahol eltérnek, 
%% a Ks megfelelő eleme kisebb, mint az Ns megfelelő eleme,
%% vagy a Ks-nek nincs már az Ns-nek megfelelő eleme, mivel előtte véget ér.
%%
%% Mj.: Az alábbi feladatban a listaelemeket
%%      a következő rendezés szerint hasonlítjuk össze:
%%      Ha mindkettő szám, aritmetikailag, különben a standard rendezés szerint.
%% A célok futása ne hagyjon felesleges választási pontokat!
%%
%% A megoldásban használjuk a megfelelő helyen az (If->Then;Else) konstrukciót!
%% 
%% Előf: Ks, Ns valódi listák.
%% kisebbLista(Ks,Ns) :- Ks lexikografikusan kisebb, mint Ns.
%% Mj.: O(n) hatékonyságú megoldást várok, ahol n
%% a Ks-ben és az Ns-ben összesen a résztermek száma.

kisebbLista(As,As) :- fail.
kisebbLista([],[_|_]).
kisebbLista([X|Ks],[Y|Ns]) :-
    ( number(X) , number(Y) ->
      ( X =:= Y -> kisebbLista(Ks,Ns)
        ; X < Y -> true
        ; fail)
    ; ( X == Y -> kisebbLista(Ks,Ns)
        ; X @< Y -> true
        ; fail
      )
    ).

%%number(X) :- integer(X) ; float(X).

/*kisebbLista([X|Ks],[Y|Ns]) :-
    ( integer(X), integer(Y) -> ( X<Y -> true
				; X>Y -> fail
				; kisebbLista(Ks,Ns) )
    ; ( X @< Y -> true
     ; X @> Y -> fail
     ; kisebbLista(Ks,Ns) )
    ).*/
/*
| ?- kisebbLista([],[]).
no
| ?- kisebbLista([alma,2],[alma,2.0,3]).
yes
| ?- kisebbLista([],[1,2]).
yes
| ?- kisebbLista([2],[1,2]).
no
| ?- kisebbLista([5,3,1,2],[5,3,szilva]).
yes
| ?- kisebbLista([3,1.0],[3,1]).
no
| ?- kisebbLista([5,g(3.0,1),0],[5,g(3,1)]).
yes
| ?- kisebbLista([1.0,0,0],[f(x),9]).
yes
| ?- kisebbLista([1.0],[1,2]).
yes
| ?- kisebbLista([1,2],[2.0,2]).
yes
| ?- kisebbLista([2,c,1,b,v],[2,c,2.0,b]).
yes
| ?- kisebbLista([2,c,z(z)],[2.0,c,a(0,0)]).
yes
| ?- kisebbLista([2.0,c,a(0,0)],[2,c,z(z)]).
no
| ?- \+ kisebbLista([],[]), kisebbLista([alma,2],[alma,2.0,3]),
     kisebbLista([],[1,2]), \+ kisebbLista([2],[1,2]),
     kisebbLista([5,3,1,2],[5,3,szilva]), \+ kisebbLista([3,1.0],[3,1]),
     kisebbLista([5,g(3.0,1),0],[5,g(3,1)]), kisebbLista([1.0,0,0],[f(x),9]),
     kisebbLista([1.0],[1,2]), kisebbLista([1,2],[2.0,2]),
     kisebbLista([2,c,1,b,v],[2,c,2.0,b]), kisebbLista([1,3],[2,2]),
     \+kisebbLista([1.5],[1,2]), kisebbLista([1,2],[1.5]),
     kisebbLista([2,c,z(z)],[2.0,c,a(0,0)]),
     \+ kisebbLista([2.0,c,a(0,0)],[2,c,z(z)]).
yes
*/

edge(a,b).	edge(a,c).	edge(b,d).
edge(c,d).	edge(d,e).	edge(f,g).

%% dagPath(A,Z,Vertices) :-
%%     Vertices is a proper list representing a path from vertex A to vertex Z
%%     in the DAG by predicate edge/2.
/*
| ?- dagPath(a,_,Vs).
Vs = [a] ? ;    Vs = [a,b] ? ;    Vs = [a,b,d] ? ;    Vs = [a,b,d,e] ? ;
Vs = [a,c] ? ;    Vs = [a,c,d] ? ;    Vs = [a,c,d,e] ? ;     no
| ?- dagPath(_,e,Vs).
Vs = [e] ? ;    Vs = [a,b,d,e] ? ;    Vs = [a,c,d,e] ? ;
Vs = [b,d,e] ? ;    Vs = [c,d,e] ? ;    Vs = [d,e] ? ;    no
| ?- dagPath(_,_,Vs).
Vs = [_A] ? ;    Vs = [a,b] ? ;    Vs = [a,b,d] ? ;    Vs = [a,b,d,e] ? ;
Vs = [a,c] ? ;     Vs = [a,c,d] ? ;    Vs = [a,c,d,e] ? ;
Vs = [b,d] ? ;    Vs = [b,d,e] ? ;    Vs = [c,d] ? ;    Vs = [c,d,e] ? ;
Vs = [d,e] ? ;    Vs = [f,g] ? ;    no
*/

dagPath(A,A,[A]).
dagPath(A,Z,[A|Vs]) :- edge(A,B), dagPath(B,Z,Vs).

%%dagPath(X,Y,[Y]).
%%dagPath(X,Y,[Y|Zs]) :- edge(X,Z), dagPath(Y,Z,Zs).

/*
| ?- prime([],3).
yes
| ?- prime([3],5).
yes
| ?- prime([3,5,7,11,13],15).
no
| ?-  prime([3,5,7,11,13],17).
yes
| ?- prime([3,5,7,11,13,17,19,21],23).
yes
% source_info
| ?- prime([3,5,7,11,13,17,19,21,23],25).
no
% source_info
| ?- prime([3,5,7,11,13,17,19,21,23],27).
no
% source_info
| ?- prime([3,5,7,11,13,17,19,21,23],29).
yes
*/

%%prime(Ps,N) :-
prime([],_).
prime([X|Xs],N) :- \+ oszthato(X,N), prime(Xs,N).

oszthato(X,N) :- 0 is N mod X.

%% 3: Calculate the list of prime numbers until a given number. (30p)
%%    
%% PreCond: N is a number, N>=2.

%% primeList(N,Ps) :- Ps is the strictly increasingly list of primes =< N.
:-use_module(library(lists), [reverse/2]).

primeList(N,Ps) :- primes(N,[2],3,Ps).

primes(N,Ns,X,Ps) :-
    ( X =< N ->
      ( prime(Ns,X) ->
        Ms = [X|Ns], X1 is X + 2, primes(N,Ms,X1,Ps)
        ; X1 is X + 2, primes(N,Ns,X1,Ps) )
      ; reverse(Ns,Ps)
    ).

/*
| ?- primeList(2,Ps).
Ps = [2] ? ;
no
| ?- primeList(3,Ps).
Ps = [2,3] ? ;
no
| ?- primeList(4,Ps).
Ps = [2,3] ? ;
no
| ?- primeList(5,Ps).
Ps = [2,3,5] ? ;
no
| ?- primeList(6,Ps).
Ps = [2,3,5] ? ;
no
% source_info
| ?- primeList(7,Ps).
Ps = [2,3,5,7] ? ;
no
| ?- primeList(8,Ps).
Ps = [2,3,5,7] ? ;
no
% source_info
| ?- primeList(9,Ps).
Ps = [2,3,5,7] ? ;
no
% source_info
| ?- primeList(10,Ps).
Ps = [2,3,5,7] ? ;
no
% source_info
| ?- primeList(11,Ps).
Ps = [2,3,5,7,11] ? ;
no
| ?- primeList(100,Ps).
Ps = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97] ? ;
no
*/