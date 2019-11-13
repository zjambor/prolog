%% -*- Mode: Prolog; coding: utf-8 -*- %% example test 1 / 2019

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(100)]).

%% Given the precondition (PreCond) of the predicate,
%% the search trees of the appropriate goals must be finite.
%%
%% The run of the goals should not leave unnecessary choice points.
%% Take advantage of first argument indexing and last call optimization.
%%
%% Provided a test with some solutions given, your program should produce the
%% same bag of solutions, possibly in a different order. No full test is given.
%%
%% The Prolog environment should not send any warning on your solutions.
%%
%% LI: the number of predicate calls while the program runs.
%%
%% Exercise: This file must be renamed and completed.
%% The name of the file to submit: x_1.pl where x is your Neptun code.
%%
%% Presentation: 1. Run \\nas1\zh1\plzh 
%%               2. Copy your solution here: megoldások\2019_1015


%% 3. Define the notion of path in DAG (directed acyclic graph) (15p)

%% PreCond: The DAG is defined by the edges of predicate edge/2

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

%% Solution:

dagPath(X,X,[X]).
dagPath(X,Y,[Y|Zs]) :- edge(X,Z), dagPath(Y,Z,Zs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 2: Decide, if a positive odd number is prime or not. (15p)
%%    
%% PreCond: N is odd number and Ps is a proper list of the odd prime numbers < N.

%% prime(Ps,N) :- N is a prime number.
%%
%% LI: O(|Ps|)
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

%% Solution:

prime([],_).
prime([X|Zs],P) :- \+ 0 is P mod X, prime(Zs,P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% 3: Calculate the list of prime numbers until a given number. (30p)
%%    
%% PreCond: N is a number, N>=2.

%% primeList(N,Ps) :- Ps is the strictly increasingly list of primes =< N.
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

%% Solution:

:-use_module(library(lists), [reverse/2]).

primeList(N,Ps) :- primes(N,[2],3,Ps).

primes(N,Ns,X,Ps) :-
    ( X =< N ->
      ( prime(Ns,X) ->
        Ms = [X|Ns], X1 is X + 2, primes(N,Ms,X1,Ps)
        ; X1 is X + 2, primes(N,Ns,X1,Ps) )
      ; reverse(Ns,Ps)
    ).

/*primeList(N,Ps) :- primeList_app(N,[],2,Ps).

primeList_app(N,Zs,X,Ps) :-
    ( X<N, prime(Zs,X) -> Zs=[X], X1 is X + 1, primeList_app(N,Zs,X1,Ps)
    ; Ps = Zs ).

:-use_module(library(lists), [reverse/2]).*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
