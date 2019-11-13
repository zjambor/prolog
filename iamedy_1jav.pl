%% -*- Mode: Prolog; coding: utf-8 -*- %% 1. zh 2019.11.05.

%% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell
%% legyen.
%% A célok futása ne hagyjon felesleges választási pontokat!
%% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
%%
%% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
%% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%%
%% Teljes értékű megoldásra a Prolog környezet sem küld figyelmeztetést.
%% Egy ismeretlen nevét aláhúzásjellel kezdjük <=> a klózban egyszer fordul
%% elő.
%%
%% LI: A program futtatásához szükséges predikátumhívások száma.
%%
%% Feladat: ezt a fájlt kell átnevezni, majd kiegészíteni.
%% A beadandó fájl neve: x_1.pl, ahol x az Ön neptun kódja, kisbetűkkel.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1: Adjon az összeadás (plus/3) predikátumtól
%%    független megoldást az s-számok elfelezésére! (20 pont)
%%
%% Előf: M vagy N vagy K s-szám, azaz természetes szám a
%%                        0, s(0), s(s(0)), ... reprezentációban.
%%
%% felez(M,N,K) :-
%%     az M s-számot elfelezve kapjuk az N és a K s-számokat, úgy,
%%     hogy N és K legfeljebb eggyel térhetnek el egymástól, és
%%     az összegük M.
%%
%% LI: O(M)
%%
/*
| ?- felez(0,N,K).
N = 0,
K = 0
| ?- felez(s(s(0)),N,K).
N = s(0),
K = s(0) 
| ?- felez(s(s(s(s(s(0))))),N,K).
N = s(s(0)),
K = s(s(s(0))) ? ;
N = s(s(s(0))),
K = s(s(0)) ? ;
no
| ?- felez(A,s(0),E).
A = s(0),
E = 0 ? ;
A = s(s(0)),
E = s(0) ? ;
A = s(s(s(0))),
E = s(s(0)) ? ;
no
| ?- felez(A,B,s(s(0))).
A = s(s(s(0))),
B = s(0) ? ;
A = s(s(s(s(0)))),
B = s(s(0)) ? ;
A = s(s(s(s(s(0))))),
B = s(s(s(0))) ? ;
no
*/

%% Megoldás:

felez(0,0,0).
felez(s(0),s(0),0).
felez(s(0),0,s(0)).
felez(s(s(M)),s(N),s(K)) :- felez(M,N,K).

%%! 20p

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 2: Melyek az N hosszú bitlisták? (20p)

%% Előf: N>=0 egész szám.
%%
%% nBs(N,Bs) :- Bs egy N hosszú valódi bitlista.
%% LI: O(N), megoldásonként.
/*
| ?- nBs(0,Bs).
Bs = []
| ?- nBs(1,Bs).
Bs = [0] ? ;    Bs = [1] ? ;    no
| ?- nBs(3,Bs).
Bs = [0,0,0] ? ;    Bs = [0,0,1] ? ;    Bs = [0,1,0] ? ;    Bs = [0,1,1] ? ;
Bs = [1,0,0] ? ;    Bs = [1,0,1] ? ;    Bs = [1,1,0] ? ;    Bs = [1,1,1] ? ;
no
| ?- nBs(2,[1,0]).
yes
| ?- nBs(2,[1,0,1]).
no
| ?- nBs(2,[0]).
no
| ?- nBs(2,[1,a]).
no
*/
%% Megoldás:

nBs(0,[]). %%! Felesleges választási pontot hagy.  nBs(0,Us) :- !, Us=[].
nBs(N,Bs) :- 
    ( N > 0 -> N1 is N-1,
      (Bs=[0|Xs] ; Bs=[1|Xs]),
      nBs(N1,Xs)
    ).

%%! 19p

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 3: bináris számjegylisták "kizáró vagy" művelete. (20p)
%%
%% Repr: A nemnegatív bináris számokat most 0,1-ek valódi listájaként
%%   ábrázoljuk, a legmagasabb helyiértékkel kezdve.
%%   Pl. 110 egy reprezentációja [1,1,0] (vezető nullák megengedettek).
%%
%% Előf: As és Bs 0,1-ek valódi, egyforma hosszú nemüres listája.
%%
%% xorBins(As,Bs,Cs) :-
%%     a Cs bináris számjegylista az As és a Bs által reprezentált 
%%     bináris számokra alkalmazott bitenkénti "kizáró vagy" művelettel
%%     kapott számot ábrázolja, az As-sel és Bs-sel azonos hosszon.
%%
%% Mj.: A feladatot a beépített és a könyvtári eljárások felhasználása 
%%      nélkül oldjuk meg! Kivételek: !/0, ;/2, (->)/2, (=)/2, (==)/2 (\==)/2.
%% LI: O(N), ahol N az As és a Bs közös hossza.
/*
| ?- xorBins([1],[1],Cs).
Cs = [0] 
| ?- xorBins([0],[1],Cs).
Cs = [1]
| ?- xorBins([1],[1],[0]).
yes
| ?- xorBins([1],[1],[]).
no
| ?- xorBins([1],[1],[1]).
no
| ?- xorBins([0,1],[0,1],Cs).
Cs = [0,0]
| ?- xorBins([1,0,0,1,0],[0,1,1,1,0],Cs).
Cs = [1,1,1,0,0]
| ?- xorBins([1,0,0,1],[1,1,0,0],Cs).
Cs = [0,1,0,1]
| ?- xorBins([0,1],[1,0],[1,1]).
yes
| ?- xorBins([0,1],[1,0],[0,1,1]).
no
| ?- xorBins([1,1],[1,0],[0,1]).
yes
| ?- xorBins([1,1],[1,0],[1]).
no
*/
%% Megoldás:

xorBins([],[],[]).
xorBins([A|As],[B|Bs],Cs) :-
    ( A == B -> Cs = [0|Ys], xorBins(As,Bs,Ys)
    ; Cs = [1|Ys], xorBins(As,Bs,Ys)
    ).

%%! 20p

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%! 59 pont