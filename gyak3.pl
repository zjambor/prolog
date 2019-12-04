%% -*- Mode: Prolog; coding: utf-8 -*- %%

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

:- set_prolog_flag(legacy_char_classification,on).

%% appf('d:/plhf/y.txt','d:/plhf/gyak2.pl').
appf(F1,F2) :-
    open(F1,append,A,[encoding('UTF-8')]),
    open(F2,read,R,[encoding('UTF-8')]),
    af(A,R),
    close(A), close(R).

af(A,R) :-
    get_code(R,C),
    ( C == -1 -> true
    ; put_code(A,C), af(A,R)
    ).

%%


append_file(F1,F2) :-
    open(F1,append,A,[encoding('UTF-8')]),
    open(F2,append,B,[encoding('UTF-8')]),
    append_stream(A,B),
    close(A),close(B).

append_stream(A,B) :-
    get_code(A,X),
    ( X == -1 -> true
    ; put_code(B,X), append_stream(A,B)
    ).

másol(In,Out) :-
    open(In,read,R,[encoding('UTF-8')]),
    open(Out,write,W,[encoding('UTF-8')]),
    másol3(R,W),
    close(R),close(W).

másol2(R,W) :-
    read(R,Y),
    ( Y == end_of_file -> true
    ; writeq(W,Y), write(W,'.\n'), másol2(R,W)
    ).

másol3(R,W) :-
    repeat,
    read(R,Y),
    ( Y == end_of_file -> true
    ; writeq(W,Y), write(W,'.\n'), fail
    ) -> true.

él(a,b).    él(b,c).    él(b,d).
él(c,a).    él(c,e).    él(d,e).    él(e,e).

:- dynamic(elérhetö/2).
elérhetö(X,X).

%% assertz((elérhetö(X,Y) :- él(X,Z), elérhetö(Z,Y))).
%% listing(elérhetö/2).

retractall_(P) :- retract((P:-_)), fail ; true.
retractall_(_P).

:- dynamic(megoldás/1).

find_all(X,Cél,_Xs) :-
    retractall(megoldás(_)), % tisztázás
    Cél,
    assertz(megoldás(X)), fail.
find_all(_X,_Cél,Xs) :- begyüjt(Xs).

begyüjt(Ys) :-
    ( retract(megoldás(X)) -> Ys = [X|Xs], begyüjt(Xs)
    ; Ys = []
    ).

párosak2(Xs,Ps) :- find_all(X, ((member(X,Xs), X mod 2 =:= 0)), Ps).

:- dynamic(megold/1).

findall2(Mit,Honnan,_Hova) :-
    retractall(megold/1),
    Honnan,
    assertz(megold(Mit)), fail.
findall2(_,_,Hova) :- begyüjt2(Hova).

begyüjt2(Hova) :-
    ( retract(megold(X)) -> Hova = [X|Xs], begyüjt2(Xs)
    ; Hova = []
    ).

% 2. Melyek egy alaplista elemei?
%
% Előf: Xs alaplista.
%
% Mj.: Alapterm alatt olyan nonvar termet értünk,
%      amelynek minden résztermje is nonvar.
%      (Ezért minden alaplista egyben valódi lista is.)
%
% member1(X,Xs) :- X az Xs eleme.
%
% Mj.: Xs minden eleme pontosan egyszer adódjon megoldásként,
%      függetlenül attól, hogy hányszor szerepel a listán!
%
% Ötlet: Próbáljuk az elemek utolsó előfordulását keresni!
/*
| ?- member1(X,[]).
no
| ?- member1(X,[1,2,3]).
X = 1 ? ;
X = 2 ? ;
X = 3 ? ;
no
| ?- member1(X,[1,2,3,2,2,1]).
X = 3 ? ;
X = 2 ? ;
X = 1 ? ;
no
| ?- member1(f(2),[1,f(2),f(3),3,g(2),f(2),f(2),f,2,f(2,f(2))]).
yes
| ?- member1(f(X),[1,f(2),f(3),3,g(2),f(2),f(2),f,3,f(3,f(3))]).
X = 3 ? ;
X = 2 ? ;
no
*/

member1(X,[X|Xs]) :- nincsmárbenne(X,Xs).
member1(X,[_|Xs]) :- member1(X,Xs).

nincsmárbenne(_,[]).
nincsmárbenne(X,[Y|Xs]) :-
    ( X == Y -> fail
    ; nincsmárbenne(X,Xs)
    ).

% 3. Gyűjtsük listába egy alaplista elemeinek első előfordulásait,
%    az eredetihez képest fordított sorrendben!
%
% Előf: Xs alaplista.
%
% Mj.: Alapterm alatt olyan nonvar termet értünk,
%      amelynek minden résztermje is nonvar.
%      (Ezért minden alaplista egyben valódi lista is.)
%
% 'elsők'(Xs,Es) :- Es az Xs elemei első előfordulásainak a listája, 
%                   az Xs-belihez képest fordított sorrendben.
/*
| ?- 'elsők'([],Es).
Es = []
| ?- 'elsők'([1,2,3],Es).
Es = [3,2,1]
| ?- 'elsők'([1,2,1,2,1,3,2,1,2,1,3,2,3,1],Es).
Es = [3,2,1]
| ?- 'elsők'([1,f(2),f(3),3,g(2),f(2),f(2),f,2,f(2,f(2))],Es).
Es = [f(2,f(2)),2,f,g(2),3,f(3),f(2),1]
*/


%%'elsők'([],[]).
'elsők'(Xs,Es) :- els(Xs,Es,[]).

els([],Zs,Zs).
els([X|Xs],Es,Zs) :-
    ( member(X,Zs) -> els(Xs,Es,Zs)
    ; els(Xs,Es,[X|Zs])
    ).

% 3. Gyűjtsük listába egy alaplista elemeinek utolsó előfordulásait,
%    az eredeti sorrendben!
%
% Előf: Xs alaplista.
%
% utolsók(Xs,Us) :- Us az Xs elemei utolsó előfordulásainak
%                   a listája, az eredeti sorrendben.
/*
| ?- utolsók([],Us).
Us = []
| ?- utolsók([1,2,3],Us).
Us = [1,2,3]
| ?- utolsók([1,2,3,2,2,1],Us).
Us = [3,2,1]
| ?- utolsók([1,f(2),f(3),3,g(2),f(2),f(2),f,2,f(2,f(2))],Us).
Us = [1,f(3),3,g(2),f(2),f,2,f(2,f(2))]
*/

utolsók([],[]).
utolsók([X|Xs],Us) :-
    (nincsmárbenne(X,Xs) -> Us=[X|Zs], utolsók(Xs,Zs)
    ; utolsók(Xs,Us)
    ).

% 1. Keressük egy listán a páratlan számok párosait!
%
% Előf: Xs valódi lista.
%
% páratlan_páros(Xs,P1,P2) :- az Xs-ben a P1-et követő elem a P2,
%                             és mindkettő páratlan egész szám.
/*
| ?- páratlan_páros([],P1,P2).
no
| ?- páratlan_páros([3],P1,P2).
no
| ?- páratlan_páros([3,5],P1,P2).
P1 = 3,  P2 = 5 ? ;    no
| ?- páratlan_páros([X3,X5],P1,P2).
no
| ?- páratlan_páros([3,3*3],P1,P2).
no
| ?- páratlan_páros([1,-3,-5,4,9,7,U,V,2,3+2,5,a,-1,3,11,-7],P1,P2).
P1 = 1,  P2 = -3 ? ;    P1 = -3,  P2 = -5 ? ;
P1 = 9,  P2 = 7 ? ;     P1 = -1,  P2 = 3 ? ;
P1 = 3,  P2 = 11 ? ;    P1 = 11,  P2 = -7 ? ;
no
| ?- páratlan_páros([1,-3,-5,4,9,7,U,V,2,3+2,5,a,-1,3,11,-7],3,11).
true ? ;    no
*/

páratlan_páros([X,Y|_Xs],P1,P2) :-
    páratlan(X), páratlan(Y) -> P1=X, P2=Y.
páratlan_páros([_X,Y|Xs],P1,P2) :- páratlan_páros([Y|Xs],P1,P2).

páratlan(X) :- integer(X), X mod 2 =:= 1.


%% 2. Gyűjtsük ki egy listáról a páratlan számok párosait!
%
% Előf: Xs valódi lista.
%
% páratlan_párosok(Xs,Ps) :- a Ps valódi lista az Xs előző feladat szerinti
%    páratlan párosait tartalmazza az Xs-beli sorrend szerint, p(P1,P2) alakban.
%
% Mj.: A megoldáshoz csak az eddig tanult beépített eljárások használhatók.
/*
| ?- páratlan_párosok([],Ps).
Ps = []
| ?- páratlan_párosok([3],Ps).
Ps = []
| ?- páratlan_párosok([3,5],Ps).
Ps = [p(3,5)]
| ?- páratlan_párosok([X3,X5],Ps).
Ps = []
| ?- páratlan_párosok([3,3*3],Ps).
Ps = []
| ?- páratlan_párosok([1,-3,-5,4,9,7,U,V,2,3+2,5,a,-1,3,11,-7],Ps).
Ps = [p(1,-3),p(-3,-5),p(9,7),p(-1,3),p(3,11),p(11,-7)]
| ?- páratlan_párosok([1,-3,-5,4,9,7,U,V,2,3+2,5,a,-1,3,11,-7],[p(-3,-5)|Ps]).
no
| ?- páratlan_párosok([0,-3,-5,4,9,7,U,V,2,3+2,5,a,-1,3,11,-7,W],[p(X,-5)|Ps]).
X = -3,  Ps = [p(9,7),p(-1,3),p(3,11),p(11,-7)]
*/

páratlan_párosok([],[]).
páratlan_párosok([_],Us) :- !, Us=[].
páratlan_párosok([X,Y|Xs],Ps) :-
    ( páratlan(X), páratlan(Y) -> Ps=[p(X,Y)|Us], páratlan_párosok([Y|Xs],Us)
    ; páratlan_párosok([Y|Xs],Ps)
    ).

% 3. Határozzuk meg egy bináris fa frontját!
%
% Jelölés: üres fa: o (névkonstans); nemüres fa: t(BalRészfa,Gyökér,JobbRészfa)
%
% Előf: T valódi bináris fa. (Egyetlen részfája sem var.)
%
% binfa_front(T,Fs) :- Fs valódi lista a T frontja
%                      (a T levelei alkotják balról jobra).
/*
| ?- binfa_front(t(t(t(o,1,t(o,2,o)),3,t(o,4,o)),5,o),Fs).
Fs = [2,4]
| ?- binfa_front(t(o,a,o),Fs).
Fs = [a]
| ?- binfa_front(o,Fs).
Fs = []
| ?- binfa_front(t(t(t(t(o,A,o),B,t(o,3.14,o)),4,t(o,5,o)),a,t(o,b(C),o)),Fs).
Fs = [A,3.14,5,b(C)]
*/

%% Az input ellenőrzéséhez:

valódi_binfa(T) :-
    ( T == o -> true
    ; nonvar(T), T = t(B,_X,J), valódi_binfa(B), valódi_binfa(J)
    ).

binfa_front(T,Fs) :- binfafront_app(T,[],Fs).

binfafront_app(o,As,As).
binfafront_app(t(L,X,R),Zs,Fs) :-
    ( L == o, R == o -> Fs=[X|Zs]
    ; binfafront_app(R,Zs,Ys), binfafront_app(L,Ys,Fs)
    ).

% 1. A család13t/3 alapján adjuk meg az 'anyósa'/2 relációt!
%
% Előf: adottak  család13t(Papa,Mama,Gyerekek)  alakú tényállítások, ahol
%   Papa és Mama névkonstansok, a Gyerekek pedig névkonstansok valódi listája.
%
% anyósa(X,Y) :- X az Y anyósa a család13t/3 predikátum szerint.
%
% Például:
család13t(p,m,[a,b]).     család13t(a,n,[c,d,e]).
család13t(c,k,[f,g]).     család13t(g,e,[h]).
/*
| ?- anyósa(X,Z).
X = m, Z = n ? ;    X = n, Z = k ? ;
X = n, Z = g ? ;    X = k, Z = e ? ;
no
*/

anyósa(X,Y) :- házastársa(Z,Y), anyja(X,Z).

házastársa(Z,Y) :- család13t(Z,Y,_).
házastársa(Z,Y) :- család13t(Y,Z,_).

anyja(X,Z) :- család13t(_,X,Xs), member(Z,Xs).

% 2. Ki kiknek az anyósa? -- Oldjuk meg az alábbi megszorítás szerint!
% 
% Előf: Családok valódi lista. Elemei egy-egy családot reprezentálnak:
%    cs(Apa,Anya,Gyerekek) alakú termek, ahol Apa és Anya atomok,
%    Gyerekek pedig atomok valódi listája.
%    Minden hölgy legfeljebb egy családban anya. Testvérházasság nincs.
%
% vejei_menyei(Családok,Anyós,VejeiMenyei) :-
%     Anyós a VejeiMenyei valódi, nemüres lista elemeinek az anyósa, 
%     ahol VejeiMenyei a Családok lista sorrendje szerint rendezett.
%
% Megszorítás: A feladat megoldásábanban a beépített eljárások közül csak a
%   kontroll struktúkák, azaz a konjunkció, a diszjunkció, a lokális és a
%   közönséges vágó és a negáció megengedettek,
%   valamint a member/2 és a különböző egyenlőségek, egyenlőtlenségek,
%   de a könytárak sem használhatók.
/*
| ?- vejei_menyei([cs(a,b,[j,k]),cs(c,d,[a,f,i]),cs(e,f,[]),cs(k,m,[n])],Anyós,VMk).                                                             
VMk = [m], Anyós = b ? ;    VMk = [b,e], Anyós = d ? ;
no
*/

vejei_menyei(Cs,A,Vs) :-
    member(cs(_P,A,Gy),Cs), házastársaik(Cs,Gy,Vs),
    Vs \== [].

%% házastársaik(Családok,Gyerekek,Házastársak) :-
%%     a Gyerekek házastársai a Családok sorrendje szerint 
%%                            a Házastársak listát adják.

házastársaik([],_,[]).
házastársaik([cs(H,W,_)|Cs],Gy,Vs) :-
    ( member(H,Gy) -> Vs=[W|Hs]
    ; member(W,Gy) -> Vs=[H|Hs]
    ; Vs = Hs
    ), házastársaik(Cs,Gy,Hs).


% 2. Melyek egy valódi lista részsorozatai?
%
% Előf: Xs valódi lista.
%
% részsorozata(Xs,Rs) :- az Xs által reprezentált sorozat egy részsorozata Rs.

/*
| ?- részsorozata([a,b,c,d],Rs).
Rs = [] ? ;    Rs = [d] ? ;     Rs = [c] ? ;     Rs = [c,d] ? ;
Rs = [b] ? ;   Rs = [b,d] ? ;   Rs = [b,c] ? ;   Rs = [b,c,d] ? ;
Rs = [a] ? ;   Rs = [a,d] ? ;   Rs = [a,c] ? ;   Rs = [a,c,d] ? ;
Rs = [a,b] ? ; Rs = [a,b,d] ? ; Rs = [a,b,c] ? ; Rs = [a,b,c,d] ? ;
no
| ?- részsorozata([A,B,C],Rs).
Rs = [] ? ;     Rs = [C] ? ;      Rs = [B] ? ;      Rs = [B,C] ? ;
Rs = [A] ? ;    Rs = [A,C] ? ;    Rs = [A,B] ? ;    Rs = [A,B,C] ? ;
no
*/
%%

részsorozata([],[]).
részsorozata([X|Xs],[X|Rs]) :- részsorozata(Xs,Rs).
részsorozata([_X|Xs],Rs) :- részsorozata(Xs,Rs).

% 2. Melyek egy alaplista duplikált elemei?
%
% Mj.: Duplikált listaelem alatt a lista egy olyan elemét értjük,
%      amely később is előfordul a listában. (K db. előfordulás (K>=1)
%      esetén tehát (K-1)-szer adódik ki az elem megoldásként.)
%
% Előf: Xs alaplista.
%
% member2(X,Xs) :- X az Xs duplikált eleme.
/*
| ?- member2(X,[]).
no
| ?- member2(X,[1,2,3]).
no
| ?- member2(X,[1,2,3,2,2,1]).
X = 1 ? ;    X = 2 ? ;    X = 2 ? ;    no
*/

member2(X,[X|Ys]) :- vanmég(X,Ys).
member2(X,[_|Ys]) :- member2(X,Ys).

vanmég(X,[Y|Ys]) :-
    ( X == Y -> true
    ; vanmég(X,Ys)
    ).

%% részlistája(Xs,Rs) :-
%%     Xs részlistája, azaz Xs-beli folyamatos szakasza Rs.
/*
| ?- részlistája([a,b,c,d],Rs).
Rs = [] ? ;
Rs = [a] ? ;    Rs = [a,b] ? ;    Rs = [a,b,c] ? ;     Rs = [a,b,c,d] ? ;
Rs = [b] ? ;    Rs = [b,c] ? ;    Rs = [b,c,d] ? ;
Rs = [c] ? ;    Rs = [c,d] ? ;    Rs = [d] ? ;
no
| ?- részlistája([A,B,C],Rs).
Rs = [] ? ;     Rs = [A] ? ;      Rs = [A,B] ? ;    Rs = [A,B,C] ? ;
Rs = [B] ? ;    Rs = [B,C] ? ;    Rs = [C] ? ;
no
*/

részlistája([],[]).
részlistája([X|Xs],Rs) :- részlista_app([X|Xs],Rs) ; részlistája(Xs,Rs).

részlista_app([X|_],[X]).
részlista_app([X|Xs],[X|Rs]) :- részlista_app(Xs,Rs).

%% Def.: Azt mondjuk, hogy a Ks lista lexikografikusan kisebb, mint az Ns lista,
%% ha az első pozíciónál, ahol eltérnek, 
%% a Ks megfelelő eleme kisebb, mint az Ns megfelelő eleme,
%% vagy a Ks-nek nincs már az Ns-nek megfelelő eleme, mivel előtte véget ér.
%%
%% Mj.: Az alábbi feladatban a listaelemeket aritmetikailag hasonlítjuk össze.
%% 
%%% Előf: Ks, Ns valódi számlisták.
%%% kisebbSzámlista(Ks,Ns) :- Ks lexikografikusan kisebb, mint Ns.
%%% Mj.: O(n) hatékonyságú megoldást várok, ahol n a rövidebbik lista hossza.
/*
| ?- kisebbSzámlista([],[]).
no
| ?- kisebbSzámlista([],[1,2]).
yes
| ?- kisebbSzámlista([1],[1,2]).
yes
| ?- kisebbSzámlista([2],[1,2]).
no
| ?- kisebbSzámlista([5,3,1],[5,3,1,2]).
yes
| ?- kisebbSzámlista([5,3,1.0],[5,3,1]).
no
| ?- kisebbSzámlista([5,3,1,0],[5,3,1]).
no
| ?- kisebbSzámlista([1,0,0,0],[9,9]).
yes
| ?- kisebbSzámlista([1.0],[1,2]).
yes
| ?- kisebbSzámlista([1],[2,2]).
yes
| ?- kisebbSzámlista([2,3,1],[2,3,2]).
yes
| ?- \+kisebbSzámlista([],[]), kisebbSzámlista([],[1,2]),
     kisebbSzámlista([1],[1,2]),
     \+kisebbSzámlista([2],[1,2]), kisebbSzámlista([5,3,1],[5,3,1,2]),
     \+kisebbSzámlista([5,3,1.0],[5,3,1]), \+kisebbSzámlista([5,3,1,0],[5,3,1]),
     kisebbSzámlista([1,0,0,0],[9,9]), kisebbSzámlista([1.0],[1,2]),
     kisebbSzámlista([1],[2,2]), kisebbSzámlista([2,3,1],[2,3,2]).
yes
*/

kisebbSzámlista([],[_|_]).
kisebbSzámlista([K|Ks],[N|Ns]) :-
    ( K =:= N -> kisebbSzámlista(Ks,Ns)
    ; K < N -> true
    ; fail
    ).

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
     \+kisebbLista([1.5],[1,2]), kisebbLista([1,2],[1.5]).
     kisebbLista([2,c,z(z)],[2.0,c,a(0,0)]),
     \+ kisebbLista([2.0,c,a(0,0)],[2,c,z(z)]).
yes
*/

kisebbLista([],[_|_]).
kisebbLista([X|Xs],[Y|Ys]) :-
    ( number(X), number(Y) ->
      ( X =:= Y -> kisebbLista(Xs,Ys)
      ; X < Y -> true
      ; fail
      )
    ; ( X == Y -> kisebbLista(Xs,Ys)
      ; X @< Y -> true
      ; fail
      )
    ).

