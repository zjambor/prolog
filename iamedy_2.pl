%% -*- Mode: Prolog; coding: utf-8 -*- %% 2. zh 2019.12.10.

%% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
%% A célok futása ne hagyjon felesleges választási pontokat!
%% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
%%
%% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
%% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%%
%% Teljes értékű megoldásra a Prolog környezet sem küld figyelmeztetést.
%% Egy ismeretlen nevét aláhúzásjellel kezdjük <=> a klózban egyszer fordul elő.
%%
%% A Prolog céloknál, célsorozatoknál lényegtelen,
%% hogy milyen sorrendben adódnak a különböző megoldások.
%%
%% LI: A program futtatásához szükséges predikátumhívások száma.
%%
%% Feladat: ezt a fájlt kell átnevezni, majd kiegészíteni.
%% A beadandó fájl neve: x_2.pl, ahol x az Ön neptun kódja.
%%
%% A megoldást ide másoljuk: penDrive:\plzh\megoldasok\

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

:- set_prolog_flag(legacy_char_classification,on).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1. Írassunk ki egy termet, mint függvénykifejezést! (30p)
%%
%% Előf: A standard output elérhető.
%%
%% typeNop(T) : Kiíratja a T termet a standard outputra 
%%               a typeNop_test(T) predikátumnak megfelelő módon.
%% 
%% LI: O(T term résztermjeinek száma)
%%
%% Mj.: A kíratáshoz a Prolog IO-hoz kapcsolódó, a SICStus Prolog rendszerben
%%      adott predikátumok közül csak a write/2 és a writeq/2 használható fel.

%% Term T is written in standard functional notation
%% instead of using operators and/or list notation.
%% Atoms and functors in T are quoted where necessary
%% to make the result acceptable as input to 'read/1'.
%% (A read/1-nek szükséges lezáró pont és fehér szóköz nem kerül kiíratásra.)
typeNop_test(T) :- write_term(user,T,[quoted(true),ignore_ops(true)]).

/*
| ?- typeNop(-2-a(3,x,-c/y+'Alma')).
-(-2,a(3,x,+(/(-(c),y),'Alma')))
yes
| ?- typeNop(a+[b,'c d\ne','F']*'$VAR'(1)).
+(a,*('.'(b,'.'('c d\ne','.'('F',[]))),'$VAR'(1)))
yes
*/

%% Megoldás:

typeNop(T) :-
    ( compound(T) ->
      functor(T,N,A),
      writeq(user,N), write(user,'('),
      t_args(T,A,1), write(user,')')
    ; writeq(user,T)
    ).

t_args(T,I,N) :-
    arg(N,T,A), typeNop(A),
    ( N<I -> write(user,','), N1 is N+1, t_args(T,I,N1)
    ; true
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 2. Az aktuális input sor decimális számjegyek Prolog listája-e? (50p)
%%
%% Mj.: Először olvassuk be az aktuális input sort karakterkódok egy
%%      listájába, majd elemezzük DCG-vel, hogy megfelelő-e a szintaxisa,
%%      mint decimális számjegyek valódi Prolog listája!
%%      (Ha az elemzést nem DCG-vel oldja meg, max. 25 pont.)
%%
%% Előf.: Az S logikai névvel megnyitott input szövegfile olvasható.
%%
%% digit_list_line(S,Ds) :- Az S input szövegfile aktuális sorát beolvasva, a 
%%       Ds valódi Prolog listának bizonyul, aminek elemei decimális számjegyek.
%%      
%% LI: O(Ns hossza)
/*
| ?- digit_list_line(user,Ds).
|: []
Ds = []
| ?- digit_list_line(user,Ds).
|: [1]
Ds = [1]
| ?- digit_list_line(user,Ds).
|: [4,2]
Ds = [4,2]
| ?- digit_list_line(user,Ds).
|: 
Ds = [3,2,6]
| ?- digit_list_line(user,Ds).
|: [1,0,3,0,4,7]
Ds = [1,0,3,0,4,7]
| ?- digit_list_line(user,Ds).
|: [,]
no
| ?- digit_list_line(user,Ds).
|: [3,]
no
| ?- digit_list_line(user,Ds).
|: [,7]
no
| ?- digit_list_line(user,Ds).
|: [9,,7]
no
| ?- digit_list_line(user,Ds).
|: [9,5,73,8]
no
| ?- digit_list_line(user,Ds).
|: [9,5,a,8]
no
| ?- digit_list_line(user,Ds).
|: 5
no
*/

%% Megoldás:

digit_list_line(S,Ds) :- get_line_(S,Zs), digit_list(Ds,Zs,[]).

digit_list(Ds) --> [0'[], digitlist_vég(Ds).

digitlist_vég([]) --> [0']], !.
digitlist_vég([D|Ds]) --> számok(D), digitlist_sor(Ds).

digitlist_sor([]) --> [0']], !.
digitlist_sor([D|Ds]) --> [0',], számok(D), digitlist_sor(Ds).

számok(D) --> [C], {C >= 0'0, C =< 0'9, D is C-0'0}.

get_line_(S,Zs) :-
    get_code(S,A),
    ( A == 0'\n -> Zs=[]
    ; Zs=[A|Us], get_line_(S,Us)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
