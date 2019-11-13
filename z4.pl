%% -*- Mode: Prolog; coding: utf-8 -*- %% 

% Ez a Prolog fájl régi géptermi Prolog zh-k feladatait és megoldásait
% tartalmazza, SICStus Prolog 4-be átírva, időrendben visszafelé.
% Az első zh-k anyaga minden évben kicsit változott.
% A második (és az esetleges harmadik) zh-k anyaga az egész félév.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -*- Mode: Prolog; coding: utf-8 -*- %% 2. zh 2016.05.09.

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
%% A beadandó fájl neve: x.pl, ahol x az Ön neptun kódja, kisbetűkkel.
%%
%% Beadás: Futtatás:  \\nas2\zh\plzh    vagy  \\nas2.inf.elte.hu\zh\plzh 
%%         A megoldást ide másoljuk: megoldások\2016_0509

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

:- set_prolog_flag(legacy_char_classification,on).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1. Olvassunk be egy számot a standard inputról, interaktív módon! (20p)
%%
%% Előf: A standard input elérhető.
%%
%% számot_olvas(X) :
%%   Ciklusban
%%     beolvassuk egy karakterkód listába a standard input aktuális sorát.
%%     Megpróbáljuk Prolog számmá konvertálni a number_codes/2 segítségével.
%%     Ha ez sikerül, KÉSZ vagyunk; az eredmény X-be kerül, és bef. a ciklust.
%%     Ha nem sikerül, kiírjuk, hogy 'Számot kérek: ',
%%     majd visszalépéssel újraindítjuk a (repeat-fail) ciklust.
%% 
%% LI: O(a standard input sorok karaktereinek száma)
%%
%% Mj.: Hasznosítsuk a sikertelen konverziónál fellépő kivételt!
/*
| ?- számot_olvas(X).
|: 31.4e-1
X = 3.14
| ?- számot_olvas(X).
|: -59
X = -59
| ?- számot_olvas(X).
|: +3,14
Számot kérek: +3.14
Számot kérek: -3.14f0
Számot kérek: -3.14E0
X = -3.14
| ?- számot_olvas(X).
|: oxFF
Számot kérek: OxFF
Számot kérek: 0xFF
X = 255
| ?- számot_olvas(X).
|: 018
X = 18
| ?- számot_olvas(X).
|: 0b12
Számot kérek: 0b101
X = 5
| ?- számot_olvas(X).
|: 0o18
Számot kérek: +0o17
Számot kérek: -0o17
X = -15
*/

%% Megoldás:

get_line(S,Cs) :-
    get_code(S,C),
    ( C==0'\n -> Cs = []
    ; Cs = [C|Ds], get_line(S,Ds)
    ).

%%% számot_olvas(X,Err) :- %% Kiderítjük, milyen exception-t kell elkapni.
%%%     catch( ( get_line(user,Cs), number_codes(X,Cs) ), Err, true ).

számot_olvas(X) :-
    repeat,
        catch( ( get_line(user,Cs), number_codes(X,Cs) ),
	       error(syntax_error('number syntax'),_), % hibás számra ez lép fel
	       ( write(user, 'Számot kérek: '), flush_output(user), fail )
	     ),
    !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 2. Írjuk meg az '=..'/2 beépített eljárás saját változatát! (30p)
%%
%% nonvar2list(Term,Xs) :
%%     ha nonvar(Term) vagy Xs egyelemű számlista vagy
%%         Xs valódi lista, aminek első eleme atom,
%%     akkor a Term =..Xs hívásnak megfelelően működik,
%%     különben meghiúsul.
%%
%% Mj.: Felhasználhatjuk a functor/3 és az arg/3 beépített eljárásokat,
%%      de az '=..'/2 eljárást természetesen nem.
%% 
%% LI: nonvar(Term) esetén O(Term aritása), kül. O(|Xs|)
/*
| ?- nonvar2list(a(X,y),Xs).
Xs = [a,X,y]
| ?- Xs = [b,1,t(x),3.14,-8], nonvar2list(T,Xs), nonvar2list(T,Ys), Xs==Ys.
T = b(1,t(x),3.14,-8)
| ?- Xs = [a], nonvar2list(T,Xs), nonvar2list(T,Ys), Xs==Ys.
T = a
| ?- Xs = [-1], nonvar2list(T,Xs), nonvar2list(T,Ys), Xs==Ys.
T = -1
| ?- Xs = [2.71], nonvar2list(T,Xs), nonvar2list(T,Ys), Xs==Ys. 
T = 2.71
| ?- Xs = [1,2], nonvar2list(T,Xs).
no
| ?- nonvar2list(T,Xs).
no
| ?- nonvar2list(T,[a(X)]).
no
| ?- nonvar2list(T,[X]).
no
| ?- Xs = [a,b|_], nonvar2list(T,Xs).
no
| ?- nonvar2list(T,[1|_]).
no
*/
%% Megoldás:

%% Teszt mo.:

nonvar2list(Term,Xs) :-
    ( nonvar(Term) -> Term =.. Xs
    ; proper_list(Xs), Xs=[A|Ys] ->
        ( Ys==[], atomic(A) -> Term =.. Xs
        ; atom(A)  -> Term =.. Xs
	)
    ).

proper_list(Xs) :-
    nonvar(Xs),
    ( Xs == [] -> true
    ; Xs = [_|Ys], proper_list(Ys)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 3. Készítsük el a  szélességi_keresés/3 predikátumot, ami megfelel az alábbi
%% breadth_first_search/3 eljárásnak, kivéve, hogy a Visited lista helyett AVL
%% fát használunk! (20p)

%% Mj.: Használhatjuk a megfelelő library(avl) könyvtári modult.
%% A megoldást a feladat végére írjuk! Az eredeti kódot hagyjuk érintetlenül!
%% A változatlanul felhasználható eljárásokat nem kell újra megírni.

%% szélességi_keresés(Start,Goal,SolPath) :-
%%     breadth_first_search(Start,Goal,SolPath).

%%  Breadth-first-search: 

/*  Data   */

% Acyclic component
    edge(a,b).   edge(a,c).   edge(a,d).  edge(a,e).  edge(d,j).  
    edge(c,f).   edge(c,g).   edge(f,h).  edge(e,k).  edge(f,i).  
    edge(j,g).   edge(g,h).   edge(k,j).  edge(b,f).  edge(b,i).

/*
     a------>b-------->i
    /|\       \      /
   | | \       \    /                        __________
   | |  \       \  /                        /          \
   | V   V       V/                        V            \
   | d   c------>f---->h                   x----->y----->z
   |  \   \          /                            |      |
   |   \   \        /                             |      |
   |    \   \      /                              V      V
   |     \   \    /                               u      t
   |      \   \  /
   V       V   V/
   e-->k-->j-->g
*/

% Cyclic component
    %% edge(t,x).
    edge(x,y).   edge(y,z).   edge(z,x).  edge(y,u).  edge(z,t).  

%% Graph-search with breadth-first-search strategy.

%% breadth_first_search(Start,Goal,SolPath) :-
%%     SolPath is a proper list representing a path of
%%     the minimal length (i.e.optimal) from Start to Goal.
breadth_first_search(Start,Goal,SolPath) :-
    ground(Start), ground(Goal),
    ( Start == Goal -> SolPath = [Start]
    ; empty(InitQueue), add(InitQueue,[Start],Queue), 
      bfs(Queue,[Start],Goal,SolPath)
    ).

:- use_module(library(lists),[reverse/2]).

%% Queue is the queue of lists of the form [Node|Ancestors] 
%% where Node is a node to which we have found the optimal
%% path but has not been expanded. Ancestors consists of
%% the ancestors of Node on this optimal path starting
%% with its parent. Visited contains the visited nodes.
bfs(Queue,Visited,Goal,SolPath) :- 
    rem(Queue,[Node|Ancestors],RemainderQueue), 
    children(Node,Children,Visited),
    ( has(Children,Goal) ->
          reverse([Goal,Node|Ancestors],SolPath)
    ; process_children(Children,[Node|Ancestors],
		   RemainderQueue,ResultQueue), 
      append(Children,Visited,NewVisited),
      bfs(ResultQueue,NewVisited,Goal,SolPath) 
    ).

%% expansion: Children is the list of
%%     the nonvisited children of Node.
children(Node,Children,Visited) :- 
    findall(Child,child(Node,Child,Visited),Children).

%% Child is a nonvisited child of Node.
child(Node,Child,Visited) :- 
    edge(Node,Child), \+ has(Visited,Child).

has([X|Xs],Y) :-
    ( X == Y -> true
    ; has(Xs,Y)
    ).

%% Add the children with their ancestors to RemainderQueue. 
process_children([FirstChild|Children],Ancestors,
		 InputQueue,ResultQueue) :- 
    add(InputQueue,[FirstChild|Ancestors],TempQueue), 
    process_children(Children,Ancestors,TempQueue,ResultQueue).
process_children([],_Ancestors,ResultQueue,ResultQueue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Queue handling:

empty(q([],[])).

add(q(Xs,Ys),E,q(Xs,[E|Ys])).

rem(q(Xs,Ys),E,q(Us,Vs)) :-
    ( Xs==[] -> reverse(Ys,[E|Us]), Vs=[]
    ; Xs=[E|Us], Ys=Vs
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Teszt (nem teljeskörű):
/*
| ?- breadth_first_search(a,a,Sol1), szélességi_keresés(a,a,Sol2), Sol1==Sol2.
Sol1 = [a],    Sol2 = [a]
| ?- breadth_first_search(a,b,Sol1), szélességi_keresés(a,b,Sol2), Sol1==Sol2.
Sol1 = [a,b],    Sol2 = [a,b]
| ?- breadth_first_search(a,g,Sol1), szélességi_keresés(a,g,Sol2), Sol1==Sol2.
Sol1 = [a,c,g],    Sol2 = [a,c,g]
| ?- breadth_first_search(a,h,Sol1), szélességi_keresés(a,h,Sol2), Sol1==Sol2.
Sol1 = [a,b,f,h],    Sol2 = [a,b,f,h]
| ?- breadth_first_search(z,u,Sol1), szélességi_keresés(z,u,Sol2), Sol1==Sol2.
Sol1 = [z,x,y,u],    Sol2 = [z,x,y,u]
| ?- breadth_first_search(x,u,Sol1), szélességi_keresés(x,u,Sol2), Sol1==Sol2.
Sol1 = [x,y,u],    Sol2 = [x,y,u]
| ?- breadth_first_search(x,x,Sol1), szélességi_keresés(x,x,Sol2), Sol1==Sol2.
Sol1 = [x],    Sol2 = [x]
| ?- szélességi_keresés(u,x,Sol).
no
| ?- szélességi_keresés(a,x,Sol).
no
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Megoldás:

%% szélességi_keresés(Start,Goal,SolPath) :-
%%     SolPath is a proper list representing a path of
%%     the minimal length (i.e.optimal) from Start to Goal.
szélességi_keresés(Start,Goal,SolPath) :-
    ground(Start), ground(Goal),
    ( Start == Goal -> SolPath = [Start]
    ; empty(InitQueue), add(InitQueue,[Start],Queue),
      empty_avl(T0), avl_store(Start,T0,0,Visited),
      bfs_avl(Queue,Visited,Goal,SolPath)
    ).

:- use_module(library(avl),[empty_avl/1,avl_fetch/2,avl_store/4]).

%% 'empty_avl(?AVL)' is true when AVL is an empty AVL tree.

%% 'avl_fetch(+KEY, +AVL)'
%%      is true when the (given) KEY is one of the keys in the (given) AVL.
%%      Use this to test whether a known Key occurs in AVL and you don't
%%      want to know the value associated with it.

%% 'avl_store(+KEY, +OLDAVL, +VAL, +NEWAVL)'
%%      is true when OLDAVL and NEWAVL define the same finite function
%%      except that NEWAVL associates VAL with KEY.  OLDAVL need not have
%%      associated any value at all with KEY.  When it didn't, you can read
%%      this as "insert (KEY->VAL) into OLDAVL giving NEWAVL".


%% Queue is the queue of lists of the form [Node|Ancestors] 
%% where Node is a node to which we have found the optimal
%% path but has not been expanded. Ancestors consists of
%% the ancestors of Node on this optimal path starting
%% with its parent. Visited contains the visited nodes.
bfs_avl(Queue,Visited,Goal,SolPath) :- 
    rem(Queue,[Node|Ancestors],RemainderQueue), 
    children_avl(Node,Children,Visited),
    ( has(Children,Goal) ->
          reverse([Goal,Node|Ancestors],SolPath)
    ; process_children(Children,[Node|Ancestors],
		   RemainderQueue,ResultQueue), 
      append_avl(Children,Visited,NewVisited),
      bfs_avl(ResultQueue,NewVisited,Goal,SolPath) 
    ).

%% expansion: Children is the list of
%%     the nonvisited children of Node.
children_avl(Node,Children,Visited) :- 
    findall(Child,child_avl(Node,Child,Visited),Children).

%% Child is a nonvisited child of Node.
child_avl(Node,Child,Visited) :- 
    edge(Node,Child), \+ avl_fetch(Child,Visited).

append_avl([Child|Children],Visited,NewVisited) :-
    avl_store(Child,Visited,0,TempVisited),
    append_avl(Children,TempVisited,NewVisited).
append_avl([],Visited,Visited).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -*- Mode: Prolog; coding: utf-8 -*- %% 1. zh 2016.03.21.

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
%% LI: A program futtatásához szükséges predikátumhívások száma.
%%
%% Feladat: ezt a fájlt kell átnevezni, majd kiegészíteni.
%% A beadandó fájl neve: x.pl, ahol x az Ön neptun kódja, kisbetűkkel.
%%
%% Beadás: Futtatás: \\nas2.inf.elte.hu\zh\plzh 
%%         A megoldást ide másoljuk: megoldások\2016_0321

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1: Honnét hová tudunk legfeljebb egy átszállással eljutni? (14p)
%%
%% Előf: Egy menetrendből a Hs valódi Prolog listába összegyűjtöttük,
%%       hogy honnét hová tudunk közvetlenül eljutni.
%%       A listaelemek h(Honnét,Hová) alakú termek,
%%       ahol Honnét és Hová névkonstansok.
%%
%% h01h(Hs,X,Y,Z) :- Hs szerint az X helyről a Z helyre legfeljebb egy 
%%    átszállással, Y-on keresztül eljuthatunk, de Y==0, ha nincs átszállás.
/*
Ha nem jól jelenik meg az alábbi gráf, váltsa át
a betűtípust (a "Default Font"-ot) "Courier New"-ra!

     a------>b-------->i
    /|\       \      /
   | | \       \    /
   | |  \       \  /
   | V   V       V/
   | d   c------>f---->h
   |  \   \          /
   |   \   \        /
   |    \   \      /
   |     \   \    /
   |      \   \  /
   V       V   V/
   e-->k-->j-->g

| ?- _Hs = [h(a,b),h(a,c),h(a,d),h(a,e),h(b,f),h(b,i),h(c,f),h(c,g),h(d,j),h(e,k),h(f,h),h(f,i),h(g,h),h(j,g),h(k,j)], h01h(_Hs,a,Y,Z).
Y = 0, Z = b ? ;  Y = 0, Z = c ? ;  Y = 0, Z = d ? ;  Y = 0, Z = e ? ;
Y = b, Z = f ? ;  Y = b, Z = i ? ;  Y = c, Z = f ? ;  Y = c, Z = g ? ;
Y = d, Z = j ? ;  Y = e, Z = k ? ;
no
| ?- _Hs = [h(a,b),h(a,c),h(a,d),h(a,e),h(b,f),h(b,i),h(c,f),h(c,g),h(d,j),h(e,k),h(f,h),h(f,i),h(g,h),h(j,g),h(k,j)], h01h(_Hs,X,Y,g).
X = c, Y = 0 ? ;  X = j, Y = 0 ? ;
X = a, Y = c ? ;  X = d, Y = j ? ;  X = k, Y = j ? ;
no
| ?- _Hs = [h(a,b),h(a,c),h(a,d),h(a,e),h(b,f),h(b,i),h(c,f),h(c,g),h(d,j),h(e,k),h(f,h),h(f,i),h(g,h),h(j,g),h(k,j)], h01h(_Hs,X,0,Y).
X = a, Y = b ? ;  X = a, Y = c ? ;  X = a, Y = d ? ;  X = a, Y = e ? ;
X = b, Y = f ? ;  X = b, Y = i ? ;  X = c, Y = f ? ;  X = c, Y = g ? ;
X = d, Y = j ? ;  X = e, Y = k ? ;  X = f, Y = h ? ;  X = f, Y = i ? ;
X = g, Y = h ? ;  X = j, Y = g ? ;  X = k, Y = j ? ;
no
*/
%% Megoldás:

h01h(Hs,X,0,Z) :- member(h(X,Z),Hs).
h01h(Hs,X,Y,Z) :- Y\==0, member(h(X,Y),Hs), member(h(Y,Z),Hs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 2: Éllista alapján adjuk meg egy gráfban egy csúcs rákövetkezőit! (24p)

%% Előf: Hs egy irányított gráf éleinek lexikografikusan szig.növ. rendezett
%%       valódi Prolog listája. Az élek h(Honnét,Hová) alakú termek, 
%%       ahol Honnét és Hová névkonstansok. C névkonstans a gráf egy csúcsa.
%%
%% hsas(Hs,C,As) :- As a Hs-nek megfelelő gráf C csúcsának szomszédossági 
%%                  listája, azaz a C csúcs közvetlen rákövetkezőinek
%%                  valódi Prolog listája, szig.mon.növ. rendezve.
%% LI: O(|Hs|)
%% Mj.: A Hs listát lehetőleg csak egyszer, és csak addig dolgozzuk fel,
%%      ameddig még van esély, hogy a C csúcsból kimenő élet találunk!
/*
     a------>b-------->i
    /|\       \      /
   | | \       \    /
   | |  \       \  /
   | V   V       V/
   | d   c------>f---->h         m
   |  \   \          /
   |   \   \        /
   |    \   \      /
   |     \   \    /
   |      \   \  /
   V       V   V/
   e-->k-->j-->g

| ?- _Hs = [h(a,b),h(a,c),h(a,d),h(a,e),h(b,f),h(b,i),h(c,f),h(c,g),h(d,j),h(e,k),h(f,h),h(f,i),h(g,h),h(j,g),h(k,j)], hsas(_Hs,a,As).
As = [b,c,d,e]
| ?- _Hs = [h(a,b),h(a,c),h(a,d),h(a,e),h(b,f),h(b,i),h(c,f),h(c,g),h(d,j),h(e,k),h(f,h),h(f,i),h(g,h),h(j,g),h(k,j)], hsas(_Hs,c,As).
As = [f,g]
| ?- _Hs = [h(a,b),h(a,c),h(a,d),h(a,e),h(b,f),h(b,i),h(c,f),h(c,g),h(d,j),h(e,k),h(f,h),h(f,i),h(g,h),h(j,g),h(k,j)], hsas(_Hs,d,As).
As = [j]
| ?- _Hs = [h(a,b),h(a,c),h(a,d),h(a,e),h(b,f),h(b,i),h(c,f),h(c,g),h(d,j),h(e,k),h(f,h),h(f,i),h(g,h),h(j,g),h(k,j)], hsas(_Hs,i,As).
As = []
| ?- _Hs = [h(a,b),h(a,c),h(a,d),h(a,e),h(b,f),h(b,i),h(c,f),h(c,g),h(d,j),h(e,k),h(f,h),h(f,i),h(g,h),h(j,g),h(k,j)], hsas(_Hs,m,As).
As = []
*/
%% Megoldás:

hsas([],_C,[]).
hsas([h(X,Y)|Hs],C,As) :-
    ( X @< C -> hsas(Hs,C,As)
    ; X @> C -> As = []
    ; As = [Y|Bs], hsas(Hs,C,Bs)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 3. A hhh/3 úthalmaz meg a h/3 élhalmaz tranzitív, irreflexív lezártja. (22p)

h(a,7,b).  h(a,4,c).  h(a,2,d).  h(a,11,e).
h(b,4,f).  h(b,9,i).  h(c,8,f).  h(c,6,g). 
h(d,7,j).  h(e,1,k).  h(f,5,h).  h(f,6,i).
h(g,8,h).  h(j,2,g).  h(k,3,j). 

%% Előf: a h/3 predikátum irányított, körmentes gráfot reprezentál:
%%   "h(Honnét,Élhossz,Hová)." alakú tényekkel reprezentált élek alkotják.
%%   ahol Honnét és hova névkonstansok, Élhossz egész szám.
%%
%% Mj.: Tetszőleges szeparált csúcsot hozzáképzelhetünk a gráfhoz.
%%
%% hhh(A,H,Z) :- az A csúcsból a tőle különböző Z csúcsba
%%               H hosszú úton el lehet jutni.
%%
/*       7        9
     a------>b-------->i
    /|\       \      /
   | | \4      \4   /6
   | |2 \       \  /
   | V   V   8   V/  5
   | d   c------>f---->h         m
 11|  \   \          /
   |   \   \        /
   |   7\   \6     /8
   |     \   \    /
   |      \   \  /
   V       V   V/
   e-->k-->j-->g
     1   3   2
     
| ?- hhh(a,H,g).
H = 10 ? ;   H = 11 ? ;   H = 17 ? ;
no
| ?- hhh(c,H,Z).
H = 8, Z = f ? ;    H = 6, Z = g ? ;
H = 13, Z = h ? ;   H = 14, Z = i ? ;   H = 14, Z = h ? ;
no
| ?- hhh(A,H,i).
A = b, H = 9 ? ;   A = f, H = 6 ? ;   A = a, H = 16 ? ;   A = a, H = 17 ? ;
A = a, H = 18 ? ;  A = b, H = 10 ? ;  A = c, H = 14 ? ;
no
| ?- hhh(A,17,Z).
A = a, Z = i ? ;    A = a, Z = h ? ;    A = a, Z = g ? ;
A = d, Z = h ? ;
no
| ?- hhh(A,H,m).
no
| ?- hhh(m,H,Z).
no
| ?- hhh(a,H,a).
no
*/

%% Megoldás:

hhh(A,H,Z) :- hhh_(A,0,H,Z).

hhh_(A,H0,H,Z) :-
    h(A,H1,Z), H is H0+H1.
hhh_(A,H0,H,Z) :-
    h(A,H1,B), H2 is H0+H1, hhh_(B,H2,H,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -*- Mode: Prolog; coding: utf-8 -*- %% 2. zh 2015.05.18.

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
%% LI: A program futtatásához szükséges predikátumhívások száma.
%%
%% Feladat: ezt a fájlt kell átnevezni, majd kiegészíteni.
%% A beadandó fájl neve: x.pl, ahol x az Ön neptun kódja, kisbetűkkel.
%%
%% Beadás: Futtatás: \\nas2.inf.elte.hu\zh\plzh 
%%         A megoldást ide másoljuk: megoldások\2015_0518

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Mj.: A valódi karakterkód listákat sztringként jelölhetjük, és úgy is
%%%     nevezzük; pl.: "1 a" == [0'1,0' ,0'a], " " == [].'

%% 1. Írassuk ki a standard outputra egy sztringnek
%%    egy másik sztringben nem szereplő karaktereit! ([10+5+5]p)
%%
%% Mj.: Adjunk a feladatra három, egymástól elvileg különböző megoldást,
%% put_string_diff_1/2, put_string_diff_2/2, put_string_diff_3/2, néven!
%% (1: listaművelet tiszta Prolog + vágó stílusban [10p],
%%  2: célokkal paraméterezhető (magasabbrendű) predikátum használata [5p],
%%  3: a negáció kifinomult használatával karakterenként íratjuk ki [5p].)
%%
%% Előf: S1, S2 Prolog sztringek.
%%       Feltehető, hogy vezérlő karaktereket (pl. újsor) nem tartalmaznak.
%%
%% put_string_diff_I(S1,S2) : Írassuk ki a standard outputra sorfolytonosan
%%              az S1 sztringnek az S2 sztringben nem szereplő karaktereit,
%%              az S1-beli sorrendjüknek megfelelően!  (I eleme {1,2,3}).
%%
%% LI: O(|S1|*|S2|)
/*
| ?- put_string_diff_1("almafa","szilva"),
     put_string_diff_2("almafa","szilva"),
     put_string_diff_3("almafa","szilva").
mf
mf
mf
yes
| ?- put_string_diff_1("szilvafán","szőlő"),
     put_string_diff_2("szilvafán","szőlő"),
     put_string_diff_3("szilvafán","szőlő").
ivafán
ivafán
ivafán
yes
| ?- put_string_diff_1("alma","maláta"),
     put_string_diff_2("alma","maláta"),
     put_string_diff_3("alma","maláta").
                                            %% 1. output (üres sor)
                                            %% 2. output (üres sor)
                                            %% 3. output (üres sor)
yes
| ?- put_string_diff_1("alma","móló"),
     put_string_diff_2("alma","móló"),
     put_string_diff_3("alma","móló").
aa
aa
aa
yes 
| ?- put_string_diff_1("lovam","alma"),
     put_string_diff_2("lovam","alma"),
     put_string_diff_3("lovam","alma").
ov
ov
ov
yes
*/

%% Megoldások:

put_string_diff_1(S1,S2) :-
    string_diff(S1,S2,Cs),
    atom_codes(A,Cs), write(user,A), nl(user).

string_diff([],_Bs,[]).
string_diff([A|As],Bs,Cs) :-
    ( member(A,Bs) -> string_diff(As,Bs,Cs)
    ; Cs = [A|Ds], string_diff(As,Bs,Ds)
    ).

put_string_diff_2(S1,S2) :-
    findall(C, ( member(C,S1), \+member(C,S2) ), Cs),
    atom_codes(A,Cs), write(user,A), nl(user).

put_string_diff_3(S1,S2) :-
    \+ ( member(C,S1), \+member(C,S2) , \+put_code(user,C) ), nl(user).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 2. Készítsünk tagolt bináris kód (tbk) felismerő DCG-t! (10p)
%%
%% Mj.: A feladatot tiszta elemző DCG-vel oldjuk meg, anélkül,
%%      hogy a nyelvtanban explicit Prolog hívást alkalmaznánk!
%%
%% Def.: Egy sztring tbk, akkor ha
%%       - ábécéjét a "0_1" sztring karakterei adják,
%%       - nemüres,
%%       - első és utolsó eleme nem az aláhúzásjel kódja,
%%       - sehol sincs benne két aláhúzásjel kód egymás után.
%%
%% Előf: Bs valódi lista.
%%
%% tbk(Bs,Ms) :- a Bs-Ms d-lista egy tbk sztring reprezentációja.
%%
%% LI: O(|Bs|), az összes megoldásra együtt.
%%
%% | ?- tbk("_","").
%% no
%% | ?- tbk("_0","").
%% no
%% | ?- tbk("1_","").
%% no
%% | ?- tbk("1_0","").
%% yes
%% | ?- tbk("0_1","").
%% yes
%% | ?- tbk("01","").
%% yes
%% | ?- tbk("1010","").
%% yes
%% | ?- tbk("1__010","").
%% no
%% | ?- tbk("1_010","").
%% yes
%% | ?- tbk("10_0_1_0101011_0","").
%% yes
%% | ?- tbk("10_0_1_0101011_0_","").
%% no
%% | ?- tbk("_10_0_1_0101011_0","").
%% no
%% | ?- tbk("10_0_1_0101011_0","").
%% no
%% | ?- tbk("1_0","_0").
%% yes
%% | ?- tbk("1_0",Ms).
%% Ms = [] ? ;
%% Ms = [95,48] ? ;
%% no

%% Megoldás:

tbk --> bit, tbk_vég.

bit --> "0".
bit --> "1".

tbk_vég --> "_", tbk.
tbk_vég --> tbk.
tbk_vég --> "".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 3. Módosítsuk az előző megoldást úgy, hogy a tbk sztring,
%%    mint egész szám értékét is meghatározzuk!
%%    Tegyük teljesen determinisztikussá a kódot! (10p)
%%
%% Mj.: A program minden mondata maradjon DCG szabály,
%%      de most már explicit Prolog hívásokra is szükség lesz.
%%
%% Előf: Bs valódi lista.
%%
%% tbk1(Szám,Bs,Ms) :- Bs tbk sztring, Ms=[],
%%                    Szám pedig a Bs bináris szám integer értéke. 
%%
%%
%% LI: O(|Bs|)
%%
%% | ?- tbk1(Érték,"1","").
%% Érték = 1
%% | ?- tbk1(Érték,"0","").
%% Érték = 0
%% | ?- tbk1(Érték,"101","").
%% Érték = 5
%% | ?- tbk1(Érték,"010","").
%% Érték = 2
%% | ?- tbk1(Érték,"_","").
%% no
%% | ?- tbk1(Érték,"","").
%% no
%% | ?- tbk1(Érték,"10_01","").
%% Érték = 9
%% | ?- tbk1(Érték,"10_0_1_0101011_0","").
%% Érték = 2390
%% | ?- tbk1(Érték,"_10_0_1_0101011_0","").
%% no
%% | ?- tbk1(Érték,"10_0_1_0101011_0_","").
%% no
%% | ?- tbk1(Érték,"10_0_1__0101011_0","").
%% no

%% | ?- tbk1(Érték,"1_0",Ms).
%% Érték = 2,    Ms = []
%% | ?- tbk1(Érték,"11",Ms).
%% Érték = 3,    Ms = []

%% | ?- tbk1(Érték,"11","1").
%% no
%% | ?- tbk1(Érték,"1_2","_2").
%% no
%% | ?- tbk1(Érték,"12",Ms).
%% no

%% Megoldás:

tbk1(Érték) --> bit(Bit), !, tbk1_vég(Bit,Érték).

tbk1_vég(E,Érték) -->
    "_", !, tbk2(E,Érték).
tbk1_vég(E,Érték) -->
    bit(Bit), !,
    {E1 is 2*E+Bit}, tbk1_vég(E1,Érték).
tbk1_vég(_E,_Érték) -->
    [_C], !, {fail}.    % Hibakezelés.
tbk1_vég(E,E) --> [].

tbk2(E,Érték) -->
    bit(Bit), !,
    {E1 is 2*E+Bit}, tbk1_vég(E1,Érték).

bit(0) --> "0".
bit(1) --> "1".

/*
| ?- tbk1(E1,"1",""), tbk1(_E0,"0",""), tbk1(_E5,"101",""), tbk1(_E2,"010",""),
     \+tbk1(_,"_",""), \+tbk1(_,"",""), tbk1(_E9,"10_01",""), 
     tbk1(_E2390,"10_0_1_0101011_0",""), \+tbk1(_,"_10_0_1_0101011_0",""),
     \+tbk1(_,"10_0_1_0101011_0_",""), \+tbk1(_,"10_0_1__0101011_0",""),
     tbk1(_E_2,"1_0",_Ms_2), tbk1(_E3,"11",Ms3), \+tbk1(_,"11","1"),
     \+tbk1(_,"1_2","_2"), \+tbk1(_,"12",_),
     E1 == 1, _E0 == 0, _E5 == 5, _E2 == 2, _E9 == 9, _E2390 == 2390,
     _E_2 == 2, _Ms_2 == [], _E3 == 3, Ms3 == [].
E1 = 1,
Ms3 = [] ? ;
no
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% -*- Mode: Prolog; coding: utf-8 -*- %% 1. zh 2015.03.17.

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
%% LI: A program futtatásához szükséges predikátumhívások száma.
%%
%% Feladat: ezt a fájlt kell átnevezni, majd kiegészíteni.
%% A beadandó fájl neve: x.pl, ahol x az Ön neptun kódja, kisbetűkkel.
%%
%% Beadás: Futtatás: \\nas2.inf.elte.hu\zh\plzh 
%%         A megoldást ide másoljuk: megoldások\2015_0323

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1: Melyek egy lista páros sorszámú elemei?
%%
%% Mj.: A feladatot tiszta Prologban, a beépített és a könyvtári eljárások
%%      felhasználása nélkül oldjuk meg!
%%
%% Előf: Xs valódi lista.
%%
%% ssz2(Xs,Y) :- Y az Xs páros sorszámú eleme.
%%
%% LI in O(|Xs|), az összes megoldásra.
%%
%% | ?- ssz2([a,b,c,d,e],Y).
%% Y = b ? ;    Y = d ? ;    no
%% | ?- ssz2([A,B,C,D,E,F],Y).
%% Y = B ? ;    Y = D ? ;    Y = F ? ;    no
%% | ?- ssz2([A,B,C,D,E,F,G],a).
%% B = a ? ;    D = a ? ;    F = a ? ;    no
%% | ?- ssz2([],Y).
%% no
%% | ?- ssz2([a],Y).
%% no
%% | ?- ssz2([a,b,c,d,e,f],d).
%% yes

%% Megoldás:

ssz2([_X,Y|_Zs],Y).
ssz2([_X,_Y|Zs],Z) :- ssz2(Zs,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 2: Gyűjtsük össze egy lista páros sorszámú elemeit!
%%
%% Előf: Xs valódi lista.
%%
%% ssz2s(Xs,Ys) :-
%%     Ys az Xs páros sorszámú elemeinek listája, az Xs-beli sorrendben.
%%
%% Mj.: A feladatot a beépített és a könyvtári eljárások felhasználása 
%%      nélkül oldjuk meg! Kivételek: !/0, ;/2, (->)/2.
%%
%% LI in O(|Xs|)
%%
%% | ?- ssz2s([a,b,c,d,e],Ys).
%% Ys = [b,d]
%% | ?- ssz2s([A,B,C,D,E,F],Ys).
%% Ys = [B,D,F]
%% | ?- ssz2s([A,B,C,D,E,F,G],[b,d,f]).
%% B = b,    D = d,    F = f
%% | ?- ssz2s([],Ys).
%% Ys = []
%% | ?- ssz2s([a],Ys).
%% Ys = []
%% | ?- ssz2s([a,b],Ys).
%% Ys = [b]
%% | ?- ssz2s([A,B,A],Ps).
%% Ps = [B]
%% | ?- ssz2s([A,B,C,B],Ps).
%% Ps = [B,B]
%% | ?- ssz2s([A,B,C,D],[B,D]).
%% yes

%% Megoldás:

ssz2s([],[]).
ssz2s([_X],Us) :- !, Us = [].
ssz2s([_X,Y|Zs],[Y|Us]) :- ssz2s(Zs,Us).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 3. bináris számjegylista inkrementálása
%%
%% Repr: a nemnegatív bináris számokat most 0,1-ek valódi listájaként
%%   ábrázoljuk, a legalacsonyabb helyiértékkel kezdve.
%%   Pl. 110 reprezentációja [0,1,1] (vezető nullák megengedettek).
%%
%% Előf: Bs 0,1-ek valódi, nemüres listája.
%%
%% incbin(Bs,Cs) :- a Cs bináris számjegylista a Bs által reprezentált 
%%                  bináris számnál eggyel nagyobb számot ábrázolja.
%%
%% Mj.: A feladatot a beépített és a könyvtári eljárások felhasználása 
%%      nélkül oldjuk meg! Kivételek: !/0, ;/2, (->)/2.
%%
%% LI in O(|Xs|)
%%
%% | ?- incbin([1,0,1],Cs).
%% Cs = [0,1,1]
%% | ?- incbin([1,1,1],Cs).
%% Cs = [0,0,0,1]
%% | ?- incbin([0],Cs).
%% Cs = [1]
%% | ?- incbin([1],Cs).
%% Cs = [0,1]
%% | ?- incbin([1,0],Cs).
%% Cs = [0,1]
%% | ?- incbin([1,0,0],Cs).
%% Cs = [0,1,0]
%% | ?- incbin([0,0,0],Cs).
%% Cs = [1,0,0]

%% Megoldás:

%% Lokális vágóval:

incbin([],[1]).
incbin([B|Bs],Cs) :-
    ( B == 0 -> Cs = [1|Bs]       % A  B=0  feltétel is megteszi, 
    ; Cs = [0|Ds], incbin(Bs,Ds)  % de így egy picit hatékonyabb.
    ).

%% Közönséges vágóval:

incbin_([],[1]).
incbin_([0|Bs],Es) :- !, Es = [1|Bs].
incbin_([1|Bs],[0|Cs]) :- incbin_(Bs,Cs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                        %% 2. zh 2014.05.12.

% Az előfeltétel implikálja a kérdések keresési fájának végességét!
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű.
%
% Teljes értékű megoldásra a Prolog környezet sem küld figyelmeztetést.
% Egy ismeretlen nevét aláhúzásjellel kezdjük <=> a klózban egyszer fordul elő.
%
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<
%   f ~=< g jelentése, hogy f eleme O(g).
%
% Ezt a fájlt kell átnevezni és kiegészíteni. A beadandó fájl neve:
% x.pl, ahol x a neptun kód, kisbetűkkel.
%
% Beadás: futtat: \\inf.elte.hu\dfs\zh\plzh ;
%         másol ebbe:  megoldások\2014_0512

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Adjuk meg egy tetszőleges számhoz a megfelelő határozott névelőt! (15p)
%
% Előf: N egész szám.
% Mj.: Ha N nem egész szám, hiúsuljon meg a predikátumhívás!
%
% az_a(N,AzA) :- az N egész számhoz tartozó megfelelő
%     magyar nyelvű határozott névelő ('Az ' ill. 'A ') AzA.
%
% Segítség: Az alábbi C++ kód megoldja a problémát.
%
%%% // Visszaadja a megfelelő határozott névelőt. 
%%% // Szerző: Ásványi Tibor, 2008. október 15.
%%% string nevelo(int n){
%%%   if( n<=0 ) return "A ";
%%%   while( n >= 1000 ) n /= 1000;
%%%   if( n==1)  return "Az ";
%%%   while( n >= 10 ) n /= 10;
%%%   if( n==5)  return "Az ";
%%%   return "A ";
%%% }
%
% LI ~=< ha(N>=10) lg(N) különben 1.
%
% Ötlet: A Prolog kódban a ciklusokat
%        egy rekurzív segéd-predikátummal helyettesítjük.
/*
| ?- az_a(1,Egy), az_a(10,Tíz), az_a(100,Száz), az_a(1024,Ezer_),
     az_a(12145,Tízezer_), az_a(157246,Százezer_), az_a(1257368,Egymillió_).
Egy = 'Az ',    Tíz = 'A ',    Száz = 'A ',    Ezer_ = 'Az ',
Tízezer_ = 'A ',    Százezer_ = 'A ',     Egymillió_ = 'Az '
| ?- az_a(5,Öt), az_a(53,Ötven_), az_a(506,Ötszáz_), az_a(5123,Ötezer_).
Öt = 'Az ',     Ötven_ = 'Az ',    Ötszáz_ = 'Az ',    Ötezer_ = 'Az '
| ?- az_a(7,Hét), az_a(4,Négy), az_a(8,Nyolc), az_a(16,Tizen_), az_a(32,Harminc_), az_a(64,Hatvan_), az_a(128,Száz_), az_a(256,Kétszáz_), az_a(512,Ötszáz_), az_a(1024,Ezer_), az_a(2048,Kétezer_). 
Hét = 'A ',    Négy = 'A ',    Ezer_ = 'Az ',    Nyolc = 'A ',
Száz_ = 'A ',    Kétezer_ = 'A ',    Kétszáz_ = 'A ',    Tizen_ = 'A ',
Harminc_ = 'A ',    Ötszáz_ = 'Az ',    Hatvan_ = 'A '
*/
% Megoldás:

az_a(N,AzA) :-
    integer(N),
    ( N =< 0 -> AzA = 'A '
    ; divs(N,1000,N1),
      ( N1 == 1 -> AzA = 'Az '
      ; divs(N1,10,N2),
	( N2 == 5 -> AzA = 'Az '
	; AzA = 'A '
	)
      )
    ).

divs(N,K,M) :-
    ( N >= K -> N1 is N//K, divs(N1,K,M)
    ; M = N
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Írjunk tesztkörnyezetet az előző programhoz! (15p)
%
% Előf: A beírt egész számok az ábrázolható tartományba esnek.
%
% az_a_teszt :-
%    "Ciklusban" bekér a standard inputról egy-egy egész számot.
%    Ha nem egész számot kap, elköszön a program.
%    Ha egész számot kap, kiírja a megfelelő szöveget:
%    "Az U számot olvastam be." vagy
%    "A V számot olvastam be.", ahol U ill. V a beolvasott szám.
%
/*
| ?-  az_a_teszt.
Kérem az egész számokat,
amik elé határozott névelőt kell tennem!
(Ha nem egész számot kapok, elköszönök.)
|: 1
Az 1 számot olvastam be.
|: 10
A 10 számot olvastam be.
|: 100
A 100 számot olvastam be.
|: 1024
Az 1024 számot olvastam be.
|: 2
A 2 számot olvastam be.
|: 2014
A 2014 számot olvastam be.
|: 42567432
A 42567432 számot olvastam be.
|: 5273510
Az 5273510 számot olvastam be.
|: 19352
A 19352 számot olvastam be.
|: 163032
A 163032 számot olvastam be.
|: 1947263
Az 1947263 számot olvastam be.
|: 16243102
A 16243102 számot olvastam be.
|: 163542310
A 163542310 számot olvastam be.
|: 1321035475
Az 1321035475 számot olvastam be.
|: Folytassam?
Viszlát!
yes
% source_info
| ?- az_a_teszt.
Kérem az egész számokat,
amik elé határozott névelőt kell tennem!
(Ha nem egész számot kapok, elköszönök.)
|: 3.14159265358979323846
Viszlát!
yes
*/
% Megoldás:

az_a_teszt :-
    write(user,'Kérem az egész számokat,\n'),
    write(user,'amik elé határozott névelőt kell tennem!\n'),
    write(user,'(Ha nem egész számot kapok, elköszönök.)\n'),
    repeat,
        sorbeolv(Kódok),
        ( catch((number_codes(N,Kódok),integer(N)),_, fail) ->
	      az_a_kiír(N), fail
        ; write(user,'Viszlát!\n')
        )
    -> true.

sorbeolv(Kódok) :-
    get_code(user,C),
    ( C==0'\n -> Kódok=[]
    ; Kódok=[C|Cs], sorbeolv(Cs)
    ).

az_a_kiír(X) :-
    az_a(X,AzA),
    write(user,AzA), write(user,X), write(user,' számot olvastam be.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Definiáljuk elemző DCG-vel a divide/5 predikátumot a qs/4 programban! (15p)
%
% Előf: Xs valódi lista.
%
% divide(Ls,Gs,X,Xs,[]) :- az Xs X-nél nagyobb elemeit Gs,
%     a többit Ls tartalmazza, a standard rendezés szerint.
%
% Mj.: Ezt a feladatot olyan elemző DCG nyelvtannal KELL megoldani, ami
%     nem tartalmaz felhasználói/ könyvtári predikátumra vonatkozó Prolog hívást.
%     Tartalmazhat viszont beépített eljárásokra vonatkozó Prolog hívásokat.
%
qs(Xs,Ys) :- quicksort(Xs,Ys,[]).

quicksort([X|Xs]) -->
    {divide(Ls,Gs,X,Xs,[])},
    quicksort(Ls), [X], quicksort(Gs).
quicksort([]) --> [].

/*
| ?- qs([alma,12,34,9.8,f(x),B],Rs).
Rs = [B,9.8,12,34,alma,f(x)]
| ?- qs([],Rs).
Rs = []
| ?- qs([1],Rs).
Rs = [1]
| ?- qs([1,2],Rs).
Rs = [1,2]
| ?- qs([M,1,2,M,4.1,g(Y),[B]],Rs).
Rs = [M,M,4.1,1,2,g(Y),[B]]
*/
% 1. Megoldás:

divide(Ls,Gs,Y) --> [X],{X@=<Y, !, Ls=[X|L1s]}, divide(L1s,Gs,Y).
divide(Ls,Gs,Y) --> [X],{X@>Y, !, Gs=[X|G1s]}, divide(Ls,G1s,Y).
divide([],[],_Y) --> [].

% 2. Megoldáshoz:

qs2(Xs,Ys) :- quicksort2(Xs,Ys,[]).

quicksort2([X|Xs]) -->
    {divide2(Ls,Gs,X,Xs,[])},
    quicksort2(Ls), [X], quicksort2(Gs).
quicksort2([]) --> [].

% 2. Megoldás:

divide2(Ls,Gs,Y) --> [X], !,
    ( {X@=<Y} -> {Ls=[X|L1s]}, divide2(L1s,Gs,Y)
    ; {Gs=[X|G1s]}, divide2(Ls,G1s,Y)
    ).
divide2([],[],_Y) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                            %% 1. zh 2014.03.17.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% Teljes értékű megoldásra a Prolog környezet sem küld figyelmeztetést.
% Egy ismeretlen nevét aláhúzásjellel kezdjük <=> a klózban egyszer fordul elő.
%
% Feladat: ezt a fájlt kell átnevezni, majd kiegészíteni.
% A beadandó fájl neve: x.pl, ahol x az Ön neptun kódja, kisbetűkkel.
%
% Beadás: futtat: \\inf.elte.hu\dfs\zh\plzh ;
%         másol ebbe:  megoldások\2014_0317

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Megoldás (10p):

páratlan_páros([X1,X2|_Xs],P1,P2) :-
    páratlan(X1), páratlan(X2), P1=X1, P2=X2.
páratlan_páros([_|Xs],P1,P2) :-
    páratlan_páros(Xs,P1,P2).

páratlan(X) :- integer(X), X mod 2 =:= 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Gyűjtsük ki egy listáról a páratlan számok párosait!
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

% Megoldás (10p):

páratlan_párosok([],[]).
páratlan_párosok([_],Ps) :- !, Ps = [].
páratlan_párosok([X1,X2|Xs],Ps) :-
    ( páratlan(X1), páratlan(X2) ->
        Ps = [p(X1,X2)|Qs], páratlan_párosok([X2|Xs],Qs)
    ; páratlan_párosok([X2|Xs],Ps)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Megoldás (10p):

%% 1. mo
binfa_front(T,Fs) :- binfafront_app(T,[],Fs).

binfafront_app(o,Fs,Fs).
binfafront_app(t(B,X,J),As,Fs) :-
    ( B == o , J == o -> Fs = [X|As]
    ; binfafront_app(J,As,Js), binfafront_app(B,Js,Fs)
    ).


%% 2. mo
binfa_front2(T,Fs) :- binfafront_2(T,[],Fs).

%% binfafront_(T,As,Fs) :-
%%     a T frontja és As összefűzésével adódik az Fs valódi lista.

binfafront_2(o,Fs,Fs).
binfafront_2(t(o,X,o),As,Fs) :- !, Fs = [X|As].
binfafront_2(t(B,_X,J),As,Fs) :- binfafront_2(J,As,Js), binfafront_2(B,Js,Fs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                           %% 2. zh 2013.05.16.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<
%   f ~=< g jelentése, hogy f eleme O(g).
%
% Ezt a fájlt kell átnevezni és kiegészíteni. A beadandó fájl neve:
% x.pl, ahol x a neptun kód, kisbetűkkel.
%
% Beadás: futtat: \\inf.elte.hu\dfs\zh\plzh ;
%         másol ebbe:  megoldások\2013_0516

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Számoljuk ki egy bináris fa egyesúlyait! (10p)
%
% Előf: T valódi bináris fa, ahol a kis 'o' atom az üres fa,
%   a nemüres fák pedig t(BalRészFa,Gyökér,JobbRészFa) alakúak.
%
% egyensúlyok(T,M,TS) :- T magassága M,
%     T másolata a TS, annyi különbséggel, hogy TS-ben a
%     nemüres részfák t(BalRészFa,Gyökér,Egyensúly,JobbRészFa) alakúak, ahol
%     Egyensúly a jobboldali és a baloldali részfa magasságának különbsége.
%
% LI ~=< a T részfáinak száma.
/*
| ?- egyensúlyok(t(t(o,2,o),3,o),M,TS).
M = 1,
TS = t(t(o,2,0,o),3,-1,o)
| ?- egyensúlyok(t(o,1,t(t(o,2,o),3,o)),M,TS).
M = 2,
TS = t(o,1,2,t(t(o,2,0,o),3,-1,o))
| ?- egyensúlyok(t(t(t(o,1,t(t(o,2,o),3,o)),4,t(o,5,o)),6,o),M,TS).
M = 4,
TS = t(t(t(o,1,2,t(t(o,2,0,o),3,-1,o)),4,-2,t(o,5,0,o)),6,-4,o)
| ?- egyensúlyok(t(o,6,t(t(o,5,o),4,t(o,1,t(t(o,2,o),3,o)))),M,TS).
M = 4,
TS = t(o,6,4,t(t(o,5,0,o),4,2,t(o,1,2,t(t(o,2,0,o),3,-1,o))))
*/
% Megoldás:

egyensúlyok(o,-1,o).
egyensúlyok(t(B,G,J),M,t(BE,G,E,JE)) :-
    egyensúlyok(B,BM,BE), egyensúlyok(J,JM,JE),
    M is 1+max(BM,JM), E is JM-BM.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Fordítsunk meg egy valódi listát DCG-vel! (10p)
%
% Előf: Xs valódi lista.
%
% reverse(Xs) --> Xs megfordítása.
%
% Mj.: Ezt a feladatot tiszta generatív DCG nyelvtannal KELL megoldani,
%      ami tehát nem tartalmaz Prolog hívást.
%
% LI ~=< a T bináris fa mérete.
/*
| ?- reverse([],Rs,[]).
Rs = []
| ?- reverse([A],Rs,[]).
Rs = [A]
| ?- reverse([A,B,C,D],Rs,[]).
Rs = [D,C,B,A]
| ?- reverse([],Rs,Vs).
Vs = Rs
| ?- reverse([A],Rs,Vs).
Rs = [A|Vs]
% source_info
| ?- reverse([A,B],Rs,Vs).
Rs = [B,A|Vs]
| ?- reverse([A,B,C],Rs,Vs).
Rs = [C,B,A|Vs]
| ?- reverse([B,A,L,A,H],Rs,[B,A]).
Rs = [H,A,L,A,B,B,A]
*/
% Megoldás:

reverse([]) --> [].
reverse([X|Xs]) --> reverse(Xs), [X].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Melyek egy lista részsorozatai? (20p)
%
% Előf: Ys valódi lista.
%
% részsorozat(Xs,Ys,[]) :- Xs az Ys nem okvetlenül folytonos részsorozata.
%
% Mj.: Ezt a feladatot tiszta elemző DCG nyelvtannal KELL megoldani.
%      ami tehát nem tartalmaz Prolog hívást.
%      Ha Ys elemei különböző termek, ne adjon duplikált megoldást! (-6p)
%
/*
| ?- részsorozat(Xs,[],[]).
Xs = [] ? ;
no
| ?- részsorozat(Xs,[A],[]).
Xs = [] ? ;    Xs = [A] ? ;
no
| ?- részsorozat(Xs,[A,B],[]).
Xs = [] ? ;    Xs = [B] ? ;    Xs = [A] ? ;    Xs = [A,B] ? ;
no
| ?- részsorozat(Xs,[A,B,C],[]).
Xs = [] ? ;     Xs = [C] ? ;      Xs = [B] ? ;      Xs = [B,C] ? ;
Xs = [A] ? ;    Xs = [A,C] ? ;    Xs = [A,B] ? ;    Xs = [A,B,C] ? ;
no
*/
% Megoldás:

részsorozat([]) --> list.
részsorozat(Xs) --> [_X], részsor1(Xs).
részsorozat([X|Xs]) --> [X], részsorozat(Xs).

részsor1(Xs) --> [_X], részsor1(Xs).
részsor1([X|Xs]) --> [X], részsorozat(Xs).

list --> [].
list --> [_X], list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                          %% 1. zh 2013.03.21.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% Feladat: ezt a fájlt kell átnevezni, majd kiegészíteni.
% A beadandó fájl neve: x.pl, ahol x az Ön neptun kódja, kisbetűkkel.
%
% Beadás: futtat: \\inf.elte.hu\dfs\zh\plzh ;
%         másol ebbe:  megoldások\2013_0321

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
%% Megoldás (10p):

anyósa(X,Z) :- anyja13(X,Y), házastársa(Y,Z).

anyja13(X,Y) :- család13t(_,X,Gs), member(Y,Gs).

házastársa(X,Y) :- család13t(X,Y,_).
házastársa(X,Y) :- család13t(Y,X,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
| ?- vejei_menyei([cs(a,b,[j,k]),cs(c,d,[a,f,i]),cs(e,f,[]),cs(k,m,[n])],
                                                               Anyós,VMk).
VMk = [m], Anyós = b ? ;    VMk = [b,e], Anyós = d ? ;
no
*/

%% Megoldás (20p):

vejei_menyei(Családok,Anyós,VejeiMenyei) :-
    member(cs(_,Anyós,Gyerekek),Családok), %% Anyós itt még csak anyósjelölt.
    házastársaik(Családok,Gyerekek,VejeiMenyei),
    VejeiMenyei\==[].                      %% Itt már igazi anyós.

%% házastársaik(Családok,Gyerekek,Házastársak) :-
%%     a Gyerekek házastársai a Családok sorrendje szerint 
%%                            a Házastársak listát adják.

házastársaik([],_Gyerekek,[]).
házastársaik([cs(Férj,Feleség,_)|Családok],Gyerekek,Házastársak) :-
    ( member(Férj,Gyerekek) -> Házastársak = [Feleség|Hk]
    ; member(Feleség,Gyerekek) -> Házastársak = [Férj|Hk]
    ; Házastársak = Hk
    ),
    házastársaik(Családok,Gyerekek,Hk).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                         %% 2. zh 2012.05.17.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<
%
% Ezt a fájlt kell átnevezni és kiegészíteni. A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
%
% Beadás: futtat: \\inf.elte.hu\dfs\zh\plzh ;
%         másol ebbe:  megoldások\2012_0517

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Melyik lista reprezentálja egy term egy ágát? (20p)
%
% Előf: A (T,Xs) páros nem tartalmaz duplikált ismeretlent.
%
% term_ág(T,Xs) :-
%     Ha a híváskor T összetett term, az Xs valódi lista első eleme
%     a T függvényszimbólum-neve, maradéka pedig a T
%     valamelyik paraméteréhez tartozó term-ág.
%     Ha a híváskor T egyszerű term, az Xs valódi lista egyetlen eleme T.
%
% LI ~=< a T résztermjeinek száma (az összes megoldásra együtt).
/*
| ?- term_ág(t(a(X),2,b(c(3,d),4.0)),Xs).
Xs = [t,b,4.0] ? ;    Xs = [t,b,c,d] ? ;    Xs = [t,b,c,3] ? ;
Xs = [t,2] ? ;        Xs = [t,a,X] ? ;
no
| ?- term_ág(t(a(X),2,b(c(3,d),4.0)),[Y,a,p]).
X = p,    Y = t
| ?- term_ág(X,Xs).
Xs = [X]
| ?- term_ág(t(a(X),2,b(c(3,d),4.0)),[Y,a,p(Z)]).
X = p(Z),    Y = t
*/
% Megoldás:

term_ág(T,Xs) :-
    ( compound(T) ->
          functor(T,F,N), Xs = [F|Fs],
          term_gyerek(N,T,G), term_ág(G,Fs)
    ; Xs = [T]
    ).

term_gyerek(N,T,G) :-
    ( arg(N,T,G)
    ; N > 1, N1 is N-1, term_gyerek(N1,T,G)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Adjuk meg egy bináris fa inorder bejárását generatív DCG-vel! (10p)
%
% Előf: T valódi bináris fa, ahol a kis 'o' atom az üres fa,
%   a nemüres fák pedig t(BalRészFa,Gyökér,JobbRészFa) alakúak.
%
% inorderDCG(T) --> T inorder bejárása.
%
% Mj.: Ezt a feladatot generatív DCG nyelvtannal KELL megoldani.
%
% LI ~=< a T bináris fa mérete.
/*
| ?- inorderDCG(o,Xs,[]).
Xs = []
| ?- inorderDCG(o,Xs,Ys).
Ys = Xs
| ?- inorderDCG(t(o,2,o),Xs,[]).
Xs = [2]
| ?- inorderDCG(t(t(o,2,o),3,o),Xs,[]).
Xs = [2,3]
| ?- inorderDCG(t(t(o,2,o),3,o),Xs,Ys).
Xs = [2,3|Ys]
| ?- inorderDCG(t(o,1,t(t(o,2,o),3,o)),Xs,[]).
Xs = [1,2,3]
| ?- inorderDCG(t(t(t(o,1,t(t(o,2,o),3,o)),4,t(o,5,o)),6,o),Xs,[]).
Xs = [1,2,3,4,5,6]
| ?- inorderDCG(t(t(t(o,1,t(t(o,2,o),3,o)),4,t(o,5,o)),6,o),Xs,Ys).
Xs = [1,2,3,4,5,6|Ys]
*/
% Megoldás:

inorderDCG(o) -->
    [].
inorderDCG(t(Bal,Gy,Jobb)) -->
    inorderDCG(Bal), [Gy], inorderDCG(Jobb).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Mely bináris fák inorder bejárása lehet egy adott lista? (10p)
%
% Jelölés: az üres fát a kis 'o' atom reprezentálja,
%   a nemüres fák pedig t(BalRészFa,Gyökér,JobbRészFa) alakúak.
%
% Előf: N >=-1 egész szám, Is valódi lista.
%
% inorder_DCG(N,T,Is,[]) :- A legfeljebb N mélységű T bináris fa
%                           inorder bejárása az Is valódi lista.
%
% Mj.: Ezt a feladatot elemző DCG nyelvtannal ajánlott megoldani.
%
/*
| ?- inorder_DCG(-1,T,[],[]).
T = o ? ;
no
| ?- inorder_DCG(0,T,[],[]).
T = o ? ;
no
| ?- inorder_DCG(-1,T,[A],[]).
no
| ?- inorder_DCG(0,T,[A],[]).
T = t(o,A,o) ? ;
no
| ?- inorder_DCG(0,T,[A,B],[]).
no
| ?- inorder_DCG(1,T,[A,B],[]).
T = t(t(o,A,o),B,o) ? ;
T = t(o,A,t(o,B,o)) ? ;
no
| ?- inorder_DCG(1,T,[A,B,C],[]).
T = t(t(o,A,o),B,t(o,C,o)) ? ;
no
| ?- inorder_DCG(1,T,[A,B,C,D],[]).
no
| ?- inorder_DCG(2,T,[A,B,C],[]).
T = t(t(t(o,A,o),B,o),C,o) ? ;
T = t(t(o,A,t(o,B,o)),C,o) ? ;
T = t(t(o,A,o),B,t(o,C,o)) ? ;
T = t(o,A,t(t(o,B,o),C,o)) ? ;
T = t(o,A,t(o,B,t(o,C,o))) ? ;
no
| ?- inorder_DCG(2,T,[A,B,C,D,E,F],[]).
T = t(t(t(o,A,o),B,t(o,C,o)),D,t(t(o,E,o),F,o)) ? ;
T = t(t(t(o,A,o),B,t(o,C,o)),D,t(o,E,t(o,F,o))) ? ;
T = t(t(t(o,A,o),B,o),C,t(t(o,D,o),E,t(o,F,o))) ? ;
T = t(t(o,A,t(o,B,o)),C,t(t(o,D,o),E,t(o,F,o))) ? ;
no
| ?- inorder_DCG(2,T,[A,B,C,D,E,F,G],[]).
T = t(t(t(o,A,o),B,t(o,C,o)),D,t(t(o,E,o),F,t(o,G,o))) ? ;
no
| ?- inorder_DCG(2,T,[A,B,C,D,E,F,G,H],[]).
no
| ?- inorder_DCG(1,T,[A,B,C],Ys). %! Ld. a feladat alábbi általánosítását!
T = t(t(o,A,o),B,t(o,C,o)),    Ys = [] ? ;
T = t(t(o,A,o),B,o),           Ys = [C] ? ;
T = t(o,A,t(o,B,o)),           Ys = [C] ? ;
T = t(o,A,o),                  Ys = [B,C] ? ;
T = o,                         Ys = [A,B,C] ? ;
no
*/
% Megoldás:

% A megoldáshoz a feladatot így általánosítjuk:
% inorder_DCG(N,T,Is,Js) :- A legfeljebb N mélységű T bináris fa
%                           inorder bejárása az Is-Js d-lista.


inorder_DCG(N,t(Bal,Gy,Jobb)) -->
    { N >= 0, N1 is N-1 },
    inorder_DCG(N1,Bal), [Gy], inorder_DCG(N1,Jobb).
inorder_DCG(_N,o)  -->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                       %% 1. zh 2012.04.12.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% Ezt a fájlt kell átnevezni és kiegészíteni. A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
%
% Beadás: futtat: \\inf.elte.hu\dfs\zh\plzh ;
%         másol ebbe:  megoldások\2012_0412

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. A család12t/3 alapján adjuk meg az 'őse'/2 relációt!
%
% Előf: adottak  család12t(Anya,Apa,Gyerekek)  alakú tényállítások, ahol
%   Anya és Apa névkonstansok, a Gyerekek pedig névkonstansok valódi listája.
%
% 'őse'(X,Y) :- X az Y őse a család12t/3 predikátum szerint.
%
% Például:
család12t(p,m,[a,b]).     család12t(a,n,[c,d,e]).
család12t(c,k,[f,g]).     család12t(g,e,[h]).
/*
| ?- 'őse'(m,Utód).
Utód = a ? ;    Utód = b ? ;    Utód = c ? ;    Utód = d ? ;    Utód = e ? ;
Utód = f ? ;    Utód = g ? ;    Utód = h ? ;    Utód = h ? ;
no
| ?- 'őse'(A,h).
A = g ? ;    A = e ? ;    A = p ? ;    A = p ? ;    A = a ? ;    A = a ? ;
A = c ? ;    A = m ? ;    A = m ? ;    A = n ? ;    A = n ? ;    A = k ? ;
no
| ?- 'őse'(b,Utód).
no
| ?- 'őse'(e,Utód).
Utód = h ? ;
no
*/

% Megoldás:

'őse'(X,Z) :- szje(X,Z).
'őse'(X,Z) :- szje(X,Y), 'őse'(Y,Z).

szje(X,Y) :- család12t(X,_,Gs), member(Y,Gs).
szje(X,Y)  :- család12t(_,X,Gs), member(Y,Gs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Melyek egy valódi lista részsorozatai?
%
% Előf: Xs valódi lista.
%
% részsorozata(Xs,Rs) :- az Xs által reprezentált sorozat egy részsorozata Rs.
%
/*
| ?- részsorozata([a,b],Rs).
Rs = [a,b] ? ;    Rs = [a] ? ;    Rs = [b] ? ;    Rs = [] ? ;
no
| ?- részsorozata([1,2,3],Rs).
Rs = [1,2,3] ? ;    Rs = [1,2] ? ;    Rs = [1,3] ? ;    Rs = [1] ? ;
Rs = [2,3] ? ;      Rs = [2] ? ;      Rs = [3] ? ;      Rs = [] ? ;
no
| ?- Rs = [_X], részsorozata([a,b,c,d],Rs).
Rs = [a] ? ;    Rs = [b] ? ;    Rs = [c] ? ;    Rs = [d] ? ;
no
| ?- Rs = [_X,_Y], részsorozata([A,B,C,D],Rs).
Rs = [A,B] ? ;    Rs = [A,C] ? ;    Rs = [A,D] ? ;
Rs = [B,C] ? ;    Rs = [B,D] ? ;    Rs = [C,D] ? ;
no
| ?- Rs = [_X,_Y,_Z|_Us], részsorozata([a,b,c,d],Rs).
Rs = [a,b,c,d] ? ;    Rs = [a,b,c] ? ;    Rs = [a,b,d] ? ;
Rs = [a,c,d] ? ;    Rs = [b,c,d] ? ;
no
| ?- Rs = [_X,_Y,c|_Us], részsorozata([a,b,c,d],Rs).
Rs = [a,b,c,d] ? ;    Rs = [a,b,c] ? ;
no
*/

% Megoldás:

részsorozata([],[]).
részsorozata([X|Xs],[X|Rs]) :- részsorozata(Xs,Rs).
részsorozata([_X|Xs],Rs) :- részsorozata(Xs,Rs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Megoldás:

'elsők'(Xs,Es) :- 'elsők'(Xs,[],Es).

%% 'elsők'(Xs,As,Es) :- az Xs alaplista As alaplistán nem szereplő elemeinek
%%       első előfordulásait fordított sorrendben az As elé fűzve adódik Es.

'elsők'([],As,As).
'elsők'([X|Xs],As,Es) :-
    ( member(X,As) -> 'elsők'(Xs,As,Es)
    ; 'elsők'(Xs,[X|As],Es)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                          %% 2. zh 2011.05.16.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<
%
% Ezt a fájlt kell átnevezni és kiegészíteni. A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
%
% Beadás: futtat: \\inf.elte.hu\dfs\zh ;
%         másol ebbe:  plzh\2011_0516

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Írjuk le a lista hatványozás fogalmát! (40p)
%
% Előf: A "Ts valódi lista", "Xs valódi lista", "N nemnegatív egész"
%       állítások közül legalább kettő igaz.
%
% hatvány(N,Xs,Ts) :-
%     az Xs valódi lista (N-1)-szer önmaga után fűzve adja a Ts valódi listát.
%     ( N=0 esetén Ts üres lista, N=1 esetén Ts=Xs,
%       N=2 esetén append(Xs,Xs,Ts), ... )
%
% LI ~=< ( N * Xs hossza ) + Ts hossza.
/*
| ?- hatvány(0,[A,B,C],Ts).
Ts = []
| ?- hatvány(1,[A,B,C],Ts).
Ts = [A,B,C]
| ?- hatvány(2,[A,B,C],Ts).
Ts = [A,B,C,A,B,C]
| ?- hatvány(3,[A,B,C],Ts).
Ts = [A,B,C,A,B,C,A,B,C]
| ?- hatvány(4,[],Ts).
Ts = []
| ?- hatvány(5,[X],Ts).
Ts = [X,X,X,X,X]
| ?- hatvány(5,Xs,[A,B,A,B,A,B,A,B,A,B]).
Xs = [A,B]
| ?- hatvány(4,Xs,[A,B,A,B,A,B,A,B,A,B]).
no
| ?- hatvány(3,Xs,[A,B,A,B,A,B,A,B,A,B]).
no
| ?- hatvány(3,Xs,[A,B,A,B,A,B,A,B,A]).
B = A,    Xs = [A,A,A]
| ?- A=a, B=b, hatvány(3,Xs,[A,B,A,B,A,B,A,B,A]).
no
| ?- hatvány(2,Xs,[A,B,A,B,A,B,A,B,A,B]).
B = A,    Xs = [A,A,A,A,A]
| ?- hatvány(1,Xs,[A,B,A,B,A,B,A,B,A,B]).
Xs = [A,B,A,B,A,B,A,B,A,B]
| ?- hatvány(0,Xs,[A,B,A,B,A,B,A,B,A,B]).
no
| ?- hatvány(0,Xs,[]).
true
| ?- hatvány(0,Xs,[A]).
no
| ?- hatvány(N,[],[A,B,A,B,A,B,A,B]).
no
| ?- hatvány(N,[],[]).
true
| ?- hatvány(N,[A],[B]).
B = A,    N = 1
| ?- hatvány(N,[X],[A,B,A,B,A,B,A,B]).
B = A,    N = 8,    X = A
| ?- hatvány(N,[X,Y],[A,B,A,B,A,B,A,B]).
N = 4,    X = A,    Y = B
| ?- hatvány(N,[X,Y,Z],[A,B,A,B,A,B,A,B]).
no
| ?- hatvány(N,[V,X,Y,Z],[A,B,A,B,A,B,A,B]).
N = 2,    V = A,    X = B,    Y = A,    Z = B
| ?- hatvány(N,[T,U,V,X,Y,Z],[A,B,A,B,A,B,A,B]).
no
| ?- hatvány(N,[X,Y,Z],[A,B,A]).
N = 1,    X = A,    Y = B,    Z = A
| ?- hatvány(N,[X,Y,Z],[A,B]).
no
| ?- hatvány(N,[X,Y,Z],[]).
N = 0
| ?- hatvány(N,[a,Y,Z],[b,B,A]).
no
*/
% Megoldás:

hatvány(N,Xs,Ts) :-
    ( N == 0 -> Ts = []
    ; integer(N) ->
        N>0, 'N_edik_hatvány'(N,Xs,Ts)
    ; Xs == [] ->
        Ts = []
    ; Ts == [] ->
        N = 0
    ; valódi_lista(Xs), valódi_lista(Ts),
      length(Xs,NX), length(Ts,NT),
      NT mod NX =:= 0, N is NT // NX,
      'magához_fűz'(N,Xs,Ts)
    ).
%% A length(Xs,N) beépített eljárás: az Xs valódi lista hossza N.

valódi_lista(Xs) :-
    ( Xs == [] -> true
    ; nonvar(Xs), Xs = [_Y|Ys], valódi_lista(Ys)
    ).

'N_edik_hatvány'(N,Xs,Ts) :-
    ( valódi_lista(Xs) -> true
    ; valódi_lista(Ts), length(Ts,NT),
      NT mod N =:= 0, NX is NT // N,
      length(Xs,NX)
    ),
    'magához_fűz'(N,Xs,Ts).

%% Az Xs listát (N-1)-szer önmaga elé fűzve kapjuk a Ts listát.
'magához_fűz'(N,Xs,Ts) :- 'eléfűz'(N,Xs,Xs,Ts).

%% Az Xs listát (N-1)-szer az As elé fűzve kapjuk a Ts listát.
'eléfűz'(N,Xs,As,Ts) :-
    ( N > 1 -> append(Xs,As,Bs), N1 is N-1, 'eléfűz'(N1,Xs,Bs,Ts)
    ; Ts = As
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


                           %% 1. zh 2011.03.28.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% Ezt a fájlt kell átnevezni és kiegészíteni. A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
%
% Beadás: futtat: \\inf.elte.hu\dfs\zh ;
%         másol ebbe:  plzh\2011_0328

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. A kiscsalád/3 alapján adjuk meg a nagyapja/2 relációt!
%
% Előf: adottak  kiscsalád(Anya,Apa,Gyerekek)  alakú tényállítások, ahol
%   Anya és Apa névkonstansok, a Gyerekek pedig névkonstansok valódi listája.
%
% nagyapja(X,Y) :- X az Y nagypapája a kiscsalád/3 predikátum szerint.
%
% Például:
kiscsalád(p,m,[a,b]).     kiscsalád(a,n,[c,d,e]).
kiscsalád(p,k,[f,g]).     kiscsalád(g,e,[h]).
/*
| ?- nagyapja(X,Y).
X = m,    Y = c ? ;
X = m,    Y = d ? ;
X = m,    Y = e ? ;
X = n,    Y = h ? ;
X = k,    Y = h ? ;
no
*/

% Megoldás:

nagyapja(X,Y) :- apja(X,Z), a_ja(Z,Y).

a_ja(X,Y) :- anyja(X,Y).
a_ja(X,Y) :- apja(X,Y).

anyja(X,Y) :- kiscsalád(X,_,Gs), member(Y,Gs).
apja(X,Y)  :- kiscsalád(_,X,Gs), member(Y,Gs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Megoldás:

member1(X,[X|Xs]) :- has_not(Xs,X).
member1(X,[_X|Xs]) :- member1(X,Xs).

has_not([],_X).
has_not([Y|Ys],X) :-
    ( Y == X -> fail
    ; has_not(Ys,X)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Megoldás:

utolsók([],[]).
utolsók([X|Xs],Us) :-
    ( has_not(Xs,X) -> Us = [X|Vs], utolsók(Xs,Vs)
    ; utolsók(Xs,Us)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                           %% 2. zh 2010.12.13.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<
%
% Ezt a fájlt kell átnevezni és kiegészíteni. A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Írjuk le DCG-vel a dadogó listák fogalmát! (15p)
%
% Előf: Ts alaplista vagy Xs alaplista
%
% dadogó(Xs,Ts,Rs) :- dadog(Xs,Ts,Rs). % ahol:
%
dadog(Xs,Ts,Rs) :-
    ( ground(Xs) -> append(Xs,Xs,XsXs), append(XsXs,Rs,Ts)
    ; append(XsXs,Rs,Ts), append(Xs,Xs,XsXs)
    ).
%
% Mj.: dadogó/3-at tiszta DCG-vel kell megírni, azaz nem tartalmazhat
%      expliciten sem Prolog predikátumot, sem Prolog hívást.
%      (Generatív és elemző irányban is ugyanaz a kód működik majd.)
/*
| ?- dadogó(Xs,[a,b,a,b,a,b,a],Rs).
Rs = [a,b,a,b,a,b,a],    Xs = [] ? ;
Rs = [a,b,a],    Xs = [a,b] ? ;
no
| ?- dadogó(Xs,[a,b,a,b,a,b,a,b],Rs).
Rs = [a,b,a,b,a,b,a,b],    Xs = [] ? ;
Rs = [a,b,a,b],    Xs = [a,b] ? ;
Rs = [],    Xs = [a,b,a,b] ? ;
no
| ?- dadogó([a,b,c],Rs,Ts).
Rs = [a,b,c,a,b,c|Ts]
| ?- dadogó([],Rs,Ts).
Ts = Rs
*/
% Megoldás:

dadogó(Xs) --> fele(Xs), fele(Xs).

fele([]) --> [].
fele([X|Xs]) --> [X], fele(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Írjuk meg az unio_sort programját generatív DCG-vel!
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

% Megoldás:

%%% member2(X,[X|Xs]) :- member_check(X,Xs).
%%% member2(X,[_X|Xs]) :- member2(X,Xs).

%%% member_check(X,[Y|Ys]) :-
%%%     ( X = Y -> true
%%%     ; member_check(X,Ys)
%%%     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Mi egy bináris fa ismeretlen részfáinak listája? (inorder)
%
% Előf: T valódi vagy részleges bináris fa
%       (minden részfája vagy ismeretlen, vagy o vagy t(G,B,J) alakú).
%
% varT(T,Vs) :- Vs a T ismeretlen részfáinak a listája,
%               az inorder bejárás sorrendjében.
/*
| ?- varT(o,Vs).
Vs = [] ? ;
no
| ?- varT(t(1,A,t(3,t(4,t(5,o,o),
                          t(C,t(7,o,D),t(B,E,F))),
                    o)),Vs).
Vs = [A,D,E,F] ? ;
no
| ?- varT(t(a,t(b,t(c,t(d,t(e,o,o),t(f,o,o)),t(g,o,o)),
                  t(h,o,o)),
              t(i,t(j,o,o),t(k,t(l,o,o),t(m,o,o)))), Vs).
Vs = [] ? ;
no
| ?- varT(T,Vs).
Vs = [T] ? ;
no
*/

% Megoldás:

%%% varT(T,Vs) :- varT_(T,[],Vs).

%%% %% varT_(T,Us,Vs) :- T ismeretlen részfáit Us elé fűzve kapjuk Vs-t.

%%% varT_(T,Us,Vs) :-
%%%     ( var(T) -> Vs = [T|Us]
%%%     ; T == o -> Vs = Us
%%%     ; T = t(_X,B,J),
%%%       varT_(J,Us,Js),
%%%       varT_(B,Js,Vs)
%%%     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


                          %% 1. zh 2010.10.13.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% Ezt a fájlt kell átnevezni és kiegészíteni. A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. A család10z/3 alapján adjuk meg a nagyanyja/2 relációt!
%
% Előf: adottak  család10z(Anya,Apa,Gyerekek)  alakú tényállítások, ahol
%   Anya és Apa névkonstansok, a Gyerekek pedig névkonstansok valódi listája.
%
% nagyanyja(X,Y) :- X az Y nagymamája a család10z/3 predikátum szerint.
%
% Például:
család10z(p,m,[a,b]).     család10z(a,n,[c,d,e]).
család10z(p,k,[f,g]).     család10z(g,e,[h]).
/*
| ?- nagyanyja(X,Y).
X = p, Y = c ? ;    X = p, Y = d ? ;    X = p, Y = e ? ;
X = a, Y = h ? ;    X = p, Y = h ? ;    no
*/

% Megoldás:

nagyanyja(X,Y) :- anyja_(X,Z), a_ja_(Z,Y).

a_ja_(X,Y) :- anyja_(X,Y).
a_ja_(X,Y) :- apja_(X,Y).

anyja_(X,Y) :- család10z(X,_,Gs), member(Y,Gs).
apja_(X,Y)  :- család10z(_,X,Gs), member(Y,Gs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Megoldás:

member2(X,[X|Xs]) :- member_check(X,Xs).
member2(X,[_X|Xs]) :- member2(X,Xs).

member_check(X,[Y|Ys]) :-
    ( X = Y -> true
    ; member_check(X,Ys)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Mi egy bináris fa ismeretlen részfáinak listája? (inorder)
%
% Előf: T valódi vagy részleges bináris fa
%       (minden részfája vagy ismeretlen, vagy o vagy t(G,B,J) alakú).
%
% varT(T,Vs) :- Vs a T ismeretlen részfáinak a listája,
%               az inorder bejárás sorrendjében.
/*
| ?- varT(o,Vs).
Vs = [] ? ;
no
| ?- varT(t(1,A,t(3,t(4,t(5,o,o),
                          t(C,t(7,o,D),t(B,E,F))),
                    o)),Vs).
Vs = [A,D,E,F] ? ;
no
| ?- varT(t(a,t(b,t(c,t(d,t(e,o,o),t(f,o,o)),t(g,o,o)),
                  t(h,o,o)),
              t(i,t(j,o,o),t(k,t(l,o,o),t(m,o,o)))), Vs).
Vs = [] ? ;
no
| ?- varT(T,Vs).
Vs = [T] ? ;
no
*/

% Megoldás:

varT(T,Vs) :- varT_(T,[],Vs).

%% varT_(T,Us,Vs) :- T ismeretlen részfáit Us elé fűzve kapjuk Vs-t.

varT_(T,Us,Vs) :-
    ( var(T) -> Vs = [T|Us]
    ; T == o -> Vs = Us
    ; T = t(_X,B,J),
      varT_(J,Us,Js),
      varT_(B,Js,Vs)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                         %% 3. zh 2010.06.22.

% Feladatok: futtat: \\inf.elte.hu\dfs\ ; másol innét: vers\plzh\2010_0512
%
% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<
%
% A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\ ; másol ebbe:  zh\plzh\2010_0622

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Számoljuk össze egy Prolog forrásfájlban
%    a szintaktikusan hibás mondatokat!        (10p)
%
% Mj.: Egy mondatot szintaktikusan hibásnak tekintünk <=>
%      a  read(F,X)  utasítással beolvasva kivételt vált ki.
%
% Előf.: F egy Prolog forrásfájl.
%
% szintaxhibaszám(F,N) :-
%     Az F-ben a szintaktikusan hibás mondatok száma N.
%
% LI ~=< Az F szövegfájlt alkotó mondatok száma.
/* 
| ?- szintaxhibaszám('err1.pl',N).
N = 4 
| ?- szintaxhibaszám('err2.pl',N).
N = 3
%%%%%%%%%%%%%%%%%%%%% Az err1.pl fájl: %%%%%%%%%%%%%%%%%%%%%%%%
g(X) :- e(X) ; p(X).
e([).
e([_|Xs]):-e(Xs).
p(X,Y) :- s(X,Y) -> true ; s(Y,X) ).
n(X) :- g(X) e(X).
p().
%%%%%%%%%%%%%%%%%%%%% Az err2.pl fájl: %%%%%%%%%%%%%%%%%%%%%%%%
g(X) :- e(X) p(X).
e([]).
3.
2.71.
e([_|Xs]):-e(Xs .
p(X,Y) :- ( s(X,Y) -> true ; s(Y,X) ).
n(X) :- p X), e(X).
p(_).
X.
*/

%% Megoldás:

szintaxhibaszám(F,N) :-
    open(F,read,S), szinthibák(S,0,N), close(S).

szinthibák(S,H,N) :-
    catch( ( read(S,X),
	     ( var(X) -> H1 is H+1 ; number(X) -> H1 is H+1 ; H1 = H ) ),
	   _, H1 is H+1 ),
    ( X == end_of_file -> N = H1
    ; szinthibák(S,H1,N)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Írjunk konstansdefiníció-elemző DCG-t! (20p)
%
% Def.: Egy konstansdefiníció alakja  C=Kif  ahol  C  kisbetűvel kezdődő
%       "angol betűs" Prolog azonosító (ezután 'id'),  Kif  pedig
%       nemnegatív egészekből (ezután 'nat') és id-ekből, bináris + és -
%       operátorokkal, továbbá zárójelezéssel felépített kifejezés
%       (az operátorok prioritása és módja a szokásos).
%
% Előf: Tokens Prolog alaplista. 
%
% konstdef(Term,Tokens,[]) :-
%     a  Tokens  lexikális elemek ( id-ek, nat-ok, '=', '.', '+', '-',
%     '(' és ')' jelek) listája által reprezentált, '.'-tal lezárt
%     konstans definíció összetett Prolog term alakja  Term,  amely
%     id-ekből, nat-okból, '='/2, '+'/2 és '-'/2 függvényszimbólumokkal
%     épül fel.
%
/*
| ?- Tokens = [a_2b,=,1,-,'(',b,+,3,')',+,22,.], konstdef(Term,Tokens,[]).
Term = (a_2b=1-(b+3)+22),
Tokens = [a_2b,=,1,-,'(',b,+,3,')',+,22,'.']

| ?- Tokens = [a_2b,=,1,-,'(',b,+,3,+,22,.], konstdef(Term,Tokens,[]).
no

| ?- Tokens = [a_2b,=,1,-,'(',b,+,3,')',*,22,.], konstdef(Term,Tokens,[]).
no
*/
%
% 
%% Megoldás:

konstdef(Id=Kif) --> id(Id), [=], kif(Kif), ['.'].

kif(Kif) --> tag(Tag), kif_vége(Tag,Kif).

tag(Id) --> id(Id).
tag(Nat) --> nat(Nat).
tag(Kif) --> ['('], kif(Kif), [')'].

id(Id) --> [Id],
    { atom(Id), atom_codes(Id,[C1|Cs]), 'angol_kisbetű_kód'(C1),
      \+ ( member(K,Cs), \+id_kód(K) )
    }.

nat(Nat) --> [Nat], { integer(Nat), Nat >= 0 }.

kif_vége(Kif1,Kif) --> ['+'], !, tag(Tag2), kif_vége(Kif1+Tag2,Kif). 
kif_vége(Kif1,Kif) --> ['-'], !, tag(Tag2), kif_vége(Kif1-Tag2,Kif).
kif_vége(Kif,Kif) --> [].

id_kód(K) :-
    ( 'angol_kisbetű_kód'(K) -> true
    ; 'angol_nagybetű_kód'(K) -> true
    ; számjegy_kód(K) -> true
    ; K == 0'_
    ).

'angol_kisbetű_kód'(K) :- 0'a =< K, K =< 0'z.
'angol_nagybetű_kód'(K) :- 0'A =< K, K =< 0'Z.
számjegy_kód(K) :- 0'0 =< K, K =< 0'9.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Gyűjtsük ki egy szövegfájlból a helytelenül definiált konstansokat! (15p)
%
% Előf.: A CF szövegfájl a 2. feladatban leírt formátumú konstans definíciók
%        sorozata. Mindegyik definíciót '.' és fehér szóköz karakter zárja le.
%        (Ezek tehát szintaktikusan helyes Prolog mondatok.)
%
% Def.: Egy konstans helytelenül definiált, ha van ugyan definíciója,
%       de az őt definiáló kifejezésben van definiálatlan konstans.
%
% Def.: Egy konstans egy kifejezésben definiálatlan, ha
%       a szövegesen megelőző konstans definíciók között
%       nincs, vagy csak helytelen definíciója van.
%
% helytelen_konstansok(CF,Hs) :-
%     A CF-ben helytelenül definiált konstansok listája Hs,
%     a helytelen definíciók sorrendjében.
%
% Ötlet: Olvassuk be a definíciókat read utasításokkal! Gyűjtsük a helyesen
%        és a helytelenül definiált konstansokat is egy-egy akkumulátorban!
%
% LI ~=< N*(K+1)*M, ahol N a konstansok, K a definiált konstansok száma,
%                   M pedig a maximális kifejezés méret.
/*
%%% A  zh10t3_konstansok.pl  fájl tartalma:
a = 1+1.
b = a-(b+2).
c = 0-a.
d = a+3-e+9.
e = 3+a-(a-c-c).
%%%
| ?- helytelen_konstansok('zh10t3_konstansok.pl',Hs).
Hs = [b,d]
*/
%

%% Megoldás:

helytelen_konstansok(CF,Hs) :-
    open(CF,read,S), helyt_kons(S,[],[],Hs), close(S).

%% Ks az eddig helyesen, Gs az eddig helytelenül definiált
%% konstanok listája, fordított sorrendben.
helyt_kons(S,Ks,Gs,Hs) :-
    read(S,CD),
    ( CD == end_of_file -> reverse(Gs,Hs)
    ; CD = ( C = Kif ),
      ( definiált_kifejezés(Kif,Ks) -> helyt_kons(S,[C|Ks],Gs,Hs)
      ; helyt_kons(S,Ks,[C|Gs],Hs)
      )
    ).

:- use_module(library(lists),[reverse/2]).

definiált_kifejezés(Kif,Ks) :-
    ( integer(Kif) -> true
    ; atom(Kif) -> member(Kif,Ks) -> true
    ; var(Kif) -> fail     % Rossz inputra se legyen végtelen rekurzió.
    ; Kif = A+B -> definiált_kifejezés(A,Ks), definiált_kifejezés(B,Ks)
    ; Kif = A-B -> definiált_kifejezés(A,Ks), definiált_kifejezés(B,Ks)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                           %% 2. zh 2010.05.12.

% Feladatok: futtat: \\inf.elte.hu\dfs\ ; másol innét: vers\plzh\2010_0512
%
% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
%
% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körű!
%
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<
%
% A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\ ; másol ebbe:  zh\plzh\2010_0512

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Melyek egy szövegfájl szavai? (15p)
%
% Def.: Egy szövegfájl egy szava egy Prolog atom.
%       A szó a fájlt alkotó karaktersorozat egy olyan szakasza, ami
%       legalább egy karaktert tartalmaz, nem tartalmaz fehér szóközt
%       (szóköz, újsor, tab karaktert),
%       de nincs olyan, őt szigorúan tartalmazó szakasz,
%       ami ne tartalmazna fehér szóközt.
%
% Előf.: F egy szövegfájl
%
% szöveg_szavai(F,As) :- As sorban az F szövegfájl szavainak listája.
%
% LI ~=< Az F szövegfájlt alkotó karaktersorozat hossza.
%
%%% Az 'alma.txt' tartalma "alma a fa alatt", fehér szóközökkel tagolva. 
% | ?- szöveg_szavai('alma.txt',As).
% As = [alma,a,fa,alatt]
%%% Az 'a.txt' fehér szóközöket tartalmaz és egy "a" betűt.
% | ?- szöveg_szavai('a.txt',As).
% As = [a]
%%% Az 'üres.txt' csak fehér szóközöket tartalmaz.
% | ?- szavai('üres.txt',As).
% As = []

%% Megoldás:

szöveg_szavai(F,As) :-
    open(F,read,S),
    get_code(S,C), szövszav(C,S,As),
    close(S).

szövszav(C,S,As) :-
    ( C == -1 -> As = []
    ; fehér_szóköz(C) -> get_code(S,D), szövszav(D,S,As)
    ; szöv1szó(C,S,Cs,D), atom_codes(A,Cs), As = [A|Bs], szövszav(D,S,Bs)
    ).

szöv1szó(C,S,[C|Cs],D) :-
    get_code(S,E),
    ( fehér_szóköz(E) -> Cs=[], D=E
    ; szöv1szó(E,S,Cs,D)
    ).

fehér_szóköz(-1).
fehér_szóköz(0' ).
fehér_szóköz(0'\n).
fehér_szóköz(0'\t).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Egy lista valahány 'a' betűből, majd ugyanannyi 'b' betűből áll. (15p)
%
% Definiáljuk az 'a^N_b^N'/3 predikátumot DCG nyelvtannal!
% A kódban a felhasználói predikátumokat DCG szintaxissal kell megírni!
% Ötlet: Induljunk ki az S --> aSb | 'epszilon' kfl nyelvtanból!
%
% Előf: N nemnegatív egész vagy AsBs valódi lista.
%
% 'a^N_b^N'(N,AsBs,[]). :- Az AsBs valódi lista sorban N db. a betűt, majd 
%                          N db. b betűt tartalmaz, ahol N nemnegatív egész.
%
% LI ~=< N.
%
% | ?- 'a^N_b^N'(3,AsBs,[]).
% AsBs = [a,a,a,b,b,b]
% | ?- 'a^N_b^N'(N,[a,a,a,b,b,b],[]).
% N = 3
% | ?- 'a^N_b^N'(0,AsBs,[]).
% AsBs = []
% | ?- 'a^N_b^N'(N,[],[]).
% N = 0
% | ?- 'a^N_b^N'(N,[a,a,b,b,b],[]).
% no
% | ?- 'a^N_b^N'(N,[a,a,a,b,b],[]).
% no
% | ?- 'a^N_b^N'(N,[A,B,C,D],[]).
% A = a,    B = a,    C = b,    D = b,    N = 2
% | ?- 'a^N_b^N'(N,[A,B,C],[]).
% no
%%% Szorgalmi (akkor is megad minden egyes megoldást,
%%%            ha var(N), de AsBs parciális lista):
% | ?- 'a^N_b^N'(N,AsBs,[]).
% N = 0,    AsBs = [] ? ;
% N = 1,    AsBs = [a,b] ? ;
% N = 2,    AsBs = [a,a,b,b] ? ;
% N = 3,    AsBs = [a,a,a,b,b,b] ? ;
% N = 4,    AsBs = [a,a,a,a,b,b,b,b] ?
% yes
% | ?- 'a^N_b^N'(N,[A|AsBs],[]).
% A = a,    N = 1,    AsBs = [b] ? ;
% A = a,    N = 2,    AsBs = [a,b,b] ? ;
% A = a,    N = 3,    AsBs = [a,a,b,b,b] ? ;
% A = a,    N = 4,    AsBs = [a,a,a,b,b,b,b] ? ;
% A = a,    N = 5,    AsBs = [a,a,a,a,b,b,b,b,b] ? 
% yes
% | ?- 'a^N_b^N'(N,[b|AsBs],[]).
% no
% | ?- 'a^N_b^N'(N,[a,b|AsBs],[]).
% N = 1,    AsBs = []

%% Megoldás:

'a^N_b^N'(N) --> { integer(N), N>=0, ! },  aNbN(N).
'a^N_b^N'(N) --> { var(N) }, aN_bN(N).

aNbN(N) --> { N>0, N1 is N-1 }, [a], aNbN(N1), [b].
aNbN(0) --> [].

aN_bN(0) --> [].
aN_bN(N) -->  [a], aN_bN(N1), [b], { N is N1+1 }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                      %% 1. zh 2010.03.17.

% A megadott előfeltétellel a vonatkozó célok
% keresési fája véges kell legyen
%
% A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
%
% Beadás: futtat: \\inf.elte.hu\dfs\zh ;
%         másol ebbe:  plzh\2010_0317

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Mi egy bináris fa maximális, gyengén bináris részfája?
%
% Mj.: Egy fa akkor gyengén bináris, ha önmaga nemüres, és
%      vagy mindkét leágazása üres, vagy mindkettő nemüres.
%
% Jelölés: üres fa: o (névkonstans);
%          nemüres fa: t(Gyökér,BalRészfa,JobbRészfa)
%                       (ahol a Gyökér egy tetszőleges Prolog term).
%
% Előf: T valódi bináris fa (minden részfája o vagy t(G,B,J) alakú).
%
% maxGyBinRfa(T,TM) :- TM a T gyökeréhez legközelebbi, ún. maximális,
%                      gyengén bináris részfája.
%
% Mj.: TM létezik <=> T nemüres bináris fa
%      TM létezik => egyértelmű
%
% | ?- maxGyBinRfa(t(1,t(2,o,t(3,o,o)),t(4,t(5,o,o),o)),S).
% S = t(1,t(2,o,t(3,o,o)),t(4,t(5,o,o),o))
%
% | ?- maxGyBinRfa(t(1,t(2,o,t(3,o,o)),o),S).
% S = t(3,o,o)
%
% | ?- maxGyBinRfa(t(1,t(2,t(4,t(5,o,o),o),t(3,o,o)),o),S).
% S = t(2,t(4,t(5,o,o),o),t(3,o,o))
%
% | ?- maxGyBinRfa(o,S).
% no

% Megoldás:

maxGyBinRfa(T,TM) :-
    T = t(_,Bal,Jobb),
    ( Bal==o, Jobb==o -> TM = T
    ; Bal\==o, Jobb\==o -> TM = T
    ; maxGyBinRfa(Bal,TB) -> TM = TB
    ; maxGyBinRfa(Jobb,TM)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Melyek egy bináris fa erősen bináris részfái?
%
% Mj.: Egy fa akkor erősen bináris, ha
%      önmaga nemüres, bináris és egyik leágazása sem üres.
%
% Előf: T valódi bináris fa (minden részfája o vagy t(G,B,J) alakú).
%
% eBinRfa(T,F) :- F a T erősen bináris részfája
%
% | ?- eBinRfa(o,F).
% no
%
% | ?- eBinRfa(t(3,o,o),F).
% no
%
% | ?- eBinRfa(t(1,t(2,o,o),o),F).
% no
%
% | ?- eBinRfa(t(1,t(2,o,o),t(3,o,o)),F).
% F = t(1,t(2,o,o),t(3,o,o)) ? ;
% no
%
% | ?- eBinRfa(t(1,o,t(3,o,o)),F).
% no
%
% | ?- eBinRfa(t(1,o,t(3,t(4,t(5,o,o),
%                            t(6,t(7,o,o),t(8,o,t(9,o,o)))),
%                        o)),F).
% F = t(4,t(5,o,o),t(6,t(7,o,o),t(8,o,t(9,o,o)))) ? ;
% F = t(6,t(7,o,o),t(8,o,t(9,o,o))) ? ;
% no

% Megoldás:

eBinRfa(T,F) :-
    T = t(_,Bal,Jobb),
    ( Bal\==o, Jobb\==o, F = T
    ; eBinRfa(Bal,F)
    ; eBinRfa(Jobb,F)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Mi egy bináris fa erősen bináris csúcsainak listája? (preorder)
%
% Mj.: Egy bináris fa egy csúcsa erősen bináris <=>
%             a hozzá tartozó részfa erősen bináris.
%
% Előf: T valódi bináris fa (minden részfája o vagy t(G,B,J) alakú).
%
% eBinCsúcsok(T,Bs) :- Bs a T erősen bináris csúcsainak listája
%                              a preorder bejárás sorrendjében.
%
% Mj.: Ne használjunk a megoldáshoz extra-logikai eszközöket.
%      (Tájékoztatásul: Eddig ilyeneket nem tanultunk.)
%
% | ?- eBinCsúcsok(o,Bs).
% Bs = []
%
% | ?- eBinCsúcsok(t(1,o,t(3,t(4,t(5,o,o),
%                                t(6,t(7,o,o),t(8,o,o))),
%                            o)),Bs).
% Bs = [4,6]
%
% | ?- eBinCsúcsok(t(a,t(b,t(c,t(d,t(e,o,o),t(f,o,o)),t(g,o,o)),
%                          t(h,o,o)),
%                      t(i,t(j,o,o),t(k,t(l,o,o),t(m,o,o)))), Bs).
% Bs = [a,b,c,d,i,k]
%
% | ?- eBinCsúcsok(t(1,o,t(3,t(4,o,t(6,t(7,o,o),o)),o)),Bs).
% Bs = []

% Megoldás:

eBinCsúcsok(T,Bs) :- eBinCsúcsok_(T,[],Bs).

eBinCsúcsok_(o,Xs,Xs).
eBinCsúcsok_(t(X,B,J),Xs,Zs) :-
    ( B\==o, J\==o -> Zs=[X|Ys]
    ; Zs=Ys
    ),
    eBinCsúcsok_(J,Xs,Js),
    eBinCsúcsok_(B,Js,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                           %% 2. zh 2009.12.10.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<
% A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2009_1210

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Melyek egy Prolog atom szavai? (20p)
%
% Def.: Egy Prolog atom egy szava szintén egy Prolog atom.
%       A szó az atomot alkotó karaktersorozat egy olyan szakasza,
%       ami legalább egy karaktert tartalmaz, nem tartalmaz szóközt, de
%       nincs olyan, őt tartalmazó szakasz, ami ne tartalmazna szóközt.
%
% Előf.: atom(A)
%
% szavai(A,As) :- As sorban az A Prolog atom szavainak listája.
%
% LI ~=< Az A atomot alkotó karaktersorozat hossza.
%
% | ?- szavai(' alma a   fa alatt    ',As).
% As = [alma,a,fa,alatt]
% | ?- szavai(alma,As).
% As = [alma]
% | ?- szavai('',As).
% As = []
%
% Ötlet: Definiáljuk a következő segédeljárást!
%% 'első_szó'(Cs,C,Bs,Ds) :- a [C|Cs] karakterkód lista első szavának
%%      karakterkód listája Bs, a maradék Ds. Feltesszük, hogy C nem szóköz.
%
% | ?- 'első_szó'("lma a fa alatt",0'a,Bs,Ds).
% Bs = [97,108,109,97],
% Ds = [97,32,102,97,32,97,108,97,116,116]
% | ?- 'első_szó'("",0'a,Bs,Ds).
% Bs = [97],
% Ds = []

szavai(A,As) :- atom_codes(A,Cs), szólista(Cs,As).

szólista([],[]).
szólista([C|Cs],Ss) :-
    ( C == (0' ) -> szólista(Cs,Ss)
    ; 'első_szó'(Cs,C,Bs,Ds), atom_codes(A,Bs), Ss = [A|As], szólista(Ds,As)
    ).

'első_szó'([],C,[C],[]).
'első_szó'([E|Es],C,[C|Cs],Ds) :-
    ( E == (0' ) -> Cs = [], Ds = Es
    ; 'első_szó'(Es,E,Cs,Ds)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Lapítsunk le egy összetett listát! (10p)
%
% Definiáljuk a flatten/3 predikátumot generatív DCG nyelvtannal!
% (Három szabály az üres lista, a nemüres lista és a nem-lista eseteire.)
% A kódban nem szabad kapcsos zárójeleket használni! 
%
% Def.: Az összetett lista olyan valódi lista (lehet üres is),
%       amelynek elemei vagy nem listatermek, vagy összetett listák.
%       Az Ls összetett lista lelapítása az Ls mélységi bejárása során
%       adódó nem-listatermek egyszerű listája (nincs listaterm eleme).
%
% Mj.: Listaterm alatt most egy [_|_] vagy [] struktúrájú termet értünk.
%
% Előf: Ls összetett alaplista.
%
% flatten(Ls,Fs,Gs) :- az Fs-Gs d-lista az Ls lelapítását reprezentálja.
%
% LI ~=< Ls-ben található függvényszimbólumok és egyszerű résztermek száma.
%
% | ?- flatten([f1(x1),2,[[x3],g(4,x4)],[[[]]],[a5,[[[[6]]]]]],Fs,Gs).
% Fs = [f1(x1),2,x3,g(4,x4),a5,6|Gs]
% | ?- flatten([f1(x1),2,[[x3],g(4,x4)],[[[]]],[a5,[[[[6]]]]]],Fs,[]).
% Fs = [f1(x1),2,x3,g(4,x4),a5,6]


flatten([]) --> !, [].
flatten([X|Xs]) --> !, flatten(X), flatten(Xs).
flatten(X) --> [X].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                         %% 1. zh 2009.10.20.

% A megadott előfeltétellel a vonatkozó célok
% keresési fája véges kell legyen.
% A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2009_1020

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Mennyi egy bináris számfa unáris részfái gyökérelemeinek összege?
%
% Mj.: Egy részfa akkor unáris, ha egyik leágazása üres, a másik nemüres.
%
% Jelölés: üres fa: o (névkonstans);
%          nemüres fa: t(Gyökér,BalRészfa,JobbRészfa)
%                       (ahol a Gyökér egy szám).
% Előf: T valódi bináris fa, minden csúcsa egy-egy számot tartalmaz.
%
%  binfaUössz(T,S) :- S a T unáris részfái gyökerében található számok összege. 
%
% | ?- binfaUössz(t(1,t(2,o,t(3,o,o)),t(4,t(5,o,o),o)),S).
% S = 6

% Megoldás:

binfaUössz(T,S) :- bfUs(T,0,S).

bfUs(T,A,S) :-
    ( T == o -> S=A
    ; T = t(G,B,J),
      bfUs(B,A,SB),
      hozzáad(SB,G,B,J,SG),
      bfUs(J,SG,S)
    ).

hozzáad(SB,G,B,J,SG) :-
    ( B == o, J\==o -> SG is SB+G
    ; B \== o, J==o -> SG is SB+G
    ; SG = SB
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Melyek egy valódi számlista monoton növekvő szakaszai?
%
% Előf: Xs valódi számlista.
%
% mnsz(Xs,Ys) :- Ys legalább két elemű lista. Ys az Xs monoton
%    növekvő szakasza. Nincs az Xs-nek olyan, monoton növekvő
%    szakasza, amely szigorúan tartalmazná Ys-t.
%
% | ?- mnsz([1,1,2,3,2,4,6,5,4,7,6,5,4,8],Ns).
% Ns = [1,1,2,3] ? ;
% Ns = [2,4,6] ? ;
% Ns = [4,7] ? ;
% Ns = [4,8] ? ;
% no


% 1. megoldás:

mnsz1([X,Y|Xs],[X,Y|Ys]) :- X=<Y, mnsz1vége(Xs,Y,Ys).
mnsz1(Xs,Ys) :- mnsz1hátrébb(Xs,Ys).

mnsz1vége([],_Y,[]).
mnsz1vége([X|Xs],Y,Ys) :-
    ( Y =< X -> Ys = [X|Zs], mnsz1vége(Xs,X,Zs)
    ; Ys = []
    ).

mnsz1hátrébb([X,Y|Xs],Ys) :-
    ( X>Y -> mnsz1([Y|Xs],Ys)
    ; mnsz1hátrébb([Y|Xs],Ys)
    ).

% 2. megoldás:

mnsz(Xs,Ms) :- mnsz2(Xs,Ys,Vs), ( Ms=Ys ; mnsz(Vs,Ms) ).

mnsz2([X,Y|Xs],Ms,Vs) :-
    ( X=<Y -> Ms=[X,Y|Ns],  mnsz2vége(Xs,Y,Ns,Vs)
    ; mnsz2([Y|Xs],Ms,Vs)
    ).

mnsz2vége([X|Xs],Y,Ns,Vs) :-
    ( Y=<X -> Ns=[X|Ys], mnsz2vége(Xs,X,Ys,Vs)
    ; Ns=[], Vs=[X|Xs]
    ).
mnsz2vége([],_Y,[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				% 3. zh 2009.06.04.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<

% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Az eredeti zh-ban a következő két sor nem volt kommentbe téve.
%%% :- module( findall, [ r_member/2, find_all_0/3, find_all/3 ] ).
%%% :- meta_predicate find_all_0(?,:,?), find_all(?,:,?).

% 1. Melyek egy összetett lista r-elemei? (10p)
%
% Def.: Az összetett lista olyan valódi lista (lehet üres is),
%       amelynek elemei vagy nem listatermek, vagy összetett listák.
%       Az Xs összetett lista r-elemei egyrészt Xs nem-listaterm elemei,
%       másrészt Xs elemeinek r-elemei.
%
% Mj.: Listaterm alatt most egy [_|_] vagy [] struktúrájú termet értünk.
%
% Előf: Xs összetett lista, (X,Xs) nem tartalmaz duplikált változót (NSTO). 
%
% r_member(X,Xs) :- X az Xs r-eleme.
%
% | ?- r_member(X,[f1(X1),2,[[X3],g(4,X4)],[[[]]],[a5,[[[[6]]]]]]).
% X = f1(X1) ? ;    X = 2 ? ;    X3 = X ? ;
% X = g(4,X4) ? ;   X = a5 ? ;    X = 6 ? ;    no

r_member(X,[X|_Xs]) :- ( var(X) -> true ; X \= [_|_], X \= [] ).
r_member(X,[Y|_Xs]) :- nonvar(Y), Y = [_|_], r_member(X,Y).
r_member(X,[_X|Xs]) :- r_member(X,Xs).

% 2. Írjuk meg findall/3-mal ekvivalens a find_all/3 predikátumot! (10p)
%
% Előf: Cél egy Prolog cél(sorozat), véges keresési fával.
%
% Mj.: Rendesen X a Cél egyik változója, vagy egy olyan összetett term,
%      aminek ismeretlenjei a Cél változói.
%
% find_all(X,Cél,Xs) :- X behelyettesítései a Cél megoldásai szerint
%     adják az Xs valódi listát, a megoldások sorrendjének megfelelően.
%
% Részleges megoldás:

:- dynamic(megoldás/1).

find_all_0(X,Cél,_Xs) :-
    retractall(megoldás(_)), % tisztázás, ha egy régi Cél elszállt volna...
    Cél, assertz(megoldás(X)), fail.
find_all_0(_X,_Cél,Xs) :- 'begyűjt_0'(Xs).

'begyűjt_0'(Ys) :-
    ( retract(megoldás(X)) -> Ys = [X|Xs], 'begyűjt_0'(Xs)
    ; Ys = []
    ).

% A find_all_0/3 a beépített findall/3-hoz hasonló:
%
% Egy lista szétvágásai 2 részre:
% | ?- find_all_0(Xs+Ys,append(Xs,Ys,[1,2,3]),As). 
% As = [[]+[1,2,3],[1]+[2,3],[1,2]+[3],[1,2,3]+[]]
%
% A fenti összeget az alábbi csavarral is megkaphatjuk:
% | ?- find_all_0(Zs+Ys,(append(Xs,Ys,[1,2,3]),findall(Z,member(Z,Xs),Zs)),As).
% As = [[]+[1,2,3],[1]+[2,3],[1,2]+[3],[1,2,3]+[]]
%
% A beágyazott find_all_0 hívás most nem várt módon működik:
% | ?- find_all_0(Zs+Ys,(append(Xs,Ys,[1,2,3]),find_all_0(Z,member(Z,Xs),Zs)),
%                                                                         As).
% As = [[1,2,3]+[]]

% Feladat: Definiáljuk a find_all/3-at a beépített findall/3-nak megfelelően!
% A megoldásban ne használjuk az előre definiált vagy könyvtári megoldás
% összegyűjtő predikátumok egyikét sem!

% Megoldás:
% (Feltesszük, hogy a Cél nem használja sem a findall:megoldás/2 predikátumot,
%  sem a findall:find_all_sorszám feketetábla kulcsot,
%  amiket a findall modul normál használat esetén eltakar.)

:- dynamic(megoldás/2).

:- bb_put(find_all_sorszám,1).

find_all(X,Cél,Xs) :-
    bb_get(find_all_sorszám,I),
    I1 is I+1, bb_put(find_all_sorszám,I1),    
    (
      retractall(megoldás(I,_)), % tisztázás, ha egy régi Cél elszállt volna...
      Cél, assertz(megoldás(I,X)), fail
    ;
      'begyűjt'(I,Xs)
    ).

'begyűjt'(I,Ys) :-
    ( retract(megoldás(I,X)) -> Ys = [X|Xs], 'begyűjt'(I,Xs)
    ; Ys = []
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				% 2. zh 2009.05.11.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<

% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2009_0511

% Mj.: Az alábbi relációt DCG-vel kell definiálni.
%
% Def: e-konjunkció: egyenlőségek és egyenlőtlenségek & operátorral képzett
%    konjunkciója. Az egyenlő(tlen)ségek: =,\=,<,>,=<,>=. Paramétereik egész
%    számok ill. a C azonosítóknak megfelelő Prolog atomok lehetnek.
%
% Mj: Az & operátor balról jobbra zárójelező, 900 prioritású 
%     infix műveleti jel (definiálni kell). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Elemezzük DCG-vel egyenlőségek és egyenlőtlenségek konjunkcióját! (12p)
%
% Előf: Ts alaplista.  
%
% konjunktív_feltétel(S,Ts,Rs) :- Ts elemei Prolog egészek és atomok. 
%     Ez utóbbiak lehetnek a C nyelv azonosítófogalmának megfelelő argumentumok,
%     egyenlőség és egyenlőtlenség szimbólumok (=,\=,<,>,=<,>=) és konjunkció 
%     jelek (&). Ts-Rs d-lista. S a (Ts-Rs) d-listában tokenlistaként ábrázolt
%     konjunkciónak megfelelő e-konjunkció. 
%
% LI ~=< a Ts lista hossza + Ts atomjainak összhossza. (Megoldásonként).
%
% | ?- konjunktív_feltétel(S,[a,>=,2,&,1,=<,z2,&,'A_1',\=,a],[]).
% S = (a>=2&1=<z2&'A_1'\=a) ? ;
% no
% | ?- konjunktív_feltétel(S,[a,>=,2,&,1,=<,z2,&,'A_1',\=,a],[a]).
% no
% | ?- konjunktív_feltétel(S,[29,>,'_D',&,-1,<,z0_,&,xx,=,a],Rs).
% S = (29>'_D'),                 Rs = [&,-1,<,z0_,&,xx,=,a] ? ;
% S = (29>'_D'& -1<z0_),         Rs = [&,xx,=,a] ? ;
% S = (29>'_D'& -1<z0_&xx=a),    Rs = [] ? ;
% no
% | ?- konjunktív_feltétel(S,[29,>,'_D',&,-1,<,&,xx,=,a],Rs).
% S = (29>'_D'),    Rs = [&,-1,<,&,xx,=,a] ? ;
% no

% Megoldás:

:- op(900,yfx,&).

konjunktív_feltétel(S) --> {ground(S)}, !, konjTokenLista(S). % generatív irány 
konjunktív_feltétel(S) --> feltétel(S1), konjunkció_vége(S1,S). % elemző irány

konjunkció_vége(S,S) --> [].
konjunkció_vége(S1,S) --> [&], feltétel(S2), konjunkció_vége(S1&S2,S).

feltétel(F) --> argumentum(A1), relációjel(R), argumentum(A2),
    { functor(F,R,2), arg(1,F,A1), arg(2,F,A2) }.

relációjel(=) --> [=].
relációjel(\=) --> [\=].
relációjel(>) --> [>].
relációjel(<) --> [<].
relációjel(>=) --> [>=].
relációjel(=<) --> [=<].

argumentum(A) --> [A],
    { integer(A) -> true
    ; atom(A), atom_codes(A,[C|Cs]), alfabetikus(C),
      \+ ( member(X,Cs), \+alfanumerikus(X) )
    }.

alfabetikus(C) :-
    ( C >= 0'a, C =< 0'z -> true
    ; C >= 0'A, C =< 0'Z -> true
    ; C == 0'_
    ).

alfanumerikus(C) :-
    ( alfabetikus(C) -> true
    ; C >= 0'0, C =< 0'9
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Egészítsük ki az előző megoldást úgy, hogy fordított irányban is működjön!
%    (Generatív irány: az előző feladattól függetlenül is megoldható.) (8p)
%
% Előf: ground(S), azaz S alapterm.
%
% konjunktív_feltétel(S,Ts,Rs) :- a Ts-Rs d-lista az S e-konjunkciónak
%                                 megfelelő tokenlista.
%
% LI ~=< az S függgvényszimbólumainak és atomic-jainak száma
%        + S atomjainak összhossza.
%
% | ?- konjunktív_feltétel(a>=2&1=<z2&'A_1'\=a,Ts,[]).
% Ts = [a,>=,2,&,1,=<,z2,&,'A_1',\=,a] ? ;
% no
% | ?- konjunktív_feltétel(29>'_D'& -1<z0_&xx=a,Ts,Rs).
% Ts = [29,>,'_D',&,-1,<,z0_,&,xx,=,a|Rs] ? ;
% no
% | ?- konjunktív_feltétel(29>'_D',Ts,Rs).
% Ts = [29,>,'_D'|Rs] ? ;
% no
% | ?- konjunktív_feltétel(29>'_D'& -1<(=)&xx=a,Ts,Rs).
% no
% | ?- konjunktív_feltétel(29>'_D'& -1<(=)&xx=\=a,Ts,Rs).
% no
% | ?- konjunktív_feltétel(29& -1<(=)&xx=\=a,Ts,Rs).
% no

% Megoldás:

konjTokenLista(S&R) --> !, konjTokenLista(S), [&], relTokenLista(R).
konjTokenLista(R) --> relTokenLista(R).

relTokenLista(Rel) -->
    { functor(Rel,RelJel,2), arg(1,Rel,A), arg(2,Rel,B) },
    argumentum(A), relációjel(RelJel), argumentum(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				% 1. zh 2009.03.23.

% A megadott előfeltétellel a vonatkozó célok
% keresési fája véges kell legyen.
% A beadandó fájl neve:
% x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2009_0323

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Melyek egy bináris fa adott mélységben lévő csúcsai?
%
% Jelölés: üres fa: o (névkonstans);
%          nem üres fa: t(Gyökér,BalRészfa,JobbRészfa)
%
% Előf: T valódi bináris fa, D egész szám.
%
% dmélycsúcs(D,T,X) :- X a T, D mélységű csúcsa.
%
% | ?- dmélycsúcs(-1,t(1,t(A2,o,t(3,o,o)),t(4,t(b5,o,o),o)),X).
% no
% | ?- dmélycsúcs(0,t(1,t(A2,o,t(3,o,o)),t(4,t(b5,o,o),o)),X).
% X = 1 ? ;    no
% | ?- dmélycsúcs(1,t(1,t(A2,o,t(3,o,o)),t(4,t(b5,o,o),o)),X).
% A2 = X ? ;    X = 4 ? ;    no
% source_info
% | ?- dmélycsúcs(2,t(1,t(A2,o,t(3,o,o)),t(4,t(b5,o,o),o)),X).
% X = 3 ? ;    X = b5 ? ;    no
% source_info
% | ?- dmélycsúcs(3,t(1,t(A2,o,t(3,o,o)),t(4,t(b5,o,o),o)),X).
% no

% Megoldás:

dmélycsúcs(D,t(_X,B,J),X) :-
    D>0, D1 is D-1,
    ( dmélycsúcs(D1,B,X) ; dmélycsúcs(D1,J,X) ).
dmélycsúcs(0,t(X,_B,_J),X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Mi egy bináris számfa adott mélységben lévő csúcsainak összege?
% Mj.: A program ne hozzon létre összetett terme(ke)t!
%
% Előf: T valódi bináris számfa, D nemnegatív egész szám.
%
% dmélycsúcssum(D,T,S) :- S a T, D mélységű csúcsainak összege.
%
% | ?- dmélycsúcssum(t(1,t(2,o,t(3,o,o)),t(4,t(5,o,o),o)),0,S).
% S = 1
% | ?- dmélycsúcssum(t(1,t(2,o,t(3,o,o)),t(4,t(5,o,o),o)),1,S).
% S = 6
% | ?- dmélycsúcssum(t(1,t(2,o,t(3,o,o)),t(4,t(5,o,o),o)),2,S).
% S = 8
% | ?- dmélycsúcssum(t(1,t(2,o,t(3,o,o)),t(4,t(5,o,o),o)),3,S).
% S = 0

% 1. megoldás:
dmélycsúcssum(o,_D,0).
dmélycsúcssum(t(X,B,J),D,S) :-
    ( D > 0 -> D1 is D-1,
               dmélycsúcssum(B,D1,SB), dmélycsúcssum(J,D1,SJ),
               S is SB+SJ
    ; S = X
    ).


% 2. megoldás (akkumulátor technikával):
dmélycsúcssum_a(T,D,S) :- dmélycsúcssum_a(T,D,0,S).

% dmélycsúcssum_a(T,D,A,S) :-
%     S az A szám és a T bináris fa D mélységű csúcsainak összege.
dmélycsúcssum_a(o,_D,S,S).
dmélycsúcssum_a(t(X,B,J),D,A0,S) :-
    ( D > 0 -> D1 is D-1,
               dmélycsúcssum_a(B,D1,A0,A1), dmélycsúcssum_a(J,D1,A1,S)
    ; S is A0+X
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Adjuk meg egy bináris fa adott mélységben lévő csúcsainak listáját!
% Mj.: Az előre definiált predikátumok közül csak az aritmetikaiak használhatók,
%      és még az '='/2.
%
% Előf: T valódi bináris fa, D nemnegatív egész szám.
%
% dmélycsúcsok(T,D,Xs) :- Xs a T, D mélységű csúcsainak listája,
%                 a fa megfelelő szintjét balról jobbra olvasva.
%
% | ?- dmélycsúcsok(t(1,t(a2,o,t(3,o,o)),t(4,t(B5,o,o),o)),0,Xs).
% Xs = [1]
% | ?- dmélycsúcsok(t(1,t(a2,o,t(3,o,o)),t(4,t(B5,o,o),o)),1,Xs).
% Xs = [a2,4]
% | ?- dmélycsúcsok(t(1,t(a2,o,t(3,o,o)),t(4,t(B5,o,o),o)),2,Xs).
% Xs = [3,B5]
% | ?- dmélycsúcsok(t(1,t(a2,o,t(3,o,o)),t(4,t(B5,o,o),o)),3,Xs).
% Xs = []

% Naiv megoldás:
dmélycsúcsok(o,_D,[]).
dmélycsúcsok(t(X,B,J),D,Xs) :-
    ( D > 0 ->
        D1 is D-1,
      	dmélycsúcsok(B,D1,Bs),
	dmélycsúcsok(J,D1,Js),
        append_(Bs,Js,Xs)
    ; Xs = [X]
    ).

append_([],Ys,Ys).
append_([X|Xs],Ys,[X|Zs]) :- append_(Xs,Ys,Zs).

% Akkumulátor technikával:
dmélycsúcsok_a(T,D,Xs) :- dmélycsúcsok_a(T,D,[],Xs).

% dmélycsúcsok_a(T,D,As,Xs) :- Az Xs listát úgy kapjuk, hogy
%     az As lista elé fűzzük a T, D mélységű csúcsainak listáját.
dmélycsúcsok_a(o,_D,Xs,Xs).
dmélycsúcsok_a(t(X,B,J),D,As,Xs) :-
    ( D > 0 ->
        D1 is D-1,
	dmélycsúcsok_a(J,D1,As,Js),
      	dmélycsúcsok_a(B,D1,Js,Xs)
    ; Xs = [X|As]
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				% 3. zh 2009.01.06.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<

% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2009_0106

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Melyek egy Prolog szabály törzsének elemi predikátumhívásai? (15p)
%
% Def.: Egy szabálytörzset konjunkciók, diszjunkciók, lokális vágók
%       és negációk struktúrálnak. Elemi predikátumhívásnak tekintünk egy olyan,
%       jól definált Prolog hívást, ami nem a felsorolt programstruktúrák
%       valamelyikére vonatkozik.
%
% Előf: Szabály egy szintaktikusan helyes Prolog szabály. 
%
%  szteh( Szabály, PH ) :- PH a Szabály törzsének egy elemi predikátumhívása.
%
% LI ~=< Szabály programstruktúráinak és elemi predikátumhívásainak száma
%        (az összes megoldásra együtt).
%
% | ?- szteh( ( p(X,Y):- ( a -> q(Y,X), r ; X ), b(X),
%                        ( c(Y) ; d(X) -> \+e(X,y) ; \+ (c(X),d(1)) ) ),
%              P ).
% P = d(1) ? ;    P = c(X) ? ;    P = e(X,y) ? ;    P = d(X) ? ;    P = c(Y) ? ;
% P = b(X) ? ;    P = r ? ;       P = q(Y,X) ? ;    P = a ? ;       no

szteh( (_Fej :- Törzs), PH ) :- teh(Törzs,PH).

teh(Törzs,PH) :-
    ( atom(Törzs) -> PH = Törzs
    ; compound(Törzs),
      ( functor(Törzs,F,N), \+programstruktúráló(F,N) -> PH = Törzs
      ; ( arg(2,Törzs,T) ; arg(1,Törzs,T) ), teh(T,PH)
      )
    ).

programstruktúráló(',',2).
programstruktúráló(';',2).
programstruktúráló('->',2).
programstruktúráló(\+,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Irassuk ki a T alaptermben előforduló valódi listákat! (15p)
%
% Mj.: A kiírások sorrendje lényegtelen.
%      Ha egy listát kiíratunk, a suffixeit ne írassuk ki külön,
%      de az elemeiben található esetleges listákat igen!
%      Ne használjunk megoldás összegyűjtő predikátumokat (pl. findall/3)!
%
% Előf: T alapterm.
%
% tLki(T) : A standard outputhoz fűzi a T-ben található valódi,
%           nemüres listákat, a Prolog read/2 által beolvasható formátumban.
/*
| ?- tLki( t([a([1,2,3],['A','B'([b])])],[c,d|e([f],[],[[g,h],i])]) ).
[[g,h],i].
[g,h].
[f].
[a([1,2,3],['A','B'([b])])].
['A','B'([b])].
[b].
[1,2,3].
*/

% Megoldás:

tLki(T) :-
    ( list1(T) -> writeq(user,T), write(user,'.\n'), tLki_elemek(T)
    ; compound(T) -> functor(T,_F,N), ntLki(N,T)
    ; true
    ).

list1([_X|Xs]) :- list_(Xs).

list_([]).
list_([_X|Xs]) :- list_(Xs).

tLki_elemek([X|Xs]) :- tLki(X), tLki_elemek(Xs).
tLki_elemek([]).

ntLki(N,T) :-
    arg(N,T,A), tLki(A), 
    ( N > 1 -> N1 is N-1, ntLki(N1,T)
    ; true
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				% 2. zh 2008.12.12.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<

% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2008_1212

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Másoljuk át egy Prolog fájlból a nulladrendű klózokat egy másikba! (15p)
%
% Def.: Egy nulladrendű Prolog klóz tényállítás lehet, vagy olyan szabály, 
%       amelynek törzsét konjunkciók, lokális vágók, diszjunkciók, zárójelek 
%       struktúrálhatják, elemi részcéljai pedig nulladrendű predikátumhívások.
% Mj.: Ezt a definíciót természetesen a feladat egyszerüsítése végett
%      mondtuk ki így.
%
% Előf: F egy olvasható szövegfájl, ami Prolog mondatokat tartalmaz. 
%
%  'nulladrendű_klózok'(F,G) :- G az F nulladrendű klózait
%                               tartalmazza, az eredeti sorrendben.
%
% LI ~=< F mondatainak száma + F szabályai törzsében az összes
%        (C1,C2), (C1;C2) és (C1->C2) programkonstrukció száma.
%
/* Egy tesztfájl, az 'a.pl' tartalma:
g :- ( e -> g ; p ).
e.    e :- 'P', n, ( p ; n -> g, e).    e :- P, k.
p :- n ; e(x).    p :- g, \+e.
p :- ( s -> true ; s, n ), e.    p :- e.
n(X) :- X -> true.
a :- ( b:-c ).
:- a.
*/
% | ?- 'nulladrendű_klózok'('a.pl','b.pl').
% yes
/* Ezután a 'b.pl' tartalma (a formátumtól eltekintve):
g:-e->g;p.
e.
e:-'P',n,(p;n->g,e).
p:-(s->true;s,n),e.
p:-e.
*/

% Megoldás:

'nulladrendű_klózok'(F,G) :- 
     open(F,read,R), open(G,write,W),
     nsz(R,W), 
     close(R), close(W).

nsz(R,W) :-
    read(R,M),
    ( M == end_of_file -> true
    ; 'nulladrendű_klóz'(M) -> writeq(W,M), write(W,'.\n'), nsz(R,W)     
    ; nsz(R,W)
    ).

'nulladrendű_klóz'(M) :-
    ( compound(M) ->                             % szabály
          M = ( Fej :- Törzs ), atom(Fej), 'nulladrendű_törzs'(Törzs)
    ; atom(M)                                    % tény
    ).

'nulladrendű_törzs'(T) :-
    ( atom(T) -> true
    ; compound(T), törzsfelbontás(T,X,Y), 
      'nulladrendű_törzs'(X), 'nulladrendű_törzs'(Y)
    ).
% A  compound(T) feltétel arra szolgál, hogy a formailag egyetlen logikai
% változóból álló Prolog célokat kiszűrje.

törzsfelbontás( (Cél1,Cél2),  Cél1, Cél2).
törzsfelbontás( (Cél1;Cél2),  Cél1, Cél2).
törzsfelbontás( (Cél1->Cél2), Cél1, Cél2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Melyek a T alaptermben előforduló listák K hosszúságú perfixei? (15p)
%
% Mj.: A megoldások sorrendje lényegtelen.
%
% Előf: T alapterm, K nemnegatív egész szám.
%
% tKLp(T,K,Xs) :- Xs egy, a T term résztermjeként előforduló lista
%                 K hosszúságú prefixe.
%
% | ?- tKLp([],0,[]).
% yes
% | ?- tKLp( t([a([1,2,3],[a,b])],[c,d|e([f],[],[[g,h],i])]), 2, Xs ).
% Xs = [[g,h],i] ? ;    Xs = [g,h] ? ;    Xs = [a,b] ? ;
% Xs = [1,2] ? ;        Xs = [2,3] ? ;    no

% Mj.: Az Xs = [2,3] is helyes megoldás, mert az az [1,2,3] lista résztermje.
% Xs = [c,d] viszont nem megoldás, mert az nem szabályos lista prefixe.

% 1. megoldás:

tKLp(T,K,Xs) :-
    ( list(T), prefix(K,T,Xs)
    ; compound(T), functor(T,_F,N), ntKLp(N,T,K,Xs)
    ).

list([]).
list([_X|Xs]) :- list(Xs).

prefix(N,[X|Xs],[X|Ys]) :- N>0, N1 is N-1, prefix(N1,Xs,Ys).
prefix(0,_Xs,[]).

ntKLp(N,T,K,Xs) :- arg(N,T,A), tKLp(A,K,Xs).
ntKLp(N,T,K,Xs) :- N > 1, N1 is N-1, ntKLp(N1,T,K,Xs).

% 2. megoldás:

tKLp2(T,K,Xs) :-
    length(Xs,K), % Generál egy K hosszú listát, vagy ellenőrzi a listahosszt.
    tLp(T,Xs).

tLp(T,Xs) :- list(T), prefix(Xs,T).
tLp(T,Xs) :- compound(T), functor(T,_F,N), tLpN(N,T,Xs).

tLpN(N,T,Xs) :- arg(N,T,A), tLp(A,Xs).
tLpN(N,T,Xs) :- N > 1, N1 is N-1, tLpN(N1,T,Xs). 

prefix([],_Ys).
prefix([X|Xs], [X|Ys]) :- prefix(Xs,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				% 1. zh 2008.10.18.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2008_1018

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Osszunk ketté egy számlistát a pozitív és negatív elemei listáiba!
%
% Előf: Xs valódi számlista.
%
% list2pozneg(Xs,Ps,Ns) :- Xs pozitív elemei a Ps, míg negatív elemei
%     az Ns listában vannak, mindkettőben az Xs-beli sorrendnek megfelelően.
%
% | ?- list2pozneg([1,-3,-2,0.0,5,-4,2,6,0,4,-4],Ps,Ns).
% Ns = [-3,-2,-4,-4],
% Ps = [1,5,2,6,4]
% Megoldás:

list2pozneg([],[],[]).
list2pozneg([X|Xs],[X|Ps],Ns) :- X > 0, list2pozneg(Xs,Ps,Ns).
list2pozneg([X|Xs],Ps,[X|Ns]) :- X < 0, list2pozneg(Xs,Ps,Ns).
list2pozneg([X|Xs],Ps,Ns) :- X =:= 0, list2pozneg(Xs,Ps,Ns).

% Megoldás zöld vágókkal ('!' utasítások; ez a zh-ban nem volt feladat):

list2pozneg1([],[],[]).
list2pozneg1([X|Xs],XPs,Ns) :- X > 0, !, XPs = [X|Ps], list2pozneg1(Xs,Ps,Ns).
list2pozneg1([X|Xs],Ps,XNs) :- X < 0, !, XNs = [X|Ns], list2pozneg1(Xs,Ps,Ns).
list2pozneg1([X|Xs],Ps,Ns) :- X =:= 0, list2pozneg1(Xs,Ps,Ns).

% Megoldás feltételes céllal (ez sem volt feladat):

list2pozneg2([],[],[]).
list2pozneg2([X|Xs],Ps,Ns) :-
    ( X > 0 -> Ps = [X|Qs], list2pozneg2(Xs,Qs,Ns)
    ; X < 0 -> Ns = [X|Ms], list2pozneg2(Xs,Ps,Ms)
    ; list2pozneg2(Xs,Ps,Ns)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Melyek egy bináris fa belső csúcsai?
%
% Jelölés: üres fa: o (névkonstans); nem üres fa: t(Gyökér,BalRészfa,JobbRészfa)
%
% Előf: T valódi bináris fa.
%
% 'binfabelső'(T,X) :- X a T belső csúcsa, azaz nem levele.
%
% | ?- 'binfabelső'(t(1,t(2,o,t(3,o,o)),t(4,t(5,o,o),o)),X).
% X = 1 ? ;    X = 2 ? ;    X = 4 ? ;    no
% | ?- 'binfabelső'(o,X).
% no

% Megoldás:

'binfabelső'(t(X,B,J),X) :-
    'nemMindKettőÜresBinFa'(B,J).
'binfabelső'(t(_X,B,_J),Y) :- 'binfabelső'(B,Y).
'binfabelső'(t(_X,_B,J),Y) :- 'binfabelső'(J,Y).

'nemMindKettőÜresBinFa'(t(_,_,_),_).
'nemMindKettőÜresBinFa'(o,t(_,_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Adjuk össze külön-külön egy bináris számfa pozitív és negatív elemeit!
%
% Előf: T valódi bináris számfa.
%
% pnSumBinfa(T,P,N) :- P tartalmazza a T pozitív,
%                      míg N a negatív elemeinek összegét.
%
% | ?- pnSumBinfa(t(1,t(-2,o,t(-3,o,o)),t(4,t(0.0,o,o),o)),P,N).
% N = -5,    P = 5

% Naiv megoldás (zöld vágókkal kiegészítve):

pnSumBinfa(o,0,0).
pnSumBinfa(t(X,B,J),P,N) :-
    X > 0, !,
    pnSumBinfa(B,BP,BN), pnSumBinfa(J,JP,JN),
    P is BP+JP+X, N is BN+JN.
pnSumBinfa(t(X,B,J),P,N) :-
    X < 0, !,
    pnSumBinfa(B,BP,BN), pnSumBinfa(J,JP,JN),
    P is BP+JP, N is BN+JN+X.
pnSumBinfa(t(X,B,J),P,N) :-
    X =:= 0,
    pnSumBinfa(B,BP,BN), pnSumBinfa(J,JP,JN),
    P is BP+JP, N is BN+JN.

% Az  X=:=0  esetet nem lehet beolvasztani az előzők egyikébe sem, mert
% ha csak a  0.0  az egyetlen  float  a fában, akkor az összegeknek  integer
% típusuaknak kell lenniük, a feladat szövege szerint (egészek összege egész).

% Végrekurzív megoldás (nem volt feladat):

pnSumBinfa_(T,P,N) :- pnSumBinfa_r(T,0,0,P,N).

pnSumBinfa_r(o,P,N,P,N).
pnSumBinfa_r(t(X,B,J),P0,N0,P,N) :-
    X > 0, !,
    P1 is P0+X,
    pnSumBinfa_r(B,P1,N0,P2,N2),
    pnSumBinfa_r(J,P2,N2,P,N).
pnSumBinfa_r(t(X,B,J),P0,N0,P,N) :-
    X < 0, !,
    N1 is N0+X,
    pnSumBinfa_r(B,P0,N1,P2,N2),
    pnSumBinfa_r(J,P2,N2,P,N).
pnSumBinfa_r(t(X,B,J),P0,N0,P,N) :-
    X =:=0,
    pnSumBinfa_r(B,P0,N0,P2,N2),
    pnSumBinfa_r(J,P2,N2,P,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                           % 3. zh 2008.05.30.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<

% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2008_0530

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Def.: Egy általános fa listás ábrázolása a következő:
%       - Az üres (rész)fa a [].
%       - Egy N-áris nemüres részfa szerkezete: [Gyökér,RészFa1,...,RészFaN]
%       - Egy nemdefiniált (rész)fa reprezentációja
%         egy helyettesítetlen logikai változó, ami a fán belül egyedi.
%       Egy fa valódi <=> nincs definiálatlan részfája (és ő sem az).
%       Egy fa parciális <=> van definiálatlan részfája (vagy ő maga az).
% (Tehát a [] valódi fa; és egy valódi fának minden részfája valódi,
%  egy parciális fának viszont van parciális részfája (esetleg önmaga az).)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Melyek egy listás ábrázolású fa levelei? (15p)
%
% Előf.: T egy tetszőleges, listás ábrázolású fa.
% 
% listafa_levele(T,L) :- L a T levele.
%
% LI ~=< T mérete, azaz részfáinak száma; az összes megoldásra együtt.
%
% | ?- listafa_levele([1,[],A,[[],B,[],[Lev]],[2],[3,[lev]],[],[G,[[]]]],L).
% Lev = L ? ;     L = 2 ? ;     L = lev ? ;    L = [] ? ;    no

% Megoldás:

listafa_levele(T,L) :-
    nonvar(T),
    ( T = [Gy] -> L=Gy
    ; T = [_Gy|Részfák], member(F,Részfák), listafa_levele(F,L)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Töröljük egy listás ábrázolású fából az üres részfákat! (15p)
%
% Előf.: T\==[], különben tetszőleges, listás ábrázolású fa.
%        F helyettesítetlen logikai változó, ami nem szerepel T-ben.
%
% 'űrtelenít'(T,F) :- F listás ábrázolású fa, a T másolata, de F-ből hiányoznak
%     az üres részfák. A nemüres és a definiálatlan részfák sorrendje
%     változatlan. A hívás T-ben nem hajt végre változóhelyettesítést.
%
% LI ~=< T részfáinak száma.
%
% | ?- 'űrtelenít'([1,[],A,[[],B,[],[]],[2],[3,[],[]],[],[G,[[]]]],F).
% F = [1,A,[[],B],[2],[3],[G,[[]]]]

% Megoldás:

'űrtelenít'(T,F) :-
    ( var(T) -> F=T
    ; T = [Gy|Részfák], F=[Gy|ÚjRészfák], részfafeldolg(Részfák,ÚjRészfák)
    ).

részfafeldolg([],[]).
részfafeldolg([T|Ts],Fs) :-
    ( T==[] -> részfafeldolg(Ts,Fs)
    ; 'űrtelenít'(T,X), Fs = [X|Xs], részfafeldolg(Ts,Xs)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                         % 2. zh 2008.05.13.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<

% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2008_0513

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Gyűjtsük ki egy Prolog fájlból a diszjunktív szabályokat! (15p)
%
% Def.: Egy Prolog szabály akkor diszjunktív, ha törzse, direkt módon
%       vagy más programstruktúrá(k)ba ( (C1,C2) vagy (C1->C2) ) ágyazva,
%       diszjunkciót tartalmaz.
% Mj.: Ezt a definíciót természetesen a feladat egyszerüsítése végett
%      mondtuk ki így, azaz túl általánosan.
%
% Előf: F egy olvasható szövegfájl, ami Prolog mondatokat tartalmaz. 
%
% diszjunktív_szabályok(F,Rs) :- Rs valódi lista, az F-beli
%     diszjunktív szabályokat tartalmazza, az eredeti sorrendben.
%
% LI ~=< F mondatainak száma + F szabályai törzsében az összes
%        (C1,C2), (C1;C2) és (C1->C2) programkonstrukció száma.
%
/* Egy tesztfájl, a 'd.pl' tartalma:

g(X) :- e(X) ; p(X).
e([]). e([_|Xs]):-e(Xs).
p(X,Y) :- n(Y), ( s(X,Y) -> true ; s(Y,X) , n(X) )  , e(Y).
n(X) :- p(X,X) -> ( g(X) ; e(X) ) -> p(X).
p(X) :- n(X), e(X).

*/
% | ?- diszjunktív_szabályok('d.pl',Rs).
% Rs = [(g(_A):-e(_A);p(_A)),
%       (p(_B,_C):-n(_C),(s(_B,_C)->true;s(_C,_B),n(_B)),e(_C)),
%       (n(_D):-p(_D,_D)->(g(_D);e(_D))->p(_D))]

% Megoldás:

diszjunktív_szabályok(F,Rs) :- open(F,read,S), dsz(S,Rs), close(S).

dsz(S,Rs) :-
    read(S,M),
    ( M == end_of_file -> Rs = []
    ; compound(M), M = ( _Fej :- Törzs ), diszjunktív_törzs(Törzs) ->
          Rs = [M|Ms], dsz(S,Ms)
    ; dsz(S,Rs)
    ).
% A  compound(M) feltétel a fenti előfeltételek mellett nem szükséges.
% Az egyetlen változót tartalmazó (helytelen) Prolog mondatokat mindenesetre
% kiszűri (és akkor, ugyanannyi erővel már a tényállítások egy részét is).

diszjunktív_törzs(T) :-
    compound(T),
    ( T = ( _Cél1 ; _Cél2 ) -> true
    ; egyéb_törzsfelbontás(T,Cél1,Cél2),
      ( diszjunktív_törzs(Cél1) -> true
      ; diszjunktív_törzs(Cél2)
      )
    ).
% A  compound(T) feltétel arra szolgál, hogy a formailag egyetlen logikai
% változóból álló Prolog célokat kiszűrje (és akkor, ugyanannyi erővel már az
% atomokat is), mint pl. a "first_solution(P) :- ( P -> true )." klózban.

egyéb_törzsfelbontás( (Cél1,Cél2),  Cél1, Cél2).
egyéb_törzsfelbontás( (Cél1->Cél2), Cél1, Cél2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Melyek egy Prolog term K aritású függvényszimbólumai? (15p)
%
% Mj.: A konstansokat nulla aritású függvényszimbólumoknak tekintjük.
%
% Előf: integer(K), K >= 0.
%
% fvszimb(T,K,F) :- a T term tartalmazza a K aritású,
%                   F nevű függvényszimbólumot.
%
% LI ~=< T mérete, azaz résztermjeinek száma (rekurzívan).
%
% | ?- fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), 0, F ).
% F = e ? ;    F = 1 ? ;    F = b ? ;    no
% | ?- fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), 1, F ).
% F = f ? ;    F = a ? ;    no
% | ?- fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), 2, F ).
% F = t ? ;    F = c ? ;    F = g ? ;    no
% | ?- fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), 3, F ).
% F = d ? ;    no
% | ?- fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), 4, F ).
% no

% Megoldás:

fvszimb(T,K,F) :-
    ( atomic(T) -> K = 0, F = T 
    ; compound(T), functor(T,Fun,N),
      ( N = K, F = Fun
      ; fvszim(N,T,K,F)
      )
    ).

fvszim(N,T,K,F) :- arg(N,T,A), fvszimb(A,K,F).
fvszim(N,T,K,F) :- N > 0, N1 is N-1, fvszim(N1,T,K,F).

% Mj.: A fenti (integer(K),K>=0) előfeltétel könnyítés volt; az itt 
%      adott megoldásra nem vonatkozik, mint az alábbi teszt is mutatja.
%
% | ?- fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), K, F ).
% F = t, K = 2 ? ;    F = c, K = 2 ? ;    F = d, K = 3 ? ;
% F = g, K = 2 ? ;    F = f, K = 1 ? ;    F = e, K = 0 ? ;
% F = 1, K = 0 ? ;    F = a, K = 1 ? ;    F = b, K = 0 ? ;
% no
% | ?- findall( F/K, fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), K, F ), Fs).
% Fs = [t/2,c/2,d/3,g/2,f/1,e/0,1/0,a/1,b/0]
% | ?- fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), -1, F ).
% no
% | ?- fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), 3.5, F ).
% no
% | ?- fvszimb( t(a(b),c(1,d(e,f(X),g(H,I)))), alma, F ).
% no

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                          % 1. zh 2008.03.18.

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\inf.elte.hu\dfs\zh ; másol ebbe:  plzh\2008_0318

% A predikátumokban "zöld" vágó(azaz:!) utasításokat helyeztem el, 
% ami a zh-ban nem volt feladat, a szemantikát nem befolyásolja,
% és optimalizációs célokat szolgál.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Osszuk szét egy lista elemeit felváltva háromfelé!
%
% Előf: As valódi lista.
%
% háromfelé(As,Xs,Ys,Zs) :- As-nek azok az elemei, amelyek sorszáma 3*k+1 alakú,
%     az Xs listában, amelyek sorszáma 3*k+2 alakú, az Ys listában,
%     a hárommal osztható sorszámúak pedig a Zs listában vannak,
%     mindháromban az As-beli sorrendnek megfelelően.
%
% | ?- háromfelé([],Xs,Ys,Zs).
% Xs = [],  Ys = [],  Zs = []
% | ?- háromfelé([a,b,c,d,e,f,g,h],Xs,Ys,Zs).
% Xs = [a,d,g],  Ys = [b,e,h],  Zs = [c,f]
% | ?- háromfelé([A,B,C,D],Xs,Ys,Zs).
% Xs = [A,D],  Ys = [B],  Zs = [C]

% Naiv megoldás:

háromfelé_([A,B,C|As],[A|Xs],[B|Ys],[C|Zs]) :- !, háromfelé_(As,Xs,Ys,Zs).
háromfelé_([A,B],[A],[B],[]) :- !.
háromfelé_([A],[A],[],[]).
háromfelé_([],[],[],[]).

% Kicsit elegánsabban:

háromfelé([],[],[],[]).
háromfelé([A|As],[A|Xs],Ys,Zs) :- háromfelé(As,Ys,Zs,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Melyek egy bináris fa levelei?
%
% Jelölés: üres fa: o (névkonstans); nem üres fa: t(Gyökér,BalRészfa,JobbRészfa)
%
% Előf: T valódi bináris fa.
%
% binfalevél(T,X) :- X a T levele.
%
% | ?- binfalevél(t(1,t(2,o,t(3,o,o)),t(4,o,o)),X).
% X = 3 ? ;    X = 4 ? ;    no
% | ?- binfalevél(o,X).
% no

% Megoldás:

binfalevél(t(X,o,o),X) :- !.
binfalevél(t(_X,B,_J),X) :- binfalevél(B,X).
binfalevél(t(_X,_B,J),X) :- binfalevél(J,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Adjuk össze külön-külön egy számlista pozitív és negatív elemeit!
%
% Előf: Xs valódi számlista.
%
% poznegsum(Xs,P,N) :- P tartalmazza az Xs pozitív,
%                      míg N a negatív elemeinek összegét.
%
% | ?- poznegsum([],P,N).
% N = 0,  P = 0
% | ?- poznegsum([1,-3,-2,5,-4,2,6,0,4,-4],P,N).
% N = -13,  P = 18
% | ?- poznegsum([-1,-3,-2,5,-4,2,6,0.0,4,4],P,N).
% N = -10,  P = 21
% | ?- poznegsum([0.0],P,N).
% N = 0,   P = 0

% Naiv megoldás:

poznegsum_([],0,0).
poznegsum_([X|Xs],P,N) :- X>0, !, poznegsum_(Xs,P0,N), P is P0+X.
poznegsum_([X|Xs],P,N) :- X<0, !, poznegsum_(Xs,P,N0), N is N0+X.
poznegsum_([X|Xs],P,N) :- X=:=0, poznegsum_(Xs,P,N).

% Végrekurzív megoldás:

poznegsum(Xs,P,N) :- poznegsum(Xs,0,0,P,N).

poznegsum([],P,N,P,N).
poznegsum([X|Xs],P0,N0,P,N) :- X>0, !, P1 is P0+X, poznegsum(Xs,P1,N0,P,N).
poznegsum([X|Xs],P0,N0,P,N) :- X<0, !, N1 is N0+X, poznegsum(Xs,P0,N1,P,N).
poznegsum([X|Xs],P0,N0,P,N) :- X=:=0, poznegsum(Xs,P0,N0,P,N).

% Az X=:=0 esetet azért vettem külön, mert pl. ha a nemnulla adatok csupa
% egészek, de van az input listán egy 0.0, akkor a feladat szövege szerint
% P és N egész értékű kell legyen, hiszen egészek összege egész.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		  % 3. Prolog zh, 2008. január 4.

% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% A megadott előfeltételekkel a vonatkozó célok keresési fája véges legyen!
% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel. 
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Def: Egy atomfa csúcsai Prolog atomok (névkonstansok). Egy egyelemű 
%      atomfa (levélcsúcs) reprezentációja a megfelelő Prolog atom.
%      Egy többelemű atomfát egy összetett Prolog term reprezentál.
%      A term függvényszimbólumának neve a gyökércsúcs, aritása a csúcs
%      gyermekeinek száma, paraméterei a részfák.
%
% Mj: Üres (rész)fákat -- ebben a zh-ban -- nem ábrázolunk.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Melyek egy atomfa levelei?
%
% Előf: Fa egy nemüres, valódi atomfa.
%
% atomfa_levél(Fa,Levél) :- Levél a Fa atomfa egy levele.
%
% LI ~=< Fa csúcsainak száma (az összes megoldásra).
%
% | ?- atomfa_levél( a(b,c(d,e),f(g(h))), Levél ).
% Levél = h ? ;    Levél = e ? ;    Levél = d ? ;    Levél = b ? ;    no
% | ?- atomfa_levél( a, Levél ).
% Levél = a ? ;    no

atomfa_levél(Fa,Levél) :-
    ( atom(Fa) -> Levél = Fa
    ; compound(Fa),
      functor(Fa,_Gyökér,LeágSzám),
      részfa(LeágSzám,Fa,Részfa),
      atomfa_levél(Részfa,Levél)
    ).

részfa(LeágSzám,Fa,Részfa) :-
    arg(LeágSzám,Fa,Részfa).
részfa(LeágSzám,Fa,Részfa) :- 
    LeágSzám > 1, LeágSzám1 is LeágSzám-1, részfa(LeágSzám1,Fa,Részfa).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Határozzuk meg egy atomfa frontját!
%
% Előf: true
%
% atomfa_frontja(Fa,Front) :- Front a Fa, valódi atomfa frontja,
%                        azaz leveleinek listája, balról jobbra.
%
% Mj: A megoldáshoz ne használjunk logikán kívüli (pl. assert/1)
%     ill. megoldás összegyűjtő (pl. findall/3) predikátumokat!
%
% Mj: Ha Fa nem egy valódi, nemüres atomfa, hiúsuljon meg az
%     atomfa_frontja(Fa,Front) hívás!
%
% LI ~=< Fa csúcsainak száma.
%
% | ?- atomfa_frontja(a,Levelek).
% Levelek = [a]
% | ?- atomfa_frontja(a(b,c(d,e),f(g(h))),Levelek).
% Levelek = [b,d,e,h]
% | ?- atomfa_frontja(a(b,c(D,e),f(g(h))),Levelek).
% no

atomfa_frontja(Fa,Levelek) :- atomfa_frontja(Fa,[],Levelek).
    
atomfa_frontja(Fa,FrontEddig,Levelek) :-
    ( atom(Fa) -> Levelek = [Fa|FrontEddig]
    ; compound(Fa),
      functor(Fa,_,LeágSzám),
      atomfa_levelei(LeágSzám,Fa,FrontEddig,Levelek)
    ).

atomfa_levelei(LeágSzám,Fa,FrontEddig,Levelek) :-
    (
      LeágSzám > 0
    ->
      arg(LeágSzám,Fa,Részfa),
      atomfa_frontja(Részfa,FrontEddig,ÚjFront),
      LeágSzám1 is LeágSzám - 1,
      atomfa_levelei(LeágSzám1,Fa,ÚjFront,Levelek)
    ;
      Levelek = FrontEddig
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                 % 2. Prolog zh, 2007. december 10.

% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% A megadott előfeltételekkel a vonatkozó célok keresési fája véges legyen!
% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel. 
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<  
% Beadás: futtat: \\ads2.inf.elte.hu\zh ; másol ebbe: plzh\2007dec10

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Def: Egy atomfa csúcsai Prolog atomok (névkonstansok). Egy egyelemű 
%      atomfa (levélcsúcs) reprezentációja a megfelelő Prolog atom.
%      Egy többelemű atomfát egy összetett Prolog term reprezentál.
%      A term függvényszimbólumának neve a gyökércsúcs, aritása a csúcs
%      gyermekeinek száma, paraméterei a részfák.
%
% Def: Az éleket  él(FaGyökere,RészfaGyökere)  alakú termekkel ábrázoljuk.
%
% Mj: Üres (rész)fákat -- ebben a zh-ban -- nem ábrázolunk.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Melyek egy atomfa élei?
%
% Előf: Fa egy valódi atomfa.
%
% atomfa_él(Fa,Él) :- Él a Fa atomfa egy éle.
%
% LI ~=< Fa csúcsainak száma (az összes megoldásra).
%
% | ?- atomfa_él( a(b,c(d,e),f(g)), Él).
% Él = él(a,f) ? ;    Él = él(f,g) ? ;    Él = él(a,c) ? ;
% Él = él(c,e) ? ;    Él = él(c,d) ? ;    Él = él(a,b) ? ;    no

atomfa_él(Fa,Él) :-
    gyökér(Fa,Gy), részfa(Fa,Részfa),
    ( gyökér(Részfa,RGy), Él = él(Gy,RGy) ; atomfa_él(Részfa,Él) ).

gyökér(Fa,Gy) :- functor(Fa,Gy,_).

részfa(Fa,Részfa) :-
    functor(Fa,_,LeágSzám), LeágSzám > 0, részfa_(LeágSzám,Fa,Részfa).

részfa_(LeágSzám,Fa,Részfa) :-
    arg(LeágSzám,Fa,Részfa).
részfa_(LeágSzám,Fa,Részfa) :- 
    LeágSzám > 1, LeágSzám1 is LeágSzám-1, részfa_(LeágSzám1,Fa,Részfa).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Határozzuk meg egy atomfa éleinek a listáját preorder bejárás szerint!
%
% Előf: Fa egy valódi atomfa.
%
% atomfa_élek(Fa,Élek) :- Élek a Fa éleinek listája,
%     a Fa preorder bejárásának megfelelő sorrendben.
%
% Mj: A preorder bejárásban a részfákba mutató élek
%     a részfák bejárásának sorrendjében következnek egymás után.
%
% LI ~=< Fa csúcsainak száma.
%
% | ?- atomfa_élek( a(b,c(d,e),f(g)), Élek ).
% Élek = [él(a,b),él(a,c),él(c,d),él(c,e),él(a,f),él(f,g)]
%
% | ?- atomfa_élek( a, Élek ).
% Élek = []
%
% | ?- atomfa_élek( a(b(c(d,e),g(h(i))),j(k(l(m)))), Élek ).
% Élek = [él(a,b),él(b,c),él(c,d),él(c,e),él(b,g),él(g,h),él(h,i),
%         él(a,j),él(j,k),él(k,l),él(l,m)]

atomfa_élek(Fa,Élek) :-
    functor(Fa,Gy,LeágSzám),
    atomfa_élei(LeágSzám,Fa,Gy,[],Élek).

atomfa_élei(LeágSzám,Fa,Gy,Élek0,Élek) :-
    (
      LeágSzám > 0
    ->
      arg(LeágSzám,Fa,RészFa), functor(RészFa,RészGy,RészLeágSzám),
      atomfa_élei(RészLeágSzám,RészFa,RészGy,Élek0,Élek1),
      LeágSzám1 is LeágSzám - 1,
      atomfa_élei(LeágSzám1,Fa,Gy,[él(Gy,RészGy)|Élek1],Élek)
    ;
      Élek = Élek0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    % 1. Prolog zh, 2007. október 15. 

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A megoldások során minden predikátumot definiálni kell, a SICStus Prolog
% beépített eljárásai kivételével. (A könyvtári predikátumok NEM beépített
% eljárások.) 
% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel.
% Beadás: futtat: \\ads2.inf.elte.hu\zh ; másol ebbe:  plzh\2007okt15\ 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. A család/3 alapján adjuk meg a féltestvére/2 relációt!
%
% Előf: adottak  család(Anya,Apa,Gyerekek)  alakú tényállítások, ahol
%   Anya és Apa névkonstansok, a Gyerekek pedig névkonstansok valódi listája.
%
% féltestvére(X,Y) :- X az Y féltestvére a család/3 predikátum szerint,
%                        azaz egyik szülőjük közös, de a másik nem.
%
% Például:
család(p,m,[a,b]).     család(q,n,[c,d,e]).
család(p,k,[f,g]).     család(q,m,[h]).
%
% | ?- féltestvére(X,Y).
% X = a, Y = f ? ; X = a, Y = g ? ; X = b, Y = f ? ; X = b, Y = g ? ;
% X = a, Y = h ? ; X = b, Y = h ? ; X = c, Y = h ? ; X = d, Y = h ? ;
% X = e, Y = h ? <ENTER>  yes % ugyanezek jönnének fordított párokkal.

féltestvére(X,Y) :-
    család(A,B,Gs), család(C,D,Hs),
    vegyes_párok(A,B,C,D),
    tagja(X,Gs), tagja(Y,Hs).

vegyes_párok(A,B,A,D) :- B \= D, !.
vegyes_párok(A,B,C,B) :- A \= C.

% tagja(X,Xs):- X eleme az Xs listának.
tagja(X,[X|_Xs]).
tagja(X,[_X|Xs]) :- tagja(X,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Kik adott szülő gyerekei?
% 
% Előf: Családok valódi lista. Elemei cs(Anya,Apa,Gyerekek) alakú termek, ahol
%   Anya és Apa névkonstansok, Gyerekek pedig névkonstansok valódi listája.
%   Szülöö adott névkonstans.
%
% gyerekei(Családok,Szülöö,Gyermekei) :- Szülöö gyermekeit a Gyermekei lista
%     reprezentálja a Családok listának megfelelő sorrendben.
%
% | ?- gyerekei([cs(p,m,[a,b]),cs(q,n,[c,d,e]),cs(p,k,[]),cs(q,m,[h])],p,Gs).
% Gs = [a,b]
% | ?- gyerekei([cs(p,m,[a,b]),cs(q,n,[c,d,e]),cs(p,k,[]),cs(q,m,[h])],m,Gs).
% Gs = [a,b,h] 
% | ?- gyerekei([cs(p,m,[a,b]),cs(q,n,[c,d,e]),cs(p,k,[]),cs(q,m,[h])],q,Gs).
% Gs = [c,d,e,h]
% | ?- gyerekei([cs(p,m,[a,b]),cs(q,n,[c,d,e]),cs(p,k,[]),cs(q,m,[h])],k,Gs).
% Gs = []
% | ?- gyerekei([cs(p,m,[a,b]),cs(q,n,[c,d,e]),cs(p,k,[]),cs(q,m,[h])],n,Gs).
% Gs = [c,d,e]
% | ?- gyerekei([cs(p,m,[a,b]),cs(q,n,[c,d,e]),cs(p,k,[]),cs(q,m,[h])],r,Gs).
% Gs = []

% Naiv megoldás:
gyerekei_naiv([],_Szülöö,[]).
gyerekei_naiv([cs(Anya,Apa,Gyerekek)|Családok],Szülöö,Gyermekei) :-
    ( anya_vagy_apa(Szülöö,Anya,Apa) ->
      gyerekei_naiv(Családok,Szülöö,TöbbiGyereke),
      'összefűz'(Gyerekek,TöbbiGyereke,Gyermekei)
    ; gyerekei_naiv(Családok,Szülöö,Gyermekei)
    ).
    
anya_vagy_apa(Anya,Anya,_Apa).
anya_vagy_apa(Apa,_Anya,Apa).

'összefűz'([],Ys,Ys).
'összefűz'([X|Xs],Ys,[X|Zs]) :- 'összefűz'(Xs,Ys,Zs).

%% 1. végrekurzív megoldás:

gyerekei([],_Szülöö,[]).
gyerekei([cs(Anya,Apa,Gyerekek)|Családok],Szülöö,Gyermekei) :-
    ( anya_vagy_apa(Szülöö,Anya,Apa) ->
      append(Gyerekek,TöbbiGyereke,Gyermekei),
      gyerekei(Családok,Szülöö,TöbbiGyereke)
    ; gyerekei(Családok,Szülöö,Gyermekei)
    ).


%% 2. végrekurzív megoldás:
gyerekei2([],_Szülöö,[]).
gyerekei2([cs(Anya,Apa,Gyerekek)|Családok],Szülöö,Gyermekei) :-
    ( anya_vagy_apa(Szülöö,Anya,Apa) ->
      gyermekei_(Gyerekek,Gyermekei,TöbbiGyereke),
      gyerekei2(Családok,Szülöö,TöbbiGyereke)
    ; gyerekei2(Családok,Szülöö,Gyermekei)
    ).

gyermekei_([],TöbbiGyereke,TöbbiGyereke).
gyermekei_([Gyerek|Gyerekek],[Gyerek|Gyermekei],TöbbiGyereke) :-
    gyermekei_(Gyerekek,Gyermekei,TöbbiGyereke).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                 % 3. Prolog zh, 2007. június 5.

% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% A megadott előfeltételekkel a vonatkozó célok keresési fája véges legyen!
% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel. 
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Melyek egy lista, pozitív számmal kezdődő olyan suffix-ei,
%    amiket közvetlenül nulla előz meg?
% Előf: Xs valódi lista.
% pozKezdSuffix(Xs,Ys) :- Ys az Xs egy nemüres, pozitív számmal kezdődő
%     suffix-e, amit közvetlenül nulla előz meg.
% LI ~=< Xs hossza.
% 
% | ?- pozKezdSuffix([0,1,b,0,2,0.0,1,2,0,a,f(x),2.2,3,-1,4],Ys).
%                 Ys = [1,b,0,2,0.0,1,2,0,a,f(x),2.2,3,-1,4] ? ;
%                       Ys = [2,0.0,1,2,0,a,f(x),2.2,3,-1,4] ? ;
%                             Ys = [1,2,0,a,f(x),2.2,3,-1,4] ? ;    no

pozKezdSuffix([X,Y|Xs],[Y|Xs]) :- number(X), number(Y), X =:= 0, Y>0.
pozKezdSuffix([_X|Xs],Ys) :- pozKezdSuffix(Xs,Ys).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Melyek egy lista pozitív szigetei?
% Def: Ys valódi lista az Xs valódi lista pozitív szigete, ha
%      Ys nemüres, Ys minden eleme pozitív szám, Ys az Xs egy valódi,
%      Xs-ben folyamatosan elhelyezkedő része és az Xs-ben az Ys-t közvetlenül
%      megelőzi és követi egy-egy nulla számérték. 
% Előf: Xs valódi lista.
% pozSziget(Xs,Ys) :- Ys valódi lista az Xs pozitív szigete.
% LI ~=< Xs hossza.
%
% | ?- pozSziget([0,1,b,0,2,0.0,1,2,0,a,f(x),2.2,3,-1,4],Ys).
% Ys = [2] ? ;    Ys = [1,2] ? ;     no
% | ?- pozSziget([5,4,0.0,1,2,0,0,2.2,3,-1,0,6,0,0,7],Ys).
% Ys = [1,2] ? ;    Ys = [6] ? ;    no

pozSziget(Xs,Ys) :- pozKezdSuffix(Xs,Zs), maxPozPrefix(Zs,Ys).

maxPozPrefix([Y,Z|Zs],Ys) :-
    number(Z),
    ( Z=:=0 -> Ys = [Y]
    ; Z > 0, Ys = [Y|Us], maxPozPrefix([Z|Zs],Us)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Irassuk ki egy számlista pozitív szigeteit a standard outputra!
% Előf: Xs valódi számlista.
% pozSzigetKiíró(Xs) : Xs pozitív szigeteit sorban kiírja a standard outputra,
%     minden szigetet új sorba, és az utolsó után egy üres sort hagy.
% LI ~=< Xs hossza.
%
% | ?- pozSzigetKiíró([0,1,b,0,2,0.0,1,2,0,a,f(x),2.2,3,-1,4]).
% [2]
% [1,2]
% 
% yes
% | ?- pozSzigetKiíró([5,4,0.0,1,2,0,0,2.2,3,-1,0,6,0,0,7]).
% [1,2]
% [6]
% 
% yes

pozSzigetKiíró(Xs) :- pozSziget(Xs,Ys), write(user,Ys), nl(user), fail.
pozSzigetKiíró(_Xs) :- nl(user).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                 % 2. Prolog zh, 2007. május 16.

% A célok futása ne hagyjon felesleges választási pontokat!
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt!
% A megadott előfeltételekkel a vonatkozó célok keresési fája véges legyen!
% A beadandó fájl neve: x.pl, ahol x az eha kód, '.elte' nélkül, kisbetűkkel. 
% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<  
% Beadás: futtat: \\ads2.inf.elte.hu\zh ; másol ebbe: plzh2007majus16

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Cseréljük le egy atom másolatában az ékezetes betűket ékezet nélküliekre!
% Előf: A egy Prolog atom (névkonstans).
%       Feltehető, hogy csak magyar ékezetes betűket tartalmaz.
% atomot_éktelenít(A,B) :- A másolata a B atom, de B-ben az ékezetes betűket a
%                          megfelelő ékezet nélküli betűkkel helyettesítettük.
% LI ~=< A atom karaktereinek száma.
%
% | ?- atomot_éktelenít('aábEÉGIíóPÖőüŰú_ÁéfÍÓöŐpÚÜű',B).
%                   B = aabEEGIioPOouUu_AefIOoOpUUu

atomot_éktelenít(A,B) :-
    atom_codes(A,Cs), ékek(Cs,Ds), atom_codes(B_,Ds), B = B_.

ékek([],[]).
ékek([C|Cs],[D|Ds]) :- karaktert_éktelenít(C,D), ékek(Cs,Ds).

karaktert_éktelenít(C,D) :-
    ( ée(C,C1) -> D = C1    % output illesztés a (lokális) vágó után
    ; D = C
    ).

ée(0'á,0'a).    ée(0'é,0'e).    ée(0'í,0'i).    
ée(0'ó,0'o).    ée(0'ö,0'o).    ée(0'ő,0'o).    
ée(0'ú,0'u).    ée(0'ü,0'u).    ée(0'ű,0'u).
ée(0'Á,0'A).    ée(0'É,0'E).    ée(0'Í,0'I).    
ée(0'Ó,0'O).    ée(0'Ö,0'O).    ée(0'Ő,0'O).    
ée(0'Ú,0'U).    ée(0'Ü,0'U).    ée(0'Ű,0'U).
% ... és az esetleges automatikus karakterkonverzió miatt:
ée(0'õ,0'o).   ée(0'û,0'u).   ée(0'Õ,0'O).   ée(0'Û,0'U).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Cseréljük le egy term másolatában az ékezetes betűket ékezet nélküliekre!
% Előf: A egy Prolog term, magyar ékezetes betűkkel.
% termet_éktelenít(A,B) :- A másolata a B term, de B-ben az ékezetes betűket 
%     az atomokban és az összetett termek függvényszimbólumaiban a megfelelő
%     ékezet nélküli betűkkel helyettesítettük.
% LI ~=< az A term mérete + atomjainak és függvényszimbólum neveinek összhossza
%
% | ?- termet_éktelenít(a(á,bEÉGI,2,í(óP,Ö,'őüŰ',ú_Á(éfÍ,Óö),'pÚÜű')),B).
%                         B = a(a,bEEGI,2,i(oP,Ö,ouU,u_A(efI,Óö),pUUu))

termet_éktelenít(A,B) :-
    ( atom(A) -> atomot_éktelenít(A,B)
    ; compound(A) -> functor(A,F,N), atomot_éktelenít(F,G), functor(B,G,N),
                     paramétereket_éktelenít(N,A,B)
    ; B = A
    ).

paramétereket_éktelenít(N,A,B) :-
    ( N > 0 -> arg(N,A,P), termet_éktelenít(P,Q), arg(N,B,Q),
               N1 is N-1, paramétereket_éktelenít(N1,A,B)
    ; true
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    % 1. Prolog zh, 2007. március 28. 

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A megoldások során minden predikátumot definiálni kell, a SICStus Prolog
% beépített eljárásai kivételével. (A könyvtári predikátumok NEM beépített
% eljárások.)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  1. Hányszor fordul elő egy listán a legnagyobb egész szám?

% Előf.: Xs valódi lista.
% maxegészek(Xs,N) :- Xs legnagyobb, egész típusú eleme N-szer fordul elő.

% Mj.: Lehetőleg csak egyszer menjünk végig a listán!
%   Ha nincs egész szám az Xs listán, hiúsuljon meg a  maxegészek(Xs,N)  cél!

% | ?- maxegészek([a,2,f(X),1,2,r,4,5.1,4,4,End],N).
% N = 3
% | ?- maxegészek([2.2,f(X),r,4.0,5.1,4.4,End],N).
% no

maxegészek([X|Xs],MaxEgészHányszor) :-
    ( integer(X) -> maxegek(Xs,X,1,MaxEgészHányszor)
    ; maxegészek(Xs,MaxEgészHányszor)
    ).

maxegek([],_M,N,N).
maxegek([X|Xs],M,N,NN) :-
    ( integer(X), X>M -> maxegek(Xs,X,1,NN)
    ; X==M -> N1 is N+1, maxegek(Xs,M,N1,NN)
    ; maxegek(Xs,M,N,NN)
    ).

% 2. mo

maxegészek_(Xs,MaxEgészHányszor) :-
    maxints(Xs,_,0,MaxEgészHányszor), MaxEgészHányszor > 0.

maxints([],_,MaxEgészHányszor,MaxEgészHányszor).
maxints([X|Xs],MaxEddig,HányEddig,MaxEgészHányszor) :-
    ( integer(X), X @> MaxEddig -> maxints(Xs,X,1,MaxEgészHányszor)
    ; X == MaxEddig ->
          HányEddig_1 is HányEddig + 1,
          maxints(Xs,MaxEddig,HányEddig_1,MaxEgészHányszor)
    ; maxints(Xs,MaxEddig,HányEddig,MaxEgészHányszor)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Egy számlistán mely pozíciókban vannak pozitív számok?

% Előf.: Xs valódi számlista.
% szárazföld(Xs,N) :- Xs N-edik eleme pozitív szám.

% Mj.: Lehetőleg ne generáljunk compound terme(ke)t!

% | ?- szárazföld([2,0,3,0.1,5,0,-1,1,0.0,-0.1],N).
% N = 1 ? ;    N = 3 ? ;    N = 4 ? ;    N = 5 ? ;    N = 8 ? ;    no

szárazföld(Xs,Poz) :- szárazföld(Xs,1,Poz).

szárazföld([X|_Xs],PX,PX) :- X > 0.
szárazföld([_X|Xs],PX,P) :- P1 is PX+1, szárazföld(Xs,P1,P).

%2. mo.
szárazföld_(Xs,Poz) :- szárazföld_(Xs,1,Poz).

szárazföld_([X|Xs],PX,P) :-    
    ( X > 0, P=PX
    ; P1 is PX+1, szárazföld_(Xs,P1,P)
    ).

%3. mo. (Ez a legegyszerűbb, de ez végzi a legtöbb összeadást.)
szárazföld_3([X|Xs],N):-
    ( X > 0, N=1
    ; szárazföld_3(Xs,N1), N is N1+1 
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Gyűjtsük ki egy számlistáról a pozitív számok pozícióit!

% Előf.: Xs valódi számlista.
% szárazföldek(Xs,Ps) :-
%     Ps az Xs pozitív elemeinek sorszámait tartalmazza, növekvő sorrendben.

% Mj.: Ne használjunk megoldásokat összegyűjtő eljárásokat, mint a findall/3!

% | ?- szárazföldek([2,0,3,0.1,5,0,-1,1,0.0,-0.1],Ps).
% Ps = [1,3,4,5,8]

szárazföldek(Xs,Ps) :- szárazföldek(Xs,1,Ps).

szárazföldek([],_PX,[]).
szárazföldek([X|Xs],PX,Ps) :-
    P1 is PX+1,
    ( X > 0 -> Ps=[PX|Qs], szárazföldek(Xs,P1,Qs)
    ; szárazföldek(Xs,P1,Ps)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                 % 2. Prolog zh, 2006. december 5.

% A célok futása ne hagyjon felesleges választási pontokat. 
% Használjuk ki az első arg. indexelést és az utolsó hívás optimalizációt.
% A megadott előfeltételekkel a vonatkozó célok keresési fája véges legyen.

% LI: A program futtatásához szükséges predikátumhívások száma
%   (LI: "logical inference"). Aszimptotikusan kisebb-egyenlő: ~=<  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Valósítsuk meg az atomok konkatenációját az atom_concat/3 mintájára!
% %! 1. feladat: 10 p
%
% Előf: A1 és A2, vagy A12 atom.
%
% atomConcat(A1,A2,A12) :- az A1 atom karakterei az A2 atom karaktereivel
%                        összefűzve sorban  az A12 atom karaktereit adják.
% Mj.: A megoldás során a SICStus predikátumai közül csak az  atom/1,
%      az  atom_codes/2  és a  throw/1  használható fel.
%
% | ?- atomConcat(A,B,abc), atomConcat(A,B,AB).
% A = '',  B = abc,  AB = abc ? ;    A = a,  B = bc,  AB = abc ? ;
% A = ab,  B = c,  AB = abc ? ;    A = abc,  B = '',  AB = abc ? ;
% no

% Egyszerű megoldás:

atomConcat(A1,A2,A12) :-
    ( atom(A1), atom(A2) ->
          atom_codes(A1,Cs), atom_codes(A2,Ds),
          listConcat(Cs,Ds,Es), atom_codes(A12,Es)
    ; atom(A12) ->
          atom_codes(A12,Es), listConcat(Cs,Ds,Es),
          atom_codes(A1,Cs), atom_codes(A2,Ds)
    ).

listConcat([],Ys,Ys).
listConcat([X|Xs],Ys,[X|Zs]) :- listConcat(Xs,Ys,Zs).

% Javított megoldás: Ha a megoldás determinisztikus, akkor a kód is az.

atom_Concat(A1,A2,A12) :-
    ( atom(A1), atom(A2) ->
          atom_codes(A1,Cs), atom_codes(A2,Ds),
          listConcat(Cs,Ds,Es), atom_codes(A12,Es)
    ; atom(A12) -> 
          atom_codes(A12,Es),
          ( atom(A1) ->
	        atom_codes(A1,Cs), listConcat(Cs,Ds,Es), atom_codes(A2,Ds)
	  ; atom(A2) ->
	        atom_codes(A2,Ds),
	        ( listConcat(Cs,Ds,Es) -> atom_codes(A1,Cs) )
	  ; var(A1), var(A2),
	    listConcat(Cs,Ds,Es),
	    atom_codes(A1,Cs), atom_codes(A2,Ds)
	  )
    ; throw(type_error(atom_Concat(A1,A2,A12)))
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Melyek egy Prolog program, adott predikátum hívásra   %! 2.fa: 20p
%    első arg. indexelés szerint illeszkedő klózai?
%
% Előf: F egy Prolog fájl, ami csak standard Prolog tényeket és szabályokat
%       tartalmaz. Feltehető, hogy szintaktikusan helyes.
%       P egy Prolog predikátum hívás (tehát a típusa atom vagy compound).
%
% indexLista(F,P,Cs) :- a  Cs  klózlista az  F  fájlban lévő klózok egy
%     részsorozatát tartalmazza. Pontosan azokat, amelyek a  P
%     predikátumhívásban az első argumentum indexelés szerint szóbajöhetnek.
%
% LI ~=< Az F klózainak száma.
%
% | ?- use_module(library(system),[]).    ...    yes
% | ?- system:shell('cat x.pl').
% párok('A',11).  párok('B',13).
% list([]). list([_|Xs]):-list(Xs).
% párok('A',10).
% list(Xs):-var(Xs).
% alma.  alma :- alma.
% 
%
% | ?- indexLista('x.pl',párok('A',2),Cs).
% Cs = [párok('A',11),párok('A',10)]
% | ?- indexLista('x.pl',párok('B',2),Cs).
% Cs = [párok('B',13)]
% | ?- indexLista('x.pl',párok(A,2),Cs).
% Cs = [párok('A',11),párok('B',13),párok('A',10)]
% | ?- indexLista('x.pl',párok('A'(1),2),Cs).
% Cs = []
% | ?- indexLista('x.pl',párok(_),Cs).
% Cs = []
% | ?- indexLista('x.pl',list([]),Cs).
% Cs = [list([]),(list(_A):-var(_A))]
% | ?- indexLista('x.pl',list([1]),Cs).
% Cs = [(list([_A|_B]):-list(_B)),(list(_C):-var(_C))]
% | ?- indexLista('x.pl',list(_),Cs).
% Cs = [list([]),(list([_A|_B]):-list(_B)),(list(_C):-var(_C))]
% | ?- indexLista('x.pl',alma,Cs).
% Cs = [alma,(alma:-alma)]

indexLista(F,P,Cs) :- %! 2p
    open(F,read,S), 
    index1_lista(S,P,Cs),
    close(S).

% index1_lista(S,P,Cs) :- az  S  input stream, első arg. indexelés szerint a  P
%                         predikátumhívásra illeszkedő klózainak a listája  Cs.
index1_lista(S,P,Cs) :- %! 6p
    read(S,Klóz),
    ( Klóz == end_of_file -> Cs = []
    ; klózfej(Klóz,Fej), ind1_illik(Fej,P) ->
          Cs = [Klóz|Ms], index1_lista(S,P,Ms)
    ; index1_lista(S,P,Cs)
    ).

% ind1_illik(Fej,P) :- a Fej klózfej az első arg. indexelés szerint
%                                megfelel a  P  predikátumhívásnak.
ind1_illik(Fej,P) :-        %! 5p
    ( atom(P) -> Fej == P
    ; compound(Fej), functor(Fej,F,N), functor(P,F,N),
      arg(1,Fej,F1), arg(1,P,P1), arg1_illik(F1,P1)
    ).

% arg1_illik(A,B) :- a klózfejből és a predikátumhívásból kinyert első
%                      argumentumok az indexelés szerint illeszkednek.
arg1_illik(A,B) :-     %! 5p
    ( var(A) -> true
    ; var(B) -> true
    ; compound(A), compound(B) -> functor(A,F,N), functor(B,F,N)
    ; A == B
    ).

%! Az első par. illeszthetősége erősebb, mint az első arg. indexelés
%! szerinti szűrés, mivel az indexelés összetett termeknél csak a
%! függvényszimbólumok (és ezen belül az aritások) megfelelését vizsgálja.
%! (Különben a Prolog nem tudna hatékonyan indexelni.)
%! Pl. legyen az x.pl fájl tartalma:  lis([]). lis([_]).  Ekkor 
%! | ?- indexLista('x.pl',lis([1,2]),Cs).
%! Cs = [lis([_A])]
%! az elvárt működés, és nem
%! Cs = []

klózfej(Klóz,Fej) :-                    %! 2p
    ( Klóz = (KlózFej :- _Törzs) -> KlózFej = Fej  % output ill. a vágó után
    ; Klóz = Fej
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                   % 1. Prolog zh, 2006. október 24. 

% A megadott előfeltétellel a vonatkozó célok keresési fája véges kell legyen.
% A megoldások során minden predikátumot definiálni kell, a SICStus Prolog
% beépített eljárásai kivételével.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. A családja/3 alapján adjuk meg a 'szülője'/2 relációt!
%
% Előf: adottak  családja(Anya,Apa,Gyerekek)  alakú tényállítások, ahol
%   Anya és Apa névkonstansok, a Gyerekek pedig névkonstansok valódi listája.
%
% 'szülője'(X,Y) :- X az Y szülője a családja/3 predikátum szerint.
%
% Például:
családja(s,a,[b,c,d]).     családja(m,b,[e,f,g]).
%
% | ?- 'szülője'(X,Y).
% X=s, Y=b ;  X=s, Y=c ;  X=s, Y=d ;  X=m, Y=e ;  X=m, Y=f ;  X=m, Y=g ;
% X=a, Y=b ;  X=a, Y=c ;  X=a, Y=d ;  X=b, Y=e ;  X=b, Y=f ;  X=b, Y=g ;  no

'szülője'(X,Y) :- családja(X,_,Gs), member(Y,Gs).
'szülője'(X,Y) :- családja(_,X,Gs), member(Y,Gs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2. Gyűjtsük ki családok listájáról a legalább két gyerekeseket!
%
% Előf: Családok valódi lista. Elemei cs(Anya,Apa,Gyerekek) alakú termek, ahol
%   Anya és Apa névkonstansok, Gyerekek pedig névkonstansok valódi listája.
%
% cs2gy(Családok,MinKétGyerekesek) :-
%     a Családok listáról kiválogatva azokat, ahol legalább két gyermek van,
%     adódik a MinKétGyerekesek lista, az eredeti sorrendben.
%
% | ?- cs2gy([cs(s,a,[b,c,d]),cs(d,i,[j,k]),cs(c,h,[]),
%             cs(m,b,[e,f,g]),cs(k,l,[m]),cs(m,e,[n,p])],Cs2s).
% Cs2s = [cs(s,a,[b,c,d]),cs(d,i,[j,k]),cs(m,b,[e,f,g]),cs(m,e,[n,p])]

cs2gy([],[]).
cs2gy([Család|Családok],Ks) :-
    ( Család = cs(_,_,[_,_|_]) -> Ks = [Család|Ls], cs2gy(Családok,Ls)
    ; cs2gy(Családok,Ks)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3. Melyik a legnagyobb család?
% 
% Előf: Családok valódi lista. Elemei cs(Anya,Apa,Gyerekek) alakú termek, ahol
%   Anya és Apa névkonstansok, Gyerekek pedig névkonstansok valódi listája.
%
% legnagyobb_család(Családok,LegCs) :- a Családok listán
%     a legtöbb tagú családok közül sorrendben az első LegCs.
%
% | ?- legnagyobb_család([cs(k,l,[m]),cs(d,i,[j,k]),cs(m,e,[n,p]),
%                         cs(s,a,[b,c,d]),cs(c,h,[]),cs(m,b,[e,f,g])],LegCs).
% LegCs = cs(s,a,[b,c,d])

legnagyobb_család([Cs1|Családok],LegCs) :-
    gyerekszám(Cs1,M1), maxcsalád(Családok,Cs1,M1,LegCs).

gyerekszám(cs(_Anya,_Apa,Gyerekek),N) :- length(Gyerekek,N).

maxcsalád([],Cs,_M,Cs).
maxcsalád([Cs1|Családok],Cs0,M0,LegCs)  :-
    gyerekszám(Cs1,M1),
    ( M1 > M0 -> maxcsalád(Családok,Cs1,M1,LegCs)
    ; maxcsalád(Családok,Cs0,M0,LegCs)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 