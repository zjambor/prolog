%% Adottak:
%% A természetes számok s-szám reprezentációja:
%%                        0, s(0), s(s(0)), ...
nat(0).
nat(s(N)) :- nat(N).

plus(0,N,N).
plus(s(M),N,s(K)) :- plus(M,N,K).

times(0,_N,0).
times(s(M),N,K) :- times(M,N,MN), plus(N,MN,K). % (M+1)*N = M*N+N

%% Elõf: A és N s-számok, azaz természetes számok a
%%                        0, s(0), s(s(0)), ... reprezentációban.
%%
%% hatvány(A,N,B) :-
%%     Az A N-edik hatványa a B s-szám.
/*
| ?- hatvány(0,s(s(0)),H).
H = 0 
| ?- hatvány(0,0,H).
H = s(0)
| ?- hatvány(s(s(0)),0,H).
H = s(0)
| ?- hatvány(s(0),s(s(s(0))),H).
H = s(0)
| ?- hatvány(s(s(0)),s(s(s(0))),H).
H = s(s(s(s(s(s(s(s(0)))))))
| ?- hatvány(s(s(s(0))),s(s(0)),H).
H = s(s(s(s(s(s(s(s(s(0)))))))))

| ?- hatvány(0,s(s(0)),H0), hatvány(0,0,H0_0_1), hatvány(s(s(0)),0,H2_0_1),
     hatvány(s(0),s(s(s(0))),H1_3_1), hatvány(s(s(0)),s(s(s(0))),H8), 
     hatvány(s(s(s(0))),s(s(0)),H9).
H0 = 0,
H0_0_1 = s(0),
H2_0_1 = s(0),
H1_3_1 = s(0),
H8 = s(s(s(s(s(s(s(s(0)))))))),
H9 = s(s(s(s(s(s(s(s(s(0)))))))))
*/

%% Ha egy tesztnél csak egy megoldást tüntettem fel, akkor az adott kérdésre
%% csak egy megoldást szabad kapnunk. A közölt teszt nem teljes körû!

hatvány_(_,0,s(0)).
hatvány_(0,s(_X),0).
%%%hatvány(Y,s(0),Y).
hatvány_(X,s(Y),H) :- hatvány_(X,Y,C), times(X,C,H).

%% 2. Definiálja tetszõleges bináris reláció általánosítását listákra! (20p)

%% Elõf: Xs vagy Ys valódi lista, és az azonos sorszámú (X,Y) listaelem-párokra
%%       az R(X,Y) reláció elõfeltétele teljesül.

%% binrels(Xs,Ys,R) :- Xs elemei sorban R(X,Y) relációban vannak
%%                     Ys megfelelõ sorszámú elemeivel.
%%
%% LI: mindegyik megoldásra O(|Xs|),
%%     eltekintve az R(X,Y) kiértékelésének mûveletigényétõl.
%%
/*
| ?- binrels(Xs,[i,j,z,n,e],a).
Xs = [a,i,a,a,i]
| ?- binrels([a,i],Xs,a).
Xs = [i,j] ? ;
Xs = [i,e] ? ;
Xs = [z,j] ? ;
Xs = [z,e] ? ;
Xs = [n,j] ? ;
Xs = [n,e] ? ;
no
| ?- binrels([3.2,4.7,-4.5,alma,-5.6],Ys,round).
Ys = [3,5,-4,alma,-6]
*/
%% A teszthez:

a(a,i).    a(a,z).    a(a,n).
a(i,j).    a(i,e).

round(A,B) :-
    ( float(A) -> B is round(A)
    ; B=A
    ).

binrels([],[],_).
binrels([X|Xs],[Y|Ys],Z) :-
    functor(C,Z,2), arg(1,C,X), arg(2,C,Y), C,
    binrels(Xs,Ys,Z).

%% 1. Olvassunk be egy számot a standard inputról, interaktív módon! (20p)
%%
%% Elõf: A standard input elérhetõ.
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
%% Mj.: Hasznosítsuk a sikertelen konverziónál fellépõ kivételt!
/*
| ?- számot_olvas(X).
|: 31.4e-1
X = 3.14
*/

get_lines(S,Cs) :-
    get_code(S,X),
    ( X == 0'\n -> Cs=[]
    ; Cs = [X|Ds], get_lines(S,Ds)
    ).

számot_olvas(X) :-
    repeat,
    catch( (get_lines(user,Cs), number_codes(X,Cs) ),
	    error(syntax_error('number syntax'),_),
	    ( write(user,'számot kérek:'), flush_output(user), fail )
	  ), !.

%% 2. Írjuk meg az '=..'/2 beépített eljárás saját változatát! (30p)
%%
%% nonvar2list(Term,Xs) :
%%     ha nonvar(Term) vagy Xs egyelemû számlista vagy
%%         Xs valódi lista, aminek elsõ eleme atom,
%%     akkor a Term =..Xs hívásnak megfelelõen mûködik,
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

/*
| ?- binrels([a,i],Xs,a) =.. X.
X = [binrels,[a,i],Xs,a] ?
*/

nonvar2list(Term,Xs) :-
    ( nonvar(Term) ->
         functor(Term,A,N), Xs=[A|Ys], hozzafüz(Term,Ys,N,1)
    ; proper_list(Xs), Xs=[A|Ys] ->
        ( Ys==[], atomic(A) -> Term = A 
        ; atom(A) -> length(Ys,N), functor(Term,A,N), hozzafüz(Term,Ys,N,1)
	)
    ).

proper_list(Xs) :-
    nonvar(Xs),
    ( Xs == [] -> true
    ; Xs = [_|Ys], proper_list(Ys)
    ).

hozzafüz(T,Xs,I,N) :-
    ( N > I -> Xs=[]
    ; arg(N,T,X), Xs=[X|Ds], N1 is N+1, hozzafüz(T,Ds,I,N1)
    ).

%% Mj.: A valódi karakterkód listákat sztringként jelölhetjük, és úgy is
%%%     nevezzük; pl.: "1 a" == [0'1,0' ,0'a], " " == [].'

%% 1. Írassuk ki a standard outputra egy sztringnek
%%    egy másik sztringben nem szereplõ karaktereit! ([10+5+5]p)
%%
%% Mj.: Adjunk a feladatra három, egymástól elvileg különbözõ megoldást,
%% put_string_diff_1/2, put_string_diff_2/2, put_string_diff_3/2, néven!
%% (1: listamûvelet tiszta Prolog + vágó stílusban [10p],
%%  2: célokkal paraméterezhetõ (magasabbrendû) predikátum használata [5p],
%%  3: a negáció kifinomult használatával karakterenként íratjuk ki [5p].)
%%
%% Elõf: S1, S2 Prolog sztringek.
%%       Feltehetõ, hogy vezérlõ karaktereket (pl. újsor) nem tartalmaznak.
%%
%% put_string_diff_I(S1,S2) : Írassuk ki a standard outputra sorfolytonosan
%%              az S1 sztringnek az S2 sztringben nem szereplõ karaktereit,
%%              az S1-beli sorrendjüknek megfelelõen!  (I eleme {1,2,3}).
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
| ?- put_string_diff_1("szilvafán","szõlõ"),
     put_string_diff_2("szilvafán","szõlõ"),
     put_string_diff_3("szilvafán","szõlõ").
ivafán
ivafán
ivafán
yes
*/

put_string_diff_1(S1,S2) :-
    string_diff(S1,S2,Xs), atom_codes(C,Xs), write(user,C), nl(user).

string_diff([],_,[]).
string_diff([X|Xs],Ys,Zs) :-
    ( member(X,Ys) -> string_diff(Xs,Ys,Zs)
    ; Zs=[X|Us], string_diff(Xs,Ys,Us)
    ).

put_string_diff_2(S1,S2) :-
    findall(C, (member(C,S1), \+member(C,S2)), Ys), atom_codes(B,Ys),
    write(user,B), nl(user).

put_string_diff_3(S1,S2) :-
    \+ (member(C,S1), \+member(C,S2), \+put_code(user,C)), nl(user).

%% 2. Készítsünk tagolt bináris kód (tbk) felismerõ DCG-t! (10p)
%%
%% Mj.: A feladatot tiszta elemzõ DCG-vel oldjuk meg, anélkül,
%%      hogy a nyelvtanban explicit Prolog hívást alkalmaznánk!
%%
%% Def.: Egy sztring tbk, akkor ha
%%       - ábécéjét a "0_1" sztring karakterei adják,
%%       - nemüres,
%%       - elsõ és utolsó eleme nem az aláhúzásjel kódja,
%%       - sehol sincs benne két aláhúzásjel kód egymás után.
%%
%% Elõf: Bs valódi lista.
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

tbk --> bit, tbkvég.

bit --> "0".
bit --> "1".

tbkvég --> "_", tbk.
tbkvég --> tbk.
tbkvég --> "".

%% 3. Módosítsuk az elõzõ megoldást úgy, hogy a tbk sztring,
%%    mint egész szám értékét is meghatározzuk!
%%    Tegyük teljesen determinisztikussá a kódot! (10p)
%%
%% Mj.: A program minden mondata maradjon DCG szabály,
%%      de most már explicit Prolog hívásokra is szükség lesz.
%%
%% Elõf: Bs valódi lista.
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

tbk1(Szám) --> bit(B), !, tbk1_vég(B,Szám).

tbk1_vég(X,Szám) --> "_", !, tbk2(X,Szám).
tbk1_vég(X,Szám) --> bit(B), !, {X1 is 2*X+B}, tbk1_vég(X1, Szám).
tbk1_vég(X,X) --> [].
tbk1_vég(_X,_Szám) --> [_C], !, {fail}.

tbk2(X,Szám) --> bit(B), !, {X1 is 2*X+B}, tbk1_vég(X1, Szám).

bit(0) --> "0".
bit(1) --> "1".

% 1. Adjuk meg egy tetszõleges számhoz a megfelelõ határozott névelõt! (15p)
%
% Elõf: N egész szám.
% Mj.: Ha N nem egész szám, hiúsuljon meg a predikátumhívás!
%
% az_a(N,AzA) :- az N egész számhoz tartozó megfelelõ
%     magyar nyelvû határozott névelõ ('Az ' ill. 'A ') AzA.
%
% Segítség: Az alábbi C++ kód megoldja a problémát.
%
%%% // Visszaadja a megfelelõ határozott névelõt. 
%%% // Szerzõ: Ásványi Tibor, 2008. október 15.
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

az_a(N,AzA) :-
    integer(N),
    ( N =< 0 -> AzA = 'A '
    ; divs(N,1000,E),
      ( E==1 -> AzA = 'Az '
      ; divs(E,10,F),
	( F==5 -> AzA = 'Az '
	; AzA = 'A '
	)
      )
    ).

divs(N,I,E) :-
    ( N >= I -> N1 is N // I, divs(N1,I,E)
    ; E = N
    ).

% 2. Írjunk tesztkörnyezetet az elõzõ programhoz! (15p)
%
% Elõf: A beírt egész számok az ábrázolható tartományba esnek.
%
% az_a_teszt :-
%    "Ciklusban" bekér a standard inputról egy-egy egész számot.
%    Ha nem egész számot kap, elköszön a program.
%    Ha egész számot kap, kiírja a megfelelõ szöveget:
%    "Az U számot olvastam be." vagy
%    "A V számot olvastam be.", ahol U ill. V a beolvasott szám.
%
/*
| ?-  az_a_teszt.
Kérem az egész számokat,
amik elé határozott névelõt kell tennem!
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
amik elé határozott névelõt kell tennem!
(Ha nem egész számot kapok, elköszönök.)
|: 3.14159265358979323846
Viszlát!
yes
*/

az_a_teszt :-
    write(user,'Kérem az egész számokat,\n'),
    write(user,'amik elé határozott névelõt kell tennem!\n'),
    write(user,'(Ha nem egész számot kapok, elköszönök.)\n'),
    repeat,
      sorbeolv(user,Vs),
      ( catch( (number_codes(N,Vs), integer(N)),_,fail ) ->
	kiir(N), fail
      ; write(user,'Viszlát!\n')
      )
      -> true.        %% vagy , true.

sorbeolv(S,Vs) :-
    get_code(S,X),
    ( X==0'\n -> Vs=[]
    ; Vs=[X|Us], sorbeolv(S,Us)
    ).

kiir(N) :- az_a(N,AzA), write(user,AzA), write(user,N), write(user,' számot olvastam be.\n').


% 3. Definiáljuk elemzõ DCG-vel a divide/5 predikátumot a qs/4 programban! (15p)
%
% Elõf: Xs valódi lista.
%
% divide(Ls,Gs,X,Xs,[]) :- az Xs X-nél nagyobb elemeit Gs,
%     a többit Ls tartalmazza, a standard rendezés szerint.
%
% Mj.: Ezt a feladatot olyan elemzõ DCG nyelvtannal KELL megoldani, ami
%     nem tartalmaz felhasználói/ könyvtári predikátumra vonatkozó Prolog hívást.
%     Tartalmazhat viszont beépített eljárásokra vonatkozó Prolog hívásokat.
%
qs(Xs,Ys) :- quicksort(Xs,Ys,[]).

quicksort([X|Xs]) -->
    {divide(Ls,Gs,X,Xs,[])},
    quicksort(Ls), [X], quicksort(Gs).
quicksort([]) --> [].

/*divide([X|Xs],Y,Ls,Gs) :-
    ( X =< Y -> Ls = [X|Ks], divide(Xs,Y,Ks,Gs)
    ; Gs = [X|Ns], divide(Xs,Y,Ls,Ns)
    ).
divide([],_Y,[],[]).*/

%divide(Ls,Gs,Y) --> [X], {X @< Y, !, Ls = [X|Ks]}, divide(Ks,Gs,Y).
%divide(Ls,Gs,Y) --> [X], {X @> Y, !, Gs = [X|Ns]}, divide(Ls,Ns,Y).
%divide([],[],_Y) --> [].

divide(Ls,Gs,Y) --> [X], !,
      ( {X @=< Y} -> {Ls = [X|Ks]}, divide(Ks,Gs,Y)
      ; {Gs = [X|Ns]}, divide(Ls,Ns,Y)
      ).
divide([],[],_Y) --> [].

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

% 1. Számoljuk ki egy bináris fa egyesúlyait! (10p)
%
% Elõf: T valódi bináris fa, ahol a kis 'o' atom az üres fa,
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
*/

/*
%egyensúlyok(o,M,TS) :- TS=t()
egyensúlyok(T,M,TS) :- egyensúlyok_app(T,0,0,0,TS).

egyensúlyok_app(o,Mb,Mj,N,TS).
egyensúlyok_app(t(B,X,J),Mb,Mj,N,TS) :-
    ( B==o, J==o -> N is Mb-Mj
    ; Mb1 is Mb+1, egyensúlyok_app(B,Mb1,Mj,N,TS),
      Mj1 is Mj+1, egyensúlyok_app(J,Mb,Mj1,N,TS)
    ).
*/

egyensúlyok(o,-1,o).
egyensúlyok(t(B,X,J),M,t(B1,X,E,J1)) :-
    egyensúlyok(B,BM,B1), egyensúlyok(J,JM,J1),
    M is 1+max(BM,JM), E is JM-BM.

% 2. Fordítsunk meg egy valódi listát DCG-vel! (10p)
%
% Elõf: Xs valódi lista.
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

reverse([]) --> [].
reverse([X|Xs]) --> reverse(Xs), [X].

% 1. Melyik lista reprezentálja egy term egy ágát? (20p)
%
% Elõf: A (T,Xs) páros nem tartalmaz duplikált ismeretlent.
%
% term_ág(T,Xs) :-
%     Ha a híváskor T összetett term, az Xs valódi lista elsõ eleme
%     a T függvényszimbólum-neve, maradéka pedig a T
%     valamelyik paraméteréhez tartozó term-ág.
%     Ha a híváskor T egyszerû term, az Xs valódi lista egyetlen eleme T.
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

%t(
%a(X), 2, b (c(3,d), 4.0)

term_ág(T,Xs) :-
    ( compound(T) -> functor(T,Tneve,I), Xs=[Tneve|Us], term_gyerek(T,I,Gyerekterm), term_ág(Gyerekterm,Us)
    ; Xs=[T]
    ).

term_gyerek(T,I,Gyerekterm) :-
    arg(I,T,Gyerekterm)
    ; I > 1, I1 is I-1, term_gyerek(T,I1,Gyerekterm).

% 2. Adjuk meg egy bináris fa inorder bejárását generatív DCG-vel! (10p)
%
% Elõf: T valódi bináris fa, ahol a kis 'o' atom az üres fa,
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

inorderDCG(o) --> [].
inorderDCG(t(B,X,J)) --> inorderDCG(B), [X], inorderDCG(J).

% 3. Mely bináris fák inorder bejárása lehet egy adott lista? (10p)
%
% Jelölés: az üres fát a kis 'o' atom reprezentálja,
%   a nemüres fák pedig t(BalRészFa,Gyökér,JobbRészFa) alakúak.
%
% Elõf: N >=-1 egész szám, Is valódi lista.
%
% inorder_DCG(N,T,Is,[]) :- A legfeljebb N mélységû T bináris fa
%                           inorder bejárása az Is valódi lista.
%
% Mj.: Ezt a feladatot elemzõ DCG nyelvtannal ajánlott megoldani.
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

%%inorder_DCG(-1,_T) --> [].
%%inorder_DCG(0,t(o,X,o)) --> [X].
%%inorder_DCG(1,t(o,X,J)) --> [X], {N1 is N-1}, inorder_DCG(N1,B)

inorder_DCG(N,t(B,X,J)) -->
    {N>=0, N1 is N-1},
    inorder_DCG(N1,B), [X], inorder_DCG(N1,J).

inorder_DCG(_,o) --> [].

% 3. Melyek egy lista részsorozatai? (20p)
%
% Elõf: Ys valódi lista.
%
% részsorozat(Xs,Ys,[]) :- Xs az Ys nem okvetlenül folytonos részsorozata.
%
% Mj.: Ezt a feladatot tiszta elemzõ DCG nyelvtannal KELL megoldani.
%      ami tehát nem tartalmaz Prolog hívást.
%      Ha Ys elemei különbözõ termek, ne adjon duplikált megoldást! (-6p)
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

részsorozat([]) --> list.
részsorozat([X|Xs]) --> [X], részsorozat(Xs).
részsorozat(Xs) --> [_], részsor(Xs).

részsor([X|Xs]) --> [X], részsorozat(Xs).
részsor(Xs) --> [_], részsor(Xs).

list --> [_], list.
list --> [].

% Írjuk le a lista hatványozás fogalmát! (40p)
%
% Elõf: A "Ts valódi lista", "Xs valódi lista", "N nemnegatív egész"
%       állítások közül legalább kettõ igaz.
%
% hatvány(N,Xs,Ts) :-
%     az Xs valódi lista (N-1)-szer önmaga után fûzve adja a Ts valódi listát.
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
*/

/*
hatvány(N,Xs,Ts) :-
    ha N=0 akkor Ts=[]
    ha N integer és N>0, akkor akkor hatvány_kiszámol(Xs)                   --ha Ts=[], és Xs valódi lista, akkor hatvány_kiszámol(Xs)
    ; ha Xs=[], akkor Ts=[]                                                 --és Ts valódi lista akkor hozzáfüz(N,Xs,Ts)
    ; ha Ts=[] akkor N=0
    ; ha XS és Ts is valódi lista, akkor Nx Xs hozza és Nt Ts hossza, és N is Nt//Nx, és hozzáfüz(N,Xs,Ts)
*/

hatvány(N,Xs,Ts) :-
    ( N == 0 -> Ts=[]
    ; integer(N) -> N>0, hatvány_kiszámol(N,Xs,Ts)
    ; Xs==[] -> Ts=[]
    ; Ts==[] -> N=0
    ; valódi_lista(Xs), valódi_lista(Ts),
      length(Xs,NX), length(Ts,NT),
      NT mod NX =:= 0, N is NT//NX,
      hozzáfüz(N,Xs,Ts)
    ).

valódi_lista(Xs) :-
    ( Xs==[] -> true
    ; nonvar(Xs), Xs=[_Y|Ys], valódi_lista(Ys)
    ).

hatvány_kiszámol(N,Xs,Ts) :-
    ( valódi_lista(Xs) -> true
    ; valódi_lista(Ts), length(Ts,NT),
      NX is NT // N, NT mod N =:= 0, length(Xs,NX)
    ),
    hozzáfüz(N,Xs,Ts).

hozzáfüz(N,Xs,Ts) :- eléfüz(N,Xs,Xs,Ts).

%% append(L1,L2,Eredménylista).

eléfüz(N,Xs,Bs,Ts) :-
    ( N>1 -> N1 is N - 1, append(Xs,Bs,As), eléfüz(N1,Xs,As,Ts)
    ; Ts=Bs
    ).

/*
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

%% 1. Írassunk ki egy termet, mint függvénykifejezést! (30p)
%%
%% Elõf: A standard output elérhetõ.
%%
%% typenonf(T) : Kiíratja a T termet a standard outputra 
%%               a typenonf_test(T) predikátumnak megfelelõ módon.
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
typenonf_test(T) :- write_term(user,T,[quoted(true),ignore_ops(true)]).

/*
| ?- typenonf(-2-a(3,x,-c/y+'Alma')).
-(-2,a(3,x,+(/(-(c),y),'Alma')))
yes
| ?- typenonf(a+[b,'c d\ne','F']*'$VAR'(1)).
+(a,*('.'(b,'.'('c d\ne','.'('F',[]))),'$VAR'(1)))
yes
*/

typenonf(T) :-
    (compound(T) ->
      functor(T,F,A),
      writeq(user,F), write(user,'('),
      typenonf_args(1,T,A), write(user,')')
    ; writeq(user,T)
    ).

typenonf_args(N,T,I) :-
    arg(N,T,A), typenonf(A),
    ( N<I -> write(','), N1 is N + 1, typenonf_args(N1,T,I)
    ; true
    ).

%%% | ?- nqueens(4,Qs).
%%% Qs = [2,4,1,3] ? ;
%%% Qs = [3,1,4,2] ? ;
%%% no

%%% - - Q -       - Q - -
%%% Q - - -       - - - Q
%%% - - - Q       Q - - -
%%% - Q - -       - - Q -

nqueens(N,Qs) :- range(1,N,Ns), permutation(Ns,Qs), safe(Qs).

range(M,N,[M|Ns]) :- M<N, !, M1 is M+1, range(M1,N,Ns).
range(N,N,[N]).

permutation(Xs,[Z|Zs]) :- select(Z,Xs,Ys), permutation(Ys,Zs).
permutation([],[]).

select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

safe([Q|Qs]) :- safe(Qs), \+ attack(Q,Qs).
safe([]).     
     
attack(X,Xs) :- attack(X,1,Xs).

attack(X,N,[Y|_]) :- N =:= abs(X-Y).
attack(X,N,[_|Ys]) :- N1 is N+1, attack(X,N1,Ys).

% 1. Írjuk le DCG-vel a dadogó listák fogalmát! (15p)
%
% Elõf: Ts alaplista vagy Xs alaplista
%
% dadogó(Xs,Ts,Rs) :- dadog(Xs,Ts,Rs). % ahol:
%
dadog_(Xs,Ts,Rs) :-
    ( ground(Xs) -> append(Xs,Xs,XsXs), append(XsXs,Rs,Ts)
    ; append(XsXs,Rs,Ts), append(Xs,Xs,XsXs)
    ).
%
% Mj.: dadogó/3-at tiszta DCG-vel kell megírni, azaz nem tartalmazhat
%      expliciten sem Prolog predikátumot, sem Prolog hívást.
%      (Generatív és elemzõ irányban is ugyanaz a kód mûködik majd.)
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

dadogó(Xs) --> dadog(Xs), dadog(Xs).

dadog([X|Xs]) --> [X], dadog(Xs).
dadog([]) --> [].

% 2. Írjuk meg az unio_sort programját generatív DCG-vel!
%
% Mj.: Duplikált listaelem alatt a lista egy olyan elemét értjük,
%      amely késõbb is elõfordul a listában. (K db. elõfordulás (K>=1)
%      esetén tehát (K-1)-szer adódik ki az elem megoldásként.)
%
% Elõf: Xs alaplista.
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

member_check(X,[Y|Ys]) :-
     ( X = Y -> true
     ; member_check(X,Ys)
     ).

member2(X,Xs) :- member2_(X,Xs).

member2_(_X,[X|Xs]) --> [X], {member_check(X,Xs)}. %%, member2_(_X,Xs).
member2_(X,Xs) --> {Xs=[_X|Ys]}, member2_(X,Ys).
member2_(_X,[]) --> [].

%% 2. Az aktuális input sor decimális számjegyek Prolog listája-e? (50p)
%%
%% Mj.: Elõször olvassuk be az aktuális input sort karakterkódok egy
%%      listájába, majd elemezzük DCG-vel, hogy megfelelõ-e a szintaxisa,
%%      mint decimális számjegyek valódi Prolog listája!
%%      (Ha az elemzést nem DCG-vel oldja meg, max. 25 pont.)
%%
%% Elõf.: Az S logikai névvel megnyitott input szövegfile olvasható.
%%
%% digit_list_line(S,Ds) :-
%%       Az S input szövegfile aktuális sorát beolvasva, az a
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
|: [3,2,6]
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


digit_list_line(S,Ds) :- get_lines2(S,Cs), digit_list(Ds,Cs,[]).

digit_list(Ds) --> digitlist_kezd, digitlist_vég(Ds).

digitlist_kezd --> üres_kar, [0'[], üres_kar.

digitlist_vég([]) --> [0']], !.
digitlist_vég([D|Ds]) --> számok(D), üres_kar, digit_sor(Ds), üres_kar.

digit_sor([]) --> [0']], !.
digit_sor([D|Ds]) --> [0',], üres_kar, számok(D), üres_kar, digit_sor(Ds).

számok(D) --> [C], {C >= 0'0, C =< 0'9, D is C-0'0}.

üres_kar --> [0' ], !, üres_kar.
üres_kar --> [].

get_lines2(S,Cs) :-
    get_code(S,C),
    ( C == 0'\n -> Cs=[]
    ; Cs=[C|Us], get_lines(S,Us)
    ).


%% 2. Írjuk meg az unio_sort programját generatív DCG-vel!

unionSort(Ds,Cs) :- us(Ds,Cs,[]).

us([X,Y|Ds]) --> !,
    {divide2(Ds,As,Bs), us([X|As],sAs,[]), us([Y|Bs],sBs,[])}, union(sAs,sBs).
us([A]) --> [A].
us([]) --> [].

divide2([],[],[]).
divide2([X],[X],[]).
divide2([X,Y|Xs],[X|Ys],[Y|Zs]) :- divide2(Xs,Ys,Zs).
%divide2([X|Xs],[X|Ys],Zs) :- divide2(Xs,Ys,Zs).

union([],[]) --> !, [].
union([],[X|Xs]) --> [X], union([],Xs).
union([X|Xs],[]) --> !, [X], union([],Xs).
union([X|Xs],[Y|Ys]) -->
    ( {X @< Y} -> [X], union(Xs,[Y|Ys])
    ; {X @> Y} -> [Y], union([X|Xs],Ys)
    ; [X], union(Xs,Ys)
    ).



