%% Adottak:
%% A term�szetes sz�mok s-sz�m reprezent�ci�ja:
%%                        0, s(0), s(s(0)), ...
nat(0).
nat(s(N)) :- nat(N).

plus(0,N,N).
plus(s(M),N,s(K)) :- plus(M,N,K).

times(0,_N,0).
times(s(M),N,K) :- times(M,N,MN), plus(N,MN,K). % (M+1)*N = M*N+N

%% El�f: A �s N s-sz�mok, azaz term�szetes sz�mok a
%%                        0, s(0), s(s(0)), ... reprezent�ci�ban.
%%
%% hatv�ny(A,N,B) :-
%%     Az A N-edik hatv�nya a B s-sz�m.
/*
| ?- hatv�ny(0,s(s(0)),H).
H = 0 
| ?- hatv�ny(0,0,H).
H = s(0)
| ?- hatv�ny(s(s(0)),0,H).
H = s(0)
| ?- hatv�ny(s(0),s(s(s(0))),H).
H = s(0)
| ?- hatv�ny(s(s(0)),s(s(s(0))),H).
H = s(s(s(s(s(s(s(s(0)))))))
| ?- hatv�ny(s(s(s(0))),s(s(0)),H).
H = s(s(s(s(s(s(s(s(s(0)))))))))

| ?- hatv�ny(0,s(s(0)),H0), hatv�ny(0,0,H0_0_1), hatv�ny(s(s(0)),0,H2_0_1),
     hatv�ny(s(0),s(s(s(0))),H1_3_1), hatv�ny(s(s(0)),s(s(s(0))),H8), 
     hatv�ny(s(s(s(0))),s(s(0)),H9).
H0 = 0,
H0_0_1 = s(0),
H2_0_1 = s(0),
H1_3_1 = s(0),
H8 = s(s(s(s(s(s(s(s(0)))))))),
H9 = s(s(s(s(s(s(s(s(s(0)))))))))
*/

%% Ha egy tesztn�l csak egy megold�st t�ntettem fel, akkor az adott k�rd�sre
%% csak egy megold�st szabad kapnunk. A k�z�lt teszt nem teljes k�r�!

hatv�ny_(_,0,s(0)).
hatv�ny_(0,s(_X),0).
%%%hatv�ny(Y,s(0),Y).
hatv�ny_(X,s(Y),H) :- hatv�ny_(X,Y,C), times(X,C,H).

%% 2. Defini�lja tetsz�leges bin�ris rel�ci� �ltal�nos�t�s�t list�kra! (20p)

%% El�f: Xs vagy Ys val�di lista, �s az azonos sorsz�m� (X,Y) listaelem-p�rokra
%%       az R(X,Y) rel�ci� el�felt�tele teljes�l.

%% binrels(Xs,Ys,R) :- Xs elemei sorban R(X,Y) rel�ci�ban vannak
%%                     Ys megfelel� sorsz�m� elemeivel.
%%
%% LI: mindegyik megold�sra O(|Xs|),
%%     eltekintve az R(X,Y) ki�rt�kel�s�nek m�veletig�ny�t�l.
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

%% 1. Olvassunk be egy sz�mot a standard inputr�l, interakt�v m�don! (20p)
%%
%% El�f: A standard input el�rhet�.
%%
%% sz�mot_olvas(X) :
%%   Ciklusban
%%     beolvassuk egy karakterk�d list�ba a standard input aktu�lis sor�t.
%%     Megpr�b�ljuk Prolog sz�mm� konvert�lni a number_codes/2 seg�ts�g�vel.
%%     Ha ez siker�l, K�SZ vagyunk; az eredm�ny X-be ker�l, �s bef. a ciklust.
%%     Ha nem siker�l, ki�rjuk, hogy 'Sz�mot k�rek: ',
%%     majd visszal�p�ssel �jraind�tjuk a (repeat-fail) ciklust.
%% 
%% LI: O(a standard input sorok karaktereinek sz�ma)
%%
%% Mj.: Hasznos�tsuk a sikertelen konverzi�n�l fell�p� kiv�telt!
/*
| ?- sz�mot_olvas(X).
|: 31.4e-1
X = 3.14
*/

get_lines(S,Cs) :-
    get_code(S,X),
    ( X == 0'\n -> Cs=[]
    ; Cs = [X|Ds], get_lines(S,Ds)
    ).

sz�mot_olvas(X) :-
    repeat,
    catch( (get_lines(user,Cs), number_codes(X,Cs) ),
	    error(syntax_error('number syntax'),_),
	    ( write(user,'sz�mot k�rek:'), flush_output(user), fail )
	  ), !.

%% 2. �rjuk meg az '=..'/2 be�p�tett elj�r�s saj�t v�ltozat�t! (30p)
%%
%% nonvar2list(Term,Xs) :
%%     ha nonvar(Term) vagy Xs egyelem� sz�mlista vagy
%%         Xs val�di lista, aminek els� eleme atom,
%%     akkor a Term =..Xs h�v�snak megfelel�en m�k�dik,
%%     k�l�nben meghi�sul.
%%
%% Mj.: Felhaszn�lhatjuk a functor/3 �s az arg/3 be�p�tett elj�r�sokat,
%%      de az '=..'/2 elj�r�st term�szetesen nem.
%% 
%% LI: nonvar(Term) eset�n O(Term arit�sa), k�l. O(|Xs|)
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
         functor(Term,A,N), Xs=[A|Ys], hozzaf�z(Term,Ys,N,1)
    ; proper_list(Xs), Xs=[A|Ys] ->
        ( Ys==[], atomic(A) -> Term = A 
        ; atom(A) -> length(Ys,N), functor(Term,A,N), hozzaf�z(Term,Ys,N,1)
	)
    ).

proper_list(Xs) :-
    nonvar(Xs),
    ( Xs == [] -> true
    ; Xs = [_|Ys], proper_list(Ys)
    ).

hozzaf�z(T,Xs,I,N) :-
    ( N > I -> Xs=[]
    ; arg(N,T,X), Xs=[X|Ds], N1 is N+1, hozzaf�z(T,Ds,I,N1)
    ).

%% Mj.: A val�di karakterk�d list�kat sztringk�nt jel�lhetj�k, �s �gy is
%%%     nevezz�k; pl.: "1 a" == [0'1,0' ,0'a], " " == [].'

%% 1. �rassuk ki a standard outputra egy sztringnek
%%    egy m�sik sztringben nem szerepl� karaktereit! ([10+5+5]p)
%%
%% Mj.: Adjunk a feladatra h�rom, egym�st�l elvileg k�l�nb�z� megold�st,
%% put_string_diff_1/2, put_string_diff_2/2, put_string_diff_3/2, n�ven!
%% (1: listam�velet tiszta Prolog + v�g� st�lusban [10p],
%%  2: c�lokkal param�terezhet� (magasabbrend�) predik�tum haszn�lata [5p],
%%  3: a neg�ci� kifinomult haszn�lat�val karakterenk�nt �ratjuk ki [5p].)
%%
%% El�f: S1, S2 Prolog sztringek.
%%       Feltehet�, hogy vez�rl� karaktereket (pl. �jsor) nem tartalmaznak.
%%
%% put_string_diff_I(S1,S2) : �rassuk ki a standard outputra sorfolytonosan
%%              az S1 sztringnek az S2 sztringben nem szerepl� karaktereit,
%%              az S1-beli sorrendj�knek megfelel�en!  (I eleme {1,2,3}).
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
| ?- put_string_diff_1("szilvaf�n","sz�l�"),
     put_string_diff_2("szilvaf�n","sz�l�"),
     put_string_diff_3("szilvaf�n","sz�l�").
ivaf�n
ivaf�n
ivaf�n
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

%% 2. K�sz�ts�nk tagolt bin�ris k�d (tbk) felismer� DCG-t! (10p)
%%
%% Mj.: A feladatot tiszta elemz� DCG-vel oldjuk meg, an�lk�l,
%%      hogy a nyelvtanban explicit Prolog h�v�st alkalmazn�nk!
%%
%% Def.: Egy sztring tbk, akkor ha
%%       - �b�c�j�t a "0_1" sztring karakterei adj�k,
%%       - nem�res,
%%       - els� �s utols� eleme nem az al�h�z�sjel k�dja,
%%       - sehol sincs benne k�t al�h�z�sjel k�d egym�s ut�n.
%%
%% El�f: Bs val�di lista.
%%
%% tbk(Bs,Ms) :- a Bs-Ms d-lista egy tbk sztring reprezent�ci�ja.
%%
%% LI: O(|Bs|), az �sszes megold�sra egy�tt.
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

tbk --> bit, tbkv�g.

bit --> "0".
bit --> "1".

tbkv�g --> "_", tbk.
tbkv�g --> tbk.
tbkv�g --> "".

%% 3. M�dos�tsuk az el�z� megold�st �gy, hogy a tbk sztring,
%%    mint eg�sz sz�m �rt�k�t is meghat�rozzuk!
%%    Tegy�k teljesen determinisztikuss� a k�dot! (10p)
%%
%% Mj.: A program minden mondata maradjon DCG szab�ly,
%%      de most m�r explicit Prolog h�v�sokra is sz�ks�g lesz.
%%
%% El�f: Bs val�di lista.
%%
%% tbk1(Sz�m,Bs,Ms) :- Bs tbk sztring, Ms=[],
%%                    Sz�m pedig a Bs bin�ris sz�m integer �rt�ke. 
%%
%%
%% LI: O(|Bs|)
%%
%% | ?- tbk1(�rt�k,"1","").
%% �rt�k = 1
%% | ?- tbk1(�rt�k,"0","").
%% �rt�k = 0
%% | ?- tbk1(�rt�k,"101","").
%% �rt�k = 5
%% | ?- tbk1(�rt�k,"010","").
%% �rt�k = 2
%% | ?- tbk1(�rt�k,"_","").
%% no
%% | ?- tbk1(�rt�k,"","").
%% no
%% | ?- tbk1(�rt�k,"10_01","").
%% �rt�k = 9

tbk1(Sz�m) --> bit(B), !, tbk1_v�g(B,Sz�m).

tbk1_v�g(X,Sz�m) --> "_", !, tbk2(X,Sz�m).
tbk1_v�g(X,Sz�m) --> bit(B), !, {X1 is 2*X+B}, tbk1_v�g(X1, Sz�m).
tbk1_v�g(X,X) --> [].
tbk1_v�g(_X,_Sz�m) --> [_C], !, {fail}.

tbk2(X,Sz�m) --> bit(B), !, {X1 is 2*X+B}, tbk1_v�g(X1, Sz�m).

bit(0) --> "0".
bit(1) --> "1".

% 1. Adjuk meg egy tetsz�leges sz�mhoz a megfelel� hat�rozott n�vel�t! (15p)
%
% El�f: N eg�sz sz�m.
% Mj.: Ha N nem eg�sz sz�m, hi�suljon meg a predik�tumh�v�s!
%
% az_a(N,AzA) :- az N eg�sz sz�mhoz tartoz� megfelel�
%     magyar nyelv� hat�rozott n�vel� ('Az ' ill. 'A ') AzA.
%
% Seg�ts�g: Az al�bbi C++ k�d megoldja a probl�m�t.
%
%%% // Visszaadja a megfelel� hat�rozott n�vel�t. 
%%% // Szerz�: �sv�nyi Tibor, 2008. okt�ber 15.
%%% string nevelo(int n){
%%%   if( n<=0 ) return "A ";
%%%   while( n >= 1000 ) n /= 1000;
%%%   if( n==1)  return "Az ";
%%%   while( n >= 10 ) n /= 10;
%%%   if( n==5)  return "Az ";
%%%   return "A ";
%%% }
%
% LI ~=< ha(N>=10) lg(N) k�l�nben 1.
%
% �tlet: A Prolog k�dban a ciklusokat
%        egy rekurz�v seg�d-predik�tummal helyettes�tj�k.
/*
| ?- az_a(1,Egy), az_a(10,T�z), az_a(100,Sz�z), az_a(1024,Ezer_),
     az_a(12145,T�zezer_), az_a(157246,Sz�zezer_), az_a(1257368,Egymilli�_).
Egy = 'Az ',    T�z = 'A ',    Sz�z = 'A ',    Ezer_ = 'Az ',
T�zezer_ = 'A ',    Sz�zezer_ = 'A ',     Egymilli�_ = 'Az '
| ?- az_a(5,�t), az_a(53,�tven_), az_a(506,�tsz�z_), az_a(5123,�tezer_).
�t = 'Az ',     �tven_ = 'Az ',    �tsz�z_ = 'Az ',    �tezer_ = 'Az '
| ?- az_a(7,H�t), az_a(4,N�gy), az_a(8,Nyolc), az_a(16,Tizen_), az_a(32,Harminc_), az_a(64,Hatvan_), az_a(128,Sz�z_), az_a(256,K�tsz�z_), az_a(512,�tsz�z_), az_a(1024,Ezer_), az_a(2048,K�tezer_). 
H�t = 'A ',    N�gy = 'A ',    Ezer_ = 'Az ',    Nyolc = 'A ',
Sz�z_ = 'A ',    K�tezer_ = 'A ',    K�tsz�z_ = 'A ',    Tizen_ = 'A ',
Harminc_ = 'A ',    �tsz�z_ = 'Az ',    Hatvan_ = 'A '
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

% 2. �rjunk tesztk�rnyezetet az el�z� programhoz! (15p)
%
% El�f: A be�rt eg�sz sz�mok az �br�zolhat� tartom�nyba esnek.
%
% az_a_teszt :-
%    "Ciklusban" bek�r a standard inputr�l egy-egy eg�sz sz�mot.
%    Ha nem eg�sz sz�mot kap, elk�sz�n a program.
%    Ha eg�sz sz�mot kap, ki�rja a megfelel� sz�veget:
%    "Az U sz�mot olvastam be." vagy
%    "A V sz�mot olvastam be.", ahol U ill. V a beolvasott sz�m.
%
/*
| ?-  az_a_teszt.
K�rem az eg�sz sz�mokat,
amik el� hat�rozott n�vel�t kell tennem!
(Ha nem eg�sz sz�mot kapok, elk�sz�n�k.)
|: 1
Az 1 sz�mot olvastam be.
|: 10
A 10 sz�mot olvastam be.
|: 100
A 100 sz�mot olvastam be.
|: 1024
Az 1024 sz�mot olvastam be.
|: 2
A 2 sz�mot olvastam be.
|: 2014
A 2014 sz�mot olvastam be.
|: 42567432
A 42567432 sz�mot olvastam be.
|: 5273510
Az 5273510 sz�mot olvastam be.
|: 19352
A 19352 sz�mot olvastam be.
|: 163032
A 163032 sz�mot olvastam be.
|: 1947263
Az 1947263 sz�mot olvastam be.
|: 16243102
A 16243102 sz�mot olvastam be.
|: 163542310
A 163542310 sz�mot olvastam be.
|: 1321035475
Az 1321035475 sz�mot olvastam be.
|: Folytassam?
Viszl�t!
yes
% source_info
| ?- az_a_teszt.
K�rem az eg�sz sz�mokat,
amik el� hat�rozott n�vel�t kell tennem!
(Ha nem eg�sz sz�mot kapok, elk�sz�n�k.)
|: 3.14159265358979323846
Viszl�t!
yes
*/

az_a_teszt :-
    write(user,'K�rem az eg�sz sz�mokat,\n'),
    write(user,'amik el� hat�rozott n�vel�t kell tennem!\n'),
    write(user,'(Ha nem eg�sz sz�mot kapok, elk�sz�n�k.)\n'),
    repeat,
      sorbeolv(user,Vs),
      ( catch( (number_codes(N,Vs), integer(N)),_,fail ) ->
	kiir(N), fail
      ; write(user,'Viszl�t!\n')
      )
      -> true.        %% vagy , true.

sorbeolv(S,Vs) :-
    get_code(S,X),
    ( X==0'\n -> Vs=[]
    ; Vs=[X|Us], sorbeolv(S,Us)
    ).

kiir(N) :- az_a(N,AzA), write(user,AzA), write(user,N), write(user,' sz�mot olvastam be.\n').


% 3. Defini�ljuk elemz� DCG-vel a divide/5 predik�tumot a qs/4 programban! (15p)
%
% El�f: Xs val�di lista.
%
% divide(Ls,Gs,X,Xs,[]) :- az Xs X-n�l nagyobb elemeit Gs,
%     a t�bbit Ls tartalmazza, a standard rendez�s szerint.
%
% Mj.: Ezt a feladatot olyan elemz� DCG nyelvtannal KELL megoldani, ami
%     nem tartalmaz felhaszn�l�i/ k�nyvt�ri predik�tumra vonatkoz� Prolog h�v�st.
%     Tartalmazhat viszont be�p�tett elj�r�sokra vonatkoz� Prolog h�v�sokat.
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

% 1. Sz�moljuk ki egy bin�ris fa egyes�lyait! (10p)
%
% El�f: T val�di bin�ris fa, ahol a kis 'o' atom az �res fa,
%   a nem�res f�k pedig t(BalR�szFa,Gy�k�r,JobbR�szFa) alak�ak.
%
% egyens�lyok(T,M,TS) :- T magass�ga M,
%     T m�solata a TS, annyi k�l�nbs�ggel, hogy TS-ben a
%     nem�res r�szf�k t(BalR�szFa,Gy�k�r,Egyens�ly,JobbR�szFa) alak�ak, ahol
%     Egyens�ly a jobboldali �s a baloldali r�szfa magass�g�nak k�l�nbs�ge.
%
% LI ~=< a T r�szf�inak sz�ma.
/*
| ?- egyens�lyok(t(t(o,2,o),3,o),M,TS).
M = 1,
TS = t(t(o,2,0,o),3,-1,o)
| ?- egyens�lyok(t(o,1,t(t(o,2,o),3,o)),M,TS).
M = 2,
TS = t(o,1,2,t(t(o,2,0,o),3,-1,o))
*/

/*
%egyens�lyok(o,M,TS) :- TS=t()
egyens�lyok(T,M,TS) :- egyens�lyok_app(T,0,0,0,TS).

egyens�lyok_app(o,Mb,Mj,N,TS).
egyens�lyok_app(t(B,X,J),Mb,Mj,N,TS) :-
    ( B==o, J==o -> N is Mb-Mj
    ; Mb1 is Mb+1, egyens�lyok_app(B,Mb1,Mj,N,TS),
      Mj1 is Mj+1, egyens�lyok_app(J,Mb,Mj1,N,TS)
    ).
*/

egyens�lyok(o,-1,o).
egyens�lyok(t(B,X,J),M,t(B1,X,E,J1)) :-
    egyens�lyok(B,BM,B1), egyens�lyok(J,JM,J1),
    M is 1+max(BM,JM), E is JM-BM.

% 2. Ford�tsunk meg egy val�di list�t DCG-vel! (10p)
%
% El�f: Xs val�di lista.
%
% reverse(Xs) --> Xs megford�t�sa.
%
% Mj.: Ezt a feladatot tiszta generat�v DCG nyelvtannal KELL megoldani,
%      ami teh�t nem tartalmaz Prolog h�v�st.
%
% LI ~=< a T bin�ris fa m�rete.
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

% 1. Melyik lista reprezent�lja egy term egy �g�t? (20p)
%
% El�f: A (T,Xs) p�ros nem tartalmaz duplik�lt ismeretlent.
%
% term_�g(T,Xs) :-
%     Ha a h�v�skor T �sszetett term, az Xs val�di lista els� eleme
%     a T f�ggv�nyszimb�lum-neve, marad�ka pedig a T
%     valamelyik param�ter�hez tartoz� term-�g.
%     Ha a h�v�skor T egyszer� term, az Xs val�di lista egyetlen eleme T.
%
% LI ~=< a T r�sztermjeinek sz�ma (az �sszes megold�sra egy�tt).
/*
| ?- term_�g(t(a(X),2,b(c(3,d),4.0)),Xs).
Xs = [t,b,4.0] ? ;    Xs = [t,b,c,d] ? ;    Xs = [t,b,c,3] ? ;
Xs = [t,2] ? ;        Xs = [t,a,X] ? ;
no
| ?- term_�g(t(a(X),2,b(c(3,d),4.0)),[Y,a,p]).
X = p,    Y = t
| ?- term_�g(X,Xs).
Xs = [X]
| ?- term_�g(t(a(X),2,b(c(3,d),4.0)),[Y,a,p(Z)]).
X = p(Z),    Y = t
*/

%t(
%a(X), 2, b (c(3,d), 4.0)

term_�g(T,Xs) :-
    ( compound(T) -> functor(T,Tneve,I), Xs=[Tneve|Us], term_gyerek(T,I,Gyerekterm), term_�g(Gyerekterm,Us)
    ; Xs=[T]
    ).

term_gyerek(T,I,Gyerekterm) :-
    arg(I,T,Gyerekterm)
    ; I > 1, I1 is I-1, term_gyerek(T,I1,Gyerekterm).

% 2. Adjuk meg egy bin�ris fa inorder bej�r�s�t generat�v DCG-vel! (10p)
%
% El�f: T val�di bin�ris fa, ahol a kis 'o' atom az �res fa,
%   a nem�res f�k pedig t(BalR�szFa,Gy�k�r,JobbR�szFa) alak�ak.
%
% inorderDCG(T) --> T inorder bej�r�sa.
%
% Mj.: Ezt a feladatot generat�v DCG nyelvtannal KELL megoldani.
%
% LI ~=< a T bin�ris fa m�rete.
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

% 3. Mely bin�ris f�k inorder bej�r�sa lehet egy adott lista? (10p)
%
% Jel�l�s: az �res f�t a kis 'o' atom reprezent�lja,
%   a nem�res f�k pedig t(BalR�szFa,Gy�k�r,JobbR�szFa) alak�ak.
%
% El�f: N >=-1 eg�sz sz�m, Is val�di lista.
%
% inorder_DCG(N,T,Is,[]) :- A legfeljebb N m�lys�g� T bin�ris fa
%                           inorder bej�r�sa az Is val�di lista.
%
% Mj.: Ezt a feladatot elemz� DCG nyelvtannal aj�nlott megoldani.
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
| ?- inorder_DCG(1,T,[A,B,C],Ys). %! Ld. a feladat al�bbi �ltal�nos�t�s�t!
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

% 3. Melyek egy lista r�szsorozatai? (20p)
%
% El�f: Ys val�di lista.
%
% r�szsorozat(Xs,Ys,[]) :- Xs az Ys nem okvetlen�l folytonos r�szsorozata.
%
% Mj.: Ezt a feladatot tiszta elemz� DCG nyelvtannal KELL megoldani.
%      ami teh�t nem tartalmaz Prolog h�v�st.
%      Ha Ys elemei k�l�nb�z� termek, ne adjon duplik�lt megold�st! (-6p)
%
/*
| ?- r�szsorozat(Xs,[],[]).
Xs = [] ? ;
no
| ?- r�szsorozat(Xs,[A],[]).
Xs = [] ? ;    Xs = [A] ? ;
no
| ?- r�szsorozat(Xs,[A,B],[]).
Xs = [] ? ;    Xs = [B] ? ;    Xs = [A] ? ;    Xs = [A,B] ? ;
no
| ?- r�szsorozat(Xs,[A,B,C],[]).
Xs = [] ? ;     Xs = [C] ? ;      Xs = [B] ? ;      Xs = [B,C] ? ;
Xs = [A] ? ;    Xs = [A,C] ? ;    Xs = [A,B] ? ;    Xs = [A,B,C] ? ;
no
*/

r�szsorozat([]) --> list.
r�szsorozat([X|Xs]) --> [X], r�szsorozat(Xs).
r�szsorozat(Xs) --> [_], r�szsor(Xs).

r�szsor([X|Xs]) --> [X], r�szsorozat(Xs).
r�szsor(Xs) --> [_], r�szsor(Xs).

list --> [_], list.
list --> [].

% �rjuk le a lista hatv�nyoz�s fogalm�t! (40p)
%
% El�f: A "Ts val�di lista", "Xs val�di lista", "N nemnegat�v eg�sz"
%       �ll�t�sok k�z�l legal�bb kett� igaz.
%
% hatv�ny(N,Xs,Ts) :-
%     az Xs val�di lista (N-1)-szer �nmaga ut�n f�zve adja a Ts val�di list�t.
%     ( N=0 eset�n Ts �res lista, N=1 eset�n Ts=Xs,
%       N=2 eset�n append(Xs,Xs,Ts), ... )
%
% LI ~=< ( N * Xs hossza ) + Ts hossza.
/*
| ?- hatv�ny(0,[A,B,C],Ts).
Ts = []
| ?- hatv�ny(1,[A,B,C],Ts).
Ts = [A,B,C]
| ?- hatv�ny(2,[A,B,C],Ts).
Ts = [A,B,C,A,B,C]
| ?- hatv�ny(3,[A,B,C],Ts).
Ts = [A,B,C,A,B,C,A,B,C]
| ?- hatv�ny(4,[],Ts).
Ts = []
| ?- hatv�ny(5,[X],Ts).
Ts = [X,X,X,X,X]
*/

/*
hatv�ny(N,Xs,Ts) :-
    ha N=0 akkor Ts=[]
    ha N integer �s N>0, akkor akkor hatv�ny_kisz�mol(Xs)                   --ha Ts=[], �s Xs val�di lista, akkor hatv�ny_kisz�mol(Xs)
    ; ha Xs=[], akkor Ts=[]                                                 --�s Ts val�di lista akkor hozz�f�z(N,Xs,Ts)
    ; ha Ts=[] akkor N=0
    ; ha XS �s Ts is val�di lista, akkor Nx Xs hozza �s Nt Ts hossza, �s N is Nt//Nx, �s hozz�f�z(N,Xs,Ts)
*/

hatv�ny(N,Xs,Ts) :-
    ( N == 0 -> Ts=[]
    ; integer(N) -> N>0, hatv�ny_kisz�mol(N,Xs,Ts)
    ; Xs==[] -> Ts=[]
    ; Ts==[] -> N=0
    ; val�di_lista(Xs), val�di_lista(Ts),
      length(Xs,NX), length(Ts,NT),
      NT mod NX =:= 0, N is NT//NX,
      hozz�f�z(N,Xs,Ts)
    ).

val�di_lista(Xs) :-
    ( Xs==[] -> true
    ; nonvar(Xs), Xs=[_Y|Ys], val�di_lista(Ys)
    ).

hatv�ny_kisz�mol(N,Xs,Ts) :-
    ( val�di_lista(Xs) -> true
    ; val�di_lista(Ts), length(Ts,NT),
      NX is NT // N, NT mod N =:= 0, length(Xs,NX)
    ),
    hozz�f�z(N,Xs,Ts).

hozz�f�z(N,Xs,Ts) :- el�f�z(N,Xs,Xs,Ts).

%% append(L1,L2,Eredm�nylista).

el�f�z(N,Xs,Bs,Ts) :-
    ( N>1 -> N1 is N - 1, append(Xs,Bs,As), el�f�z(N1,Xs,As,Ts)
    ; Ts=Bs
    ).

/*
| ?- hatv�ny(5,Xs,[A,B,A,B,A,B,A,B,A,B]).
Xs = [A,B]
| ?- hatv�ny(4,Xs,[A,B,A,B,A,B,A,B,A,B]).
no
| ?- hatv�ny(3,Xs,[A,B,A,B,A,B,A,B,A,B]).
no
| ?- hatv�ny(3,Xs,[A,B,A,B,A,B,A,B,A]).
B = A,    Xs = [A,A,A]
| ?- A=a, B=b, hatv�ny(3,Xs,[A,B,A,B,A,B,A,B,A]).
no
| ?- hatv�ny(2,Xs,[A,B,A,B,A,B,A,B,A,B]).
B = A,    Xs = [A,A,A,A,A]
| ?- hatv�ny(1,Xs,[A,B,A,B,A,B,A,B,A,B]).
Xs = [A,B,A,B,A,B,A,B,A,B]
| ?- hatv�ny(0,Xs,[A,B,A,B,A,B,A,B,A,B]).
no
| ?- hatv�ny(0,Xs,[]).
true
| ?- hatv�ny(0,Xs,[A]).
no
| ?- hatv�ny(N,[],[A,B,A,B,A,B,A,B]).
no
| ?- hatv�ny(N,[],[]).
true
| ?- hatv�ny(N,[A],[B]).
B = A,    N = 1
| ?- hatv�ny(N,[X],[A,B,A,B,A,B,A,B]).
B = A,    N = 8,    X = A
| ?- hatv�ny(N,[X,Y],[A,B,A,B,A,B,A,B]).
N = 4,    X = A,    Y = B
| ?- hatv�ny(N,[X,Y,Z],[A,B,A,B,A,B,A,B]).
no
| ?- hatv�ny(N,[V,X,Y,Z],[A,B,A,B,A,B,A,B]).
N = 2,    V = A,    X = B,    Y = A,    Z = B
| ?- hatv�ny(N,[T,U,V,X,Y,Z],[A,B,A,B,A,B,A,B]).
no
| ?- hatv�ny(N,[X,Y,Z],[A,B,A]).
N = 1,    X = A,    Y = B,    Z = A
| ?- hatv�ny(N,[X,Y,Z],[A,B]).
no
| ?- hatv�ny(N,[X,Y,Z],[]).
N = 0
| ?- hatv�ny(N,[a,Y,Z],[b,B,A]).
no
*/

%% 1. �rassunk ki egy termet, mint f�ggv�nykifejez�st! (30p)
%%
%% El�f: A standard output el�rhet�.
%%
%% typenonf(T) : Ki�ratja a T termet a standard outputra 
%%               a typenonf_test(T) predik�tumnak megfelel� m�don.
%% 
%% LI: O(T term r�sztermjeinek sz�ma)
%%
%% Mj.: A k�rat�shoz a Prolog IO-hoz kapcsol�d�, a SICStus Prolog rendszerben
%%      adott predik�tumok k�z�l csak a write/2 �s a writeq/2 haszn�lhat� fel.

%% Term T is written in standard functional notation
%% instead of using operators and/or list notation.
%% Atoms and functors in T are quoted where necessary
%% to make the result acceptable as input to 'read/1'.
%% (A read/1-nek sz�ks�ges lez�r� pont �s feh�r sz�k�z nem ker�l ki�rat�sra.)
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

% 1. �rjuk le DCG-vel a dadog� list�k fogalm�t! (15p)
%
% El�f: Ts alaplista vagy Xs alaplista
%
% dadog�(Xs,Ts,Rs) :- dadog(Xs,Ts,Rs). % ahol:
%
dadog_(Xs,Ts,Rs) :-
    ( ground(Xs) -> append(Xs,Xs,XsXs), append(XsXs,Rs,Ts)
    ; append(XsXs,Rs,Ts), append(Xs,Xs,XsXs)
    ).
%
% Mj.: dadog�/3-at tiszta DCG-vel kell meg�rni, azaz nem tartalmazhat
%      expliciten sem Prolog predik�tumot, sem Prolog h�v�st.
%      (Generat�v �s elemz� ir�nyban is ugyanaz a k�d m�k�dik majd.)
/*
| ?- dadog�(Xs,[a,b,a,b,a,b,a],Rs).
Rs = [a,b,a,b,a,b,a],    Xs = [] ? ;
Rs = [a,b,a],    Xs = [a,b] ? ;
no
| ?- dadog�(Xs,[a,b,a,b,a,b,a,b],Rs).
Rs = [a,b,a,b,a,b,a,b],    Xs = [] ? ;
Rs = [a,b,a,b],    Xs = [a,b] ? ;
Rs = [],    Xs = [a,b,a,b] ? ;
no
| ?- dadog�([a,b,c],Rs,Ts).
Rs = [a,b,c,a,b,c|Ts]
| ?- dadog�([],Rs,Ts).
Ts = Rs
*/

dadog�(Xs) --> dadog(Xs), dadog(Xs).

dadog([X|Xs]) --> [X], dadog(Xs).
dadog([]) --> [].

% 2. �rjuk meg az unio_sort programj�t generat�v DCG-vel!
%
% Mj.: Duplik�lt listaelem alatt a lista egy olyan elem�t �rtj�k,
%      amely k�s�bb is el�fordul a list�ban. (K db. el�fordul�s (K>=1)
%      eset�n teh�t (K-1)-szer ad�dik ki az elem megold�sk�nt.)
%
% El�f: Xs alaplista.
%
% member2(X,Xs) :- X az Xs duplik�lt eleme.
/*
| ?- member2(X,[]).
no
| ?- member2(X,[1,2,3]).
no
| ?- member2(X,[1,2,3,2,2,1]).
X = 1 ? ;    X = 2 ? ;    X = 2 ? ;    no
*/

% Megold�s:

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

%% 2. Az aktu�lis input sor decim�lis sz�mjegyek Prolog list�ja-e? (50p)
%%
%% Mj.: El�sz�r olvassuk be az aktu�lis input sort karakterk�dok egy
%%      list�j�ba, majd elemezz�k DCG-vel, hogy megfelel�-e a szintaxisa,
%%      mint decim�lis sz�mjegyek val�di Prolog list�ja!
%%      (Ha az elemz�st nem DCG-vel oldja meg, max. 25 pont.)
%%
%% El�f.: Az S logikai n�vvel megnyitott input sz�vegfile olvashat�.
%%
%% digit_list_line(S,Ds) :-
%%       Az S input sz�vegfile aktu�lis sor�t beolvasva, az a
%%       Ds val�di Prolog list�nak bizonyul, aminek elemei decim�lis sz�mjegyek.
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

digit_list(Ds) --> digitlist_kezd, digitlist_v�g(Ds).

digitlist_kezd --> �res_kar, [0'[], �res_kar.

digitlist_v�g([]) --> [0']], !.
digitlist_v�g([D|Ds]) --> sz�mok(D), �res_kar, digit_sor(Ds), �res_kar.

digit_sor([]) --> [0']], !.
digit_sor([D|Ds]) --> [0',], �res_kar, sz�mok(D), �res_kar, digit_sor(Ds).

sz�mok(D) --> [C], {C >= 0'0, C =< 0'9, D is C-0'0}.

�res_kar --> [0' ], !, �res_kar.
�res_kar --> [].

get_lines2(S,Cs) :-
    get_code(S,C),
    ( C == 0'\n -> Cs=[]
    ; Cs=[C|Us], get_lines(S,Us)
    ).


%% 2. �rjuk meg az unio_sort programj�t generat�v DCG-vel!

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



