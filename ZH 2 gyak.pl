%% Adottak:
%% A termÃ©szetes szÃ¡mok s-szÃ¡m reprezentÃ¡ciÃ³ja:
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

hatvány(_,0,s(0)).
hatvány(0,s(_X),0).
%%%hatvány(Y,s(0),Y).
hatvány(X,s(Y),H) :- hatvány(X,Y,C), times(X,C,H).

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


