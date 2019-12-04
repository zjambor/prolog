%% Adottak:
%% A természetes számok s-szám reprezentációja:
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

hatv�ny(_,0,s(0)).
hatv�ny(0,s(_X),0).
%%%hatv�ny(Y,s(0),Y).
hatv�ny(X,s(Y),H) :- hatv�ny(X,Y,C), times(X,C,H).

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


