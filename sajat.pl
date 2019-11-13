list([]).
list([_X|Xs]) :- list(Xs).

member_(X,[X|_Xs]).
member_(X,[_X|Xs]) :- member_(X,Xs).

rev_app([],Ys,Ys).
rev_app([X|Xs],Ys,Zs) :- rev_app(Xs,[X,Ys],Zs).
