:- use_module(KB.pl).

grid(4,4).
neo_loc(0,0). 
hostages_loc([[1,1],[1,2]]). 
booth(0,2). 
capacity(1).



isValid(X,Y):-
    grid(R,C),X>=0,X=<R,Y>=0,Y=<C.


move(X,Y,X,Y,S0,S0).
move(X,Y1,X,Y2,result(right,S),S0):- Y1<Y2,Ny is Y1+1,move(X,Ny,X,Y2,S,S0).
move(X,Y1,X,Y2,result(left,S),S0):- Ny is Y1-1,move(X,Ny,X,Y2,S,S0).
move(X1,Y1,X2,Y2,result(down,S),S0):- X1<X2,Nx is X1+1,move(Nx,Y1,X2,Y2,S,S0).
move(X1,Y1,X2,Y2,result(up,S),S0):- Nx is X1-1,move(Nx,Y1,X2,Y2,S,S0).

solve(_,_,[],S,S).
solve(InitX,InitY,[[X,Y]|T],S0,S):-
booth(BoothX,BoothY),
move(InitX,InitY,X,Y,S_after_going_to_hostage,S0),
move(X,Y,BoothX,BoothY,SafterDrop,result(carry,S_after_going_to_hostage)),
solve(BoothX,BoothY,T,result(drop,SafterDrop),S).


% isGoal(S):-
%     neo_loc(NeoX,NeoY), hostages_loc(L),solve(NeoX,NeoY,L,s0,S).

reverse(X,X_reversed):-reverse(X,X_reversed,[]).
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

% solve(X,Y,[],CarriedHostages,_):-
% solve(X,Y,[H|T],CarriedHostages,C_so_far):-



isGoal(result(drop,S)):-
booth(X,Y),neoArrived(X,Y,[],_,S).

directions(up,-1,0).
directions(down,1,0).
directions(left,0,-1).
directions(right,0,1).

% neoArrived(X,Y,s0):-neo_loc(X,Y).
% neoArrived(X,Y,result(drop,S)):-canDrop(X,Y,S),neoArrived(X,Y,S).
% neoArrived(X,Y,result(carry,S)):- canCarry(X,Y,S),neoArrived(X,Y,S).
% neoArrived(X,Y,result(A,S)):- directions(A,Dx,Dy),OldX is X-Dx,OldY is Y-Dy,
%                             isValid(OldX,OldY),neoArrived(OldX,OldY,S).


neoArrived(X,Y,L,C,s0):-neo_loc(X,Y),hostages_loc(L),capacity(C).

neoArrived(X,Y,L,FullCapcity,result(drop,S)):-canDrop(X,Y,S),
capacity(FullCapcity),
neoArrived(X,Y,L,C,S),C>=0.

neoArrived(X,Y,UncarriedHostages,NewC,result(carry,S)):- 
            canCarry(X,Y,S),
            skip(X,Y,Hostages,UncarriedHostages),
            neoArrived(X,Y,Hostages,C,S),C>0,NewC is C-1.

neoArrived(X,Y,L,C,result(A,S)):- directions(A,Dx,Dy),OldX is X-Dx,OldY is Y-Dy,
                            isValid(OldX,OldY),neoArrived(OldX,OldY,L,C,S).



canDrop(X,Y,S):-booth(X,Y),haveCarriedHostages(S).


haveCarriedHostages(result(carry,S)).
haveCarriedHostages(result(A,S)):-  ( A\=drop),(A\=carry),haveCarriedHostages(S).


canCarry(X,Y,S):-hostages_loc(L),canCarryHelper(X,Y,L,S).
canCarryHelper(X,Y,[[X,Y]|T],S).
canCarryHelper(X,Y,[_|T],S):-canCarryHelper(X,Y,T,S).


skip(X,Y,[[X,Y]|T],T).
skip(X,Y,[H|T],[H|Out]):-skip(X,Y,T,Out).











