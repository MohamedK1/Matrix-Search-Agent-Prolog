:- use_module(KB.pl).

% returns true if X,Y are valid position in the grid
isValid(X,Y):-
    grid(R,C),X>=0,X=<R,Y>=0,Y=<C.


% move(X,Y,X,Y,S0,S0).
% move(X,Y1,X,Y2,result(right,S),S0):- Y1<Y2,Ny is Y1+1,move(X,Ny,X,Y2,S,S0).
% move(X,Y1,X,Y2,result(left,S),S0):- Ny is Y1-1,move(X,Ny,X,Y2,S,S0).
% move(X1,Y1,X2,Y2,result(down,S),S0):- X1<X2,Nx is X1+1,move(Nx,Y1,X2,Y2,S,S0).
% move(X1,Y1,X2,Y2,result(up,S),S0):- Nx is X1-1,move(Nx,Y1,X2,Y2,S,S0).

% solve(_,_,[],S,S).
% solve(InitX,InitY,[[X,Y]|T],S0,S):-
% booth(BoothX,BoothY),
% move(InitX,InitY,X,Y,S_after_going_to_hostage,S0),
% move(X,Y,BoothX,BoothY,SafterDrop,result(carry,S_after_going_to_hostage)),
% solve(BoothX,BoothY,T,result(drop,SafterDrop),S).


% isGoal(S):-
%     neo_loc(NeoX,NeoY), hostages_loc(L),solve(NeoX,NeoY,L,s0,S).

% solve(X,Y,[],CarriedHostages,_):-
% solve(X,Y,[H|T],CarriedHostages,C_so_far):-

% we made the drop the last action since, the agent must drop the final hostage
% TODO may we have a case that there is a grid with no hostages.
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

%neoArrived(X,Y,Hostages,Capacity,State) returns true if Neo is in X,Y in state State with current actual capacity Capacity
% and current hostages list Hostages at the current state State.

% Base case where X,Y will be neo's inital location, L is the list of all hostages in the grid, C is the current capacity.
neoArrived(X,Y,L,C,s0):-neo_loc(X,Y),hostages_loc(L),capacity(C).

% if the action is drop we check the ability of neo to drop a hostage.
neoArrived(X,Y,L,FullCapcity,result(drop,S)):-canDrop(X,Y,S),
capacity(FullCapcity),
neoArrived(X,Y,L,C,S),C>=0.

neoArrived(X,Y,UncarriedHostages,NewC,result(carry,S)):- 
            canCarry(X,Y,S),
            skip(X,Y,Hostages,UncarriedHostages),
            neoArrived(X,Y,Hostages,C,S),C>0,NewC is C-1.

neoArrived(X,Y,L,C,result(A,S)):- directions(A,Dx,Dy),OldX is X-Dx,OldY is Y-Dy,
                            isValid(OldX,OldY),neoArrived(OldX,OldY,L,C,S).



%returns true if Neo have carriedHostages at state S and can drop them at X,Y if this is the location of the booth.
canDrop(X,Y,S):-booth(X,Y),haveCarriedHostages(S).


%Neo have carried hostages if the sequence of actions contains at least one carry not followed by a drop.
haveCarriedHostages(result(carry,S)).
haveCarriedHostages(result(A,S)):-  ( A\=drop),(A\=carry),haveCarriedHostages(S).

%return true if there is a hostage in X,Y in state S.
canCarry(X,Y,S):-hostages_loc(L),canCarryHelper(X,Y,L,S).
canCarryHelper(X,Y,[[X,Y]|T],S).
canCarryHelper(X,Y,[_|T],S):-canCarryHelper(X,Y,T,S).


%skip(X,Y,L,Rem)  the location X,Y from the List L and returns the remaining list Rem
skip(X,Y,[[X,Y]|T],T).
skip(X,Y,[H|T],[H|Out]):-skip(X,Y,T,Out).











