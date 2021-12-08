
:- include('KB.pl').



goal(S):-
booth(X,Y),hostages_loc(L),neoArrived(X,Y,L,[],_,S).

directions(up,-1,0).
directions(down,1,0).
directions(left,0,-1).
directions(right,0,1).

%*********************************************************************%
%                     Succssor state axioms                           %
%*********************************************************************%




%neoArrived(X,Y,CarriedHostages,UndroppedHostages,Capacity,S)
%is true if Neo can reach X,Y in state S starting from S0   
% and carrying all Hostages in CarriedHostages with remaining undropped hostaged in Undropped


% Base case where X,Y will be neo's inital location, L is the list of all hostages in the grid, C is the current capacity.
neoArrived(X,Y,[],L,C,s0):-neo_loc(X,Y),hostages_loc(L),capacity(C).

% if the action is drop we check the ability of neo to drop a hostage.
neoArrived(X,Y,CarriedHostages,UndroppedHostages,FullCapcity,result(drop,S)):-
capacity(FullCapcity),hostages_loc(OriginalHostages),booth(X,Y),
newUndroppedHostages(OriginalHostages,CarriedHostages,UndroppedHostages),
neoArrived(X,Y,CarriedHostages,_,C,S),C>=0,haveCarriedHostages(S).


% NewCarriedHostages =  set of Hostages carried in state results(carry,S) (either have been carried before or currently on Neo's shoulder).
neoArrived(X,Y,NewCarriedHostages,UndroppedHostages,NewC,result(carry,S)):- 
    canCarry(X,Y,NewCarriedHostages),
    skip(X,Y,NewCarriedHostages,CarriedHostages),
    neoArrived(X,Y,CarriedHostages,UndroppedHostages,C,S),C>0,NewC is C-1.


neoArrived(X,Y,L,UndroppedHostages,C,result(A,S)):- directions(A,Dx,Dy),OldX is X-Dx,OldY is Y-Dy,
                            isValid(OldX,OldY),neoArrived(OldX,OldY,L,UndroppedHostages,C,S).



%Neo have carried hostages if the sequence of actions contains at least one carry not followed by a drop.
haveCarriedHostages(result(carry,S)).
haveCarriedHostages(result(A,S)):-  ( A\=drop),(A\=carry),haveCarriedHostages(S).



%*********************************************************************%
%                          Helper predicates                          %
%*********************************************************************%


%return true if there is a hostage in X,Y in state S.
canCarry(X,Y,Hostages):-hostages_loc(L),canCarryHelper(X,Y,Hostages).
canCarryHelper(X,Y,[[X,Y]|T]).
canCarryHelper(X,Y,[_|T]):-canCarryHelper(X,Y,T).


%skip(X,Y,L,Rem)  the location X,Y from the List L and returns the remaining list Rem
skip(X,Y,[[X,Y]|T],T).
skip(X,Y,[H|T],[H|Out]):-skip(X,Y,T,Out).


% contains(Item,List) true if List contains Item.
contains(Item,[Item|T]).
contains(Item,[H|T]):- contains(Item,T).


% newUndroppedHostages(OriginalHostages,CarriedHostages,UndroppedHostages).
% true when UndroppedHostages is the set of hostages in OriginalHostages but not in CarriedHostages.
newUndroppedHostages([],_,[]).
newUndroppedHostages([H|T],CarriedHostages,[H|Out]):- \+ contains(H,CarriedHostages),newUndroppedHostages(T,CarriedHostages,Out).
newUndroppedHostages([H|T],CarriedHostages,Out):- contains(H,CarriedHostages),newUndroppedHostages(T,CarriedHostages,Out).


% returns true if X,Y are valid position in the grid
isValid(X,Y):-
    grid(R,C),X>=0,X=<R,Y>=0,Y=<C.







