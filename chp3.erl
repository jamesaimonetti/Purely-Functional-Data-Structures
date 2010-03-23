-module(chp3).

-compile(export_all).

%% exercise 3.1, pg 18
%% prove the right spine of a leftist heap of size n has at most [log(n+1)] elements

%% Heap: {Rank, N, Lheap, Rheap}

merge(H, {}) -> H;
merge({}, H) -> H;
merge({_RankA, X, La, Ra}, {_RankB, Y, _Lb, _Rb}=Hb) when X =< Y ->
    makeT(X, La, merge(Ra, Hb));
merge(Ha, {_RankB, Y, Lb, Rb}) ->
    makeT(Y, Lb, merge(Ha, Rb)).

%% rank
rank({}) -> 0;
rank({Rank, _X, _Lheap, _Rheap}) -> Rank.

%% calculates the rank of a Heap node and swaps children if necessary
makeT(X, Lheap, Rheap) ->
    Lrank = rank(Lheap),
    Rrank = rank(Rheap),
    if Lrank >= Rrank ->
            {Rrank + 1, X, Lheap, Rheap};
       true -> { Lrank + 1, X, Rheap, Lheap }
    end.

%% insert creates a singleton heap and merges it with the main heap
insert(X, H) -> merge({0, X, {}, {}}, H).

findMin({_Rank, X, _Lheap, _Rheap}) -> X;
findMin(_) -> throw("Error: Bad Heap passed").

deleteMin({_Rank, _X, Lheap, Rheap}) ->
    merge(Lheap, Rheap);
deleteMin(_) -> throw("Error: Bad Heap passed").

%% exercise 3.2, pg 19
%% define insert directly without calling merge
calc_rank({}) -> 0;
calc_rank({_Rank, _X, _Left, Right}) ->
    calc_rank(Right, 1).

calc_rank({}, Rank) -> Rank - 1;
calc_rank({_Rank, _X, _Left, {}}, R) -> R;
calc_rank({_Rank, _X, _Left, Right}, R) ->
    calc_rank(Right, R).

insert2(X, {}) -> {0, X, {}, {}};
insert2(X, {_Rank, Y, _Lheap, _Rheap}=H) when X =< Y ->
    {1, X, H, {}};
insert2(X, {Rank, Y, Lheap, Rheap}) ->
    NewRheap = insert2(X, Rheap),
    NewRank = calc_rank(NewRheap),
    LeftRank = calc_rank(Lheap),
    if 
        LeftRank >= NewRank ->
            {NewRank + 1, Y, Lheap, NewRheap};
        true ->
            {Rank + 1, Y, NewRheap, Lheap}
    end.

%% exercise 3.3, pg. 19
%% fromList produces a leftist heap from an unordered list of elements
%% merge heaps in log n passes instead of 1 foldl/r pass
fromList(L) ->
    Heaps = lists:map(fun(X) -> insert(X, {}) end, L),
    build(Heaps, []).

build([], [Heap]) -> Heap;
build([Heap], []) -> Heap;
build([], [Heap1, Heap2 | Heaps]) ->
    build( [merge(Heap1, Heap2) | Heaps], []);
build([H], [Heap | Heaps]) -> 
    build([merge(H, Heap) | Heaps], []);
build([H1, H2 | T], Heap) -> 
    build(T, [merge(H1, H2) | Heap]).
