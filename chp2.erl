-module(chp2).

-compile(export_all).

%% exercise 2.1 pg 11
suffixes(L) ->
    suffixes(L, []).

suffixes([],R) -> lists:reverse([ [] | R]);
suffixes([H|T], R) ->
    suffixes(T, [ [H|T] | R]).

%% pg 12 BST member
bst_member(_X, {}) ->
    false;
bst_member(X, {A,Y,_B}) when X < Y ->
    bst_member(X, A);
bst_member(X, {_A,Y,B}) when X > Y ->
    bst_member(X, B);
bst_member(_X, _S) ->
    true.

%% pg 13 BST insert
bst_insert(X, {}) ->
    {{},X,{}};
bst_insert(X, {A,Y,B}) when X < Y ->
    {bst_insert(X, A), Y, B};
bst_insert(X, {A,Y,B}) when X > Y ->
    {A,Y,bst_insert(X, B)};
bst_insert(_X, S) -> S.

%% exercise 2.2, pg 14
%% bst_member takes, worst case, 2d comparisons, where d is the height of the tree
%% bst_member2 takes, worst case, d+1 comparisons
bst_member2(X, Tree) ->
    bst_member2(X, Tree, undefined).

%% we delay making the equality test until we are at an empty node
bst_member2(X, {}, X) ->
    true;
%% returns the guess, or undefined if X is < any value in the tree (all lefts traversed)
bst_member2(_X, {}, Y) ->
    {false, {guess, Y}};
%% one comparison is done and we traverse the left branch with Guess intact if X < Y
bst_member2(X, {A, Y, _B}, Guess) when X < Y ->
    bst_member2(X, A, Guess);
%% otherwise, we assume Y is maybe equal, but possibly greater than X, and
%% closer to X than the previous Guess, so we check the right branch with Y as Guess
bst_member2(X, {_A, Y, B}, _Guess) ->
    bst_member2(X, B, Y).

%% exercise 2.3, pg 15
%% use exceptions to avoid copying the tree when an element exists
bst_insert2(X, S) ->
    case bst_member(X, S) of
        false ->
            bst_insert(X, S);
        true ->
            S
    end.

%% exercise 2.4, pg 15
%% create an insert that does no unnecessary copying and at most performs d+1 comparisons
bst_insert3(X, S) ->
    bst_insert3(X, S, undefined).

bst_insert3(X, {}, X) ->
    throw('Unable to add element - exists');
bst_insert3(X, {}, _Guess) ->
    {{},X,{}};
bst_insert3(X, {A, Y, B}, Guess) when X < Y ->
    {bst_insert3(X, A, Guess), Y, B};
bst_insert3(X, {A, Y, B}, _Guess) ->
    {A, Y, bst_insert3(X, B, Y)}.

%% exercise 2.5, pg 15

%% exercise 2.5(a)
%% create a function that creates a BST with X in every node, of depth D
complete(_X, 0) -> {};
complete(X, D) ->
    Branch = complete(X, D-1),
    { Branch, X, Branch}.

%% exercise 2.5(b)
%% extend 2.5(a) to create balanced trees of arbitrary size

%% M is the number of nodes (the size of the tree) in the tree.
create(X, M) ->
    N = M-1,
    { create2(X, N div 2), X, create2(X, (N div 2 + N rem 2)) }.

create2(_X, 0) -> {};
create2(X, 1) -> { {}, X, {}};
create2(X, M) ->
    N = M - 1,
    { create2(X, N div 2), X, create2(X, N div 2 + N rem 2)}.

%% exercise 2.6
%% support a finite map (assoc array) as data, rather than finite set (array)

bst_map_member(_X, {}) ->
    false;
bst_map_member({K, V}, { A, Tk, _Tv, _B}) when K < Tk ->
    bst_map_member({K, V}, A);
bst_map_member({K, V}, { _A, Tk, _Tv, B}) when K > Tk ->
    bst_map_member({K, V}, B);
bst_map_member({K, V}, { _A, K, V, _B}) ->
    true;
bst_map_member({K, _V}, { _A, K, _Tv, _B}) ->
    false.

bst_map_insert({K, V}, {}) ->
    {{}, K, V, {}};
bst_map_insert({K, V}, {A, Yk, Yv, B}) when K < Yk ->
    {bst_map_insert({K, V}, A), Yk, Yv, B};
bst_map_insert({K, V}, {A, Yk, Yv, B}) when K > Yk ->
    {A, Yk, Yv, bst_map_insert({K, V}, B)};
bst_map_insert(_X, S) -> S.
