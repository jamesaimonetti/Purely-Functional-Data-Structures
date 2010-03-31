-module(splay).

-export([new/0, is_set/1, size/1, to_list/1, from_list/1]).
-export([is_element/2, add_element/2, del_element/2]).
-export([union/2, union/1, intersection/2, intersection/1]).
-export([subtract/2, is_subset/2]).
-export([fold/3, filter/2]).

-export([all/2, any/2, foreach/2, partition/2]).

-export([new_set/0,set_to_list/1,list_to_set/1,subset/2]).
-deprecated([{new_set,0},{set_to_list,1},{list_to_set,1},{subset,2}]).

-compile(export_all).

-record(splay, {
          lte={},
          data=undefined,
          gt={}
         }).

%% new() -> Set
new() -> empty.

%% is_set(Set) -> bool()
%%   Return true if Set is a set of elements, false otherwise
is_set(empty) -> true;
is_set(#splay{lte=A, gt=B}) -> is_set(A) andalso is_set(B).

%% size(Set) -> int()
size(empty) -> 0;
size(#splay{lte=A, gt=B}) -> size(A) + 1 + size(B).

%% to_list(Set) -> [Element].
to_list(empty) -> [];
to_list(#splay{lte=A, data=X, gt=B}) ->
    lists:flatten([to_list(A), [X], to_list(B)]).

%% from_list([Element]) -> Set.
from_list(L) ->
    lists:foldl(fun add_element/2, new(), L).

%% is_element(Element, Set) -> true | false.
is_element(_, empty) -> false;
is_element(X, #splay{lte=A, data=Y}) when X < Y ->
    is_element(X, A);
is_element(X, #splay{data=Y, gt=B}) when X > Y ->
    is_element(X, B);
is_element(X, #splay{data=X}) -> true.

%% add_element(Element, Set) -> Set.
add_element(X, T) ->
    {Lte, Gt} = partition(X, T),
    #splay{lte=Lte, data=X, gt=Gt}.

%% del_element(Element, Set) -> Set
del_element(_, empty) -> empty;
del_element(X, #splay{lte=A, data=Y}=T) when X < Y ->
    T#splay{lte=del_element(X, A)};
del_element(X, #splay{data=Y, gt=B}=T) when X > Y ->
    T#splay{gt=del_element(X, B)};
del_element(X, #splay{lte=A, data=X, gt=B}) ->
    merge(A, B).

%% del_min(Set) -> {Set, Element}.
del_min(#splay{lte=empty, gt=B}) -> {B, undefined};
del_min(#splay{lte=#splay{lte=empty, gt=B}, data=Y, gt=C}) ->
    {#splay{lte=B, data=Y, gt=C}, Y};
del_min(#splay{lte=#splay{lte=A, data=X, gt=B}, data=Y, gt=C}) ->
    {Lte, Min} = del_min(A),
    {#splay{lte=Lte, data=X, gt=#splay{lte=B, data=Y, gt=C}}, A}.

%% union(Set1, Set2) -> Set.
%%   Returns the union of Set1 and Set2
union(S1, S2) ->
    fold(fun add_element/2, S1, S2).

%% union([Set]) -> Set.
%%   Returns the union of a list of sets
union([]) -> empty;
union([S]) -> S;
union([S1, S2 | Ss]) ->
    union([union(S1, S2) | Ss]).

%% intersection(Set1, Set2) -> Set.
%%   Return the intersection of Set1 and Set2
intersection(S1, S2) ->
    filter(fun(E) -> is_element(E, S2) end, S1).

%% intersection([Set]) -> Set.
intersection([]) -> empty;
intersection([S]) -> S;
intersection([S1, S2| Ss]) ->
    intersection([intersection(S1, S2) | Ss]).

%% subtract(Set1, Set2) -> Set.
%%   Return all elements of Set1 not in Set2
subtract(S1, S2) ->
    filter(fun(E) -> not is_element(E, S2) end, S1).

%% subset(Set1, Set2) -> bool()
%%   Return true is every element of S1 is a memeber of S2
is_subset(S1, S2) ->
    all(fun(E) -> is_element(E, S2) end, S1).

%% fold(Fun, Acc, Set) -> Acc.
fold(_, Acc, empty) -> Acc;
fold(F, Acc, #splay{lte=A, data=X, gt=B}) ->
    fold(F, F(X, fold(F, Acc, B)), A).

%% filter(Pred, Set) -> Set.
%%   Return elements in Set for which Pred(Element) =:= true
filter(P, T) ->
    filter(P, T, empty).

filter(_, empty, New) -> New;
filter(P, #splay{lte=A, data=X, gt=B}, New0) ->
    New1 = filter(P, A, New0),
    New2 = case P(X) of
               true -> add_element(New1);
               false -> New1
           end,
    filter(P, B, New2).

%% all(Pred, Set) -> bool().
%%   Return true if Pred(Elem) is true for all elements in Set
all(_, empty) -> true;
all(P, #splay{lte=A, data=X, gt=B}) ->
    P(X) andalso all(P, A) andalso all(P, B).

%% any(Pred, Set) -> bool().
%%   Return true if Pred(Elem) is true for any element in Set
any(_, empty) -> true;
any(P, #splay{lte=A, data=X, gt=B}) ->
    P(X) orelse any(P, A) orelse any(P, B).

%% foreach(F, Set) -> ok.
%%   Apply F to each element in Set
foreach(_, empty) -> ok;
foreach(F, #splay{lte=A, data=X, gt=B}) ->
    foreach(F, A),
    F(X),
    foreach(F, B).

merge(empty, T) -> T;
merge(#splay{lte=A, data=X, gt=B}, T) ->
    {TA, TB} = partition(X, T),
    #splay{lte=merge(TA, A), data=X, gt=merge(TB, B)}.

partition(_Pivot, empty) -> {empty,empty};
partition(Pivot, #splay{lte=A, data=X, gt=B}=T) ->
    case X =< Pivot of
        true ->
            case B of
                empty ->
                    {T, B};
                #splay{lte=B1, data=Y, gt=B2} ->
                    case Y =< Pivot of
                        true ->
                            {Lte, Gt} = partition(Pivot, B2),
                            {#splay{lte=#splay{lte=A, data=X, gt=B1}, data=Y, gt=Lte}, Gt};
                        false ->
                            {Lte, Gt} = partition(Pivot, B1),
                            {#splay{lte=A, data=X, gt=Lte}, #splay{lte=Gt, data=Y, gt=B2}}
                    end
            end;
        false ->
            case A of
                empty ->
                    {A, T};
                #splay{lte=A1, data=Y, gt=A2} ->
                    case Y =< Pivot of
                        true ->
                            {Lte, Gt} = partition(Pivot, A2),
                            {#splay{lte=A1, data=Y, gt=Lte}, #splay{lte=Gt, data=X, gt=B}};
                        false ->
                            {Lte, Gt} = partition(Pivot, A1),
                            {Lte, #splay{lte=Gt, data=Y, gt=#splay{lte=A2, data=X, gt=B}}}
                    end
            end
    end.

find_min(#splay{lte=empty, data=Min}) -> Min;
find_min(#splay{lte=A}) -> find_min(A).

%% Deprecated interface.
 
new_set() -> new().

set_to_list(S) -> to_list(S).

list_to_set(L) -> from_list(L).

subset(S1, S2) -> is_subset(S1, S2).
