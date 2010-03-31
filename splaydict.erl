-module(splaydict).

-export([new/0,is_key/2,to_list/1,from_list/1,size/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3]).
-export([update_val/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).
 
%% Deprecated interface.
-export([dict_to_list/1,list_to_dict/1]).
-deprecated([{dict_to_list,1},{list_to_dict,1}]).

-compile(export_all).

-record(splay, {
          lte=empty,
          key=undefined,
          value=undefined,
          gt=empty
         }).

%% new() -> Dict
new() -> empty.

%% is_key(Key, Dict) -> bool()
is_key(_, empty) -> false;
is_key(K, #splay{lte=A, key=K1}) when K < K1 ->
    is_key(K, A);
is_key(K, #splay{key=K1, gt=B}) when K > K1 ->
    is_key(K, B);
is_key(_, _) -> true.

%% to_list(Dict) -> [{Key,Value}].
to_list(T) -> to_list(T, []).

to_list(empty, L) -> L;
to_list(#splay{lte=A, key=K, value=V, gt=B}, L) ->
    to_list(A, [{K, V} | to_list(B, L)]).

%% from_list([{Key,Value}]) -> Dict.
from_list(L) ->
    lists:foldl(fun({K,V}, D) -> store(K, V, D) end, new(), L).

%% size(Dict) -> int()
size(empty) -> 0;
size(#splay{lte=A, gt=B}) -> splaydict:size(A) + 1 + splaydict:size(B).

%% fetch(Key, Dict) -> Value.

fetch(K, #splay{lte=A, key=K1}) when K < K1 ->
    fetch(K, A);
fetch(K, #splay{key=K1, gt=B}) when K > K1 ->
    fetch(K, B);
fetch(_, #splay{value=V}) -> V.

%% find(Key, Dict) -> {ok, Value} | error.
find(_, empty) -> error;
find(K, #splay{lte=A, key=K1}) when K < K1 ->
    find(K, A);
find(K, #splay{key=K1, gt=B}) when K > K1 ->
    find(K, B);
find(_, #splay{value=V}) -> V.

%% fetch_keys(Dict) -> [Key].
fetch_keys(T) -> fetch_keys(T, []).

fetch_keys(empty, L) -> L;
fetch_keys(#splay{lte=A, key=K, gt=B}, L) ->
    fetch_keys(A, [K | fetch_keys(B, L)]).

%% store(Key, Value, Dict) -> Dict.
store(K, V, Dict) ->
    Dict1 = case has_key(K, Dict) of
                true -> splaydict:erase(K, Dict);
                false -> Dict
            end,
    {Lte, Gt} = partition(K, Dict1),
    #splay{lte=Lte, key=K, value=V, gt=Gt}.

%% append(Key, Value, Dict) -> Dict.
append(K, V, empty) -> #splay{key=K, value=V};
append(K, V, #splay{lte=A, key=K1}=T) when K < K1 ->
    T#splay{lte=append(K, V, A)};
append(K, V, #splay{key=K1, gt=B}=T) when K > K1 ->
    T#splay{gt=append(K, V, B)};
append(K, Vs, #splay{key=K, value=V1}=T) when is_list(Vs) ->
    T#splay{value=V1 ++ Vs};
append(K, V, #splay{key=K, value=V1}=T) ->
    T#splay{value=V1 ++ [V]}.

%% append_list(K, [V], Dict) -> Dict.
append_list(K, Vs, Dict) ->
    append(K, Vs, Dict).

%% has_key(K, Dict) -> bool().
has_key(_, empty) -> false;
has_key(K, #splay{lte=A, key=K1}) when K < K1 ->
    has_key(K, A);
has_key(K, #splay{key=K1, gt=B}) when K > K1 ->
    has_key(K, B);
has_key(_, _) -> true.

%% erase(Key, Dict) -> Dict.
erase(_, empty) -> empty;
erase(K, #splay{lte=A, key=K1}) when K < K1 ->
    erase(K, A);
erase(K, #splay{key=K1, gt=B}) when K > K1 ->
    erase(K, B);
erase(K, #splay{lte=A, key=K, gt=B}) ->
    merge_dict(A, B).

%% update_val(Key, Value, Dict) -> Dict.
update_val(K, V, #splay{lte=A, key=K1}=T) when K < K1 ->
    T#splay{lte=update_val(K, V, A)};
update_val(K, V, #splay{key=K1, gt=B}=T) when K > K1 ->
    T#splay{gt=update_val(K, V, B)};
update_val(K, V, #splay{key=K}=T) ->
    T#splay{value=V}.

%% update(Key, Fun, Dict) -> Dict.
update(K, F, #splay{lte=A, key=K1}=T) when K < K1 ->
    T#splay{lte=update(K, F, A)};
update(K, F, #splay{key=K1, gt=B}=T) when K > K1 ->
    T#splay{gt=update(K, F, B)};
update(K, F, #splay{key=K, value=V}=T) ->
    T#splay{value=F(V)}.

%% update(Key, Fun, Init, Dict) -> Dict.
update(K, _, I, empty) -> #splay{key=K, value=I};
update(K, F, I, #splay{lte=A, key=K1}=T) when K < K1 ->
    T#splay{lte=update(K, F, I, A)};
update(K, F, I, #splay{key=K1, gt=B}=T) when K > K1 ->
    T#splay{gt=update(K, F, I, B)};
update(K, F, _, #splay{key=K, value=V}=T) ->
    T#splay{value=F(V)}.

%% update_counter(Key, Inc, Dict) -> Dict.
update_counter(K, I, empty) -> #splay{key=K, value=I};
update_counter(K, I, #splay{lte=A, key=K1}=T) when K < K1 ->
    T#splay{lte=update_counter(K, I, A)};
update_counter(K, I, #splay{key=K1, gt=B}=T) when K > K1 ->
    T#splay{gt=update_counter(K, I, B)};
update_counter(K, I, #splay{key=K, value=V}=T) ->
    T#splay{value=V+I}.

%% del_min(Dict) -> {Dict, Element}.
%del_min(#splay{lte=empty, gt=B}) -> {B, undefined};
%del_min(#splay{lte=#splay{lte=empty, gt=B}, key=Y, gt=C}) ->
%    {#splay{lte=B, key=Y, gt=C}, Y};
%del_min(#splay{lte=#splay{lte=A, key=X, gt=B}, key=Y, gt=C}) ->
%    {Lte, Min} = del_min(A),
%    {#splay{lte=Lte, key=X, gt=#splay{lte=B, key=Y, gt=C}}, Min}.


%% fold(Fun, Acc, Dict) -> Acc.
fold(_, Acc, empty) -> Acc;
fold(F, Acc, #splay{lte=A, key=K, value=V, gt=B}) ->
    fold(F, F(K, V, fold(F, Acc, B)), A).

%% map(Fun, Dict) -> Dict
map(_, empty) -> empty;
map(F, #splay{lte=A, key=K, value=V, gt=B}=T) ->
    T#splay{lte=map(F, A), value=F(K, V), gt=map(F, B)}.

%% filter(Pred, Dict) -> Dict.
%%   Return elements in Dict for which Pred(Element) =:= true
filter(P, T) ->
    filter(P, T, empty).

filter(_, empty, New) -> New;
filter(P, #splay{lte=A, key=K, value=V, gt=B}, New0) ->
    New1 = filter(P, A, New0),
    New2 = case P(K, V) of
               true -> store(K, V, New1);
               false -> New1
           end,
    filter(P, B, New2).

%% merge(Fun, Dict1, Dict2) -> Dict3.
merge(F, D1, D2) ->
    fold(fun(K, V2, D) ->
                 update(K, fun(V1) -> F(K, V1, V2) end, V2, D)
         end, D1, D2).

merge_dict(empty, T) -> T;
merge_dict(#splay{lte=A, key=K, gt=B}, T) ->
    {TA, TB} = partition(K, T),
    #splay{lte=merge_dict(TA, A), key=K, gt=merge_dict(TB, B)}.

partition(_Pivot, empty) -> {empty,empty};
partition(Pivot, #splay{lte=A, key=K, value=V, gt=B}=T) ->
    case K =< Pivot of
        true ->
            case B of
                empty ->
                    {T, B};
                #splay{lte=B1, key=K1, value=V1, gt=B2} ->
                    case K1 =< Pivot of
                        true ->
                            {Lte, Gt} = partition(Pivot, B2),
                            {#splay{lte=#splay{lte=A, key=K, value=V, gt=B1}, key=K1, value=V1, gt=Lte}, Gt};
                        false ->
                            {Lte, Gt} = partition(Pivot, B1),
                            {#splay{lte=A, key=K, value=V, gt=Lte}, #splay{lte=Gt, key=K1, value=V1, gt=B2}}
                    end
            end;
        false ->
            case A of
                empty ->
                    {A, T};
                #splay{lte=A1, key=K1, value=V1, gt=A2} ->
                    case K1 =< Pivot of
                        true ->
                            {Lte, Gt} = partition(Pivot, A2),
                            {#splay{lte=A1, key=K1, value=V1, gt=Lte}, #splay{lte=Gt, key=K, value=V, gt=B}};
                        false ->
                            {Lte, Gt} = partition(Pivot, A1),
                            {Lte, #splay{lte=Gt, key=K1, value=V1, gt=#splay{lte=A2, key=K, value=V, gt=B}}}
                    end
            end
    end.

%find_min(#splay{lte=empty, key=Min}) -> Min;
%find_min(#splay{lte=A}) -> find_min(A).

%% Deprecated interface.

%% dict_to_list(Dict) -> [{Key,Value}].
dict_to_list(D) -> to_list(D).

%% list_to_dict([{Key,Value}]) -> Dict.
list_to_dict(L) -> from_list(L).
