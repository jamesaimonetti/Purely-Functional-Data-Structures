-module(chp4).

-export([append/2, take/2, drop/2, reverse/1, sort/1, force/1]).

-compile(export_all).

%% append two streams together
append([], []) -> [];
append([], [H|T]) ->  [ H | fun() -> append([], T) end ];
append([X | S], T) -> [ X | fun() -> append(S, T) end ].

%% take the first N elements from a stream
take(0, _L) -> [];
take(_N, []) -> [];
take(N, [H|T]) ->
    [H | fun() -> take(N-1, T) end].

%% drop the first N elements from a stream
%drop(0, L) -> L;
%drop(_N, []) -> [];
%drop(N, [_H|T]) ->
%    [T | fun() -> drop(N-1, T) end].

%% the monolithic example from pg 35
%% suspend the drop call, but perform it at once when forced
drop(N, L) ->
    Dprime = fun(0, Lprime, _F) -> Lprime;
                (_N, [], _F) -> [];
                (Nprime, [_H|T], F) -> F(Nprime-1, T, F) end,
    fun() -> Dprime(N, L, Dprime) end.

%% monolithic version of reverse
reverse(L) ->
    Rprime = fun([], Rev, _F) -> Rev;
                ([H|T], Rev, F) -> F(T, [H | Rev], F) end,
    fun() -> Rprime(L, [], Rprime) end.

%% Exercise 4.2 - Insertion sort on streams.
sort([]) -> [];
sort(L) ->
    io:format("Cur L: ~p~n", [L]),
    Min = lists:min(L),
    [ Min | fun() -> sort(remove_val(Min, L)) end ].

remove_val(X, L) ->
    remove_val(X, L, []).

remove_val(_X, [], L) -> lists:reverse(L);
remove_val(X, [X|T], L) -> lists:reverse(L, T); % reverse L and append T
remove_val(X, [H|T], L) -> remove_val(X, T, [H|L]).

%% force evaluation of the next unit of the stream
force([]) -> [];
force(Fun) when is_function(Fun) -> Fun();
force([_X, Fun | []]) when is_function(Fun) -> Fun();
force([_X|T]) ->
    force(T).
