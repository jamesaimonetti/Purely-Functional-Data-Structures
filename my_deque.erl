-module(my_deque).

-export([empty/0, is_empty/1,
         cons/2, head/1, tail/1,
         snoc/2, last/1, init/1]).

-compile(export_all).

empty() -> {[], []}.

is_empty({[],[]}) -> true;
is_empty(_) -> false.

%% insert to front of deque
cons(X, {[],[]}) -> {[X], []};
cons(X, {[],[Y]}) -> {[X], [Y]};
cons(X, {[],_R}=Q) -> cons(X, checkf(Q));
cons(X, {F, R}) -> checkf({[X|F], R}).

%% inspect head
head({[], []}=Q) -> {undefined, Q};
head({[], [X]}) -> {X, {[], []}};
head({[], _R}=Q) -> head(checkf(Q));
head({[X|F],R}) -> {X, {F, R}}.

%% remove head from deque
tail({[], []}) -> undefined;
tail({[], [_X]}) -> empty();
tail({[],_R}=Q) -> tail(checkf(Q));
tail({[_X|F], R}) -> checkf({F, R}).

%% insert to end of list
snoc(X, {[],[]}) -> {[], [X]};
snoc(X, {[Y],[]}) -> {[Y], [X]};
snoc(X, {_F,[]}=Q) -> snoc(X, checkf(Q));
snoc(X, {F, R}) -> checkf({F, [X|R]}).

%% inspect the last element
last({[], []}=Q) -> {undefined, Q};
last({[X], []}) -> {X, {[], []}};
last({_F, []}=Q) -> last(checkf(Q));
last({F,[X|R]}) -> {X, {F, R}}.

%% remove last element from deque
init({[], []}) -> undefined;
init({[_X], []}) -> empty();
init({_F,[]}=Q) -> init(checkf(Q));
init({F, [_X|R]}) -> checkf({F, R}).

checkf({[],[]}=Q) -> Q;
checkf({[], R}) -> 
    {F, R1} = lists:split(length(R) div 2, R),
    {lists:reverse(F), R1};
checkf({[_X], []}=Q) -> Q; % don't need to move the first el over to the end list
checkf({F, []}) ->
    {F1, R} = lists:split(length(F) div 2, F),
    {F1, lists:reverse(R)};
checkf(Q) -> Q.
