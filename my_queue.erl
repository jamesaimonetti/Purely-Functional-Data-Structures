-module(my_queue).

-export([empty/0, is_empty/1, snoc/2, head/1, tail/1]).

-compile(export_all).

empty() -> {[],[]}.

is_empty({[],_R}) -> true;
is_empty(_) -> false.

head({[],[]}=Q) -> {undefined,Q};
head({[],R}) ->
    F = lists:reverse(R),
    {hd(F), {tl(F), []}};
head({[H|F],R}) ->
    {H, {F,R}}.

snoc({F, R}, X) -> checkf({F, [X | R]}).

tail({F, R}) -> checkf({tl(F), R}).

checkf({[], R}) ->
    {lists:reverse(R),[]};
checkf(Q) -> Q.
