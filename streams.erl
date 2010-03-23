%% ===============================================================
%% Lazy streams
%%
%% Copyright (C) 2000 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2 of the License, or (at your option) any later
%% version.
%%
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General
%% Public License along with this library; if not, write to the
%% Free Software Foundation, Inc., 59 Temple Place, Suite 330,
%% Boston, MA 02111-1307 USA
%%
%% wget http://erlang.mirror.su.se/ml-archive/erlang-questions/200010/msg00165.html
%%
%% Author contact: richardc@csd.uu.se
%% ===============================================================

-module(streams).

-vsn('$Id: streams.erl,v 1.2 2000/10/18 13:58:07 richardc Exp $').

-author('richardc@csd.uu.se').

-copyright('Copyright (C) 2000 Richard Carlsson').

-export([append/2, constant/1, drop/2, duplicate/2, empty/0,
	 filter/2, first/2, foldl/3, foldr/3, foreach/2,
	 integers/1, map/2, member/2, merge/3, nth/2, nthtail/2,
	 push/2, random/0, random_integers/1, seq/2, seq/3,
	 stream/1, subsequence/3, to_list/1]).

%% The type of the streams defined below is:
%%
%%	Stream :: () -> {} | {term(), Stream}
%%
%% A stream is thus always a fun. One advantage is that if a
%% producer has side effects (e.g., if it reads elements from a
%% file), we need never trigger the next effect unless we really
%% want to see the next element. Also, programming becomes very
%% straightforward. Let's hope for better compilers to make this
%% sort of programming more efficient.


%% ===============================================================
%% The following functions compute new streams without forcing
%% evaluation of their input.


%% The empty stream

empty() -> fun () -> {} end.


%% The stream consisting of H followed by all elements of T.

push(H, T) -> fun () -> {H, T} end.


%% Converts a stream into a list. Make sure never to pass an
%% infinite stream to this function!

to_list(S) ->
    case S() of
	{H, T} ->
	    [H | to_list(T)];    
	{} ->
	    []
    end.


%% Turns a proper normal list into a stream.

stream(L) ->
    fun () ->
	    case L of
		[H | T] ->
		    {H, stream(T)};
		[] ->
		    {}
	    end
    end.


%% The stream of all elements of S1 followed by all elements of
%% S2.

append(S1, S2) ->
    fun () ->
	    case S1() of
		{} ->
		    S2(); 
		{H, T} ->
		    {H, append(T, S2)}
	    end
    end.


%% The stream of integers in the interval [From, To], in ascending
%% order.

seq(From, To) ->
    seq(From, To, 1).

%% The stream of integers [From, From + D, From + 2*D, ..., From +
%% D*((To - From) mod D)]. The interval is empty if D does not
%% have the same sign as the difference To - From.

seq(From, To, D) when is_integer(From), is_integer(To),
		      From < To, D > 0 ->
    fun () -> {From, seq(From + D, To, D)} end;
seq(From, To, D) when is_integer(From), is_integer(To),
		      To < From, D < 0 ->
    fun () -> {From, seq(From + D, To, D)} end;
seq(From, To, _D) when is_integer(From), is_integer(To), To == From ->
    fun () -> {From, empty()} end;
seq(_From, _To, _D) ->
    empty().


%% The stream of integers starting at N.

integers(N) ->
    fun () -> {N, integers(N + 1)} end.


%% The infinite stream of elements X.

constant(X) ->
    fun () -> {X, constant(X)} end.


%% The stream of length N, where each element is X.

duplicate(N, X) when is_integer(N) ->
    fun () ->
	    if N > 0 ->
		    {X, duplicate(N - 1, X)};
	       N == 0 ->
		    {}
	    end
    end.


%% An infinite stream of random floating-point numbers in the
%% interval [0.0, 1.0]. `random:seed' must be used (by the same
%% process) to set the seed for the random number generator before
%% this function is called.

random() ->
    fun () -> {random:uniform(), random()} end.

%% An infinite stream of random integers in the interval [0, Max].
%% See `random' above for details.

random_integers(Max) when Max >= 1 ->
    fun () -> {random:uniform(Max), random_integers(Max)} end.


%% The stream consisting of the first N elements of S, or the
%% stream S if the length of S is not greater than N.

first(0, _S) -> empty();
first(N, S) when is_integer(N), N > 0 ->
    fun () ->
	    case S() of
		{H, T} ->
		    {H, first(N - 1, T)};
		{} ->
		    {}
	    end
    end.

%% The stream [EN+1, EN+2, ...], if S is [E1, ..., EN, EN+1, ...].

drop(0, S) -> S;
drop(N, S) when is_integer(N), N > 0 ->
    fun () ->
	    case S() of
		{_H, T} ->
		    (drop(N - 1, T))();
		{} ->
		    {}
	    end
    end.

%% The stream [EN, EN+1, ..., EN+D], if S is [E1, E2, ...].

subsequence(N, D, S) when N > 0, D >= 0 ->
    first(D, drop(N, S)).

%% The stream [F(E1), F(E2), F(E3), ...] if S is [E1, E2, E3,
%% ...].

map(F, S) ->
    fun () ->
	    case S() of
		{H, T} ->
		    {F(H), map(F, T)};
		{} ->
		    {}
	    end
    end.


%% The stream of all elements E in S (in the same order) for which
%% P(E) returns `true'. P must return either `true' or `false' for
%% all elements in S.

filter(P, S) ->
    fun () ->
	    case S() of
		{H, T} ->
		    case P(H) of
			true ->
			    {H, filter(P, T)};
			false ->
			    (filter(P, T))()
		    end;
		{} ->
		    {}
	    end
    end.


%% Returns the stream of elements in S1 and S2 where the
%% respective relative order of elements is preserved, and each
%% element Y of S2 is ordered before the first possible X of S1
%% such that P(X, Y) yields `false'. P(X, Y) must yield either
%% `true' or `false' for all X in S1 and Y in S2. (P can be read
%% as "less than".)

merge(P, S1, S2) ->
    fun () ->
	    case S1() of
		{H1, T1} ->
		    case S2() of
			{H2, T2} ->
			    case P(H1, H2) of
				true ->
				    {H1, merge(P, T1,
					       push(H2, T2))}; 
				false ->
				    {H2, merge(P, push(H1, T1),
					       T2)}
			    end;
			{} ->
			    {H1, T1}
		    end;
		{} ->
		    S2()
	    end
    end.


%% ===============================================================
%% All functions below may evaluate all or part of their input.


%% Returns `true' if X is in the stream S, and `false' otherwise.

member(X, S) ->
    case S() of
	{H, T} ->
	    if H == X -> true;
	       true -> member(X, T)
	    end;
	{} -> false
    end.


%% Returns the Nth element of stream S.

nth(N, S) ->
    S1 = S(),
    if N > 1 ->
	    nth(N - 1, tl(S1));
       true ->
	    hd(S1)
    end.


%% Returns the stream [AN+1, AN+2, ...] if S is [A1, A2, ..., AN,
%% AN+1, AN+2, ...].

nthtail(N, S) when N > 1 ->
    nthtail(N - 1, tl(S()));
nthtail(0, S) ->
    S.


%% Computes (...((A F E1) F E2)... F EN), if S is [E1, E2, ...,
%% EN] and F is a binary function (here written as an infix
%% operator).

foldl(A, F, S) ->
    case S() of
	{H, T} ->
	    foldl(F(A, H), F, T);
	{} ->
	    A
    end.


%% Computes (E1 F ...(EN-1 F (EN F A))...), if S is [E1, E2, ...,
%% EN] and F is a binary function (here written as an infix
%% operator).

foldr(S, F, A) ->
    case S() of
	{H, T} ->
	    F(H, foldr(T, F, A));
	{} ->
	    A
    end.


%% Evaluates F(E1), F(E2), ..., if S is [E1, E2, ...]. Always
%% returns `ok'.

foreach(F, S) ->
    case S() of
	{H, T} ->
	    F(H),
	    foreach(F, T);
	{} ->
	    ok
    end.
%% ===============================================================
