-module(redblack).
-export([member/2,
         insert/2,
         complies/1]).

-include("redblack.hrl").

%% http://scienceblogs.com/goodmath/2009/11/advanced_haskell_data_structur.php
%% Invariant properties that must hold about the coloring of the tree:
%%   1. The root of the tree is always black.
%%   2. All branches of a tree end in a null which is black.
%%   3. All children of red nodes are black.
%%   4. For all nodes in the tree, all downward paths from the node to a leaf contain
%%      the same number of black nodes.

%% http://www.eternallyconfuzzled.com/tuts/datastructures/jsw_tut_rbtree.aspx
%% nodes that are part of a logical B-tree node (horizontal links) are colored red,
%% while nodes that separate logical B-tree nodes (vertical links) are colored black.

%% Adapted from Okasaki "Purely Functional Data Structures" p. 28
member(_X, undefined) ->
    false;
member(X, #node{left=A, data=Y}) when X < Y ->
    member(X, A);
member(X, #node{data=Y, right=B}) when X > Y ->
    member(X, B);
member(_X, _S) ->
    true.

insert(X, undefined) ->
    #node{color=black, data=X};
insert(X, S) ->
    % to do recursive anonymous functions, pass the created fun in as 2nd parameter
    Ins = fun(undefined, _F) ->
                  #node{color=red, data=Data}; % insert the new data as a red node
             (#node{color=Color, left=A, data=Y, right=B}, F) when X < Y->
                  balance(Color, F(A, F), Y, B);
             (#node{color=Color, left=A, data=Y, right=B}, F) when X > Y->
                  balance(Color, A, Y, F(B, F));
             (Node, _F) ->
                  Node
          end,
    #node{left=A, data=Y, right=B} = Ins(S, Ins),
    #node{color=black, left=A, data=Y, right=B}.

%% detect Black->Red->Red Patterns and balance the situation
balance(black, #node{color=red, left=#node{color=red, left=A, data=X, right=B}, data=Y, right=C}, Z, D) ->
    #node{color=red, left=#node{color=black, left=A, data=X, right=B}, data=Y, right=#node{color=black, left=C, data=Z, right=D}};
balance(black, #node{color=red, left=A, data=X, right=#node{color=red, left=B, data=Y, right=C}}, Z, D) ->
    #node{color=red, left=#node{color=black, left=A, data=X, right=B }, data=Y, right=#node{color=black, left=C, data=Z, right=D}};
balance(black, A, X, #node{color=red, left=#node{color=red, left=B, data=Y, right=C}, data=Z, right=D}) ->
    #node{color=red, left=#node{color=black, left=A, data=X, right=B }, data=Y, right=#node{color=black, left=C, data=Z, right=D}};
balance(black, A, X, #node{color=red, left=B, data=Y, right=#node{color=red, left=C, data=Z, right=D}}) ->
    #node{color=red, left=#node{color=black, left=A, data=X, right=B }, data=Y, right=#node{color=black, left=C, data=Z, right=D}};
balance(Color, Left, Data, Right) ->
    #node{color=Color, left=Left, data=Data, right=Right}.

%% Test a tree by these 5 invariants - http://en.wikipedia.org/wiki/Red_black_tree#Properties
%%   1. A node is either red or black.
%%   2. The root is black.
%%   3. All leaves are black.
%%   4. Both children of every red node are black.
%%   5. Every simple path from a given node to any of its descendant leaves contains the same number of black nodes.

complies(Tree) ->
    C1 = complies1(Tree),
    C2 = complies2(Tree),
    C3 = complies3(Tree),
    C4 = complies4(Tree),
    C5 = complies5(Tree),
    io:format("Tests 1: ~w 2: ~w 3: ~w 4: ~w 5: ~w~n", [C1,C2,C3,C4,C5]),
    io:format("Tree: ~p~n", [Tree]),
    C1 andalso C2 andalso C3 andalso C4 andalso C5.

%% A node is either red or black.
%% no clause for red with no branches, as this violates #3
complies1(undefined) -> true;
complies1(#node{color=black,left=undefined,right=undefined}) -> true;
complies1(#node{color=red,left=Left,right=Right}) ->
    complies1(Left) andalso complies1(Right);
complies1(#node{color=black,left=Left,right=Right}) ->
    complies1(Left) andalso complies1(Right);
complies1(_FailedNode) ->
    false.

%% Root is black
complies2(#node{color=black}) -> true;
complies2(_) -> false.

%% All leaves are black
complies3(undefined) -> true; % assumed to be black
complies3(#node{color=black,left=undefined,right=undefined}) -> true;
complies3(#node{left=Left,right=Right}) ->
    complies3(Left) andalso complies3(Right).

%% Both children of red nodes are black
complies4(undefined) -> true;
complies4(#node{color=red,left=#node{color=red}}) -> false;
complies4(#node{color=red,right=#node{color=red}}) -> false;
complies4(#node{left=Left,right=Right}) ->
    complies4(Left) andalso complies4(Right).

complies5(_) ->
     true.
