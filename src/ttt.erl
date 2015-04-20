%% @doc
%% An implementation of a minimax-ish based tic tac toe game.
%% @end
-module(ttt).

-export([board/0, move/2, move/3, test/0]).
-export([pair_from_position/1]).

-type marker() :: x | o | undefined.
-type win_type() :: integer(1..8).
-type board() :: {marker(), marker(), marker(),
                  marker(), marker(), marker(),
                  marker(), marker(), marker()}.
-type rowcol() :: 1 | 2 | 3.
-type position() :: {rowcol(), rowcol()}.
-type position_score() :: {integer(), position()}.

-define(BOARD, {undefined, undefined, undefined,
                undefined, undefined, undefined,
                undefined, undefined, undefined}).

%% API =========================================================================

board() ->
    ?BOARD.

%% @doc
%% Start a new game, making an initial move at (Row,Col).
%% @end
-spec move(Row :: rowcol(), Col :: rowcol()) -> board().
move(Row, Col) ->
    move(?BOARD, Row, Col).

%% @doc
%% Make a move at (Row, Col) on an existing Board.
%% @end
-spec move(Board :: board(), Row :: rowcol(), Col :: rowcol()) -> ok | board().
move(Board, Row, Col) ->
    NewBoard = update_board(x, Board, Row, Col),
    case maybe_gameover(NewBoard) of
        {game_over, _, _}=R -> R;
        _ ->
            maybe_gameover(
              npc_move(NewBoard))
    end.

%% @doc
%% Run a little test of npc vs npc.
%% Should yield cats games, with the odd 'o' win due to hardcoded bit
%% in the scoring function.
%% @end
test() ->
    test(x, move(?BOARD, random:uniform(3), random:uniform(3))).

test(_, ok) -> ok;
test(Who, Board) ->
    NewWho = opposite(Who),
    {_Score, {BestRow, BestCol}} = best_move(NewWho, Board),
    test(
      NewWho,
      maybe_gameover(
        update_board(NewWho, Board, BestRow, BestCol))).


%% Internal ====================================================================

-spec maybe_gameover(Board :: board()) -> ok | board().
maybe_gameover(Board) ->
    case game_state(Board) of
        undefined ->
            print_board(Board),
            Board;
        Winner -> 
            Res = {game_over, Winner, Board},
            print_board(Res),
            Res
    end.

%% @doc
%% Select the best move for 'o' given the current board,
%% and update the board accordingly.
%% @end
-spec npc_move(Board :: board()) -> board().
npc_move(Board) ->
    {_Score, {BestRow, BestCol}} = best_move(o, Board),
    update_board(o, Board, BestRow, BestCol).

%% @doc
%% For each open position on the board, test each one, assign it 
%% a score (o win = 1, x win = -1, cat = 0), and select the move
%% with the max score. If the outcome is undefined, test all
%% subsequent moves recursively.
%% @end
-spec best_move(Who :: marker(), Board :: board()) -> position_score().
best_move(Who, Board) ->
    Fun = who_minmax_fun(Who),
    Fun(
      score_moves(playable_pairs(Board), Who, Board)).

-spec score_moves(Moves :: [position_score()], Who :: marker(),
                  Board :: board()) -> [position_score()].
score_moves(Moves, Who, Board) ->
    maybe_random_choice(
      lists:map(
        fun score_move/1,
        [{Move, Who, Board} || Move <- Moves])).

-spec score_move({Move :: position(), Who :: marker(),
                  Board :: board()}) -> position_score().
score_move({{Row, Col}=Move, Who, Board}) ->
    NewBoard = update_board(Who, Board, Row, Col),
    Score = case game_state(NewBoard) of
        {o, _} -> 1;
        {x, _} -> -1;
        undefined ->
            {Sc, _} = best_move(opposite(Who), NewBoard),
            Sc;
        cat -> 0
    end,
    {Score, Move}.

-spec maybe_random_choice(Choices :: [position_score()]) -> [position_score()].
maybe_random_choice(Choices) ->
    case proplists:get_keys(Choices) of
        [0] ->
            [lists:nth(
               random:uniform(
                 length(Choices)),
               Choices)];
        _ -> Choices
    end.

-spec who_minmax_fun(marker()) -> function().
who_minmax_fun(o) -> fun lists:max/1;
who_minmax_fun(x) -> fun lists:min/1.

-spec opposite(marker()) -> marker().
opposite(x) -> o;
opposite(o) -> x.

-spec update_board(Who :: marker(), Board :: board(),
                   Row :: rowcol(), Col :: rowcol()) -> board().
update_board(Who, Board, Row, Col) ->
    erlang:setelement(
      position_from_pair(Row, Col),
      Board,
      Who).

-spec position_from_pair(Row :: rowcol(), Col :: rowcol()) -> integer(1..9).
position_from_pair(Row, Col) ->
    (Row - 1) * 3 + Col.

-spec playable_pairs(Board :: board()) -> [position()].
playable_pairs(Board) ->
    lists:map(
      fun pair_from_position/1,
      playble_positions(Board)).

-spec playble_positions(Board :: board()) -> [integer(1..9)].
playble_positions(Board) ->
    {Pos, _} = lists:unzip(
                 lists:filter(
                   fun ({_,undefined}) -> true;
                       (_) -> false
                   end,
                   lists:zip(
                     lists:seq(1,9),
                     tuple_to_list(Board)))),
    Pos.

-spec pair_from_position(integer(1..9)) -> position().
pair_from_position(Pos) when Pos =< 3 ->
    {1, Pos};
pair_from_position(Pos) when Pos > 3, Pos =< 6 ->
    {2, Pos - 3};
pair_from_position(Pos) ->
    {3, Pos - 6}.

-spec game_state(board()) -> {marker(), win_type()}.
game_state({Z, Z, Z,
            _, _, _,
            _, _, _}) when Z =/= undefined -> {Z, 1};
game_state({_, _, _,
            Z, Z, Z,
            _, _, _}) when Z =/= undefined -> {Z, 2};
game_state({_, _, _,
            _, _, _,
            Z, Z, Z}) when Z =/= undefined -> {Z, 3};
game_state({Z, _, _,
            Z, _, _,
            Z, _, _}) when Z =/= undefined -> {Z, 4};
game_state({_, Z, _,
            _, Z, _,
            _, Z, _}) when Z =/= undefined -> {Z, 5};
game_state({_, _, Z,
            _, _, Z,
            _, _, Z}) when Z =/= undefined -> {Z, 6};
game_state({_, _, Z,
            _, Z, _,
            Z, _, _}) when Z =/= undefined -> {Z, 8};
game_state({Z, _, _,
            _, Z, _,
            _, _, Z}) when Z =/= undefined -> {Z, 7};
game_state(Board) ->
    case is_board_full(Board) of
        true -> cat;
        false -> undefined
    end.

-spec is_board_full(Board :: board()) -> boolean().
is_board_full(Board) ->
    not lists:member(undefined, tuple_to_list(Board)).

print_board({game_over, Who, Board}) ->
    io:format("Game Over - ~p wins.~n~n~p~n", [Who, print_board(Board)]);
print_board(Board) ->
    io:format(
      "~s ~s ~s~n~s ~s ~s~n~s ~s ~s~n~n",
      lists:map(
        fun (undefined) -> "_";
            (Any) -> Any end,
        tuple_to_list(Board))).

