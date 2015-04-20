-module(octothorpe_ws).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([jsonify/1, do_move/2]).

init(Req, Opts) ->
    Req0 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    {cowboy_websocket, Req0, Opts}.

%% @doc
%% handle incoming data from client.
%% @end
websocket_handle({text, <<"init">>}, Req, State) ->
    {reply, {text, jsonify(ttt:board())}, Req, State};
websocket_handle({text, Data}, Req, State) ->
    #{<<"board">> := BoardL, <<"pos">> := Pos} = jsx:decode(Data, [return_maps]),
    Response = do_move(BoardL, Pos),
    {reply, {text, jsonify(Response)}, Req, State};
websocket_handle(_Other, Req, State) ->
    {reply, {text, <<"huh?: ">>}, Req, State}.

websocket_info(shutdown, Req, State) ->
    {shutdown, Req, State};
websocket_info(Info, Req, State) ->
    io:format("info received ~p~n", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

do_move(BoardL, Pos) ->
    Board = list_to_tuple(
              lists:map(
               fun (null) -> undefined;
                   (O) -> list_to_atom(binary_to_list(O)) end,
               BoardL)),
    {Row, Col} = ttt:pair_from_position(binary_to_integer(Pos)),
    ttt:move(Board, Row, Col).

jsonify({_,_,_,_,_,_,_,_,_}=Board) ->
    response(
      {ok, 
       [{board, prepare_board(Board)}]});
jsonify({game_over, Winner, Board}) ->
    response(
     {ok,
      [{status, <<"gameover">>}, {winner, prepare_winner(Winner)}, {board, prepare_board(Board)}]}).

prepare_winner(Who) when is_atom(Who) ->
    [{who, list_to_binary(atom_to_list(Who))}, {wintype, 0}];
prepare_winner({Who, Type}) ->
    [{who, list_to_binary(atom_to_list(Who))}, {wintype, Type}].

prepare_board(Board) ->
    lists:map(
        fun(undefined) -> null;
           (O) -> O end,
         tuple_to_list(Board)).

response({ok, Data}) ->
    jsx:encode([0, Data]);
response({error, Reason}) ->
    jsx:encode([1, Reason]).

