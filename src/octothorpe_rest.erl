-module(octothorpe_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

-export([handle_post/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{'*', handle_post}
	], Req, State}.

handle_post(Req, State) ->
    {Body, Req2} =  get_body(Req, <<>>),
    case jsx:is_json(Body) of
        false ->
            {false, Req2, State};
        true ->
            play(jsx:decode(Body, [return_maps]), Req, State)
    end.

play(#{<<"board">> := BoardL, <<"pos">> := Pos}, Req, State) ->
    {true, cowboy_req:set_resp_body(octothorpe_ws:jsonify(
                                      octothorpe_ws:do_move(BoardL, Pos)),
                                    Req), State}.

get_body(Req, Acc) ->
    case cowboy_req:body(Req) of
        {ok, Data, Req2} ->
            {<<Acc/binary, Data/binary>>, Req2};
        {more, Data, Req2} ->
            get_body(Req2, <<Acc/binary, Data/binary>>)
    end.
