-module(lr_cpool_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    handle(Method, Req2, State).

handle(<<"POST">>, Req, State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    leviathan_cpool:import_binary(JsonBin),
    {ok, Req2, State};
handle(_, Req, State) ->
    {ok, Req2} = cowboy_req:reply(405, [{<<"content-type">>, <<"text/plain">>}],
				  <<"Method not allowed\n">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
