-module(lr_cen_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include("leviathan_rest_logger.hrl").

init(_, Req, Opts) ->
    {ok, Req, Opts}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    handle(Method, Req2, State).

handle(<<"POST">>, Req, #{handle_action := import} = State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    LM = leviathan_cen:decode_binary(JsonBin),
    ok = leviathan_dby:import_cens(<<"host1">>, LM),
    ok = leviathan_cen_store:import_cens(<<"host1">>, LM),
    {ok, Req2, State};
handle(<<"POST">>, Req, #{handle_action := make} = State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    Cens = [binary_to_list(C) || C <- jiffy:decode(JsonBin)],
    ?DEBUG("Preparing CENs(~p)", [Cens]),
    %% prepare the Cens asynchronously
    run(fun() -> leviathan_cen:prepare(Cens) end),
    {ok, Req2, State};
handle(<<"POST">>, Req, #{handle_action := destroy} = State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    Cens = [binary_to_list(C) || C <- jiffy:decode(JsonBin)],
    ?DEBUG("Destroying CENs(~p)", [Cens]),
                                                % destroy the Cens asynchronously
    run(fun() -> leviathan_cen:destroy(Cens) end),
    {ok, Req2, State};
handle(_, Req, State) ->
    {ok, Req2} = cowboy_req:reply(405, [{<<"content-type">>, <<"text/plain">>}],
				  <<"Method not allowed\n">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.


%% execute asynchronously. catch and log errors.
run(Fn) ->
    spawn(
      fun() ->
              case catch {ok, Fn()} of
                  {ok, Result} ->
                      ?DEBUG("run ok: ~p", [Result]);
                  Error ->
                      ?WARNING("run error: ~p", [Error])
              end
      end).
