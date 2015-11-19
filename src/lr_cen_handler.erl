-module(lr_cen_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include("leviathan_rest_logger.hrl").

%% ===================================================================
%% Handler callbacks
%% ===================================================================

init(_, Req, Opts) ->
    {ok, Req, Opts}.

handle(Req0, State) ->
    {Method, Req1} = cowboy_req:method(Req0),
    handle(Method, Req1, State).

terminate(_Reason, _Req, _State) ->
    ok.


%% ===================================================================
%% Internal functions
%% ===================================================================

handle(<<"POST">>, Req0, State) ->
    {Action, Req1} = cowboy_req:binding(action, Req0),
    handle_action(Action, Req1, State);
handle(_, Req0, State) ->
    {ok, Req1} = cowboy_req:reply(405, [{<<"content-type">>, <<"text/plain">>}],
                                  <<"Method not allowed\n">>, Req0),
    {ok, Req1, State}.

handle_action(<<"import">>, Req, State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    LM = leviathan_cen:decode_binary(JsonBin),
    ok = leviathan_dby:import_cens(<<"host1">>, LM),
    ok = leviathan_cen_store:import_cens(<<"host1">>, LM),
    {ok, Req2, State};
handle_action(<<"make">>, Req, State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    Cens = [binary_to_list(C) || C <- jiffy:decode(JsonBin)],
    ?DEBUG("Preparing CENs(~p)", [Cens]),
    %% prepare the Cens asynchronously
    run(fun() -> leviathan_cen:prepare(Cens) end),
    {ok, Req2, State};
handle_action(<<"destroy">>, Req, State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    Cens = [binary_to_list(C) || C <- jiffy:decode(JsonBin)],
    ?DEBUG("Destroying CENs(~p)", [Cens]),
    run(fun() -> leviathan_cen:destroy(Cens) end),
    {ok, Req2, State}.


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
