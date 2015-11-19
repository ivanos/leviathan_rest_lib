-module(lr_cin_handler).

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


handle_action(<<"import">>, Req0, State) ->
    {ok, JsonBin, Req1} = cowboy_req:body(Req0),
    CinsToCensBin = jiffy:decode(JsonBin, [return_maps]),
    ?DEBUG("Importing CINs(~p)", [CinsToCensBin]),
    CinLM = leviathan_cin:build_cins(json_bin_to_list(CinsToCensBin)),
    leviathan_dby:import_cins(<<"host1">>, CinLM),
    leviathan_cin_store:import_cins(<<"host1">>, CinLM),
    {ok, Req1, State};
handle_action(<<"make">>, Req0, State) ->
    {ok, JsonBin, Req1} = cowboy_req:body(Req0),
    Cins = [binary_to_list(C) || C <- jiffy:decode(JsonBin)],
    ?DEBUG("Making CINs(~p)", [Cins]),
    run(fun() -> leviathan_cin:prepare(Cins) end),
    {ok, Req1, State};
handle_action(<<"destroy">>, Req0, State) ->
    {ok, JsonBin, Req1} = cowboy_req:body(Req0),
    Cins = [binary_to_list(C) || C <- jiffy:decode(JsonBin)],
    ?DEBUG("Destroying CINs(~p)", [Cins]),
    run(fun() -> leviathan_cin:destroy(Cins) end),
    {ok, Req1, State}.

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

json_bin_to_list(BinMap) ->
    Fn = fun(K, V, Acc) ->
                 maps:put(binary_to_list(K),
                          lists:map(fun binary_to_list/1, V),
                          Acc)
         end,
    maps:fold(Fn, #{}, BinMap).
