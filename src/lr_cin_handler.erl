-module(lr_cin_handler).

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
    CinsToCensBin = jiffy:decode(JsonBin, [return_maps]),
    ?DEBUG("Importing CINs(~p)", [CinsToCensBin]),
    CinLM = leviathan_cin:build_cins(json_bin_to_list(CinsToCensBin)),
    leviathan_dby:import_cins(<<"host1">>, CinLM),
    leviathan_cin_store:import_cins(<<"host1">>, CinLM),
    {ok, Req2, State};
handle(<<"POST">>, Req, #{handle_action := make} = State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    Cins = [binary_to_list(C) || C <- jiffy:decode(JsonBin)],
    ?DEBUG("Making CINs(~p)", [Cins]),
    run(fun() -> leviathan_cin:prepare(Cins) end),
    {ok, Req2, State};
handle(<<"POST">>, Req, #{handle_action := destroy} = State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    Cins = [binary_to_list(C) || C <- jiffy:decode(JsonBin)],
    ?DEBUG("Destroying CINs(~p)", [Cins]),
    run(fun() -> leviathan_cin:destroy(Cins) end),
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

json_bin_to_list(BinMap) ->
    Fn = fun(K, V, Acc) ->
                 maps:put(binary_to_list(K),
                          lists:map(fun binary_to_list/1, V),
                          Acc)
         end,
    maps:fold(Fn, #{}, BinMap).
