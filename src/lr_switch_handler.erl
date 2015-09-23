-module(lr_switch_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    handle(Method, Req2, State).

handle(<<"POST">>, Req, State) ->
    {ok, JsonBin, Req2} = cowboy_req:body(Req),
    SwitchDpids = leviathan_switch:import_binary(JsonBin),
    %% Allow 60 seconds for the switches to start and connect.
    case wait_for_switches(SwitchDpids, 60000) of
        ok ->
            {ok, Req3} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}],
                                          jiffy:encode(SwitchDpids), Req2),
            {ok, Req3, State};
        {error, timeout} ->
            {ok, Req3} = cowboy_req:reply(500, [{<<"content-type">>, <<"text/plain">>}],
                                          <<"Switch startup timeout\n">>, Req2),
            {ok, Req3, State}
    end;
handle(_, Req, State) ->
    {ok, Req2} = cowboy_req:reply(405, [{<<"content-type">>, <<"text/plain">>}],
				  <<"Method not allowed\n">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

wait_for_switches(SwitchDpids, Timeout) ->
    StartTime = os:timestamp(),
    wait_for_switches(SwitchDpids, StartTime, Timeout).

wait_for_switches([], _, _) ->
    ok;
wait_for_switches([Switch | Switches], StartTime, Timeout) ->
    Remaining = Timeout - timer:now_diff(os:timestamp(), StartTime) div 1000,
    case leviathan_switch:wait_for_switch(Switch, Remaining) of
        ok ->
            wait_for_switches(Switches, StartTime, Timeout);
        {error, timeout} ->
            {error, timeout}
    end.
