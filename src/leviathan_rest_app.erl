-module(leviathan_rest_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = start_cowboy(),
    leviathan_rest_sup:start_link().


stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

start_cowboy() ->
    ok = erl_cowboy:routing(?MODULE,
                            lists:flatten([
                                           cen_routes(),
                                           cin_routes(),
                                           container_routes(),
                                           monitor_routes(),
                                           cpool_routes(),
                                           switch_routes()
                                          ])).

cen_routes() ->
    [
     {"/cen/:action", lr_cen_handler, []},
     {"/cen/:cen", lr_cenid_handler, []}
    ].

cin_routes() ->
    [{"/cin/:action", lr_cin_handler, #{handle_action => import}}].

container_routes() ->
    [
     {"/host/:host/:container/:cen", lr_container_cen_handler, []},
     {"/host/:host/:container", lr_container_handler, []}
    ].

monitor_routes() ->
    [
     {"/leviathan/monitor", lr_monitor_handler, []},
     {"/leviathan/monitor/test/[...]", cowboy_static, {priv_dir, leviathan_rest, "static"}}
    ].

cpool_routes() ->
    [{"/cpool/", lr_cpool_handler, []}].

switch_routes() ->
    [{"/switch/", lr_switch_handler, []}].


