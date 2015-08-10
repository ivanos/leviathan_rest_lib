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

start_cowboy() ->
    ok = erl_cowboy:routing(?MODULE,
        [
{"/cen/", lr_cen_handler, []},
{"/cen/prepare", lr_cen_prepare_handler, []}
        ]).
