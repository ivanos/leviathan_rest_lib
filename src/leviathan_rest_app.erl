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
{"/host/:host/:container/:cen", lr_container_cen_handler, []},
% {"/host/:host/:container", lr_container_handler, []},
% {"/host/:host", lr_host_handler, []},
{"/cen/prepare", lr_cen_prepare_handler, []},
{"/cen/destroy", lr_cen_destroy_handler, []},
{"/cen/:cen", lr_cenid_handler, []},
{"/cin/", lr_cen_handler, []},
{"/cin/prepare", lr_cen_prepare_handler, []},
{"/cin/destroy", lr_cen_destroy_handler, []},
{"/cpool/", lr_cpool_handler, []}
        ]).
