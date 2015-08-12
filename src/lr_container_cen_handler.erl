-module(lr_container_cen_handler).

-export([init/3,
         rest_init/2,
         resource_exists/2,
         previously_existed/2,
         allow_missing_post/2,
         known_methods/2,
         allowed_methods/2,
         options/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2,
         handle_json/2
        ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req0, _Opts) ->
    {Req1, [Host, Container, Cen]} =
                        lr_cowboy:bindings(Req0, [host, container, cen]),
    {ok, Req1, #{hostid => binary_to_list(Host),
                 containerid => binary_to_list(Container),
                 cenid => binary_to_list(Cen),
                 exists => false,
                 cen => undefined}}.

previously_existed(Req, State) -> 
    {false, Req, State}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

known_methods(Req, State) -> 
    {[<<"OPTIONS">>,<<"PUT">>,<<"DELETE">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>,<<"PUT">>,<<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

options(Req0, State) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>,
                                                    <<"content-type">>, Req0),
    {ok, Req1, State}.

resource_exists(Req0, #{hostid := HostId,
                        containerid := ContId,
                        cenid := CenId} = State) ->
    Cen = leviathan_dby:get_cen(HostId, CenId),
    #{contIDs := ContIds} = Cen,
    case {CenId, lists:member(ContId, ContIds)} of
        {null, _} ->
            {false, Req0, State};
        _ ->
            {true, Req0, State#{exists := true, cen := Cen}}
    end.

delete_resource(Req0, #{hostid := HostId,
                        containerid := ContainerId,
                        cenid := CenId} = State) ->
    ok = leviathan_cen:remove_container_from_cen(HostId, ContainerId, CenId),
    Req1 = set_cross_domain(Req0),
    {true, Req1, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_json}], Req, State}.

handle_json(Req0, State) ->
    {Method, Req1} = cowboy_req:method(Req0),
    handle_json_method(Req1, State, Method).
%
handle_json_method(Req, State, <<"GET">>) ->
    {false, Req, State};
handle_json_method(Req0, #{hostid := HostId,
                          containerid := ContainerId,
                          cenid := CenId} = State, <<"PUT">>) ->
    ok = leviathan_cen:add_container_to_cen(HostId, ContainerId, CenId),
    Req1 = set_cross_domain(Req0),
    Req2 = cowboy_req:set_resp_body(<<"true">>, Req1),
    {true, Req2, State}.

set_cross_domain(Req) ->
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req).
