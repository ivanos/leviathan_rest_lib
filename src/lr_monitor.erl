-module(lr_monitor).

-include("leviathan_rest_logger.hrl").
-include_lib("leviathan_lib/include/leviathan_lib.hrl").

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         do/2,
         stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(WebsocketPid) ->
    gen_server:start_link(?MODULE, [WebsocketPid], []).

do(Pid, Msg) ->
    gen_server:call(Pid, {do, Msg}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([WebsocketPid]) ->
    % XXX link to WebsocketPid?
    {ok, #{websocket_pid => WebsocketPid,
           monitor_id => undefined,
           subscription_id => undefined}}.

handle_call({do, Msg}, _From, State) ->
    {Reply, NewState} = execute(Msg, State),
    {reply, Reply, NewState};
handle_call(Request, _From, State) ->
    {stop, {not_implemented, call, Request}, State}.

handle_cast(stop, State) ->
    NewState = delete_subscription(State),
    {stop, normal, NewState};
handle_cast(Msg, State) ->
    {stop, {not_implemented, cast, Msg}, State}.

handle_info(Info, State) ->
    {stop, {not_implemented, info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

decode(Binary) ->
    jiffy:decode(Binary, [return_maps]).

execute(Msg, State) ->
    try
        DecodedMsg = decode(Msg),
        execute_decoded(DecodedMsg, State)
    catch
        error:{N, invalid_json} ->
            {error_reply(null, [<<"invalid JSON after ">>,
                                integer_to_binary(N),
                                <<" characters">>]),
             State}
    end.

execute_decoded(#{<<"type">> := <<"start">>,
          <<"sequence">> := Sequence,
          <<"monitor">> := <<"CIN">>,
          <<"parameters">> := Parameters,
          <<"monitorID">> := MonitorId}, State = #{monitor_id := MonitorId}) ->
    % replace existing monitor
    State1 = delete_subscription(State),
    {ReplyState, State2} = create_subscription(Parameters, State1),
    {monitor_start_response(Sequence, MonitorId, ReplyState), State2};
execute_decoded(#{<<"type">> := <<"start">>,
          <<"sequence">> := Sequence,
          <<"monitor">> := <<"CIN">>,
          <<"parameters">> := Parameters}, State) ->
    % XXX check that monitorID is *not* included
    % create new monitor
    MonitorId = monitor_id(),
    State1 = State#{monitor_id := MonitorId},
    {ReplyState, State2} = create_subscription(Parameters, State1),
    {monitor_start_response(Sequence, MonitorId, ReplyState), State2};
execute_decoded(#{<<"sequence">> := Sequence}, State) ->
    {error_reply(Sequence, <<"invalid request">>), State}.

monitor_id() ->
    {A, B, C} = now(),
    integer_to_binary(((A * 1000000) + B) * 1000000 + C).

delete_subscription(State = #{subscription_id := SubscriptionId}) ->
    ok = dby_unsubscribe(SubscriptionId),
    State#{subscription_id := undefined}.

dby_unsubscribe(undefined) ->
    ok;
dby_unsubscribe(SubscriptionId) ->
    ok = dby:unsubscribe(SubscriptionId).

create_subscription(Parameters = #{<<"cinID">> := CinId}, State) ->
    SearchOptions = [breadth,
                     {max_depth, 4},
                     persistent,
                     {delivery, delivery_fn(State)},
                     {delta, delta_fn(State)}],
    {ok, FirstResult, SubscriptionId} =
        dby:subscribe(search_fn(Parameters), [],
                            leviathan_dby:dby_cen_id(CinId), SearchOptions),
    {FirstResult, State#{subscription_id := SubscriptionId}}.
    
% ipaddr <-> in endpoint <-> container <-> CEN
% XXX look for CENs for now
% XXX need endpoint in the correct CEN - for now use the alias.
search_fn(#{<<"cinID">> := CinId, <<"container_tag">> := Tag}) ->
    DbyCinId = leviathan_dby:dby_cen_id(CinId),
    % only containers with matching tag
    fun(_, ?MATCH_IPADDR(IpAddr),
                [{_, ?MATCH_IN_ENDPOINT(_, Alias), _},
                 {_, ContainerMD = ?MATCH_CONTAINER(ContId), _},
                 {FnDbyCinId, _, _}], Acc) when Alias == CinId,
                                                FnDbyCinId == DbyCinId ->
        case container_tag(ContainerMD) of
            Tag ->
                {continue, [#{<<"containerID">> => ContId,
                              <<"ipaddress">> => IpAddr,
                              <<"tag">> => Tag} | Acc]};
            _ ->
                {continue, Acc}
        end;
       (_, _, _, Acc) ->
        {continue, Acc}
    end;
search_fn(#{<<"cinID">> := CinId}) ->
    DbyCinId = leviathan_dby:dby_cen_id(CinId),
    % any container, tag is optional
    fun(_, ?MATCH_IPADDR(IpAddr),
                [{_, ?MATCH_IN_ENDPOINT(_, Alias), _},
                 {_, ContMetadata = ?MATCH_CONTAINER(ContId), _},
                 {FnDbyCinId, _, _}], Acc) when Alias == CinId,
                                                FnDbyCinId == DbyCinId ->
        Tag = container_tag(ContMetadata),
        {continue, [#{<<"containerID">> => ContId,
                      <<"ipaddress">> => IpAddr,
                      <<"tag">> => Tag} | Acc]};
       (_, _, _, Acc) ->
        {continue, Acc}
    end.

delta_fn(#{monitor_id := MonitorId}) ->
    fun(Old, New) ->
        Created = maps:values(
                    maps:without(list_containers(Old), container_map(New))),
        {delta, #{
            <<"type">> => <<"event">>,
            <<"monitorID">> => MonitorId,
            <<"event">> => <<"create">>,
            <<"message">> => Created
        }}
    end.

list_containers(Containers) ->
    [ContId || #{<<"containerID">> := ContId} <- Containers].

container_map(Containers) ->
    maps:from_list([{ContId, Cont} ||
                Cont = #{<<"containerID">> := ContId} <- Containers]).

container_tag(#{?MDVALUE(<<"tag">>, Tag)}) ->
    Tag;
container_tag(_) ->
    null.

delivery_fn(#{websocket_pid := WebsocketPid}) ->
    fun(Message) ->
        lr_monitor_handler:send(WebsocketPid, jiffy:encode(Message)),
        ok
    end.

monitor_start_response(Sequence, MonitorId, ReplyState) ->
    jiffy:encode(
        #{
            <<"type">> => <<"response">>,
            <<"sequence">> => Sequence,
            <<"response">> => #{
                <<"monitorID">> => MonitorId,
                <<"state">> => ReplyState
            }
        }
    ).

error_reply(Sequence, Message) ->
    jiffy:encode(
        #{
            <<"type">> => <<"error">>,
            <<"sequence">> => Sequence,
            <<"response">> => #{
                <<"message">> => Message
            }
        }
    ).
