-module(lr_encode).

-export([to_json/1]).

to_json(#{cenID := CenId, contIDs := ContIds, wire_type := WireType}) ->
    jiffy:encode({[
        {<<"cenID">>, to_json(CenId)},
        {<<"contIDs">>, [to_json(ContId) || ContId <- ContIds]},
        {<<"wire_type">>, to_json(WireType)}
    ]});
to_json(B) when is_binary(B) ->
    B;
to_json(null) ->
    null;
to_json(I) when is_integer(I) ->
    I;
to_json(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
to_json(L) when is_list(L) ->
    list_to_binary(L).
