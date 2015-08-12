-module(lr_cowboy).

-export([bindings/2]).

% bind variables from cowboy
-spec bindings(Req0::term(), [atom()]) -> {Req1::term(), [term()]}.
bindings(Req, Vars) ->
    lists:foldl(
        fun(Var, {Req0, Acc}) ->
            {Val, Req1} = cowboy_req:binding(Var, Req0),
            {Req1, Acc ++ [Val]}
        end, {Req, []}, Vars).
