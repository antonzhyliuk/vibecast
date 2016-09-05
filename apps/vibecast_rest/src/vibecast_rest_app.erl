%%%-------------------------------------------------------------------
%% @doc vibecast_rest public API
%% @end
%%%-------------------------------------------------------------------

-module(vibecast_rest_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Port = 8844,
    io:format("Starting vibecast rest server at port ~p~n", [8844]),
    Dispatch = cowboy_router:compile([
				      {'_', [{"/", hello_handler, []}]}
				     ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8844}],
		      [{env, [{dispatch, Dispatch}]}]
		     ),
    vibecast_rest_sup:start_link().


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
