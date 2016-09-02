%%%-------------------------------------------------------------------
%% @doc vibecast public API
%% @end
%%%-------------------------------------------------------------------

-module(vibecast_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(vibecast, port),
    {ok, Dir} = application:get_env(mp3_dir),
    io:format("Starting vibecast server at port ~p streaming mp3" ++
		  " data from ~p~n", [Port, Dir]),
    vibecast_sup:start_link([Port, Dir]).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
