%%%-------------------------------------------------------------------
%% @doc vibecast top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(vibecast_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, Port} = application:get_env(vibecast, port),
    ListenerSpec = ranch:child_spec(vibecast_shoutcast, 100,
				    ranch_tcp, [{port, Port}],
				    shoutcast_protocol, []),
    {ok, {{one_for_one, 10, 10}, [ListenerSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
