%%%-------------------------------------------------------------------
%% @doc vibecast top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(vibecast_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Port, Dir]) ->
    ListenerSpec = ranch:child_spec(vibecast_shoutcast, 100,
				    ranch_tcp, [{port, Port}],
				    shoutcast_protocol, []),
    Mp3PlayerSpec = #{id => mp3_player,
		      start => {mp3_player, start_link, [Dir]},
		      restart => permanent,
		      shutdown => 10000,
		      type => worker,
		      modules => []},

    {ok, {{one_for_one, 10, 10}, [ListenerSpec, Mp3PlayerSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
