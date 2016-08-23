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
    % {ok, Dir} = application:get_env(vibecast, mp3_dir),
    ListenerSpec = ranch:child_spec(vibecast_shoutcast, 100,
				    ranch_tcp, [{port, Port}],
				    shoutcast_protocol, []),
    Mp3PlayerSpec = #{id => mp3_player,
		      start => {mp3_player, start_link, ["/Users/electrostation/Music/Twoism Records/OOT001 Various Artists - One on Twoism Vol.1 2007"]},
		      restart => permanent,
		      shutdown => 10000,
		      type => worker,
		      modules => []},

    {ok, {{one_for_one, 10, 10}, [ListenerSpec, Mp3PlayerSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
