-module(mp3_player).
-behaviour(gen_server).

-purpose("Mp3 player process module.").
-author("Anton Zhiliuk [crashtown.pal@gmail.com]").

%% public interface
-export([start_link/1, subscribe/0]).
%% timer callback
-export([stream_tick/0]).
%% genserver callbacks
-export([init/1, terminate/2, code_change/3, handle_cast/2, handle_call/3,
	 handle_info/2]).


-record(state, {files :: [file:filename_all()],
		current :: [file:filename_all()],
		timer :: timer:tref(),
		subscribers :: [pid()]}).

%%% Public interface -----------------------------------------------------------

start_link(Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Dir], []).

subscribe() ->
    gen_server:cast(?MODULE, {subscribe, self()}).

%%% Timer callback -------------------------------------------------------------

stream_tick() ->
    gen_server:cast(?MODULE, send_chunk).

%%% gen_server callbacks -------------------------------------------------------

init(Dir) ->
    io:format("Starting player in dir ~p~n", [Dir]),
    {ok, TRef} = timer:apply_interval(1000, ?MODULE, stream_tick, []),
    {ok, [File|Files]} = mp3_files(Dir),
    State = #state{files = Files,
		   current = File,
		   timer = TRef,
		   subscribers = []},
    {ok, State}.

terminate(_Reason, #state{timer = TRef}) ->
    timer:cancel(TRef),
    ok.

handle_cast({subscribe, Pid}, #state{subscribers = Subscribers} = State) ->
    {noreply, State#state{subscribers = [Pid|Subscribers]}};
handle_cast(send_chunk, #state{subscribers = Subscribers} = State) ->
    lists:foreach(fun(Subscriber) ->
			  Subscriber ! {data, <<"KEK\r\n">>}
		  end, Subscribers),
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Version, State, _Extra) ->
    {ok, State}.

%%% internal functions ---------------------------------------------------------

mp3_files(Dir) ->
    {ok, Re} = re:compile(".mp3", [caseless]),
    {ok, Files} = file:list_dir(Dir),
    Files1 = lists:filtermap(fun(File) ->
				     case re:run(File, Re) of
					 {match, _} ->
					     {true, filename:join(Dir, File)};
					 _ -> false
				     end
			     end, Files),
    {ok, Files1}.
