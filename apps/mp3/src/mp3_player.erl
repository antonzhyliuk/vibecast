-module(mp3_player).
-behaviour(gen_server).

-purpose("Mp3 player process module.").
-author("Anton Zhiliuk [crashtown.pal@gmail.com]").

%% public interface
-export([start_link/1, subscribe/0]).
%% timer callback
-export([stream_chunk/0]).
%% genserver callbacks
-export([init/1, terminate/2, code_change/3, handle_cast/2, handle_call/3,
	 handle_info/2]).

-type current_file() :: {file, file:filename_all(), file:fd()}.
-record(state, {files        :: [file:filename_all()],
		current_file :: current_file(),
		timer        :: timer:tref(),
		subscribers  :: [pid()]}).

-define(CHUNKSIZE, 24576).

%%% Public interface -----------------------------------------------------------

start_link(Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Dir], []).

subscribe() ->
    gen_server:cast(?MODULE, {subscribe, self()}).

%%% Timer callback -------------------------------------------------------------

stream_chunk() ->
    gen_server:cast(?MODULE, stream_chunk).

%%% gen_server callbacks -------------------------------------------------------

init(Dir) ->
    io:format("Starting player in dir ~p~n", [Dir]),
    {ok, TRef} = timer:apply_interval(1000, ?MODULE, stream_chunk, []),
    {ok, [File|Files]} = mp3_files(Dir),
    State = #state{files = Files,
		   current_file = new_current_file(File),
		   timer = TRef,
		   subscribers = []},
    {ok, State}.

terminate(_Reason, #state{timer = TRef}) ->
    timer:cancel(TRef),
    ok.

handle_cast({subscribe, Pid}, #state{subscribers = Subscribers} = State) ->
    {noreply, State#state{subscribers = [Pid|Subscribers]}};
handle_cast(stream_chunk, #state{files = Files, subscribers = Subscribers,
				 current_file = {file, Name, File}} = State) ->
    State = case file:read(File, ?CHUNKSIZE) of
		eof ->
		    [NewFile|RestFiles] = Files,
		    NewCurrFile = new_current_file(NewFile),
		    {file, _Name, File1} = NewCurrFile,
		    {ok, Chunk} = file:read(File1, ?CHUNKSIZE),
		    send(Subscribers, Chunk),
		    State#state{files = RestFiles ++ [Name],
				current_file = NewCurrFile};
		{ok, Chunk} ->
		    send(Subscribers, Chunk),
		    State
	    end,
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Version, State, _Extra) ->
    {ok, State}.

%%% internal functions ---------------------------------------------------------

new_current_file(Filename) ->
    {ok, Fd} = file:open(Filename, [read]),
    {file, Filename, Fd}.

send(Recepients, Data) ->
    lists:foreach(fun(Recepient) ->
			  Recepient ! {data, Data}
		  end, Recepients).

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
