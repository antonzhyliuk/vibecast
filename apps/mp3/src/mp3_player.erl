-module(mp3_player).
-behaviour(gen_server).

-purpose("Mp3 player process module.").
-author("Anton Zhiliuk [crashtown.pal@gmail.com]").

%% public interface
-export([start_link/1, subscribe/0, unsubscribe/0]).
%% timer callback
-export([stream_chunk/0]).
%% genserver callbacks
-export([init/1, terminate/2, code_change/3, handle_cast/2, handle_call/3,
	 handle_info/2]).

-record(current_file, { name :: file:filename_all(),
			file :: file:fd(),
			position :: pos_integer(),
			size :: pos_integer() }).

-record(state, { files        :: [file:filename_all()],
		 current_file :: #current_file{},
		 timer        :: timer:tref(),
		 subscribers  :: [pid()] }).

-define(CHUNKSIZE, 24576).

%%% Public interface -----------------------------------------------------------

start_link(Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Dir], []).

subscribe() ->
    gen_server:cast(?MODULE, {subscribe, self()}).

unsubscribe() ->
    gen_server:cast(?MODULE, {unsubscribe, self()}).

%%% Timer callback -------------------------------------------------------------

stream_chunk() ->
    gen_server:cast(?MODULE, stream_chunk).

%%% gen_server callbacks -------------------------------------------------------

init(Dir) ->
    io:format("Starting player in dir ~p~n", [Dir]),
    {ok, TRef} = timer:apply_interval(500, ?MODULE, stream_chunk, []),
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

handle_cast({unsubscribe, Pid}, #state{subscribers = Subscribers} = State) ->
    NewSubs = lists:delete(Pid, Subscribers),
    {noreply, State#state{subscribers = NewSubs}};

handle_cast(stream_chunk, #state{ subscribers = Subscribers,
				  current_file = #current_file{ position = Pos,
								size = Size,
								file = File
							      } = CurrentFile
				} = State) when Pos + ?CHUNKSIZE < Size ->
    {ok, Chunk} = file:pread(File, Pos, ?CHUNKSIZE),
    send(Subscribers, Chunk),
    NewCurrentFile = CurrentFile#current_file{position = Pos + ?CHUNKSIZE},
    {noreply, State#state{current_file = NewCurrentFile}};
handle_cast(stream_chunk, #state{ subscribers = Subscribers,
				  files = [NewFile|Rest],
				  current_file = #current_file{ name = Name,
								position = Pos,
								file = File,
								size = Size
							      } = CurrentFile
				} = State) ->
    {ok, Chunk1} = file:pread(File, Pos, ?CHUNKSIZE),
    AdditionalChunkSize = Pos + ?CHUNKSIZE - Size,
    NextCurrFile = #current_file{file = File1} = new_current_file(NewFile),
    {ok, Chunk2} = file:pread(File1, 0, AdditionalChunkSize),
    send(Subscribers, list_to_binary([Chunk1, Chunk2])),
    NewCurrentFile = NextCurrFile#current_file{position = AdditionalChunkSize},
    {noreply, State#state{current_file = NewCurrentFile,
			  files = Rest ++ [Name]}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Version, State, _Extra) ->
    {ok, State}.

%%% internal functions ---------------------------------------------------------

new_current_file(Filename) ->
    {ok, Fd} = file:open(Filename, [binary, read]),
    #current_file{name = Filename,
		  file =  Fd,
		  position = 0,
		  size = filelib:file_size(Filename)}.

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
