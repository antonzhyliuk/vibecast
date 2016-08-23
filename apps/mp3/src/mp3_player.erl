-module(mp3_player).
-author("Anton Zhiliuk").
-purpose("Mp3 player process module").

-compile(export_all).

loop([]) ->
    io:format("End of streaming ~n");
loop([Current | NextTracks]) ->
    stream_file(Current),
    loop(NextTracks).

start_link(Dir) ->
    proc_lib:start_link(?MODULE, init, [Dir, self()]).

stream_file(File) ->
    io:format("Stream: ~p~n", [File]),
    timer:sleep(2000).

init(Dir, Parent) ->
    io:format("Starting player in dir ~p~n", [Dir]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(mp3_files(Dir)).

mp3_files(Dir) ->
    {ok, Re} = re:compile(".mp3", [caseless]),
    {ok, Files} = file:list_dir(Dir),
    lists:filtermap(fun(File) ->
			    case re:run(File, Re) of
				{match, _} ->
				    {true, filename:join(Dir, File)};
				_ -> false
			    end
		    end, Files).
