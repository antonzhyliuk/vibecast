-module(shoutcast_protocol).
-behaviour(ranch_protocol).

-purpose("Protocol for shoutcast based streaming").
-author("Anton Zhiliuk [crashtown.pal@gmail.com]").

-export([start_link/4]).
-export([init/4]).

-define(CHUNKSIZE, 24576).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts) ->
    ok = ranch:accept_ack(ListenerPid),
    stream_init(Socket, Transport),
    stream_loop(Socket, Transport).

stream_init(Socket, Transport) ->
    {ok, Data} = Transport:recv(Socket, 0, 5000),
    io:format("Received header!:~n~p~n", [Data]),
    % we try to match on \r\n\r\n at the end of header
    case lists:reverse(binary_to_list(Data)) of
	"\n\r\n\r" ++ _ ->
	    Transport:send(Socket, header_response()),
	    mp3_player:subscribe();
	_ ->
	    Transport:send(Socket, <<"ICY 400 Bad Request\r\n">>),
	    Transport:close(Socket)
    end.

stream_loop(Socket, Transport) ->
    receive
	{data, Data} ->
	    ?CHUNKSIZE = byte_size(Data), % chunk size contract
	    case Transport:send(Socket, [Data, <<0>>]) of % zero is meta header
		{error, closed} ->
		    mp3_player:unsubscribe(),
		    Transport:close(Socket);
		_ -> stream_loop(Socket, Transport)
	    end;
	Any ->
	    io:format("Strange messsage: ~p~n", [Any]),
	    stream_loop(Socket, Transport)
    end.

header_response() ->
    {ok, Port} = application:get_env(vibecast, port),
    ["ICY 200 OK\r\n",
     "icy-notice1: AE vibenation\r\n",
     "icy-notice2: Erlang Shoutcast server\r\n",
     "icy-name: Vibecast\r\n",
     "icy-genre: Ambient Techno\r\n",
     "icy-url: http://localhost:" ++ integer_to_list(Port) ++ "\r\n",
     "content-type: audio/mpeg\r\n",
     "icy-pub: 1\r\n",
     "icy-metaint:" ++ integer_to_list(?CHUNKSIZE) ++ "\r\n",
     "icy-metadata:0\r\n",
     "icy-br: 96\r\n",
     "\r\n"].
