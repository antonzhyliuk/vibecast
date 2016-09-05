%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(connections_handler).

-export([init/3,
	 content_types_provided/2,
	 to_json/2]).

init(_Type, Req, Opts) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State) ->
    ConnectionsCount = ranch_server:count_connections(vibecast_shoutcast),
    ConnCountBin = integer_to_binary(ConnectionsCount),
    Body = <<"{\"connections_count\": \"", ConnCountBin/binary, "\"}">>,
    {Body, Req, State}.
