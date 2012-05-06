%%% -*- mode: erlang -*-
%%% $Id: echo.erlang,v 1.1.1.1 2004-05-19 18:09:42 bfulgham Exp $
%%% http://www.bagley.org/~doug/shootout/
%%% with help from Sebastian Strollo

%%% TBD - need to add check for valid response.

-module(echo).
-export([main/0, main/1, client/2, server/1]).
-export([small/0,medium/0,big/0]).

small() -> 12.
medium() -> 16.
big() -> 20.

-define(DATA, <<"Hello there sailor\n">>).

main() -> main(['1']).
main([Arg]) ->
    N = list_to_integer(atom_to_list(Arg)),
    ServerSock = create_server_sock(),
    spawn(?MODULE, client, [N, socket_port(ServerSock)]),
    server(ServerSock),
    init:stop().

create_server_sock() ->
    {ok, LSock} = gen_tcp:listen(0, [binary]),
    LSock.

socket_port(Sock) ->
    {ok, Port} = inet:port(Sock),
    Port.

client(N, ServerPort) ->
    {ok, Sock} = gen_tcp:connect("localhost", ServerPort, [binary]),
    client_loop(N, Sock),
    gen_tcp:close(Sock).

client_loop(0, Sock) -> ok;
client_loop(N, Sock) ->
    ok = gen_tcp:send(Sock, ?DATA),
    receive
	{tcp, Sock, _} -> client_loop(N-1, Sock);
	{tcp_closed, Sock} -> ok
    end.

server(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    server_loop(Sock, 0),
    gen_tcp:close(LSock).

server_loop(Sock, Bytes) ->
    receive
	{tcp, Sock, Packet} ->
	    ok = gen_tcp:send(Sock, Packet),
	    server_loop(Sock, Bytes + size(Packet));
	{tcp_closed, Sock} ->
	    io:format("server processed ~w bytes~n", [Bytes]),
	    gen_tcp:close(Sock)
    end.
