%%%-------------------------------------------------------------------
%%% File    : tibia_proxy.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  4 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_proxy).

-compile(export_all).

-include("tibia.hrl").

-define(PROXY_PORT, 7172).
-define(SERVER_PORT, 7175).
-define(SERVER_IP, "localhost").


install() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(tries, [{type,set},
				{attributes, record_info(fields,tries)}]),
    mnesia:create_table(player, [{type,set},
				 {disc_copies, [node()]},
				 {attributes, record_info(fields,player)}]),

    mnesia:create_table(account, [{type,set},
				  {disc_copies, [node()]},
				  {attributes, record_info(fields,account)}]).

    


start() ->
    start(?PROXY_PORT).

start(Port) ->
    crypto:start(),
    mnesia:start(),
    spawn(fun() -> tcp_start(Port) end).

tcp_start(ListenPort) ->
    process_flag(trap_exit, true),
    tibia_items:load("items.otb", "items.xml"),
    tibia_iomap:load("test.otbm"),
    {ok, ListenSocket} = gen_tcp:listen(ListenPort, [binary, inet]),
    
    io:format("Server is running.\n", []),
    tcp_listen_loop(ListenSocket, []).


tcp_listen_loop(ListenSocket, Pids) ->
    case gen_tcp:accept(ListenSocket, 500) of
	{ok, Socket} ->
	    Pid = spawn_link(?MODULE, tcp_init_connect,
			     [#state{client_socket = Socket,
				     server_pid = self()}]),
	    gen_tcp:controlling_process(Socket, Pid),
	    ?MODULE:tcp_listen_loop(ListenSocket, [Pid|Pids]);
	{error, timeout} ->
	    check_event(ListenSocket, Pids);
	{error, Reason} ->
	    io:format("{error, ~p}\n", [Reason])
    end.

check_event(ListenSocket, Pids) ->
    receive
	{'EXIT', Pid, {logout, Name}} ->
	    io:format("~p has logged out.\n", [Name]),
	    Pids2 = lists:delete(Pid,Pids),
	    check_event(ListenSocket, Pids2);
	stop ->
	    [Pid ! shutdown || Pid <- Pids],
	    io:format("Server stopped", []),
	    exit(stop)
    after 0 ->
	    ?MODULE:tcp_listen_loop(ListenSocket, Pids)
    end.


tcp_init_connect(State) ->
%%     case gen_tcp:connect(?SERVER_IP, ?SERVER_PORT, [binary, inet]) of
%% 	{ok, ServerSocket} ->
	    tcp_loop(State#state{server_socket = erverSocket}).%;
%% 	Any ->
%% 	    io:format("tcp_init_connect failed\n Returned: ~p\n", [Any])
%%     end.

tcp_loop(State=#state{client_socket = ClientSocket,
		      server_socket = ServerSocket}) ->
    inet:setopts(ClientSocket, [{active, once}]),
    %%inet:setopts(ServerSocket, [{active, once}]),
    receive
	{tcp, ServerSocket, <<Len:16/?UINT,Data:Len/binary>>} ->
	    State2 = tibia_parse:parse_server_packet(State, Data),
	    %%gen_tcp:send(ClientSocket, P),
	    ?MODULE:tcp_loop(State2);

	{tcp, ClientSocket,  <<Len:16/?UINT,Data:Len/binary>>} ->
	    State2 = tibia_parse:parse_client_packet(State, Data),
	    %%gen_tcp:send(ServerSocket, P),
	    ?MODULE:tcp_loop(State2);

	{tcp, ClientSocket,  <<Len:16/?UINT,Data1/binary>>} ->
	    Size = size(Data1),
	    {ok, Data2} = gen_tcp:recv(ClientSocket, Len-Size),
	    Data = <<Data1/binary,Data2/binary>>,
	    %%gen_tcp:send(ServerSocket, P),
	    State2 = tibia_parse:parse_client_packet(State, Data),
	    ?MODULE:tcp_loop(State2);

	{tcp, ServerSocket,  <<Len:16/?UINT,Data1/binary>>} ->
	    Size = size(Data1),
	    {ok, Data2} = gen_tcp:recv(ServerSocket, Len-Size),
	    Data = <<Data1/binary,Data2/binary>>,
	    State2 = tibia_parse:parse_server_packet(State, Data),
	    %%gen_tcp:send(ClientSocket, P),
	    ?MODULE:tcp_loop(State2);

	{tcp_closed, ServerSocket} ->
	    gen_tcp:close(ClientSocket),
	    ok;
	{tcp_closed, ClientSocket} ->
	    %%gen_tcp:close(ServerSocket),
	    ok;
	shutdown ->
	    gen_tcp:close(ClientSocket);
	Any ->
	    io:format("tcp unknown message ~p\n", [Any]),
	    ?MODULE:tcp_loop(State)
    end.

verify_checksum(PacketChecksum, Message) ->
    MsgChecksum = erlang:adler32(Message),
    if PacketChecksum =:= MsgChecksum ->
	    io:format("Checksum matches. ~p\n", [MsgChecksum]),
	    true;
       true ->
	    io:format("Checksum doesn't match.\n"),
	    false
    end.


disconnect(#state{client_socket = ClientSocket,
		  server_socket = ServerSocket,
		  key = Key}, Error) ->
    Reply = xtea:encrypt(Key, <<(size(Error)+3):16/?UINT,16#0A:8/?UINT,
			       (size(Error)):16/?UINT, Error/binary>>),
    Reply2 = <<(size(Reply)+4):16/?UINT,
	      (erlang:adler32(Reply)):32/?UINT,
	      Reply/binary>>,
    gen_tcp:send(ClientSocket, Reply2),
    gen_tcp:close(ClientSocket),
    %%gen_tcp:close(ServerSocket),
    exit(disconnect).
