%%%-------------------------------------------------------------------
%%% File    : tibia_parse.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 15 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_parse).

%%-export([parse_server_packet/2,xtea_encrypt/2,xtea_decrypt/2,
%%	 parse_client_packet/2,test/0]).
-compile(export_all).

-include("tibia.hrl").

-record(character, {name,server_name,ip,port}).

parse_server_packet(State,Packet = <<_Checksum:32/?UINT,Msg/binary>>) ->
    Decrypted = xtea:decrypt(State#state.key,Msg),
    <<_Size:16/?UINT, Protocol:8/?UINT,Message/binary>> = Decrypted,
    Reply =
	case Protocol of
	    16#14 ->
		io:format("~p\n", [State#state.account]),
		modify_login_packet(State#state.key,Message);
	    _ ->
		<<(size(Packet)):16/?UINT,Packet/binary>>
	end,
    %%io:format("", []),
    gen_tcp:send(State#state.client_socket, Reply),
    State.

modify_login_packet(Key, <<MotdSize:16/?UINT,Motd:MotdSize/binary,
			  16#64:8/?UINT,NumChars:8/?UINT,Chars/binary>>) ->
    {Characters,Rest} = get_characters(NumChars, Chars),
    <<PremDays:16/?UINT,_/binary>> = Rest,
    NewChars = build_characters(Characters),
    NewMsg = <<16#14:8/?UINT,(size(Motd)):16/?UINT,Motd/binary,
	      16#64:8/?UINT,(length(Characters)):8/?UINT,
	      NewChars/binary,PremDays:16/?UINT>>,
    Reply = xtea:encrypt(Key,<<(size(NewMsg)):16/?UINT,NewMsg/binary>>),
    <<(size(Reply)+4):16/?UINT,(erlang:adler32(Reply)):32/?UINT,Reply/binary>>.
    

build_characters(Chars) ->
    build_characters(Chars, <<(length(Chars)):8/?UINT>>).

build_characters([#player{name = Name}|Chars], Acc) ->
    build_characters(Chars, <<Acc/binary,(length(Name)):16/?UINT,
			     (list_to_binary(Name))/binary,
			     (byte_size(<<"powerflip">>)):16/?UINT,(<<"powerflip">>)/binary,
			     (list_to_binary([127,0,0,1]))/binary,7172:16/?UINT>>);
build_characters([], Acc) ->
    <<16#64:8/?UINT,Acc/binary,20:16/?UINT>>.

build_motd() ->
    Motd = <<"Welcome to pflip.">>,
    <<16#14,(byte_size(Motd)):16/?UINT,Motd/binary>>.

get_characters(NumChars,Chars) ->
    get_characters(NumChars, Chars, []).
    
get_characters(0,Rest, Acc) ->
    {Acc,Rest};
get_characters(NumChars, <<NameSize:16/?UINT,Name:NameSize/binary,
			  ServerSize:16/?UINT,Server:ServerSize/binary,
			  ServerIP:4/binary,Port:16/?UINT,Chars/binary>>, Acc) ->
    Char = #character{name = Name,
		      server_name = Server,
		      ip = list_to_tuple(binary_to_list(ServerIP)),
		      port = Port},
    get_characters(NumChars-1,Chars,[Char|Acc]).

parse_client_packet(State, Packet) when State#state.account =:= undefined ->
    <<_Checksum:32/?UINT,ProtocolId:8/?UINT,_OS:16/?UINT,
     _Version:16/?UINT,Msg/binary>> = Packet,
    case ProtocolId of
	?LOGIN_PROTOCOL ->
	    {State2, Reply} = tibia_login:parse_login_package(State, Msg),
	    gen_tcp:send(State2#state.client_socket, Reply),
	    State2;
	?GAME_PROTOCOL ->
	    {State2, Reply} = tibia_login:parse_first_game_packet(State, Msg),
	    gen_tcp:send(State2#state.client_socket, Reply),
	    State2;
	_ ->
	    io:format("Other protocol: ~p\n", [ProtocolId]),
	    State
    end;
parse_client_packet(State=#state{player = P},
		    <<_Checksum:32/?UINT,Msg/binary>>) ->
    Decrypted = xtea:decrypt(State#state.key, Msg),
    <<Size:16/?UINT, Msg2:Size/binary,_/binary>> = Decrypted,
    <<RecvByte,  Data/binary>> = Msg2,
    case RecvByte of
	16#14 ->
	    gen_tcp:close(State#state.client_socket),
	    exit({logout, State#state.player});
	16#a0 ->
	    ok;
	16#1E ->
	    ok;
	Dir when Dir >= 16#6F, Dir =< 16#72 ->
	    [C] = ets:lookup(creatures, P#player.id),
	    Turn =
		tibia_message:creature_turn(C#creature.id,
					    C#creature.pos,
					    Dir - 16#6F),
	    true = ets:insert(creatures,C#creature{direction = Dir - 16#6F}),
	    Reply = prepare_send(State#state.key,Turn),
	    gen_tcp:send(State#state.client_socket,Reply);
	Dir when Dir >= 16#65, Dir =< 16#68 ->
	    [C] = ets:lookup(creatures, P#player.id),
	    {Move,NewPos} =
		tibia_message:move_creature(C#creature.pos,
					    Dir),
	    true = ets:insert(creatures,C#creature{pos = NewPos,
						   direction = Dir - 16#65}),
	    Reply = prepare_send(State#state.key, Move),
	    gen_tcp:send(State#state.client_socket,Reply);
%% 	Dir when Dir >= 16#6A, Dir =< 16#6D ->
%% 	    [C] = ets:lookup(creatures, P#player.name),
%% 	    {Move,NewPos} =
%% 		tibia_message:move_creature(C#creature.pos,
%% 					    Dir),
%% 	    true = ets:insert(creatures,C#creature{pos = NewPos,
%% 						   direction = Dir - 16#65}),
%% 	    Reply = prepare_send(State#state.key, Move),
%% 	    gen_tcp:send(State#state.client_socket,Reply);
	16#69 ->
	    [C] = ets:lookup(creatures, P#player.id),
	    Reply = prepare_send(State#state.key,
				 tibia_message:cancel_walk(C#creature.direction)),
	    gen_tcp:send(State#state.client_socket,Reply);

	_ ->
	    io:format("Msg: ~p ~p\n", [RecvByte, Data])
    end,
    State.



prepare_send(Key, Bin) when is_binary(Bin) ->
    Encrypted = xtea:encrypt(Key, <<(byte_size(Bin)):16/?UINT,Bin/binary>>),
    <<(byte_size(Encrypted)+4):16/?UINT,
	 (erlang:adler32(Encrypted)):32/?UINT,
	 Encrypted/binary>>.


test(Key,
     <<_:6/binary, Reply/binary>>,
     <<_:6/binary, Packet/binary>>) ->
    io:format("Reply : ~p\n", [xtea:decrypt(Key, Reply)]),
    io:format("Packet: ~p\n", [xtea:decrypt(Key, Packet)]).
    
