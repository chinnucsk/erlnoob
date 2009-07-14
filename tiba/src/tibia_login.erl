%%%-------------------------------------------------------------------
%%% File    : tibia_login.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 30 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_login).

-compile(export_all).

-include("tibia.hrl").

-define(LOOKUP_ACC(Acc, Pass),
	fun() ->
		Pattern = #account{name = binary_to_list(Acc),
				   password = binary_to_list(Pass),
				   _ = '_'},
		mnesia:match_object(Pattern)
	end).

-define(LOOKUP_PLAYERS(Acc),
	fun() ->
		Pattern = #player{account = binary_to_list(Acc),
				  _ = '_'},
		mnesia:match_object(Pattern)
	end).

-define(LOOKUP_PLAYER(Name),
	fun() ->
		Pattern = #player{name = Name,
				  _ = '_'},
		mnesia:match_object(Pattern)
	end).

parse_login_package(State, <<_:12/binary,Msg:128/binary>>) ->
    Decrypted = crypto:rsa_private_decrypt(Msg, [?e, ?n, ?d], rsa_no_padding),

    <<0:8/?UINT,K1:32/?UINT,K2:32/?UINT,
     K3:32/?UINT,K4:32/?UINT,
     AccSize:16/?UINT,Acc:AccSize/binary,
     PassSize:16/?UINT,Pass:PassSize/binary,_/binary>> = Decrypted,
    Key = #key{k1 = K1,
	       k2 = K2,
	       k3 = K3,
	       k4 = K4},
    {atomic, Chars} = mnesia:transaction(?LOOKUP_PLAYERS(Acc)),
    Chars2 = tibia_parse:build_characters(Chars),
    Motd =  tibia_parse:build_motd(),
    Uncrypted = <<(byte_size(Chars2)+byte_size(Motd)):16/?UINT,
		 Motd/binary, Chars2/binary>>,
    Reply = xtea:encrypt(Key, Uncrypted),
    {check_account(State#state{key = Key}, Acc, Pass),
     <<(byte_size(Reply)+4):16/?UINT,
      (erlang:adler32(Reply)):32/?UINT,
      Reply/binary>>}.


parse_first_game_packet(State, Msg) ->
    Decrypted = crypto:rsa_private_decrypt(Msg, [?e, ?n, ?d], rsa_no_padding),
    <<0:8/?UINT,K1:32/?UINT,K2:32/?UINT,
     K3:32/?UINT,K4:32/?UINT,_GameMasterLogin:8/?UINT,
     AccSize:16/?UINT,Acc:AccSize/binary,
     NameSize:16/?UINT, Name:NameSize/binary,
     PassSize:16/?UINT, Pass:PassSize/binary,_/binary>> = Decrypted,
    Key = #key{k1 = K1,
	       k2 = K2,
	       k3 = K3,
	       k4 = K4},

	case mnesia:transaction(?LOOKUP_PLAYER(binary_to_list(Name))) of
	    {atomic, [Player=#player{pos=Pos=#coord{x=X,y=Y}}]} ->
		Player;
	    _ ->
		io:format("Couldnt find player: ~p\n", [Name]),
		Player = error,
		Pos=X=Y= error,
		tibia_proxy:disconnect(State,
				       <<"You need to create a character.">>)
	end,
    P = #creature{pos = Player#player.pos,
		  id = set_id(player),
		  name = binary_to_list(Name),
		  health = {100,200},
		  direction = Player#player.direction,
		  outfit = Player#player.outfit,
		  light = {0,0},
		  speed = 220,
		  skull = 1,
		  shield = 1},
    io:format("~p\n", [P#creature.id]),
    ets:insert(creatures,P),
    Reply0 = <<16#0A,
	      (P#creature.id):32/?UINT, % Player ID
	      16#32,
	      16#00,
	      16#00, % Custom flag - can report bugs

	      16#64, % Map description starts
	      4,0, 4,0, 7, % Coord (X,Y,Z)
	      (tibia_message:map_description(Pos#coord{x=X-8,y=Y-6},18,14))/binary,
	      131, % Magic effect
	      4,0, 4,0, 7, % Magic effect pos (X,Y,Z)
	      15, % Magic effect type

	      121,1,       % SLOT_HEAD
	      121,2,       % SLOT_NECKLACE
	      121,3,       % SLOT_BACKPACK
	      120,4,       % SLOT_ARMOR
	      233,13,      % Armor slot item ID
	      121,5,       % SLOT_RIGHT
	      120,6,       % SLOT_LEFT
	      198,12,      % Left slot item ID
	      121,7,  	   % SLOT_LEGS
	      121,8,   	   % SLOT_FEET
	      121,9,	   % SLOT_RING
	      121,10,      % SLOT_AMMO
	      160,175,0,185,0,236,144,0,0,0,0,0,0,
	      1,0,0,35,0,35,0,0,0,100,32,13,161,10,0,10,
	      12,10,0,10,0,10,0,10,0,10,0,130,250,215,141,
	      233,3,0,16,0,0,180,24,21,0,87,101,108,99,111,
	      109,101,32,116,111,32,80,111,119,101,114,102,
	      108,105,112,33,180,24,48,0,89,111,117,114,32,
	      108,97,115,116,32,118,105,115,105,116,32,119,
	      97,115,32,111,110,32,83,117,110,32,74,117,110,
	      32,50,56,32,49,52,58,49,56,58,48,55,32,50,48,48,57,46>>,
    Reply = xtea:encrypt(Key, <<(byte_size(Reply0)):16/?UINT,
			       Reply0/binary>>),
    %%Reply= get_map_description(Player#player.pos),
    %%WaitList = <<"waitlist ftw">>,
    %%Size = byte_size(WaitList),
    %%Reply = xtea:encrypt(Key, <<(Size+4):16/?UINT,16#16:8/?UINT,Size:16/?UINT,(WaitList)/binary,20:8/?UINT>>),

    {check_account(State#state{key = Key,
			       player = Player}, Acc, Pass),
     <<(byte_size(Reply)+4):16/?UINT,
      (erlang:adler32(Reply)):32/?UINT,
      Reply/binary>>}.


check_account(State, Acc, Pass) ->
    if byte_size(Acc) =:= 0 ->
	    tibia_proxy:disconnect(State,
				   <<"You need to enter an account number.">>);
	    ok;
       true ->
	    ignore
    end,
    
    case mnesia:transaction(?LOOKUP_ACC(Acc,Pass)) of
	{atomic, [Account]} ->
	    State#state{account = Account};
	{atomic, []} ->
	    tibia_proxy:disconnect(State,
				   <<"Account name or password is incorrect.">>)
    end.



%% 16#20000000 - Player
%% 16#30000000 - NPC
%% 16#40000000 - Monster
set_id(player) ->
    case get(id_player) of
	undefined ->
	    put(id_player,16#10000001),
	    16#10000001;
	Prev ->
	    put(id_player, Prev+1),
	    Prev+1
    end;
set_id(npc) ->
    case get(id_npc) of
	undefined ->
	    put(id_npc,16#30000001),
	    16#30000001;
	Prev ->
	    put(id_npc, Prev+1),
	    Prev+1
    end;
set_id(monster) ->
    case get(id_monster) of
	undefined ->
	    put(id_monster,16#60000001),
	    16#60000001;
	Prev ->
	    put(id_monster, Prev+1),
	    Prev+1
    end.
    
