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
    Id = set_id(player),
    P = #creature{pos = Player#player.pos,
		  id = Id,
		  name = binary_to_list(Name),
		  health = Player#player.health,
		  direction = Player#player.direction,
		  outfit = Player#player.outfit,
		  light = {0,0},
		  speed = 65000,
		  skull = 1,
		  shield = 1},
    ets:insert(creatures,P),
    Reply0 = <<16#0A,
	      (P#creature.id):32/?UINT, % Player ID
	      16#32,
	      16#00,
	      16#00, % Custom flag - can report bugs

	      16#64, % Map description starts
	      X:16/?UINT, Y:16/?UINT, (Pos#coord.z), % Coord (X,Y,Z)
	      (tibia_message:map_description(Pos#coord{x=X-8,y=Y-6},18,14))/binary,
	      131, % Magic effect
	      X:16/?UINT, Y:16/?UINT, (Pos#coord.z), % Magic effect pos (X,Y,Z)
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
	      160, % Player stats begin
	      175,0, % Health
	      185,0, % Max health
	      236,144,0,0, % Free capacity * 100
	      0,0,0,0, % Experience NOTE: Windows client debugs after 16#7FFFFFFF exp
	      1,0, % Player level
	      0, % Level percent
	      35,0, % Mana
	      35,0, % Max mana
	      0, % Magic level
	      0, % Magic level percent
	      100, % Soul points
	      32,13, % Stamina minutes
	      161, % Player skills begin
	      10, % Fist skill
	      0,  % Fist percent
	      10, % Club skill
	      12, % Club percent
	      10, % Sword skill
	      0,  % Sword percent
	      10, % Axe skill
	      0,  % Axe percent
	      10, % Dist skill
	      0,  % Dist percent
	      10, % Shield skill
	      0,  % Shield percent
	      10, % Fish skill
	      0,  % Fish percent
	      130, % World light
	      250, % Light level
	      215, % Light color
	      141, % Creature light
	      233,3,0,16, % Creature ID
	      0, % Light level
	      0, % Light color
	      180, % Text message
	      24, % Message class
	      21,0, % Message size
	      % Message begin
	      87,101,108,99,111,109,101,32,116,111,32,
	      80,111,119,101,114,102,108,105,112,33,
	      % Message end
	      180, % Text message
	      24, % Message class
	      48,0, % Message size
	      % Message begin
	      89,111,117,114,32,108,97,115,116,32,118,105,
	      115,105,116,32,119,97,115,32,111,110,32,83,
	      117,110,32,74,117,110,32,50,56,32,49,52,58,
	      49,56,58,48,55,32,50,48,48,57,46
	      % Message end
	      >>,
    Reply = xtea:encrypt(Key, <<(byte_size(Reply0)):16/?UINT,
			       Reply0/binary>>),
    %%Reply= get_map_description(Player#player.pos),
    %%WaitList = <<"waitlist ftw">>,
    %%Size = byte_size(WaitList),
    %%Reply = xtea:encrypt(Key, <<(Size+4):16/?UINT,16#16:8/?UINT,Size:16/?UINT,(WaitList)/binary,20:8/?UINT>>),

    {check_account(State#state{key = Key,
			       player = Player#player{id=Id}}, Acc, Pass),
     <<(byte_size(Reply)+4):16/?UINT,
      (erlang:adler32(Reply)):32/?UINT,
      Reply/binary>>}.


check_account(State, Acc, Pass) ->
    if byte_size(Acc) =:= 0 ->
	    tibia_proxy:disconnect(State,
				   <<"You need to enter an account number.">>);
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
	    get(id_player);
	Prev ->
	    put(id_player, Prev+1),
	    get(id_player)
    end;
set_id(npc) ->
    case get(id_npc) of
	undefined ->
	    put(id_npc,16#30000001),
	    get(id_npc);
	Prev ->
	    put(id_npc, Prev+1),
	    get(id_npc)
    end;
set_id(monster) ->
    case get(id_monster) of
	undefined ->
	    put(id_monster,16#60000001),
	    get(id_monster);
	Prev ->
	    put(id_monster, Prev+1),
	    get(id_monster)
    end.
    
