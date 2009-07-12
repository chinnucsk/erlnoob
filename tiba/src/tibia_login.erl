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
      Reply/binary>>
    }.


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

    Player =
	case mnesia:transaction(?LOOKUP_PLAYER(binary_to_list(Name))) of
	    {atomic, [P]} ->
		P;
	    _ ->
		io:format("Couldnt find player: ~p\n", [Name]),
		exit(could_not_find_player)
	end,
    Reply0 = <<16#0A,
	      233,3,0,16, % Player ID
	      16#32,
	      16#00,
	      16#00, % Custom flag - can report bugs

	      16#64, % Map description starts
	      4,0, 4,0, 7, % Coord (X,Y,Z)
	      117,255, % Skip , 16#FF
	      168,17, % Tile grass - cid 4520 sid - 4531/9048

	      %% Creature start
	      97,0, % Not known 16#61:16/?UINT
	      0,0,0,0, % Remove
	      233,3,0,16, % Creature ID
	      (byte_size(<<"Svett">>)):16/?UINT, % Name len
	      <<"Svett">>/binary, % Name
	      0, % Health in percent, round((CurrentHP / MaxHP)*100).
 	      2, % Direction 0-3   2 is facing down
	      128,0, % Looktype
	      %%0,0, % if looktype is 0 then show an item instead
	      %%23,12, % axe ring
	      44, % Look head
	      44, % Look body
	      44, % Look legs
	      44, % Look feet
	      0,  % Look addons
	      0, % Light level
	      0, % Light color
	      220,0, % Char speed
	      0, % Skull (0-5)
	      0, % Party shield (0-10)
	      %% Creature end

	      255,255,
	      255,255, % No tile
	      255,255, % No tile
	      255,255, % No tile
	      255,255, % No tile
	      255,255, % No tile
	      255,255, % No tile
	      105,255,
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

    {check_account(State#state{key = Key}, Acc, Pass),
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



get_map_description(C=#coord{z=Z}) ->
    if Z > 7 ->
	    EndZ =
		if 15 > Z+2 -> 15;
		   true ->     Z+2
		end,
	    get_map_description(C#coord{z=Z-2},Z, EndZ, 1, <<>>, -1);
       true ->
	    get_map_description(C#coord{z=7},Z, 0, -1, <<>>, -1)
    end.

get_map_description(_C=#coord{z=_Z},_Z2, _EndZ, _Step, _Acc, _Skip) ->
    <<232,0,10,244,3,0,16,50,0,0,100,4,0,4,0,7,117,255,163,17,79,4,76,4,77,4,78,4,97,0,0,0,0,0,244,3,0,16,5,0,83,118,101,116,116,95,2,128,0,44,44,44,44,0,0,0,220,0,0,0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,105,255,131,4,0,4,0,7,11,121,1,121,2,121,3,120,4,233,13,121,5,120,6,198,12,121,7,121,8,121,9,121,10,160,175,0,185,0,236,144,0,0,0,0,0,0,1,0,0,35,0,35,0,0,0,100,32,13,161,10,0,10,12,10,0,10,0,10,0,10,0,10,0,130,208,215,141,244,3,0,16,0,0,180,24,21,0,87,101,108,99,111,109,101,32,116,111,32,80,111,119,101,114,102,108,105,112,33,180,24,48,0,89,111,117,114,32,108,97,115,116,32,118,105,115,105,116,32,119,97,115,32,111,110,32,83,117,110,32,74,117,110,32,50,56,32,49,51,58,50,53,58,48,54,32,50,48,48,57,46,51,51,51,51,51,51>>.

