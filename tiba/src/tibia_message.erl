%%%-------------------------------------------------------------------
%%% File    : tibia_message.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 28 Jun 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_message).

-include("tibia.hrl").

-compile(export_all).

-define(FLAG_BLOCK_SOLID, 1 bsl 0).
-define(FLAG_BLOCK_PROJECTILE, 1 bsl 1).
-define(FLAG_BLOCK_PATHFIND, 1 bsl 2).
-define(FLAG_HAS_HEIGHT, 1 bsl 3).
-define(FLAG_USEABLE, 1 bsl 4).
-define(FLAG_PICKUPABLE, 1 bsl 5).
-define(FLAG_MOVEABLE, 1 bsl 6).
-define(FLAG_STACKABLE, 1 bsl 7).
-define(FLAG_FLOORCHANGEDOWN, 1 bsl 8).
-define(FLAG_FLOORCHANGENORTH, 1 bsl 9).
-define(FLAG_FLOORCHANGEEAST, 1 bsl 10).
-define(FLAG_FLOORCHANGESOUTH, 1 bsl 11).
-define(FLAG_FLOORCHANGEWEST,1 bsl  12).
-define(FLAG_ALWAYSONTOP, 1 bsl 13).
-define(FLAG_READABLE, 1 bsl 14).
-define(FLAG_ROTABLE, 1 bsl 15).
-define(FLAG_HANGABLE, 1 bsl 16).
-define(FLAG_VERTICAL, 1 bsl 17).
-define(FLAG_HORIZONTAL, 1 bsl 18).
-define(FLAG_CANNOTDECAY, 1 bsl 19).
-define(FLAG_ALLOWDISTREAD, 1 bsl 20).
-define(FLAG_UNUSED, 1 bsl 21).
-define(FLAG_CLIENTCHARGES, 1 bsl 22).

add_item(#item_type{flags = Flags,
		    client_id = Cid}) ->
    if Flags band ?FLAG_STACKABLE =:= ?FLAG_STACKABLE ->
	    ok;
       true ->
	    ok
    end,
    <<Cid:16/?UINT>>.



creature_turn(ID, #coord{x=X,y=Y,z=Z}, Dir) ->
    <<16#6B,
     X:16/?UINT,Y:16/?UINT,Z,
     1,
     16#63:16/?UINT,
     ID:32/?UINT,
     Dir>>.


get_tile(Pos) ->
    ok.
