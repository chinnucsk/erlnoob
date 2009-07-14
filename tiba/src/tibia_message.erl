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



max(A,B) ->
    if A > B -> A;
       true ->  B
    end.
		     
	    

map_description(Pos=#coord{z=Z}, Width,Height) ->
    if Z > 7 ->
	    map_description(Pos,
			    Width,Height,
			    Z-2, max(16-1,Z+2), 1,
			    Z-2,
			    -1, %Skip
			    <<>>);
       true ->
	    map_description(Pos,
			    Width,Height,
			    7, 0, -1,
			    7,
			    -1, %Skip
			    <<>>)
    end.

map_description(Pos,Width,Height,StartZ,EndZ,Zstep,NZ,Skip,Acc)
  when NZ =/= EndZ+ Zstep->
    {Data,Skip2} = floor_description(Pos#coord{z=NZ}, Width,Height,Skip),
    map_description(Pos,
		    Width,Height,
		    StartZ,EndZ,Zstep,
		    NZ+Zstep,
		    Skip2,
		   <<Acc/binary,Data/binary>>);
map_description(_,_,_,_,_,_,_,Skip,Acc) ->
    if Skip >= 0 ->
	    <<Acc/binary,Skip,16#FF>>;
       true ->
	    Acc
    end.

floor_description(Pos, Width,Height,Skip) ->
    Pos2 = Pos#coord{x=Pos#coord.x,
		     y=Pos#coord.y},
    x(Pos2,0,Width,Height,Skip, <<>>).

x(Pos,NX,Width,Height,Skip,Acc) when NX < Width ->
    {Acc2,Skip2} = y(Pos, {NX,0},Height,Skip, <<>>),
    x(Pos, NX+1, Width, Height,Skip2, <<Acc/binary,Acc2/binary>>);
x(_,X,_,_,Skip,Acc) ->
    {Acc,Skip}.

y(Pos=#coord{x=X,y=Y},{NX,NY},Height,Skip,Acc) when NY < Height ->
    case ets:lookup(map, Pos#coord{x=X+NX,y=Y+NY}) of
	[Tile] ->
	    Desc = tile_description(Tile),
	    if Skip >= 0 ->
		    Data = <<Acc/binary,Skip,16#FF,Desc/binary>>,
		    y(Pos,{NX,NY+1},Height,0,Data);
	       true ->
		    Data = <<Acc/binary,Desc/binary>>,
		    y(Pos,{NX,NY+1},Height,0,Data) 
	    end;
	[] ->
	    Skip2 = Skip+1,
	    if Skip2 =:= 16#FF ->
		    y(Pos,{NX,NY+1},Height,-1,<<Acc/binary,16#FF,16#FF>>);
	       true ->
		    y(Pos,{NX,NY+1},Height,Skip2,Acc)
	    end
    end;
y(_Pos, {_,Y}, _Height,Skip,Acc) ->
    {Acc,Skip}.


tile_description(#tile{coord = Pos,
		       type = Ground,
		       items = Items}) ->
    try Cid = ets:lookup_element(item_types,Ground,#item_type.client_id),
	<<Cid:16/?UINT,(item_description(Items))/binary,
	 (creature_description(Pos))/binary>>
    catch _:badarg ->
	    throw({tile_description,[{noexist,Ground}]})
    end.
    
item_description(Items) ->
    item_description(Items, <<>>).

item_description([#map_item{id = Id}|Items],Acc) ->
    try Cid = ets:lookup_element(item_types,Id,#item_type.client_id),
	item_description(Items,<<Acc/binary,Cid:16/?UINT>>)
    catch _:badarg ->
	    throw({item_description,[{noexist,Id}]})
    end;
item_description([],Acc) ->
    Acc.


creature_description(Pos) ->
    try Creatures = ets:match_object(creatures, #creature{pos = Pos,
							  _='_'}),
	creature_description(Creatures, <<>>)
    catch _:badarg ->
	    throw({creature_description,[{noexist,Pos}]})
    end.

%%{creature,{coord,4,4,7},"Svett",{100,200},2,{outfit,128,44,44,44,44,0,0},{0,0},220,1,1}
creature_description([#creature{name = Name,
				id = Id,
				health = {Hp,Max},
				direction = Dir,
				outfit = Outfit,
				light = {LightLevel, LightColor},
				speed = Speed,
				skull = Skull,
				shield = Shield}|C], Acc) ->
    Creature =
	<<16#61:16/?UINT,
	 0:32/?UINT,
	 Id:32/?UINT,%id
	 (length(Name)):16/?UINT,
	 (list_to_binary(Name))/binary,
	 (round((Hp / Max)*100)),
	 Dir,
	 (outfit(Outfit))/binary,
	 LightLevel,LightColor,
	 Speed:16/?UINT,
	 Skull,Shield>>,
    creature_description(C, <<Acc/binary,Creature/binary>>);
creature_description([], Acc) ->
    Acc.


outfit(#outfit{type = Type,
	       head = Head,
	       body = Body,
	       legs = Legs,
	       feet = Feet,
	       addons = Addons}) ->
    if Type =/= 0 ->
	    <<Type:16/?UINT,Head,Body,Legs,Feet,Addons>>;
       true ->
	    <<Type:16/?UINT,23,12>>
    end.

-define(NORTH, 16#65).
-define(EAST,  16#66).
-define(SOUTH, 16#67).
-define(WEST,  16#68).

move_creature(C=#coord{x=X,y=Y,z=Z},Dir) ->
    io:format("16#~.16B ", [Dir]),
    case Dir of
	?NORTH -> NewX = X,   NewY = Y-1, NewZ = Z;
	?EAST  -> NewX = X+1, NewY = Y,   NewZ = Z;
	?SOUTH -> NewX = X,   NewY = Y+1, NewZ = Z;
	?WEST  -> NewX = X-1, NewY = Y,   NewZ = Z
    end,
	    
    B =
	if Y > NewY -> % Norhh
		<<16#65,(map_description(C#coord{x=X-8,y=Y-6},18,1))/binary>>;
	   Y < NewY -> % South
		<<16#67,(map_description(C#coord{x=X-8,y=Y+7},18,1))/binary>>;
	   X < NewX -> % East
		<<16#66,(map_description(C#coord{x=X+9,y=Y-6},1,14))/binary>>;
	   X > NewX -> % West
		<<16#68,(map_description(C#coord{x=X-8,y=Y-6},1,14))/binary>>
    end,
    {<<16#6D,
      X:16/?UINT,Y:16/?UINT,Z:8/?UINT,
      1,
      NewX:16/?UINT,NewY:16/?UINT,NewZ:8/?UINT,
      B/binary>>,C#coord{x=NewX,y=NewY}}.
