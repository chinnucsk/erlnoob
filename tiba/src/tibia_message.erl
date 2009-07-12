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


%% 16#20000000 - Player
%% 16#30000000 - NPC
%% 16#40000000 - Monster
set_id() ->
    16#20000000.

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
    Pos2 = Pos#coord{x=Pos#coord.x-8,
		     y=Pos#coord.y-6},
    x(Pos2,0,Width,Height,Skip, <<>>).

x(Pos,NX,Width,Height,Skip,Acc) when NX < Width ->
    {Acc2,Skip2} = y(Pos, {NX,0},Height,Skip, <<>>),
    x(Pos, NX+1, Width, Height,Skip2, <<Acc/binary,Acc2/binary>>);
x(_,X,_,_,Skip,Acc) ->
    %%io:format("~p\n", [byte_size(Acc)]),
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


tile_description(#tile{type = Ground,
		       items = Items}) ->
    try Cid = ets:lookup_element(item_types,Ground,#item_type.client_id),
	<<Cid:16/?UINT,(item_description(Items))/binary,(creatures_description(a))/binary>>
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


creatures_description(Pos) ->
%%     Creatures = ets:lookup_element(map, Pos, #tile.creatures),

%%     creature_description(Creatures, <<>>),
    <<%% Creature start
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
     0 % Party shield (0-10)
     %% Creature end
     >>.


creature_description([#creature{name = Name,
				health = Hp,
				max_health = Max,
				direction = Dir,
				outfit = Outfit,
				light = {LightLevel, LightColor},
				speed = Speed,
				skull = Skull,
				shield = Shield}|C], Acc) ->
    Creature =
	<<16#61:16/?UINT,
	 0:32/?UINT,
	 233,3,0,16,%id
	 (byte_size(Name)):16/?UINT,
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
