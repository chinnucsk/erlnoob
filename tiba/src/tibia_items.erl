%%%-------------------------------------------------------------------
%%% File    : tibia_items.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  3 Jun 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_items).

-compile(export_all).

-include("tibia.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(ROOT_ATTR_VERSION, 16#01).

-define(ITEM_GROUP_NONE, 0).
-define(ITEM_GROUP_GROUND, 1).
-define(ITEM_GROUP_CONTAINER, 2).
-define(ITEM_GROUP_WEAPON, 3).
-define(ITEM_GROUP_AMMUNITION, 4).
-define(ITEM_GROUP_ARMOR, 5).
-define(ITEM_GROUP_CHARGES, 6).
-define(ITEM_GROUP_TELEPORT, 7).
-define(ITEM_GROUP_MAGICFIELD, 8).
-define(ITEM_GROUP_WRITEABLE, 9).
-define(ITEM_GROUP_KEY, 10).
-define(ITEM_GROUP_SPLASH, 11).
-define(ITEM_GROUP_FLUID, 12).
-define(ITEM_GROUP_DOOR, 13).
-define(ITEM_GROUP_DEPRECATED, 14).
-define(ITEM_GROUP_LAST, 15).


-define(ITEM_ATTR_FIRST, 16#10).
-define(ITEM_ATTR_SERVERID,16#10).
-define(ITEM_ATTR_CLIENTID,16#11).
-define(ITEM_ATTR_NAME,16#12).
-define(ITEM_ATTR_DESCR,16#13).
-define(ITEM_ATTR_SPEED,16#14).
-define(ITEM_ATTR_SLOT,16#15).
-define(ITEM_ATTR_MAXITEMS,16#16).
-define(ITEM_ATTR_WEIGHT,16#17).
-define(ITEM_ATTR_WEAPON,16#18).
-define(ITEM_ATTR_AMU,16#19).
-define(ITEM_ATTR_ARMOR,16#1A).
-define(ITEM_ATTR_MAGLEVEL,16#1B).
-define(ITEM_ATTR_MAGFIELDTYPE,16#1C).
-define(ITEM_ATTR_WRITEABLE,16#1D).
-define(ITEM_ATTR_ROTATETO,16#1E).
-define(ITEM_ATTR_DECAY,16#1F).
-define(ITEM_ATTR_SPRITEHASH,16#20).
-define(ITEM_ATTR_MINIMAPCOLOR,16#21).
-define(ITEM_ATTR_07,16#22).
-define(ITEM_ATTR_08,16#23).
-define(ITEM_ATTR_LIGHT,16#24).

-define(ITEM_ATTR_DECAY2,16#25).
-define(ITEM_ATTR_WEAPON2,16#26).
-define(ITEM_ATTR_AMU2,16#27).
-define(ITEM_ATTR_ARMOR2,16#28).
-define(ITEM_ATTR_WRITEABLE2,16#29).
-define(ITEM_ATTR_LIGHT2,16#2A).

-define(ITEM_ATTR_TOPORDER,16#2B).

-define(ITEM_ATTR_WRITEABLE3,16#2C).

-define(ITEM_ATTR_LAST,16#2D).


test() ->
    load("items.otb","items.xml").

test_otb() ->
    test_otb(ground).

test_otb(Type) ->
    ok = load_otb("items.otb"),
    A = ets:match_object(item_types, #item_type{server_id = Type,
						_ = '_'}),
    ets:delete(item_types),
    A.

test_xml() ->
    test_xml(100).

test_xml(Id) ->
    load_xml("items.xml"),
    A = ets:match_object(items, #item{id = Id,
				      _ = '_'}),
    ets:delete(items),
    A.

load(OtbFile, XmlFile) ->
    load_otb(OtbFile),
    load_xml(XmlFile).

    
load_otb(File) ->
    try ets:new(item_types, [{keypos, #item_type.server_id},
			     ordered_set, protected,
			     named_table]),
	io:format("Loading items: ~p\n", [File]),
	Node = tibia_files:parse(File),
	parse_otb(Node)
    catch throw:Reason ->
	    throw({otb, {error,Reason}})
    end.

load_xml(File) ->
    try ets:new(items, [{keypos, #item.id},
			ordered_set, protected,
			named_table]),
	Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
		      {Acc, P, S};  % new return format
		 (X, Acc, S) ->
		      {[X|Acc], S}
	      end,
	io:format("Loading items: ~p\n", [File]),
	{R,[]} = xmerl_scan:file(File, [{space,normalize}, {acc_fun, Acc}]),
	parse_xml(R)
    catch throw:Reason ->
	    throw({xml, {error,Reason}})
    end.



parse_otb(#node{type = 0,
		  data = <<_:32/?UINT,
			  ?ROOT_ATTR_VERSION,
			  140:16/?UINT,
			  _MajorVersion:32/?UINT,
			  _MinorVersion:32/?UINT,
			  _BuildNumber:32/?UINT,
			  _CSDVersion:128/binary>>,
		  children = Children}) ->
    parse_otb(Children);
parse_otb([#node{type = Type,
		 data = <<Flags:32/?UINT,Rest/binary>>,
		 children = []} | Nodes]) ->
    try ItemType =
	get_otb_attributes(Rest, #item_type{flags = Flags,
					    type = get_type(Type)}),
	true=ets:insert(item_types,ItemType),
	parse_otb(Nodes)
    catch _:Reason ->
	    throw({ets_parse_otb,Reason})
    end;
parse_otb([]) ->
    ok.

get_type(?ITEM_GROUP_NONE)      -> none;
get_type(?ITEM_GROUP_GROUND)    -> ground;
get_type(?ITEM_GROUP_CONTAINER) -> container;
get_type(?ITEM_GROUP_WEAPON)    -> weapon;
get_type(?ITEM_GROUP_AMMUNITION)-> ammunition;
get_type(?ITEM_GROUP_ARMOR)     -> armor;
get_type(?ITEM_GROUP_CHARGES)   -> charges;
get_type(?ITEM_GROUP_TELEPORT)  -> teleport;
get_type(?ITEM_GROUP_MAGICFIELD)-> magicfield;
get_type(?ITEM_GROUP_WRITEABLE) -> writeable;
get_type(?ITEM_GROUP_KEY)       -> key;
get_type(?ITEM_GROUP_SPLASH)    -> splash;
get_type(?ITEM_GROUP_FLUID)     -> fluid;
get_type(?ITEM_GROUP_DOOR)      -> door;
get_type(?ITEM_GROUP_DEPRECATED)-> deprecated;
get_type(?ITEM_GROUP_LAST)      -> last.


get_otb_attributes(<<>>, Item) ->
    Item;
get_otb_attributes(<<?ITEM_ATTR_SERVERID,2:16/?UINT,ServerId:16/?UINT,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{server_id = ServerId});
get_otb_attributes(<<?ITEM_ATTR_CLIENTID,2:16/?UINT,ClientId:16/?UINT,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{client_id = ClientId});
get_otb_attributes(<<?ITEM_ATTR_NAME,8:16/?UINT,Name:8/binary,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{name = Name});
get_otb_attributes(<<?ITEM_ATTR_DESCR,Len:16/?UINT,_:Len/binary,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType);
get_otb_attributes(<<?ITEM_ATTR_SPEED,2:16/?UINT,Speed:16/?UINT,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{ground_speed = Speed});
get_otb_attributes(<<?ITEM_ATTR_SPRITEHASH,16:16/?UINT,Hash:16/binary,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{sprite_hash = Hash});
get_otb_attributes(<<?ITEM_ATTR_MINIMAPCOLOR,2:16/?UINT,Color:16/?UINT,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{minimap_color = Color});
get_otb_attributes(<<?ITEM_ATTR_07,2:16/?UINT,Attr07:16/?UINT,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{sub_param_7 = Attr07});
get_otb_attributes(<<?ITEM_ATTR_08,2:16/?UINT,Attr08:16/?UINT,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{sub_param_8 = Attr08});
get_otb_attributes(<<?ITEM_ATTR_LIGHT,4:16/?UINT,
		LightLevel:16/?UINT,
		LightColor:16/?UINT,
		Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{light_color = LightColor,
					   light_level = LightLevel});
get_otb_attributes(<<?ITEM_ATTR_LIGHT2,4:16/?UINT,
		LightLevel:16/?UINT,
		LightColor:16/?UINT,
		Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{light_color = LightColor,
					   light_level = LightLevel});
get_otb_attributes(<<?ITEM_ATTR_TOPORDER,1:16/?UINT,TopOrder,Rest/binary>>, ItemType) ->
    get_otb_attributes(Rest,ItemType#item_type{always_on_top_order = TopOrder});
get_otb_attributes(<<Type,Len:16/?UINT,_/binary>>, _) ->
    throw({invalid_otb_format, [{type, Type},
				{size, Len}]}).




%%   XML Attributes

%% * NAME (text) - The name of the item.
%% * ARTICLE (text) - The article of the item (the name with 'a' or 'an' infront).
%% * PLURAL (text) - The plural form of the name. (arrow - arrows)
%% * DESCRIPTION (text) - The special description of the item, will be
%%   displayed after the normal text on the item. For example the text on a fire sword.
%% * RUNESPELLNAME (text) - The name of the spell this rune is used to cast.
%% * WEIGHT (number) - The weight of the item, specified in hundreds of one oz. So 100 is 1.0 oz.
%% * SHOWCOUNT (number) - The number of items left in the stack will be displayed when looking at the item.
%% * ROTATETO (number) - The ID of the item this will be transformed into when you rotate it. Such as a chair.
%% * CONTAINERSIZE (number) - Only valid for containers, specifies the volume of the container.
%% * FLUIDSOURCE (text) - This item will act as a source for the fluid specified by the value.
%% * WRITEABLE (boolean) - True if you can write in this item (for books, for example).
%% * MAXTEXTLEN (number) - The maximum length of the text that can be typed into this item.
%% * WRITEONCEITEMID (number) - If the item should only be writeable once, and from then
%%   onward only readable. Set this to the ID of an identical item that is not set as writeable.
%% * CORPSETYPE (text) - The type of corpse, this affects what color of blood is created
%%   with the corpse when a creature dies. Can be one of the following:
%%       o venom - Slime blood.
%%       o undead - No blood.
%%       o blood - Red blood.
%%       o fire - No blood. 
%% * EFFECT (text) - The magic effect spawned when an item is thrown on a trash container.
%% * PREVENTSKILLLOSS - If the player is wearing an item with this attribute when he
%%   dies, he will not lose any skills or experience points. A charge will be
%%   removed from the item (if it's not charged, the item will be destroyed).
%% * PREVENTITEMLOSS - If the player is wearing an item with this attribute
%%   when he dies, he will not lose any items. A charge will be removed
%%   from the item (if it's not charged, the item will be destroyed).
%%   When used in combination with preventskillloss, only one charge will
%%   be removed (ie. both effects will apply). %% Item key attributes

%% * KEY - The item is a key, when looked at, it will display the action ID of the item as key ID.
%% * MAGICFIELD - The item is a magic field, this must be specified for items
%%   used as fields in spells, since only items with this attribute set are deleted
%%   when a new field is cast on a tile. Pathfinding avoids magic fields.
%% * DEPOT - Works as a depot, simply.
%% * MAILBOX - The item will works as a mailbox
%%   (parcels/letters dropped on it will be sent to their respective player).
%% * TRASHHOLDER - A trashholder will remove any item dropped on them from the game.
%% * TELEPORT - A teleport teleports things. Anything dropped on the tile with
%%   the teleport will be transferred to it's target destination.
%% * DOOR - This must be specified for house doors, else it will not display
%%   the "It belongs to house 'ownername'" text, nor will it be saved.
%% * BED - Should be set for both bottom and top part of a bed.
%%   When set, players can use the bed to sleep in (if it's placed inside a house). 

%% * ARMOR (number) - The armor of the item, should only be specified
%%   for items that can be worn as armor, obviously. Will have no effect on weapons/shields.
%% * DEFENSE (number) - The defense of a weapon or shield.
%% * EXTRADEF (number) - The extra defense of a weapon or shield. For example the '+3' on a the magic sword.
%% * ATTACK (number) - The attack of the weapon,
%% * ELEMENTX (number) - The extra, elemental damage added to the weapon,
%%   replace the 'X' with that type (Fire Ice Energy Earth)
%% * WEAPONTYPE (text) - The type of weapon this is, this affect how it
%%   can be used, and what skills it uses/gives advances in.
%%   The following values can be specified: club, axe, shield, distance, wand, ammunition.
%% * SLOTTYPE (text) - The slot that this item fits into, leave blank
%%   for it to act as a normal, non-equipable item.
%%   The valid values are: head, body, legs, feet, backpack, two-handed, necklace, ring.
%% * AMMOTYPE (text) - The type of ammunition, for weapons this is the type
%%   of ammunition required to fire. For ammunition item this is the time of ammunition.
%%   Possible values are: bolt, arrow, spear, throwingstar, throwingknife, stone and snowball.
%%   Note that all the other types of arrows/bolts/spears can be used as synonyms (infernalbolt, sniperarrow etc.)
%% * AMMOACTION (text) - The type of action that will be done when a distance weapon is used.
%%   Can be either move, which means the ammunition will be put on the target tile (like the old spears).
%%   moveback which means the item will return to to attackers hand after it's been used.
%%   removecharge, which means one charge will be removed when attacking.
%%   And finally removecount which means one item will be removed from the stack when the weapon is used.
%% * SHOOTTYPE (text) - The type of the shoot animation used for this item (only valid for distance weapons).
%% * RANGE (number) - The range of a distance weapon, in tiles.
%% * HITCHANCE (number) - A number between 0 and 100 that is the chance
%%   (in percent) for a distance weapon to hit on attack, not that this
%%   is the lower end of this value. Higher distance skill means you're
%%   more likely to hit even if this value is very low.
%% * MAXHITCHANCE (number) - The maximum hitchance between 0 and 100.
%%   No matter how high distance skill you have the chance to hit will never be greater than this. 

%% * MOVEABLE (boolean) - If you can move the item. note: this has a
%%   default value from items.otb for most items, which means you do not
%%   need to specify it manually. It may be edited with the help of an items.otb editor.
%% * BLOCKPROJECTILE (boolean) - If the item should block projectile attacks. Common for walls.
%% * ALLOWPICKUPABLE (boolean) - If you can drop pickupable items on the tile. (True for sea tiles, for example)
%% * FLOORCHANGE (text) - Text can be one of the following:
%%       o down - The item will act as a hole in the ground, dropping you down.
%%       o south - The item will act as a stairs down south.
%%         That is, the player will be moved to the south of the hole they walked on to.
%%       o north - Same as above, but north.
%%       o west - Same as above, but west.
%%       o east - Same as above, but east. 


parse_xml(#xmlElement{name = items, content = Content}) ->
    parse_xml(Content);
parse_xml([#xmlElement{name = item, attributes = Attributes,
		       content = Content}|Elements]) ->
    try Attrs = get_xml_attributes(Attributes),
	Item =
	#item{id = proplists:get_value(id,Attrs),
	      name = proplists:get_value(name,Attrs),
	      article = proplists:get_value(article,Attrs),
	      plural = proplists:get_value(plural,Attrs),
	      attributes = lists:append(parse_xml(Content, []))},
	true = ets:insert(items, Item)
    catch _:Reason ->
	    throw({ets_parse_xml,Reason})
    end,
    parse_xml(Elements);
parse_xml([]) ->
    ok.

parse_xml([#xmlElement{name = attribute, attributes = Attributes,
		       content = []}|Elements], Acc) ->
    parse_xml(Elements, [get_xml_attributes(Attributes)|Acc]);
parse_xml([#xmlElement{name = attribute, attributes = Attributes,
		       content = Attributes2}|Elements], Acc) ->
    parse_xml(Elements, [lists:append([get_xml_attributes(Attributes)|
				       parse_xml(Attributes2, [])])|Acc]);
parse_xml([], Acc) ->
    Acc;
parse_xml([#xmlElement{name = Tag,
		       parents = Parents,
		       pos = Pos}|_],_) ->
    throw({invalid_xml_tag,[{tag_name,Tag},
			    {pos,Pos},
			    {parents,Parents}]}).


get_xml_attributes(Xml) ->
    get_xml_attributes(Xml, []).

get_xml_attributes([#xmlAttribute{name = key, value = KeyString},
		    #xmlAttribute{name = value, value = Value}|Attrs], Acc) ->
    Key = list_to_atom(KeyString),
    try 
	get_xml_attributes(Attrs, [{Key, list_to_integer(Value)}|Acc])
    catch _:_ ->
	    get_xml_attributes(Attrs, [{Key, Value}|Acc])
    end;
get_xml_attributes([#xmlAttribute{name = id, value = IdString}|Attrs], Acc) ->
    Id = list_to_integer(IdString),
    Id2 = if Id > 20000, Id < 20100 ->
		  Id-20000;
	     true ->
		  Id
	  end,
    get_xml_attributes(Attrs, [{id, Id2}|Acc]);
get_xml_attributes([#xmlAttribute{name = Key, value = Value}|Attrs], Acc)
  when Key =:= name;
       Key =:= article;
       Key =:= plural ->
    get_xml_attributes(Attrs, [{Key, Value}|Acc]);
get_xml_attributes([#xmlAttribute{name = Key,
				  value = Value,
				  pos = Pos,
				  parents = Parents}|_],_) ->
    throw({invalid_xml_attribute, [{attribute, Key},
				   {value, Value},
				   {pos, Pos},
				   {parents, Parents}]});
get_xml_attributes([], Acc) ->
    Acc.
