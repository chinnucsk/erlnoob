%%%-------------------------------------------------------------------
%%% File    : tibia_monsters.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 13 Jul 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_monsters).

-include("tibia.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-compile(export_all).

test() ->
    load_monsters("monsters.xml").

-define(ACCFUN, fun(#xmlText{value = " ", pos = P}, A, S) ->
			{A, P, S};  % new return format
		   (X, A, S) ->
			{[X|A], S}
		end).

load_monsters(File) ->
    io:format("Loading monsters: ~p\n", [File]),
    {R,[]} = xmerl_scan:file(File, [{space,normalize}, {acc_fun, ?ACCFUN}]),
    Monsters = parse_monsters_xml(R),
    parse_monster(Monsters).

parse_monster(Monsters) ->
    parse_monster(Monsters, []).

parse_monster([File|Files], Acc) ->
    {R,[]} = xmerl_scan:file("../monster/"++File,
			     [{space,normalize},
			      {acc_fun, ?ACCFUN}]),
    M = monster(R).%,
%%     parse_monster(Files, [M|Acc]);
%% parse_monster([],Acc) ->
%%     Acc,done.


monster(#xmlElement{name = monster,
		    attributes = Attributes,
		    content = Content}) ->
    Attrs = parse_attributes(Attributes),
    M = #monster{name = proplists:get_value(name,Attrs),
		 name_description = proplists:get_value(nameDescription,Attrs),
		 race = proplists:get_value(race,Attrs),
		 experience = proplists:get_value(experience,Attrs),
		 speed = proplists:get_value(speed,Attrs),
		 manacost = proplists:get_value(manacost,Attrs)},
    parse_content(Content, M).

parse_content([#xmlElement{name = health,
			   attributes = Attrs}|Rest], M) ->
    Health = parse_attributes(Attrs),
    parse_content(Rest,M#monster{health = {proplists:get_value(now,Health),
					   proplists:get_value(max,Health)}});
parse_content([#xmlElement{name = look,
			   attributes = Attrs}|Rest],M) ->
    Look = parse_attributes(Attrs),
    Outfit = #outfit{type = proplists:get_value(type,Look),
		     corpse = proplists:get_value(corpse,Look),
		     _ = 0},

    parse_content(Rest,M#monster{outfit = Outfit});
parse_content([#xmlElement{name = flags,
			   attributes = [],
			   content = Content}|Rest],M) ->
    Flags = parse_content(Content, []),
    parse_content(Rest,M#monster{flags = lists:append(Flags)});
parse_content([#xmlElement{name = flag,
			   attributes = Attrs,
			   content = []}|Rest],Acc) ->
    A = parse_attributes(Attrs),
    parse_content(Rest,[A|Acc]);
parse_content([#xmlElement{name = voices,
			   attributes = Attrs,
			   content = Content}|Rest],M) ->
    A = parse_attributes(Attrs),
    Voices = lists:append(parse_content(Content, [])),
    parse_content(Rest,M#monster{voices = {A,Voices}});
parse_content([#xmlElement{name = voice,
			   attributes = Attrs,
			   content = []}|Rest],Acc) ->
    A = parse_attributes(Attrs),
    parse_content(Rest,[A|Acc]);
parse_content([#xmlElement{name = loot,
			   attributes = [],
			   content = Content}|Rest],M) ->
    Loot = parse_content(Content, []),
    parse_content(Rest,M#monster{loot = Loot});
parse_content([#xmlElement{name = item,
			   attributes = Attrs,
			   content = Content}|Rest],Acc) ->
    A = parse_attributes(Attrs),
    Id = proplists:get_value(id,A),
    Inside = parse_content(Content, []),
    parse_content(Rest,[#map_item{id = Id,
				 attributes =lists:delete({id,Id},A),
				 content = Inside}|Acc]);
parse_content([#xmlElement{name = inside,
			   content = Content}|Rest],[]) ->
    Items = parse_content(Content, []),
    parse_content(Rest,Items);


parse_content([_|Rest],M) ->
    parse_content(Rest,M);
parse_content([],Acc) when is_list(Acc) ->
    lists:reverse(Acc);
parse_content([],M) ->
    M.




parse_monsters_xml(#xmlElement{name = monsters, content = Content}) ->
    parse_monsters_xml(Content, []).


parse_monsters_xml([#xmlElement{name = monster, attributes = XmlAttributes,
		       content = []}|Elements], Acc) ->
    Monster = parse_attributes(XmlAttributes),
    parse_monsters_xml(Elements, [proplists:get_value(file,Monster)|Acc]);
parse_monsters_xml([], Acc) ->
    Acc.



parse_attributes(Attributes) ->
    parse_attributes(Attributes,[]).

parse_attributes([#xmlAttribute{name = Name,
				value = Value}|Attrs], Acc) ->
    try 
	parse_attributes(Attrs, [{Name, list_to_integer(Value)}|Acc])
    catch _:_ ->
	    parse_attributes(Attrs, [{Name, Value}|Acc])
    end;
parse_attributes([],Acc) ->
    lists:reverse(Acc).
