%%%-------------------------------------------------------------------
%%% File    : tibia_files.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  7 Jun 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_files).

-export([parse/1,gen_file/1]).

-include("tibia.hrl").

parse(File) ->
    {ok, Data} = file:read_file(File),
    try parse_data(Data)
    catch
	throw:Reason ->
	    {error, Reason}
    end.


parse_data(<<Version:32/?UINT,Rest/binary>>) ->
    if Version > 0 ->
	    throw(wrong_version);
       true ->
	    parse_nodes(Rest)
	    
    end.


parse_nodes(<<?NODE_START:8/?UINT,Type,Rest/binary>>) ->
    {<<>>, Node} = parse_nodes(Rest, #node{type=Type}, []),
    Node.

parse_nodes(<<>>, Node, Nodes) ->
    {<<>>, Node#node{children = lists:reverse(Nodes)}};
parse_nodes(<<?NODE_END,Rest/binary>>, Node, Nodes) ->
    {Rest, Node#node{children = lists:reverse(Nodes)}};
parse_nodes(<<?NODE_START,Type, Rest/binary>>, Node, Nodes) ->
    {Rest2, Node2} = parse_nodes(Rest, #node{type=Type}, []),
    parse_nodes(Rest2, Node, [Node2|Nodes]);
parse_nodes(<<?ESCAPE_CHAR,Byte:1/binary,Rest/binary>>, Node=#node{data=Data}, Nodes) ->
    parse_nodes(Rest, Node#node{data= <<Data/binary,Byte/binary>>},Nodes);
parse_nodes(<<Byte, Rest/binary>>, Node=#node{data=Data}, Nodes) ->
    parse_nodes(Rest, Node#node{data = <<Data/binary,Byte>>}, Nodes).


gen_file(#node{type=Type,data=Data,children=Children}) ->
    <<0:32/?UINT,
     ?NODE_START,
     Type,
     (gen_data(Data))/binary,
     (gen_file(Children,<<>>))/binary>>.

gen_file([], Index) ->
    Index;
gen_file([#node{type = Type,
		data = Data,
		children = Children}|Nodes], Index) ->
    gen_file(Nodes,
	     <<Index/binary,
	      ?NODE_START,
	      Type,
	      (gen_data(Data))/binary,
	      (gen_file(Children, <<>>))/binary,
	      ?NODE_END>>).


gen_data(Data) ->
    gen_data(Data, <<>>).

gen_data(<<?NODE_END,Rest/binary>>, Acc) ->
    gen_data(Rest, <<Acc/binary,?ESCAPE_CHAR,?NODE_END>>);
gen_data(<<?NODE_START,Rest/binary>>, Acc) ->
    gen_data(Rest, <<Acc/binary,?ESCAPE_CHAR,?NODE_START>>);
gen_data(<<?ESCAPE_CHAR,Rest/binary>>, Acc) ->
    gen_data(Rest, <<Acc/binary,?ESCAPE_CHAR,?ESCAPE_CHAR>>);
gen_data(<<>>, Acc) ->
    Acc;
gen_data(<<Byte,Rest/binary>>, Acc) ->
    gen_data(Rest, <<Acc/binary,Byte>>).




    
