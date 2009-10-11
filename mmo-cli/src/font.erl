%%%-------------------------------------------------------------------
%%% File    : font.erl
%%% Author  :  <Olle@ZUBAT>
%%% Description : 
%%%
%%% Created : 11 Oct 2009 by  <Olle@ZUBAT>
%%%-------------------------------------------------------------------
-module(font).

-compile(export_all).


load_font(FontFile) ->
    ets:new(font, [set,
		   named_table,
		   public,
		   {keypos, 1}]),
    Binarys = image:read_from_sprite_file(FontFile),
    {_,_,D,_} = hd(Binarys),
    io:format("~p\n", [D]),
    Images = [{Id, wxImage:new(W,H, Data, Alpha)} || {Id, {W,H}, Data, Alpha} <- Binarys],
    {_,Img} = hd(Images),
    io:format("~p\n", [wxImage:getData(Img)]),
    Font = [{Id, image:load_texture_by_image(Image)} || {Id, Image} <- Images],
    ets:insert(font, Font).


draw_text([Letter|Rest], X,Y) ->
    case ets:lookup(font, Letter) of
	[] ->
	    io:format("Lookup failed: ~p\n", [Letter]),
	    draw_text(Rest, X+8*5,Y);
	[{Letter, TId}] ->
	    image:draw_sprite(TId, X,Y, 5),
	    draw_text(Rest, X+8*5, Y)
    end;
draw_text([], _,_) ->
    ok.
