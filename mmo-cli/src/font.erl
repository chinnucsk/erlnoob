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
    Letters = image:read_from_sprite_file(FontFile),
    Images = [{Id, wxBitmap:convertToImage(wxBitmap:new(Data, 8,8))} || {Id, Data} <- Letters],
    Font = [{Id, image:load_texture_by_image(Image)} || {Id, Image} <- Images],
    ets:insert(font, Font).


draw_text([Letter|Rest], X,Y) ->
    case ets:lookup(font, Letter) of
	[] ->
	    io:format("Lookup failed\n", []),
	    draw_text(Rest, X+8,Y);
	[{Letter, TId}] ->
	    image:draw_sprite(TId, X,Y),
	    draw_text(Rest, X+8, Y)
    end;
draw_text([], _,_) ->
    ok.
