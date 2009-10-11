%%%-------------------------------------------------------------------
%%% File    : image.erl
%%% Author  :  <Olle@ZUBAT>
%%% Description : 
%%%
%%% Created : 11 Oct 2009 by  <Olle@ZUBAT>
%%%-------------------------------------------------------------------
-module(image).

-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/wx.hrl").

-export([create_sprite_file/2,
	 load_texture_by_string/1,
	 read_from_sprite_file/1,
	 load_texture_by_image/1,
	 draw_sprite/3,draw_sprite/4]).

load_texture_by_image(Image) ->
    W = wxImage:getWidth(Image),
    H = wxImage:getHeight(Image),
    Data = get_data(Image),
    [TId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
    Format = case wxImage:hasAlpha(Image) of
		 true -> ?GL_RGB;
		 false -> ?GL_RGB
	     end,
    gl:texImage2D(?GL_TEXTURE_2D, 0,
		  Format, W, H, 0,
		  Format, ?GL_UNSIGNED_BYTE, Data),
    TId.

get_data(Image) ->
    RGB = wxImage:getData(Image),
    case wxImage:hasAlpha(Image) of
	true ->
	    Alpha = wxImage:getAlpha(Image),
	    interleave_rgb_and_alpha(RGB, Alpha),
	    RGB;
	false ->
	    RGB
    end.

interleave_rgb_and_alpha(RGB, Alpha) ->
    list_to_binary(lists:zipwith(fun({R, G, B}, A) ->
					 <<R, G, B, A>>
				 end,
				 [{R,G,B} || <<R, G, B>> <= RGB],
				 [A || <<A>> <= Alpha])).


draw_sprite(Tid, X, Y) ->
    draw_sprite(Tid, X, Y, 1).

draw_sprite(Tid, X, Y, Scale) ->
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),

    gl:pushMatrix(),
    gl:bindTexture(?GL_TEXTURE_2D, Tid),

    gl:translatef(X,Y,0),
    gl:scalef(Scale, Scale, 1),
    gl:'begin'(?GL_QUADS),

    gl:texCoord2f(0.0, 0.0), gl:vertex2i(0, 0),
    gl:texCoord2f(0.0, 1.0), gl:vertex2i(0, 8),
    gl:texCoord2f(1.0, 1.0), gl:vertex2i(8, 8),
    gl:texCoord2f(1.0, 0.0), gl:vertex2i(8, 0),

    gl:'end'(),
    gl:popMatrix().


load_texture_by_string(String) ->
    Font = wxFont:new(),
    Brush = wxBrush:new(?wxBLACK),
    TmpBmp = wxBitmap:new(200, 200),
    Tmp = wxMemoryDC:new(TmpBmp),
    wxMemoryDC:setFont(Tmp, Font),        
    {StrW, StrH} = wxDC:getTextExtent(Tmp, String),
    wxMemoryDC:destroy(Tmp),
    wxBitmap:destroy(TmpBmp),
    
    W = get_power_of_two_roof(StrW),
    H = get_power_of_two_roof(StrH),

    Bmp = wxBitmap:new(W, H),
    DC = wxMemoryDC:new(Bmp),
    wxMemoryDC:setFont(DC, Font),        
    wxMemoryDC:setBackground(DC, Brush),
    wxMemoryDC:clear(DC),
    wxMemoryDC:setTextForeground(DC, {255, 255, 255}),
    wxMemoryDC:drawText(DC, String, {0, 0}),

    Img = wxBitmap:convertToImage(Bmp),
    
    Alpha = wxImage:getData(Img),
    Data = colourize_image(Alpha, {255,255,255}),
    wxImage:destroy(Img),
    wxBitmap:destroy(Bmp),
    wxMemoryDC:destroy(DC),
    wxFont:destroy(Font),
    wxBrush:destroy(Brush),

    [TId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    %%gl:pixelStorei(?GL_UNPACK_ROW_LENGTH, 0),
    %%gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 2),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA,
  		  W, H, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Data),
    TId.

colourize_image(Alpha, {R,G,B}) ->
    << <<R:8,G:8,B:8,A:8>> || <<A:8,_:8,_:8>> <= Alpha >>.

get_power_of_two_roof(X) ->
    get_power_of_two_roof_2(1, X).

get_power_of_two_roof_2(N, X) when N >= X -> N;
get_power_of_two_roof_2(N, X) -> get_power_of_two_roof_2(N*2, X).


read_from_sprite_file(SpriteFile) ->
    {ok, Data} = file:read_file(SpriteFile),
    read_bin(Data).

read_bin(<<NumSprites:16/unsigned-integer, Data/binary>>) ->
    read_bin(Data, NumSprites, []).

read_bin(<<Id:16/unsigned-integer,
	  W:16/unsigned-integer,
	  H:16/unsigned-integer,
	  DataSize:16/unsigned-integer,
	  Data:DataSize/binary,
	  AlphaSize:16/unsigned-integer,
	  Alpha:AlphaSize/binary,
	  Rest/binary>>,
	 Sprites, Acc) when Sprites > 1 ->
    read_bin(Rest, Sprites-1, [{Id, {W,H}, Data, Alpha}|Acc]);
read_bin(<<>>, 1, Acc) ->
    lists:keysort(1, Acc);
read_bin(_, _, _) ->
    io:format("corrupt file, try export it again\n", []).


create_sprite_file(Dir, Filename) ->
    Files0 = filelib:wildcard("*.png", Dir),
    Files = [filename:join([Dir, File]) || File <- Files0],
    Bin = create_bin(Files, Dir),
    file:write_file(Filename, Bin).

create_bin(Files, Dir) ->
    create_bin(Files, Dir, 1, <<>>).

create_bin([File | Files], Dir, Num, Acc) ->
    TmpImage = wxImage:new(File),
    Bmp = wxBitmap:new(TmpImage),
    wxImage:destroy(TmpImage),
    Image = wxBitmap:convertToImage(Bmp),
    wxBitmap:destroy(Bmp),
    case wxImage:hasAlpha(Image) of
	true ->
	    Alpha = wxImage:getAlpha(Image);
	false ->
	    wxImage:initAlpha(Image),
	    Alpha = wxImage:getAlpha(Image)
    end,
    Data = wxImage:getData(Image),
    io:format("~p\n", [Data]),
    case filename:rootname(filename:basename(File)) of
	"qmark" ->
	    Id = $?;
	[Other] ->
	    Id = Other
    end,
    Bin = <<Id:16/unsigned-integer,
	   (wxImage:getWidth(Image)):16/unsigned-integer,
	   (wxImage:getHeight(Image)):16/unsigned-integer,
	   (byte_size(Data)):16/unsigned-integer,
	   Data/binary,
	   (byte_size(Alpha)):16/unsigned-integer,
	   Alpha/binary>>,
    create_bin(Files, Dir, Num+1, <<Acc/binary, Bin/binary>>);
create_bin([], _Dir, Num, Acc) ->
    <<Num:16/unsigned-integer, Acc/binary>>.
