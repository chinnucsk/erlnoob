%%%-------------------------------------------------------------------
%%% File    : heli_gl.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  7 Jul 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(heli_gl).

-export([handle_event/2,
	 handle_info/2, handle_call/3,
	 init/1, terminate/2, code_change/3]).

-export([start/0, start/1, start_link/0, start_link/1]).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-record(state, {canvas,
		image,
		circle,
		dir,
		heli}).


start() ->
    start([]).

start(Options) ->
    wx_object:start(?MODULE, Options, []).

start_link() ->
    start_link([]).

start_link(Options) ->
    wx_object:start_link(?MODULE, Options, []).

init(_Options) ->
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Heli", [{size, {800,600}}]),
    Panel = wxPanel:new(Frame, []),

    Menu = wxMenu:new(),

    wxMenu:append(Menu, ?wxID_ABOUT, "About"),
    wxMenu:append(Menu, ?wxID_EXIT, "Quit"),

    MB = wxMenuBar:new(),
    wxMenuBar:append(MB,Menu,"File"),
    wxFrame:setMenuBar(Frame, MB),
    %%wxFrame:connect(Frame, command_menu_selected),
    %%wxFrame:connect(Frame, close_window, [{skip,true}]),
    wxFrame:createStatusBar(Frame,[]),

    GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Panel, GLAttrib),

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    SizerFlags = wxSizerFlags:new(),
    wxSizerFlags:proportion(SizerFlags,1),
    wxSizerFlags:expand(SizerFlags),
    wxSizer:add(Sizer, Canvas, SizerFlags),
    wxWindow:setSizer(Panel,Sizer),

    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(Canvas),

    init_gl(Canvas),

    timer:send_interval(5, self(), move),
    wxGLCanvas:connect(Canvas, size),
    wxGLCanvas:connect(Canvas, left_up),
    wxGLCanvas:connect(Canvas, left_down),
    {Frame, #state{canvas = Canvas,
		   image = load_texture_by_image(wxImage:new("../images/test.png")),
		   circle = glu:newQuadric(),
		   heli = 0,
		   dir = down}}.

init_gl(Canvas) ->
    {W,H} = wxWindow:getClientSize(Canvas),
    gl:clearColor(1,1,1,1),
    gl:enable(?GL_TEXTURE_2D),
    %%gl:enable(?GL_COLOR_MATERIAL),
    gl:enable(?GL_BLEND),
    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA,?GL_ONE_MINUS_SRC_ALPHA),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),

    glu:ortho2D(0, W,H, 0),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    wxGLCanvas:swapBuffers(Canvas).

handle_event(#wx{event = #wxSize{size = {W,H}}},State) ->
    gl:loadIdentity(),
    gl:viewport(0,0,W,H),
    glu:perspective(45,1.0*W/H,1,100),
    draw(State),
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = left_up}},State) ->
    {noreply, State#state{dir = down}};
handle_event(#wx{event = #wxMouse{type = left_down}},State) ->
    {noreply, State#state{dir = up}}.

handle_call(_,_,State) ->
    {noreply, State}.

handle_info(move, State=#state{heli = Heli}) ->
    State2 =
	case State#state.dir of
	    down ->
		State#state{heli = Heli + 2};
	    up ->
		State#state{heli = Heli - 2}
	end,
    draw(State2),
    {noreply, State2}.


code_change(_,_,State) ->
    {noreply, State}.

terminate(_,_State) ->
    ok.


draw(#state{image = Tid, canvas = Canvas, circle = Circle, heli = Heli}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    draw_sprite(Tid, {100,Heli}),
    draw_map(Circle),
    wxGLCanvas:swapBuffers(Canvas).


draw_sprite(Tid, {X,Y}) ->
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),

    gl:pushMatrix(),
    gl:bindTexture(?GL_TEXTURE_2D, Tid),

    gl:translatef(X,Y,0),
    gl:'begin'(?GL_QUADS),

    gl:texCoord2f(0.0, 0.0), gl:vertex2i(00, 00),
    gl:texCoord2f(0.0, 1.0), gl:vertex2i(00, 64),
    gl:texCoord2f(1.0, 1.0), gl:vertex2i(64, 64),
    gl:texCoord2f(1.0, 0.0), gl:vertex2i(64, 00),

    gl:'end'(),
    gl:popMatrix().
    
draw_map(Circle) ->
    gl:color3f(0,0,0),
    gl:translatef(300,300, 0),
    gl:scalef(2.0,1, 1),

    glu:quadricDrawStyle(Circle,?GLU_LINE),
    glu:disk(Circle, 0, 100, 20, 1),
    ok.




load_texture_by_image(Image) ->
    W = wxImage:getWidth(Image),
    H = wxImage:getHeight(Image),
    Data = get_data(Image),   
    [TId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
    Format = case wxImage:hasAlpha(Image) of
		 true -> ?GL_RGBA;
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
	    interleave_rgb_and_alpha(RGB, Alpha);
	false ->
	    RGB
    end.

interleave_rgb_and_alpha(RGB, Alpha) ->
    list_to_binary(lists:zipwith(fun({R, G, B}, A) ->
					 <<R, G, B, A>>
				 end,
				 [{R,G,B} || <<R, G, B>> <= RGB],
				 [A || <<A>> <= Alpha])).
