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

-record(state, {canvas,
		image}).


start() ->
    start([]).

start(Options) ->
    wx_object:start(?MODULE, Options, []).

start_link() ->
    start_link([]).

start_link(Options) ->
    wx_object:start_link(?MODULE, Options, []).

init(_Options) ->
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "wxImage Test", [{size, {800,600}}]),
    Panel = wxPanel:new(Frame, []),

    Menu = wxMenu:new(),

    wxMenu:append(Menu, ?wxID_ABOUT, "About"),
    wxMenu:append(Menu, ?wxID_EXIT, "Quit"),

    MB = wxMenuBar:new(),
    wxMenuBar:append(MB,Menu,"File"),
    wxFrame:setMenuBar(Frame, MB),
    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, close_window, [{skip,true}]),
    wxFrame:createStatusBar(Frame,[]),

    GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Panel, GLAttrib),

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    SizerFlags = wxSizerFlags:new(),
    wxSizerFlags:proportion(SizerFlags,1),
    wxSizerFlags:expand(SizerFlags),
    wxSizer:add(Sizer, Canvas, SizerFlags),
    wxWindow:setSizer(Panel,Sizer),
    %%wxSizer:setSizeHints(Sizer,Frame),
    %% Show
    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(Canvas),

    init_gl(Canvas),

    erlang:send_after(100, self(), update),
    {Frame, #state{canvas = Canvas,
		   image = load_texture_by_image(wxImage:new("test.png"))}}.

init_gl(Canvas) ->
    {W,H} = wxWindow:getClientSize(Canvas),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W,H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:clearColor(1.0,1.0,1.0,1.0),
    gl:enable(?GL_TEXTURE_2D),
    wxGLCanvas:swapBuffers(Canvas).

handle_event(_,State) ->
    {noreply, State}.

handle_call(_,_,State) ->
    {noreply, State}.

handle_info(update, State) ->
    draw(State#state.image, State#state.canvas),
    erlang:send_after(100, self(), update),
    {noreply, State}.

code_change(_,_,State) ->
    {noreply, State}.

terminate(_,_State) ->
    ok.


draw(Tid, Canvas) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:enable(?GL_TEXTURE_2D),

    draw_sprite(Tid, 100),

    wxGLCanvas:swapBuffers(Canvas).


draw_sprite(Tid, Y) ->
    gl:translatef(100,Y,0),
    gl:bindTexture(?GL_TEXTURE_2D, Tid),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),

    enter_2d_mode(),

    gl:'begin'(?GL_QUADS),

    gl:texCoord2f(00, 00), gl:vertex2i(00, 00),
    gl:texCoord2f(00, 64), gl:vertex2i(00, 64),
    gl:texCoord2f(64, 64), gl:vertex2i(64, 64),
    gl:texCoord2f(64, 00), gl:vertex2i(64, 00),

    gl:'end'(),

    leave_2d_mode().

load_texture_by_image(Image) ->
    W = wxImage:getWidth(Image),
    H = wxImage:getHeight(Image),
    Data = wxImage:getData(Image),
    %% Create an OpenGL texture for the image
    [TId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
    gl:texImage2D(?GL_TEXTURE_2D, 0,
		  ?GL_RGB, W, H, 0,
		  ?GL_RGB, ?GL_UNSIGNED_BYTE, Data),
    TId.


enter_2d_mode() ->
    gl:pushAttrib(?GL_ENABLE_BIT),
    gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_CULL_FACE),
    gl:enable(?GL_TEXTURE_2D),
 
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
       
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
 
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity().
 
leave_2d_mode() ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:popAttrib().


