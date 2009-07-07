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

-export([start/0, start/1]).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-record(state, {canvas}).

start() ->
    start([]).

start(Options) ->
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

    self() ! update,
    {Frame, #state{canvas = Canvas}}.

init_gl(Canvas) ->
    {W,H} = wxWindow:getClientSize(Canvas),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho( -2.0, 2.0, -2.0*H/W, 2.0*H/W, -20.0, 20.0),
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
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wxGLCanvas:swapBuffers(State#state.canvas),
    timer:sleep(100),
    self() ! update,
    {noreply, State}.

code_change(_,_,State) ->
    {noreply, State}.

terminate(_,_State) ->
    ok.
