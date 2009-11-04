%%%-------------------------------------------------------------------
%%% File    : snake_gl.erl
%%% Author  : Olle Mattsson <olle@mudkipz>
%%% Description : 
%%%
%%% Created : 31 Dec 2008 by Olle Mattsson <olle@mudkipz>
%%%-------------------------------------------------------------------
-module(snake_gl).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-record(state, {frame,
		canvas,
		timer}).

start() ->
    start([]).

start(Options) ->
    wx:new(),
    wx_object:start(?MODULE, Options, []).

start_link() ->
    start_link([]).

start_link(Options) ->
    wx:new(),
    wx_object:start_link(?MODULE, Options, []).

init(_Options) ->
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Heli", [{size, {800,800}},
						      {style, ?wxDEFAULT_FRAME_STYLE band bnot ?wxRESIZE_BORDER}]),

    Menu = wxMenu:new(),

    wxMenu:append(Menu, ?wxID_ABOUT, "About"),
    wxMenu:append(Menu, ?wxID_EXIT, "Quit"),

    MB = wxMenuBar:new(),
    wxMenuBar:append(MB,Menu,"File"),
    wxFrame:setMenuBar(Frame, MB),
    %%wxFrame:connect(Frame, command_menu_selected),
    %%wxFrame:connect(Frame, close_window, [{skip,true}]),

    GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Frame, GLAttrib),

    {ok, Timer} = timer:send_interval(200, refresh),
    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(Canvas),

    init_gl(Canvas),

    {Frame, #state{frame = Frame,
		   canvas = Canvas,
		   timer = Timer}}.

init_gl(Canvas) ->
    {W,H} = wxWindow:getClientSize(Canvas),
    gl:clearColor(1,1,1,1),
    gl:enable(?GL_TEXTURE_2D),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:enable(?GL_BLEND),
    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA,?GL_ONE_MINUS_SRC_ALPHA),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),

    glu:ortho2D(0, W,H, 0),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    wxGLCanvas:connect(Canvas, size),

    wxGLCanvas:swapBuffers(Canvas).

handle_event(#wx{event = #wxSize{}},State) ->
    wxFrame:setSize(State#state.frame, {800,800}),
    draw(State),
    {noreply, State}.

handle_call(_,_,State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    draw(State),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_,_,State) ->
    {noreply, State}.

terminate(_,_State) ->
    ok.


draw(#state{canvas = Canvas}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    {W,H} = wxGLCanvas:getSize(Canvas),
    draw_grid(W,H),
    wxGLCanvas:swapBuffers(Canvas).


draw_grid(W,H) ->
    gl:'begin'(?GL_LINES),
    gl:color3ub(0,0,0),
    LinesV = 20,
    LinesH = 20,
    FunV =
	fun(Pos) ->
		gl:vertex2i(Pos * (W div LinesV),0),
		gl:vertex2i(Pos * (W div LinesV),H)
	end,
    FunH =
	fun(Pos) ->
		gl:vertex2i(0,Pos * (H div LinesH)),
		gl:vertex2i(W,Pos * (H div LinesH))
	end,
    %%io:format("~p\n", [{W,H}]),
    wx:foreach(FunV, lists:seq(0, LinesV)),
    wx:foreach(FunH, lists:seq(0, LinesH)),
    gl:'end'(),
    Quad = glu:newQuadric(),
    gl:translatef(99,95,0),
    glu:disk(Quad, 0, 20, 10,10),

    glu:deleteQuadric(Quad).
    






