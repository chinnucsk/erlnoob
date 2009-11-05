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
		options,
		timer,
		snake,
		apple}).

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
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Snake", [{size, {800,800}}]),

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
    put(quadric, glu:newQuadric()),

    {Frame, #state{frame = Frame,
		   canvas = Canvas,
		   timer = Timer,
		   snake = {[{1,1},{2,1}], []},
		   apple = {4,3}}}.

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


handle_event(#wx{event = #wxSize{size = Size}},State) ->
    resize(Size),
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

resize({W, H}) ->
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W, H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    ok.

draw(#state{canvas = Canvas, snake = Snake, apple = Apple}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    {W,H} = wxGLCanvas:getSize(Canvas),
    GridPos = draw_grid(W,H),
    draw_snake(Snake, GridPos),
    draw_apple(Apple, GridPos),
    wxGLCanvas:swapBuffers(Canvas).

-define(SQUARE_H, 20).
-define(SQUARE_W, 20).
-define(LINES_V, 20).
-define(LINES_H, 20).

draw_grid(W,H) ->
    GridW = ?SQUARE_W * ?LINES_V,
    GridH = ?SQUARE_H * ?LINES_H,
    GridPosX = (W-GridW) div 2,
    GridPosY =  (H-GridH) div 2,
    gl:pushMatrix(),
    gl:translatef(GridPosX, GridPosY, 0),
    gl:'begin'(?GL_LINES),
    gl:color3ub(0,0,0),
    FunV =
	fun(Pos) ->
		gl:vertex2i(Pos * (GridW div ?LINES_V),0),
		gl:vertex2i(Pos * (GridW div ?LINES_V),GridH)
	end,
    FunH =
	fun(Pos) ->
		gl:vertex2i(0,    Pos * (GridH div ?LINES_H)),
		gl:vertex2i(GridW,Pos * (GridH div ?LINES_H))
	end,
    wx:foreach(FunV, lists:seq(0, ?LINES_V)),
    wx:foreach(FunH, lists:seq(0, ?LINES_H)),
    gl:'end'(),
    gl:popMatrix(),
    {GridPosX,GridPosY}.

draw_snake({Head, Tail}, GridPos) ->
    draw_snake(Head, GridPos),
    draw_snake(Tail, GridPos);
draw_snake([{X,Y}|Rest], GridPos = {GridX, GridY}) ->
    gl:pushMatrix(),
    gl:color3ub(0,0,0),
    gl:translatef(GridX,GridY,0),
    gl:translatef((X-1)*?SQUARE_W,(Y-1)*?SQUARE_H,0),
    gl:'begin'(?GL_QUADS),
 
    gl:vertex2i(0, 0),
    gl:vertex2i(0, ?SQUARE_H),
    gl:vertex2i(?SQUARE_W, ?SQUARE_H),
    gl:vertex2i(?SQUARE_W, 0),
    
    gl:'end'(),
    gl:popMatrix(),
    draw_snake(Rest, GridPos);
draw_snake([], _) ->
    ok.

draw_apple({X,Y}, {GridX, GridY}) ->
    gl:pushMatrix(),
    gl:color3ub(230,50,50),
    gl:translatef(GridX,GridY,0),
    gl:translatef((X-1)*?SQUARE_W,(Y-1)*?SQUARE_H,0),
    gl:'begin'(?GL_QUADS),

    gl:vertex2i(0, 0),
    gl:vertex2i(0, ?SQUARE_H),
    gl:vertex2i(?SQUARE_W, ?SQUARE_H),
    gl:vertex2i(?SQUARE_W, 0),

    gl:'end'(),
    gl:popMatrix().







