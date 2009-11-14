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
		mem,
		grow,
		direction,
		apple}).

-define(DEFAULT_SQUARE_W, 20).
-define(DEFAULT_SQUARE_H, 20).
-define(DEFAULT_LINES, {20,20}).
-define(DEFAULT_GRID_SIZE, {300,300}).


start() ->
    Options = [],
    start(Options).

start(Options) ->
    wx:new(),
    wx_object:start(?MODULE, Options, []).

start_link() ->
    start_link([]).

start_link(Options) ->
    wx:new(),
    wx_object:start_link(?MODULE, Options, []).

init(Options) ->
    Options = parse_options(Options),
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
    wxGLCanvas:connect(Canvas, key_down, []),

    {ok, Timer} = timer:send_interval(200, refresh),
    {ok, Timer2} = timer:send_interval(50, move),
    {ok, Timer3} = timer:send_interval(500, grow),
    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(Canvas),

    init_gl(Canvas),

    {Frame, #state{frame = Frame,
		   canvas = Canvas,
		   timer = [Timer, Timer2, Timer3],
		   options = Options,
		   mem = wx:create_memory(3),
		   snake = {[{100,100},{100,110},{100,120},{100,130}], []},
		   grow = false,
		   direction = up,
		   apple = {44,39}}}.

parse_options(Options) ->
    parse_options(Options, []).

parse_options([{grid, Size={W,H}}|Options], Acc) when is_integer(W),
						      is_integer(H) ->
    parse_options(Options, [{grid_size, Size}|Acc]);
parse_options([Lines={lines, {V,H}}|Options], Acc) when is_integer(V),
							is_integer(H) ->
    parse_options(Options, [Lines|Acc]);
parse_options([_|Options], Acc) ->
    parse_options(Options, Acc);
parse_options([], Acc) ->
    Acc.

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
    {noreply, State};
handle_event(#wx{event = #wxKey{type = key_down, keyCode = KeyCode}}, State) ->
    Direction = 
	case KeyCode of
	    ?WXK_LEFT  -> left;
	    ?WXK_RIGHT -> right;
	    ?WXK_UP    -> up;
	    ?WXK_DOWN  -> down;
	    _          -> State#state.direction
	end,
    {noreply, State#state{direction = Direction}};
handle_event(Wx = #wx{},State) ->
    io:format("~p\n", [Wx]),
    {noreply, State}.

handle_call(_,_,State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    draw(State),
    {noreply, State};
handle_info(move, State) ->
    State2 = check_snake(State),
    State3 = move_snake(State2),
    draw(State3),
    {noreply, State3};
handle_info(grow, State) ->
    {noreply, State#state{grow = true}};
handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_,_,State) ->
    {noreply, State}.

terminate(_,State) ->
    lists:foreach(fun(Timer) -> timer:cancel(Timer) end, State#state.timer).

resize({W, H}) ->
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W, H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    ok.

draw(S=#state{canvas = Canvas, snake = Snake, apple = Apple}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    {W,H} = wxGLCanvas:getSize(Canvas),
    GridPos = draw_grid(W,H, S#state.options),
    draw_snake(Snake, GridPos),
    draw_apple(Apple, GridPos),
    wxGLCanvas:swapBuffers(Canvas).


draw_grid(W,H, Options) ->
    {GridW, GridH} = proplists:get_value(grid_size, Options, ?DEFAULT_GRID_SIZE),
    {LinesV, LinesH} = proplists:get_value(lines, Options, ?DEFAULT_LINES),
    GridPosX = (W-GridW) div 2,
    GridPosY =  (H-GridH) div 2,
    gl:pushMatrix(),
    gl:translatef(GridPosX, GridPosY, 0),
    gl:'begin'(?GL_LINES),
    gl:color3ub(0,255,0),
    FunV =
	fun(Pos) ->
		gl:vertex2i(Pos * (GridW div LinesV),0),
		gl:vertex2i(Pos * (GridW div LinesV),GridH)
	end,
    FunH =
	fun(Pos) ->
		gl:vertex2i(0,    Pos * (GridH div LinesH)),
		gl:vertex2i(GridW,Pos * (GridH div LinesH))
	end,
    wx:foreach(FunV, lists:seq(0, LinesV)),
    wx:foreach(FunH, lists:seq(0, LinesH)),
    gl:'end'(),
    gl:popMatrix(),
    {GridPosX,GridPosY}.

draw_snake({Head, Tail}, GridPos) ->
    draw_snake(Head, GridPos),
    draw_snake(Tail, GridPos);
draw_snake([{X,Y}|Rest], GridPos = {GridX, GridY}) ->
    gl:pushMatrix(),
    gl:translatef(GridX,GridY,0),
    gl:translatef(X-1, Y-1, 0),
    gl:'begin'(?GL_QUADS),
    gl:color3ub(0,0,255),
 
    gl:vertex2i(0, 0),
    gl:vertex2i(0, 10),
    gl:vertex2i(10,10),
    gl:vertex2i(10, 0),
    
    gl:'end'(),
    gl:popMatrix(),
    draw_snake(Rest, GridPos);
draw_snake([], _) ->
    ok.

draw_apple({X,Y}, {GridX, GridY}) ->
    gl:pushMatrix(),
    gl:translatef(GridX,GridY,0),
    gl:translatef(X-1, Y-1, 0),
    gl:'begin'(?GL_QUADS),
    gl:color3ub(230,50,50),

    gl:vertex2i(0, 0),
    gl:vertex2i(0, 10),
    gl:vertex2i(10, 10),
    gl:vertex2i(10, 0),

    gl:'end'(),
    gl:popMatrix().







move_snake(State = #state{snake = Snake = {[{X, Y} | _],
					   _T}}) ->

    Snake2 =
	case State#state.direction of
	    up ->
		do_move_snake({X, Y -10}, Snake, State#state.grow);
	    down ->
		do_move_snake({X, Y +10}, Snake, State#state.grow);
	    left ->
		do_move_snake({X -10, Y}, Snake, State#state.grow);
	    right ->
		do_move_snake({X +10, Y}, Snake, State#state.grow)
	end,
    State#state{snake = Snake2, grow = false}.

do_move_snake(NewHead, {Head,Tail}, false) ->
    {[NewHead|Head], tl(Tail)};
do_move_snake(NewHead, {Head,Tail}, true) ->
    {[NewHead|Head], Tail}.


check_snake(State = #state{snake = {Head, Tail},
			   apple = {AX,AY}}) ->
    [H={X,Y} | T] = Head,
    State2 =
	if X >= AX, X =< AX+10,
	   Y >= AY, Y =< AY+10 ->
		Size = proplists:get_value(grid_size, State#state.options, {300,300}),
		State#state{grow = true, apple = get_random_apple(Size)};
	   X+10 =< AX+10, X+10 >= AX,
	   Y+10 =< AY+10, Y+10 >= AY ->
		Size = proplists:get_value(grid_size, State#state.options, {300,300}),
		State#state{grow = true, apple = get_random_apple(Size)};
	   true ->
		State
	end,
    case Tail of
	[] ->
	    State2#state{snake = {[H], lists:reverse(T)}};
	Any when is_list(Any)->
	    State2
    end.

get_random_apple({Width, Height}) ->
    {random:uniform(Width),
     random:uniform(Height)}.



