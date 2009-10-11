%%%-------------------------------------------------------------------
%%% File    : main.erl
%%% Author  :  <Olle@ZUBAT>
%%% Description : 
%%%
%%% Created : 11 Oct 2009 by  <Olle@ZUBAT>
%%%-------------------------------------------------------------------
-module(main).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-export([start/0,start/1]).

-export([init/1, handle_event/2, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-compile(export_all).

-record(state, {canvas, image}).

start() ->
    start([{size, {800,600}}]).


start(Opts) ->
    wx_object:start(?MODULE, Opts, []).

init(_Opts) ->
    wx:new(),
    {Frame, Canvas} = gl_misc:window("hello world"),
    wxGLCanvas:connect(Canvas, size),
    wxGLCanvas:connect(Canvas, key_down),
    init_gl(Canvas),
    font:load_font("font.spr"),
    spawn_link(?MODULE, updater, [self()]),
    {Frame, #state{canvas = Canvas}}.

handle_event(#wx{event = #wxKey{keyCode = ?WXK_ESCAPE}}, State) ->
    {stop, shutdown, State};
handle_event(#wx{event = #wxSize{size = {W,H}}}, State) ->
    resize(W,H),
    wxGLCanvas:swapBuffers(State#state.canvas),
    {noreply, State};
handle_event(Wx, State) ->
    io:format("~p\n", [Wx]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update, State) ->
    draw_game_screen(),
    wxGLCanvas:swapBuffers(State#state.canvas),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



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

    wxGLCanvas:swapBuffers(Canvas).

    
resize(W,H) ->
    gl:clearColor(1,1,1,1),
    gl:viewport(0, 0, W, H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W, H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity().



updater(Pid) ->
    Pid ! update,
    timer:sleep(100),
    updater(Pid).

-define(SIZE, 4).
-define(SS, 16 * ?SIZE).
-define(GAME_PLAN, [{?SS*0,?SS*0},{?SS*1,?SS*0},{?SS*2,?SS*0},{?SS*3,?SS*0},
		    {?SS*0,?SS*1},{?SS*1,?SS*1},{?SS*2,?SS*1},{?SS*3,?SS*1},
		    {?SS*0,?SS*2},{?SS*1,?SS*2},{?SS*2,?SS*2},{?SS*3,?SS*2},
		    {?SS*0,?SS*3},{?SS*1,?SS*3},{?SS*2,?SS*3},{?SS*3,?SS*3},
		    {?SS*0,?SS*4},{?SS*1,?SS*4},{?SS*2,?SS*4},{?SS*3,?SS*4}]).

draw_game_screen() ->
    font:draw_text("olle", 100, 100).
