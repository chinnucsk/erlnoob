%%%-------------------------------------------------------------------
%%% File    : gl_misc.erl
%%% Author  :  <Olle@ZUBAT>
%%% Description : 
%%%
%%% Created : 11 Oct 2009 by  <Olle@ZUBAT>
%%%-------------------------------------------------------------------
-module(gl_misc).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-export([window/1, window/2]).

-define(DEFAULT_GL_ATTR, [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}]).
-define(DEFAULT_FRAME_OPTS, [{size, {800,600}}]).

window(Title) ->
    window(Title, []).

window(Title, Opts) ->
    FrameOpts = proplists:get_value(frame_options, Opts, ?DEFAULT_FRAME_OPTS),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, FrameOpts),
    Panel = wxPanel:new(Frame, []),
    GLAttrs = proplists:get_value(gl_attributes, Opts, ?DEFAULT_GL_ATTR),
    Canvas = wxGLCanvas:new(Panel, GLAttrs),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer, Canvas, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, Sizer),
    wxFrame:showFullScreen(Frame, proplists:get_value(fullscreen, Opts, true)),
    wxGLCanvas:setCurrent(Canvas),
    {Frame, Canvas}.

    
