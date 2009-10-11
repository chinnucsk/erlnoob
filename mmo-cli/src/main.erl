%%%-------------------------------------------------------------------
%%% File    : main.erl
%%% Author  :  <Olle@ZUBAT>
%%% Description : 
%%%
%%% Created : 11 Oct 2009 by  <Olle@ZUBAT>
%%%-------------------------------------------------------------------
-module(main).

-compile(export_all).

-include_lib("wx/include/wx.hrl").

start() ->
    start([{size, {800,600}}]).


start(Opts) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "hello world", Opts),
    Panel = wxPanel:new(Frame, []),
    GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Panel, GLAttrib),
    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(Canvas),
    timer:sleep(3000),
    wx:destroy().
    



