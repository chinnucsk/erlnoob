%%%-------------------------------------------------------------------
%%% File    : snake_board.erl
%%% Author  :  <Olle@MUDKIPZ>
%%% Description : 
%%%
%%% Created : 20 Sep 2008 by  <Olle@MUDKIPZ>
%%%-------------------------------------------------------------------
-module(snake_board).

-compile(export_all).

-include_lib("wx/include/wx.hrl").

-record(state, {board=[], pen, fonts=[]}).

new(Parent) ->
    Win = wxWindow:new(Parent, ?wxID_ANY),
    wxWindow:setFocus(Win), %% Get keyboard focus
    wxWindow:setSizeHints(Win, {250,250}),
    wxWindow:connect(Win, paint,  [{skip, true}]),
    wxWindow:connect(Win, size,   [{skip, true}]),
    wxWindow:connect(Win, key_up, [{skip, true}]),
    wxWindow:connect(Win, left_down, [{skip, true}]),
    wxWindow:connect(Win, enter_window, [{skip, true}]),
 
    %% Init pens and fonts
    Pen = wxPen:new({0,0,0}, [{width, 3}]).

