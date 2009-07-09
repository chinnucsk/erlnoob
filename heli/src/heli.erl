%%%-------------------------------------------------------------------
%%% File    : heli.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  7 Jul 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(heli).

-export([start/0]).


start() ->
    wx:new(),
    heli_gl:start().

start_link() ->
    wx:new(),
    heli_gl:start_link().
