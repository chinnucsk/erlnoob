%%%-------------------------------------------------------------------
%%% File    : snake.hrl
%%% Author  :  <Olle@MUDKIPZ>
%%% Description : 
%%%
%%% Created :  9 Nov 2008 by  <Olle@MUDKIPZ>
%%%-------------------------------------------------------------------


-record(snake, {head = [],
		tail = []}).


-record(state, {frame,
		grid,
		status_bar,
		rows,
		cols,
		panel,
		main_sizer,
		level,
		score = 0,
		direction = down,
		snake = #snake{},
		speed = 200,
		main_window_pid,
		red = ?wxRED,
		black = ?wxBLACK,
		white = ?wxWHITE,
		timer,
		apple_pos,
		mode,
		points = 10}).


