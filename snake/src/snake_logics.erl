%%%-------------------------------------------------------------------
%%% File    : snake_logics.erl
%%% Author  :  <Olle@MUDKIPZ>
%%% Description : 
%%%
%%% Created :  9 Nov 2008 by  <Olle@MUDKIPZ>
%%%-------------------------------------------------------------------
-module(snake_logics).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include("snake.hrl").



get_random_apple(Nrows, Ncols) ->
    {random:uniform(Nrows -2),
     random:uniform(Ncols -2)}.

pause_game(State) ->
    case State#state.timer of
	undefined ->
	    {ok, Timer} = timer:send_interval(State#state.speed,
					      State#state.main_window_pid, update),
	    State#state{timer = Timer};
	_Timer ->
	    timer:cancel(State#state.timer),
	    State#state{timer = undefined}
    end.

highscore(Score, Pid) ->
    Wx = wx:new(),
    Dialog = wxTextEntryDialog:new(Wx, "Game Over!\n\nYour score:" ++
				   integer_to_list(Score) ++
				   "\n\nEnter your name:", [{value, "<Your Name>"}]),
    wxTextEntryDialog:show(Dialog),
    Name = loop(Dialog),
    Pid ! {highscore, {Name, Score}}.

loop(Dialog) ->
    case wxTextEntryDialog:getValue(Dialog) of
	[] ->
	    timer:sleep(timer:seconds(2)),
	    loop(Dialog);
	"<Your Name>" ->
	    timer:sleep(timer:seconds(2)),
	    loop(Dialog);
	Other ->
	    Other
%%     after timer:minutes(1) ->
%% 	    exit(timeout)
    end.
    
move_snake(State = #state{snake = Snake = #snake{head = Head = [{Row, Col} | _],
						   tail = [Tail| Tail2]}}) ->
    
    {Head2, WhadDidIEat, ApplePos} =
	case State#state.direction of
	    left ->
		wx:batch(fun() -> do_move_snake(State, {Row, Col -1}, Tail) end);
	    right ->
		wx:batch(fun() -> do_move_snake(State, {Row, Col +1}, Tail) end);
	    up ->
		wx:batch(fun() -> do_move_snake(State, {Row -1, Col}, Tail) end);
	    down ->
		wx:batch(fun() -> do_move_snake(State, {Row +1, Col}, Tail) end)
	end,
    wxWindow:refresh(State#state.frame),
    case WhadDidIEat of
	true ->
	    NewScore = State#state.score + State#state.points,
	    wxFrame:setStatusText(State#state.frame, "Score: " ++ integer_to_list(NewScore)),
	    State#state{snake = Snake#snake{head = [Head2 | Head],
					    tail = [Tail | Tail2]},
			score = NewScore,
			apple_pos = ApplePos};
	false ->
	    State#state{snake = Snake#snake{head = [Head2 | Head],
					    tail = Tail2},
			apple_pos = ApplePos};
	game_over ->
	    State
    end.    


do_move_snake(State = #state{grid = Grid, red = Red, white = White, black = Black},
	   Head = {RowHead, ColHead}, {RowTail, ColTail}) ->
    %%io:format("Head: ~p Tail: ~p\n", [Head, Tail]),
    {DidIEat, ApplePos} =
	case wxGrid:getCellBackgroundColour(Grid, RowHead, ColHead) of
	    Red ->
		%% Head
		wxGrid:setCellBackgroundColour(Grid, RowHead, ColHead, Black),
		{Row, Col} = next_apple(State),
		wxGrid:setCellBackgroundColour(Grid, Row, Col, Red),
		{true, {Row, Col}};
	    White ->
		%% Head
		wxGrid:setCellBackgroundColour(Grid, RowHead, ColHead, Black),
		%% Tail
		wxGrid:setCellBackgroundColour(Grid, RowTail, ColTail, White),
		{false, State#state.apple_pos};
	    Black ->
		State#state.main_window_pid ! game_over,
		{game_over, State#state.apple_pos}
	end,
    {Head, DidIEat, ApplePos}.
    
next_apple(State = #state{black = Black, grid = Grid}) ->
    {Row, Col} = snake_logics:get_random_apple(State#state.rows, State#state.cols),
    case wxGrid:getCellBackgroundColour(Grid, Row, Col) of
	Black ->
	    next_apple(State);
	_Any ->
	    {Row, Col}
    end.
