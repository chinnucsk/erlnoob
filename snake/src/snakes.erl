%%%-------------------------------------------------------------------
%%% File    : cubes.erl
%%% Author  :  <dgud@LINA>
%%% Description : Playing around
%%%
%%% Created : 30 Oct 2008 by  <dgud@LINA>
%%%-------------------------------------------------------------------
-module(snakes).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-define(error(), error(?LINE)).
-define(F32, 32/native-float).
-define(GSz, 0).
-define(TEXTURE_SIZE, 512).

-define(FPS, 10000). %% Frame time in micro-sec.

-record(snake, {color={0.8,0.2,0.2},
               dir = {1.0, 0.0},
               body=[{0.0,0.0,0.0},
                     {0.0,0.0,1.0},
                     {0.0,0.0,2.0},
                     {0.0,0.0,3.0}
                    ]}).

-record(ds, {t0,   % Start time
            tn,   % Prev frame
            td=0, % Sleep to next frame
            tp,   % Previous move
            vel=500,  % Updates move every vel milli sec
            fc,
            light_pos = {-15,10,30},
            snakes = [#snake{}],
            canvas, data}).

-define(VS, {{ 0.4,  1.0, -0.4},  %1
            { 0.4,  0.0, -0.4},  %2
            {-0.4,  0.0, -0.4},
            {-0.4,  1.0, -0.4},  %4
            {-0.4,  1.0,  0.4},
            { 0.4,  1.0,  0.4},  %6
            { 0.4,  0.0,  0.4},
            {-0.4,  0.0,  0.4}}).%8

-define(FACES,
       %% Faces    Normal
       [{[1,2,3,4],{0,0,-1}},  %
        {[3,8,5,4],{-1,0,0}},  %
        {[1,6,7,2],{1,0,0} },  %
        {[6,5,8,7],{0,0,1} },  %
        {[6,1,4,5],{0,1,0} },  %
        {[7,8,3,2],{0,-1,0}}]).

start() ->
   init().

init() ->
   WX = wx:new(),
   Frame   = wxFrame:new(WX,1,"Snake",[{size, {800,600}}]),
   ok = wxFrame:connect(Frame, close_window),
   wxFrame:createStatusBar(Frame,[]),
   setup_menus(Frame),
   GLAttrs = [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0],
   Canvas = wxGLCanvas:new(Frame, [{attribList, GLAttrs}]),
   wxWindow:connect(Canvas, motion),
   wxWindow:connect(Canvas, key_up),
   Data = setup_vs(),
   wxWindow:show(Frame),
   %% Must show window before creating GL context.
   wxGLCanvas:setCurrent(Canvas),
   State0 = initGL(Canvas,Data),
   loop(Frame, State0),
   wx:destroy().

loop(Frame,State = #ds{td=Rem, fc=FC}) ->
   receive
       #wx{event=#wxClose{}} ->
           io:format("~p Destroying window ~n",[self()]),
           %%wxWindow:close(Frame,[]),
           State;
       #wx{id=11} ->
           wxWindow:close(Frame,[]),
           io:format("~p Closing window ~n",[self()]),
           State;
       #wx{event=#wxKey{keyCode=Key}} ->
           State1 = handle_key(Key, State),
           loop(Frame,calc_rem(State1));
       #wx{event=#wxMouse{type=motion,x=X,y=Y}} ->
           Str = lists:flatten(io_lib:format("Mouse {~p,~p}", [X,Y])),
           wxFrame:setStatusText(Frame, Str,[]),
           loop(Frame,calc_rem(State));
       Wx = #wx{} ->
           io:format("~p Received wx record ~p~n",[self(), Wx]),
           loop(Frame,calc_rem(State));
       Kalle ->
           io:format("Received ~p~n",[Kalle]),
           loop(Frame,calc_rem(State))
   after Rem ->
           New = drawCanvas(State),
           case FC rem 100 of
               0 ->
                   Ms = timer:now_diff(now(), State#ds.t0) div 1000,
                   Str = lists:flatten(io_lib:format(
                                         " FPS: ~p Sleep: ~p ms",
                                         [1000*FC div Ms, Rem])),
                   wxFrame:setStatusText(Frame, Str,[]);
               _ ->
                   ok
           end,
           ?error(),
           loop(Frame,calc_rem(New))
   end.

error(Line) ->
   case gl:getError() of
       0 -> ok;
       Err ->
           %% io:format("~p:~p ~n",[Line,glu:errorString(Err)])
           io:format("~p:~p (~.16X) ~n",[Line,Err,Err,"16#"])
   end.

handle_key(?WXK_UP, State = #ds{snakes=[Snake]}) ->
   State#ds{snakes=[Snake#snake{dir={0.0,-1.0}}]};
handle_key(?WXK_DOWN, State = #ds{snakes=[Snake]}) ->
   State#ds{snakes=[Snake#snake{dir={0.0,1.0}}]};
handle_key(?WXK_LEFT, State = #ds{snakes=[Snake]}) ->
   State#ds{snakes=[Snake#snake{dir={-1.0,0.0}}]};
handle_key(?WXK_RIGHT, State = #ds{snakes=[Snake]}) ->
   State#ds{snakes=[Snake#snake{dir={1.0,0.0}}]}.

setup_menus(Frame) ->
   MenuBar = wxMenuBar:new(),
   Menu    = wxMenu:new([]),
   true = wxMenuBar:append(MenuBar, Menu, "&File"),
   wxMenu:append(Menu, 10, "&About", []),
   wxMenu:append(Menu, 11, "Exit", []),

   ok = wxFrame:connect(Frame, command_menu_selected),
   ok = wxFrame:setMenuBar(Frame,MenuBar).

initGL(Canvas, Data = {Vs,Ns}) ->
   {W,H} = wxWindow:getClientSize(Canvas),
   ?error(),
   gl:viewport(0,0,W,H),
   gl:matrixMode(?GL_PROJECTION),
   gl:loadIdentity(),
   %% Setup camera information
   glu:perspective(30, W/H, 5.0, 400.0),

   gl:matrixMode(?GL_MODELVIEW),
   gl:loadIdentity(),
   %% Setup camera position
   glu:lookAt(5,50,15, 0,0,0, 0,1,0),

   gl:hint(?GL_POINT_SMOOTH_HINT, ?GL_NICEST),
   gl:enable(?GL_POINT_SMOOTH),
   gl:hint(?GL_LINE_SMOOTH_HINT, ?GL_NICEST),
   gl:enable(?GL_LINE_SMOOTH),
   gl:hint(?GL_POLYGON_SMOOTH_HINT, ?GL_NICEST),
   gl:enable(?GL_POLYGON_SMOOTH),

   gl:enable(?GL_DEPTH_TEST),
   gl:depthFunc(?GL_LESS),
   %% Background color
   gl:clearColor(0.5,0.0,0.5,1.0),

   gl:enableClientState(?GL_VERTEX_ARRAY),
   gl:vertexPointer(3, ?GL_FLOAT, 0, Vs),
   gl:enableClientState(?GL_NORMAL_ARRAY),
   gl:normalPointer(?GL_FLOAT, 0, Ns),
   gl:shadeModel(?GL_SMOOTH),

   gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {-15,10,30,0}),
   gl:enable(?GL_LIGHTING),
   gl:enable(?GL_LIGHT0),
   gl:enable(?GL_COLOR_MATERIAL),

   ?error(),
   Now = now(),
   #ds{t0=Now, tn=Now, tp=Now, td=0, fc=0,
       canvas = Canvas, data = Data}.

drawCanvas(#ds{tp=T1,vel=Vel,fc=Fc,
              snakes=Snakes0,
              canvas=Canvas
             } = State) ->

   gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
   [drawSnake(Snake) || Snake <- Snakes0],
   wxGLCanvas:swapBuffers(Canvas),
   ?error(),
   Now = now(),
   Time = timer:now_diff(Now,T1) div 1000,
   case Time > Vel of
       true ->
           Snakes1 = [update_snake(Snake) || Snake <- Snakes0],
           State#ds{fc=Fc+1,tp=Now,snakes=Snakes1};
       false ->
           State#ds{fc=Fc+1}
   end.

calc_rem(#ds{tn=T0} = State)->
   T1 = now(),
   Tdiff = timer:now_diff(T1, T0),
   Sleep = ?FPS - Tdiff,
   case Sleep =< 1000 of
       true ->
           State#ds{tn=T1,td=0};
       false ->
           State#ds{tn=T1,td=Sleep div 1000}
   end.

drawSnake(#snake{color=Col, body=List}) ->
   gl:color3fv(Col),

   Draw = fun({X,Y,Z}) ->
                  gl:pushMatrix(),
                  gl:translatef(X,Y,Z),
                  gl:drawArrays(?GL_QUADS, 0, 24*(2*?GSz+1)*(2*?GSz+1)),
                  gl:popMatrix()
          end,
   lists:foreach(Draw, List).

update_snake(Snake = #snake{dir={X,Z}, body=List}) ->
   [OldHead={X1,Y1,Z1}|Tail] = List,
   [_|All] = lists:reverse(Tail),
   New = [{X1+X,Y1,Z1+Z},OldHead|lists:reverse(All)],
   Snake#snake{body=New}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_vs() ->
   setup_vs(-?GSz, -?GSz, <<>>, <<>>).

setup_vs(GX, GY, VsA, NA) when GX =< ?GSz ->
   VPos = fun(Vertex,PX,PY) ->
                  {X,Y,Z} = element(Vertex, ?VS),
                  <<(PX+X):?F32,Y:?F32,(PY+Z):?F32>>
          end,
   Cube =
       << %% For all faces in the cube
        << (<< %% For all vertices in the face
             << (VPos(Id,GX,GY)):12/binary >> || Id <- Vs >>):48/binary >>
        || {Vs,_} <- ?FACES >>,
   Ns = << %% For all faces in the cube
         <<
          (<< %% For all vertices in the face
            <<X:?F32,Y:?F32,Z:?F32>>
            || _ <- Vs >>):48/binary >>
         || {Vs,{X,Y,Z}} <- ?FACES >>,
   setup_vs(GX+1,GY,
            <<VsA/binary,Cube:288/binary>>,
            <<NA/binary, Ns:288/binary>>);

setup_vs(_GX, GY, Vs, Ns) when GY < ?GSz ->
   setup_vs(-?GSz, GY+1, Vs, Ns);
setup_vs(_, _, Vs, Ns) ->
   {Vs,Ns}.
