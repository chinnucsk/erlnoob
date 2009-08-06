%%%-------------------------------------------------------------------
%%% File    : tibia.hrl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 15 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------

-define(p, 14299623962416399520070177382898895550795403345466153217470516082934737582776038882967213386204600674145392845853859217990626450972452084065728686565928113).
-define(q, 7630979195970404721891201847792002125535401292779123937207447574596692788513647179235335529307251350570728407373705564708871762033017096809910315212884101).
-define(d, crypto:mpint(46730330223584118622160180015036832148732986808519344675210555262940258739805766860224610646919605860206328024326703361630109888417839241959507572247284807035235569619173792292786907845791904955103601652822519121908367187885509270025388641700821735345222087940578381210879116823013776808975766851829020659073)).
-define(e, crypto:mpint(65537)).
-define(n, crypto:mpint(109120132967399429278860960508995541528237502902798129123468757937266291492576446330739696001110603907230888610072655818825358503429057592827629436413108566029093628212635953836686562675849720620786279431090218017681061521755056710823876476444260558147179707119674283982419152118103759076030616683978566631413)).

-define(UINT, unsigned-integer-little).


-define(LOGIN_PROTOCOL, 16#01).
-define(GAME_PROTOCOL, 16#0A).

-define(NODE_START, 254).
-define(NODE_END, 255).
-define(ESCAPE_CHAR, 253).

-record(state, {server_socket,
		client_socket,
		key,
		account,
		player,
		server_pid}).
-record(key, {k1,k2,k3,k4}).

-record(tries, {ip,tries}).

-record(node, {type,
	       data = <<>>,
	       children = []}).

-record(item, {id,
	       name,
	       article,
	       plural,
	       attributes}).

-record(map_item, {id,
		   attributes,
		   content}).

-record(item_type, {type,
		    flags,
		    server_id,
		    client_id,
		    ground_speed,
		    name,
		    sprite_hash,
		    minimap_color,
		    sub_param_7,
		    sub_param_8,
		    light_level,
		    light_color,
		    always_on_top_order}).

-record(coord, {x,y,z}).

-record(tile, {coord = #coord{},
	       type,
	       house_id,
	       items,
	       flags}).


-record(account, {name, password}).
-record(player, {account,
		 pos,
		 name,
		 id,
		 outfit,
		 direction,
		 health,
		 light,
		 skull,
		 shield}).

-record(outfit, {type,head,body,legs,feet,addons,corpse}).

-record(creature, {pos,
		   id,
		   name,
		   health,
		   direction,
		   outfit,
		   light,
		   speed,
		   skull,
		   shield}).

-record(monster, {name,
		  name_description,
		  race,
		  experience,
		  speed,
		  manacost,
		  health,
		  outfit,
		  flags,
		  elements,
		  immunities,
		  voices,
		  loot}).

-record(spawn, {center,radius,monsters}).
-record(spawn_monster, {monster,pos,spawn_time}).
