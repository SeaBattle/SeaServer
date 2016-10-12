%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:40
%%%-------------------------------------------------------------------
-author("tihon").

%% transport system headers
-define(PACKET_TYPE, <<"packet_type">>).
-define(PACKET_ID, <<"packet_id">>).
-define(CODE_HEAD, <<"code">>).

%% logic headers
-define(UID_HEAD, <<"uid">>).    %user's unique id (is generated is su_user_logic as uuid)
-define(USER_TOKEN, <<"token">>).      %user is registered online by this token in su_user_logic
-define(RESULT_HEAD, <<"result">>).
-define(VERSION_HEAD, <<"vsn">>).
-define(GAME_AWAIT_TTL_HEAD, <<"ttl">>).
-define(RULES_HEAD, <<"rules">>).
-define(GAME_ID_HEAD, <<"game_id">>).
-define(ACTION_HEAD, <<"action">>).
-define(PAYLOAD_HEAD, <<"payload">>).
-define(TOKEN_HEAD, <<"token">>).
-define(PRIVATE_HEAD, <<"private">>). %whether game can be found in game service or not
-define(FROM_HEAD, <<"from">>).

%% rules
%how many time game should wait after terminating player for reconnect to make a technical win
-define(RECONNECT_TIMER_HEAD, <<"reconnect_timer">>). %can be binaries of <<"NM">>, where N is integer and M is hh/mm/ss
-define(NEAR_PLACING_HEAD, <<"allow_near_placing">>). %allow ships be placed near each other
-define(REPEAT_ON_HIT_HEAD, <<"repeat_turn_on_hit">>).  %can player repeat turn when he hit enemy ship
-define(FIRES_PER_TURN_HEAD, <<"fires_per_turn">>). %how many fires can be made by player before turn changes
-define(SHIP_DECK1_HEAD, <<"deck1">>).  %deck1 ship
-define(SHIP_DECK2_HEAD, <<"deck2">>).  %deck2 ship
-define(SHIP_DECK3_HEAD, <<"deck3">>).  %deck3 ship
-define(SHIP_DECK4_HEAD, <<"deck4">>).  %deck4 ship
-define(SHIP_DECK5_HEAD, <<"deck5">>).  %deck5 ship

%% ship
-define(SHIP_ID_HEAD, <<"id">>).
-define(SHIP_X_POS_HEAD, <<"x">>).
-define(SHIP_Y_POS_HEAD, <<"y">>).
-define(SHIP_DIRECTION_HEAD, <<"W">>).
-define(SHIP_SIZE_HEAD, <<"size">>).

%% game
-define(SHIPS_HEAD, <<"ships">>).  %ships array with {id, size, x, y, w} position
-define(FIRE_TYPE_HEAD, <<"type">>).
-define(FIRE_X_HEAD, <<"y">>).
-define(FIRE_Y_HEAD, <<"x">>).
-define(SHOT_HEAD, <<"shot">>).
-define(CHANGE_TURN_HEAD, <<"change">>).