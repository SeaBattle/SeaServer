%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:40
%%%-------------------------------------------------------------------
-author("tihon").

-define(PACKET_TYPE, <<"packet_type">>).

-define(EMAIL_HEAD, <<"email">>).  %user's unique email (or any other string)
-define(UID_HEAD, <<"uid">>).    %user's unique id (is generated is su_user_logic as uuid)
-define(NAME_HEAD, <<"name">>).  %user's name
-define(LANG_HEAD, <<"lang">>).  %user's language (for password's email localisation)
-define(SECRET_HEAD, <<"secret">>).    %user's password (is generated in su_user_logic)
-define(USER_TOKEN, <<"token">>).      %user is registered online by this token in su_user_logic
-define(RESULT_HEAD, <<"result">>).
-define(CODE_HEAD, <<"code">>).
-define(VERSION_HEAD, <<"vsn">>).
-define(GAME_AWAIT_TTL_HEAD, <<"ttl">>).
-define(RULES_HEAD, <<"rules">>).
-define(GAME_ID_HEAD, <<"game_id">>).

%% rules
-define(RECONNECT_TIMER_HEAD, <<"reconnect_timer">>).
-define(NEAR_PLACING_HEAD, <<"allow_near_placing">>).
-define(REPEAT_ON_HIT_HEAD, <<"repeat_turn_on_hit">>).
-define(FIRES_PER_TURN_HEAD, <<"fires_per_turn">>).
-define(SHIP_DECK1_HEAD, <<"deck1">>).
-define(SHIP_DECK2_HEAD, <<"deck2">>).
-define(SHIP_DECK3_HEAD, <<"deck3">>).
-define(SHIP_DECK4_HEAD, <<"deck4">>).
-define(SHIP_DECK5_HEAD, <<"deck5">>).