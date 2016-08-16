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