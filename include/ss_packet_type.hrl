%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:37
%%%-------------------------------------------------------------------
-author("tihon").

%% User
-define(AUTH_PACKET, 0).
-define(REGISTER_PACKET, 1).
-define(USER_PACKET, 2).

%% Chat
-define(CHAT_MSG_PACKET, 100).
-define(CHAT_ENTER_PACKET, 110).
-define(CHAT_LEAVE_PACKET, 120).

%% Game
-define(GAME_INVITE_PACKET, 200).
-define(GAME_START_PACKET, 210).