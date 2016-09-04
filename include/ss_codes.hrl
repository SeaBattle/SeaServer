%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2016 0:09
%%%-------------------------------------------------------------------
-author("tihon").

-define(OK, 0).

-define(INCORRECT_SHIP_NUMBER, 401).
-define(INCORRECT_SHIP_POSITION, 402).

-define(SERVER_ERROR, 500).

%% game service codes
-define(GAME_STARTED, 100).       %other player just joined and game starts
-define(GAME_NOT_AVAILABLE, 101). %no game for such GID was found
-define(WAITING_FOR_CONNECT, 102).  %fast play is not able to find ready game, so it creates game and wait for connection