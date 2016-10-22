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

%% game codes
-define(INCORRECT_SHIP_NUMBER, 401).
-define(INCORRECT_SHIP_POSITION, 402).
-define(WRONG_GAME, 403).
-define(WRONG_TURN, 404).
-define(WRONG_USER, 405).

%% common errors
-define(SERVER_ERROR, 500).
-define(BAD_PACKAGE, 501).

%% game service codes
-define(GAME_STARTED, 100).       %other player just joined and game starts
-define(GAME_NOT_AVAILABLE, 101). %no game for such GID was found
-define(WAITING_FOR_CONNECT, 102).  %fast play is not able to find ready game, so it creates game and wait for connection
-define(USER_OFFLINE, 103).     %user is not for online play
-define(GAME_NOT_READY, 104). %search for fast play can't find game for player and will wait for player.