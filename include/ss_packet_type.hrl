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
-define(AUTH_PACKET, auth).

%% Game
-define(GAME_CREATE_PACKET, game_create). %user creates his game
-define(GAME_INVITE_PACKET, game_invite). %user invites other user to join his game
-define(GAME_REJECT_PACKET, game_reject). %user rejects other user's invitation
-define(GAME_SEND_SHIPS, ships_package).  %user send his ships
-define(GAME_START_PACKET, game_start).   %game starts (is sending to all players by server) when all players send their ships
-define(GAME_FIRE, game_fire).  %user makes his turn
-define(GAME_FAST_PLAY, game_fast_play).  %user wants to join random game.

%% System
-define(ERROR_PACKET, error).

-define(PACKAGES,
[
  {?ERROR_PACKET, 400},
  {?AUTH_PACKET, 100},
  {?GAME_CREATE_PACKET, 101},
  {?GAME_INVITE_PACKET, 102},
  {?GAME_REJECT_PACKET, 103},
  {?GAME_SEND_SHIPS, 104},
  {?GAME_START_PACKET, 105},
  {?GAME_FIRE, 106},
  {?GAME_FAST_PLAY, 107}
]).