%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Sep 2016 22:33
%%%-------------------------------------------------------------------
-author("tihon").

-record(game_state,
{
  game_id :: ss_types:gid(),
  player1 :: {pid(), ss_types:uid()},
  player2 :: {pid(), ss_types:uid()},
  fleet_player1 :: map(),
  fleet_player2 :: map(),
  ships_player1 = [] :: proplists:proplist(),
  ships_player2 = [] :: proplists:proplist(),
  active = 1 :: pos_integer(),  % 0 - player1, 1 - player2
  shots_left = 1 :: pos_integer(),  % used in cases of repeat_on_turn and multiple_shots per turn
  rules :: ss_types:rules_compressed()
}).

%% fire types
-define(NORMAL_TYPE, <<"normal">>).
-define(MULTI_TYPE, <<"multi">>).