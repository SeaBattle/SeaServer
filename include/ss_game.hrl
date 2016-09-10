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
  game_id :: binary(),
  player1 :: {pid(), binary()},
  player2 :: {pid(), binary()},
  fleet_player1 = [] :: map(),
  fleet_player2 = [] :: map(),
  active = 1 :: pos_integer(),  % 0 - player1, 1 - player2
  rules :: map()
}).
