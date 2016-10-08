%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2016 11:40
%%%-------------------------------------------------------------------
-module(game_logics_SUITE).
-author("tihon").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [
    test
  ].

init_per_suite(Config) ->
  Config.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

end_per_suite(Config) ->
  Config.

test_create_game(_) ->
  ct:pal("------------------~p------------------~n", [test_create_game]),


  ok.