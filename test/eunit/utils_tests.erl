%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Sep 2016 19:45
%%%-------------------------------------------------------------------
-module(utils_tests).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

replace_at_test() ->
  ?assertEqual(<<1, 1, 1, 1, 5, 0, 0, 0, 1, 1>>, ss_utils:replace_at(<<1, 1, 1, 1, 0, 0, 0, 0, 1, 1>>, 4, <<5>>)),
  ?assertEqual(<<9, 1, 1, 1, 1, 1, 1, 1, 1, 1>>, ss_utils:replace_at(<<1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>, 0, <<9>>)),
  ?assertEqual(<<1, 1, 1, 1, 5, 5, 5, 5, 1, 1>>, ss_utils:replace_at(<<1, 1, 1, 1, 5, 5, 5, 5, 1, 1>>, 4, <<5, 5, 5, 5>>)).
