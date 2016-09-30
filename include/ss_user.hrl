%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2016 11:07 AM
%%%-------------------------------------------------------------------
-author("tihon").

-record(user_state,
{
  id :: binary(),
  auth = false :: boolean()
}).