%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2016 16:08
%%%-------------------------------------------------------------------
-module(ss_types).
-author("tihon").

-type uid() :: binary().
-type gid() :: binary().
-type code() :: integer().
-type rules_compressed() :: binary().
-type rules_full() :: map().

-export_type([uid/0, gid/0, code/0, rules_compressed/0, rules_full/0]).

%% API
-export([]).