%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. мар 2014 10:59
%%%-------------------------------------------------------------------
-module(ss_auth_man).
-author("tihon").

%% API
-export([make_auth/2]).

% Password is undefined - GuestAuth
make_auth(auth_packet, {Uid, undefined}) ->
	ss_db_sup:get(db_pool, Uid, []),
	{500, <<"Internal server error">>};
% Password presents - LoginAuth
make_auth(auth_packet, {Login, Password}) ->
	ss_db_sup:get(db_pool, Login, []),
	{500, <<"Internal server error">>}.