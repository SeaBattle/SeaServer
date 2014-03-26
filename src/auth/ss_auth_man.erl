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

make_auth(guest_auth_packet, {Uid}) -> {500, <<"Internal server error">>};
make_auth(login_auth_packet, {Login, Password}) -> ok.