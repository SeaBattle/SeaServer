%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. мар 2014 23:17
%%%-------------------------------------------------------------------
-author("tihon").

% Состояние подключённого к серверу клиента
-record(client_state,
{
	socket            % сокет
}).
