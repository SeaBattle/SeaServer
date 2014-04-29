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

-include("ss_records.hrl").
-include("ss_database.hrl").

%% API
-export([make_auth/2]).

% TODO проверка на бан
% Password is undefined - GuestAuth
make_auth(auth_packet, {Uid, undefined}) ->
	case ss_db_sup:get(?DB_POOL, {?PLAYERS, Uid}, []) of
		{error, notfound} -> % запись не найдена - создать
			{ok, ss_database:create_guest(Uid)};
		{ok, Object} -> % запись найдена - вернуть
			{ok, Object}
	end;
% Password presents - LoginAuth
make_auth(auth_packet, {Login, Password}) ->
	case ss_db_sup:get(?DB_POOL, {?LOGINS, Login}, []) of
		{error, notfound} -> % запись не найдена - ошибка
			{404, <<"User not found!">>};
		{ok, Object} -> % запись найдена
			PlayerLogin = binary_to_term(riakc_obj:get_value(Object)),
			if
				Password =:= PlayerLogin#login.password -> % пароли совпадают - загрузить запись игрока
					% TODO загрузка записи игрока через login или uid
					ok;
				true -> {400, <<"Bad password!">>}
			end
	end.


