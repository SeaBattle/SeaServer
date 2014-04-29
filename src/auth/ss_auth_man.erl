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

% Password is undefined - GuestAuth
make_auth(auth_packet, {Uid, undefined}) ->
	UidBin = if is_binary(Uid) -> Uid;
		         true -> list_to_binary(Uid)
	         end,
	case ss_db_sup:get(?DB_POOL, ?PLAYERS, UidBin) of
		{ok, Object} -> % запись найдена - вернуть
			%% % TODO проверка на бан
			Player = binary_to_term(riakc_obj:get_value(Object)),
			{ok, Player};
		{error, notfound} -> % запись не найдена - создать
			{ok, ss_database:create_guest(UidBin)};
		{error, Other} ->
			io:format("~w, error occured, can't read value from db:~p~n", [?MODULE, Other]),
			{500, <<"Server error!">>}
	end;
% Password presents - LoginAuth
make_auth(auth_packet, {Login, Password}) ->
	LoginBin = if is_binary(Login) -> Login;
		           true -> list_to_binary(Login)
	           end,
	case ss_db_sup:get(?DB_POOL, ?LOGINS, LoginBin) of
		{ok, Object} -> % запись найдена
			PlayerLogin = binary_to_term(riakc_obj:get_value(Object)),
			if
				Password =:= PlayerLogin#login.password -> % пароли совпадают - загрузить запись игрока
					% TODO проверка на бан
					% TODO загрузка записи игрока через login или uid
					ok;
				true -> {400, <<"Bad password!">>}
			end;
		{error, notfound} -> % запись не найдена - ошибка
			{404, <<"User not found!">>};
		{error, Other} ->
			io:format("~w, error occured, can't read value from db:~p~n", [?MODULE, Other]),
			{500, <<"Server error!">>}
	end.


