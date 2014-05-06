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
-export([make_auth/2, register/1]).

make_auth(guest_auth, {Uid, _}) ->
	UidBin = if is_binary(Uid) -> Uid;
		         true -> list_to_binary(Uid)
	         end,
	case ss_db_sup:get(?DB_POOL, ?PLAYERS, UidBin) of
		{ok, Object} -> % запись найдена - вернуть
			%% % TODO проверка на бан
			binary_to_term(riakc_obj:get_value(Object));
		{error, notfound} -> % запись не найдена - создать
			{ok, create_guest(UidBin)};
		{error, Other} ->
			io:format("~w, error occured, can't read value from db:~p~n", [?MODULE, Other]),
			{error, {500, <<"Server error!">>}}
	end;
make_auth(login_auth, {Login, Password}) ->
	LoginBin = if is_binary(Login) -> Login;
		           true -> list_to_binary(Login)
	           end,
	case ss_db_sup:get(?DB_POOL, ?LOGINS, LoginBin) of
		{ok, Object} -> % запись найдена
			PlayerLogin = binary_to_term(riakc_obj:get_value(Object)),
			io:format("Found ~p~n", [PlayerLogin]),
			if
				Password == PlayerLogin#login.password -> % пароли совпадают - загрузить запись игрока
					% TODO проверка на бан
					% TODO загрузка записи игрока через login или uid
					ok;
				true ->
					io:format("Password mismatch ~p - ~p~n"), %TODO вместо пересылки пароля в открытом виде использовать ЦП
					{error, {403, <<"Bad password!">>}}
			end;
		{error, notfound} -> % запись не найдена - ошибка
			{error, {404, <<"User not found!">>}};
		Error ->
			io:format("~w, error occured, can't read value from db: ~p~n", [?MODULE, Error]),
			{error, {500, <<"Server error!">>}}
	end.

register({{Login, Password, Uid}, {Name, Motto, Icon}}) ->
	LoginBin = if is_binary(Login) -> Login;
		           true -> list_to_binary(Login)
	           end,
	UidBin = if is_binary(Uid) -> Uid;
		         true -> list_to_binary(Uid)
	         end,
	case ss_db_sup:get(?DB_POOL, ?LOGINS, LoginBin) of  % проверить уникальность ключа.
		{ok, _} -> % запись найдена, сохранять нельзя
			{error, {403, <<"Login already exists.">>}};
		{error, notfound} -> % запись не найдена - сохраняем
			case ss_database:create_login(LoginBin, Password, UidBin) of
				created -> % запись была создана с нуля - нужно создать игрока и стену
					create_client(LoginBin, Name, Motto, Icon);
				{used, PlayerObj} -> % была использована гостевая запись - нужно обновить запись
					update_client(PlayerObj, UidBin, Name, Motto, Icon)
			end;
		_ -> {error, {500, <<"Server error.">>}}
	end.

% Создаёт гостевую запись, стену и корабли. Сохраняет гостевую запись и стену асинхронно. Корабли - синхронно.
create_guest(Uid) ->
	Player = ss_database:create_player("Guest"),
	ShipKeys = ss_database:save_ships(Player#player.ships), % синхронно сохранить все корабли и получить сгенерированные ключи
	% асинхронно сохранить структуру игрока
	% успешность создания пользователя игнорируется, т.к. гостю не важно, сохранили его или нет
	ss_db_sup:put_async(?DB_POOL, ss_database:compile_player(Player, ShipKeys, Uid)),
	WallObj = riakc_obj:new(?WALLS, Uid, Player#player.wall),
	ss_db_sup:put_async(?DB_POOL, WallObj),
	Player.

% Создаёт запись игрока, стены, кораблей. Сохраняет игрока с кораблями и стену
create_client(Key, Name, Motto, Icon) ->
	Player = ss_database:create_player(Name, Motto, Icon),
	ShipKeys = ss_database:save_ships(Player#player.ships), % синхронно сохранить все корабли и получить сгенерированные ключи
	ok = ss_db_sup:put(?DB_POOL, ss_database:compile_player(Player, ShipKeys, Key)),
	WallObj = riakc_obj:new(?WALLS, Key, Player#player.wall),
	ok = ss_db_sup:put(?DB_POOL, WallObj),
	Player.

% Обновляет запись игрока. Устанавливает новые имя, девиз и иконку
update_client(OldPlayerObj, Key, Name, Motto, Icon) ->
	OldPlayer = binary_to_term(riakc_obj:get_value(OldPlayerObj)),
	Player = ss_database:update_player(OldPlayer, OldPlayerObj, Name, Icon),  % TODO здесь может быть exception при работе с БД. Где отлавливать?
	Wall = ss_database:update_wall(Key, Motto),
	Player#player{wall = Wall}.