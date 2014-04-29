%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. апр 2014 16:15
%%%-------------------------------------------------------------------
-module(ss_database).
-author("tihon").

-include("ss_records.hrl").
-include("ss_database.hrl").

%% API
-export([create_guest/1]).

% TODO get на стену и корабли

create_login(Login, Uid, Password) ->
	% сохранить в базу данных
	LoginObj = riakc_obj:new(?LOGINS, Login, #login{password = Password, uid = Uid}),
	% TODO доделать, проверка на уникальность + сохранение
	ok.
create_guest(Uid) ->
	% создать игровую запись игрока и бд-объекты игрока, стены и кораблей
	Player = create_player("Guest"),
	io:format("Created player = ~p~n", [Player]),
	% синхронно сохранить все корабли и получить сгенерированные ключи
	F = fun(Ship) ->
		ShipObj = riakc_obj:new(?SHIPS, undefined, Ship),
		case ss_db_sup:put(?DB_POOL, ShipObj) of
			{ok, Key} -> Key;
			{error, _} -> []
		end
	end,
	Keys = [F(S) || S <- Player#player.ships],
	io:format("Saved ships, got keys: ~p~n", [Keys]),

	% бд-объект не сохраняет стену и корабли, т.к. стена будет сохранена под тем же ключом в другой корзине,
	% а корабли будут сохранены отдельно и слинкованы с этим объектом посредством вторичных индексов
	PlayerObj = riakc_obj:new(?PLAYERS, Uid, Player#player{wall = undefined, ships = undefined}),

	% слинковать вторичными индексами сохранённые корабли и бд-объект игрока
	Meta = riakc_obj:get_update_metadata(PlayerObj),
	LinkedMeta = riakc_obj:set_secondary_index(Meta, [{{binary_index, "ships"}, Keys}]),

	% успешность создания пользователя игнорируется, т.к. гостю не важно, сохранили его или нет
	ss_db_sup:put_async(?DB_POOL, riakc_obj:update_metadata(PlayerObj, LinkedMeta)),
	WallObj = riakc_obj:new(?WALLS, Uid, Player#player.wall),
	ss_db_sup:put_async(?DB_POOL, WallObj),
	Player.

% Создаёт запись игрока + стену и базовые корабли.
create_player(Name) ->
	Wall = create_wall(), % создать стену
	Ships = [create_ship(Type) || Type <- [4, 3, 3, 2, 2, 2, 1, 1, 1, 1]], % создать базовый набор кораблей
	#player{name = Name, wall = Wall, ships = Ships}.  % создать структуру игрока

% Создаёт стену по-умолчанию.
create_wall() ->
	#wall{}.

% Создаёт корабль заданого типа.
create_ship(Type) ->
	#ship{type = Type}.