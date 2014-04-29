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
	{Player, PlayerObj, WallObj, Ships} = create_player(Uid, "Guest"),

	% синхронно сохранить все корабли и получить сгенерированные ключи
	Keys = [fun({_, Ship} = S) -> ss_db_sup:put(?DB_POOL, Ship) end || S <- Ships],
	io:format("Saved ships, got keys: ~w~n", [Keys]),

	% слинковать вторичными индексами сохранённые корабли и бд-объект игрока
	Meta = riakc_obj:get_update_metadata(PlayerObj),
	LinkedMeta = riakc_obj:set_secondary_index(Meta, [{{binary_index, "ships"}, Keys}]),

	% успешность создания пользователя игнорируется, т.к. гостю не важно, сохранили его или нет
	ss_db_sup:put_async(?DB_POOL, riakc_obj:update_metadata(PlayerObj, LinkedMeta)),
	ss_db_sup:put_async(?DB_POOL, WallObj),
	Player.

% Создаёт запись игрока + стену и базовые корабли.
create_player(Login, Name) ->
	{Wall, WallObj} = create_wall(Login), % создать стену

	ShipsTupls = [create_ship(Type) || Type <- [4, 3, 3, 2, 2, 2, 1, 1, 1, 1]], % создать базовый набор кораблей

	Player = #player{name = Name, wall = Wall, ships = proplists:get_keys(ShipsTupls)},  % создать структуру игрока

	io:format("Created player: ~w~n", [Player]),
	% бд-объект не сохраняет стену и корабли, т.к. стена будет сохранена под тем же ключом в другой корзине,
	% а корабли будут сохранены отдельно и слинкованы с этим объектом посредством вторичных индексов
	{Player, riakc_obj:new(?PLAYERS, Login, Player#player{wall = undefined, ships = undefined}), WallObj, ShipsTupls}.

% Создаёт стену по-умолчанию.
create_wall(Login) ->
	Wall = #wall{},
	io:format("Created wall: ~w~n", [Wall]),
	{Wall, riakc_obj:new(?WALLS, Login, Wall)}.

% Создаёт корабль заданого типа.
create_ship(Type) ->
	Ship = #ship{type = Type},
	io:format("Created ship: ~w~n", [Ship]),
	{Ship, riakc_obj:new(?SHIPS, undefined, Ship)}.