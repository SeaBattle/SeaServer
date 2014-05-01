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
-export([create_login/3, create_player/3, save_ships/1, compile_player/3, create_player/1, update_player/3, update_wall/2, get_player/1, get_wall/1]).

% TODO get на стену и корабли
% создаёт запись в корзине Логины. Ищет и использует предшествующую гостевую запись, если была.
create_login(Login, Password, Uid) ->
	case ss_db_sup:get(?DB_POOL, ?PLAYERS, Uid) of
		{ok, _} -> % гостевая запись найдена - новую не создаём - используем текущую
			LoginObj = riakc_obj:new(?LOGINS, Login, {Password, Uid}),  %TODO put with return_body
			case ss_db_sup:put(?DB_POOL, LoginObj) of
				{ok, _} -> used;
				{error, _} ->
					io:format("~w can't save ~w to Logins!~n", [?MODULE, Login]),
					error
			end;
		{error, notfound} -> % запись не найдена - создаём новую
			LoginObj = riakc_obj:new(?LOGINS, Login, {Password}),  %TODO put with return_body ss_db_sup:put(?DB_POOL, NewObj, [return_body])
			case ss_db_sup:put(?DB_POOL, LoginObj) of
				{ok, _} -> created;
				{error, _} ->
					io:format("~w can't save ~w to Logins!~n", [?MODULE, Login]),
					error
			end
	end.

% Возвращает структуру игрока. Игрок точно должен быть в базе.
get_player(Key) ->
	{ok, Object} = ss_db_sup:get(?DB_POOL, ?PLAYERS, Key),
	binary_to_term(riakc_obj:get_value(Object)).
% Возвращает структуру стены. Стена точно должна быть в базе.
get_wall(Key) ->
	{ok, Object} = ss_db_sup:get(?DB_POOL, ?WALLS, Key),
	binary_to_term(riakc_obj:get_value(Object)).

% Создаёт запись игрока + стену и базовые корабли.
create_player(Name) ->
	Wall = create_wall(), % создать стену
	Ships = [create_ship(Type) || Type <- [4, 3, 3, 2, 2, 2, 1, 1, 1, 1]], % создать базовый набор кораблей
	#player{name = Name, wall = Wall, ships = Ships, icon = "default"}.  % создать структуру игрока %TODO сделать стандартную иконку на случай, если игрок не задал свою
create_player(Name, Motto, Icon) ->
	Wall = create_wall(), % создать стену
	Ships = [create_ship(Type) || Type <- [4, 3, 3, 2, 2, 2, 1, 1, 1, 1]], % создать базовый набор кораблей
	#player{name = Name, wall = Wall#wall{motto = Motto}, ships = Ships, icon = Icon}.

% Обновляет запись игрока, устанавливает новое имя.
update_player(Key, [], []) -> {ok, get_player(Key)};  % обновлять не нужно, просто вернуть
update_player(Key, Name, Icon) ->
	{ok, Object} = ss_db_sup:get(?DB_POOL, ?PLAYERS, Key),
	Player = binary_to_term(riakc_obj:get_value(Object)),
	if
		Player#player.name == Name andalso Player#player.icon == Icon ->
			{ok, Player}; % данные совпадают - обновлять не нужно
		true ->
			UpIcon = if Icon == [] -> Player#player.icon; % если иконка пустая - оставляем иконку по-умолчанию
				         true -> Icon
			         end,
			UpName = if Name == [] -> Player#player.name; % если имя пустое - оставляем имя по-умолчанию
				         true -> Name
			         end,
			UpdatedPlayer = Player#player{name = UpName, icon = UpIcon}, % установить новые данные и сохранить объект
			NewObj = riakc_obj:update_value(Object, UpdatedPlayer),
			{ok, _} = ss_db_sup:put(?DB_POOL, NewObj),
			{ok, Player}
	end.

% Создаёт стену по-умолчанию.
create_wall() ->
	{_, _, Timestamp} = os:timestamp(),
	#wall{created = Timestamp}.

% Обновляет стену
update_wall(Key, []) -> {ok, get_wall(Key)};  % обновлять не нужно, просто вернуть
update_wall(Key, Motto) ->
	{ok, Object} = ss_db_sup:get(?DB_POOL, ?WALLS, Key),
	Wall = binary_to_term(riakc_obj:get_value(Object)),
	if
		Wall#wall.motto == Motto -> {ok, Wall};
		true ->
			UpdatedWall = Wall#wall{motto = Motto}, % установить новые данные и сохранить объект
			NewObj = riakc_obj:update_value(Object, UpdatedWall),
			{ok, _} = ss_db_sup:put(?DB_POOL, NewObj),
			{ok, Wall}
	end
.

% Создаёт корабль заданого типа.
create_ship(Type) ->
	#ship{type = Type}.

% Синхронно сохраняет корабли в БД, возвращает ключи, по которым были сохранены корабли
save_ships(Ships) ->
	F = fun(Ship) ->
		ShipObj = riakc_obj:new(?SHIPS, undefined, Ship),
		case ss_db_sup:put(?DB_POOL, ShipObj) of
			{ok, Key} -> Key;
			{error, _} -> []
		end
	end,
	[F(S) || S <- Ships].

% Компилирует игрока в riak-объек, прикрепляет индексы кораблей вторичными индексами в его метаданные
compile_player(Player, Ships, Key) ->
	% бд-объект не сохраняет стену и корабли, т.к. стена будет сохранена под тем же ключом в другой корзине,
	% а корабли будут сохранены отдельно и слинкованы с этим объектом посредством вторичных индексов
	PlayerObj = riakc_obj:new(?PLAYERS, Key, Player#player{wall = undefined, ships = undefined}),
	% слинковать вторичными индексами сохранённые корабли и бд-объект игрока
	Meta = riakc_obj:get_update_metadata(PlayerObj),
	LinkedMeta = riakc_obj:set_secondary_index(Meta, [{{binary_index, "ships"}, Ships}]),
	% успешность создания пользователя игнорируется, т.к. гостю не важно, сохранили его или нет
	riakc_obj:update_metadata(PlayerObj, LinkedMeta).