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
	socket,  % сокет
	player  % игрок
}).

% Учётная запись в базе данных. Сохраняется по ключу Login
-record(login,
{
	uid,      % uid пользователя, на случай, если сначала пользователь играл гостем (чтобы использовать старую учётку)
	password  % пароль пользователя
}).

%% Типы бана:
%% no_ban   нет бана
%% chat_ban   пользователь не может общаться в чате
%% wall_ban   пользователь не может писать и комментировать на стене (включая выставление рейтинга другому пользователю или высказываниям)
%% privat_ban пользователь не может писать личные сообщения
%% social_ban пользователь не может отправлять заявки на дружбу
%% login_ban  пользователю запрещено заходить на сервер

% Запись в базе данных - игрок
-record(player,
{
	level = 0,  % уровень игрока
	ban_type = no_ban, % тип бана
	ban_end,  % дата окончания бана
	name,   % имя игрока
	icon,   % иконка игрока
	wall,   % стена игрока, сохраняется в бд, как отдельная структура
	ships = []   % флот игрока, сохраняется в бд, как отдельная структура
}).

% Стена игрока
-record(wall,
{
	motto,    % девиз игрока
	rewards = [],  % награды
	created,    % когда была создана запись в БД
	messages, % сообщения на стене
	rate = 0,   % рейтинг игрока, голосуют другие игроки
	rate_voted = [],   % проголосовавшие игроки
	statistics = []  % статистика игроков по правилам
}).

% Сообщение на стене
-record(wall_message,
{
	parent_msg, % родительское сообщение (если это сообщение - ответ)
	child_msg,  % дочернее сообщение (если есть ответ на это сообщение) % TODO подумать, нужно ли это
	sender, % отправитель
	text, % текст
	rate, % рейтинг сообщения, голосуют другие игроки
	rate_voted, % проголосовавшие игроки
	add_date  % дата добавления
}).

% Личное сообщение
-record(personal_message,
{
	sender, % отправитель
	receiver, % получатель
	text  % текст
}).

% Игра, которая сохраняется, если делается перерыв
-record(db_game,
{
	players,  % игроки
	active_player,  % игрок, который ходит
	rules,    % правила
	field     % поле с кораблями и выстрелами
}).

% Корабль
-record(ship,
{
	experience = 0, % опыт корабля
	type,   % тип корабля
	level = 1,  % уровень корабля
	repair_date, % дата последнего ремонта
	name    % название корабля (не обязательно)
}).