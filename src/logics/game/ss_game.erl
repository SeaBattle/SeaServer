%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Aug 2016 16:18
%%%-------------------------------------------------------------------
-module(ss_game).
-author("tihon").

-behaviour(gen_fsm).

%% API
-export([start_link/1, send_ships/2]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-export(
[
  prepare/2, prepare/3  %two players should send their ship packages.
]).

-define(SERVER, ?MODULE).

-record(state,
{
  game_id :: binary(),
  player1 :: {pid(), binary()},
  player2 :: {pid(), binary()},
  fleet_player1 = [] :: map(),
  fleet_player2 = [] :: map(),
  active = 1 :: pos_integer(),
  rules :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================
send_ships(Gid, Ships) ->
  gen_fsm:sync_send_event(Gid, {ships, Ships}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(list()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(Args = [GID | _]) -> %TODO gid is not atom. Register me in syn.
  gen_fsm:start_link({global, GID}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #state{}} |
  {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([GID, UID1, UID2, Rules]) ->
  Pid1 = ss_utils:uid_to_pid(UID1), %TODO use syn instead
  Pid2 = ss_utils:uid_to_pid(UID2),
  monitor(process, Pid1),
  monitor(process, Pid2),
  RulesMap = ss_game_rules:decode_rules(Rules),
  {ok, prepare, #state{game_id = GID, player1 = {Pid1, UID1}, player2 = {Pid2, UID2}, rules = RulesMap}}.

prepare(_Event, State) ->
  {next_state, prepare, State}.

prepare({ships, Ships}, _From, State = #state{rules = Rules, player1 = {_P1, _}}) ->
  set_flit_to_player(_From, State),
  Reply = case ss_ship_logic:place_ships(Ships, Rules) of %TODO catch
            Fleet1 when is_list(Fleet1) -> ok;
            Error -> {false, Error}
          end,  %TODO change state if all fleets are set
  {reply, Reply, prepare, State#state{}};
prepare(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #state{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
set_flit_to_player(SenderPid, #state{player1 = {SenderPid, _}}) -> fleet_player1;
set_flit_to_player(SenderPid, #state{player2 = {SenderPid, _}}) -> fleet_player2.