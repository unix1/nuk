%%%-------------------------------------------------------------------
%% @doc `nuk_game_state' module
%%
%% This module is used to operate on {@link nuk_game_state:state()} data type.
%% This data type is used when retrieving nuk's general game session state from
%% {@link nuk_game_session:get_nuk_state/1}.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_state).

%% API
-export([new/0]).
-export([get_players/1]).
-export([get_players_turn/1]).
-export([get_status/1]).
-export([get_turn_number/1]).
-export([get_winners_losers/1]).
-export([set_players/2]).
-export([set_players_turn/2]).
-export([set_status/2]).
-export([set_turn_number/2]).
-export([set_winners_losers/3]).

-export_type([state/0]).
-export_type([status/0]).

-type status() :: nil | initialized | await_turn | complete.
%% General game session status tracked by nuk.

-opaque state() :: #{status => status(),
                     turn_number => integer(),
                     players => [nuk_user:user()],
                     players_turn => [nuk_user:user()],
                     players_winners => [nuk_user:user()],
                     players_losers => [nuk_user:user()]}.
%% Data type containing nuk's general game state. This is part of
%% {@link nuk_game_session:session()} data type. Functions in this module can
%% be used to operate on the following data:
%% - `status': game session status, default `nil', use {@link get_status/1} to
%%   extract
%% - `turn_number': current turn number, default `0', use
%%   {@link get_turn_number/1} to extract
%% - `players': list of players currently in the game session, default `[]',
%%   use {@link get_players/1} to extract
%% - `players_turn': list of players who should make turn(s) next,
%%   default `[]', use {@link get_players_turn/1} to extract
%% - `players_winners': list of players who won the game, only populated
%%   after the game completes, default `[]', use {@link get_winners_losers/1}
%%   to extract
%% - `players_losers': list of players who lost the game, only populated
%%   after the game completes, default `[]', use {@link get_winners_losers/1}
%%   to extract

%% @doc Create a new {@link state()} data type.
%%
%% Creates a new state with default values.
%% @end
-spec new() -> state().
new() ->
    #{status => nil,
      turn_number => 0,
      players => [],
      players_turn => [],
      players_winners => [],
      players_losers => []}.

%% @doc Get players currently in the game session
%%
%% Returns a list of {@link nuk_user:user()} data types that represent a list
%% of players currently joined to this game session.
%% @end
-spec get_players(State :: state()) -> [nuk_user:user()].
get_players(#{players := Players}) ->
    Players.

%% @doc Get players who's turn it is next
%%
%% Returns a list of {@link nuk_user:user()} data types that represent a list
%% of players who the game engine is expecting to make the turn(s) next. i.e.
%% the answer to "who's turn is it?" question.
%% @end
-spec get_players_turn(State :: state()) -> [nuk_user:user()].
get_players_turn(#{players_turn := PlayersTurn}) ->
    PlayersTurn.

%% @doc Get game session status
%%
%% Returns an atom status of the game session.
%% @end
-spec get_status(State :: state()) -> status().
get_status(#{status := Status}) ->
    Status.

%% @doc Get turn number
%%
%% Every time any player makes a turn nuk increments an internal turn counter.
%% This returns the current turn number from the game session.
%% @end
-spec get_turn_number(State :: state()) -> non_neg_integer().
get_turn_number(#{turn_number := TurnNumber}) ->
    TurnNumber.

%% @doc Get winners and losers lists
%%
%% Returns two lists of {@link nuk_user:user()} data types that represent a
%% list of players who have won and who have lost the game. This is only
%% relevant once the game has completed. In all other cases these lists are
%% likely to be empty.
%% @end
-spec get_winners_losers(State :: state()) ->
    {Winners :: [nuk_user:user()], Losers :: [nuk_user:user()]}.
get_winners_losers(#{players_winners := Winners, players_losers := Losers}) ->
    {Winners, Losers}.

%% @doc Set a list of players to the current game session
%%
%% Useful for setting initial set of players for the game session.
%% @end
-spec set_players(State :: state(), Players :: [nuk_user:user()]) ->
    state().
set_players(State, Players) when is_list(Players) ->
    State#{players := Players}.

%% @doc Set players who's turn it is next
%%
%% Sets a new list of players, replacing an existing list.
%% @end
-spec set_players_turn(State :: state(), Players :: [nuk_user:user()]) ->
    state().
set_players_turn(State, Players) when is_list(Players) ->
    State#{players_turn := Players}.

%% @doc Set status
%%
%% Sets a new status in the game state.
%% @end
-spec set_status(State :: state(), Status :: status()) ->
    state().
set_status(State, Status) when is_atom(Status) ->
    State#{status := Status}.

%% @doc Set turn number
%%
%% Sets turn number to a specified integer.
%% @end
-spec set_turn_number(State:: state(), TurnNumber :: non_neg_integer()) ->
    state().
set_turn_number(State, TurnNumber) when is_integer(TurnNumber) ->
    State#{turn_number := TurnNumber}.

%% @doc Set winners and losers
%%
%% This is typically used once the game has completed.
%% @end
-spec set_winners_losers(State :: state(),
                         Winners :: [nuk_user:user()],
                         Losers :: [nuk_user:user()]) ->
    state().
set_winners_losers(State, Winners, Losers) ->
    State#{players_winners := Winners, players_losers := Losers}.
