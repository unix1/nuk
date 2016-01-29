%%%-------------------------------------------------------------------
%% @doc `nuk_game_engine_state' module
%%
%% This module is used to operate on {@link nuk_game_engine_state:state()} data
%% type. This data type is used when retrieving game engine's state from
%% {@link nuk_game_session:get_nuk_state/1}.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_engine_state).

%% API
-export([new/3]).
-export([get_all/1]).
-export([get_private/1]).
-export([get_public/1]).
-export([get_player/2]).
-export([get_players/1]).
-export([set_all/4]).
-export([set_private/2]).
-export([set_public/2]).
-export([put_player/3]).
-export([remove_player/2]).
-export([set_player/3]).
-export([set_players/2]).

-export_type([state/0]).

-opaque state() :: #{private => term(),
                     public => term(),
                     players => map()}.
%% Data type containing game engine's game state. This is part of
%% {@link nuk_game_session:session()} data type. Functions in this module can
%% be used to operate on the following data:
%% - `private': part of the state that stays private to the game engine; it is
%%   never shared with any players
%% - `public': part of the state that is public - it is shared with all players
%% - `players': a map of players usernames to the data that will be shared to
%%   those specific players only
%% All above data is of type `term()' - i.e. it's up to the game engine

%% @doc Create new a new {@link state()} data type
%%
%% Creates a new state with specified values.
%% @end
-spec new(Private :: term(), Public :: term(), Players :: map()) ->
    state().
new(Private, Public, Players) ->
    #{private => Private, public => Public, players => Players}.

%% @doc Get all components of engine state
%%
%% Returns a list containing all - private, public and players - states.
%% @end
-spec get_all(State :: state()) -> list().
get_all(#{private := Private, public := Public, players := Players}) ->
    [Private, Public, Players].

%% @doc Get private state
%%
%% Gets game engine's private state.
%% @end
-spec get_private(State :: state()) -> term().
get_private(#{private := Private}) ->
    Private.

%% @doc Get public state
%%
%% Gets game engine's public state.
%% @end
-spec get_public(State :: state()) -> term().
get_public(#{public := Public}) ->
    Public.

%% @doc Get player specific state
%%
%% Gets state specific to the given player.
%% @end
-spec get_player(State :: state(), Username :: string()) -> term().
get_player(State, Username) ->
    Players = get_players(State),
    maps:get(Username, Players).

%% @doc Get a map of all player states
%%
%% Gets a map of player states with player usernames as keys.
%% @end
-spec get_players(State :: state()) -> map().
get_players(#{players := Players}) ->
    Players.

%% @doc Set all components of the engine state
%%
%% Sets all - private, public and players - states
%% @end
-spec set_all(State :: state(), Private :: term(), Public :: term(),
              Players :: map()) -> state().
set_all(State, Private, Public, Players) ->
    State#{private := Private, public := Public, players := Players}.

%% @doc Set private state
%%
%% Sets game engine private session state.
%% @end
-spec set_private(State :: state(), Private :: term()) -> state().
set_private(State, Private) ->
    State#{private := Private}.

%% @doc Set public state
%%
%% Sets game engine public session state.
%% @end
-spec set_public(State :: state(), Public :: term()) -> state().
set_public(State, Public) ->
    State#{public := Public}.

%% @doc Put a state for a new or existing player
%%
%% Sets a state for a specific player; if the Username doesn't exist, it is
%% added; if it exists its data is overwritten.
%% @end
-spec put_player(State :: state(), Username :: string(), Player :: term()) ->
    state().
put_player(#{players := Players} = State, Username, Player) ->
    State#{players := Players#{Username => Player}}.

%% @doc Remove player from players state
%%
%% Completely removes a player from the map of player states
%% @end
-spec remove_player(State :: state(), Username :: string()) -> state().
remove_player(#{players := Players}, Username) ->
    maps:remove(Username, Players).

%% @doc Set a state for an existing player
%%
%% Sets a state for a specific existing player.
%% @end
-spec set_player(State :: state(), Username :: string(), Player :: term()) ->
    state().
set_player(#{players := Players} = State, Username, Player) ->
    State#{players := Players#{Username := Player}}.

%% @doc Set all players state
%%
%% Sets a map of states for all players.
%% @end
-spec set_players(State :: state(), Players :: map()) -> state().
set_players(State, Players) ->
    State#{players := Players}.
