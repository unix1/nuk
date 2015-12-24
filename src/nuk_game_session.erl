%%%-------------------------------------------------------------------
%% @doc `nuk_game_session' module
%%
%% This module is used to operate on {@link nuk_game_session:session()} data
%% type. This data type is used when retrieving the game session state from
%% {@link nuk_games:get_game_session/1}. It tracks the following data:
%% - Game {@link nuk_game:game()} which this session is for
%% - nuk's general game session state
%% - Game engine's arbitrary state
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_session).

%% API
-export([new/1]).
-export([get_game/1]).
-export([get_game_state/1]).
-export([get_players/1]).
-export([get_players_count/1]).
-export([get_players_turn/1]).
-export([get_status/1]).
-export([get_turn_number/1]).
-export([get_winners_losers/1]).
-export([has_player/2]).
-export([is_players_turn/2]).
-export([increment_turn_number/1]).
-export([set_game_state/2]).
-export([add_player/2]).
-export([remove_player/2]).
-export([set_players/2]).
-export([set_players_turn/2]).
-export([set_status/2]).
-export([set_turn_number/2]).
-export([set_winners_losers/3]).

%% Types
-export_type([session/0]).
-export_type([nuk_state/0]).
-export_type([status/0]).

-opaque session() :: #{game => nuk_game:game(),
                       nuk_state => nuk_state(),
                       game_state => term()}.
%% Data type used to represent a game session state. Use functions in this
%% module to operate on this data type. It contains the following:
%% - `game': {@link nuk_game:game()} data type, use {@link get_game/1} to
%%   extract
%% - `nuk_state': {@link nuk_state()} data type, use functions in this module
%%   to extract specific values from this state
%% - `game_state': an arbitrary term that stores the game engine specific
%%   state, use functions provided by the respective game engine to extract
%%   information from this data type

-opaque nuk_state() :: #{status => status(),
                         turn_number => integer(),
                         players => [nuk_user:user()],
                         players_turn => [nuk_user:user()],
                         players_winners => [nuk_user:user()],
                         players_losers => [nuk_user:user()]}.
%% Data type containing nuk's general game state. This is part of
%% {@link session()} data type. Functions in this module can be used to extract
%% following data that's contained in this data type directly from game
%% session:
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

-type status() :: nil | initialized | await_turn | complete.
%% General game session status tracked by nuk.

%%====================================================================
%% API
%%====================================================================

%% @doc Create a new {@link session()} data type.
%%
%% `Game' is a {@link nuk_game:game()} data type which is stored inside the
%% session. All other values are set to their defaults. For default values see
%% the top description of this module.
%% @end
-spec new(Game :: nuk_game:game()) -> session().
new(Game) ->
    #{game => Game,
      nuk_state => #{status => nil,
                     turn_number => 0,
                     players => [],
                     players_turn => [],
                     players_winners => [],
                     players_losers => []},
      game_state => nil}.

%% @doc Get game
%%
%% Returns the {@link nuk_game:game()} data type to which this session belongs.
%% @end
-spec get_game(Session :: session()) -> nuk_game:game().
get_game(#{game := Game}) ->
    Game.

%% @doc Get game engine arbitrary state
%%
%% Returns an arbitrary game state set by the game engine. Since this value
%% is specific to a specific game engine, it is the responsibility of the
%% respective game engine to provide functions to extract information
%% from this value.
%% @end
-spec get_game_state(Session :: session()) -> term().
get_game_state(#{game_state := GameState}) ->
    GameState.

%% @doc Get players currently in the game session
%%
%% Returns a list of {@link nuk_user:user()} data types that represent a list
%% of players currently joined to this game session.
%% @end
-spec get_players(Session :: session()) -> [nuk_user:user()].
get_players(Session) ->
    NukState = get_state(Session),
    #{players := Players} = NukState,
    Players.

%% @doc Get number of players currently in the game session
%%
%% Returns number of players currently in this game session.
%% @end
-spec get_players_count(Session :: session()) -> non_neg_integer().
get_players_count(Session) ->
    length(get_players(Session)).

%% @doc Get players who's turn it is next
%%
%% Returns a list of {@link nuk_user:user()} data types that represent a list
%% of players who the game engine is expecting to make the turn(s) next. i.e.
%% the answer to "who's turn is it?" question.
%% @end
-spec get_players_turn(Session :: session()) -> [nuk_user:user()].
get_players_turn(Session) ->
    NukState = get_state(Session),
    #{players_turn := PlayersTurn} = NukState,
    PlayersTurn.

%% @doc Get game session status
%%
%% Returns an atom status of the game session
%% @end
-spec get_status(Session :: session()) -> status().
get_status(Session) ->
    NukState = get_state(Session),
    #{status := Status} = NukState,
    Status.

%% @doc Get turn number
%%
%% Every time any player makes a turn nuk increments an internal turn counter.
%% This returns the current turn number from the game session.
%% @end
-spec get_turn_number(Session :: session()) -> non_neg_integer().
get_turn_number(Session) ->
    NukState = get_state(Session),
    #{turn_number := TurnNumber} = NukState,
    TurnNumber.

%% @doc Get winners and losers lists
%%
%% Returns two lists of {@link nuk_user:user()} data types that represent a
%% list of players who have won and who have lost the game. This is only
%% relevant once the game has completed. In all other cases these lists will
%% be empty.
%% @end
-spec get_winners_losers(Session :: session()) ->
    {Winners :: [nuk_user:user()], Losers :: [nuk_user:user()]}.
get_winners_losers(Session) ->
    NukState = get_state(Session),
    #{players_winners := Winners, players_losers := Losers} = NukState,
    {Winners, Losers}.

%% @doc Is a player a member of this game session?
%%
%% This is useful for checking whether a given user is a player in this game
%% session.
%% @end
-spec has_player(Session :: session(), Player :: nuk_user:user()) -> boolean().
has_player(Session, Player) ->
    Players = get_players(Session),
    %% TODO should this check by username instead?
    lists:member(Player, Players).

%% @doc Is it a given player's turn?
%%
%% This is useful to checking whether it is OK for a given player to make a
%% turn.
%% @end
-spec is_players_turn(Session :: session(), Player :: nuk_user:user()) ->
    boolean().
is_players_turn(Session, Player) ->
    Players = get_players_turn(Session),
    %% TODO should this check by username instead?
    lists:member(Player, Players).

%% @doc Increments the internal turn number
%%
%% nuk uses an internal turn counter every time any player makes a turn. This
%% is used to increment that turn counter.
%% @end
-spec increment_turn_number(Session :: session()) -> session().
increment_turn_number(Session) ->
    TurnNumberNew = get_turn_number(Session) + 1,
    set_turn_number(Session, TurnNumberNew).

%% @doc Sets game engine state in the session
%%
%% With every successful {@link nuk_game_engine} callback the game engine
%% returns its new state. This function is used to then store that state in the
%% game session.
%% @end
-spec set_game_state(Session :: session(), GameState :: term()) -> session().
set_game_state(Session, GameState) ->
    Session#{game_state := GameState}.

%% @doc Add a player to the game session
%%
%% Whenver a players joins a game, nuk uses this function to add that player to
%% the game session.
%% @end
-spec add_player(Session :: session(), Player :: nuk_user:user()) -> session().
add_player(Session, Player) ->
    NukState = get_state(Session),
    NewNukState = nuk_state_add_player(NukState, Player),
    Session#{nuk_state := NewNukState}.

%% @doc Remove a player from the game session
%%
%% Whenver a player leaves a game, nuk uses this function to remove that player
%% from the game session.
%% @end
-spec remove_player(Session :: session(), Player :: nuk_user:user()) ->
    session().
remove_player(Session, Player) ->
    NukState = get_state(Session),
    NewNukState = nuk_state_remove_player(NukState, Player),
    Session#{nuk_state := NewNukState}.

%% @doc Set a list of players to the current game session
%%
%% nuk uses this function to set the initial players to the game session.
%% @end
-spec set_players(Session :: session(), Players :: [nuk_user:user()]) ->
    session().
set_players(Session, Players) when is_list(Players) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{players := Players}}.

%% @doc Set players who's turn it is next
%%
%% In cases where {@link nuk_game_engine} callback returns the players who's
%% turn it is next, nuk uses this function to update them in its session state.
%% @end
-spec set_players_turn(Session :: session(), Players :: [nuk_user:user()]) ->
    session().
set_players_turn(Session, Players) when is_list(Players) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{players_turn := Players}}.

%% @doc Set game session status
%%
%% nuk uses this function to update the general game status.
%% @end
-spec set_status(Session:: session(), Status :: status()) ->
    session().
set_status(Session, Status) when is_atom(Status) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{status := Status}}.

%% @doc Set turn number
%%
%% Sets turn number to a specified integer.
%% @end
-spec set_turn_number(Session:: session(), TurnNumber :: non_neg_integer()) ->
    session().
set_turn_number(Session, TurnNumber) when is_integer(TurnNumber) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{turn_number := TurnNumber}}.

%% @doc Set winners and losers
%%
%% nuk uses this function to set winners and losers in the game session. This
%% is used once the game has completed.
%% @end
-spec set_winners_losers(Session :: session(),
                  Winners :: [nuk_user:user()],
                  Losers :: [nuk_user:user()]) -> session().
set_winners_losers(Session, Winners, Losers) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{players_winners := Winners,
                                    players_losers := Losers}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Extract the general nuk state from game session
%% @private
%%
%% This is a convenience function to extract the
%% {@link nuk_game_session:nuk_state()} from the current session.
%% @end
-spec get_state(Session :: session()) -> nuk_state().
get_state(#{nuk_state := NukState}) ->
    NukState.

%% @doc Add a player to nuk state
%% @private
%%
%% This is a convenience function to add a player to the
%% {@link nuk_game_session:nuk_state()}.
%% @end
-spec nuk_state_add_player(NukState :: nuk_state(), Player :: nuk_user:user()) ->
    nuk_state().
nuk_state_add_player(#{players := Players} = NukState, Player) ->
    NukState#{players := [Player|Players]}.

%% @doc Remove player from nuk state
%% @private
%%
%% This is a convenience function to remove a player from the
%% {@link nuk_game_session:nuk_state()}.
%% @end
-spec nuk_state_remove_player(NukState :: nuk_state(), Player :: nuk_user:user()) ->
    nuk_state().
nuk_state_remove_player(#{players := Players} = NukState, Player) ->
    %% TODO should this check by username instead?
    NukState#{players := lists:delete(Player, Players)}.
