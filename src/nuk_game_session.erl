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
-export([get_nuk_state/1]).
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

-opaque session() :: #{game => nuk_game:game(),
                       nuk_state => nuk_game_state:state(),
                       game_state => term()}.
%% Data type used to represent a game session state. Use functions in this
%% module to operate on this data type. It contains the following:
%% - `game': {@link nuk_game:game()} data type, use {@link get_game/1} to
%%   extract
%% - `nuk_state': {@link nuk_game_state:state()} data type, use functions in
%%   this module to extract specific values from this state
%% - `game_state': an arbitrary term that stores the game engine specific
%%   state, use functions provided by the respective game engine to extract
%%   information from this data type

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

%% @doc Get general nuk game state
%%
%% Returns the general nuk state in the form of {@link nuk_game_state:state()}
%% data type. Use {@link nuk_game_state} module to operate on this data type.
%% @end
-spec get_nuk_state(Session :: session()) -> nuk_game_state:state().
get_nuk_state(#{nuk_state := NukState}) ->
    NukState.

%% @doc Get players currently in the game session
%%
%% Returns a list of {@link nuk_user:user()} data types that represent a list
%% of players currently joined to this game session.
%% @end
-spec get_players(Session :: session()) -> [nuk_user:user()].
get_players(#{nuk_state := NukState}) ->
    nuk_game_state:get_players(NukState).

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
get_players_turn(#{nuk_state := NukState}) ->
    nuk_game_state:get_players_turn(NukState).

%% @doc Get game session status
%%
%% Returns an atom status of the game session
%% @end
-spec get_status(Session :: session()) -> nuk_game_state:status().
get_status(#{nuk_state := NukState}) ->
    nuk_game_state:get_status(NukState).

%% @doc Get turn number
%%
%% Every time any player makes a turn nuk increments an internal turn counter.
%% This returns the current turn number from the game session.
%% @end
-spec get_turn_number(Session :: session()) -> non_neg_integer().
get_turn_number(#{nuk_state := NukState}) ->
    nuk_game_state:get_turn_number(NukState).

%% @doc Get winners and losers lists
%%
%% Returns two lists of {@link nuk_user:user()} data types that represent a
%% list of players who have won and who have lost the game. This is only
%% relevant once the game has completed. In all other cases these lists will
%% be empty.
%% @end
-spec get_winners_losers(Session :: session()) ->
    {Winners :: [nuk_user:user()], Losers :: [nuk_user:user()]}.
get_winners_losers(#{nuk_state := NukState}) ->
    nuk_game_state:get_winners_losers(NukState).

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
add_player(#{nuk_state := NukState} = Session, Player) ->
    Players = nuk_game_state:get_players(NukState),
    NewPlayers = [Player|Players],
    Session#{nuk_state := nuk_game_state:set_players(NukState, NewPlayers)}.

%% @doc Remove a player from the game session
%%
%% Whenver a player leaves a game, nuk uses this function to remove that player
%% from the game session.
%% @end
-spec remove_player(Session :: session(), Player :: nuk_user:user()) ->
    session().
remove_player(#{nuk_state := NukState} = Session, Player) ->
    Players = nuk_game_state:get_players(NukState),
    NewPlayers = lists:delete(Player, Players),
    Session#{nuk_state := nuk_game_state:set_players(NukState, NewPlayers)}.

%% @doc Set a list of players to the current game session
%%
%% nuk uses this function to set the initial players to the game session.
%% @end
-spec set_players(Session :: session(), Players :: [nuk_user:user()]) ->
    session().
set_players(#{nuk_state := NukState} = Session, Players)
        when is_list(Players) ->
    Session#{nuk_state := nuk_game_state:set_players(NukState, Players)}.

%% @doc Set players who's turn it is next
%%
%% In cases where {@link nuk_game_engine} callback returns the players who's
%% turn it is next, nuk uses this function to update them in its session state.
%% @end
-spec set_players_turn(Session :: session(), Players :: [nuk_user:user()]) ->
    session().
set_players_turn(#{nuk_state := NukState} = Session, Players)
        when is_list(Players) ->
    Session#{nuk_state := nuk_game_state:set_players_turn(NukState, Players)}.

%% @doc Set game session status
%%
%% nuk uses this function to update the general game status.
%% @end
-spec set_status(Session:: session(), Status :: nuk_game_state:status()) ->
    session().
set_status(#{nuk_state := NukState} = Session, Status) when is_atom(Status) ->
    Session#{nuk_state := nuk_game_state:set_status(NukState, Status)}.

%% @doc Set turn number
%%
%% Sets turn number to a specified integer.
%% @end
-spec set_turn_number(Session :: session(), TurnNumber :: non_neg_integer()) ->
    session().
set_turn_number(#{nuk_state := NukState} = Session, TurnNumber)
        when is_integer(TurnNumber) ->
    Session#{nuk_state := nuk_game_state:set_turn_number(NukState, TurnNumber)}.

%% @doc Set winners and losers
%%
%% nuk uses this function to set winners and losers in the game session. This
%% is used once the game has completed.
%% @end
-spec set_winners_losers(Session :: session(),
                         Winners :: [nuk_user:user()],
                         Losers :: [nuk_user:user()]) ->
    session().
set_winners_losers(#{nuk_state := NukState} = Session, Winners, Losers) ->
    Session#{nuk_state := nuk_game_state:set_winners_losers(NukState,
                                                            Winners,
                                                            Losers)}.
