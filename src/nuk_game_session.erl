%%%-------------------------------------------------------------------
%% @doc nuk game session
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
-export([has_player/2]).
-export([increment_turn_number/1]).
-export([set_game_state/2]).
-export([add_player/2]).
-export([set_players/2]).
-export([set_players_turn/2]).
-export([set_status/2]).
-export([set_turn_number/2]).
-export([set_winners_losers/3]).

%% Types
-export_type([session/0]).
-export_type([nuk_state/0]).

-opaque session() :: #{game => nuk_game:game(),
                       nuk_state => nuk_state(),
                       game_state => term()}.

-opaque nuk_state() :: #{status => atom(),
                         turn_number => integer(),
                         players => [nuk_user:user()],
                         players_turn => [nuk_user:user()],
                         players_winners => [nuk_user:user()],
                         players_losers => [nuk_user:user()]}.

%%====================================================================
%% API
%%====================================================================

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

-spec get_game(Session :: session()) -> nuk_game:game().
get_game(#{game := Game}) ->
    Game.

-spec get_game_state(Session :: session()) -> term().
get_game_state(#{game_state := GameState}) ->
    GameState.

-spec get_players(Session :: session()) -> [nuk_user:user()].
get_players(Session) ->
    NukState = get_state(Session),
    #{players := Players} = NukState,
    Players.

-spec get_players_count(Session :: session()) -> integer().
get_players_count(Session) ->
    length(get_players(Session)).

-spec get_players_turn(Session :: session()) -> [nuk_user:user()].
get_players_turn(Session) ->
    NukState = get_state(Session),
    #{players_turn := PlayersTurn} = NukState,
    PlayersTurn.

-spec get_status(Session :: session()) -> atom().
get_status(Session) ->
    NukState = get_state(Session),
    #{status := Status} = NukState,
    Status.

-spec get_turn_number(Session :: session()) -> integer().
get_turn_number(Session) ->
    NukState = get_state(Session),
    #{turn_number := TurnNumber} = NukState,
    TurnNumber.

-spec has_player(Session :: session(), Player :: nuk_user:user()) ->
    true |
    false.
has_player(Session, Player) ->
    Players = get_players(Session),
    %% TODO should this check by username instead?
    lists:member(Player, Players).

-spec increment_turn_number(Session :: session()) -> session().
increment_turn_number(Session) ->
    TurnNumberNew = get_turn_number(Session) + 1,
    set_turn_number(Session, TurnNumberNew).

-spec set_game_state(Session :: session(), GameState :: term()) -> session().
set_game_state(Session, GameState) ->
    Session#{game_state := GameState}.

-spec add_player(Session :: session(), Player :: nuk_user:user()) -> session().
add_player(Session, Player) ->
    NukState = get_state(Session),
    NewNukState = nuk_state_add_player(NukState, Player),
    Session#{nuk_state := NewNukState}.

-spec set_players(Session :: session(), Players :: [nuk_user:user()]) -> session().
set_players(Session, Players) when is_list(Players) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{players := Players}}.

-spec set_players_turn(Session :: session(), Players :: [nuk_user:user()]) -> session().
set_players_turn(Session, Players) when is_list(Players) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{players_turn := Players}}.

-spec set_status(Session:: session(), Status :: atom()) -> session().
set_status(Session, Status) when is_atom(Status) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{status := Status}}.

-spec set_turn_number(Session:: session(), TurnNumber :: integer()) -> session().
set_turn_number(Session, TurnNumber) when is_integer(TurnNumber) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{turn_number := TurnNumber}}.

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

-spec get_state(Session :: session()) -> nuk_state().
get_state(#{nuk_state := NukState}) ->
    NukState.

-spec nuk_state_add_player(NukState :: nuk_state(), Player :: nuk_user:user()) ->
    nuk_state().
nuk_state_add_player(#{players := Players} = NukState, Player) ->
    NukState#{players := [Player|Players]}.
