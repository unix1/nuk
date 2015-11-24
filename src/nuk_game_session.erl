%%%-------------------------------------------------------------------
%% @doc nuk game session
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_session).

%% API
-export([new/1, get_game/1, get_state/1, get_game_state/1, set_game_state/2]).
-export([set_players/2, set_status/2]).
-export_type([session/0, nuk_state/0]).

-opaque session() :: #{game => nuk_game:game(),
                       nuk_state => nuk_state(),
                       game_state => term()}.

-opaque nuk_state() :: #{status => atom(), players => list()}.

%%====================================================================
%% API
%%====================================================================

-spec new(Game :: nuk_game:game()) -> session().
new(Game) ->
    #{game => Game, nuk_state => #{players => [], status => nil}, game_state => nil}.

-spec get_game(nuk_game_session:session()) -> nuk_game:game().
get_game(#{game := Game}) ->
    Game.

-spec get_state(Session :: session()) -> nuk_state().
get_state(#{nuk_state := NukState}) ->
    NukState.

-spec get_game_state(nuk_game_session:session()) -> term().
get_game_state(#{game_state := GameState}) ->
    GameState.

-spec set_game_state(Session :: session(), GameState :: term()) -> session().
set_game_state(Session, GameState) ->
    Session#{game_state := GameState}.

-spec set_players(Session :: session(), Players :: [nuk_user:user()]) -> session().
set_players(Session, Players) when is_list(Players) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{players := Players}}.

-spec set_status(Session:: session(), Status :: atom()) -> session().
set_status(Session, Status) when is_atom(Status) ->
    NukState = get_state(Session),
    Session#{nuk_state := NukState#{status := Status}}.
