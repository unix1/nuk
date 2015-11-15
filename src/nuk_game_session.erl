%%%-------------------------------------------------------------------
%% @doc nuk game session
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_session).

%% API
-export([new/1, get_game_state/1, set_game_state/2]).
-export_type([session/0]).

-opaque session() :: #{game => nuk_game:game(),
                       nuk_state => term(),
                       game_state => term()}.

%%====================================================================
%% API
%%====================================================================

-spec new(Game :: nuk_game:game()) -> session().
new(Game) ->
    #{game => Game, nuk_state => nil, game_state => nil}.

-spec get_game_state(nuk_game_session:session()) -> term().
get_game_state(#{game_state := GameState}) ->
    GameState.

-spec set_game_state(Session :: session(), GameState :: term()) -> session().
set_game_state(Session, GameState) ->
    Session#{game_state := GameState}.
