%%%-------------------------------------------------------------------
%% @doc nuk game engine
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_engine).

-callback initialize(Player :: nuk_user:user(), Options :: list()) ->
    {ok, EngineState :: term()} |
    {error, invalid_options, Extra :: string()}.

-callback player_join(Player :: nuk_user:user(), EngineState :: term()) ->
    {ok, NewEngineState :: term()} |
    {error, ErrorCode :: atom(), Extra :: string()}.

-callback player_leave(Player :: nuk_user:user(), EngineState :: term()) ->
    {ok, NewEngineState :: term()} |
    {error, unknown_user, Extra :: string()} |
    {ok, complete, Winners :: [nuk_user:user()], NewEngineState :: term()}.

-callback start(EngineState :: term()) ->
    {ok, await_turn, NextTurn :: [nuk_user:user()], NewEngineState :: term()}.

-callback turn(Player :: nuk_user:user(), Turn :: term(), EngineState :: term()) ->
    {ok, await_turn, NextTurnPlayers :: [nuk_user:user()], NewEngineState :: term()} |
    {ok, complete, Winners :: [nuk_user:user()], Losers :: [nuk_user:user()], Extra :: term()} |
    {error, bad_turn_order, Extra :: string()} |
    {error, invalid_turn, Extra :: string()}.

-callback finish(EngineState :: term()) ->
    ok.
