%%%-------------------------------------------------------------------
%% @doc `nuk_game_engine' module
%%
%% This is a behavior that all game engines must implement. It is also the only
%% logic that game engines need implement. All callback function returns allow
%% engines to: (1) set the arbitrary state that's relevant to the game being
%% implemented, (2) get callbacks for important game events with the arbitrary
%% state previously set.
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
    {ok, await_turn, NextTurnPlayers :: [nuk_user:user()], NewEngineState :: term()} |
    {ok, complete, Winners :: [nuk_user:user()], Losers :: [nuk_user:user()], NewEngineState :: term()} |
    {error, game_already_started, Extra :: string()}.

-callback start(EngineState :: term()) ->
    {ok, await_turn, NextTurnPlayers :: [nuk_user:user()], NewEngineState :: term()}.

-callback turn(Player :: nuk_user:user(), Turn :: term(), EngineState :: term()) ->
    {ok, await_turn, NextTurnPlayers :: [nuk_user:user()], NewEngineState :: term()} |
    {ok, complete, Winners :: [nuk_user:user()], Losers :: [nuk_user:user()], NewEngineState :: term()} |
    {error, bad_turn_order, Extra :: string()} |
    {error, invalid_turn, Extra :: string()}.

-callback finish(EngineState :: term()) ->
    ok.
