-module(nuk_game_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    nuk_games_register_get/1,
    nuk_games_unregister/1,
    nuk_games_list/1,
    nuk_game_flow/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        nuk_games_register_get,
        nuk_games_unregister,
        nuk_games_list,
        nuk_game_flow
    ].

init_per_suite(Config) ->
    ok = application:start(nuk),
    Config.

end_per_suite(_) ->
    application:stop(nuk),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

nuk_games_register_get(_) ->
    Game1 = nuk_game:new("GoodGame1", nuk_game_bogus1, 1, 1),
    ok = nuk_games:register(Game1),
    {error, game_not_found, _} = nuk_games:get("BadGame"),
    {ok, Game1} = nuk_games:get("GoodGame1").

nuk_games_unregister(_) ->
    ok = nuk_games:register(nuk_game:new("GoodGame1", nuk_game_bogus1, 1, 1)),
    ok = nuk_games:unregister("GoodGame1"),
    {error, game_not_found, _} = nuk_games:get("GoodGame1").

nuk_games_list(_) ->
    Game1 = nuk_game:new("GoodGame1", nuk_game_bogus1, 1, 1),
    Game2 = nuk_game:new("GoodGame2", nuk_game_bogus2, 1, 1),
    ok = nuk_games:register(Game1),
    ok = nuk_games:register(Game2),
    Expected = lists:sort([Game1, Game2]),
    Expected = lists:sort(nuk_games:list()).

nuk_game_flow(_) ->
    % register game
    Game = nuk_game:new("Coin Flip", nuk_game_coinflip, 1, 1),
    ok = nuk_games:register(Game),

    % create and login user
    User1 = nuk_user:new("User1", "Pass1"),
    User2 = nuk_user:new("User2", "Pass2"),
    ok = nuk_users:put(User1),
    ok = nuk_users:put(User2),
    {ok, UserSessionId1} = nuk_users:login("User1", "Pass1"),
    {ok, UserSessionId2} = nuk_users:login("User2", "Pass2"),

    % create a game and join players
    {ok, GameSessionId} = nuk_games:create(UserSessionId1, "Coin Flip"),
    % TODO test valid join: cannot be tested in 1p mode
    {error, user_already_joined, _} = nuk_games:join(GameSessionId, UserSessionId1),
    {error, max_users_reached, _} = nuk_games:join(GameSessionId, UserSessionId2),
    {error, game_session_not_found, _} = nuk_games:get_game_session("foobar"),
    {ok, GameSession} = nuk_games:get_game_session(GameSessionId),
    ExpectedGameState = #{turn_number => 0, wins => 0, max_turns => 3, user => User1},
    ExpectedGameState = nuk_game_session:get_game_state(GameSession),
    [User1] = nuk_game_session:get_players(GameSession),
    1 = nuk_game_session:get_players_count(GameSession),
    true = nuk_game_session:has_player(GameSession, User1).
