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
    Game1 = nuk_game:new("GoodGame1", nuk_game_bogus1),
    ok = nuk_games:register(Game1),
    {error, game_not_found, _} = nuk_games:get("BadGame"),
    {ok, Game1} = nuk_games:get("GoodGame1").

nuk_games_unregister(_) ->
    ok = nuk_games:register(nuk_game:new("GoodGame1", nuk_game_bogus1)),
    ok = nuk_games:unregister("GoodGame1"),
    {error, game_not_found, _} = nuk_games:get("GoodGame1").

nuk_games_list(_) ->
    Game1 = nuk_game:new("GoodGame1", nuk_game_bogus1),
    Game2 = nuk_game:new("GoodGame2", nuk_game_bogus2),
    ok = nuk_games:register(Game1),
    ok = nuk_games:register(Game2),
    Expected = lists:sort([Game1, Game2]),
    Expected = lists:sort(nuk_games:list()).

nuk_game_flow(_) ->
    % register game
    Game = nuk_game:new("Coin Flip", nuk_game_coinflip),
    ok = nuk_games:register(Game),
    % create and login user
    ok = nuk_users:put(nuk_user:new("User1", "Pass1")),
    {ok, UserSessionId} = nuk_users:login("User1", "Pass1"),
    {ok, GameSessionId} = nuk_games:create(UserSessionId, "Coin Flip"),
    {error, user_already_joined, _} = nuk_games:join(GameSessionId, UserSessionId).
