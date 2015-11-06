%%%-------------------------------------------------------------------
%% @doc nuk games
%% @end
%%%-------------------------------------------------------------------

-module(nuk_games).

%% API
-export([register/1, unregister/1, get/1, list/0]).

-spec register(Game :: nuk_game:game()) -> ok.
register(Game) ->
    ok = nuk_game_store_server:put(Game).

-spec unregister(GameName :: string()) -> ok.
unregister(GameName) ->
    ok = nuk_game_store_server:delete(GameName).

-spec get(GameName :: string()) ->
    {ok, nuk_game:game()} |
    {error, game_not_found, string()}.
get(GameName) ->
    nuk_game_store_server:get(GameName).

-spec list() -> [nuk_game:game()].
list() ->
    nuk_game_store_server:list().
