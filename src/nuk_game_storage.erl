%%%-------------------------------------------------------------------
%% @doc nuk game storage
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_storage).

-callback delete(GameName :: string()) ->
    'ok'.

-callback get(GameName :: string()) ->
    {ok, nuk_game:game()} |
    {error, ErrorCode :: game_not_found, ErrorText :: string()}.

-callback put(Game :: nuk_game:game()) ->
    'ok'.

-callback list() ->
    [nuk_game:game()].
