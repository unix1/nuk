%%%-------------------------------------------------------------------
%% @doc `nuk_game_storage' module
%%
%% This behavior allows to extend the storage service for registered games.
%% When a new game engine is registered with nuk via
%% {@link nuk_games:register/1} it is stored internally by the system.
%% Implementing this behavior allows a custom storage backend to be defined.
%% The default simple implementation is provided with
%% {@link nuk_game_store_server}.
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
