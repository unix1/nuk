%%%-------------------------------------------------------------------
%% @doc `nuk_game_session_storage' module
%%
%% This is a behavior that allows to extend the game session ID to game process
%% ID map. The default simple proof of concept implementation is provided in
%% {@link nuk_game_session_store_server}.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_session_storage).

-callback get_pid(SessionId :: string()) ->
    {ok, pid()} |
    {error, ErrorCode :: game_session_not_found, ErrorText :: string()}.

-callback put(Pid :: pid()) -> SessionId :: string().

-callback delete(SessionId :: string()) ->
    ok.
