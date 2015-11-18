%%%-------------------------------------------------------------------
%% @doc nuk game session storage
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_session_storage).

-callback get(SessionId :: string()) ->
    {ok, nuk_game_session:session()} |
    {error, ErrorCode :: session_not_found, ErrorText :: string()}.

-callback delete(SessionId :: string()) ->
    ok.

-callback list() ->
    [nuk_game_session:session()].
