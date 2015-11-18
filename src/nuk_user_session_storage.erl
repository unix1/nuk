%%%-------------------------------------------------------------------
%% @doc nuk user session storage
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_session_storage).

-callback get(SessionId :: string()) ->
    {ok, nuk_user_session:session()} |
    {error, ErrorCode :: session_not_found, ErrorText :: string()}.

-callback delete(SessionId :: string()) ->
    ok.

-callback list() ->
    [nuk_user_session:session()].
