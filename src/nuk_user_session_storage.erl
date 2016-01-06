%%%-------------------------------------------------------------------
%% @doc nuk user session storage
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_session_storage).

-callback get_pid(SessionId :: string()) ->
    {ok, pid()} |
    {error, ErrorCode :: user_session_not_found, ErrorText :: string()}.

-callback put(Pid :: pid()) ->
    SessionId :: string().

-callback delete(SessionId :: string()) ->
    ok.

-callback list() ->
    [nuk_user_session:session()].
