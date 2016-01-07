%%%-------------------------------------------------------------------
%% @doc `nuk_user_session_storage' module
%%
%% This is a behavior that allows to extend the user session ID to user process
%% ID map. The default simple proof of concept implementation is provided in
%% {@link nuk_user_session_store_server}.
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
