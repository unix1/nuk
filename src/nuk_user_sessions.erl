%%%-------------------------------------------------------------------
%% @doc nuk sessions
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_sessions).

%% API
-export([get/1, get_user/1, put/1, delete/1, list/0]).

-spec get(SessionId :: string()) ->
    {ok, nuk_user_session:session()} |
    {error, user_session_not_found, Extra :: string()}.
get(SessionId) ->
    nuk_user_session_store_server:get(SessionId).

-spec get_user(SessionId :: string()) ->
    {ok, nuk_user:user()} |
    {error, user_session_not_found, Extra :: string()}.
get_user(SessionId) ->
    case nuk_user_sessions:get(SessionId) of
        {ok, Session} ->
            {ok, nuk_user_session:get_user(Session)};
        {error, user_session_not_found, Reason} ->
            {error, user_session_not_found, Reason}
    end.

-spec put(Pid :: pid()) -> SessionId :: string().
put(Pid) when is_pid(Pid) ->
    nuk_user_session_store_server:put(Pid).

-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    nuk_user_session_store_server:delete(SessionId).

-spec list() -> [nuk_user_session:session()].
list() ->
    nuk_user_session_store_server:list().
